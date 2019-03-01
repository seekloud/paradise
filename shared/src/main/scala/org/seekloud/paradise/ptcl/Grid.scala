/*
 * Copyright 2018 seekloud (https://github.com/seekloud)
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package org.seekloud.paradise.ptcl

import scala.util.Random
import scala.math._
import org.seekloud.paradise.ptcl.paradise.Protocol._

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 5:34 PM
  */
trait Grid {

  val boundary: Point

  def debug(msg: String): Unit

  def info(msg: String): Unit

  val random = new Random(System.nanoTime())

  val defaultLength = 10
  val appleNum = 50
  val appleLife = 150
  val historyRankLength = 5

  var frameCount = 0l

  var grid = Map[Point, Spot]()
  var snakes = Map.empty[String, SkDt]

  var deadSnakes = Map.empty[String,DeadSk]
  var pointMap = Map.empty[String,List[Point]]

  var actionMap = Map.empty[Long, Map[String, Int]]
  var mouseMap = Map.empty[Long,Map[String,(Double,Double)]]  //frame ->(id ->(x,y))
  var speedMap  = Map.empty[Long, Map[String, Boolean]]

  var lengthIncreaseMap = Map.empty[Long, Int]
  var lengthLastIncrease = 0

  def removeSnake(id: String): Option[SkDt] = {
    val r = snakes.get(id)
    if (r.isDefined) {
      snakes -= id
    }
    r
  }


  def addSpeedWithFrame(id: String, isSpeedUp:Boolean, frame: Long) ={
    val map = speedMap.getOrElse(frame, Map.empty[String, Boolean])
    val tmp = map + (id -> isSpeedUp)
    speedMap += (frame -> tmp)
  }

  def addActionWithFrame(id: String, keyCode: Int, frame: Long) = {
    val map = actionMap.getOrElse(frame, Map.empty[String, Int])
    val tmp = map + (id -> keyCode)
    actionMap += (frame -> tmp)
  }

  def addMouseWithFrame(id: String, x: Double, y:Double, frame: Long) = {
    val map = mouseMap.getOrElse(frame, Map.empty[String,(Double,Double)])
    val tmp = map + (id -> (x,y))
    mouseMap += (frame -> tmp)
  }

  def checkRemovePoint(id:String) = {
    //判断拐点是否需要被去掉，更新尾巴位置
    if (snakes.get(id).isDefined) {
      val snake = snakes(id)

      var tail = snake.tail
      val length = snake.length
      var start = snake.header
      var totalDis = 0.0
      var getResult = true
      if (pointMap.get(id).isDefined && pointMap(id).nonEmpty) {
        pointMap(id).zipWithIndex.foreach { p =>
          val dis = sqrt(pow(p._1.x - start.x, 2) + pow(p._1.y - start.y, 2)) / 4
          if (totalDis + dis >= length && getResult) {
            val tpLast = pointMap(id).last
            val tpSecondLast = if(pointMap(id).lengthCompare(1) > 0) pointMap(id)(pointMap(id).length - 2) else start
            totalDis += dis
            val off = totalDis - length
            if(off >= dis) {

              tail = tpLast

            }else {
              val tailX = tpLast.x + (tpSecondLast.x - tpLast.x) * off / dis
              val tailY = tpLast.y + (tpSecondLast.y - tpLast.y) * off / dis
              tail = Point(tailX, tailY)
            }

            //            snakes  += (id -> snakes(id).copy(tail = Point(tailX, tailY)))
            getResult = false
            //            println("here index is ",p._2)
            pointMap += (id -> pointMap(id).take(p._2))
          } else if(p._2 == pointMap(id).length - 1 && getResult) {
            totalDis += dis
            val off = length - totalDis
            val tpLast = tail
            val tpSecondLast = pointMap(id).last
            val distance = sqrt(pow(tpLast.x - tpSecondLast.x, 2) + pow(tpLast.y - tpSecondLast.y, 2)) / 4
            val tailX = tpLast.x + (tpSecondLast.x - tpLast.x) * (1 - (off / distance))
            val tailY = tpLast.y + (tpSecondLast.y - tpLast.y) * (1 - (off / distance))
            tail = Point(tailX, tailY)
          } else {
            totalDis += dis
          }
          start = p._1
        }
      } else {
        try {
          val dis = sqrt(pow(snake.header.x - snake.tail.x, 2) + pow(snake.header.y - snake.tail.y, 2)) / 4
          val off = dis - snake.length
          val tailX = snake.tail.x + (snake.header.x - snake.tail.x) * off / dis
          val tailY = snake.tail.y + (snake.header.y - snake.tail.y) * off / dis
          //        snakes  += (id -> snakes(id).copy(tail = Point(tailX, tailY)))
          tail = Point(tailX, tailY)
        } catch {
          case e: Exception =>
            println(s"exception:$e")
        }
      }
      tail
    }else{
      Point(0, 0)
    }
  }

  def update(justSynced: Boolean) = {
//    println(s"-------- grid update frameCount= $frameCount ---------")
    if(!justSynced){
      updateSnakes()
    }
    updateSpots()
    if(speedMap.get(frameCount+1).isEmpty){
      speedMap += (frameCount+1 -> speedMap.getOrElse(frameCount,Map.empty))
    }
    mouseMap -= (frameCount-Protocol.advanceFrame-5)
    speedMap -= (frameCount-Protocol.advanceFrame-5)
    actionMap -= frameCount
    if(!justSynced){
      frameCount += 1
    }
  }

  def feedApple(appleCount: Int): Unit
  def updateDeadSnake(newSnakes:List[SkDt],killers:Map[String,Int],deads:List[SkDt]):Unit
  def checkRush(newDirection:Point, snake:SkDt, tail: Point):Either[(String,SkDt), SkDt]
  def updateDeadMessage(killerName: String,deadId:String, deadKill:Int, deadLength:Int):Unit

  private[this] def updateSpots() = {
    var appleCount = 0
    grid = grid.filter { case (p, spot) =>
      spot match {
        case Body(id, life, _, _) if life >= 0 && snakes.contains(id) => true
        case Apple(_, life) if life >= 0 => true
        case Barrier(_, _) => true
        case _ => false
      }
    }.map {
      case (p, b@Body(id, life, _, _)) =>
        val direct = try{snakes(id).direction} catch{case e:Exception => Point(1,0)}
        val finalLife = if(direct == Point(0,0)) life else life -1
        (p, b.copy(life = finalLife, protect = snakes(id).protect - 1))

      case (p, a@Apple(_, life)) =>
        if(life == 1000)
          (p,a)
        else {
          appleCount += 1
          (p, a.copy(life = life - 1))
        }

      case x => x
    }

    feedApple(appleCount)
  }

  def judgePoint(point:Point) = {
    val results =
      grid.filter {
        case (p, _) =>
          p match {
            case a if a.x <= point.x + 20 && a.x >= point.x - 20 && a.y <= point.y +20 && a.y >= point.y - 20 => true
            case _ => false
          }
        case _ => false
      }.map { _ => 0}
    results.filter(res => res == 0).toList
  }

  def randomEmptyPoint(): Point = {
    var r = Point(random.nextInt(boundary.x.toInt - 40)+20 , random.nextInt(boundary.y.toInt -40) + 20)
    //判断已有点是否会与该随机点重合
    var result = judgePoint(r)

    while (result.nonEmpty) {
      r = Point(random.nextInt(boundary.x.toInt - 40) +20, random.nextInt(boundary.y.toInt - 40) + 20)
      result = judgePoint(r)
    }
    r
  }

  def updateASnake(snake: SkDt, actMap: Map[String, (Double,Double)]): Either[(String,SkDt), SkDt] = {
    val MousePosition = actMap.get(snake.id)
    //蛇越长速度越慢
    val newDirection = {
      val keyDirection = MousePosition match {
        case Some(position) =>
          //            val distance = sqrt(pow(position._1 + snake.header.x,2) + pow(position._2 + snake.header.y, 2))
          //鼠标位置距离顶部120，距离左侧8
          //            val x = position._1 - snake.header.x *10 //鼠标位置和蛇头的差
          //            val y = position._2 - snake.header.y *10//鼠标位置和蛇头的差

          //增加拐点
          val getPointList = pointMap.getOrElse(snake.id,List.empty[Point])
          val list = try{

            if(getPointList.isEmpty || (getPointList.nonEmpty && pointMap(snake.id).head != snake.header))
//            if(!pointMap.getOrElse(snake.id,List.empty[Point]).contains(snake.header))
              snake.header :: getPointList
            else
              getPointList
          } catch{
            case e:Exception =>
//              pointMap.getOrElse(snake.id,List.empty[Point])
            getPointList
          }

          pointMap += (snake.id -> list)

          getDirect(snake.direction,position._1,position._2)


        case _ =>
          snake.direction

      }
      if (keyDirection + snake.direction != Point(0, 0)) {
        keyDirection
      } else {
        snake.direction
      }
    }

    val tail = checkRemovePoint(snake.id)//判断是否需要移除无用拐点
    val newHeader = snake.header + newDirection * snake.speed
    //判断是否撞墙
    val deadWall = if(newHeader.x <= 10 || newHeader.x >= boundary.x-10  || newHeader.y <= 10 || newHeader.y >= boundary.y-10 ) true else false

    if(!deadWall){
      checkRush(newDirection,snake, tail)//碰撞检测，前端只返回前进的蛇，后台检测所有信息
    }
    else{
      Right(snake.copy( direction = Point(0,0),protect = snake.protect -1))
    }

  }

  private[this] def updateSnakes() = {

    var mapKillCounter = Map.empty[String, Int]
    var updatedSnakes = List.empty[SkDt]
    var deadSnakes = List.empty[SkDt]

    val acts = mouseMap.getOrElse(frameCount, Map.empty[String, (Double,Double)])
    //更新速度
    val speeds = speedMap.getOrElse(frameCount,Map.empty[String, Boolean])

    val speedSnakes = speeds.filter(s=>snakes.get(s._1).isDefined).map{s =>
      val snakeCopy =if(s._2){
        snakes(s._1).copy(speed = Protocol.speedUp)
      }else{
        snakes(s._1).copy(speed = Protocol.startSpeed)
      }

      (s._1 ,snakeCopy)
    }


    snakes ++= speedSnakes

    snakes.values.map(updateASnake(_, acts)).foreach {
      case Right(s) => updatedSnakes ::= s
      case Left((killerId,deads)) =>
        updatedSnakes ::= deads
        deadSnakes ::= deads
        mapKillCounter += killerId -> (mapKillCounter.getOrElse(killerId, 0) + 1)
        snakes.values.find(_.id==killerId) match {
          case Some(killer)=>
            updateDeadMessage(killer.name,deads.id,deads.kill,deads.length)
          case None=>
            updateDeadMessage("障碍物",deads.id,deads.kill,deads.length)
        }

    }

    updateDeadSnake(updatedSnakes,mapKillCounter,deadSnakes)//后台判定死亡情况

  }

  def getGridData = {

    var pointDetails:List[Tp] = Nil

    pointMap.foreach{
      case (id,list) =>
      if(snakes.get(id).isDefined){
        val color = snakes(id).color
        val protect = snakes(id).protect
        list.foreach{ l =>
          pointDetails ::= Tp(id,color,protect,l.x,l.y)
        }
      }
    }

    GridDataSync(
      frameCount,
      snakes.values.toList,
      pointDetails
    )
  }


  def nextDirection(id:String)={
    val act = mouseMap.getOrElse(frameCount,Map.empty)
    act.get(id) match {
      case Some(p) => Some(Point(p._1,p._2))
      case _=> None
    }

  }

  def getDegree(x:Double,y:Double)={
    atan2(y,x )
  }

  //根据鼠标方向获取方向的单位向量
  def getDirect(direction:Point,x:Double,y:Double)={
//    val deg = getDegree(x,y)
//    val degX = if((cos(deg)).isNaN) 0 else (cos(deg))
//    val degY = if((sin(deg)).isNaN) 0 else (sin(deg))
//    Point(degX,degY)
    val deg1 = atan2( direction.y ,direction.x )
    val deg = atan2(y ,x )//鼠标位置相对于头的角度

    if( abs(deg - deg1) <= Protocol.maxAngle || abs(2 * Math.PI - abs(deg - deg1)) <= Protocol.maxAngle || direction == Point(0,0)){//与原本方向的差值
      //              println("here not check",deg)
      val degX = if(cos(deg).isNaN) 0 else cos(deg)
      val degY = if(sin(deg).isNaN) 0 else sin(deg)
      //            Point(degX * snake.speed,degY * snake.speed)
      Point(degX,degY)
    }
    else{
      val dif = deg1 - deg //原本方向 - 鼠标方向
      var difDeg = if(dif <= - Math.PI && dif > -2 * Math.PI ){
        deg1 - Protocol.maxAngle
      }
      else if(dif <= 0 && dif > - Math.PI){
        deg1 + Protocol.maxAngle
      }
      else if(dif <= Math.PI && dif > 0){
        deg1 - Protocol.maxAngle
      }
      else {//if(dif <= 2 * Math.PI && dif > Math.PI )
        deg1 + Protocol.maxAngle
      }

      if(difDeg > Math.PI)
        difDeg = difDeg - 2 * Math.PI

      if(difDeg < - Math.PI)
        difDeg = difDeg + 2 * Math.PI

      val degX = if(cos(difDeg).isNaN) 0 else cos(difDeg)
      val degY = if(sin(difDeg).isNaN) 0 else sin(difDeg)

      Point( degX, degY)
    }
  }

}
