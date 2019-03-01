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

package org.seekloud.paradise.snake

import org.seekloud.paradise.ptcl.PDGameEvent.{MouseEvent, Snapshot, SpeedEvent}
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.ptcl._
import org.slf4j.LoggerFactory
import shapeless.ops.nat.Max

import scala.util.Random

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 9:55 PM
  */
class GridOnServer(override val boundary: Point) extends Grid {


  private[this] val log = LoggerFactory.getLogger(this.getClass)

  override def debug(msg: String): Unit = log.debug(msg)

  override def info(msg: String): Unit = log.info(msg)


  private[this] var waitingJoin = Map.empty[String, (String,Point,String)] //  id -> (name,header,color)

  private[this] var feededApples: List[Ap] = Nil
  private[this] var barriers: List[Br] = Nil
  private[this] var newSnakes: List[SkDt] = Nil
  private[this] var deadBody: List[(String,Point)] = Nil
//  private[this] var deadBody = Map.empty[String,Point]
  private[this] var eatenApples = Map.empty[String,List[Ap]]
  private[this] var KillMsg = List.empty[KillMessage]

  var vicSnakes = List.empty[String]
  var removeGrid = List.empty[(Point, Int, String)]

  var currentRank = List.empty[Score]
  private[this] var historyRankMap = Map.empty[String, Score]
  var historyRankList = historyRankMap.values.toList

  private[this] val historyRankThreshold = if (historyRankList.isEmpty) {
    -1
  } else {
    historyRankList.map(_.k).min
  }

  def addSnake(id: String, name: String): (Point,String) = {
    val header = randomEmptyPoint()
    val color = new Random(System.nanoTime()).nextInt(7).toString
    waitingJoin += (id -> (name,header,color))
    (header,color)
  }

  private[this] def genWaitingSnake() = {
    waitingJoin.filterNot(kv => snakes.contains(kv._1)).foreach { case (id, info) =>
      val name = info._1
      val header = info._2
      val color = info._3
      grid += header -> Body(id, defaultLength - 1, color, 1000 / Protocol.frameRate * 8)
      val tail = Point(header.x - 36, header.y)
      snakes += id -> SkDt(id, name, color,header,tail)
    }
    waitingJoin = Map.empty[String, (String,Point,String)]
  }

  implicit val scoreOrdering = new Ordering[Score] {
    override def compare(x: Score, y: Score): Int = {
      var r = y.k - x.k
      if (r == 0) {
        r = y.l - x.l
      }
      if (r == 0) {
        r = x.id.compareTo(y.id)
      }
      r

    }
  }

  private[this] def updateRanks() = {
    currentRank = snakes.values.map(s => Score(s.id, s.name, s.kill, s.length)).toList.sortBy(_.l).reverse
    var historyChange = false
    currentRank.foreach { cScore =>
      historyRankMap.get(cScore.id) match {
        case Some(oldScore) if cScore.k > oldScore.k || (cScore.k == oldScore.k && cScore.l > oldScore.l) =>
          historyRankMap += (cScore.id -> cScore)
          historyChange = true

        case None if cScore.k > historyRankThreshold =>
          historyRankMap += (cScore.id -> cScore)
          historyChange = true

        case _ =>
        //do nothing.
      }
    }

    if (historyChange) {
      historyRankList = historyRankMap.values.toList.sorted.take(historyRankLength)
//      historyRankThreshold = historyRankList.lastOption.map(_.k).getOrElse(-1)
      historyRankMap = historyRankList.map(s => s.id -> s).toMap
    }

  }

  private[this] def updateVic() = {
    val f = snakes.filter(_._2.length >= 300)
    if (f.nonEmpty) {
      vicSnakes = f.keys.toList
    }
  }

  override def feedApple(appleCount: Int): Unit = {
    feededApples = Nil
    var appleNeeded = snakes.size * 2 + appleNum - appleCount
    while (appleNeeded > 0) {
      val p = randomEmptyPoint()
      val score = random.nextDouble() match {
        case x if x > 0.9 => 6
        case x if x > 0.7 => 4
        case _ => 2
      }
      val apple = Apple(score, appleLife)
      feededApples ::= Ap(score, appleLife, p.x, p.y)
      grid += (p -> apple)
      appleNeeded -= 1
    }
  }

  override def updateDeadMessage(killerName: String, deadId: String, deadKill: Int, deadLength: Int): Unit={

    var killedName = ""
    snakes.values.find(_.id == deadId) match{
      case Some(s)=>
        killedName = s.name
      case None=>
    }
    KillMsg ::= KillMessage(deadId,killedName,killerName,deadKill,deadLength)
  }

  def updateDeadBody(deadSnakeId: String) = {
    grid = grid.map {
      case (p, Body(id, life, _, _)) if id == deadSnakeId && life > 0 =>
        val randomPoint = Point(math.min(math.max(0, p.x - 20 + Random.nextInt(40)), boundary.x), math.min(boundary.y, math.max(0, p.y - 20 + Random.nextInt(40))))
        deadBody ::= (id,randomPoint)
        randomPoint -> Apple(1,1000)
      case other => other
    }
  }

  override def updateDeadSnake(updatedSnakes: List[SkDt],mapKillCounter: Map[String,Int],deads: List[SkDt]): Unit = {

    var newKillCounter = mapKillCounter

    var dead: List[SkDt] = Nil

    deadBody = Nil

    //if two (or more) headers go to the same point,
    val snakesInDanger = updatedSnakes.groupBy(_.header).filter(_._2.lengthCompare(1) > 0).values

    val deadSnakes =
      snakesInDanger.flatMap { hits =>
        val sorted = hits.toSeq.sortBy(_.length)
        val winner = sorted.head
        val deads = sorted.tail
        println("here header attack",winner.id,deads.head.id)

        dead ++= deads
        newKillCounter += winner.id -> (newKillCounter.getOrElse(winner.id, 0) + deads.length)
        deads
      }.map(_.id).toSet


    val finalSnakes = updatedSnakes.filterNot(s => deadSnakes.contains(s.id) || deads.exists(d => d.id == s.id)).map { s =>
      newKillCounter.get(s.id) match {
        case Some(k) => s.copy(kill = s.kill + k)
        case None => s
      }
    }

    if (newKillCounter.nonEmpty) {
      dead ++= deads
      dead.foreach(d => {
        updateDeadBody(d.id)
        pointMap += d.id -> List.empty[Point]
      })
      newSnakes = finalSnakes
    }
    grid ++= finalSnakes.map(s => s.header -> Body(s.id, s.length, s.color, s.protect))
    snakes = finalSnakes.map(s => (s.id, s)).toMap
  }

  //补点的时候去掉长度之外的点
  private def updateBody(snakeId:String,newAdd:Int) = {
    grid = grid.map {
      case (p, b@Body(id, life,_,_)) =>
        if(id == snakeId){
          (p, b.copy(life = life - newAdd))
        }
        else (p,b)
      case x => x
    }
  }

  def judgefield(slength:Int,range:Double,a:Point,b:Point)={
    if (a.x <= b.x + getSnakeRadius(slength) + range && a.x >= b.x - getSnakeRadius(slength) - range && a.y <= b.y +getSnakeRadius(slength) + range && a.y >= b.y - getSnakeRadius(slength) - range)
      true
    else
      false
  }

  override def checkRush(newDirection:Point, snake:SkDt, tail: Point): Either[(String,SkDt), SkDt] = {

    val newHeader = snake.header + newDirection * snake.speed
    var apples=List.empty[Ap]

    if(newDirection != Point(0,0)){
      val scale = snake.speed / 4 //scale倍速
      for(i <- 1 until scale){
        val newAdd = Point(snake.header.x + i * (newHeader.x-snake.header.x)/scale , snake.header.y +i * (newHeader.y - snake.header.y)/scale)
        grid += (newAdd -> Body(snake.id, snake.length + i - 1,snake.color,snake.protect))
      }
      updateBody(snake.id,scale - 1)
    }

    val results =
      grid.filter { case (p, _) =>
        p match {
          case a if judgefield(snake.length,30,a,newHeader) => true
          case _ => false
        }
      case _ => false
      }.map {
        case (p, Body(id, _,_,_)) =>
          if(id == snake.id || snake.protect >= 0 || snakes.getOrElse(id,SkDt("","","",tail = Point(0,0), protect = 0)).protect >= 0) ("nothing" ,"",p,0)//如果蛇在保护期或撞到处于保护期的蛇不算死亡
          else if (judgefield(snake.length,8,p,newHeader)) ("dead" ,id,p,0)
          else ("nothing" ,"",p,0)
        case (p, a@Apple(score, life)) =>
          apples ::= Ap(score,life,p.x,p.y)
          removeGrid ::= (p, score, snake.id)
          ("apple" ,"",p,score)
        case (p,Barrier(_,_))=>
          if(snake.protect >= 0) ("nothing" ,"",p,0)//如果此蛇在保护期就撞到障碍不算死亡
          else if (judgefield(snake.length,8,p,newHeader)) ("dead" ,"",p,0)
          else ("nothing" ,"",p,0)
        case x =>
          ("nothing" ,"",x._1,0)
      }
      eatenApples += (snake.id->apples)

//    if(!deadWall){
      var speed = snake.speed
      val direct = newDirection
      if(snake.speed > Protocol.startSpeed && snake.length <= Protocol.startLen ){//长度小于9时强制减速
        speed = Protocol.startSpeed
        var temp = speedMap.getOrElse(frameCount,Map.empty)
        temp += (snake.id -> false)
        speedMap += (frameCount -> temp)
        //          direct = Point(newDirection.x * 16.0/24.0,newDirection.y * 16.0/24.0)
      }
      if(results.exists(res => res._1 == "dead") ) {
        Left((results.filter(res => res._1 == "dead").head._2,snake))
      }
      else if(results.exists(res => res._1 == "apple")){
        var len = snake.length
        val result = results.filter(res => res._1 == "apple")
        result.foreach{ r=>
          len += r._4
          grid -= r._3
        }
        if(snake.speed > Protocol.startSpeed && snake.length >Protocol.startLen){
          updateBody(snake.id,1)
          len  = len - 1
        }
        Right(snake.copy(header = newHeader, tail = tail, direction = direct, length = len, protect = snake.protect -1,speed = speed))//吃到苹果也不换头结点
      }
      else{
        var len = snake.length
        if(snake.speed > Protocol.startSpeed && snake.length >Protocol.startLen){
          updateBody(snake.id,1)
          len = len -1
        }
        Right(snake.copy(header = newHeader, tail = tail, direction = direct,length = len, protect = snake.protect -1,speed = speed))
      }
  }

  //判断是否生成过障碍
  def judgeBarrier(br:Point) ={
    if(br.x > 0 && br.x < Boundary.w && br.y > 0 && br.y < Boundary.h ){
      true
    }else{
      false
    }
  }

  //上下左右生成障碍
  def genBarrierUnit(udlr:Int, num:Int ,Start:Point) ={
    // 0 右 1 左 2 下 3 上 其他默认向上
    val revise = Point(judgeRange*2,judgeRange*2)
    val End = udlr match {
      case 0 =>
        for(i <- 0 to num){
          val curP = Start + Point(judgeRange *2,0.0) * i
          if(judgeBarrier(curP)){
            barriers ::= Br(i, curP.x, curP.y, true)
          }
        }
        Start + Point(judgeRange *2,0.0)*num + revise

      case 1 =>
        for(i <- 0 to num){
          val curP = Start - Point(judgeRange *2,0.0) * i
          if(judgeBarrier(curP)){
            barriers ::= Br(i, curP.x, curP.y, true)
          }
        }
        Start - Point(judgeRange *2,0.0) * num + revise

      case 2 =>
        for(i <- 0 to num){
          val curP = Start + Point(0.0,judgeRange *2) * i
          if(judgeBarrier(curP)){
            barriers ::= Br(i, curP.x, curP.y, true)
          }
        }
        Start + Point(0.0,judgeRange *2) * num + revise

      case 3 =>
        for(i <- 0 to num){
          val curP = Start - Point(0.0,judgeRange *2) * i
          if(judgeBarrier(curP)){
            barriers ::= Br(i, curP.x, curP.y, true)
          }
        }
        Start - Point(0.0,judgeRange *2) * num + revise

      case _ =>
        for(i <- 0 to num){
          val curP = Start + Point(judgeRange *2,0.0) * i
          if(judgeBarrier(curP)){
            barriers ::= Br(i, curP.x, curP.y, true)
          }
        }
        Start + Point(judgeRange *2,0.0)*num + revise
    }

    val distance = 4

    //上面
    for(x <- Start.x.toInt to End.x.toInt by distance){
      barriers ::= Br(0,x,Start.y,false)
    }
    //左面
    for(y <- Start.y.toInt to End.y.toInt by distance){
      barriers ::= Br(0,Start.x,y,false)
    }

    //下面
    for(x <- Start.x.toInt to End.x.toInt by distance){
      barriers ::= Br(0,x,End.y,false)
    }
    //右面

    for(y <- Start.y.toInt to End.y.toInt by distance){
      barriers ::= Br(0,End.x,y,false)
    }

    barriers = barriers.filter(br=> br.x>0 && br.x < Boundary.w && br.y>0 && br.y < Boundary.h)
    grid ++= barriers.map(br => Point(br.x, br.y) -> Barrier(br.num, br.center))
  }

  def genRandomBarriers() = {
    barriers = Nil
    for(i<- 1 to 8 ){
      val random = Random.nextInt(5)
      val p = randomEmptyPoint()

      random match {
        //这里注意 ，有出现图形空缺的情况多半是监测到要生成的点上原本有障碍点了
        //后面加上Point(0.0,1.0) 是为了防止新生成的点不在原来的点上
        //7
        case 0 =>
          genBarrierUnit(0,5,p)
          genBarrierUnit(2,5,p+Point(judgeRange*2,0.0)*5 + Point(0.0,1.0) )

        //L
        case 1 =>
          genBarrierUnit(2,5,p)
          genBarrierUnit(0,5,p+Point(0.0,judgeRange*2)*5 + Point(1.0,0.0))

        //十
        case 2 =>
          genBarrierUnit(0,10,p)
          genBarrierUnit(2,5,p+Point(judgeRange*2,0.0)*5 + Point(0.0,1.0))
          genBarrierUnit(3,5,p+Point(judgeRange*2,0.0)*5)

        // 二
        case 3 =>
          genBarrierUnit(0,20,p)
          genBarrierUnit(0,20,p+Point(0.0,judgeRange*2)*5)

        // ||
        case 4 =>
          genBarrierUnit(2,20,p)
          genBarrierUnit(2,20,p+Point(judgeRange*2,0.0)*5)

        //T
        case 5 =>
          genBarrierUnit(0,10,p)
          genBarrierUnit(2,7,p+Point(judgeRange*2,0.0)*5 + Point(0.0,1.0))

        case _ =>
          genBarrierUnit(0,5,p)
          genBarrierUnit(2,5,p+Point(judgeRange*2,0.0)*5 )
      }



    }

  }

  override def update(justSynced:Boolean): Unit = {
    super.update(justSynced)
    genWaitingSnake()
    updateRanks()
    updateVic()
  }

  def getDataAfterDead() = {
    var appleDetails: List[Ap] = Nil
    grid.foreach {
      case (p, Apple(score, life)) => appleDetails ::= Ap(score, life, p.x, p.y)
      case _ => //nothing
    }
    //GridDataSyncAfterDead
    SyAD(
      appleDetails
    )
  }

  def resetFoodData(): Unit = {
    eatenApples = Map.empty
  }

  def clearKillMsg(): Unit = {
    KillMsg = Nil
  }

  def getDeadSnakes = deadSnakes

  def getFeededApple = feededApples
  def getBarriers = barriers
  def getNewSnakes = newSnakes
  def getDeads = deadBody
  def getEatenApples = eatenApples
  def getKillMsg = KillMsg

  def getSnapshot = {
    var apples = List.empty[Ap]
    grid.foreach {
      case (p ,Apple(score, life)) => apples = Ap(score, life, p.x, p.y) :: apples
      case _ =>
    }

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

    Snapshot(
      snakes.values.toList,
      pointDetails,
      apples,
      getBarriers,
      historyRankList
    )
  }

  def getMouseEvent(frame: Long): List[MouseEvent] = {
    mouseMap.getOrElse(frame, Map.empty[String,(Double,Double)]).toList.map(a => MouseEvent(a._1, a._2._1, a._2._2))
  }

  def getSpeedEvent(frame: Long): List[SpeedEvent] = {
    speedMap.getOrElse(frame, Map.empty[String, Boolean]).toList.map(a => SpeedEvent(a._1, a._2))
  }

  //战绩获取
  def getExploit(userId: String): (Int,Int) = {
    if(historyRankList.exists(_.id == userId)) {
      val result = historyRankList.filter(_.id == userId).head
      (result.k,result.l)
    }else{
      (0,0)
    }
  }

}