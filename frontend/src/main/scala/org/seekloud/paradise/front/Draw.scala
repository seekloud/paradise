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

package org.seekloud.paradise.front

import org.seekloud.paradise.front.utils.Performance
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.ptcl._
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import org.scalajs.dom.html
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.HTMLElement

import scala.collection.mutable
import scalatags.JsDom.short._
import scala.math._
import scala.util.Random

class Draw(ctx: dom.CanvasRenderingContext2D,canvas: Canvas) {

  private val bounds = Point(Boundary.w, Boundary.h)
  private var window = Point(dom.document.documentElement.clientWidth,dom.document.documentElement.clientHeight)
  private[this] val offCanvas = dom.document.getElementById("offCanvas").asInstanceOf[Canvas]
  private[this] val offCtx = offCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  offCanvas.width = bounds.x.toInt
  offCanvas.height = bounds.y.toInt

  var size = (window.x*0.01).toInt
  private val leftBegin = 10
  private var rightBegin = dom.document.documentElement.clientWidth - 162
  private val textLineHeight = 14

  private val Monster = img(*.style := "width:15px;")(*.src := s"/paradise/static/img/monster.png").render
  private val Fire = img(*.style := "width:15px;")(*.src := s"/paradise/static/img/fire.png").render
  private val Stone = img(*.style := "width:15px;")(*.src := s"/paradise/static/img/yunshi1.png").render
  private val MapMy = img(*.src := s"/paradise/static/img/starYell.png").render
  private val King = img(*.src := s"/paradise/static/img/king.png").render

  private val a = dom.document.getElementById("grass").asInstanceOf[HTMLElement]
  private val b = dom.document.getElementById("bk1").asInstanceOf[HTMLElement]
  private val c = dom.document.getElementById("bk2").asInstanceOf[HTMLElement]
  val MyHead = img(*.style := "width:15px;")(*.src := s"/paradise/static/img/mysnake.png").render
  val OtherHead = img(*.style := "width:15px;")(*.src := s"/paradise/static/img/osnake.png").render

  val gameover =img(*.src := s"/paradise/static/img/gameover.png").render
  val win =img(*.src := s"/paradise/static/img/win.png").render
  val dead =img(*.src := s"/paradise/static/img/dead.png").render

  var OutsideColor = ""
  var bk = a
  var barrierImg = Monster

  private val mapCacheMap = mutable.HashMap[Int,html.Canvas]()
  private val appleCacheMap = mutable.HashMap[Int,html.Canvas]()


  def reSetScreen() = {
    window = Point(dom.document.documentElement.clientWidth, dom.document.documentElement.clientHeight )
    canvas.width = window.x.toInt
    canvas.height = window.y.toInt
    val NewSize = (window.x*0.01).toInt
    size = NewSize
    rightBegin = dom.document.documentElement.clientWidth - 150
    drawBackground()
  }

  private def generateMap(score:Int):html.Canvas = {
    val canvasCache = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctxCache = canvasCache.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val color = score match {
      case 1 => "rgb(255,0,0)"
      case _ => "rgb(255,255,255)"
    }

    canvasCache.width = 10
    canvasCache.height = 10

    ctxCache.beginPath()
    ctxCache.fillStyle = color
    ctxCache.arc(5,5, 5,0,2*Math.PI)
    ctxCache.fill()
    canvasCache
  }

  private def generateApple(apple:Ap):html.Canvas = {
    val canvasCache = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    val ctxCache = canvasCache.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val color = apple.score match {
      case 1 => Color.Cyan.toString()
      case 6 => "rgb(255, 255, 0)"
      case 4 => "rgb(30,144,255)"
      case _ => "rgb(255, 79, 0)"
    }

    canvasCache.width = 50
    canvasCache.height = 50

    ctxCache.beginPath()
    ctxCache.shadowBlur=20	//设置像素模糊值为50；光影效果
    ctxCache.shadowOffsetX= 0//横向值为0;
    ctxCache.shadowOffsetY= 0//纵向值为0；
    ctxCache.shadowColor="white"
    ctxCache.fillStyle = color
    ctxCache.arc(25,25, 5,0,2*Math.PI)
    ctxCache.fill()
    //    ctxCache.closePath()
    canvasCache
  }

  def setDefault(num: Long) = {
    num match{
      case 1 =>
        OutsideColor = "#000"
        bk = b
        barrierImg = Fire
      case 2 =>
        OutsideColor = "#3F8630"
        bk = a
        barrierImg = Monster
      case 3 =>
        OutsideColor = "#01021E"
        bk = c
        barrierImg = Stone
      case _ =>
        bk = a
        barrierImg = Monster
    }
  }


  def drawTextLine(str: String, x: Int, lineNum: Int, lineBegin: Int = 0,`type`:Int) = {
    ctx.textAlign = "left"
    ctx.textBaseline = "top"
    `type` match{
      case 3 =>
        ctx.font = "16px Comic Sans MS"
      case _ =>
        ctx.font = "20px 黑体"
        ctx.fillStyle="white"

    }
    ctx.fillText(str, x, (lineNum + lineBegin - 1) * textLineHeight)
  }

  def drawRank(Rank:List[Score],CurrentOrNot:Boolean,id:String):Int={
    val num=Rank.size
    val RankBaseLine = 2
    var index = 0
    var x=leftBegin
    var text="实时排行榜"
    if (!CurrentOrNot){
      x=rightBegin-100
      text="历史排行榜"
    }

    ctx.fillStyle="rgba(192,192,192,0.6)"
    ctx.fillRect(x,0,250,40+35*num)
    drawTextLine(s"       ${text}        ", x+30, index, RankBaseLine-1,2)
    ctx.beginPath()
    ctx.strokeStyle=Color.White.toString()
    ctx.moveTo(x,30)
    ctx.lineTo(x+250,30)
    ctx.stroke()
    Rank.foreach { score =>
      index += 1
      if (score.id!=id){
        ctx.fillStyle="white"
      }
      else {
        ctx.fillStyle="#FFFF00"
      }
      drawTextLine(s"$index:  ${if(score.n.length>5)score.n.take(5)+".." else score.n}    ${score.l}   kill=${score.k}", x+10, index*2, RankBaseLine,3)
    }
    index+=1
    40+35*num
  }

  def drawBarrage(s:String,x:Double,y:Double):Unit={
    ctx.save()
    ctx.font=s"${size}px Comic Sans Ms"
    ctx.fillStyle=Color.White.toString()
    ctx.fillText(s,x,y)
    ctx.restore()
  }

  // 小地图背景绘制
  def drawSmallMap(myId:String,snakes:List[SkDt],barriers: List[Br]): Unit = {
    val MapSize = 120
    val MapStartX = window.x-MapSize*2
    val MapStartY = window.y-MapSize

    ctx.fillStyle = "rgba(192,192,192,0.6)"

    ctx.fillRect(MapStartX,MapStartY,MapSize*2,MapSize)

    ctx.fillStyle = "rgba(255,0,0)"
    barriers.filter(_.center).filter(br=> br.x>0 && br.x < bounds.x && br.y>0 && br.y < bounds.y).foreach{
      case Br(_,x,y,_) =>
        val cacheCanvas = mapCacheMap.getOrElseUpdate(1, generateMap(1))
        val MapX = x/bounds.x*MapSize*2
        val MapY = y/bounds.y*MapSize
        ctx.drawImage(cacheCanvas, MapStartX+MapX-2,MapStartY+MapY-2,4,4)
    }

    var drawKing = true
    snakes.sortBy(_.id).sortBy(_.length).reverse.foreach { snake =>
      val id = snake.id
      val MapX = snake.header.x/bounds.x*MapSize*2
      val MapY = snake.header.y/bounds.y*MapSize

      if(drawKing){
        ctx.drawImage(King,MapStartX+MapX-6,MapStartY+MapY-15,15,20)
        drawKing = false
      }
      else{
        if (id == myId) {
          ctx.drawImage(MapMy,MapStartX+MapX-7,MapStartY+MapY-7,15,15)
        } else {
          val cacheCanvas = mapCacheMap.getOrElseUpdate(2, generateMap(2))
          ctx.drawImage(cacheCanvas, MapStartX+MapX-4,MapStartY+MapY-4,8,8)
        }
      }
    }
  }

  def drawBackground(): Unit ={
    val backgroundCanvas = dom.document.getElementById("backCanvas").asInstanceOf[Canvas]
    val backCtx = backgroundCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    backgroundCanvas.width = window.x.toInt
    backgroundCanvas.height = window.y.toInt

    backCtx.beginPath()
    backCtx.fillStyle = OutsideColor
    backCtx.fillRect(0, 0 ,backgroundCanvas.width,backgroundCanvas.height)
  }

  def drawOffMap()={

    //地图绘制
    offCtx.drawImage(bk,0,0,Boundary.w.toInt,bounds.y.toInt)
    //边界绘制
    offCtx.strokeStyle = "#ccc"
    offCtx.lineWidth=20
    offCtx.rect(0,0,bounds.x.toInt,bounds.y.toInt)
    offCtx.stroke()
  }

  def drawOffBarrier(barriers:List[Br]) ={
    offCtx.beginPath()
    barriers.filter(br=> br.center).foreach{case Br(_,x,y,_) =>
      offCtx.drawImage(barrierImg,x - 8,y - 8,16,16)
    }
  }

  def drawWin() = {
    ctx.drawImage(win,0,0,window.x,window.y)
    ctx.fillStyle = Color.Red.toString()
    ctx.fillText("Congratulations On Your Victory!",window.x*0.13,window.y*0.09)
    ctx.fillText("Press Space Key To Start A NEW GAME!", window.x*0.13, window.y*0.13)
  }

  def drawVic(vicSnake:String) = {
    ctx.drawImage(gameover,0,0,window.x,window.y)
    ctx.fillStyle = Color.Black.toString()
    ctx.font = "30px Helvetica"
    ctx.fillText(s"Congratulations to $vicSnake for victory!", window.x*0.13, window.y*0.14)
    ctx.fillText("Press Space Key To Start A NEW GAME!", window.x*0.13, window.y*0.23)
  }

  def drawWait(firstCome:Int,killer:String,finalLength:Int,finalKill:Int) = {
    ctx.save()
    ctx.fillStyle = "#ccc"//Color.Black.toString()
    ctx.fillRect(0, 0, window.x , window.y )
    ctx.font = "30px Helvetica"
    if(firstCome == 0) {
      ctx.fillStyle = Color.Black.toString()
      ctx.fillText("Please wait. or Refresh to change your Room", window.x*0.13, window.y*0.26)
    }else if(firstCome == 1){
      ctx.fillStyle = "#CD3700"
      ctx.fillText(s"You Dead, Press Space Key To Revenge!", window.x*0.3, window.y*0.16)
      drawRoundRect(window.x*0.3,window.y*0.28,window.x/3,window.y/4,30)
      ctx.drawImage(dead,window.x*0.65,window.y*0.37,window.x*0.1,window.x*0.1)
      ctx.font = s"${window.x*0.02}px Comic Sans MS"
      ctx.fillStyle="#EE9A00"
      ctx.fillText(s"The   Killer  Is    :", window.x*0.32, window.y*0.3)
      ctx.fillText(s"Your  Final   Length:", window.x*0.32, window.y*0.37)
      ctx.fillText(s"Your  Final   Kill  :", window.x*0.32, window.y*0.44)
      ctx.fillStyle=Color.Black.toString()
      ctx.fillText(s"$killer",window.x*0.56,window.y*0.3)
      ctx.fillText(s"$finalLength",window.x*0.56,window.y*0.37)
      ctx.fillText(s"$finalKill",window.x*0.56,window.y*0.44)
    }
    else if(firstCome == 2){
      drawRoundRect(window.x*0.3,window.y*0.28,window.x/3,window.y/4,30)
      ctx.drawImage(dead,window.x*0.65,window.y*0.37,window.x*0.1,window.x*0.1)
      ctx.fillStyle = "#CD3700"
      ctx.fillText(s"您观战的玩家已经死亡，请等待玩家重新开始", window.x*0.31, window.y*0.37)
    }
    else if(firstCome == 3){
      drawRoundRect(window.x*0.3,window.y*0.28,window.x/3,window.y/4,30)
      ctx.drawImage(dead,window.x*0.65,window.y*0.37,window.x*0.1,window.x*0.1)
      ctx.fillStyle = "#CD3700"
      ctx.fillText(s"此房间没有玩家，请退出重新选择", window.x*0.31, window.y*0.37)
    }
    else if(firstCome == 5) {
      ctx.font = "36px Helvetica"
      ctx.fillText("Loading", window.x*0.13, window.y*0.26)
    } else if(firstCome == 6) {
      ctx.font = "36px Helvetica"
      ctx.fillText("该游戏已结束", window.x*0.13, window.y*0.26)
    } else if(firstCome == 7) {
      ctx.font = "36px Helvetica"
      ctx.fillText("回放结束", window.x*0.13, window.y*0.26)
    } else if(firstCome == 8) {
      ctx.font = "36px Helvetica"
      ctx.fillText("该游戏已结束", window.x*0.13, window.y*0.26)
    }
    else{
      drawRoundRect(window.x*0.3,window.y*0.28,window.x/3,window.y/4,30)
      ctx.drawImage(dead,window.x*0.65,window.y*0.37,window.x*0.1,window.x*0.1)
      ctx.fillStyle = "#CD3700"
      ctx.fillText(s"您观战的玩家已离开，请退出重新选择", window.x*0.31, window.y*0.37)
    }
    ctx.restore()
  }

  def drawGameOff(firstCome:Int,nextFrame:Int): Unit = {

    ctx.fillStyle = "#ccc"//Color.Black.toString()
    ctx.fillRect(0, 0, window.x , window.y )
    ctx.fillStyle = "rgb(0, 0, 0)"
    if (firstCome == 0) {
      ctx.font = "36px Helvetica"
      ctx.fillText("Welcome.", window.x*0.13, window.y*0.26)
    } else if(firstCome == 5) {
      ctx.font = "36px Helvetica"
      ctx.fillText("Loading", window.x*0.13, window.y*0.26)
    } else if(firstCome == 6) {
      ctx.font = "36px Helvetica"
      ctx.fillText("该游戏已结束", window.x*0.13, window.y*0.26)
    } else if(firstCome == 7) {
      dom.window.cancelAnimationFrame(nextFrame)
      ctx.font = "36px Helvetica"
      ctx.fillText("回放结束", window.x*0.13, window.y*0.26)
    } else if(firstCome == 8) {
      dom.window.cancelAnimationFrame(nextFrame)
      ctx.font = "36px Helvetica"
      ctx.fillText("该游戏已结束", window.x*0.13, window.y*0.26)
    } else if(firstCome == 100){
      dom.window.cancelAnimationFrame(nextFrame)
      ctx.font = "36px Helvetica"
      ctx.fillText("异地登陆", window.x*0.13, window.y*0.26)
    }else {
      //好像没停
      dom.window.cancelAnimationFrame(nextFrame)
      ctx.font = "36px Helvetica"
      ctx.fillText("Ops, connection lost.", window.y*0.13, window.y*0.26)
    }
  }

  def generateColor(num:Int):String = {
    num match {
      case 0 => "#FFE1FF"
      case 1 => "#FFC125"
      case 2  => "#EEE685"
      case 3  => "#98FB98"
      case 4  => "#FFC125"
      case 5  => "#AB82FF"
      case 6  => "#B8B8B8"
      case _  => "#FFE1FF"
    }
  }

  def snakeScale(rate:Double,x:Double,y:Double) = {
    ctx.translate(x,y)
    ctx.scale(rate,rate)
    ctx.translate(-x,-y)
  }

  def drawRoundRect(x:Double,y:Double,w:Double,h:Double,r:Double)={
    val pta=Point(x+r,y)
    val ptb=Point(x+w,y)
    val ptc=Point(x+w,y+h)
    val ptd=Point(x,y+h)
    val pte=Point(x,y)
    ctx.beginPath()
    ctx.moveTo(pta.x,pta.y)
    ctx.arcTo(ptb.x,ptb.y,ptc.x,ptc.y,r)
    ctx.arcTo(ptc.x,ptc.y,ptd.x,ptd.y,r)
    ctx.arcTo(ptd.x,ptd.y,pte.x,pte.y,r)
    ctx.arcTo(pte.x,pte.y,pta.x,pta.y,r)
    ctx.fillStyle="#F0FFF0"
    ctx.fill()
  }

  def drawGrid(uid: String, data: GridDataSync,grid: Grid,offsetTime:Long): Unit = {
    //重置画布
//    canvas.width = window.x.toInt
    ctx.clearRect(0, 0, window.x.toInt, window.y.toInt)

    val snakes = data.snakes

    val turnPoints = data.pointDetails
    val apples = grid.grid.filter {
      case (_, Apple(_,_)) => true
      case _ => false
    }.flatMap{
      case (p, Apple(score, life)) => List(Ap(score,life, p.x, p.y))
      case _ => Nil
    }.toList

    val barriers = grid.grid.filter {
      case (_, Barrier(_,_)) => true
      case _ => false
    }.flatMap{
      case (p, Barrier(num, center)) => List(Br(num, p.x, p.y, center))
      case _ => Nil
    }.toList

    val scale =
      if(snakes.exists(_.id == uid)){
        getScale(snakes.filter(_.id == uid).head.length)
      }
      else
        1.0

    //后面画边界的时候
    val reviseScale = offsetTime / Protocol.frameRate.toDouble
    def getDirect(id:String): Point ={
      val reviseDirect = try {
        val dir = snakes.filter(_.id == id).head.direction
        if (grid.nextDirection(id).isDefined) {
          val snake = grid.nextDirection(id).get
          grid.getDirect(dir,snake.x, snake.y) * snakes.filter(_.id == id).head.speed
        } else {
          dir * snakes.filter(_.id == id).head.speed
        }
      }
      catch{
        case e:Exception =>
          Point(0,0)
      }
      reviseDirect * reviseScale
    }

    val Head = try{snakes.filter(_.id == uid).head.header + getDirect(uid)}
    catch{
      case e:Exception =>
        Point(window.x/2,window.y/2)
    }

    //地图偏移修正
    val offX = window.x/2 - Head.x
    val offY = window.y/2 - Head.y
    val curX = Head.x
    val curY = Head.y

    //渲染范围
    val minShowX = curX-(window.x/2.0+20)/scale
    val maxShowX = curX+(window.x/2.0+20)/scale
    val minShowY = curY-(window.y/2.0+20)/scale
    val maxShowY = curY+(window.y/2.0+20)/scale

    def innerJudge(x: Double, y: Double): Boolean = {
      if (minShowX <= x && x < maxShowX && minShowY <= y && y < maxShowY) {
        true
      } else {
        false
      }
    }

    //画蛇
    def drawSnake(tpListMap:Map[String, List[Tp]],offX:Double,offY:Double) = {
      snakes.foreach { s =>
        val id = s.id
        val color = s.color
        val protect = s.protect
        val otherDirect = getDirect(id)
        val start = s.header + otherDirect
        val tail = s.tail
        val speed = s.speed
        val cons = sqrt(pow(s.direction.x,2)+ pow(s.direction.y,2))
        val d = (s.direction.x/cons , s.direction.y/cons) //行动方向的单位向量
        val deg = atan2(d._2,d._1)
        val r = getSnakeRadius(s.length)

        //画蛇身
        ctx.save()
        ctx.lineWidth = 2 * r
        ctx.lineCap = "round"
        ctx.lineJoin = "round"
        ctx.strokeStyle= generateColor(color.toInt)
        ctx.beginPath()
        if(speed > Protocol.startSpeed){
          ctx.shadowBlur = 20 //设置像素模糊值为50；光影效果
          ctx.shadowOffsetX = 0 //横向值为0;
          ctx.shadowOffsetY = 0 //纵向值为0；
          ctx.shadowColor = "yellow"
        }
        if (protect >= 0) {
          ctx.shadowBlur = 20 //设置像素模糊值为50；光影效果
          ctx.shadowOffsetX = 0 //横向值为0;
          ctx.shadowOffsetY = 0 //纵向值为0；
          ctx.shadowColor = "white"
        }
        ctx.moveTo(s.header.x + otherDirect.x + offX, s.header.y + otherDirect.y +offY)
        if (tpListMap.contains(id)) { //有拐点
          val tpList = tpListMap(id)
          val filterList = tpList.reverse.dropRight(1)
          if(filterList.nonEmpty) {
            filterList.foreach { p =>
              ctx.lineTo(p.x + offX, p.y + offY)
            }
          }
          try {
            val direct = Point(tpList.reverse.last.x, tpList.reverse.last.y)
            if (snakes.filter(_.id == id).head.direction == Point(0, 0)) {
              ctx.lineTo(direct.x + offX, direct.y + offY)
              ctx.lineTo(tail.x + offX, tail.y + offY)
            } else {
              val dis = sqrt(pow(direct.x - tail.x, 2) + pow(direct.y - tail.y, 2))
              val moveDis = speed * reviseScale
              if (moveDis < dis) {
                val revise = Point(direct.x - tail.x, direct.y - tail.y)
                ctx.lineTo(direct.x + offX, direct.y + offY)
                ctx.lineTo(tail.x + offX + revise.x * speed * reviseScale / dis, tail.y + offY + revise.y * speed * reviseScale / dis)
              } else {
                val off = moveDis - dis
                val l = tpList.reverse
                val secondPoint = if (tpList.lengthCompare(2) > 0) Point(l(tpList.length - 2).x, l(tpList.length-2).y) else snakes.filter(_.id == id).head.header
                val revise = Point(secondPoint.x - direct.x, secondPoint.y - direct.y)
                val reviseDis = sqrt(pow(secondPoint.x - direct.x, 2) + pow(secondPoint.y - direct.y, 2))
                ctx.lineTo(direct.x + offX + revise.x * off / reviseDis, direct.y + offY + revise.y * off / reviseDis)
              }
            }
            ctx.stroke()
          } catch {
            case e: Exception =>
              println(s"catch exception:$e")
          }
        } else {
          val sp = if(s.direction == Point(0, 0)) 0 else speed
          val direct = s.header
          val dis = sqrt(pow(direct.x - tail.x, 2) + pow(direct.y - tail.y, 2))
          val revise = Point(direct.x - tail.x, direct.y - tail.y)
          ctx.lineTo(tail.x + offX + revise.x * sp * reviseScale /dis, tail.y + offY + revise.y * sp * reviseScale/ dis)
          ctx.stroke()
        }
        ctx.restore()

        //画头和名字
        ctx.font = s"${(20/scale).toInt}px Comic Sans MS"
        if (id == uid) {
          ctx.save()
          ctx.fillStyle=Color.Yellow.toString()
          ctx.fillText(s"${s.name}",window.x/2.0-(15/scale).toInt,window.y/2.0-(40/scale).toInt)
          ctx.translate(window.x/2.0,window.y/2.0)
          ctx.rotate(deg-0.5*Math.PI)
          ctx.drawImage(MyHead,-r* 3.75 /2.0,-r* 1.5,r * 3.75,r * 3.75)
          ctx.restore()
        } else {
          val otherDirect = getDirect(id)
          val x = s.header.x + offX + otherDirect.x
          val y = s.header.y + offY + otherDirect.y
          ctx.save()
          ctx.fillStyle=Color.White.toString()
          ctx.fillText(s"${s.name}",x-(15/scale).toInt,y-(40/scale).toInt)
          ctx.translate(x,y)
          ctx.rotate(deg-0.5*Math.PI)
          ctx.drawImage(OtherHead,-r* 3.75 /2.0,-r* 1.5,r * 3.75,r * 3.75)
          ctx.restore()
        }

      }
    }

    ctx.save()
    snakeScale(scale,window.x/2,window.y/2)

    ctx.drawImage(offCanvas,offX,offY,bounds.x, bounds.y)

    ctx.save()
    apples.filter(ap => innerJudge(ap.x,ap.y)).foreach { case a@Ap(score, _, x, y) =>
      val cacheCanvas = appleCacheMap.getOrElseUpdate(score, generateApple(a))
      ctx.drawImage(cacheCanvas, x + 1 + offX - 25, y + 1 + offY - 25)
    }

    ctx.restore()

    drawSnake(turnPoints.groupBy(_.id),offX,offY)

    ctx.restore()//缩放的保存
    drawSmallMap(uid,snakes,barriers)//缩放不包括小地图、边界、撞到墙上的尸体

  }

}
