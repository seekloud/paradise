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

package org.seekloud.paradise.scene

import java.awt.Rectangle
import java.io.File
import java.util
import javafx.animation.TransitionBuilder
import javafx.scene.effect.DropShadow
//import java.util.Random

import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.embed.swing.SwingFXUtils
import javafx.event.{ActionEvent, EventHandler}
import javafx.geometry.VPos
import javafx.scene.SnapshotParameters
import javafx.scene.canvas.GraphicsContext
import javafx.scene.effect.{BlendMode, Bloom, BoxBlur}
import javafx.scene.image.Image
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import org.seekloud.paradise.ptcl._
import javafx.scene.paint.{Color, Paint}
import scala.util.Random

//import org.seekloud.paradise.controller.GameController._
import javafx.scene.text.{Font, TextAlignment}
import javafx.stage.Stage
import javafx.util.Duration
import javax.imageio.ImageIO
import javafx.scene.{Group, Scene}
import javafx.scene.canvas.Canvas

import org.seekloud.paradise.ptcl._
import org.seekloud.paradise.ptcl.paradise.Protocol.GridDataSync

import java.lang.Math._
import javafx.scene.shape.{StrokeLineCap, StrokeLineJoin}
/**
  * Created by zx0 on 2018/11/14.
  **/
class GameCanvas(canvas: Canvas) {
  val ctx = canvas.getGraphicsContext2D

  private val bounds = Point(Boundary.w, Boundary.h)
  private var window = Point(1000,800)


  private val leftBegin = 10
  private var rightBegin = 750
  private val textLineHeight = 14

  private val Monster = new Image("img/monster.png")
  private val Fire = new Image("img/fire.png")
  private val Stone = new Image("img/yunshi1.png")
  private val MapMy = new Image("img/starYell.png")
  private val King = new Image("img/king.png")

  private val a = new Image("img/background.png")
  private val b = new Image("img/background4.png")
  private val c = new Image("img/background2.png")
  val MyHead = new Image("img/mysnake.png")
  val OtherHead = new Image("img/osnake.png")

  val gameover =new Image("img/gameover.png")
  val win = new Image("img/win.png")
  val dead = new Image("img/dead.png")

  var OutsideColor = "#000"
  var bk = b
  var barrierImg = Fire

  def reSet(x:Double,y:Double) ={
    window = Point(x,y)
    rightBegin = (x-162).toInt
  }

  //try
  def setBk(roomId: Int) = {
    roomId match{
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
  def setDefault(num:Int) = {
    OutsideColor = "#000"
    bk = a
    barrierImg = Monster
  }


  def drawTextLine(str: String, x: Int, lineNum: Int, lineBegin: Int = 0,`type`:Int) = {
    ctx.setTextAlign(TextAlignment.LEFT)
    ctx.setTextBaseline(VPos.TOP)
    `type` match{
      case 3 =>
        ctx.setFont(Font.font("Comic Sans MS",16))
      case _ =>
        ctx.setFont(Font.font("黑体",20))
        ctx.setFill(Color.WHITE)

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

    ctx.setFill(Color.web("rgba(192,192,192,0.6)"))
    ctx.fillRect(x,0,250,40+35*num)
    drawTextLine(s"       $text        ", x+40, index, RankBaseLine-1,2)
    ctx.beginPath()
    ctx.setStroke(Color.WHITE)
    ctx.moveTo(x,30)
    ctx.lineTo(x+250,30)
    ctx.stroke()
    Rank.foreach { score =>
      index += 1
      if (score.id!=id){
        ctx.setFill(Color.WHITE)
      }
      else {
        ctx.setFill(Color.web("#FFFF00"))
      }
      drawTextLine(s"$index:  ${if(score.n.length>5)score.n.take(5)+".." else score.n}    ${score.l}   kill=${score.k}", x+10, index*2, RankBaseLine,3)
    }
    index+=1
    40+35*num
  }

  def drawBarrage(s:String,x:Double,y:Double):Unit={
    ctx.save()
    ctx.setFont(Font.font("Comic Sans Ms",30))
    ctx.setFill(Color.WHITE)
    ctx.fillText(s,x,y)
    ctx.restore()
  }

  // 小地图背景绘制
  def drawSmallMap(myId:String,snakes:List[SkDt],barriers: List[Br]): Unit = {
    val MapSize = 120
    val MapStartX = window.x-MapSize*2.2
    val MapStartY = window.y-MapSize*1.4

    ctx.setFill(Color.web("rgba(192,192,192,0.6)"))

    ctx.fillRect(MapStartX,MapStartY,MapSize*2,MapSize)

    ctx.setFill(Color.web("rgba(255,0,0,0)"))
    barriers.filter(_.center).filter(br=> br.x>0 && br.x < bounds.x && br.y>0 && br.y < bounds.y).foreach{
      case Br(_,x,y,_) =>
        val MapX = x/bounds.x*MapSize*2
        val MapY = y/bounds.y*MapSize
        val color = Color.rgb(255,0,0)
        ctx.beginPath()
        ctx.setFill(color)
        ctx.fillOval(MapStartX+MapX-2,MapStartY+MapY-2,4,4)
        ctx.fill()
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
          val color = Color.rgb(255,255,255)
          ctx.beginPath()
          ctx.setFill(color)
          ctx.fillOval(MapStartX+MapX-4,MapStartY+MapY-4,8,8)
          ctx.fill()
        }
      }
    }
  }

  def drawOffMap()={
    //地图绘制
    ctx.drawImage(bk,0,0,window.x,window.y)
    //边界绘制
    ctx.setStroke(Color.web("#ccc"))
    ctx.setLineWidth(20)
    ctx.rect(0,0,bounds.x.toInt,bounds.y.toInt)
    ctx.stroke()
  }
  def drawCache(offx: Double, offy: Double): Unit = { //离屏缓存的更新--缓存边界
    //    ctx.clearRect(0,0,canvas.getWidth,canvas.getHeight)
    ctx.setFill(Color.web("#ccc"))
//    //画边界
    ctx.fillRect(offx, offy-20, bounds.x, 20)
    ctx.fillRect(offx - 20, offy, 20, bounds.y)
    ctx.fillRect(offx, bounds.y + offy , bounds.x, 20)
    ctx.fillRect(bounds.x + offx - 20, offy, 20, bounds.y)
  }
  def drawOffBarrier(barriers:List[Br]) ={
    ctx.beginPath()
    barriers.filter(br=> br.center).foreach{case Br(_,x,y,_) =>
      ctx.drawImage(barrierImg,x,y,16,16)
    }
  }

  def drawWin() = {
    ctx.drawImage(win,0,0,window.x,window.y)
    ctx.setFill(Color.RED)
    ctx.fillText("Congratulations On Your Victory!",window.x*0.13,window.y*0.09)
    ctx.fillText("Press Space Key To Start A NEW GAME!", window.x*0.13, window.y*0.13)
  }

  def drawVic(vicSnake:String) = {
    ctx.drawImage(gameover,0,0,window.x,window.y)
    ctx.setFill(Color.BLACK)
    ctx.setFont(Font.font("Helvetica",30))
    ctx.fillText(s"Congratulations to $vicSnake for victory!", window.x*0.13, window.y*0.14)
    ctx.fillText("Press Space Key To Start A NEW GAME!", window.x*0.13, window.y*0.23)
  }

  def drawWait(firstCome:Boolean,killer:String,finalLength:Int,finalKill:Int) = {
    ctx.save()
    ctx.setFill(Color.web("#ccc")) //Color.Black.toString()
    ctx.fillRect(0, 0, window.x , window.y )
    ctx.setFont(Font.font("Helvetica",30))
    if(firstCome) {
      ctx.setFill(Color.BLACK)
      ctx.fillText("Please wait. or Refresh to change your Room", window.x*0.13, window.y*0.26)
    }else{
      ctx.setFill(Color.web("#CD3700"))
      ctx.fillText(s"You Dead, Press Space Key To Revenge!", window.x*0.3, window.y*0.16)
      drawRoundRect(window.x*0.3,window.y*0.28,window.x/3,window.y/4,30)
      ctx.drawImage(dead,window.x*0.65,window.y*0.37,window.x*0.1,window.x*0.1)
      ctx.setFont(Font.font("Comic Sans MS",window.x*0.02))
      ctx.setFill(Color.web("#EE9A00"))
      ctx.fillText(s"The   Killer  Is    :", window.x*0.32, window.y*0.3)
      ctx.fillText(s"Your  Final   Length:", window.x*0.32, window.y*0.37)
      ctx.fillText(s"Your  Final   Kill  :", window.x*0.32, window.y*0.44)
      ctx.setFill(Color.BLACK)
      ctx.fillText(s"$killer",window.x*0.56,window.y*0.3)
      ctx.fillText(s"$finalLength",window.x*0.56,window.y*0.37)
      ctx.fillText(s"$finalKill",window.x*0.56,window.y*0.44)
    }
    ctx.restore()
  }

  def drawGameOff(firstCome:Boolean): Unit = {

    ctx.setFill(Color.web("#ccc"))//Color.Black.toString()
    ctx.fillRect(0, 0, window.x , window.y )
    ctx.setFill(Color.web("rgb(0, 0, 0)"))
    ctx.setFont(Font.font("Helvetica",36))
    if (firstCome) {
      ctx.fillText("Welcome.", window.x*0.13, window.y*0.26)
    } else {
      ctx.fillText("Ops, connection lost.", window.y*0.13, window.y*0.26)
    }
  }

  def generateColor(num:Int):Paint = {
    num match {
      case 0 => Color.web("#FFE1FF")
      case 1 => Color.web("#FFC125")
      case 2  => Color.web("#EEE685")
      case 3  => Color.web("#98FB98")
      case 4  => Color.web("#FFC125")
      case 5  => Color.web("#AB82FF")
      case 6  => Color.web("#B8B8B8")
      case _  => Color.web("#FFE1FF")
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
    ctx.setFill(Color.web("#F0FFF0"))
    ctx.fill()
  }

  def drawGrid(uid: String, data: GridDataSync,grid: Grid,offsetTime:Long): Unit = {
    //重置画布
    //    canvas.width = window.x.toInt
    ctx.clearRect(0, 0, window.x.toInt, window.y.toInt)
    ctx.setFill(Color.web(OutsideColor))
    ctx.fillRect(0, 0 ,window.x,window.y)
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
//    drawBackground()
//    drawOffMap()
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
        val tail = s.tail
        val speed = s.speed
        val cons = sqrt(pow(s.direction.x,2)+ pow(s.direction.y,2))
        val d = if(cons != 0.0)(s.direction.x/cons , s.direction.y/cons) else (0.0,0.0) //行动方向的单位向量
        val deg = atan2(d._2,d._1)
        val r = getSnakeRadius(s.length)

        //画蛇身
        ctx.save()
        ctx.setLineWidth(2 * r)
        ctx.setLineCap(StrokeLineCap.ROUND)
        ctx.setLineJoin(StrokeLineJoin.ROUND)
        ctx.setStroke(generateColor(color.toInt))
        ctx.beginPath()
        if(speed > Protocol.startSpeed){
          ctx.setEffect(new DropShadow(10 * scale, Color.YELLOW))	//设置像素模糊值为50；光影效果
        }
        if (protect >= 0) {
          ctx.setEffect(new DropShadow(10 * scale, Color.WHITE))
        }
        ctx.moveTo(s.header.x + otherDirect.x + offX -2*r , s.header.y + otherDirect.y +offY )
        if (tpListMap.contains(id)) { //有拐点
          val tpList = tpListMap(id)
          val filterList = tpList.reverse.dropRight(1)
          if(filterList.nonEmpty) {
            filterList.foreach { p =>
              ctx.lineTo(p.x + offX -2*r , p.y  + offY)
            }
          }
          try {
            val direct = Point(tpList.reverse.last.x, tpList.reverse.last.y)
            if (snakes.filter(_.id == id).head.direction == Point(0, 0)) {
              ctx.lineTo(direct.x + offX -2*r , direct.y + offY)
              ctx.lineTo(tail.x + offX -2*r , tail.y + offY )
            } else {
              val dis = sqrt(pow(direct.x - tail.x, 2) + pow(direct.y - tail.y, 2))
              val moveDis = speed*reviseScale
              if (moveDis < dis) {
                val revise = Point(direct.x - tail.x, direct.y - tail.y)
                ctx.lineTo(direct.x + offX -2*r , direct.y + offY )
                ctx.lineTo(tail.x + offX -2*r  + revise.x * speed*reviseScale / dis, tail.y + offY  + revise.y * speed*reviseScale / dis)
              } else {
                val off = moveDis - dis
                val l = tpList.reverse
                val secondPoint = if (tpList.lengthCompare(2) > 0) Point(l(tpList.length - 2).x, l(tpList.length-2).y) else snakes.filter(_.id == id).head.header
                val revise = Point(secondPoint.x - direct.x, secondPoint.y - direct.y)
                val reviseDis = sqrt(pow(secondPoint.x - direct.x, 2) + pow(secondPoint.y - direct.y, 2))
                ctx.lineTo(direct.x + offX -2*r  + revise.x * off / reviseDis, direct.y + offY + revise.y * off / reviseDis)
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
          ctx.lineTo(tail.x + offX -2*r + revise.x * sp * reviseScale /dis, tail.y + offY + revise.y * sp * reviseScale / dis)
          ctx.stroke()
        }
        ctx.restore()

        //画头和名字
        ctx.setFont(Font.font("Comic Sans MS",15))
        if (id == uid) {
          ctx.save()
          ctx.setFill(Color.YELLOW)
          ctx.fillText(s"${s.name}",window.x/2.0-(15/scale).toInt,window.y/2.0-(40/scale).toInt)
          ctx.translate(window.x/2.0 -2*r ,window.y/2.0 )
          ctx.rotate(90*(2*deg/Math.PI-1))//此处值应该是度数
          ctx.drawImage(MyHead,-r* 3.75 /2.0 ,-r* 1.5 ,r * 3.75,r * 3.75)
          ctx.restore()
        } else {
          val otherDirect = getDirect(id)
          val x = s.header.x + offX + otherDirect.x
          val y = s.header.y + offY + otherDirect.y
          ctx.save()
          ctx.setFill(Color.WHITE)
          ctx.fillText(s"${s.name}",x-(15/scale).toInt,y-(40/scale).toInt)
          ctx.translate(x -2*r ,y )
          ctx.rotate(90*(2*deg/Math.PI-1))//此处值应该是度数
          ctx.drawImage(OtherHead,-r* 3.75 /2.0 ,-r* 1.5 ,r * 3.75,r * 3.75)
          ctx.restore()
        }

      }
    }

    ctx.save()
//    snakeScale(scale,window.x/2,window.y/2)


    ctx.drawImage(bk,offX,offY,bounds.x, bounds.y)
    val r = if(snakes.exists(_.id == uid)){
      getSnakeRadius(snakes.filter(_.id == uid).head.length)
    }
    else
      0

    barriers.filter(br=> br.center).foreach{case Br(_,x,y,_) =>
      ctx.drawImage(barrierImg,offX+x - 2 * r,offY+y,16,16)
    }

    ctx.save()
    apples.filter(ap => innerJudge(ap.x,ap.y)).foreach { case a@Ap(score, _, x, y) =>
      val color = score match {
        case 1 => Color.CYAN
        case 6 => Color.rgb(255, 255, 0)
        case 4 => Color.rgb(30,144,255)
        case _ => Color.rgb(255, 79, 0)
      }

      ctx.beginPath()
      ctx.setFill(color)
      ctx.setEffect(new DropShadow(5 * scale, color))	//设置像素模糊值为50；光影效果
      ctx.fillOval( x + 1 + offX - 25, y + 1 + offY - 25, 8,8)
      ctx.fill()
    }

    ctx.restore()

    drawCache(offX,offY)
    ctx.restore()

    drawSnake(turnPoints.groupBy(_.id),offX,offY)

    ctx.restore()//缩放的保存
    drawSmallMap(uid,snakes,barriers)//缩放不包括小地图、边界、撞到墙上的尸体

  }

}


