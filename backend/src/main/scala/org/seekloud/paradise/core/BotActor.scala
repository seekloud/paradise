package org.seekloud.paradise.core

import java.awt.event.KeyEvent

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.paradise.core.RoomActor.FromUserActor
import org.seekloud.paradise.ptcl.PDGameEvent.MouseEvent
import org.seekloud.paradise.ptcl._
import org.seekloud.paradise.ptcl.paradise.Protocol.{Key, MM}
import org.seekloud.paradise.snake.GridOnServer
import org.slf4j.LoggerFactory

import concurrent.duration._
import scala.math.{abs, atan2, cos, sin}
import scala.util.Random

/**
  * Created by Anjiansan on 2019/01/08.
  **/
object BotActor {
  //简单的bot：在某个房间生成后生成陪玩bot
  private val log = LoggerFactory.getLogger(this.getClass)

  sealed trait Command

  case class InitInfo(botName: String, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command]) extends Command

  case class MakeAction(a: Int) extends Command

  case object KillBot extends Command

  case object BotDead extends Command

  case object Space extends Command

  case object BackToGame extends Command

  private final case object MakeActionKey

  private final case object SpaceKey

  case object CheckRush extends Command

  private final case object CheckRushKey

  def create(botId: String): Behavior[Command] = {
    implicit val stashBuffer: StashBuffer[Command] = StashBuffer[Command](Int.MaxValue)
    Behaviors.withTimers[Command] { implicit timer =>
      Behaviors.receive[Command] { (ctx, msg) =>
        msg match {
          case InitInfo(botName, grid, roomActor) =>
            val frameRate = Protocol.frameRate
            roomActor ! RoomActor.JoinRoom4Bot(botId, botName, ctx.self)
            val randomTime = 1 + scala.util.Random.nextInt(20)
            timer.startSingleTimer(MakeActionKey, MakeAction(0), randomTime * frameRate.millis)
            gaming(botId, grid, roomActor, frameRate)

          case unknownMsg@_ =>
            log.warn(s"${ctx.self.path} unknown msg: $unknownMsg")
            stashBuffer.stash(unknownMsg)
            Behaviors.unhandled
        }
      }
    }
  }

  def gaming(botId: String, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command], frameRate: Int)
            (implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case MakeAction(a) =>
          val rTime = 1 + scala.util.Random.nextInt(15)
          timer.startSingleTimer(MakeActionKey, MakeAction(rTime), (rTime + Random.nextInt(a + 1)) * frameRate.millis)
          timer.startSingleTimer(CheckRushKey, CheckRush, 1 * frameRate.millis)
          var x = if(scala.util.Random.nextBoolean()) scala.util.Random.nextDouble() else -1 * scala.util.Random.nextDouble()
          var y = if(scala.util.Random.nextBoolean()) scala.util.Random.nextDouble() else -1 * scala.util.Random.nextDouble()
          val snake = grid.snakes.find(_._1 == botId)
          if(snake.nonEmpty)
            if (checkRush(getDirect(snake.get._2.direction, x * 10, y * 10), snake.get._2, grid, true)) {
              x = -1 * x
              y = -1 * y
            }
          roomActor ! FromUserActor(MM(botId, x * 10, y * 10, grid.frameCount + 1))
          gaming(botId, grid, roomActor, frameRate)

        case CheckRush =>
          val snake = grid.snakes.find(_._1 == botId)
          if(snake.nonEmpty) {
            var x = snake.get._2.direction.x
            var y = snake.get._2.direction.y
            if (checkRush(snake.get._2.direction, snake.get._2, grid, false)) {
//              log.info(s"$botId direction=$x,$y")
              x = -1 * x
              y = -1 * y
              roomActor ! FromUserActor(MM(botId, x, y, grid.frameCount + 1))
            }
          }
          timer.startSingleTimer(CheckRushKey, CheckRush, 1 * frameRate.millis)
          gaming(botId, grid, roomActor, frameRate)

        case BotDead =>
          log.info(s"bot dead:$botId")
          timer.startSingleTimer(SpaceKey, Space, (2 + scala.util.Random.nextInt(8)) * frameRate.millis)
          timer.cancel(MakeActionKey)
          timer.cancel(CheckRush)
          dead(botId, grid, roomActor, frameRate)

        case KillBot =>
          log.debug(s"botActor:$botId go to die...")
          Behaviors.stopped

        case unknownMsg@_ =>
          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg,be 111")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  def dead(botId: String, grid: GridOnServer, roomActor: ActorRef[RoomActor.Command], frameRate: Int)
          (implicit stashBuffer: StashBuffer[Command], timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case Space =>
          log.info(s"recv Space: botId:$botId")
          roomActor ! FromUserActor(Key(botId, KeyEvent.VK_SPACE, grid.frameCount + 1))
          Behaviors.same

        case BackToGame =>
          log.info(s"back to game: botId:$botId")
          val randomTime = 2 + scala.util.Random.nextInt(20)
          timer.startSingleTimer(MakeActionKey, MakeAction(0), randomTime * frameRate.millis)
          gaming(botId, grid, roomActor, frameRate)

        case KillBot =>
//          log.debug(s"botActor:$botId go to die...")
          Behaviors.stopped

        case unknownMsg@_ =>
          log.warn(s"${ctx.self.path} unknown msg: $unknownMsg,be 222")
          stashBuffer.stash(unknownMsg)
          Behaviors.unhandled
      }
    }
  }

  //根据鼠标方向获取方向的单位向量
  def getDirect(direction:Point,x:Double,y:Double)={
    val deg1 = atan2( direction.y ,direction.x )
    val deg = atan2(y ,x )//鼠标位置相对于头的角度

    if( abs(deg - deg1) <= Protocol.maxAngle || abs(2 * Math.PI - abs(deg - deg1)) <= Protocol.maxAngle || direction == Point(0,0)){//与原本方向的差值
    val degX = if(cos(deg).isNaN) 0 else cos(deg)
      val degY = if(sin(deg).isNaN) 0 else sin(deg)
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

  def judgefield(slength:Int,range:Double,a:Point,b:Point)={
    if (a.x <= b.x + getSnakeRadius(slength) + range && a.x >= b.x - getSnakeRadius(slength) - range && a.y <= b.y +getSnakeRadius(slength) + range && a.y >= b.y - getSnakeRadius(slength) - range)
      true
    else
      false
  }

  def checkRush(newDirection:Point, snake:SkDt, grid: GridOnServer, checkHuman: Boolean): Boolean = {
    val newHeader = snake.header + newDirection * snake.speed

    grid.grid.filter { case (p, _) =>
      p match {
        case a if judgefield(snake.length,60,a,newHeader) => true
        case _ => false
      }
    case _ => false
    }.map {
      case (p,Barrier(_,_))=>
        true
      case (p, Body(id, _,_,_)) if checkHuman =>
        true
      case x =>
        false
    }.toList.contains(true)
  }

}
