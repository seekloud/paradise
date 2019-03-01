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

package org.seekloud.paradise.core

/**
  * User: easego
  * Date: 2018/11/1
  * Time: 12:13
  */

import scala.concurrent.ExecutionContext.Implicits.global

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.actor.typed.scaladsl.AskPattern._
import akka.stream.scaladsl.Flow
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.util.Random
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.Boot.{esheepSyncClient, roomManager}
import org.seekloud.paradise.core.GameReply.{InitReplay, StopReplay}
import org.seekloud.paradise.core.RoomActor.{FromUserActor, LeftRoom4Watch, UserActorTDead}
import org.seekloud.paradise.core.RoomManager.{GetUserInRoom, PlayerJoinRoomReq, WatcherJoinRoomReq}
import org.seekloud.paradise.models.DAO.RecordDAO.{getInfoByUserId, getRecordById}

import scala.concurrent.Future

object UserActor {

  private val log = LoggerFactory.getLogger(this.getClass)
  private case object BehaviorChangeKey

  sealed trait Command
  case class TimeOut(msg: String) extends Command
  case class FromFront(orderOpt: Option[MsgFromFront]) extends Command
  case class StreamCompleteMsg(userId: String) extends Command
  case class StreamFailedMsg(ex: Throwable) extends Command
  case class FrontActor(frontActor: ActorRef[MsgFromBackend]) extends Command
  case class FrontActorDead(frontActor: ActorRef[MsgFromBackend]) extends Command
  case class PlayerStartGame(roomId: Long) extends Command
  case class WatcherStart(roomId:Long,watcherId:String,playerId:String) extends Command
  case class WatchRecordStart(watchedId: String,watchId: String,watchName: String,recordId: Long,frame: Int) extends Command
  case class GameReplyDead(recordId: Long,playerId: String,actor: ActorRef[GameReply.Command]) extends Command
  case class RoomInfoRsp(roomId: Long,roomActor: ActorRef[RoomActor.Command]) extends Command
  case class RoomInfoTRsp(watcherId: String,watchedId: String,roomId: Long,roomActor: ActorRef[RoomActor.Command]) extends  Command
  case class FromRoomActor(gameMessage: GameMessage) extends Command
  case object EnterRecordReplyBusy extends Command
  case object ChangeBehaviorToInit extends Command
  case class OutActorDead(outActor: ActorRef[MsgFromBackend]) extends Command

  case class GetRecordFrame(playerId: String, replyTo: ActorRef[CommonRsp]) extends Command

  private[this] def switchBehavior( ctx: ActorContext[Command],
    behaviorName: String,
    behavior: Behavior[Command],
    durationOpt: Option[FiniteDuration] = None,
    timeOut: TimeOut  = TimeOut("busy time error"))
    (implicit stashBuffer: StashBuffer[Command], timer:TimerScheduler[Command]) = {
//    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }

  def create(userId: String,name: String):Behavior[Command] = {
    log.debug(s"userActor-$userId-$name is starting...")
    Behaviors.setup[Command]{
      ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        Behaviors.withTimers[Command]{implicit timer =>
          switchBehavior(ctx,"init",init(userId,name))
        }
    }
  }

  def init(userId: String,name: String)(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {

    Behaviors.receive[Command]{
      (ctx,msg) =>
        msg match{

          case FrontActor(outActor) =>
            ctx.watchWith(outActor,OutActorDead(outActor))
            switchBehavior(ctx,"idle",idle(userId,name,outActor))

          case OutActorDead(outActor) =>
//          流刚建立时关闭流，对应userActor直接死亡
            ctx.unwatch(outActor)
            log.debug(s"userActor-$userId-$name is stopping...")
            Behaviors.stopped

          case GameReplyDead(_,_,actor) =>
            ctx.unwatch(actor)
            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} init state get unknown msg: $unknow")
            stashBuffer.stash(unknow)
            Behaviors.same


        }
    }

  }

  def idle(userId: String,name: String,outActor: ActorRef[MsgFromBackend])(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]) = {
    Behaviors.receive[Command]{
      (ctx,msg) =>
        msg match {
          case PlayerStartGame(roomId) =>
            roomManager ! PlayerJoinRoomReq(userId,name,ctx.self,roomId)
            Behaviors.same

          case WatcherStart(roomId,watcherId,playerId) =>
            roomManager ! WatcherJoinRoomReq(watcherId,playerId,roomId,ctx.self)
            Behaviors.same

          case WatchRecordStart(watchedId,watchId,watchName,recordId,frame) =>
            val gameReply = getGameReplyer(ctx,recordId,watchedId)
            ctx.self ! EnterRecordReplyBusy
            switchBehavior(ctx,"reply",reply(watchedId,watchId,watchName,recordId,frame,outActor,gameReply))

          case RoomInfoRsp(roomId,roomActor) =>
            log.debug(s"user-$userId-$name join room-$roomId success")
            switchBehavior(ctx,"play",play(userId,name,outActor,roomId,roomActor))

          case RoomInfoTRsp(watcherId,watchedId,roomId,roomActor) =>
            log.debug(s"user-$userId-$name watch room-$roomId success")
            switchBehavior(ctx,"watch",watch(watcherId,name,watchedId,outActor,roomId,roomActor))

          case ChangeBehaviorToInit=>
            outActor ! InitErrorMessage
            ctx.unwatch(outActor)
            switchBehavior(ctx,"init",init(userId,name))

          case OutActorDead(outActorT) =>
            ctx.unwatch(outActorT)
            log.debug(s"userActor-$userId-$name is stopping...")
            Behaviors.stopped


          case GameReplyDead(_,_,actor) =>
            ctx.unwatch(actor)
            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} idle state get unknown msg: $unknow")
            stashBuffer.stash(unknow)
            Behaviors.same

        }
    }
  }

  def play(userId: String,
    userName: String,
    outActor: ActorRef[MsgFromBackend],
    roomId: Long,
    roomActor: ActorRef[RoomActor.Command]
  )(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {

    Behaviors.receive[Command]{
      (ctx,msg) =>
        msg match{
          case FromRoomActor(gameMessage) =>
            //收到来自roomActor的发往前端的消息,直接转发前端
            //            log.debug(s"get msg from room-$roomId: $gameMessage")
            outActor ! gameMessage
            Behaviors.same

          case FromFront(msgFromFrontOpt) =>
            //收到来自outActor的发往房间里的消息,转发给对应的roomActor
            msgFromFrontOpt match{
              case None =>
              case Some(msgFromFront) =>
                roomActor ! FromUserActor(msgFromFront)

            }
            Behaviors.same

          case ChangeBehaviorToInit=>
            outActor ! InitErrorMessage
            ctx.unwatch(outActor)
            //通知roomActor
            roomActor ! UserActorTDead(userId,userName,ctx.self)
            switchBehavior(ctx,"init",init(userId,userName))

          case OutActorDead(outActorT) =>
            ctx.unwatch(outActorT)
            log.debug(s"userActor-$userId-$userName is stopping...")
            Behaviors.stopped

          case GameReplyDead(_,_,actor) =>
            ctx.unwatch(actor)
            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} get unknown msg: $unknow")
            Behaviors.same

        }
    }

  }

  def watch(watcherId: String,
    watcherName: String,
    watchedId: String,
    outActor: ActorRef[MsgFromBackend],
    roomId: Long,
    roomActor: ActorRef[RoomActor.Command]
  )(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {

    Behaviors.receive[Command]{
      (ctx,msg) =>
        msg match{
          case FromRoomActor(gameMessage) =>
            //收到来自roomActor的发往前端的消息,直接转发前端
//            log.debug(s"get msg from room-$roomId: $gameMessage when watch")
            outActor ! gameMessage
            Behaviors.same

          case FromFront(msgFromFrontOpt) =>
            //收到来自outActor的发往房间里的消息,转发给对应的roomActor
            msgFromFrontOpt match{
              case None =>
              case Some(msgFromFront) =>
                roomActor ! FromUserActor(msgFromFront)

            }
            Behaviors.same

          case ChangeBehaviorToInit=>
            outActor ! InitErrorMessage
            ctx.unwatch(outActor)
            //通知roomActor
            roomActor ! LeftRoom4Watch(watcherId,watchedId,ctx.self)
            switchBehavior(ctx,"init",init(watcherId,watcherName))

          case OutActorDead(outActorT) =>
            ctx.unwatch(outActorT)
            log.debug(s"userActor-$watcherId-$watcherName is stopping...")
            Behaviors.stopped

          case GameReplyDead(_,_,actor) =>
            ctx.unwatch(actor)
            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} get unknown msg: $unknow")
            Behaviors.same

        }
    }

  }

  def reply(watchedId: String,
    watchId: String,
    watchName: String,
    recordId: Long,
    frame: Int,
    outActor: ActorRef[MsgFromBackend],
    gameReply: ActorRef[GameReply.Command]
  )(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {

    Behaviors.receive[Command] {
      (ctx,msg) =>
        msg match {
          case EnterRecordReplyBusy =>
            log.debug(s"${ctx.self} get msg: $msg----------------------------------------")
            gameReply ! InitReplay(outActor,watchedId,frame)
            Behaviors.same

          case FromFront(msgFromFrontOpt) =>
            //收到来自outActor的消息
            msgFromFrontOpt match{
              case None =>
              case Some(msgFromFront) =>
                msgFromFront match {
                  case TickTock =>
                      //收到前端定时信息

                  case _ =>
                }

            }
            Behaviors.same

          case ChangeBehaviorToInit=>
            log.debug("------- get changeBehaviorToInit")
            outActor ! InitErrorReply
            ctx.unwatch(outActor)
            //通知对应播放器actor
            gameReply ! StopReplay
            switchBehavior(ctx,"init",init(watchId,watchName))

          case OutActorDead(outActorT) =>
            ctx.unwatch(outActorT)
            log.debug(s"userActor-$watchId-$watchName is stopping...")
            Behaviors.stopped

          case GameReplyDead(_,_,actor) =>
            ctx.unwatch(actor)
            Behaviors.same

          case GetRecordFrame(playerId, replyTo) =>
            gameReply ! GameReply.GetRecordFrame(playerId, replyTo)
            Behavior.same
          case unknow =>
            log.debug(s"${ctx.self} reply state get unknown msg: $unknow")
            Behaviors.same
        }
    }

  }

  private def getGameReplyer(ctx: ActorContext[Command],recordId: Long,playerId: String): ActorRef[GameReply.Command] = {

    val childName = s"gameReply-$recordId-$playerId"
    ctx.child(childName).getOrElse{
      val actor = ctx.spawn(GameReply.create(recordId,playerId),childName)
      ctx.watchWith(actor,GameReplyDead(recordId,playerId,actor))
      actor
    }.upcast[GameReply.Command]

  }

}


