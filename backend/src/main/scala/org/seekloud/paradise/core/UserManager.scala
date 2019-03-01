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
  * Date: 2018/10/31
  * Time: 15:53
  */
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.actor.typed.scaladsl.AskPattern._
import akka.stream.scaladsl.Flow
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import akka.stream.{ActorAttributes, OverflowStrategy, Supervision}
import org.seekloud.paradise.core.RoomManager.RoomActorDead
import org.seekloud.paradise.core.UserActor._
import org.seekloud.paradise.ptcl.Protocol
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.util.{Left, Random, Right}
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.byteobject.ByteObject._
import org.seekloud.byteobject.MiddleBufferInJvm

import scala.collection.mutable


object UserManager {

  private val log = LoggerFactory.getLogger(this.getClass)
  private case object BehaviorChangeKey

  sealed trait Command
  case class TimeOut(msg: String) extends Command
  case class PlayerJoinReq(userId: String,userName: String,roomId: Long,ref: ActorRef[PlayerJoinRsp]) extends Command
  case class PlayerJoinRsp(flow: Flow[Message,Message,Any]) extends Command
  case class WatcherJoinReq(playerId:String,watcherId: String,watcherName:String,roomId: Long,ref: ActorRef[WatcherJoinRsp]) extends Command
  case class WatcherJoinRsp(flow: Flow[Message,Message,Any]) extends Command
  case class UserActorDead(userId: String, userName: String, userActor: ActorRef[UserActor.Command]) extends Command
  case class PlayerWatchRecordReq(watchId: String,watchedId: String,playerName: String,recordId: Long,frame: Int,ref: ActorRef[PlayerWatchRecordRsp]) extends Command
  case class PlayerWatchRecordRsp(flow: Flow[Message,Message,Any]) extends Command
  case class GetRecordFrame(recordId: Long, playerId: String, replyTo: ActorRef[CommonRsp]) extends Command

  //数据统计
  val statics = mutable.HashMap[String, Double](
    "newSnake" -> 0.0,
    "syncAfterDead" ->0.0,
    "sync" -> 0.0,
    "left" -> 0.0,
    "mouseMove"-> 0.0,
    "speed"-> 0.0,
    "kill" -> 0.0,
    "feed" -> 0.0,
    "eat" -> 0.0,
    "apple"-> 0.0,
    "remove" -> 0.0,
    "rank" -> 0.0,
    "ping"-> 0.0,
    "others" -> 0.0
  )
  var timer = System.currentTimeMillis()
  val period = 30 * 1000

  private[this] def switchBehavior( ctx: ActorContext[Command],
    behaviorName: String,
    behavior: Behavior[Command],
    durationOpt: Option[FiniteDuration] = None,
    timeOut: TimeOut  = TimeOut("busy time error"))
    (implicit stashBuffer: StashBuffer[Command], timer:TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }

  def create():Behavior[Command] = {
    log.debug("userManager is starting...")
    Behaviors.setup{
      ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        Behaviors.withTimers[Command]{ implicit timer =>
          val userIdGenerator = new AtomicInteger(100)
          switchBehavior(ctx,"idle",idle(userIdGenerator))
        }
    }
  }

  def idle(userIdGenerator: AtomicInteger)(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]):Behavior[Command] = {
    Behaviors.receive[Command] {
      (ctx,msg) =>
        msg match {

          case PlayerJoinReq(userId,userName,roomId,ref) =>
            log.debug(s"${ctx.self} receive msg: $msg")
            getUserActorOpt(ctx,userId) match {
              case Some(userActorT) =>
                userActorT ! ChangeBehaviorToInit

              case None =>
            }
            val userActor = getUserActor(ctx,userId,userName)
            val flow = createFlow(userId,userActor)
            ref ! PlayerJoinRsp(flow)
            userActor ! PlayerStartGame(roomId)
            Behaviors.same

          case WatcherJoinReq(playerId,watcherId,watcherName,roomId,ref) =>
            log.debug(s"${ctx.self} receive msg: $msg")
            getUserActorOpt(ctx,watcherId) match {
              case Some(userActorT) =>
                userActorT ! ChangeBehaviorToInit

              case None =>
            }
            val userActor = getUserActor(ctx,watcherId,watcherName)
            val flow = createFlow(watcherId,userActor)
            ref ! WatcherJoinRsp(flow)
            userActor ! WatcherStart(roomId,watcherId,playerId)
            Behaviors.same

          case PlayerWatchRecordReq(watchId,watchedId,watchName,recordId,frame,ref) =>
            log.debug(s"${ctx.self} receive msg: $msg")
            getUserActorOpt(ctx,watchId) match {
              case Some(userActorT) =>
                userActorT ! ChangeBehaviorToInit

              case None =>

            }
            val userActor = getUserActor(ctx,watchId,watchName)
            val flow = createFlow(watchId,userActor)
            ref ! PlayerWatchRecordRsp(flow)
            userActor !  WatchRecordStart(watchedId,watchId,watchName,recordId,frame)
            Behaviors.same

          case UserActorDead(userId,userName,userActor) =>
            log.debug(s"userManager get userActor dead msg from user-$userId-$userName")
            ctx.unwatch(userActor)
            Behaviors.same

          case GetRecordFrame(recordId, playerId, replyTo) =>
            getUserActor(ctx, playerId, playerId) ! UserActor.GetRecordFrame(playerId, replyTo)
            Behavior.same

          case unknow =>
            log.debug(s"${ctx.self} get unknown msg: $unknow")
            Behaviors.same

        }
    }
  }

  private def getUserActor(ctx: ActorContext[Command],userId: String,name: String): ActorRef[UserActor.Command] = {

    val childName = s"userActor-$userId"
    ctx.child(childName).getOrElse{
      val actor = ctx.spawn(UserActor.create(userId,name),childName)
      ctx.watchWith(actor,UserActorDead(userId,name,actor))
      actor
    }.upcast[UserActor.Command]

  }

  private def getUserActorOpt(ctx: ActorContext[Command],userId:String): Option[ActorRef[UserActor.Command]] = {
    val childName = s"userActor-$userId"
    ctx.child(childName).map(_.upcast[UserActor.Command])
  }

  def playInSink(userId: String,actor: ActorRef[UserActor.Command]) = ActorSink.actorRef[UserActor.Command](
    ref = actor,
    onCompleteMessage = StreamCompleteMsg(userId),
    onFailureMessage = StreamFailedMsg.apply
  )

  def flow(userId: String,userActor: ActorRef[UserActor.Command]): Flow[UserActor.Command, MsgFromBackend, Any] = {

    val in =
      Flow[UserActor.Command]
        .to(playInSink(userId,userActor))

    val out =
      ActorSource.actorRef[MsgFromBackend](
        completionMatcher = {
          case CompleteMsg ⇒
        },
        failureMatcher = {
          case FailMsg(e) ⇒ e
        },
        bufferSize = 64,
        overflowStrategy = OverflowStrategy.dropHead
      ).mapMaterializedValue(outActor => userActor ! FrontActor(outActor))

    Flow.fromSinkAndSource(in, out)
  }

  import akka.util.ByteString

  def createFlow(userId: String,userActor: ActorRef[UserActor.Command]): Flow[Message, Message, Any] = {

    import scala.language.implicitConversions

    implicit def parseJsonString2MsgFromFront(s:String): Option[MsgFromFront] = {
      import io.circe.generic.auto._
      import io.circe.parser._

      try {
        val wsMsg = decode[MsgFromFront](s).right.get
        Some(wsMsg)
      }catch {
        case e:Exception =>
          log.warn(s"parse front msg failed when json parse,s=${s}")
          None
      }
    }

    Flow[Message]
      .collect {
        case TextMessage.Strict(msg) =>
          FromFront(msg)

        case BinaryMessage.Strict(bMsg) =>
          //decode process.
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          val msg =
            bytesDecode[MsgFromFront](buffer) match {
              case Right(v) => FromFront(Some(v))
              case Left(e) =>
                println(s"decode error: ${e.message}")
                FromFront(None)
            }
          msg
      }

      .via(flow(userId,userActor))

      .map {
        case t: GameMessage =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          val buff = new MiddleBufferInJvm(409600)
          val msg = t.fillMiddleBuffer(buff).result()
          t match{
            case _: NewSnakeJoined =>
              statics.update("newSnake", statics("newSnake") + msg.length.toDouble/1024)
            case _: SyAD =>
              statics.update("syncAfterDead", statics("syncAfterDead") + msg.length.toDouble/1024)
            case _: GridDataSync =>
              statics.update("sync", statics("sync") + msg.length.toDouble/1024)
            case _: SnakeLeft =>
              statics.update("left", statics("left") + msg.length.toDouble/1024)
            case _: SA =>
              statics.update("mouseMove", statics("mouseMove") + msg.length.toDouble/1024)
            case _: SnakeSpeed =>
              statics.update("speed", statics("speed") + msg.length.toDouble/1024)
            case _: KillMessage =>
              statics.update("kill", statics("kill") + msg.length.toDouble/1024)
            case _: FA=>
              statics.update("feed", statics("feed") + msg.length.toDouble/1024)
            case _: EA=>
              statics.update("eat", statics("eat") + msg.length.toDouble/1024)
            case _: ApplesRsp =>
              statics.update("apple", statics("apple") + msg.length.toDouble/1024)
            case _: RD =>
              statics.update("remove", statics("remove") + msg.length.toDouble/1024)
            case _: RC=>
              statics.update("rank", statics("rank") + msg.length.toDouble/1024)
            case _: RH=>
              statics.update("rank", statics("rank") + msg.length.toDouble/1024)
            case _: Po =>
              statics.update("ping", statics("ping") + msg.length.toDouble/1024)
            case _ =>
              statics.update("others", statics("others") + msg.length.toDouble/1024)

          }
          if(System.currentTimeMillis() - timer > period){
            timer = System.currentTimeMillis()
            val total =  statics.values.sum
            var details = ""
            details = details + s"\nTOTAL:$total kb\n"
            statics.foreach{s =>
              details = details + s"${s._1}: ${s._2} kb\n"
              statics.update(s._1, 0)
            }
            log.info(details)

          }
          BinaryMessage.Strict(ByteString(
            t.fillMiddleBuffer(sendBuffer).result()
          ))

        case s: GameReply =>
          val sendBuffer = new MiddleBufferInJvm(409600)
          BinaryMessage.Strict(ByteString(
            s.fillMiddleBuffer(sendBuffer).result()
          ))


        case x =>
          TextMessage.apply("")


      }.withAttributes(ActorAttributes.supervisionStrategy(decider))
  }


  val decider: Supervision.Decider = {
    e: Throwable =>
      e.printStackTrace()
      println(s"WS stream failed with $e")
      Supervision.Resume
  }



}

