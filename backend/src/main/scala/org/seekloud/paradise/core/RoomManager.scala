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
  * Time: 16:05
  */

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import akka.actor.typed.scaladsl.AskPattern._
import akka.stream.scaladsl.Flow
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import akka.stream.{ActorAttributes, OverflowStrategy, Supervision}
import akka.util.Timeout
import org.seekloud.paradise.core.RoomActor.{JoinRoom, JoinRoom4Watch}
import org.seekloud.paradise.core.UserActor._
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.util.{Left, Random, Right}
import org.seekloud.paradise.ptcl.paradise.Protocol._

import scala.concurrent.Future
import org.seekloud.paradise.Boot.{esheepSyncClient, executor, scheduler, timeout}
import org.seekloud.paradise.protocol.EsheepProtocol._
import org.seekloud.paradise.ptcl.paradise.{Protocol, WsSourceProtocol}

import scala.collection.mutable

object RoomManager {

  implicit val timeout: akka.util.Timeout = akka.util.Timeout(20.seconds)

  private val log = LoggerFactory.getLogger(this.getClass)
  private val roomMap = mutable.HashMap[Long, mutable.HashSet[(String, String)]]() //roomId->Set((userId, name))
  private case object BehaviorChangeKey

  trait Command
  case class TimeOut(msg: String) extends Command
  case class RoomActorDead(roomId: Long,roomActor: ActorRef[RoomActor.Command]) extends Command
  case class PlayerJoinRoomReq(userId: String,name: String,userActor:ActorRef[UserActor.Command],roomId: Long) extends Command
  case class WatcherJoinRoomReq(watcherId: String,watchedId: String,roomId: Long,userActor: ActorRef[UserActor.Command]) extends Command
  case class GetUserInRoom(roomId:Long,replyTo:ActorRef[GetPlayerInRoomRsp]) extends Command
  case class GetAllRoom(replyTo:ActorRef[List[Long]]) extends Command
  case class UserLeftRoom(userId: String,userName: String,roomId: Long) extends Command
  case class GetAllRoomInfoReq(ref: ActorRef[AllRoomInfoRsp]) extends Command
  case class AllRoomInfoRsp(roomInfo: mutable.HashMap[Long,Int]) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
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
    log.debug("roomManager is starting...")
    Behaviors.setup[Command] {
      ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        Behaviors.withTimers[Command] { implicit timer =>
          switchBehavior(ctx,"idle",idle())
        }
    }
  }

  def idle()(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {
    Behaviors.receive[Command]{
      (ctx,msg) =>
        msg match {
          case PlayerJoinRoomReq(userId,name,userActor,roomId) =>
            if (roomMap.get(roomId).isDefined) {
              roomMap.put(roomId, roomMap(roomId) += ((userId, name)))
            } else {
              roomMap.put(roomId, mutable.HashSet((userId, name)))
            }
            getRoomActor(ctx,roomId) ! JoinRoom(userId,name,userActor)
            Behaviors.same

          case WatcherJoinRoomReq(watcherId,watchedId,roomId,userActor) =>
            getRoomActor(ctx,roomId) ! JoinRoom4Watch(watcherId,roomId,watchedId,userActor)
            Behaviors.same

          case RoomActorDead(roomId,roomActor) =>
            roomMap.remove(roomId)
            ctx.unwatch(roomActor)
            Behaviors.same

          case UserLeftRoom(userId,userName,roomId) =>
            log.debug("roomManager receive userLeftRoom")
            if(roomMap.exists(_._1 == roomId)){
              roomMap(roomId).remove((userId,userName))
            }
            Behaviors.same

          case GetUserInRoom(roomId,replyTo) =>
            if(roomMap.get(roomId).isDefined && roomMap(roomId).nonEmpty){
              val list = roomMap(roomId).map(user => PlayerInfo(user._1,user._2)).toList
              replyTo ! GetPlayerInRoomRsp(PlayerList(list))
            }
            else {
              replyTo ! GetPlayerInRoomRsp(PlayerList(Nil),errCode = 100006,msg = "该房间未被创建或没有玩家")
            }
            Behaviors.same

          case GetAllRoom(replyTo) =>
            replyTo ! roomMap.keySet.toList
            Behaviors.same

          case GetAllRoomInfoReq(ref) =>
            val result = if(roomMap.isEmpty) {
              mutable.HashMap.empty[Long,Int]
            }else{
              roomMap.map{r => (r._1,r._2.size)}
            }
            ref ! AllRoomInfoRsp(result)
            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} receive unknown msg: $unknow")
            Behaviors.same

        }
    }
  }

  private def getRoomActor(ctx: ActorContext[Command],roomId: Long): ActorRef[RoomActor.Command] = {
    val childName = s"roomActor-$roomId"
    ctx.child(childName).getOrElse{
      val actor = ctx.spawn(RoomActor.create(roomId),childName)
      ctx.watchWith(actor,RoomActorDead(roomId,actor))
      actor
    }.upcast[RoomActor.Command]
  }

}
