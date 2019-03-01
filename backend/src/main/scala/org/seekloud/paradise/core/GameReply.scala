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

import java.io.File

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.paradise.common.AppSettings
import org.seekloud.paradise.models.DAO.RecordDAO
import org.seekloud.paradise.ptcl.PDGameEvent.{GameInformation, UserBaseInfo, UserJoinLeft}
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.ptcl.paradise.WsSourceProtocol
import org.seekloud.byteobject.MiddleBufferInJvm
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import org.seekloud.utils.ESSFSupport._
import org.seekloud.essf.io.{FrameData, FrameInputStream}
import org.seekloud.paradise.Boot.executor
import org.seekloud.paradise.core.UserActor.GetRecordFrame
import org.seekloud.paradise.protocol.EsheepProtocol.{GetRecordFrameRsp, RecordFrameInfo}

/**
  * Created by Anjiansan on 2018/11/13.
  **/
object GameReply {

  sealed trait Command

  private val log = LoggerFactory.getLogger(this.getClass)

  private val waitTime=10.minutes

  case class TimeOut(msg: String) extends Command

  private final case object BehaviorChangeKey
  private final case object BehaviorWaitKey
  private final case object GameLoopKey
  case object GameLoop extends Command

  case class Left() extends Command
  case class GetRecordFrame(playerId: String, replyTo: ActorRef[CommonRsp]) extends Command
  case object StopReplay extends Command

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None,timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer:TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey, timeOut, _))
    stashBuffer.unstashAll(ctx, behavior)
  }

  case class InitReplay(subscriber: ActorRef[MsgFromBackend], userId:String, f:Int) extends Command

  def create(recordId:Long, playerId: String): Behavior[Command] = {
    Behaviors.setup[Command]{ ctx=>
      log.info(s"${ctx.self.path} is starting..")
      implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
      implicit val sendBuffer = new MiddleBufferInJvm(81920)
      Behaviors.withTimers[Command] { implicit timer =>
        RecordDAO.getRecordById(recordId).map {
          case Some(r)=>
            try{
              val replay =
                if(new File(r.filePath).exists()){
                  initInput(r.filePath)
                } else if(new File("../" + r.filePath).exists()){
                  initInput("../" + r.filePath)
                } else {
                  initInput("./backend" + r.filePath)
                }
              val info = replay.init()
              log.debug(s"userInfo:${userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m}")
              RecordDAO.getRoomIdByRecordId(recordId).map {
                roomId =>
                  ctx.self ! SwitchBehavior("work",
                    work(
                      replay,
                      roomId,
                      metaDataDecode(info.simulatorMetadata).right.get,
                      info.frameCount,
                      userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m
                    ))
              }
            }catch {
              case e:Throwable=>
                log.error("error---"+e.getMessage)
                ctx.self ! SwitchBehavior("initError",initError)
            }
          case None=>
            log.debug(s"record--$recordId didn't exist!!")
            ctx.self ! SwitchBehavior("initError",initError)
        }
        switchBehavior(ctx,"busy",busy())
      }
    }
  }

  def work(fileReader:FrameInputStream,
           roomId: Long,
           metaData:GameInformation,
           frameCount: Int,
           userMap:List[((UserBaseInfo, List[UserJoinLeft]))],
           userOpt:Option[ActorRef[MsgFromBackend]]=None
          )(
            implicit stashBuffer:StashBuffer[Command],
            timer:TimerScheduler[Command],
            sendBuffer: MiddleBufferInJvm
          ):Behavior[Command]={

    log.debug("进入work状态")

    Behaviors.receive[Command]{(ctx,msg)=>
      msg match {
        case msg:InitReplay=>
          log.info("start new replay!")
          print(s"msg.userId:::::${msg.userId}")
          timer.cancel(GameLoopKey)
          timer.cancel(BehaviorWaitKey)
          userMap.find(_._1.id == msg.userId) match {
            case Some(u)=>
              //todo dispatch gameInformation
              dispatchTo(msg.subscriber, UserInfo(msg.userId))
              log.info(s" set replay from frame=${msg.f}")
              log.info(s"该存储块的起始帧号为:${metaData.initFrame}")
              log.debug(s"get snapshot index::${fileReader.getSnapshotIndexes}")
              val nearSnapshotIndex = fileReader.gotoSnapshot(msg.f)
              log.debug(s"nearSnapshotIndex: $nearSnapshotIndex")
              log.info(s"replay from frame=${fileReader.getFramePosition}")
              log.debug(s"start loading ======")
              dispatchTo(msg.subscriber, StartLoading(nearSnapshotIndex, roomId))

              for(i <- 0 until (msg.f - fileReader.getFramePosition)){
                if(fileReader.hasMoreFrame){
                  fileReader.readFrame().foreach { f => dispatchByteTo(msg.subscriber, f)}
                }else{
                  log.debug(s"${ctx.self.path} file reader has no frame, reply finish")
                  dispatchTo(msg.subscriber, ReplayFinish(msg.userId))
                }
              }
              log.debug(s"start replay ======")
              dispatchTo(msg.subscriber, StartReplay(nearSnapshotIndex, fileReader.getFramePosition))

              if(fileReader.hasMoreFrame){
                timer.startPeriodicTimer(GameLoopKey, GameLoop, 150.millis)
                work(fileReader,roomId,metaData,frameCount,userMap,Some(msg.subscriber))
              }else{
                timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
                Behaviors.same
              }
            case None=>
              timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
              Behaviors.same
          }

        case GameLoop=>
          if(fileReader.hasMoreFrame){
            userOpt.foreach(u=>
              fileReader.readFrame().foreach { f =>
                dispatchByteTo(u, f)
              }
            )
            Behaviors.same
          }else{
            log.debug(s"has not more frame")
            timer.cancel(GameLoopKey)
            userOpt.foreach(u => dispatchTo(u, ReplayFinish("userId")))

            timer.startSingleTimer(BehaviorWaitKey,TimeOut("wait time out"),waitTime)
            Behaviors.same
          }

        case GetRecordFrame(playerId, replyTo) =>
          replyTo ! GetRecordFrameRsp(RecordFrameInfo(fileReader.getFramePosition, frameCount, 150))
          Behaviors.same

        case StopReplay =>
          log.info(s"${ctx.self.path} is stopping..")
          Behaviors.stopped

        case msg:TimeOut=>
          Behaviors.stopped

        case unKnowMsg =>
          stashBuffer.stash(unKnowMsg)
          Behavior.same
      }
    }
  }

  private def initError(
                         implicit sendBuffer: MiddleBufferInJvm
                       ):Behavior[Command]={

    log.debug("进入initError状态")

    Behaviors.receive[Command]{(ctx,msg)=>
      msg match {
        case msg:InitReplay =>
          log.debug(s"游戏文件不存在或已损坏")
          dispatchTo(msg.subscriber, InitReplayError("游戏文件不存在或者已损坏！！"))
          Behaviors.stopped

        case msg:GetRecordFrame=>
          msg.replyTo ! ErrorRsp(10001,"init error")
          Behaviors.stopped

        case msg=>
          log.debug(s"unknown message:$msg")
          Behaviors.stopped
      }
    }
  }

  def dispatchTo(subscriber: ActorRef[MsgFromBackend], msg: GameReply)(implicit sendBuffer: MiddleBufferInJvm)= {
    subscriber ! msg
  }

  def dispatchByteTo(subscriber: ActorRef[MsgFromBackend], msg:FrameData)(implicit sendBuffer: MiddleBufferInJvm) = {
    val events = replayEventDecode(msg.eventsData)
    val state = if(msg.stateData.isEmpty) None else Some(replayStateDecode(msg.stateData.get))
    subscriber ! ReplayFrameData(msg.frameIndex, events, state)
  }

  private def busy()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SwitchBehavior(name, behavior,durationOpt,timeOut) =>
          log.debug(s"switchBehavior")
          switchBehavior(ctx,name,behavior,durationOpt,timeOut)

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=${m}")
          Behaviors.stopped

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }

}
