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

import akka.actor.typed.{Behavior, PostStop}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.paradise.common.AppSettings
import org.seekloud.paradise.models.DAO.RecordDAO
import org.seekloud.paradise.models.SlickTables
import org.seekloud.paradise.ptcl.PDGameEvent
import org.seekloud.paradise.ptcl.PDGameEvent._
import org.seekloud.byteobject.MiddleBufferInJvm
import org.seekloud.essf.io.FrameOutputStream
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.seekloud.paradise.Boot.executor

/**
  * Created by Anjiansan on 2018/11/01.
  **/
object GameRecorder {

  import org.seekloud.byteobject.ByteObject._
  import org.seekloud.utils.ESSFSupport.initRecorder

  sealed trait Command

  final case class SaveDate(stop:Int) extends Command
  final case object Save extends Command
  final case object RoomClose extends Command
  final case object StopRecord extends Command

  final case class GameRecord(frame: Long, event: (List[GameEvent], Snapshot)) extends Command
  private final case object BehaviorChangeKey
  private final case object SaveDateKey
  private final val saveTime = AppSettings.gameRecordTime.minute

  private final val maxRecordNum = 100
  private final val fileMaxRecordNum = 100000000
  private final val log = LoggerFactory.getLogger(this.getClass)

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  case class TimeOut(msg:String) extends Command


  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer:TimerScheduler[Command]) = {
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }

  private[this] def getFileName(roomId: Long, startTime: Long) = s"paradise_${roomId}_$startTime"

  def create(roomId: Long, initState: Snapshot, gameInfo: GameInformation):Behavior[Command] = {
    Behaviors.setup{ ctx =>
      log.info(s"${ctx.self.path} is starting..")
      implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
      implicit val middleBuffer = new MiddleBufferInJvm(100 * 4096)
      Behaviors.withTimers[Command] { implicit timer =>
        timer.startSingleTimer(SaveDateKey, Save, saveTime)
        val recorder: FrameOutputStream = initRecorder(getFileName(roomId, gameInfo.startTime), gameInfo.index, gameInfo, Some(initState))
        switchBehavior(ctx,"work",work(recorder, gameInfo,mutable.HashMap[UserBaseInfo,List[UserJoinLeft]](), mutable.HashMap[String, String](),mutable.HashMap[String, String](),Nil,gameInfo.initFrame,-1l))
      }
    }
  }


  private def work(recorder: FrameOutputStream,
                   gameInfo: GameInformation,
                   essfMap: mutable.HashMap[UserBaseInfo,List[UserJoinLeft]],
                   userMap: mutable.HashMap[String, String],
                   userHistoryMap: mutable.HashMap[String, String],
                   eventRecorder: List[(List[GameEvent], Option[Snapshot])],
                   startFrame: Long,
                   endFrame: Long,
                   tickCount: Long = 0L
                  )(
                    implicit stashBuffer:StashBuffer[Command],
                    timer:TimerScheduler[Command],
                    middleBuffer: MiddleBufferInJvm
                  ) : Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case GameRecord(frame, event) =>
          val snapshot =
            if(event._1.exists {
              case MouseEvent(_,_,_) => false
              case SpeedEvent(_,_) => false
              case RankEvent(_,_) => false
              case FeedAppleEvent(_) => false
              case EatenAppleEvent(_) => false
              case RemoveEvent(_) => false
              case KillEvent(_, _, _, _, _) => false
              case DeadEvent(_,_,_) => false
              case _ => true
            } || tickCount % 50 == 0) Some(event._2) else None //是否做快照

          event._1.foreach {
            case JoinEvent(id, info) =>
              val name = if(info.isDefined){info.get.name} else {"somebody"}
              userMap.put(id, name)
              userHistoryMap.put(id, name)
              if(essfMap.get(UserBaseInfo(id, name)).nonEmpty) {
                essfMap.put(UserBaseInfo(id, name), essfMap(UserBaseInfo(id, name)) ::: List(UserJoinLeft(frame, -1l)))
              } else {
                essfMap.put(UserBaseInfo(id, name), List(UserJoinLeft(frame, -1l)))
              }

            case LeftEvent(id, nickName) =>
              userMap.remove(id)
              essfMap.get(UserBaseInfo(id, nickName)) match {
                case Some(joinOrLeftInfo) =>
                  if(joinOrLeftInfo.lengthCompare(1) == 0)
                    essfMap.put(UserBaseInfo(id, nickName), List(UserJoinLeft(joinOrLeftInfo.head.joinFrame, frame)))
                  else {
                    val join = joinOrLeftInfo.filter(_.leftFrame == -1l).head.joinFrame
                    essfMap.put(UserBaseInfo(id, nickName), essfMap(UserBaseInfo(id, nickName)) ::: List(UserJoinLeft(join, frame)))
                  }
                case None => log.warn(s"get ${UserBaseInfo(id, nickName)} from essfMap error..1")
              }

            case DeadEvent(_,deads,_) =>
              deads.groupBy(_._1).map(d => (d._1,d._2.last)).values.foreach {
                dead =>
                  val id = dead._1
                  val nickName = userMap.getOrElse(id, "")
                  userMap.remove(id)
                  essfMap.get(UserBaseInfo(id, nickName)) match {
                    case Some(joinOrLeftInfo) =>
                      if (joinOrLeftInfo.lengthCompare(1) == 0)
                        essfMap.put(UserBaseInfo(id, nickName), List(UserJoinLeft(joinOrLeftInfo.head.joinFrame, frame)))
                      else {
                        val join = joinOrLeftInfo.filter(_.leftFrame == -1l).head.joinFrame
                        essfMap.put(UserBaseInfo(id, nickName), essfMap(UserBaseInfo(id, nickName)) ::: List(UserJoinLeft(join, frame)))
                      }
                    case None => log.warn(s"get ${UserBaseInfo(id, nickName)} from essfMap error..2")
                  }
              }

            case VictoryEvent(ids) =>
              ids.foreach {
                id =>
                  val nickName = userMap.getOrElse(id, "")
                  userMap.remove(id)
                  essfMap.get(UserBaseInfo(id, nickName)) match {
                    case Some(joinOrLeftInfo) =>
                      if (joinOrLeftInfo.lengthCompare(1) == 0)
                        essfMap.put(UserBaseInfo(id, nickName), List(UserJoinLeft(joinOrLeftInfo.head.joinFrame, frame)))
                      else {
                        val join = joinOrLeftInfo.filter(_.leftFrame == -1l).head.joinFrame
                        essfMap.put(UserBaseInfo(id, nickName), essfMap(UserBaseInfo(id, nickName)) ::: List(UserJoinLeft(join, frame)))
                      }
                    case None => log.warn(s"get ${UserBaseInfo(id, nickName)} from essfMap error..3")
                  }
              }

            case _ =>
          }

          var newEventRecorder =  (event._1, snapshot) :: eventRecorder

          if (newEventRecorder.lengthCompare(maxRecordNum) > 0) { //每一百帧写入一次
            newEventRecorder.reverse.foreach {
              case (events, Some(state)) if events.nonEmpty =>
                recorder.writeFrame(events.fillMiddleBuffer(middleBuffer).result(), Some(state.fillMiddleBuffer(middleBuffer).result()))
              case (events, None) if events.nonEmpty => recorder.writeFrame(events.fillMiddleBuffer(middleBuffer).result())
              case _ => recorder.writeEmptyFrame()
            }
            newEventRecorder = Nil
          }
          switchBehavior(ctx, "work", work(recorder, gameInfo, essfMap, userMap, userHistoryMap, newEventRecorder, startFrame, frame, tickCount + 1))

        case Save =>
          log.info(s"${ctx.self.path} work get msg Save")
          timer.startSingleTimer(SaveDateKey, Save, saveTime)
          if(userHistoryMap.nonEmpty){
            ctx.self ! SaveDate(0)
          }
          switchBehavior(ctx, "save", save(recorder, gameInfo, essfMap, userMap, userHistoryMap, startFrame, endFrame))

        case RoomClose =>
          log.debug(s"gameRecorder-${gameInfo.roomId} is stopping...")
          if(userHistoryMap.nonEmpty){
            ctx.self ! SaveDate(0)
          }
          switchBehavior(ctx, "save", save(recorder, gameInfo, essfMap, userMap, userHistoryMap, startFrame, endFrame))

        case _ =>
          Behaviors.unhandled
      }
    }.receiveSignal{
      case (ctx, PostStop) =>
        timer.cancelAll()
        log.info(s"${ctx.self.path} stopping....")
        val mapInfo = essfMap.map{ essf=>
          val newJoinLeft = essf._2.map {
            case UserJoinLeft(joinFrame, -1l) => UserJoinLeft(joinFrame, endFrame)
            case other => other
          }
          (essf._1, newJoinLeft)
        }.toList

        recorder.putMutableInfo(AppSettings.essfMapKeyName, EssfMapInfo(mapInfo).fillMiddleBuffer(middleBuffer).result())
        eventRecorder.reverse.foreach {
          case (events, Some(state)) if events.nonEmpty =>
            recorder.writeFrame(events.fillMiddleBuffer(middleBuffer).result(), Some(state.fillMiddleBuffer(middleBuffer).result()))
          case (events, None) if events.nonEmpty => recorder.writeFrame(events.fillMiddleBuffer(middleBuffer).result())
          case _ => recorder.writeEmptyFrame()
        }
        recorder.finish()
        val filePath =  AppSettings.gameDataDirectoryPath + getFileName(gameInfo.roomId, gameInfo.startTime) + s"_${gameInfo.index}"
        RecordDAO.saveGameRecorder(gameInfo.roomId, gameInfo.startTime, System.currentTimeMillis(), filePath).onComplete{
          case Success(recordId) =>
//            val usersInRoom = userHistoryMap.map(u => SlickTables.rUserInRecord(u._1, recordId, gameInfo.roomId)).toSet
            val usersInRoom = essfMap.map(e => SlickTables.rUserInRecord(e._1.id,recordId,gameInfo.roomId, e._1.name)).toSet
            RecordDAO.saveUserInGame(usersInRoom).onComplete{
              case Success(_) =>
                log.debug(s"save the detail of UserInGame in db success... while PostStop")

              case Failure(e) =>
                log.warn(s"save the detail of UserInGame in db fail...$e while PostStop")
            }

          case Failure(e) =>
            log.warn(s"save the detail of GameRecorder in db fail...$e while PostStop")
        }
        Behaviors.stopped
    }
  }

  def save(recorder: FrameOutputStream,
           gameInfo: GameInformation,
           essfMap: mutable.HashMap[UserBaseInfo,List[UserJoinLeft]] = mutable.HashMap[UserBaseInfo,List[UserJoinLeft]](),
           userMap: mutable.HashMap[String, String] = mutable.HashMap[String, String](),
           userHistoryMap: mutable.HashMap[String, String] = mutable.HashMap[String, String](),
           startFrame: Long,
           endFrame: Long
          )(implicit stashBuffer: StashBuffer[Command],
            timer: TimerScheduler[Command],
            middleBuffer: MiddleBufferInJvm): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SaveDate(stop) =>
          val mapInfo = essfMap.map{ essf=>
            val newJoinLeft = essf._2.map {
              case UserJoinLeft(joinFrame, -1l) => UserJoinLeft(joinFrame, endFrame)
              case other => other
            }
            (essf._1, newJoinLeft)
          }.toList

          recorder.putMutableInfo(AppSettings.essfMapKeyName, EssfMapInfo(mapInfo).fillMiddleBuffer(middleBuffer).result())
          recorder.finish()

          val filePath = AppSettings.gameDataDirectoryPath + getFileName(gameInfo.roomId, gameInfo.startTime) + s"_${gameInfo.index}"
          RecordDAO.saveGameRecorder(gameInfo.roomId, gameInfo.startTime, System.currentTimeMillis(), filePath).onComplete{
            case Success(recordId) =>
//              val usersInRoom = userHistoryMap.map(u => SlickTables.rUserInRecord(u._1, recordId, gameInfo.roomId)).toSet
              val usersInRoom = essfMap.map(e => SlickTables.rUserInRecord(e._1.id,recordId,gameInfo.roomId, e._1.name)).toSet
              RecordDAO.saveUserInGame(usersInRoom).onComplete{
                case Success(_) =>
                  log.warn("save the detail of UserInGame in db success...")
                  if(stop == 0){
                    ctx.self ! SwitchBehavior("resetRecord", resetRecord(gameInfo, userMap))
                  }else{
                    ctx.self ! StopRecord
                  }

                case Failure(_) =>
                  log.warn("save the detail of UserInGame in db fail...")
                  if(stop == 0){
                    ctx.self ! SwitchBehavior("resetRecord", resetRecord(gameInfo, userMap))
                  }else{
                    ctx.self ! StopRecord
                  }
              }

            case Failure(e) =>
              log.warn("save the detail of GameRecorder in db fail...")
              if(stop == 0){
                ctx.self ! SwitchBehavior("resetRecord", resetRecord(gameInfo, userMap))
              }else{
                ctx.self ! StopRecord
              }
          }
          switchBehavior(ctx,"busy", busy())

        case RoomClose =>
          log.debug(s"gameRecorder-${gameInfo.roomId} is stopping...")
          Behaviors.stopped

        case _ =>
          Behaviors.unhandled
      }
    }
  }

  def resetRecord(gameInfo: GameInformation,
                  userMap: mutable.HashMap[String, String] = mutable.HashMap[String, String](),
                 )(implicit stashBuffer: StashBuffer[Command],
                   timer: TimerScheduler[Command],
                   middleBuffer: MiddleBufferInJvm): Behavior[Command] = {

    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case GameRecord(frame, event) => //新的文件初始化
          val newUserMap = userMap
          val newGameInfo = GameInformation(gameInfo.roomId, System.currentTimeMillis(), gameInfo.index + 1, frame)
          val recorder: FrameOutputStream = initRecorder(getFileName(gameInfo.roomId, newGameInfo.startTime), newGameInfo.index, newGameInfo, Some(event._2))
          val newEventRecorder = List((event._1, Some(event._2)))
          val newEssfMap = mutable.HashMap.empty[UserBaseInfo, List[UserJoinLeft]]

          newUserMap.foreach { user =>
            newEssfMap.put(UserBaseInfo(user._1, user._2), List(UserJoinLeft(frame, -1L)))
          }
          switchBehavior(ctx, "work", work(recorder, newGameInfo, newEssfMap, newUserMap, newUserMap, newEventRecorder, frame, -1l))

        case StopRecord =>
          log.debug(s"gameRecorder-${gameInfo.roomId} is stopping...")
          Behaviors.stopped

        case RoomClose =>
          log.debug(s"gameRecorder-${gameInfo.roomId} is stopping...")
          Behaviors.stopped

        case _ =>
          Behaviors.unhandled

      }
    }
  }

  private def busy()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] =
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case SwitchBehavior(name, behavior,durationOpt,timeOut) =>
          switchBehavior(ctx,name,behavior,durationOpt,timeOut)

        case TimeOut(m) =>
          log.debug(s"${ctx.self.path} is time out when busy,msg=$m")
          Behaviors.stopped

        case unknownMsg =>
          stashBuffer.stash(unknownMsg)
          Behavior.same
      }
    }


}
