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
  * Time: 16:03
  */
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicInteger
import java.awt.event.KeyEvent

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer, TimerScheduler}
import org.seekloud.paradise.core.UserActor._
import org.seekloud.paradise.Boot.roomManager
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.collection.mutable.Map

import org.seekloud.paradise.snake.GridOnServer
import org.seekloud.paradise.common.Constants._
import org.seekloud.paradise.core.GameRecorder.RoomClose
import org.seekloud.paradise.core.RoomManager.UserLeftRoom
import org.seekloud.paradise.ptcl.PDGameEvent._
import org.seekloud.paradise.ptcl._
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.ptcl.Protocol.{frameRate, rate}

import org.seekloud.paradise.models.DAO.RecordDAO._
import org.seekloud.paradise.models.DAO.RecordDAO._
import org.seekloud.paradise.Boot.esheepSyncClient
import org.seekloud.paradise.Boot.userManager
import org.seekloud.paradise.common.AppSettings
import scala.collection.mutable

object RoomActor {

  private val log = LoggerFactory.getLogger(this.getClass)
  private case object BehaviorChangeKey
  case class UserInfo(userName: String,userActor: ActorRef[UserActor.Command])
  case class WatchInfo(watchedId: String,watcherActor: ActorRef[UserActor.Command])

  sealed trait Command
  case class TimeOut(msg: String) extends Command
  case class JoinRoom(userId: String,name: String,userActor: ActorRef[UserActor.Command]) extends Command
  case class FromUserActor(msgFromFront: MsgFromFront) extends Command
  case class UserActorTDead(userId: String, userName: String,userActor: ActorRef[UserActor.Command]) extends Command

  case class JoinRoom4Bot(id: String, name: String, botActor: ActorRef[BotActor.Command]) extends Command

  case class LeftRoom4Watch(watcherId: String,watchedId: String,userActor: ActorRef[UserActor.Command]) extends Command with RoomManager.Command
  case class JoinRoom4Watch(watcherId: String,roomId: Long,watchedId: String,userActor4Watch: ActorRef[UserActor.Command]) extends Command with  RoomManager.Command
  case object SyncData extends Command

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

  def create(roomId: Long): Behavior[Command] = {
    log.debug(s"roomActor-$roomId is starting...")
    Behaviors.setup[Command] {
      ctx =>
        implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
        val grid = new GridOnServer(bounds)
        grid.genRandomBarriers()
        Behaviors.withTimers[Command] { implicit timer =>
          getGameRecorder(ctx, roomId, grid)
          AppSettings.botMap.foreach { b =>
            val id = "bot_" + roomId + b._1
            getBotActor(ctx, id) ! BotActor.InitInfo(b._2, grid, ctx.self)
          }
          timer.startPeriodicTimer(s"syncData-$roomId",SyncData,frameRate.millis)
          switchBehavior(ctx,"idle",idle(roomId,grid,mutable.ArrayBuffer[(Long, GameEvent)]()))
        }
    }
  }

  def idle(roomId: Long,grid: GridOnServer, gameEvent: mutable.ArrayBuffer[(Long, GameEvent)])(implicit stashBuffer: StashBuffer[Command],timer: TimerScheduler[Command]): Behavior[Command] = {

    val userActors = Map.empty[String,UserInfo]
    val watcherMap = Map.empty[String,WatchInfo]  //watcherId -> (watchedId,watcherActor)
    val deadSnakes = Map.empty[String,Point]
    var tickCount = 0l
    val botMap: mutable.HashMap[String, (String, ActorRef[BotActor.Command])] = mutable.HashMap[String, (String, ActorRef[BotActor.Command])]()

    //判断房间中是否有此玩家id
    def judgeId(userId: String)(f: String => Unit) = {
      if(userId.take(3) == "bot") {
        val user = botMap.find(_._1 == userId)
        user match {
          case Some(userInfo) =>
            f(userInfo._2._1)

          case None =>
            log.warn(s"can not find bot-$userId in room-$roomId")
        }
      }
      else {
        val user = userActors.find(_._1 == userId)
        user match {
          case Some(userInfo) =>
            f(userInfo._2.userName)

          case None =>
            log.warn(s"can not find user-$userId in room-$roomId")
        }
      }
    }

    //判断房间中是否有此观看者id
    def judgeIdOfWatcher(watcherId: String)(f: WatchInfo => Unit) = {
      val watch = watcherMap.find(_._1 == watcherId)
      watch match {
        case Some(watchInfo) =>
          f(watchInfo._2)

        case None =>
          log.warn(s"can not find watcher-$watcherId in room-$roomId")
      }
    }

    //专用于往前端发的消息
    def dispatchToAll(gameMessage: GameMessage) = {
      val fromRoomActor = FromRoomActor(gameMessage)
      userActors.values.map(_.userActor).foreach{userActor => userActor ! fromRoomActor}
      watcherMap.values.map(_.watcherActor).foreach{userActor => userActor ! fromRoomActor}

    }

    def dispatchToId(userId: String,gameMessage: GameMessage) = {
      val fromRoomActor = FromRoomActor(gameMessage)
      userActors.filter(_._1 == userId).values.map(_.userActor).foreach{userActor => userActor ! fromRoomActor}
    }

    def dispatchToWatcherId(watcherId: String,gameMessage: GameMessage) = {
      val fromRoomActor = FromRoomActor(gameMessage)
      watcherMap.filter(_._1 == watcherId).values.map(_.watcherActor).foreach{userActor => userActor ! fromRoomActor}
    }

    Behaviors.receive[Command] {
      (ctx,msg) =>
        msg match {
          case JoinRoom(userId,name,userActor) =>
            log.debug(s"${ctx.self} receive msg: $msg user $userId join room")
            //把房间号发给userActor
            userActor ! RoomInfoRsp(roomId,ctx.self)
            grid.addSnake(userId,name)
            userActors.put(userId,UserInfo(name,userActor))
            ctx.watchWith(userActor,UserActorTDead(userId,name,userActor))
            dispatchToId(userId,IdInfo(userId,roomId))
            dispatchToAll(NewSnakeJoined(userId,name))
            dispatchToId(userId,Barriers(grid.getBarriers))
            dispatchToId(userId,grid.getDataAfterDead())
            dispatchToAll(grid.getGridData)
            gameEvent += ((grid.frameCount, JoinEvent(userId, None)))
            if (userActors.size + botMap.size > AppSettings.minPlayerNum && botMap.nonEmpty) {
              val killBot = botMap.head
              botMap.-=(killBot._1)
              getBotActor(ctx, killBot._1) ! BotActor.KillBot

              grid.removeSnake(killBot._1)
              watcherMap.filter(_._2.watchedId == userId).foreach{w => dispatchToWatcherId(w._1,PlayerLeft())}
              dispatchToAll(SnakeLeft(killBot._1, killBot._2._1))
              gameEvent += ((grid.frameCount, LeftEvent(killBot._1, killBot._2._1)))
            }
            Behaviors.same

          case JoinRoom4Bot(userId,name,userActor) =>
            log.debug(s"${ctx.self} receive msg: $msg user $userId join room")
            botMap.put(userId, (name, userActor))
            grid.addSnake(userId,name)
            dispatchToAll(NewSnakeJoined(userId,name))
            dispatchToAll(grid.getGridData)
            gameEvent += ((grid.frameCount, JoinEvent(userId, None)))
            Behaviors.same

          case UserActorTDead(userId,userName,userActor) =>
            log.debug(s"roomActor get userActor dead msg from user-$userId-$userName")
            judgeId(userId) { _ =>

              grid.removeSnake(userId)
              userActors.remove(userId)
              ctx.unwatch(userActor)
              watcherMap.filter(_._2.watchedId == userId).foreach{w => dispatchToWatcherId(w._1,PlayerLeft())}
              roomManager ! UserLeftRoom(userId,userName,roomId)
              dispatchToAll(SnakeLeft(userId,userName))
              gameEvent += ((grid.frameCount, LeftEvent(userId, userName)))

              //离开房间时写入战绩
              val (killing,score) = grid.getExploit(userId)
              getInfoByUserId(userId).map{
                infoSeq =>
                  val recordId = infoSeq.map(_.recordId).toList.last
                  getRecordById(recordId).map{
                    case Some(l)=>
                      val startTime = l.startTime
                      val endTime = l.endTime
                      esheepSyncClient ! EsheepSyncClient.InputRecord(userId,userName,killing,1,score,startTime,endTime)
                    case None =>
                      log.debug(s"record--$recordId didn't exist!!")

                  }
              }
            }

            if (userActors.size + botMap.size < AppSettings.minPlayerNum) {
              if (userActors.isEmpty) {
                botMap.foreach(b => getBotActor(ctx, b._1) ! BotActor.KillBot)
                Behaviors.stopped
              } else {
                if (!AppSettings.botMap.forall(b => botMap.keys.toList.contains("bot_" + roomId + b._1))) {
                  val newBot = AppSettings.botMap.filterNot(b => botMap.keys.toList.contains("bot_" + roomId + b._1)).head
                  getBotActor(ctx, "bot_" + roomId + newBot._1) ! BotActor.InitInfo(newBot._2, grid, ctx.self)
                }
                Behaviors.same
              }
            }

            if(userActors.isEmpty){
              getGameRecorder(ctx, roomId, grid) ! RoomClose
              Behaviors.stopped
            }else {
              Behaviors.same
            }

          case JoinRoom4Watch(watcherId, _, watchedId, userActor4Watch) =>
            log.debug(s"${ctx.self.path} watcher join msg=${msg}")
            //把房间号发给userActor
            userActor4Watch ! RoomInfoTRsp(watcherId,watchedId,roomId,ctx.self)
            watcherMap.put(watcherId,WatchInfo(watchedId,userActor4Watch))
            ctx.watchWith(userActor4Watch,LeftRoom4Watch(watcherId,watchedId,userActor4Watch))
            if(watchedId =="no player"){
              dispatchToWatcherId(watcherId,PlayerIsNone())
            }else{//处理观战信息
              dispatchToWatcherId(watcherId,IdInfo(watchedId,roomId))
              val watcherCount = watcherMap.count(_._2.watchedId == watchedId)
              val watchedName = if(userActors.contains(watchedId)) { userActors(watchedId).userName } else { "somebody" }
              dispatchToAll(NewWatcherJoined(watcherCount,watchedName))
              dispatchToWatcherId(watcherId,Barriers(grid.getBarriers))
              dispatchToWatcherId(watcherId,grid.getDataAfterDead())
              val gridData = grid.getGridData
              dispatchToWatcherId(watcherId, gridData)
              if(deadSnakes.contains(watchedId))
                dispatchToWatcherId(watcherId, PlayerIsDead())
            }
            Behaviors.same

          case LeftRoom4Watch(watcherId: String, watchedId:String, userActor4Watch:ActorRef[UserActor.Command]) =>
            log.debug(s"${ctx.self.path} watcher left msg=${msg}")
            judgeIdOfWatcher(watcherId){_ =>
              watcherMap.remove(watcherId)
              ctx.unwatch(userActor4Watch)
            }
            Behaviors.same

          case FromUserActor(msgFromFront) =>
//            log.debug(s"room-$roomId receive msg from userActor: $msgFromFront")
            msgFromFront match{
              case Key(userId,keyCode,frameCount) =>
                judgeId(userId) { userName =>
                  if (keyCode == KeyEvent.VK_SPACE) {
                    deadSnakes.remove(userId)
                    //死亡的玩家按空格复活
                    val info = grid.addSnake(userId, userName)
                    if(userId.take(3) == "bot") {
                      log.info(s"userId=$userId space")
                      getBotActor(ctx, userId) ! BotActor.BackToGame
                    }
                    else
                      dispatchToId(userId, grid.getDataAfterDead())
                    gameEvent += ((grid.frameCount, SpaceEvent(userId, userName, info._1, info._2)))
                    gameEvent += ((grid.frameCount, JoinEvent(userId, None)))
                  } else {
                    grid.addActionWithFrame(userId, keyCode, frameCount)
                  }
                }

              case Pi(userId,createTime) => //Ping
                judgeId(userId){ _ =>
                  dispatchToId(userId,Po(createTime)) //PongData
                }


              case MM(userId,x,y,frameCount) => //MouseMove
                judgeId(userId) { _ =>
                  grid.addMouseWithFrame(userId,x,y,frameCount)
                  dispatchToAll(SA(userId,x,y,frameCount)) //SnakeAction
                }

              case MS(userId,frameCount,speedUP) => //MouseSpeed
                judgeId(userId) { _ =>
                  grid.addSpeedWithFrame(userId,speedUP,frameCount)
                  dispatchToAll(SnakeSpeed(userId,speedUP,frameCount))
                }

              case ApplesReq(userId) =>
                judgeId(userId) { a =>
                  val apples = grid.grid.filter(_._2.isInstanceOf[Apple])
                  dispatchToId(userId,ApplesRsp(apples))
                }

              case AC(userId,x,y,frameCount) => //ActionCheck
                judgeId(userId) { _ =>
                  if (grid.frameCount - 5 <= frameCount) {
                    try {
                      val action = grid.mouseMap(frameCount)(userId)
                      if (action._1 != x || action._2 != y) {
                        //发现移动出错重传
                        dispatchToId(userId, grid.getGridData)
                      }
                    }catch{
                      case e:Exception =>
                        log.warn(s"error in ActionCheck at frame:$frameCount with msg:${e.getMessage}")
                    }
                  }
                }

              case SC(userId,isSpeedUp,frameCount) => //SpeedCheck
                judgeId(userId){ _ =>
                  if(grid.frameCount - 5 <= frameCount) {
                    try {
                      val speed = grid.speedMap(frameCount)(userId)
                      if (speed != isSpeedUp) {
                        //发现速度出错重传
                        dispatchToId(userId, grid.getGridData)
                      }
                    }catch{
                      case e: Exception =>
//                        log.warn(s"error in SpeedCheck at frame:$frameCount with msg:${e.getMessage}")
                    }
                  }
                }

              case NetTest(userId,createTime) =>
                judgeId(userId) { _ =>
                  log.debug(s"Net Test: createTime = $createTime")
                  dispatchToId(userId, NetDelayTest(createTime))
                }

              case TextInfo(userId,info) =>
                judgeId(userId) { _ =>
                  log.debug(s"get msg:$info from user-$userId")
                }

              case _ =>

            }
            Behaviors.same

          case SyncData =>
//            log.debug(s"测试: gameEvent" + gameEvent.toString + s"frame: ${grid.frameCount}")
            tickCount += 1
            if(userActors.keys.nonEmpty){
              if(grid.getDeads.nonEmpty){
                val newSnake = grid.getNewSnakes
                if(grid.getKillMsg.nonEmpty){
                  grid.getKillMsg.foreach{ msg =>
                    dispatchToAll(msg)

                    gameEvent += ((grid.frameCount, KillEvent(msg.killedId, msg.killedName, msg.killerName, msg.killedCount, msg.killedLength)))
                  }
                  grid.clearKillMsg()
                }
                grid.getDeads.map(d =>
                  deadSnakes.put(d._1,d._2)//记录死亡的玩家，使观战者第一次进入时能获知该玩家是否在死亡状态
                )
                dispatchToAll(NewSnakes(newSnake,grid.getDeads,grid.frameCount))

                gameEvent += ((grid.frameCount, DeadEvent(newSnake,grid.getDeads,grid.frameCount)))
//                log.info(s"deads=${ grid.getDeads}")
                grid.getDeads.groupBy(_._1).keySet.foreach {
                  d =>
                    if(d.take(3) == "bot")
                      getBotActor(ctx, d) ! BotActor.BotDead
                }
              }

              grid.update(false)

              val feedApples = grid.getFeededApple
              val eatenApples = grid.getEatenApples
              grid.resetFoodData()
              if(grid.vicSnakes.nonEmpty) {
                dispatchToAll(Victory(grid.vicSnakes))

                gameEvent += ((grid.frameCount - 1, VictoryEvent(grid.vicSnakes)))

//                log.info(s"botMap=$botMap")
                botMap.foreach {
                  b =>
                    getBotActor(ctx, b._1) ! BotActor.BotDead
                }

                grid.grid = grid.grid.filterNot(_._2.isInstanceOf[Apple])
                grid.snakes = scala.collection.immutable.Map.empty[String,SkDt]
                grid.vicSnakes = List.empty[String]
              }

              if(tickCount % rate == 5) {
                dispatchToAll(grid.getGridData)
              }

              if(feedApples.nonEmpty) {
                dispatchToAll(FA(feedApples)) //FeedApples

                gameEvent += ((grid.frameCount - 1, FeedAppleEvent(feedApples)))
              }

              if(eatenApples.values.exists(a => a.nonEmpty)) {
                dispatchToAll(EA(eatenApples.map(e => EatInfo(e._1,e._2)).toList))

                gameEvent += ((grid.frameCount - 1, EatenAppleEvent(eatenApples.map(e => EatInfo(e._1,e._2)).toList)))
              }

              if(grid.removeGrid.nonEmpty) {
                dispatchToAll(RD(grid.removeGrid)) //RemoveData
                grid.removeGrid = List.empty[(Point,Int,String)]

                gameEvent += ((grid.frameCount - 1, RemoveEvent(grid.removeGrid)))
              }

              if(tickCount % rate == 1) {
//                dispatchToAll(Ranks(grid.currentRank,grid.historyRankList))

                gameEvent += ((grid.frameCount - 1, RankEvent(grid.currentRank,grid.historyRankList)))
              }

              if(tickCount % rate == 1) {
                dispatchToAll(RC(grid.currentRank))//RanksCurrent
              }

              if(tickCount % rate == 11) {
                dispatchToAll(RH(grid.historyRankList))//RanksHistory
              }

              //            GameRecorder
              val frame = grid.frameCount - 1
              val mouseEvent = grid.getMouseEvent(frame)
              val speedEvent = grid.getSpeedEvent(frame)
              val baseEvent = gameEvent.filter(_._1 == frame).map {
                case (f, JoinEvent(id, None)) => (f, JoinEvent(id, grid.snakes.get(id)))
                case other => other
              }.map(_._2).toList
              gameEvent --= gameEvent.filter(_._1 == frame )
//              log.debug(s"测试: baseEvent" + baseEvent.toString + s"frame: $frame")
              getGameRecorder(ctx, roomId, grid) ! GameRecorder.GameRecord(frame, (baseEvent ::: mouseEvent ::: speedEvent, grid.getSnapshot))
            }

            Behaviors.same

          case unknow =>
            log.debug(s"${ctx.self} receive unknown msg: $unknow")
            Behaviors.same
        }
    }
  }

  private def getGameRecorder(ctx: ActorContext[Command], roomId: Long, grid: GridOnServer): ActorRef[GameRecorder.Command] = {
    val childName = s"gameRecorder-$roomId"
    ctx.child(childName).getOrElse {
      val snapshot = grid.getSnapshot
      val actor = ctx.spawn(GameRecorder.create(roomId, snapshot,
        GameInformation(roomId, System.currentTimeMillis(), 0, grid.frameCount)), childName)
      ctx.watch(actor)
      actor
    }.upcast[GameRecorder.Command]
  }

  private def getBotActor(ctx: ActorContext[Command], botId: String) = {
    val childName = botId
    ctx.child(childName).getOrElse {
      val actor = ctx.spawn(BotActor.create(botId), childName)
      actor
    }.upcast[BotActor.Command]
  }


}


