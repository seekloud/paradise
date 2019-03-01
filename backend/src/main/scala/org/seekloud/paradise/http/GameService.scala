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

package org.seekloud.paradise.http

import java.util.concurrent.atomic.AtomicInteger

import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.stream.scaladsl.Flow
import org.seekloud.paradise.core.UserManager._
import org.seekloud.paradise.ptcl.Protocol._

import scala.language.implicitConversions

import org.slf4j.LoggerFactory
import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorAttributes, Materializer, Supervision}
import akka.util.{ByteString, Timeout}

import scala.concurrent.{ExecutionContextExecutor, Future}
import akka.actor.typed.scaladsl.AskPattern._
import org.seekloud.paradise.Boot._
import org.seekloud.paradise.core.RoomManager
import org.seekloud.paradise.core.RoomManager.{AllRoomInfoRsp, GetAllRoomInfoReq, GetUserInRoom}
import org.seekloud.paradise.protocol.EsheepProtocol._
import org.seekloud.paradise.ptcl.paradise.Protocol._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Random
import io.circe.generic.auto._

/**
  * Created by zx0 on 2018/11/01.
  **/
trait GameService extends AuthService {


  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer
  implicit val scheduler: Scheduler
  implicit val timeout: Timeout

  val idGenerator = new AtomicInteger(1000000)
  val random = new Random(System.nanoTime())

  private[this] val log = LoggerFactory.getLogger("org.seekloud.paradise.http.GameService")

  val playGameWs = (pathPrefix("playGame") & get){
    parameter(
      'playerId.as[String],
      'playerName,
      'accessCode.as[String],
      'roomId.as[Long]) { (playerId,name, acCode,roomIdT) =>
      authPlatUser(acCode){ user =>
        val rstFutureForRoomInfo: Future[AllRoomInfoRsp] = roomManager ? (ref => GetAllRoomInfoReq(ref))
        dealFutureResult{
          rstFutureForRoomInfo.map {
            allRoomInfo =>
              val roomInfo = (1 to 3).map{ r =>
                (r.toLong,allRoomInfo.roomInfo.getOrElse(r.toLong,0))
              }.toMap
              val roomId = if(roomInfo.isEmpty) {
                //所有房间都是空，随机挑选一间进入
                (random.nextInt(3) + 1).toLong
              }else{
                val availableRoom = roomInfo.filter(_._2 < 10)
                if(availableRoom.isEmpty) {
                  //所有房间都是限定人数之上，则挑选人少的进入
                  roomInfo.minBy(_._2)._1
                }else{
                  //有限定人数以下的房间，则挑选人多的进入
                  availableRoom.maxBy(_._2)._1
                }
              }
              val rstFutureForPlayerJoin: Future[PlayerJoinRsp] = userManager ? (ref => PlayerJoinReq(playerId,name,roomId,ref))
              dealFutureResult{
                rstFutureForPlayerJoin.map{
                  rst =>
                    handleWebSocketMessages(rst.flow)
                }
              }
          }
        }

      }
    }
  }

  val playGameWs4Client = (pathPrefix("playGame4Client") & get){
    parameter(
      'playerId.as[String],
      'playerName,
      'accessCode.as[String],
      'roomId.as[Long]) { (playerId,name, acCode,roomIdT) =>
      authPlatUser(acCode){ user =>
        val rstFutureForRoomInfo: Future[AllRoomInfoRsp] = roomManager ? (ref => GetAllRoomInfoReq(ref))
        dealFutureResult{
          rstFutureForRoomInfo.map {
            allRoomInfo =>
              val roomInfo = (1 to 3).map{ r =>
                (r.toLong,allRoomInfo.roomInfo.getOrElse(r.toLong,0))
              }.toMap
              val roomId = if(roomInfo.isEmpty) {
                //所有房间都是空，随机挑选一间进入
                (random.nextInt(3) + 1).toLong
              }else{
                val availableRoom = roomInfo.filter(_._2 < 10)
                if(availableRoom.isEmpty) {
                  //所有房间都是限定人数之上，则挑选人少的进入
                  roomInfo.minBy(_._2)._1
                }else{
                  //有限定人数以下的房间，则挑选人多的进入
                  availableRoom.maxBy(_._2)._1
                }
              }
              val rstFutureForPlayerJoin: Future[PlayerJoinRsp] = userManager ? (ref => PlayerJoinReq(playerId,name,roomId,ref))
              dealFutureResult{
                rstFutureForPlayerJoin.map{
                  rst =>
                    handleWebSocketMessages(rst.flow)
                }
              }
          }
        }

      }
    }
  }

//  val playGameWs4Client = (pathPrefix("playGame4Client") & get){
//    parameter(
//      'playerId.as[String],
//      'playerName,
//      'accessCode.as[String],
//      'roomId.as[Long]) { (playerId,name, acCode,roomId) =>
//      authPlatUser(acCode){user =>
//        val rstFuture: Future[PlayerJoinRsp] = userManager ? (ref => PlayerJoinReq(playerId,name,roomId,ref))
//        dealFutureResult{
//          rstFuture.map{
//            rst =>
//              handleWebSocketMessages(rst.flow)
//          }
//        }
//      }
//    }
//  }

  val watchGameWs = (pathPrefix("watchGame") & get){
    parameter(
      'playerId.as[String].?,
      'accessCode.as[String],
      'roomId.as[Long]) { (playerId, acCode,roomId) =>
      authPlatUser(acCode){ playerInfo =>
          val watcherId = playerInfo.playerId
          playerId match{
            case Some(playId) =>
              log.info(s"watcher is watching $playId")
              val rstFuture: Future[WatcherJoinRsp] = userManager ? (ref => WatcherJoinReq(playId,watcherId,playerInfo.nickname,roomId,ref))
              dealFutureResult{
                rstFuture.map{
                  rst =>
                    handleWebSocketMessages(rst.flow)
                }
              }

            case None => //随机分配玩家视角
              log.info("watcher join random watch")
              val resFuture: Future[GetPlayerInRoomRsp] = roomManager ? (ref =>GetUserInRoom(roomId,ref))
              dealFutureResult{
                resFuture.map{res =>
                  if(res.errCode == 0){
                    val order = random.nextInt(res.data.playerList.length)//从总玩家人数里随机选取次序
                    val id = res.data.playerList(order).playerId//随机获取房间内的玩家id
                    log.info(s"watcher is watching $id randomly")
                    val rstFuture: Future[WatcherJoinRsp] = userManager ? (ref => WatcherJoinReq(id,watcherId,playerInfo.nickname,roomId,ref))
                    dealFutureResult{
                      rstFuture.map{
                        rst =>
                          handleWebSocketMessages(rst.flow)
                      }
                    }
                  }
                  else{
                    log.info("room is empty")
                    val rstFuture: Future[WatcherJoinRsp] = userManager ? (ref => WatcherJoinReq("no player",watcherId,playerInfo.nickname,roomId,ref))
                    dealFutureResult{
                      rstFuture.map{
                        rst =>
                          handleWebSocketMessages(rst.flow)
                      }
                    }
                  }
                }
              }
          }
      }
    }
  }

  val replyGame = (pathPrefix("watchRecord") & get) {

    parameters(
      'recordId.as[Long],
      'playerId.as[String],
      'frame.as[Int],
      'accessCode.as[String]
    ) { (recordId,playerId,frame,accessCode) =>

      authPlatUser(accessCode) { playerInfo =>
        val rstFuture: Future[PlayerWatchRecordRsp] = userManager ? (ref => PlayerWatchRecordReq(playerInfo.playerId,playerId,playerInfo.nickname,recordId,frame,ref))
        dealFutureResult{
          rstFuture.map{
            rst =>
              handleWebSocketMessages(rst.flow)
          }
        }

      }

    }

  }

  val gameRoutes = pathPrefix("game") {
    playGameWs ~ playGameWs4Client ~ watchGameWs ~ replyGame
  }

}
