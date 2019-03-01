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

import java.io.File

import org.seekloud.paradise.common.AppSettings
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.stream.scaladsl.FileIO

import scala.language.implicitConversions
import org.slf4j.LoggerFactory
import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorAttributes, Materializer, Supervision}
import akka.util.{ByteString, Timeout}
import akka.actor.typed.scaladsl.AskPattern._
import org.seekloud.paradise.Boot._
import org.seekloud.paradise.core.{RoomManager, UserManager}
import org.seekloud.paradise.core.RoomManager.GetUserInRoom
import org.seekloud.paradise.protocol.EsheepProtocol._
import org.seekloud.paradise.ptcl.paradise.Protocol.{CommonRsp, ErrorRsp}

import scala.concurrent.{ExecutionContextExecutor, Future}
import org.seekloud.paradise.models.DAO.RecordDAO
import org.seekloud.utils.ESSFSupport.{initInput, userMapDecode, metaDataDecode}


/**
  * User: shuai
  * Date: 2018/10/31
  * Time: 10:08
  */
trait EsheepApiService extends ServiceUtils{
  import io.circe._
  import io.circe.generic.auto._

  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: Materializer
  implicit val scheduler: Scheduler
  implicit val timeout: Timeout

  private val log = LoggerFactory.getLogger(getClass)

  private val getRoomList = (path("getRoomList") & post & pathEndOrSingleSlash) {
    dealPostReqWithoutData{
      val msg: Future[List[Long]] = roomManager ? (RoomManager.GetAllRoom(_))
      msg.map{
        rst =>
          if(rst.nonEmpty)
            complete(GetRoomRsp(RoomList(rst)))
          else
            complete(ErrorRsp(100000, "no room"))
      }
    }
  }

  private def getRoomPlayerListErrorRsp(msg:String) = ErrorRsp (100003,msg)

  private val getRoomPlayerList = (path("getRoomPlayerList") & post) {
    dealPostReq[GetPlayerInRoomReq] {
      req =>
        val rstFuture:Future[GetPlayerInRoomRsp] = roomManager ? (GetUserInRoom(req.roomId,_))
        rstFuture.map{
          rst =>
            if(rst.data.playerList.nonEmpty)
              complete(rstFuture)
            else
              complete(ErrorRsp(100000, "no user in room"))
        }
    }
  }

  private def getGameVideoListErrorRsp(msg:String) = ErrorRsp(100004,msg)
  private val getRecordList = (path("getRecordList") & post) {

    dealPostReq[GetAllVideoreq]{
      req =>
        RecordDAO.getRecordList(req.lastRecordId,req.count).map{
          r =>
            val game = r.groupBy(_._1)
            var gameList = List.empty[Video]
            for((i,j) <- game){
                gameList = gameList :+ Video(i.recordId,i.roomId,i.startTime,i.endTime,j.length,j.map(r => (r._2.userId,r._2.nickname)))
            }
            complete(GetVideoRsp(Some(gameList.sortBy(_.recordId))))
        }.recover{
          case e :Exception =>
            log.debug(s"获取游戏录像失败，recover error:$e")
            complete(getGameVideoListErrorRsp(s"获取游戏录像失败，recover error:$e"))

        }

    }

  }

  private val getRecordListByTime =(path("getRecordListByTime") & post) {
    dealPostReq[GetVideoByTimeReq]{
      req =>
        RecordDAO.getRecordListByTime(req.startTime,req.endTime,req.lastRecordId,req.count).map{
          r =>
            val game = r.groupBy(_._1)
            var gameList = List.empty[Video]
            for((i,j) <- game){
              gameList = gameList :+ Video(i.recordId,i.roomId,i.startTime,i.endTime,j.length,j.map(r => (r._2.userId,r._2.userId)))
            }
            complete(GetVideoRsp(Some(gameList.sortBy(_.recordId))))
        }.recover{
          case e:Exception =>
            log.debug(s"获取游戏录像失败，recover error:$e")
            complete(getGameVideoListErrorRsp(s"获取游戏录像失败，recover error:$e"))
        }
    }

  }

  private val getRecordListByPlayer = (path("getRecordListByPlayer") & post) {
    dealPostReq[GetVideoByPlayerReq]{
      req =>
        RecordDAO.getRecordListByPlayer(req.playerId,req.lastRecordId,req.count).map{
          r =>
            val game = r.groupBy(_._1)
            var gameList = List.empty[Video]
            for((i,j) <- game){
              gameList = gameList :+ Video(i.recordId,i.roomId,i.startTime,i.endTime,j.length,j.map(r => (r._2.userId,r._2.userId)))
            }
            complete(GetVideoRsp(Some(gameList.sortBy(_.recordId))))
        }.recover{
          case e:Exception =>
            log.debug(s"获取游戏录像失败，recover error:$e")
            complete(getGameVideoListErrorRsp(s"获取游戏录像失败，recover error:$e"))
        }
    }
  }

  private def downloadRecordErrorRsp(msg:String) = ErrorRsp(100005,msg)
  private val downloadRecord = (path("downloadRecord") & post){
    dealPostReq[downloadRecordReq]{
      req =>

          RecordDAO.getFilePath(req.recordId).map{
            r =>
              val file = r.head
              val f = new File(file)
              if(f.exists()) {
                val responseEntity = HttpEntity(
                  ContentTypes.`application/octet-stream`,
                  file.length,
                  FileIO.fromPath(f.toPath, chunkSize = 262144)
                )
                complete(responseEntity)
              }else{
                complete("can not download record")
              }

          }.recover{
            case e:Exception =>
              log.debug(s"下载录像失败$e")
              complete(downloadRecordErrorRsp(s"下载录像失败$e"))
          }



    }

  }

  private val getRecordFrame = (path("getRecordFrame") & post & pathEndOrSingleSlash) {
    dealPostReq[GetRecordFrameReq] { req =>
      val rstF: Future[CommonRsp] = userManager ? (UserManager.GetRecordFrame(req.recordId, req.playerId, _))
      rstF.map {
        case rsp: GetRecordFrameRsp =>
          complete(rsp)
        case _ =>
          complete(ErrorRsp(100001, "录像不存在或已损坏"))
      }.recover{
        case e:Exception =>
          log.debug(s"获取游戏录像失败，recover error:$e")
          complete(ErrorRsp(100001, "录像不存在或已损坏"))
      }
    }
  }

  private val getRecordPlayerList = (path("getRecordPlayerList")& post & pathEndOrSingleSlash){
    dealPostReq[getRecordPlayerListReq] { req =>
      RecordDAO.getRecordById(req.recordId).map {
        case Some(r) =>
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
            val frameCount = info.frameCount
            val initFrame = metaDataDecode(info.simulatorMetadata).right.get.initFrame
            val playerList = userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m
            val playerInfo = playerList.map { ls =>
              val existTime = ls._2.map { f => ExistTime(f.joinFrame - initFrame, f.leftFrame - initFrame) }
              PlayerInVideo(ls._1.id, ls._1.name, existTime)
            }
            complete(getRecordPlayerListRsp(RecordPlayerList(frameCount, playerInfo)))
          } catch {
            case e: Exception =>
              log.debug(s"get record player list error: $e")
              complete(ErrorRsp(100001,"文件不存在或已损坏"))
          }

        case None =>
          complete(ErrorRsp(111000, s"can not find record-${req.recordId}"))
      }
    }
  }

  val roomApiRoutes: Route =  {
   getRoomList ~ getRoomPlayerList ~ getRecordList ~ getRecordListByTime ~ getRecordListByPlayer ~downloadRecord ~ getRecordPlayerList ~ getRecordFrame
  }


}
