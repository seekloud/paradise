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

package org.seekloud.utils

import org.seekloud.paradise.common.AppSettings
import org.seekloud.utils.SecureUtil.{PostEnvelope, genPostEnvelope}
import org.slf4j.LoggerFactory
import org.seekloud.paradise.Boot.executor
import org.seekloud.paradise.ptcl.paradise.Protocol.{ErrorRsp, SuccessRsp}

import scala.concurrent.Future

object EsheepClient extends HttpUtil {
  import io.circe.parser.decode
  import io.circe.syntax._
  import io.circe._
  import io.circe.generic.auto._
  import org.seekloud.paradise.protocol.EsheepProtocol

  private val log = LoggerFactory.getLogger(this.getClass)

  private val baseUrl = s"${AppSettings.esheepProtocol}://${AppSettings.esheepHost}:${AppSettings.esheepPort}"

  private val gameId = AppSettings.esheepGameId
  private val gameServerKey = AppSettings.esheepGameKey

  def gsKey2Token(): Future[Either[ErrorRsp,EsheepProtocol.GameServerKey2TokenInfo]] = {
    val methodName = s"gsKey2Token"
    val url = s"$baseUrl/esheep/api/gameServer/gsKey2Token"
    val data = EsheepProtocol.GameServerKey2TokenReq(gameId,gameServerKey).asJson.noSpaces



    postJsonRequestSend(methodName,url,Nil,data).map{
      case Right(jsonStr) =>
        decode[EsheepProtocol.GameServerKey2TokenRsp](jsonStr) match {
          case Right(rsp) =>
            if(rsp.errCode == 0 && rsp.data.nonEmpty){
              Right(rsp.data.get)
            }else{
              log.debug(s"$methodName failed,error:${rsp.msg}")
              Left(ErrorRsp(rsp.errCode, rsp.msg))
            }
          case Left(error) =>
            log.warn(s"$methodName parse json error:${error.getMessage}")
            Left(ErrorRsp(-1, error.getMessage))
        }
      case Left(error) =>
        log.debug(s"$methodName  failed,error:${error.getMessage}")
        Left(ErrorRsp(-1,error.getMessage))
    }
  }

  def verifyAccessCode(accessCode:String,token:String): Future[Either[ErrorRsp,EsheepProtocol.PlayerInfo]] = {
    val methodName = s"verifyAccessCode"
    val url = s"$baseUrl/esheep/api/gameServer/verifyAccessCode?token=$token"

    val data = Json.obj(
      ("gameId",gameId.asJson),
      ("accessCode",accessCode.asJson)
    ).noSpaces


    postJsonRequestSend(methodName,url,Nil,data).map{
      case Right(jsonStr) =>
        decode[EsheepProtocol.VerifyAccessCodeRsp](jsonStr) match {
          case Right(rsp) =>
            if(rsp.errCode == 0 && rsp.data.nonEmpty){
              Right(rsp.data.get)
            }else{
              log.debug(s"$methodName failed,error:${rsp.msg}")
              Left(ErrorRsp(rsp.errCode, rsp.msg))
            }
          case Left(error) =>
            log.warn(s"$methodName parse json error:${error.getMessage}")
            Left(ErrorRsp(-1, error.getMessage))
        }
      case Left(error) =>
        log.debug(s"$methodName  failed,error:${error.getMessage}")
        Left(ErrorRsp(-1,error.getMessage))
    }
  }


  def inputBatRecorder(token:String, playerId: String, nickname: String, killing: Int, killed: Int, score: Int, gameExtent: String, startTime: Long, endTime: Long): Future[Either[String,String]]={
    val methodName = s"addPlayerRecord"
    val url = s"$baseUrl/esheep/api/gameServer/addPlayerRecord?token=$token"

    val info = EsheepProtocol.BatRecordInfo(playerId,gameId,nickname,killing,killed,score,gameExtent,startTime,endTime)
    val data = EsheepProtocol.BatRecord(info).asJson.noSpaces


    postJsonRequestSend(methodName,url,Nil,data).map{
      case Right(jsonStr) =>
        decode[SuccessRsp](jsonStr) match {
          case Right(rsp) =>
            if(rsp.errCode == 0){
              Right(s"$methodName success")
            }else{
              log.debug(s"$methodName failed,error:${rsp.msg}")
              Left("error")
            }
          case Left(error) =>
            log.warn(s"$methodName parse json error:${error.getMessage}")
            Left("error")
        }
      case Left(error) =>
        log.debug(s"$methodName  failed,error:${error.getMessage}")
        Left("error")
    }



  }




}
