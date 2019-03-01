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

package org.seekloud.paradise.controller

import org.seekloud.paradise.model.Protocol4Client._
import org.slf4j.LoggerFactory
import org.seekloud.paradise.utils.HttpUtil
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import org.seekloud.paradise.ClientBoot.executor
import org.seekloud.paradise.common.AppSettings

import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by nwh on 2018/10/24.
  */
object Api4GameAgent extends  HttpUtil{


  private[this] val log = LoggerFactory.getLogger(this.getClass)

  def getLoginRspFromEs() = {
    val methodName = "GET"
    val url = "http://" + AppSettings.esheepDomain + "/esheep/api/gameAgent/login"
    log.info("start getLoginRspFromEs.")
    getRequestSend(methodName, url, Nil).map {
      case Right(r) =>
        decode[LoginRsp](r) match {
          case Right(rsp) =>
            log.info("end getLoginRspFromEs.")
            Right(UrlData(rsp.data.wsUrl, rsp.data.scanUrl.replaceFirst("data:image/png;base64,", "")))
          case Left(e) =>
            Left(s"error:$e")
        }
      case Left(e) =>
        log.info(s"$e")
        Left("error")
    }
  }

  //fixme 尚未添加bot玩家
  def linkGameAgent(gameId: Long, playerId: String, token: String) = {
    val data = LinkGameAgentReq(gameId, playerId).asJson.noSpaces
    val url = "http://" + AppSettings.esheepDomain + "/esheep/api/gameAgent/joinGame?token=" + token

    postJsonRequestSend("post", url, Nil, data).map {
      case Right(jsonStr) =>
        println(s"linkGameAgent: $jsonStr")
        decode[LinkGameAgentRsp](jsonStr) match {
          case Right(res) =>
            Right(LinkGameAgentData(res.data.accessCode, res.data.gsPrimaryInfo))
          case Left(le) =>
            Left("decode error: " + le)
        }
      case Left(erStr) =>
        Left("get return error:" + erStr)
    }

  }

  def main(args: Array[String]): Unit = {
    getLoginRspFromEs()
  }

}

