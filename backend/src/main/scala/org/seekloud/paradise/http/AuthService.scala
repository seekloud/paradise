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

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.util.Timeout
import org.seekloud.paradise.common.AppSettings
import akka.actor.typed.scaladsl.AskPattern._

import scala.concurrent.{ExecutionContextExecutor, Future}
import org.seekloud.paradise.Boot.{esheepSyncClient, executor, scheduler, timeout}
import org.seekloud.paradise.core.EsheepSyncClient
import org.seekloud.paradise.protocol.EsheepProtocol
import org.seekloud.paradise.ptcl.paradise.Protocol.ErrorRsp
import org.slf4j.LoggerFactory

trait AuthService extends ServiceUtils{

  import org.seekloud.utils.CirceSupport._
  import io.circe.generic.auto._

  private val log = LoggerFactory.getLogger(this.getClass)

  private def AuthUserErrorRsp(msg: String) = ErrorRsp(10001001, msg)

  val uid = new AtomicLong(1L)

  implicit val timeout: Timeout

  protected def authPlatUser(accessCode:String)(f: EsheepProtocol.PlayerInfo => server.Route):server.Route = {
    if(true) {
      import akka.util.{ByteString, Timeout}
      val verifyAccessCodeFutureRst: Future[EsheepProtocol.VerifyAccessCodeRsp] = esheepSyncClient ? (e => EsheepSyncClient.VerifyAccessCode(accessCode, e))
      dealFutureResult{
        verifyAccessCodeFutureRst.map{ rsp =>
          if(rsp.errCode == 0 && rsp.data.nonEmpty){
            f(rsp.data.get)
          } else{
            complete(AuthUserErrorRsp(rsp.msg))
          }
        }.recover{
          case e:Exception =>
            log.warn(s"verifyAccess code failed, code=$accessCode, error:${e.getMessage}")
            complete(AuthUserErrorRsp(e.getMessage))
        }
      }
    } else {
      val id = uid.getAndIncrement()
      f(EsheepProtocol.PlayerInfo(s"test_$id",s"test_$id"))
    }
  }

}
