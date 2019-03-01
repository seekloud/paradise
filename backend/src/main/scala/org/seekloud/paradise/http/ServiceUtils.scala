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

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.seekloud.paradise.common.AppSettings
import org.seekloud.paradise.ptcl.paradise.Protocol.ErrorRsp
import org.seekloud.utils.{CirceSupport, SecureUtil}
import org.seekloud.utils.SecureUtil.PostEnvelope
import org.slf4j.LoggerFactory
import io.circe.parser.decode
import io.circe.Error
import io.circe.Decoder

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * User: Taoz
  * Date: 11/18/2016
  * Time: 7:57 PM
  */

object ServiceUtils{
  private val log = LoggerFactory.getLogger(this.getClass)

  case class CommonRsp(errCode: Int = 0, msg: String = "ok")

  final val SignatureError = CommonRsp(1000001, "signature error.")

  final val RequestTimeout = CommonRsp(1000003, "request timestamp is too old.")

  final val AppClientIdError = CommonRsp(1000002, "appClientId error.")

  final val INTERNAL_ERROR = CommonRsp(10001, "Internal error.")

  final val JsonParseError = CommonRsp(10002, "Json parse error.")
}

trait ServiceUtils extends CirceSupport{

  import ServiceUtils._
  import io.circe.generic.auto._
  import org.seekloud.utils.CirceSupport._


  def htmlResponse(html: String): HttpResponse = {
    HttpResponse(entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, html))
  }

  def jsonResponse(json: String): HttpResponse = {
    HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, json))
  }

  def textResponse(text: String): HttpResponse = {
    HttpResponse(entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, text))
  }

  def dealFutureResult(future: => Future[server.Route]): Route = {
    onComplete(future) {
      case Success(rst) => rst
      case Failure(e) =>
        e.printStackTrace()
        log.error("internal error: {}", e.getMessage)
        complete(ErrorRsp(1000, "internal error."))
    }
  }

  def ensureAuth(
                  appClientId: String,
                  timestamp: String,
                  nonce: String,
                  sn: String,
                  data: List[String],
                  signature: String
                )(f: => Future[server.Route]): server.Route = {
    val p = getSecureKey(appClientId) match {
      case Some(secureKey) =>
        val paramList = List(appClientId.toString, timestamp, nonce, sn) ::: data
        if (timestamp.toLong + 120000 < System.currentTimeMillis()) {
          Future.successful(complete(RequestTimeout))
        } else if (SecureUtil.checkSignature(paramList, signature, secureKey)) {
          f
        } else {
          Future.successful(complete(SignatureError))
        }
      case None =>
        Future.successful(complete(AppClientIdError))
    }
    dealFutureResult(p)
  }

  def ensurePostEnvelope(e: PostEnvelope)(f: => Future[server.Route]) = {
    ensureAuth(e.appId, e.timestamp, e.nonce, e.sn, List(e.data), e.signature)(f)
  }

  private def getSecureKey(appId: String) = AppSettings.appSecureMap.get(appId)

  def dealPostReq[A](f: A => Future[server.Route])(implicit decoder: Decoder[A]): server.Route = {
    entity(as[Either[Error, PostEnvelope]]) {
      case Right(envelope) =>
        ensurePostEnvelope(envelope) {
          decode[A](envelope.data) match {
            case Right(req) =>
              f(req)

            case Left(e) =>
              log.error(s"json parse detail type error: $e")
              Future.successful(complete(JsonParseError))
          }
        }

      case Left(e) =>
        log.error(s"json parse PostEnvelope error: $e")
        complete(JsonParseError)
    }
  }

  def dealPostReqWithoutData(f:  => Future[server.Route]): server.Route = {
    entity(as[Either[Error, PostEnvelope]]) {
      case Right(envelope) =>
        ensurePostEnvelope(envelope) {
          f
        }

      case Left(e) =>
        log.error(s"json parse PostEnvelope error: $e")
        complete(JsonParseError)
    }
  }

  def dealGetReq(f: => Future[server.Route]): server.Route = {
    entity(as[Either[Error, PostEnvelope]]) {
      case Right(envelope) =>
        ensurePostEnvelope(envelope) {
          f
        }

      case Left(e) =>
        log.error(s"json parse PostEnvelope error: $e")
        complete(JsonParseError)
    }
  }

}
