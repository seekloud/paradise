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

package org.seekloud.paradise.actor

import akka.Done
import akka.actor.typed._
import akka.actor.typed.scaladsl.{Behaviors, TimerScheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Keep, Sink}
import akka.stream.typed.scaladsl.{ActorSink, ActorSource}
import akka.util.ByteString
import org.seekloud.byteobject.ByteObject.{bytesDecode, _}
import org.seekloud.byteobject.MiddleBufferInJvm
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContextExecutor, Future}
import org.seekloud.paradise.common.{AppSettings, StageContext}
import org.seekloud.paradise.controller.{GameController, LoginController}
import org.seekloud.paradise.ClientBoot.{executor, materializer, scheduler, system}
import org.seekloud.paradise.model.Protocol4Client.{HeartBeat, Ws4AgentRsp, WsData}
import org.seekloud.paradise.ptcl.paradise.Protocol.{MsgFromFront, WsSendComplete, WsSendFailed, WsSendMsg}



object LoginActor {
  private[this] val log = LoggerFactory.getLogger(this.getClass)

  sealed trait WsCommand

  case class EstablishConnection2Es(wsUrl: String) extends WsCommand

  def create(context: StageContext, loginController: LoginController): Behavior[WsCommand] = {
    Behaviors.setup[WsCommand] { ctx =>
      Behaviors.withTimers { implicit timer =>
        idle(context, loginController)(timer)
      }
    }
  }

  def idle(context: StageContext, loginController: LoginController)(implicit timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case EstablishConnection2Es(wsUrl: String) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(wsUrl))

          val source = getSource
          val sink = getSink4EstablishConnection(ctx.self, context, loginController)
          val response =
            source
              .viaMat(webSocketFlow)(Keep.right)
              .toMat(sink)(Keep.left)
              .run()
          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {

              Future.successful("LoginClient connect success. EstablishConnectionEs!")
            } else {
              throw new RuntimeException(s"LoginClient connection failed: ${upgrade.response.status}")
            }
          } //链接建立时
          connected.onComplete(i => log.info(i.toString))
          Behavior.same
      }
    }
  }

  def getSink4EstablishConnection(self: ActorRef[WsCommand], context: StageContext, loginController: LoginController):Sink[Message,Future[Done]] = {
    Sink.foreach {
      case TextMessage.Strict(msg) =>
        import io.circe.generic.auto._
        import io.circe.parser.decode

        import org.seekloud.paradise.controller.Api4GameAgent.linkGameAgent
        import org.seekloud.paradise.ClientBoot.executor

        val gameId = AppSettings.esheepGameId
        decode[WsData](msg) match {
          case Right(res) =>
            res match {
              case Ws4AgentRsp(data, errCode, errMsg) =>
                if (errCode != 0) {
                  log.debug(s"receive responseRsp error....$errMsg")
                } else {
                  val playerId = "user" + data.userId.toString
                  val playerName = data.nickname
                  linkGameAgent(gameId, playerId, data.token).map {
                    case Right(r) =>
                      loginController.switchToGaming(playerId, playerName, r.accessCode, r.gsPrimaryInfo.domain)

                    case Left(e) =>
                      log.debug(s"linkGameAgent..$e")
                  }
                }

              case HeartBeat =>
              //收到心跳消息不做处理
            }

          case Left(e) =>
            log.debug(s"decode esheep webmsg error! Error information:$e")
        }

      case unknown@_ =>
        log.debug(s"i receive an unknown msg:$unknown")
    }
  }

  private[this] def getSource = ActorSource.actorRef[WsSendMsg](
    completionMatcher = {
      case WsSendComplete =>
    }, failureMatcher = {
      case WsSendFailed(ex) ⇒ ex
    },
    bufferSize = 8,
    overflowStrategy = OverflowStrategy.fail
  ).collect {
    case message: MsgFromFront =>
      val sendBuffer = new MiddleBufferInJvm(409600)
      BinaryMessage.Strict(ByteString(
        message.fillMiddleBuffer(sendBuffer).result()
      ))

  }
}
