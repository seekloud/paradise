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

import java.net.URLEncoder

import akka.actor.typed._
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer, TimerScheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink}
import akka.stream.typed.scaladsl.ActorSource
import akka.util.{ByteString, ByteStringBuilder}
import org.seekloud.paradise.controller.GameController
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.byteobject.ByteObject.{bytesDecode, _}
import org.seekloud.byteobject.MiddleBufferInJvm
import org.slf4j.LoggerFactory
import org.seekloud.paradise.ClientBoot.{executor, materializer, scheduler, system}
import org.seekloud.paradise.scene.Performance

import scala.concurrent.Future
import scala.util.Random

/**
  * Created by dry on 2018/10/23.
  **/
object PlayGameWebSocket {

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  sealed trait WsCommand

  case class ConnectGame(id:String,name:String,roomId:String,accessCode:String, domain: String) extends WsCommand

  case class MsgToService(sendMsg: WsSendMsg) extends WsCommand

  def create(gameController: GameController): Behavior[WsCommand] = {
    Behaviors.setup[WsCommand] { ctx =>
      implicit val stashBuffer: StashBuffer[WsCommand] = StashBuffer[WsCommand](Int.MaxValue)
      Behaviors.withTimers { implicit timer =>
        idle(gameController)
      }
    }
  }

  def idle(gameController: GameController)(implicit stashBuffer: StashBuffer[WsCommand], timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case ConnectGame(id,name,roomId,accessCode, domain) =>
          val webSocketFlow = Http().webSocketClientFlow(WebSocketRequest(getWebSocketUri(id, name, roomId, accessCode, domain)))
          val source = getSource
          val sink = getSink(gameController)
          val ((stream, response), closed) =
            source
              .viaMat(webSocketFlow)(Keep.both) // keep the materialized Future[WebSocketUpgradeResponse]
              .toMat(sink)(Keep.both) // also keep the Future[Done]
              .run()

          val connected = response.flatMap { upgrade =>
            if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
              gameController.openConnect()
              Future.successful("connect success")
            } else {
              throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
            }
          } //ws建立
          closed.onComplete { _ =>
            log.info("connect to service closed!")
            gameController.loseConnect()
          } //ws断开
          connected.onComplete(i => log.info(i.toString))
          connecting(stream)

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  def connecting(actor: ActorRef[WsSendMsg])(implicit stashBuffer: StashBuffer[WsCommand], timer: TimerScheduler[WsCommand]): Behavior[WsCommand] = {
    Behaviors.receive[WsCommand] { (ctx, msg) =>
      msg match {
        case m@MsgToService(sendMsg) =>
          actor ! sendMsg
          Behaviors.same

        case unknown@_ =>
          log.debug(s"i receive an unknown msg:$unknown")
          Behaviors.unhandled
      }
    }
  }

  private[this] def getSink(gameController: GameController) =
    Sink.foreach[Message] {
      case TextMessage.Strict(msg) =>
        log.debug(s"msg from webSocket: $msg")

      case BinaryMessage.Strict(bMsg) =>
        Performance.setDataSize(bMsg.size)
        //decode process.
        val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
        val a = bytesDecode[GameMessage](buffer)
        a match {
          case Right(v) => gameController.gameMessageReceiver(v)
          case Left(e) =>
            println(s"strict decode error: ${e.message} ")
        }

      case msg:BinaryMessage.Streamed =>
        val f = msg.dataStream.runFold(new ByteStringBuilder().result()){
          case (s, str) => s.++(str)
        }

        f.map { bMsg =>
          Performance.setDataSize(bMsg.size)
          val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
          bytesDecode[GameMessage](buffer) match {
            case Right(v) => gameController.gameMessageReceiver(v)
            case Left(e) =>
              println(s"stream decode error: ${e.message}")
          }
        }

      case unknown@_ =>
        log.debug(s"i receiver an unknown message:$unknown")
    }

  private[this] def getSource = ActorSource.actorRef[WsSendMsg](
    completionMatcher = {
      case WsSendComplete =>
    }, failureMatcher = {
      case WsSendFailed(ex) ⇒ ex
    },
    bufferSize = 64,
    overflowStrategy = OverflowStrategy.fail
  ).collect {
    case message: MsgFromFront =>
      val sendBuffer = new MiddleBufferInJvm(409600)
      BinaryMessage.Strict(ByteString(
        message.fillMiddleBuffer(sendBuffer).result()
      ))

  }

  def getWebSocketUri(playerId: String, playerName: String, roomId:String,accessCode: String, domain: String): String = {
    val wsProtocol = "ws"
    val domain = "10.1.29.250:30370"
    val name = URLEncoder.encode(playerName, "UTF-8")
    s"$wsProtocol://$domain/paradise/game/playGame4Client?playerId=$playerId&playerName=$name&accessCode=$accessCode&roomId=$roomId"
    //    s"$wsProtocol://$domain/paradise/join?id=$playerId&name=$playerName"
  }

}
