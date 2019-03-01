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

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.{ActorContext, StashBuffer, TimerScheduler}
import org.seekloud.paradise.common.AppSettings
import org.slf4j.LoggerFactory

import scala.language.implicitConversions
import scala.concurrent.duration._
import org.seekloud.utils.EsheepClient
import org.seekloud.paradise.Boot.executor
import org.seekloud.paradise.protocol.EsheepProtocol
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.protocol.EsheepProtocol.GameServerKey2TokenRsp

import scala.util.{Failure, Success}

object EsheepSyncClient {

  private val log = LoggerFactory.getLogger(this.getClass)

  private final val InitTime = Some(5.minutes)
  private final val GetTokenTime = Some(5.minutes)
  private final val ReTryTime = 20.seconds
  private final case object BehaviorChangeKey
  private final case object RefreshTokenKey

  sealed trait Command

  final case class SwitchBehavior(
                                   name: String,
                                   behavior: Behavior[Command],
                                   durationOpt: Option[FiniteDuration] = None,
                                   timeOut: TimeOut = TimeOut("busy time error")
                                 ) extends Command

  case class TimeOut(msg:String) extends Command


  final case object RefreshToken extends Command

  final case class VerifyToken(rsp:ActorRef[GameServerKey2TokenRsp]) extends Command
  final case class VerifyAccessCode(accessCode:String, rsp:ActorRef[EsheepProtocol.VerifyAccessCodeRsp]) extends Command
  final case class InputRecord(playerId:String,nickname: String, killing: Int, killed:Int, score: Int, startTime: Long, endTime: Long ) extends Command

  private[this] def switchBehavior(ctx: ActorContext[Command],
                                   behaviorName: String, behavior: Behavior[Command], durationOpt: Option[FiniteDuration] = None,timeOut: TimeOut  = TimeOut("busy time error"))
                                  (implicit stashBuffer: StashBuffer[Command],
                                   timer:TimerScheduler[Command]) = {
    log.debug(s"${ctx.self.path} becomes $behaviorName behavior.")
    timer.cancel(BehaviorChangeKey)
    durationOpt.foreach(timer.startSingleTimer(BehaviorChangeKey,timeOut,_))
    stashBuffer.unstashAll(ctx,behavior)
  }


  def create: Behavior[Command] = {
    Behaviors.setup[Command]{ctx =>
      log.debug(s"${ctx.self.path} is starting...")
      implicit val stashBuffer = StashBuffer[Command](Int.MaxValue)
      Behaviors.withTimers[Command] { implicit timer =>
        ctx.self ! RefreshToken
        switchBehavior(ctx,"init",init(),InitTime,TimeOut("init"))
      }
    }
  }


  private def init()(
    implicit stashBuffer:StashBuffer[Command],
    timer:TimerScheduler[Command]
  ): Behavior[Command] = {
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case RefreshToken =>
          if(AppSettings.esheepAuthToken){
            EsheepClient.gsKey2Token().onComplete{
              case Success(rst) =>
                rst match {
                  case Right(rsp) =>
                    ctx.self ! SwitchBehavior("work",work(rsp))

                  case Left(error) =>
                    log.error(s"${ctx.self.path} get token failed.error:${error.msg}")
                    ctx.self ! SwitchBehavior("stop", Behaviors.stopped)
                }
              case Failure(error) =>
                log.error(s"${ctx.self.path} get token failed.,error:${error.getMessage}")
                ctx.self ! SwitchBehavior("stop", Behaviors.stopped)
            }
            switchBehavior(ctx, "busy", busy(), GetTokenTime, TimeOut("Get Token"))
          } else{
            switchBehavior(ctx, "work", work(EsheepProtocol.GameServerKey2TokenInfo("",System.currentTimeMillis() + 2.days.toMillis)))
          }



        case TimeOut(m) =>
          log.error(s"${ctx.self.path} is time out when busy,msg=$m")
          Behaviors.stopped

        case unknownMsg =>
          stashBuffer.stash(unknownMsg)
          Behavior.same
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

        case unknowMsg =>
          stashBuffer.stash(unknowMsg)
          Behavior.same
      }
    }


  private def work(
                    tokenInfo:EsheepProtocol.GameServerKey2TokenInfo
                  )(
                    implicit stashBuffer:StashBuffer[Command],
                    timer:TimerScheduler[Command]
                  ): Behavior[Command] = {
    timer.startSingleTimer(RefreshTokenKey, RefreshToken, tokenInfo.expireTime.seconds)
    Behaviors.receive[Command] { (ctx, msg) =>
      msg match {
        case VerifyAccessCode(accessCode, rsp) =>

          EsheepClient.verifyAccessCode(accessCode, tokenInfo.token).onComplete{
            case Success(rst) =>
              rst match {
                case Right(value) => rsp ! EsheepProtocol.VerifyAccessCodeRsp(Some(value))
                case Left(error) =>
                  println(error)
                  handleErrorRsp(ctx, msg, error) { () =>
                    rsp !errorRsp2VerifyAccessCodeRsp(error)
                  }
              }
            case Failure(exception) =>
              log.warn(s"${ctx.self.path} VerifyAccessCode failed, error:${exception.getMessage}")
          }
          Behaviors.same

        case RefreshToken =>
          ctx.self ! RefreshToken
          timer.cancel(RefreshTokenKey)
          switchBehavior(ctx,"init",init(),InitTime,TimeOut("init"))

        case r:InputRecord =>
          EsheepClient.inputBatRecorder(tokenInfo.token,r.playerId.toString,r.nickname,r.killing,r.killed,r.score,"",r.startTime,r.endTime).onComplete{
            case Success(rst) =>
              rst match {
                case Right(value) =>
                  log.info(s"${ctx.self.path} input record success")
                case Left(error) =>
                  log.error(s"${ctx.self.path} input record fail,error: $error")
              }
            case Failure(exception) =>
              log.warn(s"${ctx.self.path} input record fail,error: $exception")
          }
          Behaviors.same

        case VerifyToken(rsp) =>
          rsp ! EsheepProtocol.GameServerKey2TokenRsp(Some(tokenInfo))
          Behaviors.same

        case unknowMsg =>
          log.warn(s"${ctx.self.path} recv an unknow msg=$msg")
          Behaviors.same

      }


    }
  }

  implicit def errorRsp2VerifyAccessCodeRsp(errorRsp: ErrorRsp): EsheepProtocol.VerifyAccessCodeRsp =  EsheepProtocol.VerifyAccessCodeRsp(None, errorRsp.errCode, errorRsp.msg)

  private def handleErrorRsp(ctx:ActorContext[Command],msg:Command,errorRsp:ErrorRsp)(unknownErrorHandler:() => Unit) = {
    errorRsp.errCode match {
      case 200003 =>
        //token过期处理，gameId与token不匹配, token有误
        ctx.self ! RefreshToken
        ctx.self ! msg

      case 200004 =>
        //token过期处理，token不存在或已过期，或gameId有误
        ctx.self ! RefreshToken
        ctx.self ! msg

//      case 200005 =>
//        //token过期处理，accessCode不存在或已过期
//        ctx.self ! RefreshToken
//        ctx.self ! msg

      case _ =>
        unknownErrorHandler()
    }

  }

}
