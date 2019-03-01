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

package org.seekloud.paradise.ptcl.paradise

import org.seekloud.paradise.ptcl.PDGameEvent.GameEvent
import org.seekloud.paradise.ptcl._

/**
  * User: easego
  * Date: 2018/11/1
  * Time: 13:59
  */

object Protocol {

  trait CommonRsp {
    val errCode: Int
    val msg: String
  }
  final case class ErrorRsp(
                             errCode: Int,
                             msg: String
                           ) extends CommonRsp

  final case class SuccessRsp(
                               errCode: Int = 0,
                               msg: String = "ok"
                             ) extends CommonRsp

  sealed trait MsgFromBackend

  case object CompleteMsg extends MsgFromBackend
  case class FailMsg(ex: Exception) extends MsgFromBackend

  trait GameMessageBeginning extends MsgFromBackend

  sealed trait GameMessage extends MsgFromBackend
  case class IdInfo(id: String,roomId: Long) extends GameMessage
  case class NewSnakeJoined(id: String, name: String) extends GameMessage
  case class NewWatcherJoined(count: Int, playerName: String) extends GameMessage
  case class Barriers(barrierDetails: List[Br]) extends GameMessage
  case class SyAD(appleDetails: List[Ap]) extends GameMessage //GridDataSyncAfterDead
  case class GridDataSync(frameCount: Long, snakes: List[SkDt], pointDetails: List[Tp], scale: Double = 1.0) extends GameMessage
  case class SnakeLeft(id: String, name: String) extends GameMessage
  case class SA(id: String, x: Double, y: Double, frame: Long) extends GameMessage //SnakeAction
  case class SnakeSpeed(id: String, isSpeedUp: Boolean, frame: Long) extends GameMessage
  case class NetDelayTest(createTime: Long) extends GameMessage
  case class ApplesRsp(apples: Map[Point,Spot]) extends GameMessage
  case class KillMessage(killedId: String, killedName:String, killerName:String, killedCount:Int, killedLength:Int) extends  GameMessage
  case class NewSnakes(snakes: List[SkDt], deads: List[(String,Point)], frameCount:Long) extends GameMessage
  case class Victory(VicSnakes: List[String]) extends GameMessage
  case class FA(aLs: List[Ap]) extends GameMessage //FeedApples
  case class EA(eatinfo:List[EatInfo]) extends GameMessage //EatApples
  case class RD(removePoints: List[(Point, Int, String)]) extends GameMessage //RemoveData
  case class Ranks(currentRank: List[Score], historyRank: List[Score]) extends GameMessage
  case class RC(currentRank: List[Score]) extends GameMessage   //RanksCurrent
  case class RH(historyRank: List[Score]) extends GameMessage  //RanksHistory
  case class TextMsg(msg: String) extends GameMessage
  case class Po(createTime: Long) extends GameMessage //PongData
  case object InitErrorMessage extends GameMessage
  case class PlayerIsDead() extends GameMessage
  case class PlayerIsNone() extends GameMessage
  case class PlayerLeft() extends GameMessage

  sealed trait WsSendMsg
  case object WsSendComplete extends WsSendMsg
  case class WsSendFailed(ex: Throwable) extends WsSendMsg

  sealed trait GameReply extends MsgFromBackend
  case class UserInfo(id: String) extends GameReply
  case class StartLoading(frame: Int, roomId: Long) extends GameReply
  case class StartReplay(firstSnapshotFrame: Int, firstReplayFrame: Int) extends GameReply
  case class ReplayFrameData(frameIndex: Int, eventsData: GameEvent, stateData: Option[GameEvent]) extends GameReply
  case class ReplayFinish(id: String) extends GameReply
  case class InitReplayError(info: String) extends GameReply
  case object InitErrorReply extends GameReply



  sealed trait MsgFromFront extends WsSendMsg
  case class Key(id: String,keyCode: Int,frame: Long) extends MsgFromFront
  case class MM(id: String, x: Double, y: Double, frame: Long) extends MsgFromFront   //MouseMove
  case class MS(id: String, frame: Long, speedUP: Boolean) extends MsgFromFront   //MouseSpeed
  case class NetTest(id: String, createTime: Long) extends MsgFromFront
  case class TextInfo(id: String, info: String) extends MsgFromFront
  case class ApplesReq(id: String) extends MsgFromFront
  case class AC(id: String, x: Double, y: Double,frame:Long) extends MsgFromFront //ActionCheck
  case class SC(id: String,isSpeedUp: Boolean,frame: Long) extends MsgFromFront  //SpeedCheck
  case class Pi(id: String,createTime: Long) extends MsgFromFront //PingData
  case class ReplyTextInfo(msg: String) extends MsgFromFront
  case object TickTock extends MsgFromFront


}

