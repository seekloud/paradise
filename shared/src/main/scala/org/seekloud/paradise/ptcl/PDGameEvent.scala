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

package org.seekloud.paradise.ptcl

import org.seekloud.paradise.ptcl.paradise.Protocol.GameMessage

/**
  * Created by Anjiansan on 2018/11/05.
  **/
object PDGameEvent {

  sealed trait GameEvent

  case class Snapshot(
                       snakes: List[SkDt],
                       pointDetails:List[Tp],
                       apples: List[Ap],
                       barriers: List[Br],
                       killHistory: List[Score]
                     ) extends GameEvent


  case class GameInformation(
                              roomId: Long,
                              startTime: Long,
                              index: Int,
                              initFrame: Long
                            )

  case class UserJoinLeft(joinFrame: Long, leftFrame: Long)

  case class UserBaseInfo(id:String, name: String)

  case class JoinEvent(id: String, snakeInfo: Option[SkDt]) extends GameEvent

  case class LeftEvent(id: String, nickName: String) extends GameEvent

  case class EssfMapInfo(m:List[(UserBaseInfo, List[UserJoinLeft])])

  case class SpaceEvent(id: String,name: String,header: Point,color: String) extends GameEvent

  case class MouseEvent(id: String, x: Double, y: Double) extends GameEvent

  case class SpeedEvent(id: String, isSpeedUp: Boolean) extends GameEvent

  case class VictoryEvent(ids: List[String]) extends GameEvent

  case class RankEvent(currentRank: List[Score], historyRank: List[Score]) extends GameEvent

  case class KillEvent(killedId: String, killedName:String, killerName:String, killedCount:Int, killedLength:Int) extends  GameEvent

  case class DeadEvent(snakes: List[SkDt], deads: List[(String,Point)], frameCount:Long) extends GameEvent

  case class RemoveEvent(removePoints: List[(Point, Int, String)]) extends GameEvent

  case class FeedAppleEvent(aLs: List[Ap]) extends GameEvent

  case class EatenAppleEvent(eatInfo:List[EatInfo]) extends GameEvent

  case class EventData(events: List[GameEvent]) extends GameEvent

  case class DecodeError() extends GameEvent

//  case class ReplayFrameData(frameIndex: Int, eventsData: GameEvent, stateData: Option[GameEvent]) extends GameMessage





}
