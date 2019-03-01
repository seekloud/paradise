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

package org.seekloud.paradise.protocol

import org.seekloud.paradise.core.RoomManager
import akka.actor.typed.ActorRef
import org.seekloud.paradise.ptcl.PDGameEvent.UserJoinLeft

object EsheepProtocol {

  import org.seekloud.paradise.ptcl.paradise.Protocol._

  case class GameServerKey2TokenReq(
                                     gameId:Long,
                                     gsKey:String
                                   )

  case class GameServerKey2TokenInfo(
                                      token:String,
                                      expireTime:Long
                                    )

  case class GameServerKey2TokenRsp(
                                     data: Option[GameServerKey2TokenInfo],
                                     errCode: Int = 0,
                                     msg: String = "ok"
                                   ) extends CommonRsp


  case class PlayerInfo(
                         playerId:String,
                         nickname:String
                       )

  case class VerifyAccessCodeInfo(
                                   playerInfo:PlayerInfo
                                 )

  case class VerifyAccessCodeRsp(
                                  data: Option[PlayerInfo],
                                  errCode: Int = 0,
                                  msg: String = "ok"
                                ) extends CommonRsp

  /**获取录像内玩家列表*/
  case class GetUserInRecordReq(
                                 recordId:Long,
                                 playerId:String
                               )

  case class GetUserInRecordRsp(
                                 data:PlayerList,
                                 errCode: Int = 0,
                                 msg: String = "ok"
                               ) extends CommonRsp

  case class PlayerList(playerList:List[PlayerInfo])

  /**获取录像播放进度}*/
  case class GetRecordFrameReq(
                                recordId:Long,
                                playerId:String //正在观看玩家的ID
                              )

  case class GetRecordFrameRsp(
                                data:RecordFrameInfo,
                                errCode: Int = 0,
                                msg: String = "ok"
                              ) extends CommonRsp

  case class RecordFrameInfo(frame:Int,frameNum:Long,frameDuration: Long)

  //战绩信息
  case class BatRecordInfo(
                            playerId: String,
                            gameId: Long,
                            nickname: String,
                            killing: Int,
                            killed: Int,
                            score: Int,
                            gameExtent: String,
                            startTime: Long,
                            endTime: Long
                          )
  case class BatRecord(
                        playerRecord: BatRecordInfo
                      )


  //获取当前房间的玩家列表
  case class GetPlayerInRoomReq(roomId:Long)

  case class GetPlayerInRoomRsp(
    data:PlayerList,
    errCode: Int=0,
    msg:String="ok"
  )


  //获取进行游戏的房间列表
  case class GetRoomRsp(
    data:RoomList,
    errCode: Int=0,
    msg: String="ok"
  ) extends CommonRsp

  case class RoomList(
    roomList: List[Long]
  )

  //获取比赛录像列表
  //全量获取
  case class GetAllVideoreq(
    lastRecordId : Long,
    count:Int
  )
  //按时间筛选
  case class GetVideoByTimeReq(
    startTime:Long,
    endTime:Long,
    lastRecordId : Long,
    count:Int
  )
  //筛选指定玩家
  case class GetVideoByPlayerReq(
    playerId:String,
    lastRecordId : Long,
    count:Int
  )

  case class GetVideoRsp(
    data:Option[List[Video]],
    errCode:Int=0,
    msg:String="ok"
  )extends CommonRsp

  case class Video(
    recordId: Long,
    roomId: Long,
    startTime: Long,
    endTime: Long,
    userCounts: Int,
    userList: Seq[(String,String)]
  )

  //下载比赛录像
  case class downloadRecordReq(
    recordId:Long
  )

  //获取录像内玩家列表
  case class getRecordPlayerListReq(
    recordId:Long,
    playerId:String
  )

  case class getRecordPlayerListRsp(
    data:RecordPlayerList,
    errCode:Int =0,
    msg:String="ok"
  )extends CommonRsp

  case class RecordPlayerList(
    totalFrame:Long,
    playerList:List[PlayerInVideo]
  )

  case class PlayerInVideo(
    playerId:String,
    nickname:String,
    existTime:List[ExistTime]
  )

  case class ExistTime(
                        startFrame: Long,
                        endFrame: Long
                      )

}
