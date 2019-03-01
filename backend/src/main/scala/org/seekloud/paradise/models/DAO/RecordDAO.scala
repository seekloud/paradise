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

package org.seekloud.paradise.models.DAO

import org.seekloud.utils.DBUtil.db
import slick.jdbc.PostgresProfile.api._
import org.seekloud.paradise.models.SlickTables._
import org.seekloud.paradise.Boot.executor
import net.sf.ehcache.search.aggregator.Count

import scala.concurrent.Future

/**
  * User: Jason
  * Date: 2018/10/22
  * Time: 14:49
  */
object RecordDAO {
  def getRecordList(lastRecord: Long,count: Int)= {
    if(lastRecord == 0L){
      val action = {
        tGameRecord.sortBy(_.recordId.desc).take(count) join tUserInRecord on { (game, user) =>
          game.recordId === user.recordId
        }
      }.result
      db.run(action)
    }
    else{
      val action = {
        tGameRecord.filter(_.recordId < lastRecord).sortBy(_.recordId.desc).take(count) join tUserInRecord on { (game, user) =>
          game.recordId === user.recordId
        }
      }.result
      db.run(action)
    }
  }

  def getRecordListByTime(startTime: Long,endTime: Long,lastRecord: Long,count: Int) = {
    if(lastRecord == 0L){
      val action = {
        tGameRecord.sortBy(_.recordId.desc) join tUserInRecord on { (game, user) =>
          (game.recordId === user.recordId) && game.startTime >= startTime && game.endTime <= endTime
        }
      }.result
      db.run(action)
    }
    else{
      val action = {
        tGameRecord.filter(_.recordId < lastRecord).sortBy(_.recordId.desc) join tUserInRecord on { (game, user) =>
          game.recordId === user.recordId && game.startTime >= startTime && game.endTime <= endTime
        }
      }.result
      db.run(action)
    }

  }

  def getRecordListByPlayer(playerId: String,lastRecord: Long,count: Int) = {
    if(lastRecord == 0L){
      val action = {
        tGameRecord.sortBy(_.recordId.desc) join tUserInRecord on { (game, user) =>
          game.recordId === user.recordId
        }
      }.result
      db.run(action)
    }
    else{
      val action = {
        tGameRecord.filter(_.recordId < lastRecord).sortBy(_.recordId.desc) join tUserInRecord on { (game, user) =>
          game.recordId === user.recordId
        }
      }.result
      db.run(action)
    }
  }

  def getRecordPath(recordId: Long) = db.run(
    tGameRecord.filter(_.recordId === recordId).map(_.filePath).result.headOption
  )

  def saveGameRecorder(roomId: Long, startTime: Long, endTime: Long, filePath: String): Future[Long] = {
    db.run{
      tGameRecord.returning(tGameRecord.map(_.recordId)) += rGameRecord(-1l, roomId, startTime, endTime, filePath)
    }
  }

  def saveUserInGame(users: Set[rUserInRecord]) = {
    db.run(tUserInRecord ++= users)
  }

  def getRecordById(id:Long)={
    db.run(tGameRecord.filter(_.recordId===id).result.headOption)
  }

  def getFilePath(id:Long)={
    db.run(tGameRecord.filter(_.recordId === id).map(_.filePath).result)
  }
  def getRoomIdByRecordId(rId: Long) = db.run(
    tGameRecord.filter(_.recordId === rId).map(_.roomId).result.head
  )

  def getInfoByUserId(id:String)={
    db.run(tUserInRecord.filter(_.userId === id).sortBy(_.recordId).result)
  }

}
