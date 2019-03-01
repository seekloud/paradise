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

package org.seekloud.utils

import java.io.File

import org.seekloud.paradise.common.AppSettings
import org.seekloud.paradise.ptcl.PDGameEvent
import org.seekloud.paradise.ptcl.PDGameEvent._
import org.seekloud.byteobject.{MiddleBuffer, MiddleBufferInJvm, decoder}
import org.seekloud.essf.io.{FrameData, FrameInputStream, FrameOutputStream}
import org.slf4j.LoggerFactory

/**
  * Created by Anjiansan on 2018/11/05.
  **/
object ESSFSupport {

  import org.seekloud.byteobject.ByteObject._
  private final val log = LoggerFactory.getLogger(this.getClass)

  def initRecorder(fileName:String, index:Int, gameInformation: GameInformation, initStateOpt:Option[Snapshot] = None): FrameOutputStream = {
    val middleBuffer = new MiddleBufferInJvm(100 * 4096)
    val name = "paradise"
    val version = "0.1"

    val dir = new File(AppSettings.gameDataDirectoryPath)
    if(!dir.exists()){
      dir.mkdir()
    }
    val file = AppSettings.gameDataDirectoryPath + fileName + s"_$index"
    val gameInformationBytes = gameInformation.fillMiddleBuffer(middleBuffer).result()
    val initStateBytes = initStateOpt.map{
      case t: Snapshot =>
        t.fillMiddleBuffer(middleBuffer).result()
      case x => Array[Byte]()
    }.getOrElse(Array[Byte]())
    val recorder = new FrameOutputStream(file)
    recorder.init(name,version,gameInformationBytes,initStateBytes)
    log.debug(s" init success")
    recorder
  }


  def initInput(targetFile: String): FrameInputStream = {
    new FrameInputStream(targetFile)
  }

  /**解码*/
  def metaDataDecode(a:Array[Byte])={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[GameInformation](buffer)
  }

  def initStateDecode(a:Array[Byte]) ={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[Snapshot](buffer)
  }

  def userMapDecode(a:Array[Byte])={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[EssfMapInfo](buffer)
  }

  /**用于后端先解码数据然后再进行编码传输*/
  def replayEventDecode(a:Array[Byte]): GameEvent={
    if (a.length > 0) {
      val buffer = new MiddleBufferInJvm(a)
      bytesDecode[List[GameEvent]](buffer) match {
        case Right(r) =>
          EventData(r)
        case Left(e) =>
          println(s"decode events error:")
          DecodeError()
      }
    }else{
//      println(s"events None")
      DecodeError()
    }
  }

  def replayStateDecode(a:Array[Byte]): GameEvent={
    val buffer = new MiddleBufferInJvm(a)
    bytesDecode[Snapshot](buffer) match {
      case Right(r) =>
//        println(s"states decode success")
        r
      case Left(e) =>
        println(s"decode states error:")
        DecodeError()
    }
  }

  def main(args: Array[String]): Unit = {
    val replay = initInput("/Users/anjiansan/Documents/Ebupt/paradise/backend/gameDataDirectoryPath/paradise_1_1542953191341_1")
    val info = replay.init()
    try{
      log.debug(s"userInfo:${userMapDecode(replay.getMutableInfo(AppSettings.essfMapKeyName).getOrElse(Array[Byte]())).right.get.m}")
      log.debug(s"get snapshot index::${replay.getSnapshotIndexes}")
      val nearSnapshotIndex = replay.gotoSnapshot(76)
      log.debug(s"nearSnapshotIndex: $nearSnapshotIndex")
      log.info(s"replay from frame=${replay.getFramePosition}")
      log.debug(s"start loading ======")
      var isCountinue = true
      while (isCountinue) {
        if (replay.hasMoreFrame) {
          replay.readFrame().foreach {
            f =>
              if(f.eventsData.length > 0) {
                val middleBuffer = new MiddleBufferInJvm(f.eventsData)
                val encodedData: Either[decoder.DecoderFailure, List[GameEvent]] =
                  bytesDecode[List[GameEvent]](middleBuffer) // get encoded data.
                encodedData match {
                  case Right(data) =>
                    if (data.nonEmpty)
                      println(f.frameIndex + ":" + data)
                }
              }
          }
        } else {
          isCountinue = false
        }
      }
    }catch {
      case e:Throwable=>
        log.error("error---"+e.getMessage)
    }
  }


}
