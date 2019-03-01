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

package org.seekloud.paradise.scene

import javafx.scene.canvas.Canvas
import javafx.scene.paint.Color
import javafx.scene.text.Font

object Performance {
  //drawTime Fps
  private var lastUpdateTime = System.currentTimeMillis()
  private var currentDrawTime = 0l
  private var count = 0
  private var totalTime = 0l

  //data size
  private var dataSize = 0.0
  private var showDataSize = 0.0
  //wsTime
  private var sendTime = 0l
  private var sendFrame = 0l
  private var receiveTime = 0l

  private var fps = 0
  private var wsTime = 0l

  def getDrawTime(drawTime:Long): Long ={
    val time = System.currentTimeMillis()
    count += 1
    totalTime += drawTime
    if(time - lastUpdateTime > 1000){
      lastUpdateTime = time
      currentDrawTime = totalTime/count
      totalTime = 0
      fps = count
      count = 0
      showDataSize = dataSize
      dataSize = 0
      wsTime = receiveTime
    }
    currentDrawTime
  }

  def sendWs(time:Long,frame:Long) = {
    if(frame != sendFrame){
      sendTime = time
      sendFrame = frame
    }
  }

  def receiveWs(time:Long,frame:Long) = {
    if(frame == sendFrame){
      receiveTime = time - sendTime
    }
  }

  def setDataSize(size:Double) = {
    dataSize += size
  }

  def render(drawTime:Long,canvas:Canvas,rankHeight:Int,pingPongResult: Long) = {
    val ctx = canvas.getGraphicsContext2D
    val drawPer = getDrawTime(drawTime)
    val fpsValue = fps
    ctx.setFont(Font.font("黑体",14))
    ctx.setFill(if(drawPer < 10) Color.WHITE else if(drawPer < 17) Color.web("#FFFF00") else Color.RED)
    ctx.fillText(s"绘制时间: $drawPer ms ", 10, 2+rankHeight)
    ctx.setFill(if(fpsValue > 50) Color.WHITE  else if(fpsValue >30) Color.web("#FFFF00") else Color.RED)
    ctx.fillText(s"fps: $fpsValue ",10,20 + rankHeight)
    ctx.setFill(Color.WHITE)
    ctx.fillText(s"收发时间: $pingPongResult ms ",10,40+ rankHeight)
    ctx.fillText(s"数据包: $showDataSize byte ",10,60 + rankHeight)
  }

}
