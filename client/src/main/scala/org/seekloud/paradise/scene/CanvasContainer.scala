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

import org.seekloud.paradise.common.StageContext
import javafx.scene.{Group, Scene}
import javafx.scene.canvas.Canvas
import javafx.scene.input.KeyCode
import org.seekloud.paradise.ptcl._
import org.seekloud.paradise.ptcl.paradise.Protocol.GridDataSync

/**
  * Created by zx0 on 2018/11/14.
  **/
object CanvasContainer{
  trait GameSceneListener {
    def onKeyPressed(e: KeyCode): Unit
  }
}

class CanvasContainer(roomId:Int,stage:StageContext) {

  import CanvasContainer._
  var gameSceneListener: GameSceneListener = _

  private val bounds = Point(Boundary.w, Boundary.h)
  var window = Point(1000,800)
  val group = new Group
  var canvas = new Canvas(window.x, window.y)
  canvas.setStyle("z-index: 2")
//  val backCanvas = new Canvas(viewWidth, viewHeight)
//  backCanvas.setStyle("z-index: 1")

  val scene = new Scene(group)

//  if((window.x != stage.getWindowSize.windowWidth) || (window.y != stage.getWindowSize.windowHeight)) {
//    drawGame.reSet(stage.getWindowSize.windowWidth, stage.getWindowSize.windowHeight)
//    window = Point(stage.getWindowSize.windowWidth, stage.getWindowSize.windowHeight)
//  }

//  group.getChildren.add(backCanvas)
  group.getChildren.add(canvas)
  var drawGame = new GameCanvas(canvas)
//  drawGame.setDefault(roomId)

  canvas.requestFocus()
  canvas.setOnKeyPressed(event => gameSceneListener.onKeyPressed(event.getCode))

  //try
  def setRoomId(roomId: Int): Unit = {
    drawGame.setBk(roomId)
  }

  def draw(myId:String, data: GridDataSync,grid:Grid,vicSnakes:List[(String,String)],barrierFlag:Boolean,offsetTime:Long): Unit ={
    if((window.x != stage.getWindowSize.windowWidth) || (window.y != stage.getWindowSize.windowHeight)) {
      drawGame.reSet(stage.getWindowSize.windowWidth, stage.getWindowSize.windowHeight)
      window = Point(stage.getWindowSize.windowWidth, stage.getWindowSize.windowHeight)
      canvas.setWidth(window.x)
      canvas.setHeight(window.y)
    }
    //绘制游戏
    drawGame.drawGrid(myId, data , grid,offsetTime)

    //绘制障碍
    if(barrierFlag){
      val barriers = grid.grid.filter {
        case (_, Barrier(_,_)) => true
        case _ => false
      }.flatMap{
        case (p, Barrier(num, center)) => List(Br(num, p.x, p.y, center))
        case _ => Nil
      }.toList
//      drawGame.drawOffBarrier(barriers)
    }

  }

  //绘制排行榜
  def drawRank(myId:String,historyRank:List[Score], currentRank:List[Score]):Int ={
    //绘制排行榜
    drawGame.drawRank(historyRank,false,myId)
    drawGame.drawRank(currentRank,true,myId)
  }

  //绘制初始化页面
  def drawStart(myId:String,championId:Int,firstCome:Boolean,vicSnakes:List[(String,String)],killInfo:(String,Int,Int)) = {
    if(championId == 1){
      drawGame.drawWin()
    }else{
      if(vicSnakes.nonEmpty){
        drawGame.drawVic(vicSnakes.head._2)
      }else{
        drawGame.drawWait(firstCome,killInfo._1,killInfo._2,killInfo._3)
      }
    }
  }

  //绘制结束页面
  def drawGameOff(firstCome:Boolean) = {
    drawGame.drawGameOff(firstCome)
  }

  //绘制弹幕
  def drawBarrage(info:String) ={
    drawGame.drawBarrage(info,window.x*0.5 - 100,window.y*0.17)
  }

  //绘制性能数据
  def drawPerform(drawTime:Long,rankHeight:Int,pingPongResult:Long): Unit ={
    Performance.render(drawTime,canvas,rankHeight,pingPongResult)
  }

  def setGameSceneListener(listener: GameSceneListener) {
    gameSceneListener = listener
  }

}
