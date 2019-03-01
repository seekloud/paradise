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

package org.seekloud.paradise.front

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import org.seekloud.paradise.ptcl.Protocol._
import org.seekloud.paradise.ptcl._
import org.seekloud.paradise.front.utils._
import org.seekloud.paradise.ptcl.PDGameEvent._
import org.scalajs.dom
import org.scalajs.dom.ext.{Color, KeyCode}
import org.scalajs.dom.html.{Document => _, _}
import org.scalajs.dom.raw._
import org.scalajs.dom.raw.MouseEvent

import scala.scalajs.js
import scalatags.JsDom.short._
import scala.scalajs.js.typedarray.ArrayBuffer
import org.seekloud.byteobject.ByteObject._
import org.seekloud.byteobject.{MiddleBufferInJs, decoder}
import org.seekloud.paradise.ptcl.paradise.Protocol._

import scala.collection.mutable
import org.scalajs.dom.html
import scala.util.Random

/**
  * User: Taoz
  * Date: 9/1/2016
  * Time: 12:45 PM
  */
object NetGameHolder extends js.JSApp {

  val  isPlay = dom.window.location.href.contains("playGame")

  val bounds = Point(Boundary.w, Boundary.h)
  var window = Point(dom.document.documentElement.clientWidth,dom.document.documentElement.clientHeight)

  var currentRank = List.empty[Score]
  var historyRank = List.empty[Score]

  var myId = "-1"
  var barragetime=0
  var killer= ""
  var finallength=0
  var finalkill=0

  var drawTime = 0l

  var barrierFlag = true

  var barrage=""

  var vicSnakes = List.empty[(String, String)]
  var eatenapples = Map[String,List[Ap]]()

  val grid = new GridOnClient(bounds)

  var firstCome = 0//0代表第一次进入，1代表玩家死亡，2代表被观战的玩家已死亡
  var wsSetup = false
  var justSynced = false

  var nextFrame = 0//requestAnimationFrame

  private var logicFrameTime = System.currentTimeMillis()

  var CheckFlag = true // 确保一帧内鼠标有动时候只发一次校验请求
  var speedFlag = false
  var FormerDegree = 0.0

  var NewData : GridDataSync = _
  var isChampion = 0

  var SynDate: scala.Option[GridDataSync]  = None

  var pingPongList = List.empty[Long]
  val pingPongTimes = 10
  var pingPongResult = 0l

  var loading = true
  var replayFinish = false
  var gameRecorder = scala.collection.mutable.Map.empty[Long, GameReply]
  //收集暂时没有同步的快照
  val snapShotMap = mutable.HashMap.empty[Int,Snapshot]
  //收集暂时没有执行的食物事件
  val feedAppleEventMap = mutable.HashMap.empty[Int,FeedAppleEvent]
  val eatenAppleEventMap = mutable.HashMap.empty[Int,EatenAppleEvent]
  val deadEventMap = mutable.HashMap.empty[Int,DeadEvent]
  val killEventMap = mutable.HashMap.empty[Int,KillEvent]

  val watchKeys = Set(
    KeyCode.Space,
    KeyCode.F2
  )

  private[this] val canvas = dom.document.getElementById("GameView").asInstanceOf[Canvas]
  private[this] val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private[this] val audio = dom.document.getElementById("music").asInstanceOf[HTMLAudioElement]
  private[this] val victory = dom.document.getElementById("victory").asInstanceOf[HTMLAudioElement]
  private[this] val end = dom.document.getElementById("end").asInstanceOf[HTMLAudioElement]
  private[this] val drawGame: Draw = new Draw(ctx,canvas)

  @scala.scalajs.js.annotation.JSExport
  override def main(): Unit = {
    drawGame.drawGameOff(firstCome,nextFrame)
    canvas.width = window.x.toInt
    canvas.height = window.y.toInt

    val random = new Random(System.nanoTime())
    val para = Func.getParameter()
    val roomId = para._2.getOrElse("roomId",(random.nextInt(3)+1).toString).toInt
    val search = if(para._2.get("roomId").isDefined)dom.window.location.search else dom.window.location.search + s"&roomId=$roomId"

    para._1 match {
      case "playGame" =>
        joinGame(search,"playGame")
      case "watchGame" =>
        joinGame(search,"watchGame")
        drawGame.setDefault(roomId)
        drawGame.drawBackground()
        drawGame.drawOffMap()
      case "watchRecord" =>
        replayGame(search,"watchRecord")
      case _ =>
        JsFunc.alert("url有误，请确认！")
    }

    if(para._1 != "watchRecord") {
      logicFrameTime = System.currentTimeMillis()
      dom.window.setInterval(() => gameLoop("notWatchRecord"), Protocol.frameRate)
      nextFrame = dom.window.requestAnimationFrame(gameRender())
    }

//    dom.window.setInterval(() => gameLoop(), Protocol.frameRate / subTotalFrame )
  }

  def gameRender():Double => Unit = {
    d =>
      val curTime = System.currentTimeMillis()
      val offsetTime = curTime - logicFrameTime
      val time = System.currentTimeMillis()
      if((window.x != dom.document.documentElement.clientWidth ) || (window.y != dom.document.documentElement.clientHeight)) {
        drawGame.reSetScreen()
        window = Point(dom.document.documentElement.clientWidth , dom.document.documentElement.clientHeight)
      }
      draw(offsetTime)
      drawTime = System.currentTimeMillis() - time
      nextFrame = dom.window.requestAnimationFrame(gameRender())
  }

  def gameLoop(types: String): Unit = {
    logicFrameTime = System.currentTimeMillis()
    CheckFlag = true
    if (wsSetup) {
      if (!justSynced) {
        update(false,types)
      } else {
        if(types == "watchRecord") {
          update(true,types)
          justSynced = false
        }else {
          sync(SynDate)
          SynDate = None
          update(true, types)
          justSynced = false
        }
      }

      if(types == "watchRecord") {
        if(snapShotMap.keys.toList.contains(grid.frameCount.toInt)) {
          replayMessageHandler(snapShotMap(grid.frameCount.toInt),grid.frameCount.toInt)
          snapShotMap.remove(grid.frameCount.toInt)
        }

        if(feedAppleEventMap.keys.toList.contains(grid.frameCount.toInt)) {
          replayMessageHandler(feedAppleEventMap(grid.frameCount.toInt),grid.frameCount.toInt)
          feedAppleEventMap.remove(grid.frameCount.toInt)
        }

        if(eatenAppleEventMap.keys.toList.contains(grid.frameCount.toInt)) {
          replayMessageHandler(eatenAppleEventMap(grid.frameCount.toInt),grid.frameCount.toInt)
          eatenAppleEventMap.remove(grid.frameCount.toInt)
        }

        if(deadEventMap.keys.toList.contains(grid.frameCount.toInt)) {
          replayMessageHandler(deadEventMap(grid.frameCount.toInt),grid.frameCount.toInt)
          deadEventMap.remove(grid.frameCount.toInt)
        }

        if(killEventMap.keys.toList.contains(grid.frameCount.toInt)) {
          replayMessageHandler(killEventMap(grid.frameCount.toInt),grid.frameCount.toInt)
          killEventMap.remove(grid.frameCount.toInt)
        }
      }

    }


  }

  def update(justSynced:Boolean,types: String): Unit = {
    moveEatenApple()
    grid.updateInClient(justSynced,types)
  }

  def moveEatenApple()={
    val invalidApple=Ap(0,0,0,0)
    eatenapples=eatenapples.filterNot(apple=> !grid.snakes.exists(_._2.id==apple._1))
    eatenapples.foreach{ info=>
      val snakeOpt=grid.snakes.get(info._1)
      if (snakeOpt.isDefined){
        val snake=snakeOpt.get
        val applesOpt=eatenapples.get(info._1)
        var apples=List.empty[Ap]
        if (applesOpt.isDefined){
          apples=applesOpt.get
          if (apples.nonEmpty){
            val newheader=snake.header+(snake.direction*snake.speed)
            apples=apples.map{apple=>
              grid.grid-=Point(apple.x,apple.y)
              val nextLocOpt=Point(apple.x,apple.y).pathTo(newheader)
              if (nextLocOpt.nonEmpty){
                val nextLoc=nextLocOpt.get
                grid.grid.get(nextLoc) match{
                  case Some(Body(_,_,_,_)) => invalidApple
                  case _ =>
                    val nextapple=Apple(apple.score,apple.life)
                    grid.grid+=(nextLoc->nextapple)
                    Ap(apple.score,apple.life,nextLoc.x,nextLoc.y)
                }
              }else
                invalidApple
            }.filterNot(a => a==invalidApple)
            eatenapples += (snake.id -> apples)
          }
        }
      }
    }
  }

  def draw(offsetTime:Long): Unit = {
    if (wsSetup) {
      val data = grid.getGridData

      //绘制游戏
      drawGame.drawGrid(myId, data , grid,offsetTime)
      //绘制排行榜
      drawGame.drawRank(currentRank,true,myId)
      val historyHeight = drawGame.drawRank(historyRank,false,myId)
      //绘制障碍
      if(barrierFlag){
        val barriers = grid.grid.filter {
          case (_, Barrier(_,_)) => true
          case _ => false
        }.flatMap{
          case (p, Barrier(num, center)) => List(Br(num, p.x, p.y, center))
          case _ => Nil
        }.toList
        drawGame.drawOffBarrier(barriers)
        barrierFlag = false
      }
      //绘制弹幕
      if (barragetime>0){
        val w = ctx.measureText(barrage).width.toInt
        drawGame.drawBarrage(barrage,window.x*0.6- w,window.y*0.17)

        barragetime-=1
      }
      //绘制性能数据
      Performance.render(drawTime,ctx,historyHeight,pingPongResult,isPlay)
      //绘制初始化页面
      if(!data.snakes.exists(_.id == myId) || isChampion !=0){
        if(isChampion == 1){
          println("===============drawWin")
          drawGame.drawWin()
        }else{

          if(vicSnakes.nonEmpty){
            drawGame.drawVic(vicSnakes.head._2)
          }else{
            drawGame.drawWait(firstCome,killer,finallength,finalkill)
          }
        }
      }else{
        firstCome = 1
      }
      if(firstCome == 8)
        drawGame.drawGameOff(firstCome, nextFrame)
    } else {
      drawGame.drawGameOff(firstCome,nextFrame)
    }
  }

  val sendBuffer = new MiddleBufferInJs(4096) //sender buffer

  def joinGame(search:String,path:String): Unit = {

    println("joinGame")

    val gameStream = new WebSocket(getWebSocketUri(dom.document, path+search))
    gameStream.onopen = { (event0: Event) =>
      wsSetup = true
      canvas.focus()
      dom.window.onfocus = {(e:Event) =>
        audio.play()
        if(myId != "-1"){
          val msg: MsgFromFront = ApplesReq(myId)
          msg.fillMiddleBuffer(sendBuffer) //encode msg
          val ab: ArrayBuffer = sendBuffer.result() //get encoded data.
          gameStream.send(ab) // send data.
        }
        e.preventDefault()
      }
      dom.window.onblur = {(e:Event) =>
        e.preventDefault()
        audio.pause()
      }
      if(path != "watchGame"){//观战时不发动作数据
        canvas.onkeydown = {
          (e: dom.KeyboardEvent) => {
            //          println(s"keydown: ${e.keyCode}")
            if (watchKeys.contains(e.keyCode)) {
              if(e.keyCode == KeyCode.Space){
                //              这里对蛇是否存在进行判断
                end.pause()
                end.currentTime = 0.0
                victory.pause()
                victory.currentTime = 0.0
                audio.play()
                isChampion = 0
                firstCome = 0
                vicSnakes = List.empty[(String, String)]
              }
              val msg: MsgFromFront = if (e.keyCode == KeyCode.F2) {
                NetTest(myId, System.currentTimeMillis())
              } else {
                Key(myId, e.keyCode, grid.frameCount)
              }
              msg.fillMiddleBuffer(sendBuffer) //encode msg
              val ab: ArrayBuffer = sendBuffer.result() //get encoded data.
              gameStream.send(ab) // send data.
              e.preventDefault()
            }
          }
        }
        canvas.onmousemove = {(e:MouseEvent) =>
          val x = e.pageX - window.x / 2
          val y = e.pageY - window.y / 2
          val sendFrame = grid.frameCount+advanceFrame
          if(math.abs(grid.getDegree(x,y)-FormerDegree)*180/math.Pi>5){
            FormerDegree = grid.getDegree(x, y)

            //取mouseMap中的最大帧号
            val maxMouseFrame = if(grid.mouseMap.keySet.isEmpty){
              0
            }else{
              val temp = grid.mouseMap.keySet.toList.max
              //当前的5帧内并且不等于发送帧号
              if(temp != sendFrame && temp>=grid.frameCount-5){
                temp
              }else{
                0
              }
            }

            //取speedMap中的最大帧号
            val maxSpeedFrame = if(grid.speedMap.keySet.isEmpty){
              0
            }else{
              val temp = grid.speedMap.keySet.toList.max
              //当前的5帧内并且不等于发送帧号
              if(temp != sendFrame && temp>=grid.frameCount-5){
                temp
              }else{
                0
              }
            }

            if(CheckFlag){
              grid.addMouseWithFrame(myId, x, y, sendFrame)//一帧内只发一次鼠标移动请求
              //            Performance.sendWs(System.currentTimeMillis(),sendFrame)
              val msg: MsgFromFront = MM(myId,x,y,sendFrame) //MouseMove
              msg.fillMiddleBuffer(sendBuffer) //encode msg
              val ab: ArrayBuffer = sendBuffer.result() //get encoded data.
              gameStream.send(ab) // send data.

              CheckFlag = false

              //取上一帧的动作
              var SendFlag = true
              val action = try{
                grid.mouseMap(maxMouseFrame)(myId)
              } catch {
                case e: Exception =>
                  SendFlag = false
                  (0.0, 0.0)
              }

              val speed = try{
                grid.speedMap(maxSpeedFrame)(myId)
              } catch {
                case e: Exception =>
                  //                SendFlag = false
                  false
              }
              if(SendFlag){
                val CheckMouseMsg: MsgFromFront = AC(myId, action._1, action._2, maxMouseFrame) //ActionCheck
                CheckMouseMsg.fillMiddleBuffer(sendBuffer) //encode msg
                val Checkab: ArrayBuffer = sendBuffer.result() //get encoded data.
                gameStream.send(Checkab) // send data.
                if(speedFlag){
                  val CheckSpeedMsg: MsgFromFront = SC(myId, speed,maxSpeedFrame) //SpeedCheck
                  CheckSpeedMsg.fillMiddleBuffer(sendBuffer) //encode msg
                  val Speedab: ArrayBuffer = sendBuffer.result() //get encoded data.
                  gameStream.send(Speedab) // send data.
                }

              }

            }

            e.preventDefault()
          }
        }
        canvas.onmousedown = { (e: MouseEvent) =>
          if(grid.snakes.contains(myId)) {
            if (grid.snakes(myId).length > 9) {
              speedFlag = true
              speedChange(true)
            }
          }
        }
        canvas.onmouseup = {(e:MouseEvent) =>
          speedChange(false)
//          speedFlag = false
        }

        def speedChange(UpOrDown:Boolean)={
          if(grid.snakes.contains(myId)){
            val sendFrame = grid.frameCount+advanceFrame
            val msg: MsgFromFront = MS(myId,sendFrame,UpOrDown)//MouseSpeed
            msg.fillMiddleBuffer(sendBuffer) //encode msg
            val ab: ArrayBuffer = sendBuffer.result() //get encoded data.
            gameStream.send(ab) // send data.
            //          println(s"speedup at${grid.frameCount}")
            grid.addSpeedWithFrame(myId,UpOrDown,sendFrame)
            //          grid.snakes += (myId ->grid.snakes(myId).copy(speed = 20))
          }
        }
      }

      event0
    }

    gameStream.onerror = { (event: ErrorEvent) =>
      drawGame.drawGameOff(firstCome,nextFrame)
//      dom.window.cancelAnimationFrame(nextFrame)
      wsSetup = false
    }

    gameStream.onmessage = { (event: MessageEvent) =>
      event.data match {
        case blobMsg: Blob =>
          Performance.setDataSize(blobMsg.size)
          val fr = new FileReader()
          fr.readAsArrayBuffer(blobMsg)
          fr.onloadend = { _: Event =>
            val buf = fr.result.asInstanceOf[ArrayBuffer] // read data from ws.
            val middleDataInJs = new MiddleBufferInJs(buf) //put data into MiddleBuffer
          val encodedData: Either[decoder.DecoderFailure, GameMessage] =
            bytesDecode[GameMessage](middleDataInJs) // get encoded data.
            encodedData match {
              case Right(data) =>

                data match {

                case IdInfo(id,roomId) =>
                  myId = id
                  println(s"房间号: $roomId-----------------------------------")
                  if(path == "playGame"){
                    drawGame.setDefault(roomId)
                    drawGame.drawBackground()
                    drawGame.drawOffMap()
                    dom.window.setInterval(() => gameStream.send(Pi(myId,System.currentTimeMillis()).asInstanceOf[MsgFromFront].fillMiddleBuffer(sendBuffer).result()),100) //Ping
                  }

                case Po(createTime) => //PongData
                  pingPongList = (System.currentTimeMillis() - createTime) :: pingPongList
                  if(pingPongList.lengthCompare(pingPongTimes) >= 0) {
                    pingPongResult = pingPongList.sum / pingPongList.length
                    pingPongList = List.empty[Long]
                  }

                case InitErrorMessage =>
                  audio.pause()
                  firstCome = 100
                  wsSetup = false

                case TextMsg(message) => println("ws send msg",message)
                case NewSnakeJoined(id, user) =>
                  barrage=s"$user 加入了游戏"
                  barragetime=300
                case NewWatcherJoined(count,player) =>
                  barrage=s"$player 技术高超，正在被$count 人围观"
                  barragetime=300
                case SnakeLeft(id, user) =>
                  grid.snakes-=id
                  barrage=s"$user 离开了游戏"
                  barragetime=300
                case KillMessage(id,skilled,skiller,kill,length)=>
                  barrage=s"$skiller 杀死了 $skilled"
                  barragetime=300
                  if (id==myId) {
                    audio.pause()
                    end.play()
                    killer = skiller
                    finalkill = kill
                    finallength = length
                  }
                case SA(id, x, y, frame) => //SnakeAction
//                  Performance.receiveWs(System.currentTimeMillis(),frame)
                  if(path == "watchGame")
                    grid.addMouseWithFrame(id, x, y, frame)
                  else if(id!=myId ){
                    grid.addMouseWithFrame(id, x, y, frame)
                  }
                case EA(apples)=> //EatApples
                  apples.foreach{ apple=>
                    val lastEaten=eatenapples.getOrElse(apple.snakeId,List.empty)
                    val curEaten=lastEaten:::apple.apples
                    eatenapples += (apple.snakeId -> curEaten)
                  }
                case SnakeSpeed(id,isSpeedUp,frame) =>
                  if(path == "watchGame")
                    grid.addSpeedWithFrame(id,isSpeedUp,frame)
                  else if(id!=myId){
                    grid.addSpeedWithFrame(id,isSpeedUp,frame)
                  }

                case Ranks(current, history) =>
                  currentRank = current
                  historyRank = history

                case RC(current) => //RanksCurrent
                  currentRank = current

                case RH(history) => //RanksHistory
                  historyRank = history

                case Barriers(brs) =>
                  barrierFlag = true //有新的障碍产生
                  grid.grid ++= brs.map(b => Point(b.x, b.y) -> Barrier(b.num, b.center))

                case Victory(snk) =>
                  if(snk.nonEmpty){
                    grid.grid = grid.grid.filter{
                      case (_, Apple(_,_)) => false
                      case _ => true
                    }
                    vicSnakes = snk.map {s =>
                      isChampion = if(s == myId) 1 else 2
                      val name = grid.snakes.filter(_._1 == s).head._2.name
                      (s,name)
                    }
                    victory.play()
                    audio.pause()
                    grid.snakes = Map.empty[String, SkDt]
                  }

                case FA(apples) => //FeedApples
                  grid.grid ++=apples.map(a => Point(a.x, a.y) -> Apple(a.score, a.life))

                case RD(pointsList) => //RemoveData
                  val points = pointsList.map(p => p._1)
                  grid.grid --= points
                  pointsList.groupBy(_._3).foreach {
                    case (id, list) =>
                    val score = list.map(_._2).sum
//                      grid.lengthIncreaseMap += (id -> score)
                      if(grid.snakes.exists(_._1 == id)) grid.snakes += id ->grid.snakes(id).copy(length = grid.snakes(id).length + score)
                  }

                case NewSnakes(newSnakes,deads,frameCount) =>
                  //击杀数取发过来的，但是蛇的身体位置以客户端为主
                  val killMap = newSnakes.map(ns=>(ns.id,ns.kill)).toMap
                  val killerSnake = grid.snakes.filter(s=>killMap.get(s._1).isDefined).map(s => (s._1,s._2.copy(kill = killMap(s._1))))
                  grid.snakes = killerSnake

                  deads.foreach(d=>{
                    grid.grid += d._2 -> Apple(1,1000)
                    grid.pointMap += d._1 ->List.empty[Point]
                  })

                case ApplesRsp(apples) =>
                  val result = grid.grid.filter{
                    case (_,Apple(_,_)) => false
                    case _ => true
                  } ++ apples
                  grid.grid = result

                case data: SyAD => //GridDataSyncAfterDead
                  val appleMap = data.appleDetails.map(b => Point(b.x, b.y) -> Apple(b.score, b.life)).toMap
                  grid.grid = grid.grid.filter{
                    case (_, Barrier(_,_)) => true
                    case (_, Body(_,_,_,_)) => true
                    case _ => false} ++ appleMap

                case data: GridDataSync =>
                  SynDate = Some(data)
                  justSynced = true

                case NetDelayTest(createTime) =>
                  val receiveTime = System.currentTimeMillis()
                  val m = s"Net Delay Test: createTime=$createTime, receiveTime=$receiveTime, twoWayDelay=${receiveTime - createTime}"

                case PlayerIsDead() =>
                  firstCome = 2//观战者进入时，玩家已经死亡

                case PlayerIsNone() =>
                  firstCome = 3//该房间没有玩家

                case PlayerLeft() =>
                  firstCome = 4//该房间被观战的玩家已离开

                case _ =>
                  println(s"got nothing-data:$data")
              }
              case Left(e) =>
                println(s"got error: ${e.message}")
            }
          }
      }
    }

    gameStream.onclose = { (event: Event) =>
      barrage="ws 关闭"
      barragetime=300
      audio.pause()
      drawGame.drawGameOff(firstCome,nextFrame)
      wsSetup = false
    }

  }

  def replayGame(search:String,path:String): Unit = {

    println("replayGame")

    val gameStream = new WebSocket(getWebSocketUri(dom.document, path+search))
    gameStream.onopen = { (event0: Event) =>
      wsSetup = true
      canvas.focus()

      val tickTock = new Function0[Unit] {
        val msg: MsgFromFront = TickTock
        msg.fillMiddleBuffer(sendBuffer)
        val ab: ArrayBuffer = sendBuffer.result()
        def apply(): Unit = gameStream.send(ab)
      }

      dom.window.setInterval(tickTock, 20000)

      event0
    }

    gameStream.onerror = { (event: ErrorEvent) =>
      drawGame.drawGameOff(firstCome,nextFrame)
      wsSetup = false
    }

    gameStream.onmessage = { (event: MessageEvent) =>
      event.data match {
        case blobMsg: Blob =>
          Performance.setDataSize(blobMsg.size)
          val fr = new FileReader()
          fr.readAsArrayBuffer(blobMsg)
          fr.onloadend = { _: Event =>
            val buf = fr.result.asInstanceOf[ArrayBuffer] // read data from ws.
          val middleDataInJs = new MiddleBufferInJs(buf) //put data into MiddleBuffer
          val encodedData: Either[decoder.DecoderFailure, GameReply] =
            bytesDecode[GameReply](middleDataInJs) // get encoded data.
              encodedData match {
                case Right(data) =>
                  data match {
                    case UserInfo(userId) =>
                      myId = userId

                    case StartLoading(frame, roomId) =>
                      println(s"start loading  =========")
                      loading = true
                      grid.frameCount = frame.toLong
                      gameRecorder = scala.collection.mutable.Map.empty[Long, GameReply]
                      drawGame.setDefault(roomId.toInt)
                      drawGame.drawBackground()
                      drawGame.drawOffMap()
                      firstCome = 5 //loading录像
                      drawGame.drawGameOff(firstCome,nextFrame)

                    case StartReplay(firstSnapshotFrame,firstReplayFrame) =>
                      println(s"firstSnapshotFrame::$firstSnapshotFrame")
                      println(s"firstReplayFrame::$firstReplayFrame")
                      firstCome = 6 //观看录像
                      drawGame.drawGameOff(firstCome,nextFrame)
                      eatenapples = Map.empty[String, List[Ap]]
                      for(i <- firstSnapshotFrame until firstReplayFrame if gameRecorder.exists(r => r._1 == i))  {
                        if (wsSetup) {
                          if(gameRecorder.contains(grid.frameCount)) {
                            val data = gameRecorder(grid.frameCount).asInstanceOf[ReplayFrameData]

                            data.eventsData match {
                              case EventData(events) =>
                                events.foreach { event =>
                                  (event, loading) match {
                                    case (_, true) => replayMessageHandler(event, data.frameIndex)
                                    case _ =>
                                  }
                                }

                              case DecodeError() =>
//                                println("events decode error")

                              case _ =>
                            }

                            if(data.stateData.nonEmpty) {
                              data.stateData.get match {
                                case msg: Snapshot =>
                                  replayMessageHandler(msg, data.frameIndex)
                                case DecodeError() =>
                                  println("state decode error")
                                case _ =>
                              }
                            }

                            println(s"==================${grid.frameCount}")
                            update(false,"watchRecord")
                            gameRecorder = gameRecorder.filter(_._1 >= grid.frameCount)
                          }
                        }

                      }
                      loading = false
                      audio.play()
                      logicFrameTime = System.currentTimeMillis()
                      dom.window.setInterval(() => gameLoop("watchRecord"), Protocol.frameRate)
                      nextFrame = dom.window.requestAnimationFrame(gameRender())

                    case InitReplayError(info) =>
                    //drawGame.drawGameOff(firstCome, Some(false), loading, true)

                    case x@ReplayFinish(_) =>
                      println("get message replay finish")
                      replayFinish = true
                      audio.pause()
                      firstCome = 7
                      wsSetup = false

                    case InitErrorReply =>
                      audio.pause()
                      firstCome = 100
                      wsSetup = false


                    case data@ReplayFrameData(frameIndex, eventsData, stateData) =>
//                      println(s"receive replayFrameData,grid.frameCount:${grid.frameCount},frameIndex:$frameIndex")
                      if(frameIndex == 0) grid.frameCount = 0

                      eventsData match {
                        case EventData(events) =>
                          events.foreach { event =>
                            (event, loading) match {
                              case (_, false) => replayMessageHandler(event, frameIndex)
                              case _ =>
                            }
                          }

                        case DecodeError() =>
                        //                          println("events decode error")

                        case _ =>
                      }

                      if(stateData.nonEmpty) {
                        stateData.get match {
                          case msg: Snapshot =>
                            replayMessageHandler(msg, frameIndex)
                          case DecodeError() =>
                            println("state decode error")
                          case _ =>
                        }
                      }

                      gameRecorder += (frameIndex.toLong -> data)

                    case _ =>
                      println(s"got nothing-data:$data")
                  }

                case Left(e) =>
                  println(s"got error: ${e.message}")
              }
            }
      }
    }

    gameStream.onclose = { (event: Event) =>
      barrage="ws 关闭"
      barragetime=300
      audio.pause()
      drawGame.drawGameOff(firstCome,nextFrame)
      wsSetup = false
    }

  }

  private def replayMessageHandler(data: GameEvent, frameIndex: Int): Unit = {
    data match {
      case JoinEvent(id, snakeInfo) =>
        if(snakeInfo.isDefined) {
          grid.addSnake(id, snakeInfo.get.name, snakeInfo.get.header, snakeInfo.get.color.toString)
        }

        if(id != myId) {
          val name = if(snakeInfo.isDefined){
            snakeInfo.get.name
          }else{
            "somebody"
          }
          barrage = s"$name 加入了游戏"
          barragetime = 300
        }

      case LeftEvent(id, name) =>
        grid.snakes-=id
        barrage=s"$name 离开了游戏"
        barragetime=300

      case SpaceEvent(id,name,headerT,color) =>
        println("get spaceEvent---------------------------------")
        grid.addSnake(id,name,headerT,color)
        if(id == myId) {
          end.pause()
          end.currentTime = 0.0
          victory.pause()
          victory.currentTime = 0.0
          audio.play()
          isChampion = 0
//          firstCome = 0
          vicSnakes = List.empty[(String, String)]
//          nextFrame = dom.window.requestAnimationFrame(gameRender())
        }

      case PDGameEvent.MouseEvent(id, x, y) =>
        grid.addMouseWithFrame(id, x, y, frameIndex)

      case SpeedEvent(id,isSpeedUp) =>
        grid.addSpeedWithFrame(id,isSpeedUp,frameIndex)

      case msg@FeedAppleEvent(apples) =>
        if(grid.frameCount >= frameIndex) {
          grid.grid ++= apples.map(a => Point(a.x, a.y) -> Apple(a.score, a.life))
        }else{
          feedAppleEventMap.put(frameIndex,msg)
        }

      case msg@EatenAppleEvent(apples) =>
        if(grid.frameCount >= frameIndex) {
          apples.foreach { apple =>
            val lastEaten = eatenapples.getOrElse(apple.snakeId, List.empty)
            val curEaten = lastEaten ::: apple.apples
            eatenapples += (apple.snakeId -> curEaten)
          }
        }else{
          eatenAppleEventMap.put(frameIndex,msg)
        }

      case RemoveEvent(pointsList) =>
        val points = pointsList.map(p => p._1)
        grid.grid --= points
        pointsList.groupBy(_._3).foreach {
          case (id, list) =>
            val score = list.map(_._2).sum
            if(grid.snakes.exists(_._1 == id)) grid.snakes += id ->grid.snakes(id).copy(length = grid.snakes(id).length + score)
        }

      case msg@KillEvent(id,skilled,skiller,kill,length)=>
        if(grid.frameCount >= frameIndex) {
          barrage = s"$skiller 杀死了 $skilled"
          barragetime = 300
          if (id == myId) {
            audio.pause()
            end.play()
            killer = skiller
            finalkill = kill
            finallength = length
          }
        }else{
          killEventMap.put(frameIndex,msg)
        }

      case msg@DeadEvent(newSnakes,deads,frameCount) =>
        //击杀数取发过来的，但是蛇的身体位置以客户端为主
        if(grid.frameCount >= frameIndex) {
          val killMap = newSnakes.map(ns => (ns.id, ns.kill)).toMap
          val killerSnake = grid.snakes.filter(s => killMap.get(s._1).isDefined).map(s => (s._1, s._2.copy(kill = killMap(s._1))))
          grid.snakes = killerSnake

          deads.foreach(d => {
            grid.grid += d._2 -> Apple(1, 1000)
            grid.pointMap += d._1 -> List.empty[Point]
          })
        }else{
          deadEventMap.put(frameIndex,msg)
        }

      case VictoryEvent(snk) =>
        if(snk.nonEmpty){
          grid.grid = grid.grid.filter{
            case (_, Apple(_,_)) => false
            case _ => true
          }
          vicSnakes = snk.map {s =>
            isChampion = if(s == myId) 1 else 2
            val name = grid.snakes.filter(_._1 == s).head._2.name
            (s,name)
          }
          victory.play()
          audio.pause()
          grid.snakes = Map.empty[String, SkDt]
          println(s"-----------------$snk")
        }

      case RankEvent(current, history) =>
        currentRank = current
        historyRank = history

      case msg@Snapshot(snakes, pointDetails, apples, barriers, history) =>

        println(s"get snapshot:$msg")

        if(grid.frameCount >= frameIndex.toLong) { //重置

          println("yes-----------------------------------")

          grid.frameCount = frameIndex
          grid.snakes = snakes.map(s => s.id -> s).toMap
          grid.pointMap = pointDetails.groupBy(_.id).map{
            case(id,list) =>
              id ->list.map{l => Point(l.x,l.y)}.reverse
          }

          grid.grid = grid.grid.filter{
            case (_, Barrier(_,_)) => true
            case _ => false
          } ++ apples.map(a => (Point(a.x, a.y), Apple(a.score, a.life)))

          barrierFlag = true //有新的障碍产生
          grid.grid ++= barriers.map(b => Point(b.x, b.y) -> Barrier(b.num, b.center))

          historyRank = history

          justSynced = true

          if(pointDetails.isEmpty && isChampion == 0) {
            firstCome = 8
            drawGame.drawWait(firstCome, "", 0, 0)
            println(s"-----------------$firstCome")
          }

        }else{
          snapShotMap.put(frameIndex,msg)
        }

      case _ =>
    }
  }


  def getWebSocketUri(document: Document,path:String): String = {
    println("her get ws url")
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}/paradise/game/$path"
  }

  def sync(dataOpt: scala.Option[GridDataSync])={


    if(dataOpt.isDefined){
      val data = dataOpt.get
      grid.actionMap = grid.actionMap.filterKeys(_ > data.frameCount)
      //                  grid.mouseMap = grid.mouseMap.filterKeys(_ > data.frameCount)
      grid.mouseMap = grid.mouseMap.filterKeys(_ >= data.frameCount - 1 - advanceFrame)
      grid.speedMap = grid.speedMap.filterKeys(_ >= data.frameCount - 1 - advanceFrame)
      if(data.frameCount > grid.frameCount){
        val count = data.frameCount - grid.frameCount
        for( i <- 1 to count.toInt ){
          grid.speedMap += (grid.frameCount+i -> grid.speedMap.getOrElse(grid.frameCount,Map.empty))
        }
      }

      grid.frameCount = data.frameCount
      grid.snakes = data.snakes.map(s => s.id -> s).toMap
      grid.pointMap = data.pointDetails.groupBy(_.id).map{
        case(id,list) =>
          id ->list.map{l => Point(l.x,l.y)}.reverse
      }


      grid.grid = grid.grid.filter{
        case (_, Barrier(_,_)) => true
        case (_, Apple(_, _)) => true
        case _ => false
      } //++ gridMap

   }

  }

}
