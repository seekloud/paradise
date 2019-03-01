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

package org.seekloud.paradise.controller

import javafx.animation.{Animation, AnimationTimer, KeyFrame, Timeline}
import javafx.util.Duration
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.ActorRef
import org.seekloud.paradise.ClientBoot
import org.seekloud.paradise.common.StageContext
import org.seekloud.paradise.model.{GridOnClient, Protocol4Client}
import org.seekloud.paradise.scene.CanvasContainer
import org.seekloud.paradise.ptcl.paradise.Protocol._
import org.seekloud.paradise.ptcl._
import javafx.scene.input.KeyCode
import java.awt.event.KeyEvent
import org.seekloud.paradise.actor.PlayGameWebSocket
import org.slf4j.LoggerFactory
/**
	* Created by wangxicheng on 2018/10/25.
	*/
object GameController {
	val bounds = Point(Boundary.w, Boundary.h)
	val grid = new GridOnClient(bounds)
	var basicTime = 0l
	var firstCome = false


	val watchKeys = Set(
		KeyCode.SPACE,
		KeyCode.LEFT,
		KeyCode.UP,
		KeyCode.RIGHT,
		KeyCode.DOWN,
		KeyCode.F2
	)

	def keyCode2Int(c: KeyCode) = {
		c match {
			case KeyCode.SPACE => KeyEvent.VK_SPACE
			case KeyCode.LEFT => KeyEvent.VK_LEFT
			case KeyCode.UP => KeyEvent.VK_UP
			case KeyCode.RIGHT => KeyEvent.VK_RIGHT
			case KeyCode.DOWN => KeyEvent.VK_DOWN
			case KeyCode.F2 => KeyEvent.VK_F2
			case _ => KeyEvent.VK_F2
		}
	}
}

class GameController(id: String,
										 name: String,
                     roomId: String,
										 accessCode: String,
										 stageCtx: StageContext,
										 gameScene: CanvasContainer) {

	import GameController._
  private[this] val log = LoggerFactory.getLogger(this.getClass)

  private val playActor = ClientBoot.system.spawn(PlayGameWebSocket.create(this), "playActor")

	var firstCome = true
  var wsSetup = false
	var justSynced = false
  var vicSnakes = List.empty[(String,String)]
  var barrierFlag = true
  var SyncData: scala.Option[GridDataSync]  = None
  var currentRank = List.empty[Score]
  var historyRank = List.empty[Score]
  var barrage = ""
  var barrageTime = 0
  var drawTime = 0l
  var pingPongList = List.empty[Long]
  val pingPongTimes = 10
  var pingPongResult = 0l
  var isChampion = 0
  var eatenapples = Map[String,List[Ap]]()
  var myId = id
  var speedFlag = false
  var CheckFlag = true // 确保一帧内鼠标有动时候只发一次校验请求
  private var logicFrameTime = System.currentTimeMillis()

  def start(domain: String): Unit = {
    playActor ! PlayGameWebSocket.ConnectGame(id,name,roomId,accessCode, domain)
    addUserActionListen()
    startGameLoop()
  }


  def startGameLoop() = {
    logicFrameTime = System.currentTimeMillis()
		val animationTimer = new AnimationTimer() {
			override def handle(now: Long): Unit = {
        val time = System.currentTimeMillis()
				draw(System.currentTimeMillis() - logicFrameTime)
        drawTime = System.currentTimeMillis()-time
			}
		}
		val timeline = new Timeline()
		timeline.setCycleCount(Animation.INDEFINITE)
		val keyFrame = new KeyFrame(Duration.millis(Protocol.frameRate), { _ =>
			logicLoop()
		})

		timeline.getKeyFrames.add(keyFrame)
		animationTimer.start()
		timeline.play()
	}

	private def logicLoop() = {
		logicFrameTime = System.currentTimeMillis()
    playActor ! PlayGameWebSocket.MsgToService(Pi(id,logicFrameTime)) //Ping
    CheckFlag = true
    if (wsSetup) {
      if (!justSynced) {
        moveEatenApple()
        grid.update(false)
      } else {
        sync(SyncData)
        SyncData = None
        moveEatenApple()
        grid.update(true)
        justSynced = false
      }
    }
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
      gameScene.draw(id, grid.getGridData, grid, vicSnakes, barrierFlag,offsetTime)
      //绘制排行榜
      val rankHeight = gameScene.drawRank(id, historyRank, currentRank)
      //绘制弹幕
      if (barrageTime>0){
        gameScene.drawBarrage(barrage)
        //        println("111"+ (window.x*0.65 - w)
        barrageTime-=1
      }
      //绘制性能数据
      gameScene.drawPerform(drawTime, rankHeight, pingPongResult)
      //绘制初始化页面
      if (!data.snakes.exists(_.id == id) || isChampion != 0) {
        gameScene.drawStart(id, isChampion, firstCome, vicSnakes, grid.killInfo)
      } else {
        firstCome = false
      }
    }
    else{
      gameScene.drawGameOff(firstCome)
    }
  }

  def loseConnect(): Unit = {
    gameScene.drawGameOff(firstCome)
    barrage="ws 关闭"
    barrageTime=300
    wsSetup = false
    //    animationTimer.stop()
  }

  def openConnect() ={
    wsSetup = true
  }

  def gameMessageReceiver(msg: GameMessage): Unit = {
    msg match {
      case IdInfo(snakeId,roomIdT) =>
        myId = snakeId
        gameScene.setRoomId(roomIdT.toInt)

      case Po(createTime) => //PongData
        pingPongList = (System.currentTimeMillis() - createTime) :: pingPongList
        if(pingPongList.lengthCompare(pingPongTimes) >= 0) {
          pingPongResult = pingPongList.sum / pingPongList.length
          pingPongList = List.empty[Long]
        }

      case TextMsg(message) => println("ws send msg",message)

      case NewSnakeJoined(snakeId, user) =>
        barrage=s"$user 加入了游戏"
        barrageTime=300
      case NewWatcherJoined(count,player) =>
        barrage=s"$player 技术高超，正在被$count 人围观"
        barrageTime=300
      case SnakeLeft(snakeId, user) =>
        grid.snakes-=snakeId
        barrage=s"$user 离开了游戏"
        barrageTime=300
      case KillMessage(snakeId,skilled,skiller,kill,length)=>
        barrage=s"$skiller 杀死了 $skilled"
        barrageTime=300
        if (snakeId==id) {
          grid.killInfo =(skiller,kill,length)
        }
      case SA(snakeId, x, y, frame) => //SnakeAction
        //                  Performance.receiveWs(System.currentTimeMillis(),frame)
        if(snakeId!=id ){
          grid.addMouseWithFrame(snakeId, x, y, frame)
        }
      case EA(apples)=> //EatApples
        apples.foreach{ apple=>
          val lastEaten=eatenapples.getOrElse(apple.snakeId,List.empty)
          val curEaten=lastEaten:::apple.apples
          eatenapples += (apple.snakeId -> curEaten)
        }
      case SnakeSpeed(snakeId,isSpeedUp,frame) =>
        if(snakeId!=id){
          grid.addSpeedWithFrame(snakeId,isSpeedUp,frame)
        }

      case Ranks(current, history) =>
        currentRank = current
        historyRank = history

      case RC(current) => //RankCurrent
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
            isChampion = if(s == id) 1 else 2
            val name = grid.snakes.filter(_._1 == s).head._2.name
            (s,name)
          }
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
        SyncData = Some(data)
        justSynced = true
      case NetDelayTest(createTime) =>
        val receiveTime = System.currentTimeMillis()
        val m = s"Net Delay Test: createTime=$createTime, receiveTime=$receiveTime, twoWayDelay=${receiveTime - createTime}"

      case InitErrorMessage =>
        firstCome = false
        wsSetup = false
      case _ =>
        println(s"got nothing")
    }
  }


  def sync(dataOpt: scala.Option[GridDataSync])={

    if(dataOpt.isDefined){
      val data = dataOpt.get
      grid.actionMap = grid.actionMap.filterKeys(_ > data.frameCount)
      //                  grid.mouseMap = grid.mouseMap.filterKeys(_ > data.frameCount)
      grid.mouseMap = grid.mouseMap.filterKeys(_ >= data.frameCount - 1 - Protocol.advanceFrame)
      grid.speedMap = grid.speedMap.filterKeys(_ >= data.frameCount - 1 - Protocol.advanceFrame)
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

//	gameScene.setGameSceneListener(new CanvasContainer.GameSceneListener {
//		override def onKeyPressed(key: KeyCode): Unit = {
//      println("her pressed")
//			if (watchKeys.contains(key)) {
//				val msg: MsgFromFront = if (key == KeyCode.F2) {
//					NetTest(id, System.currentTimeMillis())
//				} else {
//					grid.addActionWithFrame(id, keyCode2Int(key), grid.frameCount )
//					Key(id, keyCode2Int(key), grid.frameCount + Protocol.advanceFrame )
//				}
//				playActor ! PlayGameWebSocket.MsgToService(msg)
//			}
//		}
//	})

	stageCtx.setStageListener(new StageContext.StageListener {
		override def onCloseRequest(): Unit = {
			playActor ! PlayGameWebSocket.MsgToService(WsSendComplete)
			stageCtx.closeStage()
		}
	})

  def addUserActionListen() = {
    gameScene.canvas.requestFocus()

    gameScene.canvas.setOnKeyPressed{ event =>
      val key = event.getCode
      if (watchKeys.contains(key)) {
        if(key == KeyCode.SPACE){
          //              这里对蛇是否存在进行判断
          isChampion = 0
          firstCome = true
          vicSnakes = List.empty[(String, String)]
        }
        val msg: MsgFromFront = if (key == KeyCode.F2) {
          NetTest(id, System.currentTimeMillis())
        } else {
          grid.addActionWithFrame(id, keyCode2Int(key), grid.frameCount )
          Key(id, keyCode2Int(key), grid.frameCount + Protocol.advanceFrame )
        }
        playActor ! PlayGameWebSocket.MsgToService(msg)
      }
    }

    gameScene.canvas.setOnMouseMoved{ e =>
      val x = e.getX.toFloat - gameScene.window.x/2
      val y = e.getY.toFloat - gameScene.window.y / 2
      val sendFrame = grid.frameCount+1
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
      grid.addMouseWithFrame(myId, x, y, sendFrame)
      //            Performance.sendWs(System.currentTimeMillis(),sendFrame)
      val msg: MsgFromFront = MM(myId,x,y,sendFrame)//MouseMove
      playActor ! PlayGameWebSocket.MsgToService(msg)
      if(CheckFlag) {
        CheckFlag = false
        //取上一帧的动作
        var SendFlag = true
        val action = try {
          grid.mouseMap(maxMouseFrame)(myId)
        } catch {
          case e: Exception =>
            SendFlag = false
            (0.0, 0.0)
        }

        val speed = try {
          grid.speedMap(maxSpeedFrame)(myId)
        } catch {
          case e: Exception =>
            //                SendFlag = false
            false
        }
        if (SendFlag) {
          val CheckMouseMsg: MsgFromFront = AC(myId, action._1, action._2, maxMouseFrame) //ActionCheck
          playActor ! PlayGameWebSocket.MsgToService(CheckMouseMsg)
          if (speedFlag) {
            val CheckSpeedMsg: MsgFromFront = SC(myId, speed, maxSpeedFrame) //SpeedCheck
            playActor ! PlayGameWebSocket.MsgToService(CheckSpeedMsg)
          }
        }
      }

    }

    gameScene.canvas.setOnMousePressed{e=>
      if(grid.snakes.contains(myId)) {
        if (grid.snakes(myId).length > 9) {
          speedFlag = true
          speedChange(true)
        }
      }
    }

    gameScene.canvas.setOnMouseReleased{e=>
      speedChange(false)
    }

    def speedChange(UpOrDown:Boolean)={
      if(grid.snakes.contains(myId)){
        val sendFrame = grid.frameCount+1
        val msg: MsgFromFront = MS(myId,sendFrame,UpOrDown) //MouseSpeed
        playActor ! PlayGameWebSocket.MsgToService(msg)
        //          println(s"speedup at${grid.frameCount}")
        grid.addSpeedWithFrame(myId,UpOrDown,sendFrame)
        //          grid.snakes += (myId ->grid.snakes(myId).copy(speed = 16))
      }
    }

  }
}
