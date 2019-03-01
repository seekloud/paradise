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

import java.io.ByteArrayInputStream

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.adapter._
import org.seekloud.paradise.ClientBoot
import org.seekloud.paradise.actor.LoginActor
import org.seekloud.paradise.actor.LoginActor.EstablishConnection2Es
import org.seekloud.paradise.scene.{CanvasContainer, LoginScene}
import org.seekloud.paradise.actor.PlayGameWebSocket
import org.seekloud.paradise.common.StageContext
import org.seekloud.paradise.ClientBoot.{executor, materializer, system}
import org.seekloud.paradise.scene.LoginScene
import org.seekloud.paradise.controller.Api4GameAgent._
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
	* Created by wangxicheng on 2018/10/25.
	*/
class LoginController(
											loginScene: LoginScene,
											stageCtx: StageContext) {
	val loginActor = system.spawn(LoginActor.create(stageCtx, this), "loginActor")

	private[this] val log = LoggerFactory.getLogger(this.getClass)

	loginScene.setLoginSceneListener(new LoginScene.LoginSceneListener {
		override def onButtonConnect(): Unit = {
			getLoginRspFromEs().map {
				case Right(r) =>
					val wsUrl = r.wsUrl
					val scanUrl = r.scanUrl
					loginScene.drawScanUrl(imageFromBase64(scanUrl))
					loginActor ! EstablishConnection2Es(wsUrl)

				case Left(_) =>
					log.debug("failed to getLoginRspFromEs.")
			}
		}
	})

	def imageFromBase64(base64Str:String): ByteArrayInputStream  = {
		if(base64Str == null) null

		import sun.misc.BASE64Decoder
		val decoder = new BASE64Decoder
		val bytes:Array[Byte]= decoder.decodeBuffer(base64Str)
		bytes.indices.foreach{ i =>
			if(bytes(i) < 0) bytes(i)=(bytes(i)+256).toByte
		}
		val  b = new ByteArrayInputStream(bytes)
		b
	}

	def showScene() {
		ClientBoot.addToPlatform {
			stageCtx.switchScene(loginScene.scene, "Login", false)
		}
	}

	def switchToGaming(id:String,name:String,accessCode:String, domain: String):Unit = {
		ClientBoot.addToPlatform {
			val random = new Random(System.nanoTime())
			val roomId = random.nextInt(2)+1
			val playGameScreen = new CanvasContainer(roomId,stageCtx)
			stageCtx.switchScene(playGameScreen.scene, flag = false)
			new GameController(id, name , roomId.toString , accessCode ,stageCtx, playGameScreen).start(domain)
		}
	}

}
