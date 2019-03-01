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

package org.seekloud.paradise

import javafx.application.{Application, Platform}
import javafx.stage.Stage

import akka.actor.{ActorSystem, Scheduler}
import akka.stream.ActorMaterializer
import org.seekloud.paradise.common.AppSettings._
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.Http
import org.seekloud.paradise.actor.PlayGameWebSocket
import org.seekloud.paradise.common.StageContext
import org.seekloud.paradise.controller.LoginController
import org.seekloud.paradise.model.GridOnClient
import org.seekloud.paradise.scene.{LoginScene, GameCanvas, CanvasContainer}

import scala.util.{Failure, Success}


/**
	* Created by wangxicheng on 2018/10/23.
	*/
object ClientBoot {
	implicit val system = ActorSystem("paradise", config)
	// the executor should not be the default dispatcher.
	implicit val executor = system.dispatchers.lookup("akka.actor.my-blocking-dispatcher")
	implicit val materializer: ActorMaterializer = ActorMaterializer()
	implicit val scheduler: Scheduler = system.scheduler

	def addToPlatform(fun: => Unit) = {
		Platform.runLater(() => fun)
	}

}

class ClientBoot extends javafx.application.Application {

	import ClientBoot._
	override def start(mainStage: Stage): Unit = {
    val context = new StageContext(mainStage)
    val loginScene = new LoginScene()
    val loginController = new LoginController(loginScene, context)
		loginController.showScene()


//		val gameViewScene = new GameScene()
//		mainStage.setMaximized(true)
//		context.switchScene(gameViewScene.GameViewScene,"Medusa")


	}

}
