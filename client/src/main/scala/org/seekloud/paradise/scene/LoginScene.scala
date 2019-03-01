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

import java.io.ByteArrayInputStream
import javafx.geometry.Insets
import javafx.scene.canvas.Canvas
import javafx.scene.{Group, Scene}
import javafx.scene.control.Button
import javafx.scene.layout.{GridPane, Pane}
import javafx.scene.paint.{Color, Paint}
import javafx.scene.text.{Font, FontPosture, FontWeight}

import akka.actor.typed.ActorRef
import org.seekloud.paradise.ClientBoot
import org.seekloud.paradise.ClientBoot
//import org.seekloud.paradise.actor.WSClient
//import org.seekloud.paradise.actor.WSClient.ConnectGame
import org.seekloud.paradise.common.StageContext
//import org.seekloud.paradise.controller.GameController
import javafx.scene.image.Image
//import org.seekloud.paradise.controller.LoginController
import com.sun.xml.internal.messaging.saaj.util.ByteInputStream
/**
	* Created by wangxicheng on 2018/10/24.
	*/
object LoginScene {
	trait LoginSceneListener {
		def onButtonConnect()
	}
}
class LoginScene() {

	import LoginScene._

	val width = 500
	val height = 500
	val group = new Group
	val button = new Button("登录")



	val canvas = new Canvas(width, height)
	val canvasCtx = canvas.getGraphicsContext2D
	var loginSceneListener: LoginSceneListener = _


	button.setLayoutX(220)
	button.setLayoutY(240)
	button.setStyle("-fx-background-radius: 5; -fx-border-radius: 5; -fx-effect: dropShadow(three-pass-box, #528B8B, 10.0, 0, 0, 0); -fx-font:17 Helvetica; -fx-font-weight: bold; -fx-font-posture:italic")



	//canvasCtx.setFill(Color.rgb(153, 255, 153))
	canvasCtx.setFill(Color.web("#ccc"))
	canvasCtx.fillRect(0, 0, width, height)
	canvasCtx.setFont(Font.font("Helvetica", FontWeight.BOLD ,FontPosture.ITALIC,28))
	canvasCtx.setFill(Color.web("rgb(250, 250, 250)"))
	canvasCtx.fillText(s"Welcome to paradise!",100,125)
	group.getChildren.add(canvas)
	group.getChildren.add(button)

	val scene = new Scene(group)

	button.setOnAction(_ => loginSceneListener.onButtonConnect())

	def drawScanUrl(imageStream:ByteArrayInputStream) = {
		ClientBoot.addToPlatform{
			group.getChildren.remove(button)
			val img = new Image(imageStream)
			canvasCtx.drawImage(img,100,100)
			canvasCtx.setFont(Font.font("Helvetica", FontWeight.BOLD ,FontPosture.ITALIC,28))
			canvasCtx.fillText(s"请扫码登录！",160,70)
		}
	}

	def setLoginSceneListener(listener: LoginSceneListener) {
		loginSceneListener = listener
	}
}
