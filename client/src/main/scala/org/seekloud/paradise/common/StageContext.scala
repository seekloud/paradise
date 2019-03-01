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

package org.seekloud.paradise.common

import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.stage.{Stage, WindowEvent}

/**
	* Created by wangxicheng on 2018/10/24.
	*/

object StageContext{
	trait StageListener {
		def onCloseRequest(): Unit
	}
}

class StageContext(stage: Stage) {

	import StageContext._

	var stageListener: StageListener = _

	stage.setOnCloseRequest(_ => stageListener.onCloseRequest())

	case class syncWindowSize(windowWidth: Double, windowHeight: Double)
	def getWindowSize = {
		val windowWidth = stage.getWidth
		val windowHeight = stage.getHeight
		syncWindowSize(windowWidth, windowHeight)
	}

	def switchScene(scene: Scene, title: String = "Paradise", flag: Boolean) = {
		stage.setScene(scene)
		stage.setTitle(title)
		stage.sizeToScene()
		stage.centerOnScreen()
		if(flag) {
			stage.setFullScreen(true)
		}
		stage.getWidth
		stage.show()
	}

	def setStageListener(listener: StageListener): Unit = {
		stageListener = listener
	}

	def closeStage(): Unit = {
		stage.close()
		System.exit(0)
	}
}