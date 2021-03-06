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

package org.seekloud.paradise.model

import java.awt.event.KeyEvent

import org.seekloud.paradise.ptcl._

/**
  * User: Taoz
  * Date: 9/3/2016
  * Time: 10:13 PM
  */
class GridOnClient(override val boundary: Point) extends Grid {

	override def debug(msg: String): Unit = println(msg)

	override def info(msg: String): Unit = println(msg)

	var killInfo = ("",0,0)

	override def feedApple(appleCount: Int): Unit = {} //do nothing.

	override def checkRush(newDirection: Point, snake: SkDt, tail: Point): Either[(String,SkDt), SkDt] = {
		val newHeader = snake.header + newDirection * snake.speed
		var speed = snake.speed
		if(snake.speed > Protocol.startSpeed && snake.length <= Protocol.startLen ){//长度小于9时强制减速
			speed = Protocol.startSpeed
			var temp = speedMap.getOrElse(frameCount,Map.empty)
			temp += (snake.id -> false)
			speedMap += (frameCount -> temp)
			//      direct = Point(newDirection.x * 16.0/24.0,newDirection.y * 16.0/24.0)
		}
		var len = snake.length
		if(snake.speed > Protocol.startSpeed && snake.length >Protocol.startLen) len = len -1
		Right(snake.copy(header = newHeader, tail = tail, direction = newDirection,length = len, protect = snake.protect -1,speed = speed))
	}

	override def updateDeadMessage(killerName: String,deadId: String,deadKill: Int,deadLength: Int): Unit = {}

	override def updateDeadSnake(updatedSnakes:List[SkDt],mapKillCounter:Map[String,Int],deads:List[SkDt]):Unit = {
		grid ++= updatedSnakes.map(s => s.header -> Body(s.id, s.length,s.color,s.protect))
		snakes = updatedSnakes.map(s => (s.id, s)).toMap
	}
	
}
