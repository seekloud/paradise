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

package org.seekloud.paradise.ptcl

/**
  * User: Taoz
  * Date: 8/29/2016
  * Time: 9:40 PM
  */
object Protocol {

  val frameRate = 150
//  val frameRate = 1000
  val rate = 20 // 后台每隔多少次往前端发消息
  val advanceFrame = 1 //客户端提前的帧数

  val maxAngle = Math.PI / 3

  val startLen = 9

  val startSpeed = 20
  val speedUp = 28

  val userPrefix = "paradise-guest-"
  val watcherName = "paradiseGuest"
}
