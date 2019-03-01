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

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.util.Timeout
import org.seekloud.paradise.core.{EsheepSyncClient, RoomManager, UserManager}
import org.seekloud.paradise.http.HttpService
import akka.actor.typed.ActorRef

import scala.language.postfixOps
import akka.actor.typed.scaladsl.adapter._
import org.seekloud.utils.ESSFSupport

import scala.language.postfixOps

/**
  * User: Taoz
  * Date: 8/26/2016
  * Time: 10:25 PM
  */
object Boot extends HttpService {

  import concurrent.duration._
  import org.seekloud.paradise.common.AppSettings._


  override implicit val system = ActorSystem("paradise", config)
  // the executor should not be the default dispatcher.
  override implicit val executor = system.dispatchers.lookup("akka.actor.my-blocking-dispatcher")
  override implicit val materializer = ActorMaterializer()
  override implicit val scheduler = system.scheduler

  override val timeout = Timeout(20 seconds) // for actor asks

  val log: LoggingAdapter = Logging(system, getClass)

  val userManager: ActorRef[UserManager.Command] = system.spawn(UserManager.create(),"userManager")
  val roomManager: ActorRef[RoomManager.Command] = system.spawn(RoomManager.create(),"roomManager")

  val esheepSyncClient:ActorRef[EsheepSyncClient.Command] = system.spawn(EsheepSyncClient.create,"esheepSyncClient")

  def main(args: Array[String]) {
    log.info("Starting.")
    Http().bindAndHandle(routes, httpInterface, httpPort)
//    ESSFSupport.main(new Array[String](1))
    log.info(s"Listen to the $httpInterface:$httpPort")
    log.info("Done.")
  }






}
