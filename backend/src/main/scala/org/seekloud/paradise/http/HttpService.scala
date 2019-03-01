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

package org.seekloud.paradise.http

import akka.actor.{ActorRef, ActorSystem, Scheduler}
import akka.http.scaladsl.model.headers.{CacheDirective, CacheDirectives, `Cache-Control`}
import akka.http.scaladsl.server.{Directive0, Route}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.Timeout

import scala.concurrent.ExecutionContextExecutor

/**
  * User: Taoz
  * Date: 8/26/2016
  * Time: 10:27 PM
  */
trait HttpService extends GameService with ResourceService with EsheepApiService {


  implicit val system: ActorSystem

  implicit val executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  implicit val timeout: Timeout

  implicit val scheduler: Scheduler

  //  只使用强制缓存,设置强制缓存时间,去除协商缓存的字段
  //  addCacheControlHeadersWithFilter(`public`,`max-age`(cacheSeconds))
  //  private val cacheSeconds = 24 * 60 * 60
  def addCacheControlHeadersWithFilter(first: CacheDirective, more: CacheDirective*): Directive0 = {
    mapResponseHeaders { headers =>
      `Cache-Control`(first, more: _*) +: headers.filterNot(h => h.name() == "Last-Modified" || h.name() == "ETag")
    }
  }

  //只使用强制缓存,设置不缓存，去除协商缓存字段
  def setNoCacheInHeaderWithFilter: Directive0 = {
    mapResponseHeaders { headers =>
      `Cache-Control`.apply(CacheDirectives.`no-cache`) +: headers.filterNot(h => h.name() == "Last-Modified" || h.name() == "ETag")
    }
  }

  val playGame = (pathPrefix("playGame") & get){
    parameter(
      'playerId.as[String],
      'playerName,
      'accessCode.as[String],
      'roomId.as[Long].?) { (playerId,name, acCode,roomId) =>
//      authPlatUser(acCode){user=>
        setNoCacheInHeaderWithFilter {
          getFromResource("html/netSnake.html")
        }
//      }
    }
  }

  val watchGame = (pathPrefix("watchGame") & get){
    parameter(
      'playerId.as[String].?,
      'accessCode.as[String],
      'roomId.as[Long]) { (playerId, acCode,roomId) =>
//      authPlatUser(acCode){user=>
        setNoCacheInHeaderWithFilter {
          getFromResource("html/netSnake.html")
        }
//      }
    }
  }

  val watchRecord = (pathPrefix("watchRecord") & get){
    parameter(
      'recordId.as[Long],
      'playerId.as[String],
      'frame.as[Int],
      'accessCode.as[String]) { (recordId, playerId, frame, accessCode) =>
      //      authPlatUser(acCode){user=>
        setNoCacheInHeaderWithFilter {
          getFromResource("html/netSnake.html")
        }
      //      }
    }
  }

  val routes =
    pathPrefix("paradise") {
      playGame ~ watchGame ~ watchRecord ~ gameRoutes ~ resourceRoutes ~ roomApiRoutes
    }


}
