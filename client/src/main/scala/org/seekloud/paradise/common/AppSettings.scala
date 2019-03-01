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

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

/**
  * User: Taoz
  * Date: 9/4/2015
  * Time: 4:29 PM
  */
object AppSettings {


  val log = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.parseResources("product.conf").withFallback(ConfigFactory.load())

  val appConfig = config.getConfig("app")
  val gameDataDirectoryPath = appConfig.getString("gameDataDirectoryPath")

  val httpInterface = appConfig.getString("http.interface")
  val httpPort = appConfig.getInt("http.port")


  val appSecureMap = {
    import collection.JavaConverters._
    val appIds = appConfig.getStringList("client.appIds").asScala
    val secureKeys = appConfig.getStringList("client.secureKeys").asScala
    require(appIds.length == secureKeys.length, "appIdList.length and secureKeys.length not equel.")
    appIds.zip(secureKeys).toMap
  }

  val esheepConfig = appConfig.getConfig("esheep")
  val esheepProtocol = esheepConfig.getString("protocol")
  val esheepDomain = esheepConfig.getString("domain")
  val esheepGameId = esheepConfig.getLong("gameId")
  val esheepGsKey = esheepConfig.getString("gsKey")

  val slickConfig = config.getConfig("slick.db")
  val slickUrl = slickConfig.getString("url")
  val slickUser = slickConfig.getString("user")
  val slickPassword = slickConfig.getString("password")
  val slickMaximumPoolSize = slickConfig.getInt("maximumPoolSize")
  val slickConnectTimeout = slickConfig.getInt("connectTimeout")
  val slickIdleTimeout = slickConfig.getInt("idleTimeout")
  val slickMaxLifetime = slickConfig.getInt("maxLifetime")

}
