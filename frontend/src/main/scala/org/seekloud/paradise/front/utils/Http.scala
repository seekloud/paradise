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

package org.seekloud.paradise.front.utils

import io.circe.{Decoder, Error}
import org.scalajs.dom
import org.scalajs.dom.experimental._
import org.scalajs.dom.raw.{FileReader, FormData}

import scala.concurrent.Future

object Http {

  import concurrent.ExecutionContext.Implicits.global

  def get(url: String, withCookie: Boolean = true): Future[String] = {
    try {
      val requestInit = RequestInit(
        method = HttpMethod.GET,
        credentials = RequestCredentials.`same-origin`
        //        credentials = RequestCredentials.include,
        //        mode = RequestMode.`no-cors`
      )
      dom.experimental.Fetch.fetch(
        url, requestInit
      ).toFuture.flatMap { r =>
        //        println(s"msg sent to $url")
        r.text().toFuture
      }.map { rst =>
        //        println(s"rst got: $rst")
        rst
      }
    } catch {
      case e: Exception =>
        println(s"get error ${e.getMessage}")
        //        val errorDetailMsg = Shortcut.errorDetailMsg(e)
        //        JsFunc.alert(s"sendGet errMsg: $errorDetailMsg")
        throw e
    }

  }



  def getAndParse[T](
    url: String,
    withCookie: Boolean = true)(implicit decoder: Decoder[T]): Future[Either[Error, T]] = {
    import io.circe.parser._
    get(url, withCookie).map(s => decode[T](s))
  }






}
