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

import org.seekloud.paradise.front.utils.JsFunc.decodeURI
import org.scalajs.dom.window

/**
  * Created by zx0 on 2018/10/23.
  **/
object Func {
  def getParameter():(String,Map[String,String]) = {
    val url = window.location.href.split("/").last
    val paras = url.split("[?]")//前一个代表观战或者加入游戏，后一个是参数

    val pairs = paras(1).split("&").filter(s => s.length > 0)//参数对

    val list = pairs.map(_.split("=")).filter(_.length == 2)

    val map = list.map(l=>l(0) ->l(1)).toMap
    (paras(0),map)
  }

}
