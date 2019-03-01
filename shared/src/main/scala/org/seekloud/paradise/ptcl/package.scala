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

package object ptcl {

  sealed trait Spot
  case class Body(id: String, life: Int, color:String,protect:Int) extends Spot//出生保护时间
  //  case class Header(id: Long, life: Int) extends Spot
  case class Apple(score: Int, life: Int) extends Spot
  case class Barrier(num:Int,center:Boolean) extends  Spot
  //  case object DeadBody extends Spot

  case class Score(id: String, n: String, k: Int, l: Int, t: Option[Long] = None)
  //  case class Bd(id: Long, life: Int, color:String,protect:Int, x: Double, y: Double)
  case class Ap(score: Int, life: Int, x: Double, y: Double)
  case class Br(num:Int,x: Double, y: Double,center:Boolean)
  case class Tp(id: String, color:String,protect:Int, x: Double, y: Double)


  case class Point(x: Double, y: Double) {
    def +(other: Point) = Point(x + other.x, y + other.y)

    def -(other: Point) = Point(x - other.x, y - other.y)

    def *(n: Double) = Point(x * n, y * n)

    def %(other: Point) = Point(x % other.x, y % other.y)

    def pathTo(other: Point): Option[Point] = {
      import math._
      val (x0, x1) = if(x > other.x) (other.x, x) else (x, other.x)
      val (y0, y1) = if(y > other.y) (other.y, y) else (y, other.y)
      val distance = sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2))
      def step(distance: Double) = {
        distance match {
          case 0 => 0
          case n if n > 0 && n < 5 => 1
          case n if n >= 5 && n < 10 => 3
          case n if n >= 10 && n < 15 => 5
          case n if n >= 15 && n < 20 => 7
          case n if n >= 20 && n < 25 => 9
          case n if n >= 25 && n <= 30 => 11
          case n if n >= 30 && n <= 40 => 19
          case n if n >= 40 && n <= 50 => 25
          case _ => 30
        }
      }
      if (distance <= 30 ) {
        None
      } else  {
        val nextX = if (x > other.x) x - step(x - other.x) else x + step(other.x - x)
        val nextY = if (y > other.y) y - step(y - other.y) else y + step(other.y - y)
        Some(Point(nextX, nextY))
      }
    }
  }


  class Snake(x: Double, y: Double, len: Int = 10, d: Point = Point(1, 0)) {
    var length = len
    var direction = d
    var header = Point(x, y)
  }

  case class SkDt(
                   id: String,
                   name: String,
                   color: String,
                   header: Point = Point(20, 20),
                   tail: Point,
                   direction: Point = Point(1, 0),
                   length: Int = Protocol.startLen,
                   kill: Int = 0,
                   protect: Int = 1000/Protocol.frameRate * 10,//10s保护时间
                   speed: Int = Protocol.startSpeed
                 )

  case class Dead(
                   snake:SkDt,
                   killer:String
                 )

  case class DeadSk(
                     id: String,
                     length:Int=9,
                     kill:Int=0,
                     killer:String
                   )

  case class EatInfo(
                      snakeId:String,
                      apples:List[Ap]
                    )


  object Boundary{
    val w = 1600*2
    val h = 800*2
  }


  object Window{
    val w = 1600
    val h = 800
  }

  val judgeRange = 8.0

  def getSnakeRadius(len:Int):Double = {
    6/getScale(len)+ len * 0.008
  }

  def getScale(len:Int):Double ={
    -0.02 * Math.sqrt(len) + 1.06
  }


}
