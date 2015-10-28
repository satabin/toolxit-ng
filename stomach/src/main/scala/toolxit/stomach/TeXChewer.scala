/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package stomach

import eyes._
import mouth._
import util._

import scala.util.{
  Try,
  Failure
}

abstract class TeXChewer[Out] {

  val mouth: TeXMouth

  val env = mouth.env

  private def toStream[T](t: Try[T]): Stream[Try[T]] =
    t match {
      case Failure(_: EOIException) => Eos
      case t                        => Chunk(List(t))
    }

  private[this] def command: Stream[Try[Command]] =
    toStream(mouth.parseCommand)

  private[this] def number: Stream[Try[Int]] =
    toStream(mouth.parseNumber)

  def chew: Iteratee[Command, Try, Out]

}
