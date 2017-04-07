/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.util

import java.io.File

sealed trait Position {
  val name: Option[String]
  val line: Int
  val column: Int
}

/** A position in a file which is simply a line and a column
 *
 *  @author Lucas Satabin
 */
case class SimplePosition(line: Int, column: Int, name: Option[String]) extends Position {

  override def toString = name match {
    case Some(name) =>
      f"$name: [$line.$column]"
    case None =>
      f"[$line.$column]"
  }

}

case class StackedPosition(current: Position, next: Position) extends Position {

  val line = current.line

  val column = current.column

  val name = current.name

  override def toString =
    f"$current expanded from position $next"

}

/** The empty position */
object NoPosition extends Position {

  val line = -1

  val column = -1

  val name = None

  override def toString =
    "<unknown position>"

}
