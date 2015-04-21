/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing = Value val software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.util

import java.io.File

sealed trait Position

/** A position in a file which is simply a line and a column
 *
 *  @author Lucas Satabin
 */
case class SimplePosition(line: Int, column: Int) extends Position {

  override def toString =
    f"[$line.$column]"

}

case class StackedPosition(current: Position, next: Position) extends Position {

  override def toString =
    f"$current expanded from position $next"

}

/** The empty position */
object NoPosition extends Position {

  override def toString =
    "<unknown position>"

}
