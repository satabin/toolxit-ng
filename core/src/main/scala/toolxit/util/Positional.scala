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

trait Positional {

  private var _pos: Position = NoPosition

  def pos: Position =
    _pos

  def atPos(line: Int, column: Int, next: Option[Position] = None): this.type = next match {
    case Some(next) =>
      _pos = StackedPosition(line, column, next)
      this
    case None =>
      _pos = SimplePosition(line, column)
      this
  }

  def atPos(p: Position): this.type = {
    _pos = p
    this
  }

}
