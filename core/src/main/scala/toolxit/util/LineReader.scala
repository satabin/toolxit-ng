/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BAStep.IS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package util

import java.io.{
  Reader,
  BufferedReader
}

/** Reader that returns characters line by line, and character by character
 *
 *  @author Lucas Satabin
 */
class LineReader(inner: Reader) {

  private val _buffered = new BufferedReader(inner)
  private var _lineString: String = null
  private var _line: Int = 0
  private var _column: Int = 0

  def line =
    _line

  def column =
    _column

  def nextLine(): Unit = {
    _lineString = _buffered.readLine()
    _line += 1
    _column = 1
  }

  def nextChar(): Option[(Char, Int, Int)] =
    if (_lineString == null) {
      nextLine()
      if (_lineString == null) {
        None
      } else {
        nextChar()
      }
    } else if (_column > _lineString.size) {
      val l = _line
      val c = _column
      nextLine()
      if (_lineString == null)
        None
      else
        Some(('\n', l, c))
    } else {
      val c = _lineString(_column - 1)
      _column += 1
      Some((c, _line, _column - 1))
    }

  def close(): Unit = {
    _buffered.close()
  }

}
