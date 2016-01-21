/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

/** @author Lucas Satabin
 *
 */
class TeXException(msg: String, inner: Throwable) extends Exception(msg, inner) {
  def this() = this(null, null)
  def this(msg: String) = this(msg, null)
}

class TeXInternalException(msg: String, inner: Throwable) extends Exception(msg, inner) {
  def this() = this(null, null)
  def this(msg: String) = this(msg, null)
}

class ControlSequenceException(msg: String) extends TeXException(msg)

case class EOIException(line: Int, column: Int) extends Exception
