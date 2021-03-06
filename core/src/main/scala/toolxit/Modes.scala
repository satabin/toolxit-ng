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

import enumeratum._

/** A mode in which TeX is.
 *
 *  @author Lucas Satabin
 *
 */
sealed trait Mode extends EnumEntry

object Mode extends Enum[Mode] {

  def values = findValues

  case object VerticalMode extends Mode
  case object InternalVerticalMode extends Mode
  case object HorizontalMode extends Mode
  case object RestrictedHorizontalMode extends Mode
  case object MathMode extends Mode
  case object DisplayMathMode extends Mode
}
