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
package toolxit

import enumeratum.values._

/** A category code to which a character is associated.
 *
 *  @author Lucas Satabin
 *
 */
sealed abstract class Category(val value: Int) extends IntEnumEntry
object Category extends IntEnum[Category] {

  val values = findValues

  case object ESCAPE_CHARACTER extends Category(0) // 0
  case object BEGINNING_OF_GROUP extends Category(1) // 1
  case object END_OF_GROUP extends Category(2) // 2
  case object MATH_SHIFT extends Category(3) // 3
  case object ALIGNMENT_TAB extends Category(4) // 4
  case object END_OF_LINE extends Category(5) // 5
  case object PARAMETER extends Category(6) // 6
  case object SUPERSCRIPT extends Category(7) // 7
  case object SUBSCRIPT extends Category(8) // 8
  case object IGNORED_CHARACTER extends Category(9) // 9
  case object SPACE extends Category(10) // 10
  case object LETTER extends Category(11) // 11
  case object OTHER_CHARACTER extends Category(12) // 12
  case object ACTIVE_CHARACTER extends Category(13) // 13
  case object COMMENT_CHARACTER extends Category(14) // 14
  case object INVALID_CHARACTER extends Category(15) // 15

}
