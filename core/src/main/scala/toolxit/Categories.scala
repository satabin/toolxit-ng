/*
* Copyright (c) 2015 Lucas Satabin
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
package toolxit

import gnieh.pp._

/** A category code to which a character is associated.
 *
 *  @author Lucas Satabin
 *
 */
object Category extends Enumeration {

  val ESCAPE_CHARACTER = Value(0) // 0
  val BEGINNING_OF_GROUP = Value(1) // 1
  val END_OF_GROUP = Value(2) // 2
  val MATH_SHIFT = Value(3) // 3
  val ALIGNMENT_TAB = Value(4) // 4
  val END_OF_LINE = Value(5) // 5
  val PARAMETER = Value(6) // 6
  val SUPERSCRIPT = Value(7) // 7
  val SUBSCRIPT = Value(8) // 8
  val IGNORED_CHARACTER = Value(9) // 9
  val SPACE = Value(10) // 10
  val LETTER = Value(11) // 11
  val OTHER_CHARACTER = Value(12) // 12
  val ACTIVE_CHARACTER = Value(13) // 13
  val COMMENT_CHARACTER = Value(14) // 14
  val INVALID_CHARACTER = Value(15) // 15

  def debug(cat: Value): Doc = cat match {
    case ESCAPE_CHARACTER   => "ESCAPE_CHARACTER(" :: 0 :: ")"
    case BEGINNING_OF_GROUP => "BEGINNING_OF_GROUP(" :: 1 :: ")"
    case END_OF_GROUP       => "END_OF_GROUP(" :: 2 :: ")"
    case MATH_SHIFT         => "MATH_SHIFT(" :: 3 :: ")"
    case ALIGNMENT_TAB      => "ALIGNMENT_TAB(" :: 4 :: ")"
    case END_OF_LINE        => "END_OF_LINE(" :: 5 :: ")"
    case PARAMETER          => "PARAMETER(" :: 6 :: ")"
    case SUPERSCRIPT        => "SUPERSCRIPT(" :: 7 :: ")"
    case SUBSCRIPT          => "SUBSCRIPT(" :: 8 :: ")"
    case IGNORED_CHARACTER  => "IGNORED_CHARACTER(" :: 9 :: ")"
    case SPACE              => "SPACE(" :: 10 :: ")"
    case LETTER             => "LETTER(" :: 11 :: ")"
    case OTHER_CHARACTER    => "OTHER_CHARACTER(" :: 12 :: ")"
    case ACTIVE_CHARACTER   => "ACTIVE_CHARACTER(" :: 13 :: ")"
    case COMMENT_CHARACTER  => "COMMENT_CHARACTER(" :: 14 :: ")"
    case INVALID_CHARACTER  => "INVALID_CHARACTER(" :: 15 :: ")"
  }

}
