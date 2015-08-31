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
package mouth

import scala.util.{
  Try,
  Failure,
  Success
}

import scala.annotation.tailrec

trait TeXNumbers {
  this: TeXMouth =>

  def parseNumber(): Try[Int] =
    parseSigns()

  def parsePlusOrMinus(): Try[Int] =
    read() flatMap {
      case CharacterToken('+', Category.OTHER_CHARACTER) =>
        swallow()
        Success(1)
      case CharacterToken('-', Category.OTHER_CHARACTER) =>
        swallow()
        Success(-1)
      case tok =>
        Failure(new TeXMouthException(s"Plus or minus expected but $tok found", tok.pos))
    }

  /** Parses zero or more signs and returns the resulting sign:
   *   - `1` means `+`
   *   - `-1` meas '-'
   */
  @tailrec
  final def parseSigns(current: Int = 1): Try[Int] =
    parseSpaces() match {
      case Success(()) =>
        parsePlusOrMinus() match {
          case Success(sign) =>
            parseSigns(current * sign)
          case Failure(_) =>
            parseSpaces().map(_ => current)
        }
      case Failure(t) =>
        Failure(t)
    }

  @tailrec
  final def parseIntegerConstant(acc: Option[Int] = None): Try[Int] =
    parseDigit() match {
      case Success(i) =>
        parseIntegerConstant(Some(acc.getOrElse(0) * 10 + i))
      case Failure(_) if acc.isDefined =>
        Success(acc.get)
      case Failure(t) =>
        Failure(t)
    }

  @tailrec
  final def parseOctalConstant(acc: Option[Int] = None): Try[Int] =
    parseOctalDigit() match {
      case Success(i) =>
        parseOctalConstant(Some(acc.getOrElse(0) * 8 + i))
      case Failure(_) if acc.isDefined =>
        Success(acc.get)
      case Failure(t) =>
        Failure(t)
    }

  @tailrec
  final def parseHexConstant(acc: Option[Int] = None): Try[Int] =
    parseHexDigit() match {
      case Success(i) =>
        parseHexConstant(Some(acc.getOrElse(0) * 16 + i))
      case Failure(_) if acc.isDefined =>
        Success(acc.get)
      case Failure(t) =>
        Failure(t)
    }

  def parseDigit(): Try[Int] =
    read() flatMap {
      case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
        Success(c - 48)
      case tok =>
        Failure(new TeXMouthException(s"Digit expected but $tok found", tok.pos))
    }

  def parseOctalDigit(): Try[Int] =
    read() flatMap {
      case CharacterToken(c, Category.OTHER_CHARACTER) if c - 48 >= 0 && c - 48 < 8 =>
        Success(c - 48)
      case tok =>
        Failure(new TeXMouthException(s"Octal digit expected but $tok found", tok.pos))
    }

  def parseHexDigit(): Try[Int] =
    read() flatMap {
      case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
        Success(c - 48)
      case CharacterToken(c, Category.OTHER_CHARACTER | Category.LETTER) if c - 65 >= 0 && c - 65 < 6 =>
        Success(c - 65)
      case tok =>
        Failure(new TeXMouthException(s"Octal digit expected but $tok found", tok.pos))
    }

}
