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
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package mouth

import util._

import scala.util.Try

trait TeXNumbers {
  this: TeXMouth =>

  def number: Processor[Int] =
    signs()

  /** Parses zero or more signs and returns the resulting sign:
   *   - `1` means `+`
   *   - `-1` meas '-'
   */
  final def signs(current: Int = 1): Processor[Int] =
    for {
      _ <- spaces
      s <- read
      s <- s match {
        case CharacterToken('+', Category.OTHER_CHARACTER) =>
          for (() <- swallow)
            yield current
        case CharacterToken('-', Category.OTHER_CHARACTER) =>
          for (() <- swallow)
            yield -current
        case _ =>
          done(current)
      }
    } yield s

  val integerConstant: Processor[Int] =
    for {
      digits <- takeWhile {
        case CharacterToken(c, Category.OTHER_CHARACTER) =>
          c.isDigit
        case _ =>
          false
      }
    } yield digits.foldLeft(0) {
      case (acc, CharacterToken(c, _)) =>
        acc * 10 + c - 48
      case _ =>
        ???
    }

  val octalConstant: Processor[Int] =
    for {
      digits <- takeWhile {
        case CharacterToken(c, Category.OTHER_CHARACTER) =>
          c - 48 >= 0 && c - 48 < 8
        case _ =>
          false
      }
    } yield digits.foldLeft(0) {
      case (acc, CharacterToken(c, _)) =>
        acc * 8 + c - 48
      case _ =>
        ???
    }

  val hexConstant: Processor[Int] =
    for {
      digits <- takeWhile {
        case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
          true
        case CharacterToken(c, Category.OTHER_CHARACTER | Category.LETTER) =>
          c - 65 >= 0 && c - 65 < 6
        case _ =>
          false
      }
    } yield digits.foldLeft(0) {
      case (acc, CharacterToken(c, Category.OTHER_CHARACTER)) if c.isDigit =>
        acc * 16 + c - 48
      case (acc, CharacterToken(c, Category.OTHER_CHARACTER | Category.LETTER)) =>
        acc * 16 + c - 65
      case _ =>
        ???
    }

}
