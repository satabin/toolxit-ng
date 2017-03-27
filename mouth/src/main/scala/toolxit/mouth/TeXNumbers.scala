/*
* Copyright (c) 2017 Lucas Satabin
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

  def isDecimalDigit(tok: Token): Boolean = tok match {
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      c.isDigit
    case _ =>
      false
  }

  def integerConstant(acc: Int): Processor[Int] =
    for {
      digit <- read
      i <- digit match {
        case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
          for {
            () <- swallow
            i <- integerConstant(acc * 10 + (c - 48))
          } yield i
        case t =>
          done(acc)
      }
    } yield i

  def isOctalDigit(tok: Token): Boolean = tok match {
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      c - 48 >= 0 && c - 48 < 8
    case _ =>
      false
  }

  def octalConstant(acc: Int): Processor[Int] =
    for {
      digit <- read
      i <- digit match {
        case CharacterToken(c, Category.OTHER_CHARACTER) if c - 48 >= 0 && c - 48 < 8 =>
          for {
            () <- swallow
            i <- octalConstant(acc * 8 + (c - 48))
          } yield i
        case t =>
          done(acc)
      }
    } yield i

  def isHexDigit(tok: Token): Boolean = tok match {
    case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
      true
    case CharacterToken(c, Category.OTHER_CHARACTER | Category.LETTER) =>
      c - 65 >= 0 && c - 65 < 6
    case _ =>
      false
  }

  def hexConstant(acc: Int): Processor[Int] =
    for {
      digit <- read
      i <- digit match {
        case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
          for {
            () <- swallow
            i <- hexConstant(acc * 16 + (c - 48))
          } yield i
        case CharacterToken(c, Category.OTHER_CHARACTER) if c - 65 >= 0 && c - 65 < 6 =>
          for {
            () <- swallow
            i <- hexConstant(acc * 16 + (c - 65))
          } yield i
        case t =>
          done(acc)
      }
    } yield i

  def bit8(pos: Position): Processor[Byte] =
    number.flatMap { i =>
      if (i >= 0 && i <= 255)
        done(i.toByte)
      else
        throwError(new TeXMouthException(f"8-bit number expected but got $i", pos))
    }

  def char(pos: Position): Processor[Char] =
    number.map(_.toChar)

  def catNumber(pos: Position): Processor[Category] =
    number.flatMap { i =>
      if (i >= 0 && i <= 15)
        done(Category.withValue(i))
      else
        throwError(new TeXMouthException(f"Category number (0-15) expected but got $i", pos))
    }

  val unsignedNumber: Processor[Int] =
    for {
      tok <- read
      i <- tok match {
        case t if isDecimalDigit(t) =>
          integerConstant(0)
        case CharacterToken(''', Category.OTHER_CHARACTER) =>
          for {
            () <- swallow
            i <- octalConstant(0)
          } yield i
        case CharacterToken('"', Category.OTHER_CHARACTER) =>
          for {
            () <- swallow
            i <- hexConstant(0)
          } yield i
        case CharacterToken('`', Category.OTHER_CHARACTER) =>
          for {
            () <- swallow
            // the character token is not expanded
            next <- read
            i <- next match {
              case CharacterToken(c, _) => swallow.andThen(done(c.toInt))
              case ControlSequenceToken(name, _) if name.size == 1 => swallow.andThen(done(name(0).toInt))
              case _ => throwError[Token](TeXMouthException("character or single character constrol sequence expected", next.pos))
            }
          } yield i
        case Primitives.IntegerParameter(name) =>
          for (() <- swallow)
            yield env.integerParameter(name)

        case Primitives.SpecialInteger(name) =>
          for (() <- swallow)
            yield env.integers.getOrElse(name, 0)

        case tok @ Primitives.Codename("catcode") =>
          for {
            () <- swallow
            i <- bit8(tok.pos)
          } yield env.category(i.toChar).value

        case tok @ Primitives.Codename("mathcode") =>
          ???

        case tok @ Primitives.Codename("lccode") =>
          ???

        case tok @ Primitives.Codename("uccode") =>
          ???

        case tok @ Primitives.Codename("sfcode") =>
          ???

        case tok @ Primitives.Codename("sfcode") =>
          ???

        case tok @ Primitives.Codename("delcode") =>
          ???

        // internal integers with parameters
        case tok @ Primitives.InternalInteger("count") =>
          for {
            () <- swallow
            i <- bit8(tok.pos)
          } yield env.count(i)

        case tok @ Primitives.InternalInteger("hyphenchar") =>
          for {
            () <- swallow
            f <- font
          } yield ???

        case tok @ Primitives.InternalInteger("skewchar") =>
          for {
            () <- swallow
            f <- font
          } yield ???

        case Primitives.InternalInteger(name) =>
          for (() <- swallow)
            yield env.integers.getOrElse(name, 0)

        case ControlSequenceToken(CharDef(c), _) =>
          for (() <- swallow)
            yield c.toInt

        case tok =>
          throwError[Token](new TeXMouthException(f"Expected integer but got $tok", tok.pos))

      }
      _ <- optSpace
    } yield i

  val number: Processor[Int] =
    for {
      sign <- signs()
      i <- unsignedNumber
    } yield sign * i

  object CharDef {
    def unapply(name: String): Option[Char] = env.css(name) match {
      case Some(TeXChar(_, c)) => Some(c)
      case _                   => None
    }
  }

}
