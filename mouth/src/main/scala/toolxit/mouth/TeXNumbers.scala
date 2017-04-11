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

  def bit15(pos: Position): Processor[Int] =
    number.flatMap { i =>
      if (i >= 0 && i <= 32767)
        done(i)
      else
        throwError(new TeXMouthException(f"15-bit number expected but got $i", pos))
    }

  def bit24(pos: Position): Processor[Int] =
    number.flatMap { i =>
      if (i < 0x1000000)
        done(i)
      else
        throwError(new TeXMouthException(f"24-bit number expected but got $i", pos))
    }

  def char(pos: Position): Processor[Char] =
    number.flatMap { i =>
      if (i >= 0 && i <= 65535)
        done(i.toChar)
      else
        throwError(new TeXMouthException(f"Character expected but got $i", pos))
    }

  def catNumber(pos: Position): Processor[Byte] =
    number.flatMap { i =>
      if (i >= 0 && i <= 15)
        done(i.toByte)
      else
        throwError(new TeXMouthException(f"Category number (0-15) expected but got $i", pos))
    }

  val internalInteger: Processor[Int] =
    for {
      tok <- read
      i <- tok match {
        case ControlSequenceToken(CharDef(c), _) =>
          for (() <- swallow)
            yield c.toInt

        case ControlSequenceToken(CountDef(i), _) =>
          for (() <- swallow)
            yield i

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
          for {
            () <- swallow
            i <- char(tok.pos)
          } yield env.mathcode(i.toChar)

        case tok @ Primitives.Codename("lccode") =>
          for {
            () <- swallow
            c <- char(tok.pos)
          } yield env.lccode(c).toInt

        case tok @ Primitives.Codename("uccode") =>
          for {
            () <- swallow
            c <- char(tok.pos)
          } yield env.uccode(c).toInt

        case tok @ Primitives.Codename("sfcode") =>
          ???

        case tok @ Primitives.Codename("delcode") =>
          for {
            () <- swallow
            c <- char(tok.pos)
          } yield env.delcode(c)

        // internal integers with parameters
        case tok @ Primitives.InternalInteger("count") =>
          for {
            () <- swallow
            i <- bit8(tok.pos)
          } yield env.count(i)

        case tok @ Primitives.InternalInteger("hyphenchar") =>
          for {
            () <- swallow
            (fn, mag) <- font
          } yield env.fontManager.hyphenchar(fn, mag).map(_.toInt).getOrElse(0)

        case tok @ Primitives.InternalInteger("skewchar") =>
          for {
            () <- swallow
            (fn, mag) <- font
          } yield env.fontManager.skewchar(fn, mag).map(_.toInt).getOrElse(0)

        case Primitives.InternalInteger(name) =>
          for (() <- swallow)
            yield env.integers.getOrElse(name, 0)

        case tok =>
          throwError[Token](new TeXMouthException(f"Expected internal integer but got $tok", tok.pos))

      }
    } yield i

  val normalInteger: Processor[Int] =
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
              case _ => throwError[Token](new TeXMouthException("character or single character constrol sequence expected", next.pos))
            }
          } yield i

        case StartsInternalInteger() => internalInteger

        case tok =>
          throwError[Token](new TeXMouthException(f"Expected integer but got $tok", tok.pos))

      }
      _ <- optSpace
    } yield i

  val number: Processor[Int] =
    for {
      sign <- signs()
      t <- read
      i <- t match {
        case StartsNormalInteger() => normalInteger
        case StartsInternalDimen() => internalDimension.map(_.sps)
        // TODO add support for coerced integers
        case _                     => throwError[Token](new TeXMouthException(f"Number expected but got $t", t.pos))
      }
    } yield sign * i

  // extractors

  object StartsNormalInteger {
    def unapply(tok: Token): Boolean =
      tok match {
        case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit => true
        case CharacterToken(''', Category.OTHER_CHARACTER) => true
        case CharacterToken('"', Category.OTHER_CHARACTER) => true
        case CharacterToken('`', Category.OTHER_CHARACTER) => true
        case _ => StartsInternalInteger.unapply(tok)
      }
  }

  object StartsInternalInteger {
    def unapply(tok: Token): Boolean =
      tok match {
        case Primitives.IntegerParameter(_)      => true
        case Primitives.SpecialInteger(_)        => true
        case Primitives.Codename(_)              => true
        case Primitives.InternalInteger(_)       => true
        case ControlSequenceToken(CharDef(_), _) => true
        case CountdefToken(_)                    => true
        case _                                   => false
      }
  }

  object CharDef {
    def unapply(name: String): Option[Char] = env.css(name) match {
      case Some(TeXChar(_, CharacterToken(c, _))) => Some(c)
      case _                                      => None
    }
  }

  object CountDef {
    def unapply(name: String): Option[Int] = env.css(name) match {
      case Some(TeXCounter(_, cnt)) => Some(env.count(cnt))
      case _                        => None
    }
  }

}
