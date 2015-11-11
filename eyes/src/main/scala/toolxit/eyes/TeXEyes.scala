/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package eyes

import scala.util.{
  Try,
  Success,
  Failure
}

import util._

/** The TeX eyes as defined in the ''TeX Book'' mainly in chapter 8 '''The Characters You Type'''.
 *  This is the lexer that generates a stream of TeX tokens out of a character stream.
 *  It uses as input a character stream that is line aware, allowing for better error messages,
 *  because the parsed lines is recognized as it is parsed and can be partially displayed in messages.
 *
 *  @author Lucas Satabin
 */
class TeXEyes(env: TeXEnvironment) extends Iteratees[(Char, Int, Int), Try] {

  object Hexa {
    val hexaLower = "0123456789abcdef"
    def unapply(c: Char): Option[Int] = {
      val idx = hexaLower.indexOf(c)
      if (idx >= 0)
        Some(idx)
      else
        None
    }
  }

  // the environment defines a lot of useful extractors and variables
  import env._

  /** The input is first preprocessed to replace character code by their
   *   value
   *   a character code is of the form:
   *     - `^^XX` where X is one of `0123456789abcdef`
   *     - `^^A` where A is a letter (< 128)
   *     - any other character
   */
  val preprocessor: Iteratee[(Char, Int, Int), Try, Unit] =
    peek(4).flatMap {
      case List(
        (SUPERSCRIPT(sup1), l, c),
        (SUPERSCRIPT(sup2), _, _),
        (Hexa(h1), _, _),
        (Hexa(h2), _, _)) if sup1 == sup2 =>
        for {
          () <- swallow(4)
          () <- pushback((((h1 << 4) + h2).toChar, l, c))
        } yield ()
      case List(
        (SUPERSCRIPT(sup1), l, c),
        (SUPERSCRIPT(sup2), _, _),
        (ch, _, _), _*) if sup1 == sup2 =>
        for {
          () <- swallow(3)
          () <- pushback((if (ch < 64) (ch + 64).toChar else (ch - 64).toChar, l, c))
        } yield ()
      case _ =>
        noop
    }

  def tokenize(line: Int, column: Int): Iteratee[(Char, Int, Int), Try, Token] =
    for {
      () <- preprocessor
      t <- take(EOIException(line, column)).flatMap {
        case (IGNORED_CHARACTER(_), l, c) =>
          // the obvious case is to ignore currently ignored characters
          tokenize(l, c)
        case (SPACE(_), l, c) if state == ReadingState.S || state == ReadingState.N =>
          // when in reading state 'skipping blanks' or 'new line', spaces are ignored as well
          tokenize(l, c)
        case (COMMENT_CHARACTER(_), _, _) =>
          // when a comment started it lasts until the end of line is reached
          // the end of line character is then eaten as well
          for {
            () <- dropWhile {
              case (END_OF_LINE(_), _, _) => false
              case _                      => true
            }
            // eat the end of line character
            () <- swallow
            Some((_, l, c)) <- peek
            t <- tokenize(l, c)
          } yield t
        case (ACTIVE_CHARACTER(ch), l, c) =>
          // an active character is control sequence, and after control sequence we go into
          // the 'skipping blanks' state
          state = ReadingState.S
          done(ControlSequenceToken(ch.toString, true).atPos(SimplePosition(l, c)))
        case (ESCAPE_CHARACTER(_), l, c) =>
          // a control sequence starts with an escape character and is followed by letters
          // or a single other character
          // after control sequence we go into the 'skipping blanks' state
          env.state = ReadingState.S
          peek.flatMap {
            case Some((LETTER(_), _, _)) =>
              for {
                csname <- takeWhile {
                  case (LETTER(_), _, _) => true
                  case _                 => false
                }
              } yield ControlSequenceToken(csname.mkString, false).atPos(SimplePosition(l, c))
            case Some((ch, _, _)) =>
              for(() <- swallow)
                yield ControlSequenceToken(ch.toString, false).atPos(SimplePosition(l, c))
            case None =>
              // we reached end of input, this is absolutely not correct
              throwError(new TeXEyesException(l, c, "control sequence name expected but end of input reached"))
          }
        case (END_OF_LINE(_), l, c) if env.state == ReadingState.N =>
          // when reading end of line and if we are in the 'new line' reading state,
          // this is equivalent to the `\par` control sequence and stay in the same state
          done(ControlSequenceToken("par", false).atPos(SimplePosition(l, c)))
        case (END_OF_LINE(_), l, c) if env.state == ReadingState.M =>
          // otherwise in any reading state 'middle of line', it is considered as a space and we go into
          // 'new line' reading state
          env.state = ReadingState.N
          done(CharacterToken(' ', Category.SPACE).atPos(SimplePosition(l, c)))
        case (END_OF_LINE(_), l, c) =>
          // otherwise we are skipping blank characters, so just ignore it
          tokenize(l, c)
        case (SPACE(_), l, c) =>
          // if this is a space character, we go into the 'skipping blanks' reading state
          // the space token is always ' ' even if it were some other characters that
          // was assigned the SPACE category
          env.state = ReadingState.S
          done(CharacterToken(' ', Category.SPACE).atPos(SimplePosition(l, c)))
        case (ch, l, c) =>
          // otherwise, any other character is returned and we are now in the 'middle of line'
          // reading state
          env.state = ReadingState.M
          done(CharacterToken(ch, category(ch)).atPos(SimplePosition(l, c)))
      }
    } yield t

}

case class TeXEyesException(line: Int, column: Int, expected: String) extends Exception
case class EOIException(line: Int, column: Int) extends Exception
