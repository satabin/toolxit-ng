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

import scala.util.Try

import util._

/** The TeX eyes as defined in the ''TeX Book'' mainly in chapter 8 '''The Characters You Type'''.
 *  This is the lexer that generates a stream of TeX tokens out of a character stream.
 *  It uses as input a character stream that is line aware, allowing for better error messages,
 *  because the parsed lines is recognized as it is parsed and can be partially displayed in messages.
 *
 *  @author Lucas Satabin
 */
class TeXEyes(env: TeXEnvironment) extends Iteratees[(Char, Int, Int)] {

  type Processor[T] = Iteratee[(Char, Int, Int), T]

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
  val preprocessor: Processor[(Char, Int, Int)] =
    peek(4).flatMap {
      case List(
        (SUPERSCRIPT(sup1), l, c),
        (SUPERSCRIPT(sup2), _, _),
        (Hexa(h1), _, _),
        (Hexa(h2), _, _)) if sup1 == sup2 =>
        for (() <- swallow(4))
          yield (((h1 << 4) + h2).toChar, l, c)
      case List(
        (SUPERSCRIPT(sup1), l, c),
        (SUPERSCRIPT(sup2), _, _),
        (ch, _, _), _*) if sup1 == sup2 =>
        for (() <- swallow(3))
          yield (if (ch < 64) (ch + 64).toChar else (ch - 64).toChar, l, c)
      case ps @ List((ch, l, c), _*) =>
        for (() <- swallow)
          yield (ch, l, c)
      case Nil =>
        throwError(EOIException(env.lastPosition.line, env.lastPosition.column))
    }

  val tokenize: Processor[Token] =
    for {
      (ch, l, c) <- preprocessor
      t <- ch match {
        case IGNORED_CHARACTER(_) =>
          // the obvious case is to ignore currently ignored characters
          tokenize
        case SPACE(_) if state == ReadingState.S || state == ReadingState.N =>
          // when in reading state 'skipping blanks' or 'new line', spaces are ignored as well
          tokenize
        case COMMENT_CHARACTER(_) =>
          // when a comment started it lasts until the end of line is reached
          // the end of line character is then eaten as well
          for {
            next <- dropWhile {
              case (END_OF_LINE(_), _, _) => false
              case _                      => true
            }
            // eat the end of line character
            () <- next.fold(noop)(_ => swallow)
            t <- tokenize
          } yield t
        case ACTIVE_CHARACTER(ch) =>
          // an active character is control sequence, and after control sequence we go into
          // the 'skipping blanks' state
          state = ReadingState.S
          done(ControlSequenceToken(ch.toString, true).atPos(SimplePosition(l, c)))
        case ESCAPE_CHARACTER(_) =>
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
              } yield ControlSequenceToken(csname.map(_._1).mkString, false).atPos(SimplePosition(l, c))
            case Some((ch, _, _)) =>
              for (() <- swallow) yield {
                ControlSequenceToken(ch.toString, false).atPos(SimplePosition(l, c))
              }
            case None =>
              // we reached end of input, this is absolutely not correct
              throwError(new TeXEyesException(l, c, "control sequence name expected but end of input reached"))
          }
        case END_OF_LINE(_) if env.state == ReadingState.N =>
          // when reading end of line and if we are in the 'new line' reading state,
          // this is equivalent to the `\par` control sequence and stay in the same state
          done(ControlSequenceToken("par", false).atPos(SimplePosition(l, c)))
        case END_OF_LINE(_) if env.state == ReadingState.M =>
          // otherwise in any reading state 'middle of line', it is considered as a space and we go into
          // 'new line' reading state
          env.state = ReadingState.N
          done(CharacterToken(' ', Category.SPACE).atPos(SimplePosition(l, c)))
        case END_OF_LINE(_) =>
          // otherwise we are skipping blank characters, so just ignore it
          tokenize
        case SPACE(_) =>
          // if this is a space character, we go into the 'skipping blanks' reading state
          // the space token is always ' ' even if it were some other characters that
          // was assigned the SPACE category
          env.state = ReadingState.S
          done(CharacterToken(' ', Category.SPACE).atPos(SimplePosition(l, c)))
        case _ =>
          // otherwise, any other character is returned and we are now in the 'middle of line'
          // reading state
          env.state = ReadingState.M
          done(CharacterToken(ch, category(ch)).atPos(SimplePosition(l, c)))
      }
    } yield {
      env.lastPosition = t.pos
      t
    }

}

case class TeXEyesException(line: Int, column: Int, expected: String) extends Exception
