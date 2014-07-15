/*
* This file is part of the ToolXiT project.
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

import scala.annotation.tailrec

/** The TeX eyes as defined in the TeX Book mainly in chapter 8 '''The Characters You Type'''.
 *  This is the lexer that generates a stream of TeX tokens out of a character stream.
 *  It uses as input a character stream that is line aware, allowing for better error messages,
 *  because the parsed lines is revognized as it is parsed and can be partially displayed in messages.
 *
 *  @author Lucas Satabin
 */
object TeXEyes {

  def lookahead(state: ReadingState.Value, env: TeXEnvironment, stream: LineStream): Try[Token] =
    lookahead(1)(state, env, stream).map(_.head)

  def lookahead(n: Int)(state: ReadingState.Value, env: TeXEnvironment, stream: LineStream): Try[List[Token]] = {
    @tailrec
    def loop(n: Int, state: ReadingState.Value, stream: LineStream, acc: List[Token]): Try[List[Token]] =
      if(n == 0) {
        Success(acc.reverse)
      } else {
        next(state, env, stream) match {
          case Success((token, newState, newStream)) =>
            loop(n - 1, newState, newStream, token :: acc)
          case Failure(t) =>
            Failure(t)
        }
      }
    loop(n, state, stream, Nil)
  }

  /** Returns the next token to be read from this input */
  @tailrec
  def next(state: ReadingState.Value, env: TeXEnvironment, stream: LineStream): Try[(Token, ReadingState.Value, LineStream)] = {
    import env._

    object ControlSequence {
      def unapply(str: LineStream): Option[(String, LineStream)] = str match {
        case ESCAPE_CHARACTER(_) !:: LETTER(l) !:: rest =>
          // a control sequence starts with an escape character and is followed by letters
          val (csname: LineStream, rest1: LineStream) = rest.span {
            case LETTER(_) => true
            case _         => false
          }
          Some(csname.mkString(""), rest1)
        case ESCAPE_CHARACTER(_) !:: c !:: rest =>
          // or it starts with an escape character and is followed by one single other character
          Some(c.toString, rest)
        case _ =>
          None
      }
    }

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

    // we first preprocess the input stream to replace character code by their
    // value
    // a character code is of the form:
    //  - `^^XX` where X is one of `0123456789abcdef`
    //  - `^^A` where A is a letter (< 128)
    //  - any other character
    val stream1 = stream match {
      case SUPERSCRIPT(sup1) !:: SUPERSCRIPT(sup2) !:: Hexa(h1) !:: Hexa(h2) !:: rest if sup1 == sup2 =>
        ((h1 << 4) + h2).toChar !:: rest
      case SUPERSCRIPT(sup1) !:: SUPERSCRIPT(sup2) !:: c !:: rest if c < 128 && sup1 == sup2 =>
        val char =
          if (c < 64)
            (c + 64).toChar
          else
            (c - 64).toChar
        char !:: rest
      case _ =>
        stream
    }

    stream1 match {
      case IGNORED_CHARACTER(_) !:: rest =>
        // the obvious case is to ignore currently ignored characters
        next(state, env, rest)
      case SPACE(_) !:: rest if state == ReadingState.S || state == ReadingState.N =>
        // when in reading state 'skipping blanks' or 'new line', spaces are ignored as well
        next(state, env, rest)
      case COMMENT_CHARACTER(_) !:: rest =>
        // when a comment started it lasts until the end of line is reached
        // the end of line character is then eaten as well
        next(state, env, rest.dropWhile {
          case END_OF_LINE(_) => false
          case _              => true
        }.tail)
      case ACTIVE_CHARACTER(c) !:: rest =>
        // an active character is control sequence, and after control sequence we go into
        // the 'skipping blanks' state
        Try(ControlSequenceToken(c.toString, true).atPos(stream.lineNum, stream.colNum), ReadingState.S, rest)
      case ControlSequence(cs, rest) =>
        // after control sequence we go into the 'skipping blanks' state
        Try(ControlSequenceToken(cs, false).atPos(stream.lineNum, stream.colNum), ReadingState.S, rest)
      case ESCAPE_CHARACTER(_) !:: LineNil =>
        // we reached end of input, this is absolutely not correct
        Failure(new TeXEyesException(stream.lineNum, stream.colNum, "control sequence name expected but end of input reached"))
      case END_OF_LINE(_) !:: rest if state == ReadingState.N =>
        // when reading end of line and if we are in the 'new line' reading state,
        // this is equivalent to the `\par` control sequence and stay in the same state
        Try(ControlSequenceToken("par", false).atPos(stream.lineNum, stream.colNum), state, rest)
      case END_OF_LINE(_) !:: rest if state == ReadingState.M =>
        // otherwise in any reading state 'middle of line', it is considered as a space and we go into
        // 'new line' reading state
        Try(CharacterToken(' ', Category.SPACE).atPos(stream.lineNum, stream.colNum), ReadingState.N, rest)
      case END_OF_LINE(_) !:: rest =>
        // otherwise we are skipping blank characters, so just ignore it
        next(state, env, rest.tail)
      case SPACE(_) !:: rest =>
        // if this is a space character, we go into the 'skipping blanks' reading state
        // the space token is always ' ' even if it were some other characters that
        // was assigned the SPACE category
        Try(CharacterToken(' ', Category.SPACE).atPos(stream.lineNum, stream.colNum), ReadingState.S, rest)
      case c !:: rest =>
        // otherwise, any other character is returned and we are now in the 'middle of line'
        // reading state
        Try(CharacterToken(c, category(c)).atPos(stream.lineNum, stream.colNum), ReadingState.M, rest)
      case LineNil =>
        // we read an unexpected character at this point
        Failure(EOIException(stream.lineNum, stream.colNum))
    }
  }

}

case class TeXEyesException(line: Int, column: Int, expected: String) extends Exception
case class EOIException(line: Int, column: Int) extends Exception
