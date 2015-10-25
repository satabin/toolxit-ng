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

import dimen.Dimension

import scala.util.{
  Try,
  Success,
  Failure
}

import scala.annotation.tailrec

/** The macros and expansion mechanisms described in chapter 20 of the TeX Book are done here.
 *
 *  @author Lucas Satabin
 */
trait TeXMacros {
  this: TeXMouth =>

  /** Parses the definition of a new macro.
   *  A macro definition is something that looks like:
   *  {{{
   *  <macro def> ::= \def <control sequence> <parameter text> { <replacement text> }
   *  }}}
   */
  def parseMacroDef(long: Boolean, outer: Boolean, global: Boolean): Try[(Boolean, TeXMacro)] = {

    // a macro name is an unexpanded control sequence
    def parseName(): Try[String] =
      read().flatMap {
        case ControlSequenceToken(name, _) =>
          swallow()
          Success(name)
        case t =>
          Failure(new TeXMouthException(f"Macro name must be a control sequence or an active character", t.pos))
      }

    def parseParameters(): Try[(Boolean, List[Token])] = {
      @tailrec
      def loop(nextParam: Int, acc: List[Token]): Try[(Boolean, List[Token])] = read() match {
        case Success(CharacterToken(_, Category.PARAMETER)) =>
          // here comes a parameter, we expect the correct number afterward
          swallow()
          read() match {
            case Success(CharacterToken(int(i), _)) if i == nextParam =>
              // this is a correct parameter consume its number
              swallow()
              // and go forward
              loop(nextParam + 1, ParameterToken(i) :: acc)
            case Success(tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
              // this is the `#{` sequence at the end of the parameter list
              // a kind of special sequence which inserts a `{' at both the end of the parameter
              // list and the replacement text
              Success(true, (tok :: acc).reverse)
            case Success(tok) =>
              // wrong parameter number
              Failure(new TeXMouthException(f"Parameters must be numbered consecutively. Got $tok but expected $nextParam", tok.pos))
          }

        case Success(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
          // we reached the end of the parameter list return it
          Success(false, acc.reverse)

        case Success(tok @ ControlSequenceToken(name, _)) if env.css.isOuter(name) =>
          // macro declared as `outer' are not allowed in the parameter text
          Failure(new TeXMouthException(f"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

        case Success(token) =>
          // any other character is added to the current list of parameters
          swallow()
          loop(nextParam, token :: acc)

        case Failure(t) =>
          Failure(t)
      }
      loop(1, Nil)
    }

    def parseMacroDecl(global: Boolean): Try[(Boolean, Boolean, String, List[Token], Boolean)] = read() flatMap {
      case ControlSequenceToken("def", _) =>
        swallow()
        for {
          name <- parseName()
          (appendBrace, params) <- parseParameters()
        } yield (global, false, name, params, appendBrace)
      case ControlSequenceToken("gdef", _) =>
        swallow()
        for {
          name <- parseName()
          (appendBrace, params) <- parseParameters()
        } yield (true, false, name, params, appendBrace)
      case ControlSequenceToken("edef", _) =>
        swallow()
        for {
          name <- parseName()
          (appendBrace, params) <- parseParameters()
        } yield (false, true, name, params, appendBrace)
      case ControlSequenceToken("xdef", _) =>
        swallow()
        for {
          name <- parseName()
          (appendBrace, params) <- parseParameters()
        } yield (true, true, name, params, appendBrace)
      case t =>
        Failure(new TeXMouthException(f"Unexpected token when parsing macro declaration $t", t.pos))
    }

    def parseReplacement(appendBrace: Boolean): Try[List[Token]] =
      parseGroup(true, false).map {
        case GroupToken(_, tokens, _) =>
          if (appendBrace)
            CharacterToken('{', Category.BEGINNING_OF_GROUP) :: tokens
          else
            tokens
      }

    read() flatMap {
      case ControlSequenceToken("def" | "gdef" | "edef" | "xdef", _) =>
        for {
          // first comes the declaration (and tokens are not expanded during this phase)
          (global, expanded, name, params, appendBrace) <- withExpansion(false)(parseMacroDecl(global))
          // and the replacement text expanded only if needed
          replacement <- withExpansion(expanded)(parseReplacement(appendBrace))
        } yield (global, TeXMacro(name, params, replacement, long, outer))

      case t =>
        Failure(new TeXMouthException(f"Unexpected token when parsing macro declaration $t", t.pos))

    }

  }

  def parseIf(): Try[Token] =
    read() flatMap {
      case ControlSequenceToken("ifnum", _) =>
        swallow()
        for {
          n1 <- parseNumber()
          rel <- parseRel[Int]()
          n2 <- parseNumber()
          () <- parseIfBody(rel(n1, n2))
          t <- read()
        } yield t

      case ControlSequenceToken("ifdim", _) =>
        swallow()
        for {
          d1 <- parseDimen()
          rel <- parseRel[Dimension]()
          d2 <- parseDimen()
          () <- parseIfBody(rel(d1, d2))
          t <- read()
        } yield t

      case ControlSequenceToken("ifodd", _) =>
        swallow()
        for {
          n <- parseNumber()
          () <- parseIfBody(n % 2 == 1)
          t <- read()
        } yield t

      case ControlSequenceToken("ifvmode", _) =>
        swallow()
        for {
          () <- parseIfBody(env.mode == Mode.VerticalMode || env.mode == Mode.InternalVerticalMode)
          t <- read()
        } yield t

      case ControlSequenceToken("ifhmode", _) =>
        swallow()
        for {
          () <- parseIfBody(env.mode == Mode.HorizontalMode || env.mode == Mode.RestrictedHorizontalMode)
          t <- read()
        } yield t

      case ControlSequenceToken("ifmmode", _) =>
        swallow()
        for {
          () <- parseIfBody(env.mode == Mode.MathMode || env.mode == Mode.DisplayMathMode)
          t <- read()
        } yield t

      case ControlSequenceToken("ifinner", _) =>
        swallow()
        for {
          () <- parseIfBody(env.mode == Mode.InternalVerticalMode || env.mode == Mode.RestrictedHorizontalMode || env.mode == Mode.MathMode)
          t <- read()
        } yield t

      case ControlSequenceToken("ifcase", _) =>
        swallow()
        for {
          n <- parseNumber()
          () <- parseCaseBody(n)
          t <- read()
        } yield t

      case t =>
        Failure(new TeXMouthException("if construct expected", t.pos))
    }

  private def parseRel[T: Ordering](): Try[(T, T) => Boolean] = {
    val ordering = implicitly[Ordering[T]]
    read() flatMap {
      case CharacterToken('<', Category.OTHER_CHARACTER) =>
        swallow()
        Success(ordering.lt)
      case CharacterToken('>', Category.OTHER_CHARACTER) =>
        swallow()
        Success(ordering.gt)
      case CharacterToken('=', Category.OTHER_CHARACTER) =>
        swallow()
        Success(ordering.equiv)
      case t =>
        Failure(new TeXMouthException("Integer relation operator expected", t.pos))
    }
  }

  /* if the condition is `true`, we keep the (unexpanded) tokens until the next matching
   * `\else` or `\fi`. Otherwise, skip the first branch and keep the (unexpanded) tokens until the next matching `\fi`.
   * User defined ifs are taken into account when skipping over an if-branch to pair ifs and fis together. */
  private def parseIfBody(cond: Boolean): Try[Unit] = withExpansion(false) {
    @tailrec
    def parseThen(lvl: Int, acc: List[Token]): Try[List[Token]] =
      read() match {
        case Success(ControlSequenceToken("else" | "fi", _)) if lvl == 0 =>
          Success(acc)
        case Success(t @ ControlSequenceToken("fi", _)) =>
          swallow()
          parseThen(lvl - 1, t :: acc)
        case Success(If(t)) =>
          swallow()
          parseThen(lvl + 1, t :: acc)
        case Success(t) =>
          swallow()
          parseThen(lvl, t :: acc)
        case Failure(t) =>
          Failure(t)
      }
    @tailrec
    def parseElse(lvl: Int, acc: List[Token]): Try[List[Token]] =
      read() match {
        case Success(ControlSequenceToken("else", _)) if lvl == 0 =>
          // drop the else
          swallow()
          parseElse(lvl, acc)
        case Success(ControlSequenceToken("fi", _)) if lvl == 0 =>
          swallow()
          Success(acc)
        case Success(t @ ControlSequenceToken("fi", _)) =>
          swallow()
          parseElse(lvl - 1, t :: acc)
        case Success(If(t)) =>
          swallow()
          parseElse(lvl + 1, t :: acc)
        case Success(t) =>
          swallow()
          parseElse(lvl, t :: acc)
        case Failure(t) =>
          Failure(t)
      }

    for {
      t <- parseThen(0, Nil)
      e <- parseElse(0, Nil)
    } yield if(cond) {
      pushback(t)
    } else {
      pushback(e)
    }
  }

  private def parseCaseBody(n: Int): Try[Unit] = withExpansion(false) {
    import scala.collection.mutable.Builder
    @tailrec
    def parseCases(lvl: Int, acc: Builder[List[Token], Vector[List[Token]]], current: List[Token]): Try[Vector[List[Token]]] =
      read() match {
        case Success(ControlSequenceToken("else" | "fi", _)) if lvl == 0 =>
          current match {
            case Nil =>
              Success(acc.result)
            case _ =>
              Success((acc +=current).result)
          }
        case Success(t @ ControlSequenceToken("fi", _)) =>
          swallow()
          parseCases(lvl - 1, acc, t :: current)
        case Success(If(t)) =>
          swallow()
          parseCases(lvl + 1, acc, t :: current)
        case Success(ControlSequenceToken("or", _)) =>
          // drop the or
          swallow()
          parseCases(lvl, acc += current, Nil)
        case Success(t) =>
          swallow()
          parseCases(lvl, acc, t :: current)
        case Failure(t) =>
          Failure(t)
      }
    @tailrec
    def parseElse(lvl: Int, acc: List[Token]): Try[List[Token]] =
      read() match {
        case Success(ControlSequenceToken("else", _)) if lvl == 0 =>
          // drop the else
          swallow()
          parseElse(lvl, acc)
        case Success(ControlSequenceToken("fi", _)) if lvl == 0 =>
          swallow()
          Success(acc)
        case Success(t @ ControlSequenceToken("fi", _)) =>
          swallow()
          parseElse(lvl - 1, t :: acc)
        case Success(If(t)) =>
          swallow()
          parseElse(lvl + 1, t :: acc)
        case Success(t) =>
          swallow()
          parseElse(lvl, t :: acc)
        case Failure(t) =>
          Failure(t)
      }

    for {
      cs <- parseCases(0, Vector.newBuilder[List[Token]], Nil)
      e <- parseElse(0, Nil)
    } yield cs.applyOrElse(n, e)
  }

}
