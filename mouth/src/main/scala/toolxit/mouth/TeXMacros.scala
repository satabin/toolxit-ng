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

import util.Position

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

  /** Parses the arguments of the given TeX macro.
   *  The parser parses according to the parameter text
   *  that was parsed during the macro definition.
   *  If the macro is declared as `long`, then `\par` is allowed
   *  to appear as argument. In any cases, `outer` macros are not allowed
   *  to appear in the arguments.
   *  Arguments are returned in order, but the tokens of each argument are returned in
   *  reverse order.
   */
  private def parseArguments(long: Boolean, parameters: List[Token]): Try[List[List[Token]]] = {
    @tailrec
    def loop(parameters: List[Token], stop: Option[Token], localAcc: List[Token], acc: List[List[Token]]): Try[List[List[Token]]] = stop match {
      case Some(stop) =>
        // we must read until the stop token has been reached
        // if the macro was declared as long, then \par is allowed.
        raw() match {
          case Success(tok @ ControlSequenceToken(name, _)) if env.css.isOuter(name) =>
            Failure(new TeXMouthException("Outer macros are not authorized in macro parameterf", tok.pos))
          case Success(tok @ ControlSequenceToken("par", _)) if !long =>
            Failure(new TeXMouthException("Runaway argument. new paragraph is not allowed in the parameter list", tok.pos))
          case Success(tok) if tok == stop =>
            loop(parameters, None, Nil, localAcc :: acc)
          case Success(tok) =>
            loop(parameters, Some(stop), tok :: localAcc, acc)
          case Failure(t) =>
            Failure(t)
        }
      case None =>
        parameters match {
          case Nil =>
            Success(acc.reverse)
          case ParameterToken(_) :: rest =>
            rest match {
              case Nil | ParameterToken(_) :: _ =>
                // an undelimited parameter
                raw() match {
                  case Success(t) =>
                    loop(rest, None, Nil, List(t) :: acc)
                  case Failure(t) =>
                    Failure(t)
                }
              case token :: rest =>
                loop(rest, Some(token), Nil, acc)
            }
          case (token @ (ControlSequenceToken(_, _) | CharacterToken(_, _))) :: rest =>
            // this is a delimiter token, accept it and go forward
            eat(token) match {
              case Success(()) =>
                loop(rest, None, Nil, acc)
              case Failure(t) =>
                Failure(t)
            }
          case (token @ GroupToken(_, _, _)) :: _ =>
            Failure(new TeXMouthException("Groups are not allowed in macro parameter listf", token.pos))
        }

    }
    loop(parameters, None, Nil, Nil)
  }

  final def expandCs(cs: ControlSequence, pos: Position): Try[Token] =
    cs match {
      case TeXMacro(_, parameters, replacement, long, outer) =>
        // consume the macro name name
        swallow()
        // parse the arguments
        parseArguments(long, parameters) flatMap { args =>
          // we then push back the replacement text onto the token stack
          // replacing as it goes the parameters by the parsed ones.
          replacement.foreach {
            case ParameterToken(i) =>
              // by construction (with the parser) the parameter exists
              val arg =
                if(env.debugPositions)
                  args(i).map(a => a.atPos(pos, Some(a.pos)))
                else
                  args(i)
              pushback(arg)
            case token =>
              pushback(token)
          }
          // and read again
          read()

        }
      case _ =>
        ???
    }

  def expandIf(): Try[Token] =
    raw() flatMap {
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

  def expandInput(): Try[Token] =
    raw() match {
      case Success(ControlSequenceToken("input", _)) =>
        // read the filename which consists in all the consecutive non space character tokens following the input macro
        swallow()
        @tailrec
        def loop(acc: StringBuilder): Try[String] =
          read() match {
            case Success(CharacterToken(_, Category.SPACE) | ControlSequenceToken(_, _)) if acc.nonEmpty =>
              val acc1 =
                if(acc.endsWith(".tex"))
                  acc
                else
                  acc.append(".tex")
              Success(acc1.toString)
            case Success(CharacterToken(c, _)) =>
              loop(acc.append(c))
            case Success(t) =>
              Failure(new TeXMouthException("Missing input file name", t.pos))
            case Failure(t) =>
              Failure(t)
          }
        for {
          name <- loop(new StringBuilder)
          () <- openInput(name)
          t <- read()
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\input command", t.pos))
      case t =>
        t
    }

  private val decimals = Vector(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
  private val romans = Vector(
    List(CharacterToken('m', Category.OTHER_CHARACTER)),
    List(CharacterToken('m', Category.OTHER_CHARACTER), CharacterToken('c', Category.OTHER_CHARACTER)),
    List(CharacterToken('d', Category.OTHER_CHARACTER)),
    List(CharacterToken('d', Category.OTHER_CHARACTER), CharacterToken('c', Category.OTHER_CHARACTER)),
    List(CharacterToken('c', Category.OTHER_CHARACTER)),
    List(CharacterToken('c', Category.OTHER_CHARACTER), CharacterToken('x', Category.OTHER_CHARACTER)),
    List(CharacterToken('l', Category.OTHER_CHARACTER)),
    List(CharacterToken('l', Category.OTHER_CHARACTER), CharacterToken('x', Category.OTHER_CHARACTER)),
    List(CharacterToken('x', Category.OTHER_CHARACTER)),
    List(CharacterToken('x', Category.OTHER_CHARACTER), CharacterToken('i', Category.OTHER_CHARACTER)),
    List(CharacterToken('v', Category.OTHER_CHARACTER)),
    List(CharacterToken('v', Category.OTHER_CHARACTER), CharacterToken('i', Category.OTHER_CHARACTER)),
    List(CharacterToken('i', Category.OTHER_CHARACTER)))

  assert(decimals.size == romans.size)

  // the result is in reverse order, so that it can be pushed back directly on the token stack
  @tailrec
  private def toRoman(i: Int, idx: Int, acc: List[Token]): List[Token] =
    if(i <= 0) {
      acc
    } else if(i < decimals(idx)) {
      toRoman(i, idx + 1, acc)
    } else {
      toRoman(i - decimals(idx), 0, romans(idx) ++ acc)
    }

  def expandRomannumeral(): Try[Token] =
    raw() match {
      case Success(ControlSequenceToken("romannumeral", _)) =>
        swallow()
        for {
          n <- parseNumber()
          tokens = toRoman(n, 0, Nil)
          () = pushback(tokens)
          t <- read()
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\romannumeral command", t.pos))
      case f =>
        f
    }

  // the result is in reverse order, so that it can be pushed back directly on the token stack
  private def toTokens(negative: Boolean, n: Int): List[Token] =
    if(n < 10) {
      if(negative) {
        List(CharacterToken(('0' + n).toChar, Category.OTHER_CHARACTER), CharacterToken('-', Category.OTHER_CHARACTER))
      } else {
        List(CharacterToken(('0' + n).toChar, Category.OTHER_CHARACTER))
      }
    } else {
      CharacterToken(('0' + (n % 10)).toChar, Category.OTHER_CHARACTER) :: toTokens(negative, n / 10)
    }

  // the result is in reverse order, so that it can be pushed back directly on the token stack
  private def toTokens(s: String): List[Token] =
    s.foldLeft(List.empty[Token]) { (acc, c) =>
      if(c == ' ') {
        CharacterToken(c, Category.SPACE) :: acc
      } else {
        CharacterToken(c, Category.OTHER_CHARACTER) :: acc
      }
    }

  def expandNumber(): Try[Token] =
    raw() match {
      case Success(ControlSequenceToken("number", _)) =>
        swallow()
        for {
          n <- parseNumber()
          tokens = toTokens(n < 0, math.abs(n))
          () = pushback(tokens)
          t <- read()
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\number command", t.pos))
      case f =>
        f
    }

  def expandString(): Try[Token] =
    raw() match {
      case Success(ControlSequenceToken("string", _)) =>
        swallow()
        raw() match {
          case Success(ControlSequenceToken(n, true)) =>
            assert(n.size == 1)
            swallow()
            val c = n(0)
            pushback(CharacterToken(c, if(c == ' ') Category.SPACE else Category.OTHER_CHARACTER))
            read()
          case Success(ControlSequenceToken(n, false)) =>
            swallow()
            for(c <- n.reverse)
              pushback(CharacterToken(c, if(c == ' ') Category.SPACE else Category.OTHER_CHARACTER))
            pushback(CharacterToken(env.escapechar, Category.OTHER_CHARACTER))
            read()
          case Success(CharacterToken(c, _)) =>
            pushback(CharacterToken(c, if(c == ' ') Category.SPACE else Category.OTHER_CHARACTER))
            read()
          case Success(t) =>
            Failure(new TeXMouthException("expected token", t.pos))
          case f =>
            f
        }
      case Success(t) =>
        Failure(new TeXMouthException("expected \\number command", t.pos))
      case f =>
        f
    }

  private def meaning(t: Token) = t match {
    case CharacterToken(c, Category.ESCAPE_CHARACTER) =>
      toTokens(f"escape character $c")
    case CharacterToken(c, Category.BEGINNING_OF_GROUP) =>
      toTokens(f"begin-group character $c")
    case CharacterToken(c, Category.END_OF_GROUP) =>
      toTokens(f"end-group character $c")
    case CharacterToken(c, Category.MATH_SHIFT) =>
      toTokens(f"math shift character $c")
    case CharacterToken(c, Category.ALIGNMENT_TAB) =>
      toTokens(f"alignment tab character $c")
    case CharacterToken(c, Category.END_OF_LINE) =>
      toTokens(f"end-of-line character $c")
    case CharacterToken(c, Category.PARAMETER) =>
      toTokens(f"macro parameter character $c")
    case CharacterToken(c, Category.SUPERSCRIPT) =>
      toTokens(f"superscript character $c")
    case CharacterToken(c, Category.SUBSCRIPT) =>
      toTokens(f"subscript character $c")
    case CharacterToken(c, Category.IGNORED_CHARACTER) =>
      toTokens(f"ignored character character $c")
    case CharacterToken(c, Category.SPACE) =>
      toTokens(f"space character character $c")
    case CharacterToken(c, Category.LETTER) =>
      toTokens(f"the letter $c")
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      toTokens(f"the character $c")
    case CharacterToken(c, Category.ACTIVE_CHARACTER) =>
      toTokens(f"active character $c")
    case CharacterToken(c, Category.COMMENT_CHARACTER) =>
      toTokens(f"comment character $c")
    case CharacterToken(c, Category.INVALID_CHARACTER) =>
      toTokens(f"invalid character $c")
    case ControlSequenceToken(n, _) =>
      env.css(n) match {
        case Some(TeXMacro(name, parameters, replacement, _, _)) =>
          toTokens(f"macro:${parameters.map(_.toString(env)).mkString}->${replacement.reverseMap(_.toString(env)).mkString}")
        case Some(_) =>
          toTokens(f"${env.escapechar}$n")
        case None =>
          toTokens("undefined")
      }
    case _ =>
      throw new TeXInternalException("this case should never occur.")
  }

  def expandMeaning(): Try[Token] =
    raw() match {
      case Success(ControlSequenceToken("meaning", _)) =>
        swallow()
        for {
          token <- raw()
          () = swallow()
          () = pushback(meaning(token))
          t <- read()
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\meaning command", t.pos))
      case f =>
        f
    }

  def expandCsname(): Try[Token] =
    raw() match {
      case Success(start @ ControlSequenceToken("csname", _)) =>
        swallow()
        @tailrec
        def loop(acc: List[Token]): Try[List[Token]] =
          read() match {
            case Success(ControlSequenceToken("endcsname", _)) =>
              swallow()
              Success(acc)
            case Success(c @ CharacterToken(_, _)) =>
              swallow()
              loop(c :: acc)
            case Success(t) =>
              Failure(new TeXMouthException("character tokens only are expected inside \\csname..\\endcsname", t.pos))
            case Failure(f) =>
              Failure(f)
          }
        for {
          name <- loop(Nil)
          t <- env.css(name.map(_.toString(env)).mkString) match {
            case Some(cs @ TeXMacro(_, _, _, _, _)) => expandCs(cs, start.pos)
            case _ => read() // fi not found it does nothing, just go ahead
          }
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\csname command", t.pos))
      case f =>
        f
    }

  def expandafter(): Try[Token] =
    raw() match {
      case Success(start @ ControlSequenceToken("expandafter", _)) =>
        swallow()
        for {
          first <- raw()
          () = swallow()
          _ <- read()
          () = pushback(first)
          t <- read()
        } yield t
      case Success(t) =>
        Failure(new TeXMouthException("expected \\expandafter command", t.pos))
      case f =>
        f
    }

}
