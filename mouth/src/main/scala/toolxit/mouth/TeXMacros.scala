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

import dimen.Dimension

import util._

import java.io.File

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
  def macroDef(long: Boolean, outer: Boolean, global: Boolean): Processor[(Boolean, TeXMacro)] = {

    val read = onEOI("End of input reached while parsing macro parameters")(this.read)
    val raw = onEOI("End of input reached while parsing macro parameters")(this.raw)

    // a macro name is an unexpanded control sequence
    val name: Processor[String] =
      raw.flatMap {
        case ControlSequenceToken(name, _) =>
          for (() <- swallow)
            yield name
        case t =>
          throwError(new TeXMouthException(f"Macro name must be a control sequence or an active character but got $t", t.pos))
      }

    val parameters: Processor[(Boolean, List[Token])] = {
      def loop(nextParam: Int, acc: List[Token]): Processor[(Boolean, List[Token])] = raw.flatMap {
        case CharacterToken(_, Category.PARAMETER) =>
          for {
            // here comes a parameter, we expect the correct number afterward
            () <- swallow
            res <- raw.flatMap {
              case CharacterToken(int(i), _) if i == nextParam =>
                for {
                  // this is a correct parameter consume its number
                  () <- swallow
                  // and go forward
                  res <- loop(nextParam + 1, ParameterToken(i) :: acc)
                } yield res
              case tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
                // this is the `#{` sequence at the end of the parameter list
                // a kind of special sequence which inserts a `{' at both the end of the parameter
                // list and the replacement text
                done((true, (tok :: acc).reverse))
              case tok =>
                // wrong parameter number
                throwError(new TeXMouthException(f"Parameters must be numbered consecutively. Got $tok but expected $nextParam", tok.pos))
            }
          } yield res

        case CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
          // we reached the end of the parameter list return it
          done((false, acc.reverse))

        case tok @ ControlSequenceToken(name, _) if env.css.isOuter(name) =>
          // macro declared as `outer' are not allowed in the parameter text
          throwError(new TeXMouthException(f"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

        case token =>
          for {
            // any other character is added to the current list of parameters
            () <- swallow
            res <- loop(nextParam, token :: acc)
          } yield res

      }
      loop(1, Nil)
    }

    def macroDecl(global: Boolean): Processor[(Boolean, Boolean, String, List[Token], Boolean)] = read.flatMap {
      case ControlSequenceToken("def", _) =>
        for {
          () <- swallow
          name <- name
          (appendBrace, params) <- parameters
        } yield (global, false, name, params, appendBrace)
      case ControlSequenceToken("gdef", _) =>
        for {
          () <- swallow
          name <- name
          (appendBrace, params) <- parameters
        } yield (true, false, name, params, appendBrace)
      case ControlSequenceToken("edef", _) =>
        for {
          () <- swallow
          name <- name
          (appendBrace, params) <- parameters
        } yield (global, true, name, params, appendBrace)
      case ControlSequenceToken("xdef", _) =>
        for {
          () <- swallow
          name <- name
          (appendBrace, params) <- parameters
        } yield (true, true, name, params, appendBrace)
      case t =>
        throwError(new TeXMouthException(f"Macro definition expected", t.pos))
    }

    def replacement(expanded: Boolean, appendBrace: Boolean): Processor[List[Token]] = {
      val oldexp = env.expanding
      val oldrepl = env.inReplacement
      env.expanding = expanded
      env.inReplacement = true
      group(true, false, true, true, false).map {
        case GroupToken(_, tokens, _) =>
          if (appendBrace)
            CharacterToken('{', Category.BEGINNING_OF_GROUP) :: tokens
          else
            tokens
      } map { tokens =>
        env.expanding = oldexp
        env.inReplacement = oldrepl
        tokens
      } recoverWith { e =>
        env.expanding = oldexp
        env.inReplacement = oldrepl
        throwError(e)
      }
    }

    read.flatMap {
      case ControlSequenceToken("def" | "gdef" | "edef" | "xdef", _) =>
        for {
          // first comes the declaration (and tokens are not expanded during this phase)
          (global, expanded, name, params, appendBrace) <- macroDecl(global)
          // and the replacement text expanded only if needed
          replacement <- replacement(expanded, appendBrace)
        } yield (global, TeXMacro(name, params, replacement, long, outer))

      case t =>
        throwError(new TeXMouthException(f"Unexpected token when parsing macro declaration $t", t.pos))

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
  private def arguments(long: Boolean, parameters: List[Token]): Processor[List[List[Token]]] = {
    val read = onEOI("End of input is not allowed in macro parameter list")(this.read)
    val raw = onEOI("End of input is not allowed in macro parameter list")(this.raw)
    def loop(parameters: List[Token], stop: Option[Token], localAcc: List[Token], acc: List[List[Token]]): Processor[List[List[Token]]] = stop match {
      case Some(stop) =>
        // we must read until the stop token has been reached
        // if the macro was declared as long, then \par is allowed.
        read.flatMap {
          case tok @ ControlSequenceToken(name, _) if env.css.isOuter(name) =>
            throwError(new TeXMouthException("Outer macros are not authorized in macro parameter", tok.pos))
          case tok @ ControlSequenceToken("par", _) if !long =>
            throwError(new TeXMouthException("Runaway argument. new paragraph is not allowed in the parameter list", tok.pos))
          case CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
            for {
              g <- group(true, long, false, false, false)
              p <- loop(parameters, Some(stop), g :: localAcc, acc)
            } yield p
          case tok if tok == stop =>
            for {
              () <- swallow
              p <- loop(parameters, None, Nil, localAcc :: acc)
            } yield p
          case tok =>
            for {
              () <- swallow
              p <- loop(parameters, Some(stop), tok :: localAcc, acc)
            } yield p
        }
      case None =>
        parameters match {
          case Nil =>
            done(acc.reverse)
          case ParameterToken(_) :: rest =>
            rest match {
              case Nil | ParameterToken(_) :: _ =>
                for {
                  // an undelimited parameter
                  t <- read.flatMap {
                    case CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
                      for (GroupToken(_, tokens, _) <- group(true, long, false, false, false))
                        yield tokens
                    case tok @ ControlSequenceToken(name, _) if env.css.isOuter(name) =>
                      throwError(new TeXMouthException("Outer macros are not authorized in macro parameter", tok.pos))
                    case tok @ ControlSequenceToken("par", _) if !long =>
                      throwError(new TeXMouthException("Runaway argument. new paragraph is not allowed in the parameter list", tok.pos))
                    case tok => swallow.andThen(done(List(tok)))
                  }
                  res <- loop(rest, None, Nil, t :: acc)
                } yield res
              case token :: rest =>
                for {
                  p <- loop(rest, Some(token), Nil, acc)
                } yield p
            }
          case (token @ (ControlSequenceToken(_, _) | CharacterToken(_, _))) :: rest =>
            for {
              // this is a delimiter token, accept it and go forward
              () <- accept(token)
              res <- loop(rest, None, Nil, acc)
            } yield res
          case token :: _ =>
            throwError(new TeXMouthException(f"Unexpected $token in macro parameter list", token.pos))
        }

    }
    loop(parameters, None, Nil, Nil)
  }

  final def expandMacro(parameters: List[Token], replacement: List[Token], long: Boolean, outer: Boolean, pos: Position): Processor[Token] =
    for {
      // consume the macro name
      () <- swallow
      // parse the arguments
      args <- withExpansion(false)(arguments(long, parameters))
      // we then push back the replacement text onto the token stack
      // replacing as it goes the parameters by the parsed ones.
      args1 = replacement.flatMap {
        case ParameterToken(i) =>
          // by construction (with the parser) the parameter exists
          if (env.debugPositions)
            args(i - 1).flatMap {
              case GroupToken(_, tokens, _) =>
                tokens.map(a => a.atPos(pos, Some(a.pos)))
              case a =>
                List(a.atPos(pos, Some(a.pos)))
            }
          else
            args(i - 1)
        case token =>
          List(token)
      }
      () <- pushback(args1)
      // and read again
      t <- read
    } yield t

  val expandIf: Processor[Token] = {
    val read = onEOI("End of input reached when parsing if")(this.read)
    val raw = onEOI("End of input reached when parsing if")(this.raw)

    def charcode(t: Token): Char = t match {
      case CharacterToken(c, _) => c
      case ControlSequenceToken(name, _) => env.css(name) match {
        case Some(TeXCsAlias(_, CharacterToken(c, _))) => c
        case _                                         => 255.toChar
      }
      case _ => 255.toChar
    }

    def catcode(t: Token): Int = t match {
      case CharacterToken(_, c) => c.value
      case ControlSequenceToken(name, _) => env.css(name) match {
        case Some(TeXCsAlias(_, CharacterToken(_, c))) => c.value
        case _                                         => 16.toChar
      }
      case _ => 16.toChar
    }

    def agree(t1: Token, t2: Token): Boolean = (t1, t2) match {
      case (CharacterToken(_, _), CharacterToken(_, _)) => t1 == t2
      case (Primitive(_), Primitive(_)) => t1 == t2
      case (ControlSequenceToken(cs1, _), ControlSequenceToken(cs2, _)) => env.css(cs1) == env.css(cs2)
      case _ => false
    }

    raw.flatMap {
      case ControlSequenceToken("ifnum", _) =>
        for {
          () <- swallow
          n1 <- number
          rel <- rel[Int]()
          n2 <- number
          () <- ifBody(rel(n1, n2))
          t <- read
        } yield t

      case ControlSequenceToken("ifdim", _) =>
        for {
          () <- swallow
          d1 <- dimen
          rel <- rel[Dimension]()
          d2 <- dimen
          () <- ifBody(rel(d1, d2))
          t <- read
        } yield t

      case ControlSequenceToken("ifodd", _) =>
        for {
          () <- swallow
          n <- number
          () <- ifBody(n % 2 == 1)
          t <- read
        } yield t

      case ControlSequenceToken("ifvmode", _) =>
        for {
          () <- swallow
          () <- ifBody(env.mode == Mode.VerticalMode || env.mode == Mode.InternalVerticalMode)
          t <- read
        } yield t

      case ControlSequenceToken("ifhmode", _) =>
        for {
          () <- swallow
          () <- ifBody(env.mode == Mode.HorizontalMode || env.mode == Mode.RestrictedHorizontalMode)
          t <- read
        } yield t

      case ControlSequenceToken("ifmmode", _) =>
        for {
          () <- swallow
          () <- ifBody(env.mode == Mode.MathMode || env.mode == Mode.DisplayMathMode)
          t <- read
        } yield t

      case ControlSequenceToken("ifinner", _) =>
        for {
          () <- swallow
          () <- ifBody(env.mode == Mode.InternalVerticalMode || env.mode == Mode.RestrictedHorizontalMode || env.mode == Mode.MathMode)
          t <- read
        } yield t

      case ControlSequenceToken("if", _) =>
        for {
          () <- swallow
          t1 <- read
          () <- swallow
          t2 <- read
          () <- swallow
          () <- ifBody(charcode(t1) == charcode(t2))
          t <- read
        } yield t

      case ControlSequenceToken("ifcat", _) =>
        for {
          () <- swallow
          t1 <- read
          () <- swallow
          t2 <- read
          () <- swallow
          () <- ifBody(catcode(t1) == catcode(t2))
          t <- read
        } yield t

      case ControlSequenceToken("ifx", _) =>
        for {
          () <- swallow
          t1 <- raw
          () <- swallow
          t2 <- raw
          () <- swallow
          () <- ifBody(agree(t1, t2))
          t <- read
        } yield t

      case ControlSequenceToken("iftrue", _) =>
        for {
          () <- swallow
          () <- ifBody(true)
          t <- read
        } yield t

      case ControlSequenceToken("iffalse", _) =>
        for {
          () <- swallow
          () <- ifBody(true)
          t <- read
        } yield t

      case ControlSequenceToken("ifcase", _) =>
        for {
          () <- swallow
          n <- number
          () <- caseBody(n)
          t <- read
        } yield t

      case t =>
        throwError(new TeXMouthException("if construct expected", t.pos))
    }
  }

  private def rel[T: Ordering](): Processor[(T, T) => Boolean] = {
    val ordering = implicitly[Ordering[T]]
    read.flatMap {
      case CharacterToken('<', Category.OTHER_CHARACTER) =>
        for (() <- swallow)
          yield ordering.lt
      case CharacterToken('>', Category.OTHER_CHARACTER) =>
        for (() <- swallow)
          yield ordering.gt
      case CharacterToken('=', Category.OTHER_CHARACTER) =>
        for (() <- swallow)
          yield ordering.equiv
      case t =>
        throwError(new TeXMouthException("Integer relation operator expected", t.pos))
    }
  }

  /* if the condition is `true`, we keep the (unexpanded) tokens until the next matching
   * `\else` or `\fi`. Otherwise, skip the first branch and keep the (unexpanded) tokens until the next matching `\fi`.
   * User defined ifs are taken into account when skipping over an if-branch to pair ifs and fis together. */
  private def ifBody(cond: Boolean): Processor[Unit] = {
    val read = onEOI("End of input reached when parsing if body")(this.read)
    val raw = onEOI("End of input reached when parsing if body")(this.raw)
    def then_(lvl: Int, acc: List[Token]): Processor[List[Token]] =
      raw.flatMap {
        case ControlSequenceToken("else" | "fi", _) if lvl == 0 =>
          done(acc)
        case t @ ControlSequenceToken("fi", _) =>
          for {
            () <- swallow
            t <- then_(lvl - 1, t :: acc)
          } yield t
        case If(t) =>
          for {
            () <- swallow
            t <- then_(lvl + 1, t :: acc)
          } yield t
        case t =>
          for {
            () <- swallow
            t <- then_(lvl, t :: acc)
          } yield t
      }
    def else_(lvl: Int, acc: List[Token]): Processor[List[Token]] =
      raw.flatMap {
        case ControlSequenceToken("else", _) if lvl == 0 =>
          for {
            // drop the else
            () <- swallow
            e <- else_(lvl, acc)
          } yield e
        case ControlSequenceToken("fi", _) if lvl == 0 =>
          for (() <- swallow)
            yield acc
        case t @ ControlSequenceToken("fi", _) =>
          for {
            () <- swallow
            e <- else_(lvl - 1, t :: acc)
          } yield e
        case If(t) =>
          for {
            () <- swallow
            e <- else_(lvl + 1, t :: acc)
          } yield e
        case t =>
          for {
            () <- swallow
            e <- else_(lvl, t :: acc)
          } yield e
      }

    for {
      t <- then_(0, Nil)
      e <- else_(0, Nil)
      () <- if (cond) pushback(t) else pushback(e)
    } yield ()
  }

  private def caseBody(n: Int): Processor[Unit] = {
    val read = onEOI("End of input reached when parsing if body")(this.read)
    val raw = onEOI("End of input reached when parsing if body")(this.raw)
    import scala.collection.mutable.Builder
    def cases(lvl: Int, acc: Builder[List[Token], Vector[List[Token]]], current: List[Token]): Processor[Vector[List[Token]]] =
      raw.flatMap {
        case ControlSequenceToken("else" | "fi", _) if lvl == 0 =>
          current match {
            case Nil =>
              done(acc.result)
            case _ =>
              done((acc += current).result)
          }
        case t @ ControlSequenceToken("fi", _) =>
          for {
            () <- swallow
            c <- cases(lvl - 1, acc, t :: current)
          } yield c
        case If(t) =>
          for {
            () <- swallow
            c <- cases(lvl + 1, acc, t :: current)
          } yield c
        case ControlSequenceToken("or", _) =>
          for {
            // drop the or
            () <- swallow
            c <- cases(lvl, acc += current, Nil)
          } yield c
        case t =>
          for {
            () <- swallow
            c <- cases(lvl, acc, t :: current)
          } yield c
      }
    def else_(lvl: Int, acc: List[Token]): Processor[List[Token]] =
      raw.flatMap {
        case ControlSequenceToken("else", _) if lvl == 0 =>
          for {
            // drop the else
            () <- swallow
            e <- else_(lvl, acc)
          } yield e
        case ControlSequenceToken("fi", _) if lvl == 0 =>
          for (() <- swallow)
            yield acc
        case t @ ControlSequenceToken("fi", _) =>
          for {
            () <- swallow
            e <- else_(lvl - 1, t :: acc)
          } yield e
        case If(t) =>
          for {
            () <- swallow
            e <- else_(lvl + 1, t :: acc)
          } yield e
        case t =>
          for {
            () <- swallow
            e <- else_(lvl, t :: acc)
          } yield e
      }

    for {
      cs <- cases(0, Vector.newBuilder[List[Token]], Nil)
      e <- else_(0, Nil)
      () <- pushback(cs.applyOrElse(n, (_: Int) => e))
    } yield ()
  }

  def expandInput: Processor[Token] =
    raw.flatMap {
      case ControlSequenceToken("input", _) =>
        for {
          // read the filename which consists in all the consecutive non space character tokens following the input macro
          () <- swallow
          t <- read
          name <- filename(new StringBuilder)
          rname <- {
            val rname =
              if (name.endsWith(".tex"))
                name
              else
                name + ".tex"
            val f = new File(rname)
            if (f.exists) {
              done(rname)
            } else {
              throwError[Token](new TeXMouthException(f"I can't find file `$name'.", t.pos))
            }
          }
          () <- openInput(rname)
          t <- read
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\input command", t.pos))
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
  private def toRoman(i: Int, idx: Int, acc: List[Token]): List[Token] =
    if (i <= 0) {
      acc
    } else if (i < decimals(idx)) {
      toRoman(i, idx + 1, acc)
    } else {
      toRoman(i - decimals(idx), 0, romans(idx) ++ acc)
    }

  def expandRomannumeral: Processor[Token] =
    raw.flatMap {
      case ControlSequenceToken("romannumeral", _) =>
        for {
          () <- swallow
          n <- number
          tokens = toRoman(n, 0, Nil)
          () <- pushback(tokens)
          t <- read
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\romannumeral command", t.pos))
    }

  // the result is in reverse order, so that it can be pushed back directly on the token stack
  private def toTokens(negative: Boolean, n: Int): List[Token] =
    if (n < 10) {
      if (negative) {
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
      if (c == ' ') {
        CharacterToken(c, Category.SPACE) :: acc
      } else {
        CharacterToken(c, Category.OTHER_CHARACTER) :: acc
      }
    }

  def expandNumber: Processor[Token] =
    raw.flatMap {
      case ControlSequenceToken("number", _) =>
        for {
          () <- swallow
          n <- number
          tokens = toTokens(n < 0, math.abs(n))
          () <- pushback(tokens)
          t <- read
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\number command", t.pos))
    }

  def expandString: Processor[Token] =
    raw.flatMap {
      case ControlSequenceToken("string", _) =>
        def expand(t: Token): Processor[Token] = t match {
          case ControlSequenceToken(n, true) =>
            assert(n.size == 1)
            for {
              () <- swallow
              c = n(0)
              () <- pushback(CharacterToken(c, if (c == ' ') Category.SPACE else Category.OTHER_CHARACTER))
              t <- read
            } yield t
          case ControlSequenceToken(n, false) =>
            for {
              () <- swallow
              n1 = n.reverseMap(c => CharacterToken(c, if (c == ' ') Category.SPACE else Category.OTHER_CHARACTER)).toList
              () <- pushback(n1)
              () <- pushback(CharacterToken(env.escapechar, Category.OTHER_CHARACTER))
              t <- read
            } yield t
          case CharacterToken(c, _) =>
            for {
              () <- swallow
              () <- pushback(CharacterToken(c, if (c == ' ') Category.SPACE else Category.OTHER_CHARACTER))
              t <- read
            } yield t
          case t =>
            throwError(new TeXMouthException("expected token", t.pos))
        }
        for {
          () <- swallow
          t <- raw
          t <- expand(t)
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\number command", t.pos))
    }

  def meaning(t: Token) = t match {
    case CharacterToken(c, Category.ESCAPE_CHARACTER) =>
      f"escape character $c"
    case CharacterToken(c, Category.BEGINNING_OF_GROUP) =>
      f"begin-group character $c"
    case CharacterToken(c, Category.END_OF_GROUP) =>
      f"end-group character $c"
    case CharacterToken(c, Category.MATH_SHIFT) =>
      f"math shift character $c"
    case CharacterToken(c, Category.ALIGNMENT_TAB) =>
      f"alignment tab character $c"
    case CharacterToken(c, Category.END_OF_LINE) =>
      f"end-of-line character $c"
    case CharacterToken(c, Category.PARAMETER) =>
      f"macro parameter character $c"
    case CharacterToken(c, Category.SUPERSCRIPT) =>
      f"superscript character $c"
    case CharacterToken(c, Category.SUBSCRIPT) =>
      f"subscript character $c"
    case CharacterToken(c, Category.IGNORED_CHARACTER) =>
      f"ignored character character $c"
    case CharacterToken(c, Category.SPACE) =>
      f"space character character $c"
    case CharacterToken(c, Category.LETTER) =>
      f"the letter $c"
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      f"the character $c"
    case CharacterToken(c, Category.ACTIVE_CHARACTER) =>
      f"active character $c"
    case CharacterToken(c, Category.COMMENT_CHARACTER) =>
      f"comment character $c"
    case CharacterToken(c, Category.INVALID_CHARACTER) =>
      f"invalid character $c"
    case Primitive(n) =>
      f"\\$n"
    case ControlSequenceToken(n, _) =>
      env.css(n) match {
        case Some(TeXMacro(name, parameters, replacement, _, _)) =>
          f"macro:${parameters.map(_.toString(env)).mkString}->${replacement.reverseMap(_.toString(env)).mkString}"
        case Some(TeXCounter(name, cnt)) =>
          f"counter:${cnt.toInt}"
        case Some(TeXDimension(name, dim)) =>
          f"dimension:${dim.toInt}"
        case Some(_) =>
          f"${env.escapechar}$n"
        case None =>
          "undefined"
      }
    case _ =>
      throw new TeXMouthException("THIS IS A BUG. this case should never occur.", t.pos)
  }

  def expandMeaning: Processor[Token] =
    raw.flatMap {
      case ControlSequenceToken("meaning", _) =>
        for {
          () <- swallow
          token <- raw
          () <- swallow
          () <- pushback(toTokens(meaning(token)))
          t <- read
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\meaning command", t.pos))
    }

  def expandCsname: Processor[Token] =
    raw.flatMap {
      case start @ ControlSequenceToken("csname", _) =>
        def loop(acc: List[Token]): Processor[List[Token]] =
          read.flatMap {
            case ControlSequenceToken("endcsname", _) =>
              for (() <- swallow)
                yield acc
            case c @ CharacterToken(_, _) =>
              for {
                () <- swallow
                t <- loop(c :: acc)
              } yield t
            case t =>
              throwError(new TeXMouthException("character tokens only are expected inside \\csname..\\endcsname", t.pos))
          }
        for {
          () <- swallow
          name <- loop(Nil)
          t <- env.css(name.map(_.toString(env)).mkString) match {
            case Some(cs @ TeXMacro(_, parameters, replacement, long, outer)) => expandMacro(parameters, replacement, long, outer, start.pos)
            case _ => read // if not found it does nothing, just go ahead
          }
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\csname command", t.pos))
    }

  def expandafter: Processor[Token] =
    raw.flatMap {
      case start @ ControlSequenceToken("expandafter", _) =>
        for {
          () <- swallow
          first <- raw
          () <- swallow
          _ <- read
          () <- pushback(first)
          t <- read
        } yield t
      case t =>
        throwError(new TeXMouthException("expected \\expandafter command", t.pos))
    }

}
