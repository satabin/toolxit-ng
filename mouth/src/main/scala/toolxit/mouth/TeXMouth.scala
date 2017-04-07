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

import scala.util.{
  Try,
  Success,
  Failure
}

import util._

import java.io.{
  FileReader,
  LineNumberReader,
  FileNotFoundException
}

/** The TeX mouth is a parser from tokens produced by the [[toolxit.eyes.TeXEyes]]
 *  and produces in turn commands that can be digested by the stomach.
 *  This parser interpretes all primitive TeX commands that define new macros, counters, or change internal quantities.
 *  These commands include (but are not limited to):
 *   - Macro definitions,
 *   - If/then/else commands,
 *   - Inputs,
 *   - Character code definition,
 *   - ...
 *
 *  Control sequences are reduced until no more macro with the given name exists.
 *  The result is that the parser always returns either a character typesetting command, or a control sequence that corresponds to
 *  something that is no macro or interpreted counter, quantity, ...
 *  It is up to the consumer to determine what to do with the result, whether more arguments are expected or
 *  the control sequence is unknown or it simply queries the next token.
 *
 *  @author Lucas Satabin
 */
class TeXMouth(val env: TeXEnvironment)
    extends Iteratees[Token]
    with TeXMacros
    with TeXNumbers
    with TeXDimensions
    with TeXGlues
    with TeXFonts
    with TeXAssignments {

  type Processor[T] = Iteratee[Token, T]

  def accept(token: Token): Processor[Unit] =
    read.flatMap { t =>
      if (t == token) {
        swallow
      } else {
        throwError(new TeXMouthException(f"Expected $token", t.pos))
      }
    }

  def openInput(name: String): Processor[Unit] =
    Try(new LineNumberReader(new FileReader(name))) match {
      case Success(reader) =>
        env.pushInput(reader, Some(name), None)
        noop
      case Failure(e: Exception) =>
        throwError(e)
      case Failure(t) =>
        throw t
    }

  object Primitive {
    def unapply(cs: ControlSequenceToken): Option[String] =
      env.css(cs.name) match {
        case Some(_) => None
        case None    => Some(cs.name)
      }
  }

  object Implicit {
    def unapply(token: Token): Option[CharacterToken] =
      token match {
        case ControlSequenceToken(n, _) =>
          env.css(n) match {
            case Some(TeXChar(_, c)) => Some(c)
            case _                   => None
          }
        case _ =>
          None
      }
  }

  object UserDefined {
    @inline
    def unapply(name: String): Option[ControlSequence] =
      env.css(name)
  }

  object If {
    def unapply(name: String): Boolean =
      Primitives.isIf(name)
    def unapply(token: Token): Option[Token] =
      token match {
        case ControlSequenceToken(name, _) if Primitives.isIf(name) =>
          Some(token)
        case _ =>
          None
      }
  }

  def withExpansion[T](expand: Boolean)(p: Processor[T]): Processor[T] = {
    val oldexp = env.expanding
    env.expanding = expand
    p map { v =>
      env.expanding = oldexp
      v
    } recoverWith { e =>
      env.expanding = oldexp
      throwError(e)
    }
  }

  /** Returns the next unexpanded token */
  lazy val raw: Processor[Token] = peek.flatMap {
    case Some(t) => done(t)
    case None    => done(EOIToken().atPos(env.lastPosition))
  }

  def onEOI(msg: String)(it: Processor[Token]): Processor[Token] =
    it.flatMap {
      case t @ EOIToken() => throwError(new TeXMouthException(msg, t.pos))
      case t              => done(t)
    }

  /** Returns the next token, expanded if necessary */
  protected[this] lazy val read: Processor[Token] =
    raw.flatMap { token =>
      if (!env.expanding)
        done(token)
      else token match {
        case token @ ControlSequenceToken(name, _) =>
          // if it is a macro, expand it
          env.css(name) match {
            case Some(TeXMacro(_, parameters, replacement, long, outer)) =>
              // expand it if found
              expandMacro(parameters, replacement, long, outer, token.pos)
            case Some(_) =>
              // another user defined cs, will be treated in commands
              done(token)
            case None =>
              // check for primitive control sequence expansions
              name match {
                case If() =>
                  expandIf
                case "input" =>
                  expandInput
                case "endinput" =>
                  for {
                    () <- swallow
                    () = env.endinputEncountered = true
                    t <- read
                  } yield t
                case "jobname" =>
                  for {
                    () <- swallow
                    name = env.jobname.toList.reverseMap(c =>
                      if (c == ' ')
                        CharacterToken(c, Category.SPACE)
                      else
                        CharacterToken(c, Category.OTHER_CHARACTER))
                    () <- pushback(name)
                    t <- read
                  } yield t
                case "romannumeral" =>
                  expandRomannumeral
                case "number" =>
                  expandNumber
                case "string" =>
                  expandString
                case "meaning" =>
                  expandMeaning
                case "csname" =>
                  expandCsname
                case "expandafter" =>
                  expandafter
                case "noexpand" =>
                  for {
                    () <- swallow
                    t <- raw
                  } yield t
                case _ =>
                  if (env.inReplacement)
                    throwError(new TeXMouthException(f"Undefined control sequence \\$name.", token.pos))
                  else
                    // otherwise return it
                    done(token)
              }
          }
        case t =>
          // otherwise just return it
          done(t)
      }
    }

  /** Parses and returns the next command. Tokens are expanded as needed, and we are ensured to get a typesetting command back */
  lazy val command: Processor[Command] =
    modifiers().flatMap {
      case (long, outer, global) =>
        read.flatMap {
          case p @ Primitive("def" | "gdef" | "edef" | "xdef") =>
            // define a new macro, register it and parse next command
            for {
              (global, m) <- macroDef(long, outer, global)
              () = env.css(m.name, global) = m
              c <- command
            } yield c

          case tok @ StartsAssignment() =>
            // this is an assignment
            if (long || outer) {
              throwError(new TeXMouthException(f"Only prefix `\\global' is allowed for assignments", tok.pos))
            } else {
              simpleAssignment(global)
            }
          case tok =>

            if (long || outer || global) {
              throwError(new TeXMouthException(f"You cannot use a prefix with `${meaning(tok)}'", tok.pos))
            } else {
              tok match {

                case tok @ EOIToken() =>
                  throwError(new EOIException(tok.pos))
                case tok @ CharacterToken(c, Category.END_OF_GROUP) =>
                  throwError(new TeXMouthException(f"Too many $c's.", tok.pos))

                case CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
                  env.enterGroup
                  for {
                    GroupToken(_, tokens, _) <- group(true, true, true, false, true)
                    () <- pushback(tokens)
                    () = env.leaveGroup
                    c <- command
                  } yield c

                case char @ CharacterToken(c, _) =>
                  // This will probably be the case most of the time,
                  // the command is simply to type set some character.
                  for {
                    () <- swallow
                    ts <- done(Typeset(c).atPos(char.pos))
                  } yield ts

                case p @ Primitive("par") =>
                  for (() <- swallow)
                    yield Par

                case Primitive("relax") =>
                  for (() <- swallow)
                    yield Relax

                case Primitive("end") =>
                  for (() <- swallow)
                    yield End

                case t @ ControlSequenceToken(UserDefined(cs), _) =>
                  cs match {
                    case TeXChar(_, c) =>
                      for {
                        () <- swallow
                        () <- pushback(c)
                        // read again
                        c <- command
                      } yield c

                    case TeXCsAlias(_, t) =>
                      for {
                        () <- swallow
                        () <- pushback(t)
                        // read again
                        c <- command
                      } yield c

                    case TeXMacro(_, _, _, _, _) =>
                      throwError(new TeXMouthException("Macros should already be expanded in commands", t.pos))

                    case _ =>
                      throwError(new TeXMouthException(f"Command ${cs.name} not implemented yet", t.pos))
                  }

                case cs @ ControlSequenceToken(name, _) =>
                  for {
                    () <- swallow
                    cs <- done(CS(name).atPos(cs.pos))
                  } yield cs

                case t =>
                  throwError(new TeXMouthException(f"Unexpected token $t instead of a TeX command", t.pos))

              }
            }
        }
    }

  def modifiers(long: Boolean = false,
    outer: Boolean = false,
    global: Boolean = false): Processor[(Boolean, Boolean, Boolean)] = read.flatMap {
    case ControlSequenceToken("long", _) =>
      for {
        () <- swallow
        m <- modifiers(true, outer, global)
      } yield m

    case ControlSequenceToken("outer", _) =>
      for {
        () <- swallow
        m <- modifiers(long, true, global)
      } yield m

    case ControlSequenceToken("global", _) =>
      for {
        () <- swallow
        m <- modifiers(long, outer, true)
      } yield m

    case _ =>
      done((long, outer, global))

  }

  /** Parses a correctly nested group of the form:
   *  {{{
   *  <group> ::= `{' (<token> | <group>)* `}'
   *  }}}
   *  The `reverted` parameter indicates whether the tokens of this group are returned in reverse
   *  order.
   *  This is particularily useful when parsing the replacement text of a macro because
   *  the token list is saved in reverse order for efficiency reasons.
   *  The `allowOuter` parameter indicates whether control sequence declared as `outer` are allowed in
   *  this group.
   *  Typically, when parsing a group as a replacement text of a macro definition, they are not allowed.
   */
  def group(reverted: Boolean, allowOuter: Boolean, allowPar: Boolean, withParams: Boolean, localScope: Boolean): Processor[GroupToken] = {
    val read = onEOI("End of input reached while parsing group")(this.read)
    def loop(level: Int, open: Token, acc: List[Token]): Processor[GroupToken] = read.flatMap {
      case tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
        for {
          // start a new nested group, consume the opening token
          () <- swallow
          () = env.enterGroup
          // and parses the rest adding the opening token to the accumulator
          g <- loop(level + 1, open, tok :: acc)
          () = env.leaveGroup
        } yield g

      case tok @ CharacterToken(_, Category.END_OF_GROUP) if level == 0 =>
        for {
          // closing the top-level group, this is an exit condition, consume the token
          () <- swallow
          // and return the built group
        } yield if (reverted) GroupToken(open, acc, tok) else GroupToken(open, acc.reverse, tok)

      case tok @ CharacterToken(_, Category.END_OF_GROUP) =>
        for {
          // closing a nested group, consume the character
          () <- swallow
          // and continue, adding it to the accumulator
          g <- loop(level - 1, open, tok :: acc)
        } yield g

      case tok @ ControlSequenceToken("par", _) if !allowPar =>
        throwError(new TeXMouthException("`\\par` is not allowed in parameter text", tok.pos))
      case tok @ ControlSequenceToken(name, _) if !allowOuter && env.css.isOuter(name) =>
        // macro declared as `outer' are not allowed in the parameter text
        throwError(new TeXMouthException(f"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

      case param @ CharacterToken(_, Category.PARAMETER) if withParams =>
        // parsing a group with parameters inside
        def parameter(tok: Token): Processor[Token] = tok match {
          case tok @ CharacterToken(_, Category.PARAMETER) =>
            // this is an escaped parameter token
            for (() <- swallow)
              yield tok
          case CharacterToken(int(i), _) =>
            // this is a parameter token
            for (() <- swallow)
              yield ParameterToken(i).atPos(param.pos)
          case tok =>
            throwError(new TeXMouthException(f"Expecting an integer or a parameter token but got $tok", tok.pos))
        }
        for {
          () <- swallow
          tok <- read
          tok <- parameter(tok)
          g <- loop(level, open, tok :: acc)
        } yield g

      case tok =>
        for {
          // any other character is consumed
          () <- swallow
          // and added to the accumulator
          g <- loop(level, open, tok :: acc)
        } yield g

    }
    read.flatMap {
      case tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
        for {
          // ok, so we start a new group
          // consume the opening token
          () <- swallow
          () = if (localScope) env.enterGroup
          // and loop until this group is correctly closed
          g <- loop(0, tok, Nil)
          () = if (localScope) env.leaveGroup
        } yield g
      case tok =>
        // this is not an opening group, meaning, this is an error
        throwError(new TeXMouthException(f"Beginning of group character expected but $tok found", tok.pos))
    }

  }

  /** Reads an space character (with category code SPACE). */
  val optSpace: Processor[Option[CharacterToken]] =
    read.flatMap {
      case c @ CharacterToken(_, Category.SPACE) =>
        for (() <- swallow)
          yield Some(c)
      case _ =>
        done(None)
    }

  /** Reads zero or more space characters (with category code SPACE). */
  lazy val spaces: Processor[Unit] =
    read.flatMap {
      case CharacterToken(_, Category.SPACE) =>
        for {
          () <- swallow
          () <- spaces
        } yield ()
      case _ =>
        done(())
    }

  /** Reads a keyword of the form:
   *  {{{
   *  <keyword(name)> ::= <space>* <name>
   *  }}}
   */
  final def keyword(name: String, optional: Boolean): Processor[Boolean] = {
    def loop(idx: Int, acc: List[CharacterToken]): Processor[Boolean] =
      if (idx >= name.size) {
        done(true)
      } else {
        read.flatMap {
          case char @ CharacterToken(c, _) if c.toLower == name.charAt(idx).toLower =>
            for {
              () <- swallow
              b <- loop(idx + 1, char :: acc)
            } yield b
          case t =>
            if (optional)
              // push back what we read
              for {
                () <- pushback(acc)
              } yield false
            else
              throwError(new TeXMouthException(f"Expected ${name.charAt(idx)} but $t found", t.pos))
        }
      }
    for {
      () <- spaces
      b <- loop(0, Nil)
    } yield b
  }

  /** Reads a keyword of the form:
   *  {{{
   *  <keyword(name)> ::= <space>* <name>
   *  }}}
   *  amongst one of the provided keywords and returns the matching one in lower case.
   *  Longest match wins.
   */
  final def keyword(keywords: Seq[String]): Processor[String] = {
    val trie = new TrieNode(keywords)
    def loop(pos: Position, trie: TrieNode, acc: StringBuilder): Processor[String] =
      read.flatMap {
        case char @ CharacterToken(c, _) =>
          trie.query(c) match {
            case Some(trie) =>
              for {
                () <- swallow
                s <- loop(pos, trie, acc.append(c.toLower))
              } yield s
            case None =>
              if (trie.word)
                done(acc.toString)
              else
                throwError(new TeXMouthException(f"Expected one of ${keywords.map(_.toLowerCase).mkString("(", ", ", ")")}", pos))
          }
        case t =>
          throwError(new TeXMouthException(f"Expected one of ${keywords.map(_.toLowerCase).mkString("(", ", ", ")")}", t.pos))
      }
    for {
      () <- spaces
      t <- read
      kwd <- loop(t.pos, trie, new StringBuilder)
    } yield kwd
  }

  val filler: Processor[Unit] =
    for {
      () <- spaces
      t <- read
      () <- t match {
        case ControlSequenceToken("relax", _) =>
          for {
            () <- swallow
            () <- spaces
            () <- filler
          } yield ()
        case _ =>
          done(())
      }
    } yield ()

  val generalText: Processor[List[Token]] =
    for {
      () <- filler
      t <- read
      l <- t match {
        case CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
          for (GroupToken(_, tokens, _) <- group(false, true, true, false, false))
            yield tokens
        case Implicit(c @ CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
          for {
            () <- swallow
            () <- pushback(c)
            GroupToken(_, tokens, _) <- group(false, true, true, false, false)
          } yield tokens
        case _ =>
          throwError[Token](new TeXMouthException("Explicit or implicit beginning of group character expected", t.pos))
      }
    } yield l

}
