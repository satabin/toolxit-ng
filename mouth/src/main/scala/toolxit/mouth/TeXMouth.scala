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

import scala.util.{
  Try,
  Failure,
  Success
}

import eyes._
import util._

import scala.annotation.tailrec

import scala.collection.mutable.Stack

import java.io.{
  Reader,
  FileReader
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
class TeXMouth(private var _env: TeXEnvironment, reader: Reader)
    extends TeXMacros
    with TeXParameters
    with TeXInternals
    with TeXNumbers
    with TeXDimensions {

  def env = _env

  private def env_=(e: TeXEnvironment): Unit =
    _env = e

  // the current stack of tokens that are read or expanded
  private val tokens: Stack[Token] =
    Stack.empty[Token]

  /** Pushes the given sequence back in reverse order into the input token stream */
  protected[this] def pushback(rev: Seq[Token]): Unit =
    tokens.pushAll(rev)
  protected[this] def pushback(t: Token): Unit =
    tokens.push(t)

  /** the input stream to read */
  private val eyes: Stack[TeXEyes] =
    Stack(new TeXEyes(reader))

  /** Indicates whether the parser is expanding control sequences */
  private var expanding: Boolean =
    true

  /** Indicates whether the input must be closed at the nex end of line.
   *  This is the case when the `\endinput` command has been encountered. */
  private var closeAtEOL: Boolean =
    false

  def openInput(name: String): Try[Unit] =
    Try(eyes.push(new TeXEyes(new FileReader(name))))

  def closeInput(): Try[Unit] =
    Try {
      closeAtEOL = false
      eyes.pop().close()
    }

  protected[this] object Primitive {
    def unapply(cs: ControlSequenceToken): Option[String] =
      env.css(cs.name) match {
        case Some(_) => None
        case None    => Some(cs.name)
      }
  }

  protected[this] object If {
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

  /** Execute the given body with the expansion status, and restore the old status afterward */
  final def withExpansion[T](value: Boolean)(body: => T): T = {
    val old = expanding
    try {
      expanding = value
      body
    } finally {
      expanding = old
    }
  }

  /** Accepts only the given token, and returns it */
  def eat(token: Token): Try[Unit] =
    raw() flatMap {
      case t if t == token =>
        swallow()
        Success(())
      case t =>
        Failure(new TeXMouthException(f"expected $token, but found $t", t.pos))
    }

  /** Pops the first token out of the stack if any */
  def swallow(): Unit =
    if (tokens.nonEmpty)
      tokens.pop

  /** Returns the next unexpanded token */
  @tailrec
  final def raw(): Try[Token] =
    if (tokens.isEmpty) {
      eyes.headOption match {
        case Some(eyes) =>
          eyes.next(env) flatMap { token =>
            // push the token we just read onto the token stack, it will be popped when actually consumed
            tokens.push(token)
            token match {
              case CharacterToken(_, Category.END_OF_LINE) if closeAtEOL =>
                for(() <- closeInput())
                  yield token
              case _ =>
                Success(token)
            }
          }
        case None if eyes.size > 1 =>
          // this stream is exhausted, but there is more!
          closeInput()
          raw()
        case None =>
          Failure(new TeXInternalException("No more input stream available"))
      }
    } else {
      Success(tokens.head)
    }

  /** Returns the next expanded token */
  final def expand(cs: ControlSequence, pos: Position): Try[Token] =
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
              tokens.pushAll(arg)
            case token =>
              tokens.push(token)
          }
          // and read again
          read()

        }
      case _ =>
        ???
    }

  /** Returns the next token, expanded if necessary */
  protected[this] def read(): Try[Token] =
    raw().flatMap {
      case token @ ControlSequenceToken(name, _) if expanding =>
        // if it is a macro and we are in expanding mode, expand it
        env.css(name) match {
          case Some(cs) =>
            // expand it if found
            expand(cs, token.pos)
          case None =>
            // check for primitive control sequence expansions
            name match {
              case If() =>
                expandIf()
              case "input" =>
                expandInput()
              case "endinput" =>
                swallow()
                closeAtEOL = true
                read()
              case "jobname" =>
                swallow()
                for(c <- env.jobname.reverse) {
                  if(Character.isWhitespace(c))
                    tokens.push(CharacterToken(c, Category.SPACE))
                  else
                    tokens.push(CharacterToken(c, Category.OTHER_CHARACTER))
                }
                read()
              case "romannumeral" =>
                expandRomannumeral()
              case "number" =>
                expandNumber()
              case "string" =>
                expandString()
              case _ =>
                // otherwise return it
                swallow()
                Success(token)
            }
        }

      case t =>
        // otherwise just return it
        Success(t)
    }

  /** Parses and returns the next command. Tokens are expanded as needed, and we are ensured to get a typesetting command back */
  @tailrec
  final def parseCommand(): Try[Command] =
    parseModifiers() match {
      case Success((long, outer, global)) =>
        read() match {
          case Success(char @ CharacterToken(c, _)) =>
            if (long || outer || global) {
              Failure(new TeXMouthException(f"You cannot use a prefix with `the letter $c'", char.pos))
            } else {
              // This will probably be the case most of the time,
              // the command is simply to type set some character.
              Success(CTypeset(c).atPos(char.pos))
            }

          case Success(Primitive("def" | "gdef" | "edef" | "xdef")) =>
            // define a new macro, register it and parse next command
            parseMacroDef(long, outer, global) match {
              case Success((global, m)) =>
                if (global)
                  // register the macro in global scope
                  env.css.global(m.name) = m
                else
                  // register the macro in local scope
                  env.css(m.name) = m
                parseCommand()
              case Failure(t) =>
                Failure(t)
            }

          case Success(cs @ ControlSequenceToken(name, _)) =>
            Success(CControlSequence(name).atPos(cs.pos))

          case Success(t) =>
            Failure(new TeXMouthException(f"Unexpected token $t instead of a TeX command", t.pos))

          case Failure(t) =>
            Failure(t)
        }
      case Failure(t) =>
        Failure(t)
    }

  // This parsing method is not implemented with `flatMap` to allow
  // for tail-recursion. I am not sure whether this is a useful optimization
  // in this case but I can imagine scenarios in which the expansion process
  // may introduce a lot of successive modifiers and where it could be helpful
  // to spare stack.
  // The main drawback in writing this method with a match instead of a `flatMap`
  // is that we have to pattern-match `Failure` and build it back to type-check.
  // Moreover, we also must extract the `Success` constructor.
  // Well, that's still ok I guess, if this turns out to be armful, it will be easy
  // to change this to use `flatMap`
  @tailrec
  final def parseModifiers(long: Boolean = false,
    outer: Boolean = false,
    global: Boolean = false): Try[(Boolean, Boolean, Boolean)] = read() match {
    case Success(ControlSequenceToken("long", _)) =>
      swallow()
      parseModifiers(true, outer, global)

    case Success(ControlSequenceToken("outer", _)) =>
      swallow()
      parseModifiers(long, true, global)

    case Success(ControlSequenceToken("global", _)) =>
      swallow()
      parseModifiers(long, outer, true)

    case Success(_) =>
      Success(long, outer, global)

    case Failure(t) =>
      Failure(t)

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
  def parseGroup(reverted: Boolean, allowOuter: Boolean): Try[GroupToken] = {
    @tailrec
    def loop(level: Int, open: Token, acc: List[Token]): Try[GroupToken] = read() match {
      case Success(tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
        // start a new nested group, consume the opening token
        swallow()
        // and parses the rest adding the opening token to the accumulator
        loop(level + 1, open, tok :: acc)

      case Success(tok @ CharacterToken(_, Category.END_OF_GROUP)) if level == 0 =>
        // closing the top-level group, this is an exit condition, consume the token
        swallow()
        // and return the built group
        if (reverted)
          Success(GroupToken(open, acc, tok))
        else
          Success(GroupToken(open, acc.reverse, tok))

      case Success(tok @ CharacterToken(_, Category.END_OF_GROUP)) =>
        // closing a nested group, consume the character
        swallow()
        // and continue, adding it to the accumulator
        loop(level - 1, open, tok :: acc)

      case Success(tok @ ControlSequenceToken(name, _)) if !allowOuter && env.css.isOuter(name) =>
        // macro declared as `outer' are not allowed in the parameter text
        Failure(new TeXMouthException(f"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

      case Success(tok) =>
        // any other character is consumed
        swallow()
        // and added to the accumulator
        loop(level, open, tok :: acc)

      case Failure(t) =>
        Failure(t)

    }
    read() flatMap {
      case tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP) =>
        // ok, so we start a new group
        // consume the opening token
        swallow()
        // and loop until this group is correctly closed
        loop(0, tok, Nil)
      case tok =>
        // this is not an opening group, meaning, this is an error
        Failure(new TeXMouthException(f"Beginning of group character expected but $tok found", tok.pos))
    }

  }

  /** Reads an space character (with category code SPACE). */
  final def parseOptSpace(): Try[Option[CharacterToken]] =
    read() match {
      case Success(c @ CharacterToken(_, Category.SPACE)) =>
        swallow()
        Success(Some(c))
      case Success(_) =>
        Success(None)
      case Failure(t) =>
        Failure(t)
    }

  /** Reads zero or more space characters (with category code SPACE). */
  @tailrec
  final def parseSpaces(): Try[Unit] =
    read() match {
      case Success(CharacterToken(_, Category.SPACE)) =>
        swallow()
        parseSpaces()
      case Success(_) =>
        Success(())
      case Failure(t) =>
        Failure(t)
    }

  /** Reads a keyword of the form:
   *  {{{
   *  <keyword(name)> ::= <space>* <name>
   *  }}}
   */
  final def keyword(name: String): Try[List[CharacterToken]] =
    parseSpaces() match {
      case Success(()) =>
        @tailrec
        def loop(name: String, acc: List[CharacterToken]): Try[List[CharacterToken]] =
          if (name.isEmpty) {
            Success(acc.reverse)
          } else {
            read() match {
              case Success(char @ CharacterToken(c, _)) if c.toLower == name.charAt(0).toLower =>
                swallow()
                loop(name.substring(1), char :: acc)
              case Success(t) =>
                Failure(new TeXMouthException(f"Expected ${name.charAt(0)} but $t found", t.pos))
              case Failure(t) =>
                Failure(t)
            }
          }
        loop(name, Nil)
      case Failure(t) =>
        Failure(t)
    }

}
