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

/** The TeX mouth is a parser from tokens produced by the [[toolxit.eyes.TeXEyes]]
 *  and produces in turn commands that can be digested by the stomach.
 *  This parser interpretes all primitive TeX commands that directly impacts the parsing
 *  of the rest itself.
 *  These commands include (but are not limited to):
 *   - Macro definitions,
 *   - If/then/else commands,
 *   - Inputs,
 *   - Character code definition,
 *   - ...
 *
 *  @author Lucas Satabin
 */
class TeXMouth(protected var env: TeXEnvironment, protected var stream: LineStream) {

  // the current token that is read
  private var token: Option[Token] = None

  /** The current reading state. Starts in new line' state */
  protected var state: ReadingState.Value =
    ReadingState.N

  /** Indicates whether the parser is expanding control sequences */
  protected var expanding: Boolean =
    true

  /** Accepts only the given token, and returns it */
  def eat(token: Token): Try[Token] =
    read() flatMap {
      case t if t == token =>
        swallow()
        Success(t)
      case t =>
        Failure(new TeXParsingException(s"expected $token, but found $t", t.pos))
    }

  def swallow(): Unit =
    token = None

  /** Returns the next unexpanded token */
  private def unexpanded(): Try[Token] =
    token match {
      case Some(token) =>
        Success(token)
      case None =>
        val tok = TeXEyes.next(state, env, stream) map {
          case (token, newState, newStream) =>
            state = newState
            stream = newStream
            token
        }
        token = tok.toOption
        tok
    }

  /** Returns the next expanded token */
  private def expanded(): Try[Token] =
    unexpanded() flatMap {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXMacro(_, parameters, replacement, long, outer)) =>
            ???
          case Some(TeXPrimitive(_)) =>
            ???
          case None =>
            Failure(new ControlSequenceException(s"undefined control sequence `$name`"))
        }
    }

  /** Returns the next token, expanded if necessary */
  def read(): Try[Token] =
    if(expanding)
      expanded()
    else
      unexpanded()

  /** Parses and returns the next command. Tokens are expanded as needed, and we are ensured to get a typesetting command back */
  @tailrec
  final def parseCommand(): Try[Command] = read() match {
    case Success(char @ CharacterToken(c, _)) =>
      // This will probably be the case most of the time,
      // the command is simply to type set some character.
      Success(CTypeset(c).atPos(char.pos))

    case Success(ControlSequenceToken("def" | "gdef" | "edef" | "xdef" | "outer" | "long" | "global", _)) =>
      // define a new macro, register it and parse next command
      parseMacroDef() match {
        case Success((global, m)) =>
          if(global)
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
      Failure(new TeXParsingException(s"Unexpected token $t instead of a TeX command", t.pos))

    case Failure(t) =>
      Failure(t)
  }

  /** Parses the definition of a new macro.
   *  A macro definition is something that looks like:
   *  {{{
   *  \def <control sequence> <parameter text> { <replacement text> }
   *  }}}
   */
  def parseMacroDef(): Try[(Boolean, TeXMacro)] = {

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
    def parseModifiers(long: Boolean = false,
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

    // a macro name is an unexpanded control sequence
    def parseName(): Try[String] =
      unexpanded().flatMap {
        case ControlSequenceToken(name, _) =>
          swallow()
          Success(name)
        case t =>
          Failure(new TeXParsingException(s"Macro name must be a control sequence", t.pos))
      }

    def parseParameters(expanded: Boolean): Try[List[Parameter]] =
      ???

    def parseMacroDecl(global: Boolean): Try[(Boolean, Boolean, String, List[Parameter])] = read() flatMap {
      case ControlSequenceToken("def", _) =>
        swallow()
        for {
          name <- parseName()
          params <- parseParameters(false)
        } yield (global, false, name, params)
      case ControlSequenceToken("gdef", _) =>
        swallow()
        for {
          name <- parseName()
          params <- parseParameters(false)
        } yield (true, false, name, params)
      case ControlSequenceToken("edef", _) =>
        swallow()
        for{
          name <- parseName()
          params <- parseParameters(true)
        } yield (false, true, name, params)
      case ControlSequenceToken("xdef", _) =>
        swallow()
        for {
          name <- parseName()
          params <- parseParameters(true)
        } yield (true, true, name, params)
      case t =>
        Failure(new TeXParsingException(s"Unexpected token when parsing macro declaration $t", t.pos))
    }

    def parseReplacement(expanded: Boolean): Try[List[Token]] =
      ???

    read() flatMap {
      case ControlSequenceToken("def" | "gdef" | "edef" | "xdef" | "long" | "outer" | "global", _) =>
        for {
          // a macro declaration starts with optional modifiers
          (long, outer, global) <- parseModifiers()
          // then comes the declaration
          (global, expanded, name, params) <- parseMacroDecl(global)
          // and the replacement text
          replacement <- parseReplacement(expanded)
        } yield (global, TeXMacro(name, params, replacement, long, outer))

      case t =>
        Failure(new TeXParsingException(s"Unexpected token when parsing macro declaration $t", t.pos))

    }

  }

}
