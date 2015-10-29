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
class TeXMouth(private var env: TeXEnvironment, private var stream: LineStream) {

  // the current stack of tokens that are read or expanded
  private val tokens: Stack[Token] =
    Stack.empty[Token]

  /** The current reading state. Starts in new line' state */
  private var state: ReadingState.Value =
    ReadingState.N

  /** Indicates whether the parser is expanding control sequences */
  private var expanding: Boolean =
    true

  /** Indicates whether the parser accepts implicit characters */
  private var implicits: Boolean =
    true

  private object Primitive {
    def unapply(cs: ControlSequenceToken): Option[String] =
      env.css(cs.name) match {
        case Some(TeXPrimitive(name)) => Some(name)
        case Some(_) | None           => None
      }
  }

  /** Execute the given body with the expansion status, and restore the old status afterward */
  private def withExpansion[T](value: Boolean)(body: =>T): T = {
    val old = expanding
    try {
      expanding = value
      body
    } finally {
      expanding = old
    }
  }

  /** Execute the given body with the implicit status, and restore the old status afterward */
  private def withImplicits[T](value: Boolean)(body: =>T): T = {
    val old = implicits
    try {
      implicits = value
      body
    } finally {
      implicits = old
    }
  }

  /** Accepts only the given token, and returns it */
  def eat(token: Token): Try[Token] =
    read() flatMap {
      case t if t == token =>
        swallow()
        Success(t)
      case t =>
        Failure(new TeXParsingException(s"expected $token, but found $t", t.pos))
    }

  /** Pops the first token out of the stack if any */
  def swallow(): Unit =
    if(tokens.nonEmpty)
      tokens.pop

  /** Returns the next unexpanded token */
  private def raw(): Try[Token] =
    if(tokens.isEmpty) {
      TeXEyes.next(state, env, stream) map {
        case (token, newState, newStream) =>
          state = newState
          stream = newStream
          // create a new token stack with the one we just read
          tokens.push(token)
          token
      }
    } else {
      Success(tokens.head)
    }

  /** Returns the next expanded token */
  private def expand(cs: ControlSequence): Try[Token] =
    cs match {
      case TeXMacro(_, parameters, replacement, long, outer) =>
        // consume the macro name name
        swallow()
        // parse the arguments
        ???
      case TeXPrimitive(_) =>
        ???
      case _ =>
        ???
    }

  /** Returns the next token, expanded if necessary */
  def read(): Try[Token] =
    raw().flatMap {
      case token @ ControlSequenceToken(name, false) if implicits =>
        // if this control sequence is an alias and implicit characters are allowed,
        // push its meaning back in the input stream and re-read
        env.css(name) match {
          case Some(TeXCharAlias(_, token)) =>
            // consume it
            swallow()
            // push the replacement token onto the token stack
            tokens.push(token)
            // and return it
            Success(token)
          case Some(TeXCsAlias(_, m)) =>
            // this is a macro, expand it
            expand(m)
          case Some(cs) if expanding =>
            // go through normal expansion process
            expand(cs)
          case Some(_) | None =>
            // unknown control sequence or no expansion, return it as is
            Success(token)
        }

      case token @ ControlSequenceToken(name, _) if expanding =>
        // if it is a macro and we are in expanding mode, expand it
        env.css(name) match {
          case Some(cs) =>
            // expand it if found
            expand(cs)
          case None =>
            // otherwise return it
            Success(token)
        }

      case t =>
        // otherwise just return it
        Success(t)
    }

  /** Parses and returns the next command. Tokens are expanded as needed, and we are ensured to get a typesetting command back */
  @tailrec
  final def parseCommand(): Try[Command] = read() match {
    case Success(char @ CharacterToken(c, _)) =>
      // This will probably be the case most of the time,
      // the command is simply to type set some character.
      Success(CTypeset(c).atPos(char.pos))

    case Success(Primitive("def" | "gdef" | "edef" | "xdef" | "outer" | "long" | "global")) =>
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
   *  <macro def> ::= \def <control sequence> <parameter text> { <replacement text> }
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
      read().flatMap {
        case ControlSequenceToken(name, _) =>
          swallow()
          Success(name)
        case t =>
          Failure(new TeXParsingException(s"Macro name must be a control sequence or an active character", t.pos))
      }

    def parseParameters(): Try[(Boolean, List[Token])] = withImplicits(true) {
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
              Failure(new TeXParsingException(s"Parameters must be numbered consecutively. Got $tok but expected $nextParam", tok.pos))
          }

        case Success(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
          // we reached the end of the parameter list return it
          Success(false, acc.reverse)

        case Success(tok @ ControlSequenceToken(name, _)) if env.css.isOuter(name) =>
          // macro declared as `outer' are not allowed in the parameter text
          Failure(new TeXParsingException(s"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

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
        Failure(new TeXParsingException(s"Unexpected token when parsing macro declaration $t", t.pos))
    }

    def parseReplacement(appendBrace: Boolean): Try[List[Token]] =
      parseGroup(true, false).map {
        case GroupToken(_, tokens, _) =>
          if(appendBrace)
            CharacterToken('{', Category.BEGINNING_OF_GROUP) :: tokens
          else
            tokens
      }

    read() flatMap {
      case ControlSequenceToken("def" | "gdef" | "edef" | "xdef" | "long" | "outer" | "global", _) =>
        for {
          // a macro declaration starts with optional modifiers
          (long, outer, global) <- parseModifiers()
          // then comes the declaration (and tokens are not expanded during this phase)
          (global, expanded, name, params, appendBrace) <- withExpansion(false)(parseMacroDecl(global))
          // and the replacement text expanded only if needed
          replacement <- withExpansion(expanded)(parseReplacement(appendBrace))
        } yield (global, TeXMacro(name, params, replacement, long, outer))

      case t =>
        Failure(new TeXParsingException(s"Unexpected token when parsing macro declaration $t", t.pos))

    }

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
        if(reverted)
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
        Failure(new TeXParsingException(s"Macro $name declared as `\\outer` is not allowed in parameter text", tok.pos))

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
        Failure(new TeXParsingException(s"Beginning of group character expected but $tok found", tok.pos))
    }

  }

}
