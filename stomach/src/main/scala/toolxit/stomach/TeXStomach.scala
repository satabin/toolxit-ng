/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package stomach

import util._

import scala.annotation.tailrec

import java.io.PrintWriter

class TeXStomach(env: TeXEnvironment, out: PrintWriter) extends Iteratees[Command] {

  type Processor[+T] = Iteratee[Command, T]

  lazy val process: Processor[Unit] = headOption.flatMap {
    case Some(End) =>
      throwError(EndException)
    case Some(cs @ CS(name)) => throwError(new TeXStomachException(f"Undefined control sequence \\$name", cs.pos))
    case Some(v) =>
      v match {
        case Typeset(c)        => out.print(c)
        case Par               => out.print("\n\n")
        case Relax             => // do nothing
        case Assignment(assgn) => assign(assgn)
        case Uppercase(tokens) => makeUppercase(tokens)
        case Lowercase(tokens) => makeLowercase(tokens)
        case cs @ CS(name)     => assert(false)
      }
      process
    case None =>
      Done(())
  }

  def assign(assignment: Assignment): Unit = assignment match {
    case CounterAssignment(cnt, v, mode, global) =>
      env.count(cnt, mode, global) = v
    case DimensionAssignment(dim, v, mode, global) =>
      env.dimen(dim, mode, global) = v
    case IntegerParameterAssignment(name, v, mode, global) =>
      env.integerParameter(name, mode, global) = v
    case CategoryAssignment(char, cat, global) =>
      env.category(char, global) = cat
    case LccodeAssignment(char, lc, global) =>
      env.lccode(char, global) = lc
    case UccodeAssignment(char, uc, global) =>
      env.uccode(char, global) = uc
    case CharacterDefinition(name, char, global) =>
      env.css(name, global) = TeXChar(name, char)
    case CounterDefinition(name, number, global) =>
      env.css(name, global) = TeXCounter(name, number)
    case DimensionDefinition(name, number, global) =>
      env.css(name, global) = TeXDimension(name, number)
    case LetAssignment(name, alias, global) => alias match {
      case cs @ ControlSequenceToken(alias, _) => env.css(alias) match {
        case Some(cs) =>
          env.css(name, global) = cs
        case None =>
          // if the control sequence does not exist, then no alias is created
          if (Primitives.all.contains(alias))
            env.css(name, global) = TeXCsAlias(name, cs)
      }
      case _ =>
        env.css(name, global) = TeXCsAlias(name, alias)
    }
    case CurrentFontAssignment(fname, magnification, global) =>
      env.font(global) = (fname, magnification)
    case TextFontAssignment(number, fname, magnification, global) =>
      env.textfont(number, global) = (fname, magnification)
    case ScriptFontAssignment(number, fname, magnification, global) =>
      env.scriptfont(number, global) = (fname, magnification)
    case ScriptScriptFontAssignment(number, fname, magnification, global) =>
      env.scriptscriptfont(number, global) = (fname, magnification)
    case FontAssignment(name, fname, mag, global) =>
      env.css(name, global) = TeXFont(name, fname, mag)
    case FontDimensionAssignment(param, fname, mag, dimen) =>
      env.fontManager.update(fname, mag, param, dimen)
    case HyphenationCharacterAssignment(fname, mag, c) =>
      env.fontManager.hyphenchar(fname, mag) = c
    case SkewCharacterAssignment(fname, mag, c) =>
      env.fontManager.skewchar(fname, mag) = c
    case HtAssignment(num, value) =>
      env.ht(num) = value
    case WdAssignment(num, value) =>
      env.wd(num) = value
    case DpAssignment(num, value) =>
      env.dp(num) = value
    case InteractionModeAssignment(mode) =>
      env.interactionmode = mode
    case SpecialIntegerAssignment(name, value) =>
      env.integers(name) = value
    case SpecialDimensionAssignment(name, value) =>
      env.dimensions(name) = value
    case BoxAssignment(number, box)                 =>
    // TODO
    case StartHBoxAssignment(number, specification) =>
    // TODO
    case StartVBoxAssignment(number, specification) =>
    // TODO
    case StartVTopAssignment(number, specification) =>
    // TODO
    case Read(inputno, cs, global)                  =>
    // TODO read next line in input number if exists and is open. otherwise read from stdin
  }

  def makeUppercase(tokens: List[Token]): Unit = {
    @tailrec
    def loop(tokens: List[Token], uppercased: List[Token]): Unit = tokens match {
      case Nil                                     => env.pushReadAgain(uppercased)
      case CharacterToken(c, cat) :: rest          => loop(rest, CharacterToken(env.uccode(c), cat) :: uppercased)
      case GroupToken(open, tokens, close) :: rest => assert(false)
      case t :: rest                               => loop(rest, t :: uppercased)
    }
    loop(tokens, Nil)
  }

  def makeLowercase(tokens: List[Token]): Unit = {
    @tailrec
    def loop(tokens: List[Token], uppercased: List[Token]): Unit = tokens match {
      case Nil                                     => env.pushReadAgain(uppercased)
      case CharacterToken(c, cat) :: rest          => loop(rest, CharacterToken(env.lccode(c), cat) :: uppercased)
      case GroupToken(open, tokens, close) :: rest => assert(false)
      case t :: rest                               => loop(rest, t :: uppercased)
    }
    loop(tokens, Nil)
  }

}

case object EndException extends Exception
