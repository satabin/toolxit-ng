/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

import dimen._
import glue._
import font._
import box._
import util._

/** A TeX command returned by the parser.
 *  The calling context may decide to interpret as it wishes the commands.
 *  Basically, the parser returns a character typeset command or a control sequence.
 *  Parsing-related commands are interpreted directly by the parser and expanded as needed.
 *  This includes:
 *   - Macro definition,
 *   - If/then/else commands,
 *   - Input command,
 *   - Character code definition,
 *   - ...
 *  Formatting and typesetting commands are left up to the caller.
 *  This architecture allows for varying tools based on TeX syntax but having different purpose
 *  than being typesetting system (e.g. a tool to work on the structured document, ...).
 *
 *  @author Lucas Satabin
 *
 */
sealed trait Command extends Positional

/** The most basic command is the typesetting of a character */
case class Typeset(what: Char) extends Command

/** A new paragraph command. */
case object Par extends Command

/** No-op command. */
case object Relax extends Command

/** The end command. */
case object End extends Command

/** A control sequence that was not interpreted by the parser.
 *
 *  @author Lucas Satabin
 */
case class CS(name: String) extends Command

sealed trait Assignment extends Command
object Assignment {
  def unapply(cmd: Command): Option[Assignment] =
    cmd match {
      case assgn: Assignment => Some(assgn)
      case _                 => None
    }
}

sealed trait AssignmentOp[+T]
case class SetOp[T](value: T) extends AssignmentOp[T]
case class Advance[T](by: T) extends AssignmentOp[T]
case class Multiply(by: Int) extends AssignmentOp[Nothing]
case class Divide(by: Int) extends AssignmentOp[Nothing]

/** An integer parameter assignment command. */
case class IntegerParameterAssignment(name: String, op: AssignmentOp[Int], global: Boolean) extends Assignment

/** A counter assignment command. */
case class CounterAssignment(id: Byte, op: AssignmentOp[Int], global: Boolean) extends Assignment

/** A dimension assignment command. */
case class DimensionAssignment(id: Byte, op: AssignmentOp[Dimension], global: Boolean) extends Assignment

/** A dimension parameter assignment command. */
case class DimensionParameterAssignment(name: String, op: AssignmentOp[Dimension], global: Boolean) extends Assignment

/** A glue assignment command. */
case class GlueAssignment(id: Byte, op: AssignmentOp[Glue], global: Boolean) extends Assignment

/** A glue parameter assignment command. */
case class GlueParameterAssignment(name: String, op: AssignmentOp[Glue], global: Boolean) extends Assignment

/** A token list assignment command. */
case class TokensAssignment(id: Byte, value: List[Token], global: Boolean) extends Assignment

/** A token list parameter assignment command. */
case class TokensParameterAssignment(name: String, value: List[Token], global: Boolean) extends Assignment

/** A character category assignment */
case class CategoryAssignment(c: Char, cat: Category, global: Boolean) extends Assignment

/** A character math code assignment */
case class MathCodeAssignment(c: Char, code: Int, global: Boolean) extends Assignment

/** A delimiter code assignment */
case class DelimiterCodeAssignment(c: Char, code: Int, global: Boolean) extends Assignment

/** A character uppercase code assignment. */
case class UccodeAssignment(c: Char, uc: Char, global: Boolean) extends Assignment

/** A character lowercase code assignment. */
case class LccodeAssignment(c: Char, lc: Char, global: Boolean) extends Assignment

/** A space factor code assignment. */
case class SfcodeAssignment(c: Char, sf: Int, global: Boolean) extends Assignment

/** A character definition. */
case class CharacterDefinition(name: String, c: CharacterToken, global: Boolean) extends Assignment

/** A math character definition. */
case class MathCharacterDefinition(name: String, code: Int, global: Boolean) extends Assignment

/** A counter definition. */
case class CounterDefinition(name: String, number: Byte, global: Boolean) extends Assignment

/** A dimension definition. */
case class DimensionDefinition(name: String, number: Byte, global: Boolean) extends Assignment

/** A token list definition. */
case class TokensDefinition(name: String, number: Byte, global: Boolean) extends Assignment

/** A `\let` assignment. */
case class LetAssignment(name: String, meaning: Token, global: Boolean) extends Assignment

/** Assigns the content of next line in the input with given number to the given control sequence. */
case class Read(inputno: Byte, cs: String, global: Boolean) extends Assignment

/** Assigns the current font to the given one. */
case class CurrentFontAssignment(fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment

/** Assigns the given textfont. */
case class TextFontAssignment(number: Byte, fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment

/** Assigns the given scriptfont. */
case class ScriptFontAssignment(number: Byte, fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment

/** Assigns the given scriptscriptfont. */
case class ScriptScriptFontAssignment(number: Byte, fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment

/** Assigns a font. */
case class FontAssignment(cs: String, fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment

/** Assigns the value of a font parameter. */
case class FontDimensionAssignment(parameter: Int, fname: String, magnification: Option[Either[Dimension, Double]], dimen: Dimension) extends Assignment

/** Assigns the hyphenation character of a font. */
case class HyphenationCharacterAssignment(fname: String, magnification: Option[Either[Dimension, Double]], char: Char) extends Assignment

/** Assigns the skew character of a font. */
case class SkewCharacterAssignment(fname: String, magnification: Option[Either[Dimension, Double]], char: Char) extends Assignment

case class HtAssignment(number: Byte, dimen: Dimension) extends Assignment

case class WdAssignment(number: Byte, dimen: Dimension) extends Assignment

case class DpAssignment(number: Byte, dimen: Dimension) extends Assignment

case class InteractionModeAssignment(mode: InteractionMode) extends Assignment

case class SpecialIntegerAssignment(name: String, value: Int) extends Assignment

case class SpecialDimensionAssignment(name: String, value: Dimension) extends Assignment

case class BoxAssignment(target: Byte, box: Box) extends Assignment

case class StartHBoxAssignment(number: Byte, spec: Option[Specification]) extends Assignment

case class StartVBoxAssignment(number: Byte, spec: Option[Specification]) extends Assignment

case class StartVTopAssignment(number: Byte, spec: Option[Specification]) extends Assignment

case class EndBox(mode: Mode) extends Command

case class Uppercase(tokens: List[Token]) extends Command

case class Lowercase(tokens: List[Token]) extends Command

case class Message(tokens: List[Token], error: Boolean) extends Command

case class Showthe(tokens: List[Token]) extends Command
