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
import font._
import util._

import enumeratum._

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

sealed trait AssignmentMode extends EnumEntry
object AssignmentMode extends Enum[AssignmentMode] {
  val values = findValues
  case object Set extends AssignmentMode
  case object Advance extends AssignmentMode
  case object Multiply extends AssignmentMode
  case object Divide extends AssignmentMode
}

/** An integer parameter assignment command. */
case class IntegerParameterAssignment(name: String, value: Int, mode: AssignmentMode, global: Boolean) extends Assignment

/** A counter assignment command. */
case class CounterAssignment(id: Byte, value: Int, mode: AssignmentMode, global: Boolean) extends Assignment

/** A dimension assignment command (in sp). */
case class DimensionAssignment(id: Byte, value: Int, mode: AssignmentMode, global: Boolean) extends Assignment

/** A character category assignment */
case class CategoryAssignment(c: Char, cat: Category, global: Boolean) extends Assignment

/** A character definition. */
case class CharacterDefinition(name: String, c: CharacterToken, global: Boolean) extends Assignment

/** A counter definition. */
case class CounterDefinition(name: String, number: Byte, global: Boolean) extends Assignment

/** A dimension definition. */
case class DimensionDefinition(name: String, number: Byte, global: Boolean) extends Assignment

/** A `\let` assignment. */
case class LetAssignment(name: String, meaning: Token, global: Boolean) extends Assignment

/** Assigns the content of next line in the input with given number to the given control sequence. */
case class Read(inputno: Byte, cs: String, global: Boolean) extends Assignment

/** Assigns the current font to the given one. */
case class CurrentFontAssignment(fname: String, magnification: Option[Either[Dimension, Double]], global: Boolean) extends Assignment
