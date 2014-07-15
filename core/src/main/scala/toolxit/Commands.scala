/*
* This file is part of the ToolXiT project.
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

import util.Positional

/** A TeX primitive command.
 *
 *  @author Lucas Satabin
 *
 */
sealed trait Command extends Positional

case object CControlSpace extends Command

case object CItalicCorrection extends Command

case class CAboveWithDelims(delim1: Token, delim2: Token, dimen: Dimension) extends Command

case class CAccent(accent: Byte, assignments: List[Assignment]) extends Command

case class CAdvance(variable: ControlSequence, by: Int) extends Command

case class CAfterAssignment(token: Token) extends Command

case class CAfterGroup(token: Token) extends Command

case object CBatchMode extends Command

case object CBeginGroup extends Command

case object CBotMark extends Command

case class CBox(register: Byte) extends Command

case class CChar(number: Byte) extends Command

case class CCLeader(boxOrGlue: ControlSequence, glue: ControlSequence) extends Command

case class CCloseIn(number: Byte) extends Command

case class CCloseOut(number: Byte) extends Command

case class CCopy(number: Byte) extends Command

case class CCountDef(name: String, number: Byte) extends Command

case object CCr extends Command

case object CCrCr extends Command

case class CCsName(tokens: List[Token]) extends Command

case class CDef(name: String, modifiers: List[Modifier.Value], parameters: List[Parameter], replacement: List[Token]) extends Command

case class CDelimiter(number: Int) extends Command

case class CDimenDef(name: String, number: Byte) extends Command

case class CDiscretionary(preBreak: List[Token], postBreak: List[Token], noBreak: List[Token]) extends Command

case object CDiplayLimits extends Command

case object CDisplayStyle extends Command

case class CDivide(variable: ControlSequence, by: Option[Int]) extends Command

case object CDump extends Command

case class CEdef(cs: ControlSequence, parameters: List[Parameter], replacement: List[Token]) extends Command

case object CEnd extends Command

case object CEndGroup extends Command

case object CEndInput extends Command

case class CEqNo() extends Command

sealed trait Assignment extends Command

case class Typeset(what: String) extends Command
