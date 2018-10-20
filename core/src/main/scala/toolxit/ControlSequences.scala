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

sealed trait ControlSequence {
  val name: String
}

final case class TeXChar(
    name: String,
    char: CharacterToken) extends ControlSequence

final case class TeXCounter(
    name: String,
    number: Byte) extends ControlSequence

final case class TeXMathChar(
    name: String,
    code: Int) extends ControlSequence

final case class TeXDimension(
    name: String,
    number: Byte) extends ControlSequence

final case class TeXGlue(
    name: String,
    number: Byte) extends ControlSequence

final case class TeXMuglue(
    name: String,
    number: Byte) extends ControlSequence

/** This control sequence represents a macro with a parameter sequence and a replacement text.
 *  It is expected for the replacement text to be a list of the body tokens in reverse order.
 *  This may seem odd at first sight, but it relies on the fact that these tokens will be
 *  pushed back onto a stack, and that the efficient implementation pushes a sequence in
 *  its order, so that the last elements of the sequence will be the on top of the stack
 */
final case class TeXMacro(
    name: String,
    parameters: List[Token],
    replacement: List[Token],
    long: Boolean,
    outer: Boolean) extends ControlSequence

final case class TeXCsAlias(
    name: String,
    tokens: Token) extends ControlSequence

final case class TeXCharAlias(
    name: String,
    replacement: CharacterToken) extends ControlSequence

final case class TeXTokenList(
    name: String,
    number: Byte) extends ControlSequence

final case class TeXFont(
    name: String,
    fname: String,
    magnification: Option[Either[Dimension, Double]]) extends ControlSequence
