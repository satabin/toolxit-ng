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

import enumeratum._

/** TeX basically has very few types:
 *   - signed integer,
 *   - character,
 *   - math character,
 *   - dimension,
 *   - glue,
 *   - muglue,
 *   - macro (defined by the user),
 *   - alias (defined by `\let` or `\futurelet`),
 *   - token lists,
 *   - font.
 *
 *  @author Lucas Satabin
 *
 */
sealed trait TeXType extends EnumEntry
object TeXType extends Enum[TeXType] {
  val values = findValues

  object TeXCounter extends TeXType
  object TeXChar extends TeXType
  object TeXMathChar extends TeXType
  object TeXDimension extends TeXType
  object TeXGlue extends TeXType
  object TeXMuglue extends TeXType
  object TeXMacro extends TeXType
  object TeXAlias extends TeXType
  object TeXTokenList extends TeXType
  object TeXFont extends TeXType
}

sealed trait ControlSequence {
  val name: String
  val tpe: TeXType
}

final case class TeXChar(name: String,
    char: Char) extends ControlSequence {
  val tpe = TeXType.TeXChar
}

final case class TeXCounter(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXCounter
}

final case class TeXMathChar(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXMathChar
}

final case class TeXDimension(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXDimension
}

final case class TeXGlue(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXGlue
}

final case class TeXMuglue(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXMuglue
}

/** This control sequence represents a macro with a parameter sequence and a replacement text.
 *  It is expected for the replacement text to be a list of the body tokens in reverse order.
 *  This may seem odd at first sight, but it relies on the fact that these tokens will be
 *  pushed back onto a stack, and that the efficient implementation pushes a sequence in
 *  its order, so that the last elements of the sequence will be the on top of the stack
 */
final case class TeXMacro(name: String,
    parameters: List[Token],
    replacement: List[Token],
    long: Boolean,
    outer: Boolean) extends ControlSequence {
  val tpe = TeXType.TeXMacro
}

final case class TeXCsAlias(name: String,
    tokens: Token) extends ControlSequence {
  val tpe = TeXType.TeXAlias
}

final case class TeXCharAlias(name: String,
    replacement: CharacterToken) extends ControlSequence {
  val tpe = TeXType.TeXAlias
}

final case class TeXTokenList(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXTokenList
}

final case class TeXFont(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXFont
}
