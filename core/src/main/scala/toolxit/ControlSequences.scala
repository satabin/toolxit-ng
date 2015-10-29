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
object TeXType extends Enumeration {
  val TeXInteger = Value
  val TeXChar = Value
  val TeXMathChar = Value
  val TeXDimension = Value
  val TeXGlue = Value
  val TeXMuglue = Value
  val TeXMacro = Value
  val TeXAlias = Value
  val TeXTokenList = Value
  val TeXFont = Value
  val TeXPrimitive = Value
}

sealed trait ControlSequence {
  val name: String
  val tpe: TeXType.Value
}

final case class TeXInteger(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXInteger
}

final case class TeXChar(name: String,
    number: Byte) extends ControlSequence {
  val tpe = TeXType.TeXChar
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
 *  its order, so that the last elements of the sequence will be the on top of the stack */
final case class TeXMacro(name: String,
    parameters: List[Token],
    replacement: List[Token],
    long: Boolean,
    outer: Boolean) extends ControlSequence {
  val tpe = TeXType.TeXMacro
}

final case class TeXCsAlias(name: String,
    replacement: TeXMacro) extends ControlSequence {
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

final case class TeXPrimitive(name: String) extends ControlSequence {
  val tpe = TeXType.TeXPrimitive
}
