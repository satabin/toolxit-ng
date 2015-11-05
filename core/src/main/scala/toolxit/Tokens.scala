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

import util.Positional

import gnieh.pp._

/** TeX works with a token stream */
sealed trait Token extends Positional {
  def debug: Doc
  def toString(env: TeXEnvironment): String
}

/** A character token read as input. It may be one of the following tokens:
 *   - escape character (by default `\`)
 *   - beginning of group (by default `{`)
 *   - end of group (by default `}`)
 *   - math shift (by default `$`)
 *   - alignment tab (by default `&`)
 *   - end of line (by default `\n`)
 *   - parameter (by default `#`)
 *   - superscript (by default `^`)
 *   - subscript (by default `_`)
 *   - ignored character (for example `null`)
 *   - space (such as ` `)
 *   - a letter (by default a UTF-8 encoded character)
 *   - active character (by default `~`)
 *   - comment character (by default `%`)
 *   - invalid character (<delete>)
 *   - other character (none of the above)
 *
 *  @author Lucas Satabin
 *
 */
case class CharacterToken(value: Char, category: Category.Value) extends Token {
  lazy val debug = "Character(" :: value :: ")[" :: Category.debug(category) :: "]"
  def toString(env: TeXEnvironment) = value.toString
}

/** A control sequence token has not category.
 *
 *  @author Lucas Satabin
 */
case class ControlSequenceToken(name: String, active: Boolean = false) extends Token {
  lazy val debug = "ControlSequence(" :: name :: ")"
  def toString(env: TeXEnvironment) = f"${env.escapechar}$name"
}

/** A parameter token may only occur in the parameter or replacement text
 *  of a control sequence.
 *
 *  @author Lucas Satabin
 */
case class ParameterToken(number: Int) extends Token {
  lazy val debug = "Parameter(" :: number :: ")"
  def toString(env: TeXEnvironment) = f"#$number"
}

/** A bunch of token nested between a token of category BEGINNING_OF_GROUP and
 *  a token of category END_OF_GROUP
 *
 *  @author Lucas Satabin
 */
case class GroupToken(open: Token, inner: List[Token], close: Token) extends Token {
  lazy val debug = group {
    nest(1) {
      "Group(" :: inner.foldRight(empty) { (token, acc) =>
        token.debug :: "," :|: acc
      }
    } :: ")"
  }
  def toString(env: TeXEnvironment) = f"$open${inner.reverseMap(_.toString(env)).mkString}"
}
