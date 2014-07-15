/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
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

object TeXMouth {

  /** Accepts only the given token, and returns it */
  def accept(state: ReadingState.Value,
    env: TeXEnvironment,
    stream: LineStream,
    token: Token): Try[(Token, ReadingState.Value, TeXEnvironment, LineStream)] =
    TeXEyes.next(state, env, stream) flatMap {
      case (t, newState, newStream) if t == token =>
        Success(t, newState, env, newStream)
      case (t, _, _) =>
        Failure(new TeXParsingException(s"expected $token, but found $t", t.pos))
    }

  /** Returns the next unexpanded token */
  def unexpanded(state: ReadingState.Value,
    env: TeXEnvironment,
    stream: LineStream): Try[(Token, ReadingState.Value, TeXEnvironment, LineStream)] =
      TeXEyes.next(state, env, stream) map {
        case (token, newState, newStream) =>
          (token, newState, env, newStream)
      }

  /** Returns the next expanded token */
  def expanded(state: ReadingState.Value,
    env: TeXEnvironment,
    stream: LineStream): Try[(Token, ReadingState.Value, TeXEnvironment, LineStream)] =
    TeXEyes.next(state, env, stream) flatMap {
      case (ControlSequenceToken(name, _), newState, newEnv) =>
        env.css(name) match {
          case Some(TeXMacro(_, parameters, replacement, long)) =>
            ???
          case Some(TeXPrimitive(_)) =>
            ???
          case None =>
            Failure(new ControlSequenceException(s"undefined control sequence `$name`"))
        }
    }

}
