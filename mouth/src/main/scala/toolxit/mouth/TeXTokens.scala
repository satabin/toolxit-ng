/*
* Copyright (c) 2017 Lucas Satabin
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

import util._

import scala.util.Try

trait TeXTokens {
  this: TeXMouth =>

  val tokenVariable: Processor[List[Token]] =
    for {
      t <- read
      v <- t match {
        case Primitives.TokenParameter(name) =>
          for (() <- swallow)
            yield env.tokenParameter(name)
        case ToksdefToken(number) =>
          for (() <- swallow)
            yield env.toks(number)
        case Primitive("toks") =>
          for {
            () <- swallow
            n <- bit8(t.pos)
          } yield env.toks(n)
        case _ =>
          throwError[Token](new TeXMouthException("Expected token variable.", t.pos))
      }
    } yield v

  object StartsTokenVariable {
    def unapply(token: Token): Boolean = token match {
      case Primitives.TokenParameter(_) => true
      case ToksdefToken(_)              => true
      case Primitive("toks")            => true
      case _                            => false
    }
  }

}
