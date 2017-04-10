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

import glue._

trait TeXGlues {
  this: TeXMouth =>

  lazy val internalGlue: Processor[Glue] = ???

  lazy val internalMuglue: Processor[Muglue] = ???

  val plus = keyword("plus", true)
  val minus = keyword("minus", true)

  val stretch: Processor[Amount] =
    for {
      () <- spaces
      p <- plus
      s <- if (p) ??? else done(ZeroAmount)
    } yield s

  val shrink: Processor[Amount] =
    for {
      () <- spaces
      m <- minus
      s <- if (m) ??? else done(ZeroAmount)
    } yield s

  val glue: Processor[Glue] =
    for {
      sign <- signs()
      t <- read
      g <- t match {
        case StartsInternalGlue() => internalGlue.map(sign * _)
        case _ =>
          for {
            va <- dimen
            st <- stretch
            sh <- shrink
          } yield Glue(sign * va, st, sh)
      }
    } yield ???

  // extractors

  object StartsInternalGlue {
    def unapply(token: Token): Boolean =
      false
  }

  object StartsInternalMuglue {
    def unapply(token: Token): Boolean =
      false
  }

}
