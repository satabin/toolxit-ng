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
import dimen._
import font._

trait TeXFonts {
  this: TeXMouth =>

  lazy val font: Processor[(String, Option[Either[Dimension, Double]])] =
    for {
      t <- read
      f <- t match {
        case FontdefToken(fn, mag) =>
          for (() <- swallow)
            yield (fn, mag)
        case Primitives.Font("font") =>
          for (() <- swallow)
            yield env.font().get
        case t @ Primitives.Font("textfont") =>
          for {
            () <- swallow
            n <- catNumber(t.pos)
          } yield env.textfont(n).get
        case Primitives.Font("scriptfont") =>
          for {
            () <- swallow
            n <- catNumber(t.pos)
          } yield env.scriptfont(n).get
        case Primitives.Font("scriptscriptfont") =>
          for {
            () <- swallow
            n <- catNumber(t.pos)
          } yield env.scriptscriptfont(n).get
        case _ =>
          throwError[Token](new TeXMouthException("Font expected", t.pos))
      }
    } yield f

}

