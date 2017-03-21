/*
* Copyright (c) 2015 Lucas Satabin
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

trait TeXParameters {
  this: TeXMouth =>

  def integerParameter: Processor[ControlSequenceToken] =
    read.flatMap {
      case cs @ ControlSequenceToken(name, false) if Primitives.integerParameter.contains(name) =>
        done(cs)
      case tok =>
        throwError(new TeXMouthException(s"Integer parameter expected but $tok found", tok.pos))
    }

  def dimenParameter: Processor[ControlSequenceToken] =
    read.flatMap {
      case cs @ ControlSequenceToken(name, false) if Primitives.dimenParameter.contains(name) =>
        done(cs)
      case tok =>
        throwError(new TeXMouthException(s"Dimension parameter expected but $tok found", tok.pos))
    }

  def glueParameter: Processor[ControlSequenceToken] =
    read.flatMap {
      case cs @ ControlSequenceToken(name, false) if Primitives.glueParameter.contains(name) =>
        done(cs)
      case tok =>
        throwError(new TeXMouthException(s"Glue parameter expected but $tok found", tok.pos))
    }

  def muglueParameter: Processor[ControlSequenceToken] =
    read.flatMap {
      case cs @ ControlSequenceToken(name, false) if Primitives.muglueParameter.contains(name) =>
        done(cs)
      case tok =>
        throwError(new TeXMouthException(s"Muglue parameter expected but $tok found", tok.pos))
    }

  def tokenParameter: Processor[ControlSequenceToken] =
    read.flatMap {
      case cs @ ControlSequenceToken(name, false) if Primitives.tokenParameter.contains(name) =>
        done(cs)
      case tok =>
        throwError(new TeXMouthException(s"Token parameter expected but $tok found", tok.pos))
    }

}
