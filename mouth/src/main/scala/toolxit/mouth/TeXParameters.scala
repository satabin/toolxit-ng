/*
* This file is part of the ToolXiT project.
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

import scala.util.{
  Try,
  Failure,
  Success
}

trait TeXParameters {
  this: TeXMouth =>

  def parseIntegerParameter(): Try[ControlSequenceToken] =
    read() match {
      case Success(cs @ ControlSequenceToken(name, false)) if Primitives.integerParameter.contains(name) =>
        Success(cs)
      case Success(tok) =>
        Failure(new TeXParsingException(s"Integer parameter expected but $tok found", tok.pos))
      case Failure(t) =>
        Failure(t)
    }

  def parseDimenParameter(): Try[ControlSequenceToken] =
    read() match {
      case Success(cs @ ControlSequenceToken(name, false)) if Primitives.dimenParameter.contains(name) =>
        Success(cs)
      case Success(tok) =>
        Failure(new TeXParsingException(s"Dimension parameter expected but $tok found", tok.pos))
      case Failure(t) =>
        Failure(t)
    }

  def parseGlueParameter(): Try[ControlSequenceToken] =
    read() match {
      case Success(cs @ ControlSequenceToken(name, false)) if Primitives.glueParameter.contains(name) =>
        Success(cs)
      case Success(tok) =>
        Failure(new TeXParsingException(s"Glue parameter expected but $tok found", tok.pos))
      case Failure(t) =>
        Failure(t)
    }

  def parseMuglueParameter(): Try[ControlSequenceToken] =
    read() match {
      case Success(cs @ ControlSequenceToken(name, false)) if Primitives.muglueParameter.contains(name) =>
        Success(cs)
      case Success(tok) =>
        Failure(new TeXParsingException(s"Muglue parameter expected but $tok found", tok.pos))
      case Failure(t) =>
        Failure(t)
    }

  def parseTokenParameter(): Try[ControlSequenceToken] =
    read() match {
      case Success(cs @ ControlSequenceToken(name, false)) if Primitives.tokenParameter.contains(name) =>
        Success(cs)
      case Success(tok) =>
        Failure(new TeXParsingException(s"Token parameter expected but $tok found", tok.pos))
      case Failure(t) =>
        Failure(t)
    }

}
