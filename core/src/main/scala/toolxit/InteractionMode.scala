/*
* Copyright (c) 2017 Lucas Satabin
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

import enumeratum._
import enumeratum.EnumEntry._

sealed trait InteractionMode extends EnumEntry with Lowercase
object InteractionMode extends Enum[InteractionMode] {
  val values = findValues

  case object ErrorStopMode extends InteractionMode
  case object ScrollMode extends InteractionMode
  case object NonStopMode extends InteractionMode
  case object BatchMode extends InteractionMode
}
