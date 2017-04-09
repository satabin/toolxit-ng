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
package box

import dimen._

sealed trait Box

case class BoxRegister(number: Byte, copy: Boolean) extends Box

case object LastBox extends Box

case class VSplit(number: Byte, height: Dimension) extends Box

case class HBox() extends Box

case class VBox(contents: List[Command]) extends Box

case class VTop(contents: List[Command]) extends Box

sealed trait Specification

case class To(dimen: Dimension) extends Specification
case class Spread(dimen: Option[Dimension]) extends Specification
