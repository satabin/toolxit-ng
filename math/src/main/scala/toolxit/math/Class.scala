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
package math

import enumeratum.values._

sealed abstract class Class(val value: Byte) extends ByteEnumEntry
object Class extends ByteEnum[Class] {

  def values = findValues

  case object Ordinary extends Class(0)
  case object LargeOperator extends Class(1)
  case object BinaryOperator extends Class(2)
  case object Relation extends Class(3)
  case object Opening extends Class(4)
  case object Closing extends Class(5)
  case object Punctuation extends Class(6)
  case object Variable extends Class(7)

}
