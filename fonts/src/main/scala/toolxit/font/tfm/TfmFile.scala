/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package font
package tfm

import dimen._

import enumeratum.values._

sealed abstract class Tag(val value: Byte) extends ByteEnumEntry
object Tag extends ByteEnum[Tag] {
  val values = findValues

  case object NoTag extends Tag(0)
  case object LigTag extends Tag(1)
  case object ListTag extends Tag(2)
  case object ExtTag extends Tag(3)
}

case class CharInfoWord(
    widthIndex: Int,
    heightIndex: Int,
    depthIndex: Int,
    italicIndex: Int,
    tag: Tag,
    remainder: Int)

case class TfmFontMetrics(
    checkSum: Int,
    designSize: Dimension,
    codingScheme: Option[String],
    fontIdentifier: Option[String],
    face: Option[Either[String, Byte]],
    firstChar: Int,
    charInfo: Vector[CharInfoWord],
    width: Vector[Double],
    height: Vector[Double],
    depth: Vector[Double],
    italic: Vector[Double],
    ligKern: Vector[Int],
    kern: Vector[Double],
    exten: Vector[Int],
    params: Vector[Double]) extends FontMetrics {

  def slantPerPoint: Dimension =
    params(0).pt

  def interwordSpace: Dimension =
    params(1) * designSize

  def interwordStretch: Dimension =
    params(2) * designSize

  def interwordShrink: Dimension =
    params(3) * designSize

  def xHeight: Dimension =
    params(4) * designSize

  def quadWidth: Dimension =
    params(5) * designSize

  def extraSpace: Dimension =
    params(6) * designSize

  def num1: Dimension =
    params(7) * designSize

  def num2: Dimension =
    params(8) * designSize

  def num3: Dimension =
    params(9) * designSize

  def denom1: Dimension =
    params(10) * designSize

  def denom2: Dimension =
    params(11) * designSize

  def sup1: Dimension =
    params(12) * designSize

  def sup2: Dimension =
    params(13) * designSize

  def sup3: Dimension =
    params(14) * designSize

  def sub1: Dimension =
    params(15) * designSize

  def sub2: Dimension =
    params(16) * designSize

  def supdrop: Dimension =
    params(17) * designSize

  def subdrop: Dimension =
    params(18) * designSize

  def delim1: Dimension =
    params(19) * designSize

  def delim2: Dimension =
    params(20) * designSize

  def axisHeight: Dimension =
    params(21) * designSize

  def defaultRuleThickness: Dimension =
    params(7) * designSize

  def bigopSpacing1: Dimension =
    params(8) * designSize

  def bigopSpacing2: Dimension =
    params(9) * designSize

  def bigopSpacing3: Dimension =
    params(10) * designSize

  def bigopSpacing4: Dimension =
    params(11) * designSize

  def bigopSpacing5: Dimension =
    params(12) * designSize

  def at(dim: Dimension): FontMetrics =
    if (dim == designSize)
      this
    else
      copy(designSize = dim)

  def apply(param: Int) =
    if (param == 1)
      slantPerPoint
    else
      params.applyOrElse(param - 1, (_: Int) => 0d) * designSize

  def updated(param: Int, value: Dimension): FontMetrics =
    if (param == 1) {
      if (value == slantPerPoint)
        this
      else
        copy(params = params.updated(0, value.pts))
    } else {
      val p = if (param <= params.size) params(param - 1) else 0
      if (p * designSize == value) {
        this
      } else {
        val newParams =
          if (param < params.size)
            params.updated(param - 1, value.pts)
          else
            params ++ Vector.fill(param - params.size)(0.0d).updated(param - params.size, value.pts)
        copy(params = newParams)
      }
    }

}
