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
package font

import dimen._

/** Special font with no character in it. */
case object NullFont extends FontMetrics {

  val designSize: Dimension = ZeroDimen

  val axisHeight: Dimension = ZeroDimen
  val bigopSpacing1: Dimension = ZeroDimen
  val bigopSpacing2: Dimension = ZeroDimen
  val bigopSpacing3: Dimension = ZeroDimen
  val bigopSpacing4: Dimension = ZeroDimen
  val bigopSpacing5: Dimension = ZeroDimen
  val defaultRuleThickness: Dimension = ZeroDimen
  val delim1: Dimension = ZeroDimen
  val delim2: Dimension = ZeroDimen
  val denom1: Dimension = ZeroDimen
  val denom2: Dimension = ZeroDimen
  val extraSpace: Dimension = ZeroDimen
  val interwordShrink: Dimension = ZeroDimen
  val interwordSpace: Dimension = ZeroDimen
  val interwordStretch: Dimension = ZeroDimen
  val num1: Dimension = ZeroDimen
  val num2: Dimension = ZeroDimen
  val num3: Dimension = ZeroDimen
  val quadWidth: Dimension = ZeroDimen
  val slantPerPoint: Dimension = ZeroDimen
  val sub1: Dimension = ZeroDimen
  val sub2: Dimension = ZeroDimen
  val subdrop: Dimension = ZeroDimen
  val sup1: Dimension = ZeroDimen
  val sup2: Dimension = ZeroDimen
  val sup3: Dimension = ZeroDimen
  val supdrop: Dimension = ZeroDimen
  val xHeight: Dimension = ZeroDimen

  def updated(param: Int, value: Dimension): FontMetrics = this

  def at(dim: Dimension): FontMetrics = this

}
