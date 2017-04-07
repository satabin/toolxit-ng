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

import dimen._

/** When ToolXiT loads a font, it requires some parameters to be available so that
 *  the font can be used.
 *  This trait abstracts over access to these parameters.
 *
 *  @groupdesc Standard parameters present in all fonts
 *  @groupdesc MathSym parameters present in math symbol fonts
 *  @groupdesc MathExt parameters present in math extended fonts
 */
trait FontMetrics {

  def designSize: Dimension

  /** Builds this font at the given design size and returns it. */
  def at(size: Dimension): FontMetrics

  /** @group Standard */
  def slantPerPoint: Dimension

  /** @group Standard */
  def interwordSpace: Dimension

  /** @group Standard */
  def interwordStretch: Dimension

  /** @group Standard */
  def interwordShrink: Dimension

  /** @group Standard */
  def xHeight: Dimension

  /** @group Standard */
  def quadWidth: Dimension

  /** @group Standard */
  def extraSpace: Dimension

  /** @group MathSym */
  def num1: Dimension

  /** @group MathSym */
  def num2: Dimension

  /** @group MathSym */
  def num3: Dimension

  /** @group MathSym */
  def denom1: Dimension

  /** @group MathSym */
  def denom2: Dimension

  /** @group MathSym */
  def sup1: Dimension

  /** @group MathSym */
  def sup2: Dimension

  /** @group MathSym */
  def sup3: Dimension

  /** @group MathSym */
  def sub1: Dimension

  /** @group MathSym */
  def sub2: Dimension

  /** @group MathSym */
  def supdrop: Dimension

  /** @group MathSym */
  def subdrop: Dimension

  /** @group MathSym */
  def delim1: Dimension

  /** @group MathSym */
  def delim2: Dimension

  /** @group MathSym */
  def axisHeight: Dimension

  /** @group MathExt */
  def defaultRuleThickness: Dimension

  /** @group MathExt */
  def bigopSpacing1: Dimension

  /** @group MathExt */
  def bigopSpacing2: Dimension

  /** @group MathExt */
  def bigopSpacing3: Dimension

  /** @group MathExt */
  def bigopSpacing4: Dimension

  /** @group MathExt */
  def bigopSpacing5: Dimension

  /** Updates the given parameter number, extending the parameter array if needed.
   *  The updated font is returned.
   */
  def updated(param: Int, value: Dimension): FontMetrics

}
