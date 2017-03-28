/*
* Copyright (c) 2015 Lucas Satabin
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
package glue

import dimen._

/** @author Lucas Satabin
 *
 */
case class Glue(value: Dimension,
    stretch: Amount,
    shrink: Amount) {

  def +(that: Glue): Glue =
    Glue(this.value + that.value, this.stretch + that.stretch, this.shrink + that.shrink)

}

/** Special glue, which is zero */
object ZeroGlue extends Glue(ZeroDimen, ZeroAmount, ZeroAmount)

sealed abstract class Amount {
  def +(that: Amount): Amount = (this, that) match {
    case (DimenAmount(d1), DimenAmount(d2)) => DimenAmount(d1 + d2)
    case (DimenAmount(_), _)                => that
    case (_, DimenAmount(_))                => this
    case (FillAmount(f1, lvl1), FillAmount(f2, lvl2)) =>
      if (lvl1 < lvl2)
        that
      else if (lvl1 > lvl2)
        this
      else
        FillAmount(f1 + f2, lvl1)
  }
}
case class DimenAmount(d: Dimension) extends Amount
case class FillAmount(factor: Double, level: Int) extends Amount
object ZeroAmount extends DimenAmount(ZeroDimen)

case class Muglue(value: Int,
  stretch: Int = 0,
  shrink: Int = 0)

/** Special muglue, which is zero */
object ZeroMuglue extends Muglue(0)
