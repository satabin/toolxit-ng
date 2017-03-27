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

/** @author Lucas Satabin
 *
 */
package object dimen {

  /** Special dimension, which is zero */
  val ZeroDimen = Dimension(0)

  implicit class IntDimen(val i: Int) extends AnyVal {

    def sp = Dimension(i)

    def pt = Dimension.ofPoint(i)

    def pc = Dimension.ofPica(i)

    def in = Dimension.ofInch(i)

    def bp = Dimension.ofBigPoint(i)

    def cm = Dimension.ofCentimeter(i)

    def mm = Dimension.ofMillimeter(i)

    def dd = Dimension.ofDidotPoint(i)

    def cc = Dimension.ofCicero(i)

  }

  implicit class DoubleDimen(val f: Double) extends AnyVal {

    def sp = Dimension(f.toInt)

    def pt = Dimension.ofPoint(f)

    def pc = Dimension.ofPica(f)

    def in = Dimension.ofInch(f)

    def bp = Dimension.ofBigPoint(f)

    def cm = Dimension.ofCentimeter(f)

    def mm = Dimension.ofMillimeter(f)

    def dd = Dimension.ofDidotPoint(f)

    def cc = Dimension.ofCicero(f)

  }

  implicit class IntDimenOps(val i: Int) extends AnyVal {
    @inline
    def *(dim: Dimension) = dim.copy(sps = (dim.sps * i))
  }

  implicit class IntGlueOps(val i: Int) extends AnyVal {
    @inline
    def *(glue: Glue) = Glue(i * glue.value, i * glue.stretch, i * glue.shrink)
  }

  implicit class DoubleDimenOps(val d: Double) extends AnyVal {
    @inline
    def *(dim: Dimension) = dim.copy(sps = (dim.sps * d).toInt)
  }

  implicit class DoubleGlueOps(val d: Double) extends AnyVal {
    @inline
    def *(glue: Glue) = Glue((d * glue.value).toInt, (d * glue.stretch).toInt, (d * glue.shrink).toInt)
  }

}
