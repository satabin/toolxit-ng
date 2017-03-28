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

package object toolxit {

  import dimen._
  import glue._

  type Parameter = Either[ParameterToken, List[Token]]

  implicit class IntOps(val i: Int) extends AnyVal {
    @inline
    def *(dim: Dimension): Dimension = dim.copy(sps = (dim.sps * i))

    @inline
    def *(glue: Glue): Glue = Glue(*(glue.value), *(glue.stretch), *(glue.shrink))

    @inline
    def *(amount: Amount): Amount = amount match {
      case DimenAmount(dim)          => DimenAmount(*(dim))
      case FillAmount(factor, level) => FillAmount(i * factor, level)
    }

  }

  implicit class DoubleOps(val d: Double) extends AnyVal {
    @inline
    def *(dim: Dimension): Dimension = dim.copy(sps = (dim.sps * d).toInt)

    @inline
    def *(glue: Glue): Glue = Glue(*(glue.value), *(glue.stretch), *(glue.shrink))

    @inline
    def *(amount: Amount): Amount = amount match {
      case DimenAmount(dim)          => DimenAmount(*(dim))
      case FillAmount(factor, level) => FillAmount(d * factor, level)
    }

  }

}
