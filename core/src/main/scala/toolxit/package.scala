/*
* This file is part of the ToolXiT project.
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

import scala.language.implicitConversions

/** @author Lucas Satabin
 *
 */
package object toolxit {

  import dimen._

  type Parameter = Either[ParameterToken, List[Token]]

  implicit def toDimenMult(i: Int) = new {
    def *(dim: Dimension) = dim.copy(sps = (dim.sps * i))
  }

  implicit def toGlueMult(i: Int) = new {
    def *(glue: Glue) = Glue(i * glue.value, i * glue.stretch, i * glue.shrink)
  }

}
