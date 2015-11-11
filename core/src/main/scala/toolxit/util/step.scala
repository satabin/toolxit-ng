/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BAStep.IS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package util

import scala.language.higherKinds

sealed abstract class Step[In, Monad[+_]: Monadic, +Out]

object Step {
  final case class Done[Out, Monad[+_]: Monadic, In](a: Out, remaining: Input[In]) extends Step[In, Monad, Out]
  final case class Cont[In, Monad[+_]: Monadic, +Out](k: Input[In] => Iteratee[In, Monad, Out]) extends Step[In, Monad, Out]
  final case class Error[In, Monad[+_]: Monadic](t: Throwable, input: Input[In]) extends Step[In, Monad, Nothing]
}
