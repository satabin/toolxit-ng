/*
* Copyright (c) 2015 Lucas Satabin
* Based on code from https://github.com/djspiewak/gll-combinators
* by Daniel Spiewak
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
package toolxit.util

object Enumeratees {

  def sequence[EltOuter, EltInner, Out](fi: Iteratee[EltOuter, EltInner]): Enumeratee[EltOuter, EltInner, Out] = {
    case i @ Cont(k) =>
      lazy val finished: Iteratee[EltOuter, Boolean] = Cont {
        case Chunk(Nil)       => finished
        case chunk @ Chunk(_) => Done(false, chunk)
        case Eoi              => Done(true, Eoi)
      }
      finished.flatMap {
        case true =>
          Error(new Exception("Not enough data"), Eoi)
        case false =>
          fi.flatMap { v => Done(k(Chunk(List(v))), Chunk(Nil)).flatMap(sequence(fi)) }
      }
    case i =>
      Done(i, Chunk(Nil))
  }

  def join[EltOuter, EltInner, Out](outer: Iteratee[EltOuter, Iteratee[EltInner, Out]]): Iteratee[EltOuter, Out] = {
    def check(inner: Iteratee[EltInner, Out]): Iteratee[EltOuter, Out] =
      inner match {
        case Cont(k)     => Done(k(Eoi), Chunk(Nil)).flatMap(check)
        case Done(v, r)  => Done(v, Chunk(Nil))
        case Error(e, r) => Error(e, Chunk(Nil))
      }
    outer.flatMap(check)
  }

}
