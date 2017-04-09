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
*
*/
package toolxit
package util

/** A non-empty list data strucutre. */
sealed abstract class List1[+T] {
  def head: T

  def ::[U >: T](u: U): List1[U] =
    More(u, this)
}
object List1 {
  def apply[T](fst: T, rest: T*): List1[T] =
    rest match {
      case Seq()          => One(fst)
      case Seq(h, t @ _*) => More(fst, apply(h, t: _*))
    }
}

/** The base case with one single element. */
final case class One[T](head: T) extends List1[T]

/** More than one element. */
final case class More[T](head: T, tail: List1[T]) extends List1[T]
