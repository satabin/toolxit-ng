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
* distributed under the License is distributed on an "AS IS" BAStep.IS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package util

sealed abstract class Stream[+Elt] {
  def ++[Elt1 >: Elt](that: Stream[Elt1]): Stream[Elt1] = (this, that) match {
    case (Eos(None), _)         => that
    case (Eos(_), _)            => this
    case (_, Eos(_))            => this
    case (Chunk(s1), Chunk(s2)) => Chunk(s1 ++ s2)
  }

  def isEmpty: Boolean
  def size: Int
  def toSeq: Seq[Elt]
}

final case class Eos(exn: Option[Exception]) extends Stream[Nothing] {
  val isEmpty = true
  val size = 0
  val toSeq = Nil
}

final case class Chunk[Elt](elts: Seq[Elt]) extends Stream[Elt] {
  val isEmpty = elts.isEmpty
  val size = elts.size
  val toSeq = elts
}
