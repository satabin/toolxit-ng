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

object Enumerator {

  class StreamEnumerator[Res](_stream: LineStream) extends Enumerator[(Char, Int, Int), Res] {
    private var stream = _stream
    def apply(step: Iteratee[(Char, Int, Int), Res]): Iteratee[(Char, Int, Int), Res] = step match {
      case Cont(k) =>
        if(stream.isEmpty) {
          k(Eoi)
        } else {
          val hd = stream.head
          val line = stream.lineNum
          val col = stream.colNum
          stream = stream.tail
          k(Chunk(List((hd, line, col))))
        }
      case Done(v, rest) =>
        Done(v, rest)
      case Error(t, rest) =>
        Error(t, rest)
    }
  }

  def fromLineStream[Res](stream: LineStream): Enumerator[(Char, Int, Int), Res] =
    new StreamEnumerator(stream)

}
