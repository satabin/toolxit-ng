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

import scala.annotation.tailrec

object Enumerator {

  private class TeXEnvironmentEnumerator[Res](env: TeXEnvironment, sendEoi: Boolean) extends Iteratees[(Char, Int, Int)] with Enumerator[(Char, Int, Int), Res] {

    private def nextChar(k: Input[(Char, Int, Int)] => Iteratee[(Char, Int, Int), Res]): (Iteratee[(Char, Int, Int), Res], Boolean) =
      try {
        env.inputs.headOption match {
          case Some(input) =>
            if (env.endOfLineEncountered) {
              // don't care about the rest of the line, just go ahead
              input.nextLine()
              // and of course reset this state
              env.endOfLineEncountered = false
            }
            input.nextChar() match {
              case Some(c) =>
                if (env.endinputEncountered && c._1 == '\n') {
                  // we must close the input
                  input.close()
                  env.inputs.pop
                }
                k(Chunk(List(c))) -> true
              case None =>
                // the input is exhausted go back to the old one
                // after having closed the current one
                input.close()
                env.inputs.pop
                // and re-read
                nextChar(k)
            }
          case None =>
            k(Eoi) -> false
        }
      } catch {
        case e: Exception =>
          throwError(new TeXException("Something wrong happened", e)) -> false
      }

    @tailrec
    final def apply(step: Iteratee[(Char, Int, Int), Res]): Iteratee[(Char, Int, Int), Res] = step match {
      case Cont(k) =>
        val (it, cont) = nextChar(k)
        if (cont)
          apply(it)
        else
          it
      case _ =>
        step
    }

  }

  def fromEnv[Res](env: TeXEnvironment, sendEoi: Boolean = true): Enumerator[(Char, Int, Int), Res] =
    new TeXEnvironmentEnumerator(env, sendEoi)

}
