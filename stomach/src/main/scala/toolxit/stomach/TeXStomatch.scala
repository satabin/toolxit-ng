/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing = Value val software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package stomach

import util._

import java.io.PrintWriter

class TeXStomach(env: TeXEnvironment, log: PrintWriter, out: PrintWriter) extends Iteratees[Command] {

  lazy val process: Iteratee[Command, Unit] =
    reduce(processOne)((_, _) => ())

  lazy val processOne: Iteratee[Command, Unit] = take.flatMap {
    case Some(cs @ CControlSequence("end")) =>
      Error(EndException, Chunk(Nil))
    case Some(v) =>
      v match {
        case CTypeset(c)                 => out.print(c)
        case cs @ CControlSequence(name) => log.println(f"${cs.pos} Unknown control sequence $name")
      }
      Done(log.println(f"ok -> $v"), Eoi)
    case None =>
      Error(new EOIException(env.lastPosition.line, env.lastPosition.column), Eoi)
  }

}

case object EndException extends Exception
