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

  type Processor[+T] = Iteratee[Command, T]

  lazy val process: Processor[Unit] = headOption.flatMap {
    case Some(cs @ CS("end")) =>
      throwError(EndException)
    case Some(v) =>
      v match {
        case Typeset(c)        => out.print(c)
        case Par               => out.print("\n\n")
        case Assignment(assgn) => assign(assgn)
        case cs @ CS(name)     => log.println(f"${cs.pos} Undefined control sequence \\$name")
      }
      process
    case None =>
      Done(())
  }

  def assign(assignment: Assignment): Unit = assignment match {
    case CounterAssignment(cnt, v, mode, global)           => env.count(cnt, mode, global) = v
    case IntegerParameterAssignment(name, v, mode, global) => env.integerParameter(name, mode, global) = v
    case CategoryAssignment(char, cat, global)             => env.category(char, global) = cat
  }

}

case object EndException extends Exception
