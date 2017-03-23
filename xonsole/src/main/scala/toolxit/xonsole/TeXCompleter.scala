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
package xonsole

import java.util.{ List => JList }

import scala.collection.JavaConverters._

import org.jline.reader._

class TeXCompleter(env: TeXEnvironment) extends Completer {

  import TeXCompleter._

  def complete(reader: LineReader, line: ParsedLine, candidates: JList[Candidate]): Unit = {
    line.word() match {
      case CsRegex(name) =>
        val potential =
          // macros etc
          env.css.names.filter(_.startsWith(name)).map(ProposedCs(_)("user defined")) ++
            // and fallback in primitives
            Primitives.all.filter(_.startsWith(name)).map(ProposedCs(_)("primitive"))
        candidates.addAll(potential.map(p => new Candidate("\\" + p.name, "\\" + p.name, null, p.group, null, null, true)).asJava)
      case _ =>
      // no completion for the rest
    }
  }

}

object TeXCompleter {

  val CsRegex = """\\(\w*)""".r

}

private case class ProposedCs(name: String)(val group: String)
