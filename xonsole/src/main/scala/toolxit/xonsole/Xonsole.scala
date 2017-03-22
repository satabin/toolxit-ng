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

import util._
import eyes._
import mouth._
import stomach._

import org.jline.terminal.TerminalBuilder
import org.jline.reader._

import java.nio.file.Paths
import java.io.{
  StringReader,
  PrintWriter,
  FileOutputStream
}

import scala.util.Properties

class Xonsole {

  def open(): Unit = {
    val terminal = TerminalBuilder.builder().build()

    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .build()
    // TeX makes heavy use of \ which should not be expanded as event
    reader.setOpt(LineReader.Option.DISABLE_EVENT_EXPANSION)

    reader.setVariable(LineReader.HISTORY_FILE, Paths.get(Properties.userHome, ".xonsole_history"));

    val environment = new TeXEnvironment("xonsole")

    val eyes = new TeXEyes(environment)

    val mouth = new TeXMouth(environment)

    val out = new PrintWriter(new FileOutputStream(f"xonsole.txt"))

    val stomach = new TeXStomach(environment, terminal.writer, out)

    val enumerator = Enumerator.fromEnv[List[Token]](environment, false)

    //val init =
    //  Enumeratees.join(
    //    Enumeratees.join(Enumeratees.sequence(eyes.tokenize)(Enumeratees.sequence(mouth.command)(stomach.process))))
    val its = new Iteratees[(Char, Int, Int)] {}
    val init = its.fold[Token, List[Token]](eyes.tokenize)(Nil)(_ :+ _)

    terminal.writer.println("This is Xonsole, Version 0.0.1")

    var it = init

    try {
      while (true) {
        try {

          val line = reader.readLine("*")

          if (line == null || line == "") {
            terminal.writer.println("(Please type a command or say `\\end')")
          } else {

            val r = new util.LineReader(new StringReader(line + "\n\n"))

            environment.inputs.push(r)

            it = enumerator(it)

            it match {
              case Done(ts, _) =>
                println(ts)
                it = init
              case Error(EndException, _) =>
                throw new EndOfFileException
              case Error(TeXMouthException(msg, pos), _) =>
                terminal.writer.println(f"$pos $msg")
                it = init
              case Error(t, _) =>
                t.printStackTrace
                it = init
              case i @ Cont(_) =>
                it = i
            }
          }

        } catch {
          case _: UserInterruptException =>
          // Ignore
          case _: EndOfFileException =>
            return
        }
      }
    } finally {
      terminal.close()
      out.close()
    }
  }

}
