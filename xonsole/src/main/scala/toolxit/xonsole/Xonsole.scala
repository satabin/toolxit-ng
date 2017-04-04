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
import org.jline.reader.impl.DefaultParser

import java.nio.file.Paths
import java.io.{
  StringReader,
  PrintWriter,
  FileOutputStream,
  InputStreamReader,
  LineNumberReader
}

import scala.util.{
  Properties,
  Try,
  Success,
  Failure
}

import scala.annotation.tailrec

class Xonsole {

  def open(format: Option[String] = Some("minimal")): Unit = {

    val environment = new TeXEnvironment("xonsole")

    val terminal = TerminalBuilder.builder().build()

    val out = new PrintWriter(new FileOutputStream(f"xonsole.txt"))

    try {
      // \ is not an escape character
      val parser = new DefaultParser
      parser.setEscapeChars(Array.empty[Char])

      val reader = LineReaderBuilder.builder()
        .terminal(terminal)
        .parser(parser)
        .completer(new TeXCompleter(environment))
        .build()
      // TeX makes heavy use of \ which should not be expanded as event
      reader.setOpt(LineReader.Option.DISABLE_EVENT_EXPANSION)

      reader.setVariable(LineReader.HISTORY_FILE, Paths.get(Properties.userHome, ".xonsole_history"));

      val eyes = new TeXEyes(environment)

      val mouth = new TeXMouth(environment)

      val stomach = new TeXStomach(environment, out)

      val it =
        Enumeratees.join(
          Enumeratees.sequenceStream(eyes.tokenize)(Enumeratees.join(Enumeratees.sequenceStream(mouth.command)(stomach.process))))

      for (f <- format) {
        environment.pushInput(new LineNumberReader(new InputStreamReader(getClass.getResourceAsStream(f"/$f.tex"))), None)
        val enumerator = Enumerator.env[Unit](environment)
        val i = enumerator(it).flatMap(run(4, _))
        i match {
          case Success(())              =>
          // format loaded
          case Failure(EOIException(_)) =>
          // format loaded
          case Failure(TeXException(msg, pos)) =>
            terminal.writer.println(f"$pos $msg")
          case Failure(t) =>
            t.printStackTrace
        }
      }

      terminal.writer.println(f"This is Xonsole, Version 0.0.1${format.fold("")(f => f" (preloaded format=$f)")}")

      while (true) {
        try {

          val line = reader.readLine("*")

          if (line == null || line == "") {
            terminal.writer.println("(Please type a command or say `\\end')")
          } else {

            environment.pushInput(new LineNumberReader(new StringReader(line.replaceAll("\\s*$", "\n"))), None)
            val enumerator = Enumerator.env[Unit](environment)

            // we give a credit of 4 retries because of the peeking of 4 characters
            // to expand escaped characters
            val res = enumerator(it).flatMap(run(4, _))

            res match {
              case Success(())              =>
              // next line
              case Failure(EOIException(_)) =>
              // next line
              case Failure(EndException) =>
                throw new EndOfFileException
              case Failure(TeXException(msg, pos)) =>
                terminal.writer.print(" " * pos.column)
                terminal.writer.println("^")
                terminal.writer.println(f"$pos $msg")
              case Failure(t) =>
                t.printStackTrace
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
