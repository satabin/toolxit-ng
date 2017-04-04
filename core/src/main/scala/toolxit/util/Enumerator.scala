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

import scala.annotation.tailrec

import scala.util.{
  Try,
  Success,
  Failure
}

import java.io.{
  File,
  Reader,
  FileReader,
  BufferedReader,
  LineNumberReader,
  InputStreamReader
}

/** Set of standard enumerators. */
object Enumerator {

  @inline
  private def feedI[Elt, A](k: K[Elt, A], s: Stream[Elt]): Try[Iteratee[Elt, A]] =
    k(s).flatMap { case (it, _) => Try(it) }

  /** Feeds an end-of-stream to the iteratee. */
  def eos[Elt, A]: Enumerator[Elt, A] = {
    case Cont(None, k) => feedI(k, Eos(None)).flatMap {
      case i @ Done(_)      => Try(i)
      case Cont(None, _)    => Try(throwError(exnDivergent))
      case Cont(Some(e), _) => Try(throwError(e))
    }
    case i => Try(i)
  }

  /** Feeds an error to the iteratee. */
  def err[Elt, A](e: Exception): Enumerator[Elt, A] = {
    case Cont(None, k) => feedI(k, Eos(Some(e))).flatMap {
      case i @ Done(_)      => Try(i)
      case Cont(None, _)    => Try(throwError(exnDivergent))
      case Cont(Some(e), _) => Try(throwError(e))
    }
    case i => Try(i)
  }

  /** Feeds a string to the iteratee. */
  def string[A](in: String): Enumerator[Char, A] = {
    case Cont(None, k) => feedI(k, Chunk(in.toVector))
    case i             => Try(i)
  }

  /** Feeds a sequence to the iteratee. */
  def seq[T, A](s: Seq[T]): Enumerator[T, A] = {
    case Cont(None, k) => feedI(k, Chunk(s))
    case i             => Try(i)
  }

  /** Feeds the iteratee with the string content of a reader, line by line.
   *  New lines are represented by the `\n` character and spaces at the end of lines are trimmed.
   */
  def reader[A](reader: Reader, chunkSize: Int = 1024): Enumerator[(Char, Int, Int), A] = {
    case Cont(None, k) =>
      def loop(line: Int, reader: BufferedReader, k: K[(Char, Int, Int), A]): Try[Iteratee[(Char, Int, Int), A]] =
        Try(reader.readLine()) match {
          case Success(null) => feedI(k, Eos(None))
          case Success(s) =>
            feedI(k, Chunk(s.replaceAll("\\s*$", "\n").zipWithIndex.map { case (c, idx) => (c, line, idx + 1) })).flatMap(check(line + 1, reader))
          case Failure(e: Exception) => feedI(k, Eos(Some(e)))
          case Failure(t)            => throw t
        }
      def check(line: Int, reader: BufferedReader)(it: Iteratee[(Char, Int, Int), A]) = it match {
        case Cont(None, k) => loop(line, reader, k)
        case i             => Try(i)
      }
      for {
        r <- loop(1, new BufferedReader(reader, chunkSize), k)
        _ <- Try(Try(reader.close).getOrElse(Unit))
      } yield r
    case i => Try(i)
  }

  /** Feeds the iteratee with the string content of a file, character by character. */
  def textFile[A](file: File, chunkSize: Int = 1024): Enumerator[(Char, Int, Int), A] =
    reader[A](new FileReader(file), chunkSize)

  /** Feeds the iteratee with the string content of a resource, character by character. */
  def resource[A](name: String, chunkSize: Int = 1024): Enumerator[(Char, Int, Int), A] =
    reader(new InputStreamReader(getClass.getResourceAsStream(name)), chunkSize)

  def env[A](env: TeXEnvironment): Enumerator[(Char, Int, Int), A] = {
    case it @ Cont(None, k) =>
      @tailrec
      def loop(k: K[(Char, Int, Int), A]): Try[Iteratee[(Char, Int, Int), A]] =
        env.popInput() match {
          case None =>
            // no input left, end of the story
            feedI(k, Eos(None))
          case Some((reader, None)) =>
            // no line currently read by this reader, process next one
            if (env.endinputEncountered) {
              // actually we encountered an endinput command, close
              // current input and process next open input
              env.endinputEncountered = false
              Try(reader.close()) match {
                case Success(())           => loop(k)
                case Failure(e: Exception) => feedI(k, Eos(Some(e)))
                case Failure(t)            => throw t
              }
            } else
              // yes we really do want to read the next line
              Try(reader.readLine()) match {
                case Success(null) =>
                  // however none is left in the input, close and notify eos
                  Try(reader.close()) match {
                    case Success(())           => loop(k)
                    case Failure(e: Exception) => feedI(k, Eos(Some(e)))
                    case Failure(t)            => throw t
                  }
                case Success(s) =>
                  // there is something left to read in the current input!
                  // buffer the line and feed it character by character
                  // push the new buffered line with first character processed
                  // ToolXiT reads the line, remove trailing spaces and add a \n in the end
                  // so a line is never empty
                  val line = s.replaceAll("\\s*$", "\n")
                  env.pushInput(reader, Some(line -> 1))
                  feedI(k, Chunk(List((line(0), reader.getLineNumber, 1)))) match {
                    case Success(Cont(None, k)) => loop(k)
                    case i                      => i
                  }
                case Failure(e: Exception) =>
                  env.pushInput(reader, None)
                  feedI(k, Eos(Some(e)))
                case Failure(t) => throw t
              }
          case Some((reader, Some((line, col)))) =>
            // currently reading a buffered line
            if (env.endOfLineEncountered || col >= line.length) {
              // but we encountered an end of line character, then drop it and goto next
              // or we reached the end of line, goto next
              env.endOfLineEncountered = false
              env.pushInput(reader, None)
              loop(k)
            } else {
              // feed with next character and continue processing line
              env.pushInput(reader, Some(line -> (col + 1)))
              feedI(k, Chunk(List((line(col), reader.getLineNumber, col + 1)))) match {
                case Success(Cont(None, k)) => loop(k)
                case i                      => i
              }
            }
        }
      for {
        r <- loop(k)
      } yield r
    case i => Try(i)
  }

}
