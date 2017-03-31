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
            feedI(k, Chunk(s.replaceAll("\\s+$", "").zipWithIndex.map { case (c, idx) => (c, line, idx + 1) })).flatMap(check(line + 1, reader))
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

}
