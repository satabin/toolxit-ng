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

  /** Feeds the iteratee with the string content of a reader, character by character. */
  def reader[A](reader: Reader, chunkSize: Int = 1024): Enumerator[(Char, Int, Int), A] = {
    case Cont(None, k) =>
      def loop(line: Int, column: Int, reader: BufferedReader, k: K[(Char, Int, Int), A], p: Array[Char]): Try[Iteratee[(Char, Int, Int), A]] =
        Try(reader.read(p, 0, chunkSize)) match {
          case Success(-1) => feedI(k, Eos(None))
          case Success(n) =>
            val (l, c, chars) = p.take(n).foldLeft((line, column, Seq.empty[(Char, Int, Int)])) {
              case ((line, column, chars), '\n') =>
                (line + 1, 1, chars :+ ('\n', line, column))
              case ((line, column, chars), c) =>
                (line, column + 1, chars :+ (c, line, column))
            }
            feedI(k, Chunk(chars)).flatMap(check(l, c, reader, p))
          case Failure(e: Exception) => feedI(k, Eos(Some(e)))
          case Failure(t)            => throw t
        }
      def check(line: Int, column: Int, reader: BufferedReader, p: Array[Char])(it: Iteratee[(Char, Int, Int), A]) = it match {
        case Cont(None, k) => loop(line, column, reader, k, p)
        case i             => Try(i)
      }
      for {
        r <- loop(1, 1, new BufferedReader(reader, chunkSize), k, Array.ofDim[Char](chunkSize))
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
