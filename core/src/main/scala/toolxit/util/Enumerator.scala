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
  Closeable,
  File,
  FileReader,
  BufferedReader
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

  /** Feeds the iteratee with the string content of a file, character by character. */
  def textFile[A](file: File, chunkSize: Int = 1024): Enumerator[Char, A] = {
    case Cont(None, k) =>
      def loop(reader: BufferedReader, k: K[Char, A], p: Array[Char]): Try[Iteratee[Char, A]] =
        Try(reader.read(p, 0, chunkSize)) match {
          case Success(-1)           => feedI(k, Eos(None))
          case Success(n)            => feedI(k, Chunk(p.take(n))).flatMap(check(reader, p))
          case Failure(e: Exception) => feedI(k, Eos(Some(e)))
          case Failure(t)            => throw t
        }
      def check(reader: BufferedReader, p: Array[Char])(it: Iteratee[Char, A]) = it match {
        case Cont(None, k) => loop(reader, k, p)
        case i             => Try(i)
      }
      Try(new BufferedReader(new FileReader(file), chunkSize)) match {
        case Success(reader) =>
          for {
            r <- loop(reader, k, Array.ofDim[Char](chunkSize))
            _ <- Try(Try(reader.close).getOrElse(Unit))
          } yield r
        case Failure(t: Exception) =>
          feedI(k, Eos(Some(t)))
        case Failure(t) =>
          // it is porbably a JVM error
          throw t
      }
    case i => Try(i)
  }

}
