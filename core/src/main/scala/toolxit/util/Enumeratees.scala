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

import scala.util.Try

object Enumeratees {

  private def lift[Elt, A](m: Try[A]): Iteratee[Elt, A] =
    Cont(None, s => m.flatMap(v => Try((Done(v), s))))

  @inline
  private def feedI[Elt, A](k: K[Elt, A], s: Stream[Elt]): Try[Iteratee[Elt, A]] =
    k(s).flatMap { case (it, _) => Try(it) }

  def mapStream[EltO, EltI, A](f: EltO => EltI): Enumeratee[EltO, EltI, A] = {
    case Cont(None, k) =>
      def step(k: K[EltI, A]): K[EltO, Iteratee[EltI, A]] = {
        case Chunk(Seq()) => Try((Cont(None, step(k)), Chunk(Nil)))
        case Chunk(l)     => feedI(k, Chunk(l.map(f))).flatMap(i => Try((mapStream[EltO, EltI, A](f).apply(i), Chunk(Nil))))
        case s            => Try((Done(Cont(None, k)), s))
      }
      Cont(None, step(k))
    case i => Done(i)
  }

  private def isStreamFinished[Elt]: Iteratee[Elt, Option[Exception]] = {
    def check(s: Stream[Elt]): Try[(Iteratee[Elt, Option[Exception]], Stream[Elt])] =
      s match {
        case Chunk(Seq()) => Try((Cont(None, check), Chunk(Nil)))
        case Eos(e)       => Try((Done(e.orElse(Some(exnEos))), s))
        case s            => Try((Done(None), s))
      }
    Cont(None, check)
  }

  def sequenceStream[EltO, EltI, A](fi: Iteratee[EltO, EltI]): Enumeratee[EltO, EltI, A] = {
    case i @ Cont(None, k) =>
      def step(k: K[EltI, A]) =
        fi.flatMap(v => lift[EltO, Iteratee[EltI, A]](feedI(k, Chunk(List(v))))).flatMap(sequenceStream[EltO, EltI, A](fi))
      isStreamFinished[EltO].flatMap(_.map(_ => Done[EltO, Iteratee[EltI, A]](i)).getOrElse(step(k)))
    case i => Done(i)
  }

  def join[EltO, EltI, A](outer: Iteratee[EltO, Iteratee[EltI, A]]): Iteratee[EltO, A] = {
    def step(inner: Iteratee[EltI, A]): Iteratee[EltO, A] =
      inner match {
        case Cont(None, k)    => lift(k(Eos(None))).flatMap(p => step(p._1))
        case Done(v)          => Done(v)
        case Cont(Some(e), k) => throwError(e)
      }
    outer.flatMap(step)
  }

}
