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

import scala.util.Try

abstract class Iteratees[Elt] {

  type Processor[+T] = Iteratee[Elt, T]

  def setEos(s: Stream[Elt]): Exception = s match {
    case Eos(Some(e)) => e
    case _            => exnEos
  }

  @inline
  def exnEos: Exception =
    new Exception("End of stream")

  @inline
  def exnDivergent: Exception =
    new Exception("Divergent iteratee")

  @inline
  val emptyStream: Stream[Elt] =
    Chunk(Nil)

  @inline
  def cont[A](k: K[Elt, A])(implicit name: sourcecode.Enclosing): Iteratee[Elt, A] =
    Cont(None, k)

  @inline
  def done[A](v: A)(implicit name: sourcecode.Enclosing): Iteratee[Elt, A] =
    Done(v)

  @inline
  def noop(implicit name: sourcecode.Enclosing): Iteratee[Elt, Unit] =
    Cont(None, s => Try(done(()) -> s))

  @inline
  def doneM[A](v: A, s: Stream[Elt])(implicit name: sourcecode.Enclosing): Try[(Iteratee[Elt, A], Stream[Elt])] =
    Try((Done(v), s))

  @inline
  def contM[A](k: K[Elt, A])(implicit name: sourcecode.Enclosing): Try[(Iteratee[Elt, A], Stream[Elt])] =
    Try((cont(k), emptyStream))

  @inline
  def ret[A](it: Iteratee[Elt, A])(implicit name: sourcecode.Enclosing): Try[(Iteratee[Elt, A], Stream[Elt])] =
    Try((it, emptyStream))

  @inline
  def feedI[A](k: K[Elt, A], s: Stream[Elt])(implicit name: sourcecode.Enclosing): Try[Iteratee[Elt, A]] =
    k(s).flatMap { case (it, _) => Try(it) }

  val isStreamFinished: Iteratee[Elt, Option[Exception]] = {
    def check(s: Stream[Elt]): Try[(Iteratee[Elt, Option[Exception]], Stream[Elt])] =
      s match {
        case Chunk(Seq()) => contM(check)
        case Eos(e)       => doneM(e.orElse(Some(exnEos)), s)
        case s            => doneM(None, s)
      }
    cont(check)
  }

  val snapshotStream: Iteratee[Elt, Stream[Elt]] =
    cont(s => doneM(s, s))

  def break(pred: Elt => Boolean): Iteratee[Elt, List[Elt]] = {
    def step(acc: List[Elt])(s: Stream[Elt]): Try[(Iteratee[Elt, List[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq()) => contM(step(acc))
      case Chunk(l) => l.span(e => !pred(e)) match {
        case (_, Seq()) => contM(step(acc ++ l))
        case (l, tail)  => doneM(acc ++ l, Chunk(tail))
      }
      case _ => doneM(acc, s)
    }
    cont(step(Nil))
  }

  def dropWhile(pred: Elt => Boolean): Iteratee[Elt, Option[Elt]] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Option[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq()) => contM(step)
      case Chunk(l) => l.dropWhile(pred) match {
        case Seq()          => contM(step)
        case l @ Seq(h, _*) => doneM(Some(h), Chunk(l))
      }
      case _ => doneM(None, s)
    }
    cont(step)
  }

  def takeWhile(pred: Elt => Boolean): Iteratee[Elt, Seq[Elt]] = {
    def step(prefix: Seq[Elt], s: Stream[Elt]): Try[(Iteratee[Elt, Seq[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq()) => contM(step(prefix, _))
      case Chunk(l) => l.span(pred) match {
        case (s, Seq()) => contM(step(prefix ++ s, _))
        case (p, s)     => doneM(prefix ++ p, Chunk(s))
      }
      case _ => doneM(prefix, s)
    }
    cont(step(Seq(), _))
  }

  val peek: Iteratee[Elt, Option[Elt]] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Option[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq())      => contM(step)
      case Chunk(Seq(h, _*)) => doneM(Some(h), s)
      case _                 => doneM(None, s)
    }
    cont(step)
  }

  def lazily[T](it: => Iteratee[Elt, T]): Iteratee[Elt, T] =
    noop.andThen(it)

  val swallow: Iteratee[Elt, Unit] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Unit], Stream[Elt])] = s match {
      case Chunk(Seq())             => contM(step)
      case Chunk(Seq(_, rest @ _*)) => doneM((), Chunk(rest))
      case Eos(e)                   => Try((Cont(Some(setEos(s)), step), s))
    }
    cont(step)
  }

  def pushback(elt: Elt): Iteratee[Elt, Unit] = cont {
    case Chunk(s) => doneM((), Chunk(elt +: s))
    case Eos(e)   => doneM((), Chunk(Seq(elt)))
  }

  def pushback(elts: Seq[Elt]): Iteratee[Elt, Unit] = cont {
    case Chunk(s) => doneM((), Chunk(elts.foldLeft(s)((acc, elt) => elt +: acc)))
    case Eos(e)   => doneM((), Chunk(elts.reverse))
  }

  val head: Iteratee[Elt, Elt] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Elt], Stream[Elt])] = s match {
      case Chunk(Seq())          => contM(step)
      case Chunk(Seq(h, t @ _*)) => doneM(h, Chunk(t))
      case _                     => Try((Cont(Some(setEos(s)), step), s))
    }
    cont(step)
  }

  val headOption: Iteratee[Elt, Option[Elt]] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Option[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq())          => contM(step)
      case Chunk(Seq(h, t @ _*)) => doneM(Some(h), Chunk(t))
      case Eos(None)             => doneM(None, s)
      case Eos(Some(e))          => Try((throwError(e), s))
    }
    cont(step)
  }

  def heads(elts: Seq[Elt]): Iteratee[Elt, Int] = {
    def step(acc: Int, elts: Seq[Elt])(s: Stream[Elt]): Try[(Iteratee[Elt, Int], Stream[Elt])] = (elts, s) match {
      case (_, Chunk(Seq())) => contM(step(acc, elts))
      case (Seq(h1, t1 @ _*), Chunk(Seq(h2, t2 @ _*))) =>
        if (h1 == h2) step(acc + 1, t1)(Chunk(t2))
        else doneM(acc, s)
      case (_, _) => doneM(acc, s)
    }
    def loop(acc: Int, elts: Seq[Elt]): Iteratee[Elt, Int] = elts match {
      case Seq() => Done(acc)
      case l     => cont(step(acc, l))
    }
    loop(0, elts)
  }

  val skipTillEos: Iteratee[Elt, Unit] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Unit], Stream[Elt])] = s match {
      case Chunk(_) => contM(step)
      case _        => doneM((), s)
    }
    cont(step)
  }

  def drop(n: Int): Iteratee[Elt, Unit] = {
    def step(n: Int)(s: Stream[Elt]): Try[(Iteratee[Elt, Unit], Stream[Elt])] = s match {
      case Chunk(l) if l.size < n => contM(step(n - l.size))
      case Chunk(l)               => doneM((), Chunk(l.drop(n)))
      case _                      => doneM((), s)
    }
    cont(step(n))
  }

  val chunk: Iteratee[Elt, Seq[Elt]] = {
    def step(s: Stream[Elt]): Try[(Iteratee[Elt, Seq[Elt]], Stream[Elt])] = s match {
      case Chunk(Seq()) => contM(step)
      case Chunk(l)     => doneM(l, Chunk(Nil))
      case _            => doneM(Nil, s)
    }
    cont(step)
  }

  def enum2[A, B](it1: Iteratee[Elt, A], it2: Iteratee[Elt, B]): Iteratee[Elt, (Iteratee[Elt, A], Iteratee[Elt, B])] =
    (it1, it2) match {
      case (Cont(None, k1), Cont(None, k2)) =>
        def step(k1: K[Elt, A], k2: K[Elt, B]): K[Elt, (Iteratee[Elt, A], Iteratee[Elt, B])] = {
          case Chunk(Seq()) => contM(step(k1, k2))
          case c @ Chunk(_) => feedI(k1, c).flatMap(i1 => feedI(k2, c).flatMap(i2 => ret(enum2(i1, i2))))
          case s            => doneM((cont(k1), cont(k2)), s)
        }
        cont(step(k1, k2))
      case (Cont(None, _), _) => it1.flatMap(v1 => done((done(v1), it2)))
      case (_, Cont(None, _)) => it2.flatMap(v2 => done((it1, done(v2))))
      case (_, _)             => done((it1, it2))
    }

  def many[A, Z](f: (Z, A) => Z)(z: Z)(it: Iteratee[Elt, A]): Iteratee[Elt, Z] =
    it.flatMap(a => many(f)(f(z, a))(it)).recover(_ => z)

}

sealed abstract class Iteratee[Elt, +A](implicit name: sourcecode.Enclosing, clazz: sourcecode.Name) {

  def flatMap[B](f: A => Iteratee[Elt, B]): Iteratee[Elt, B] = this match {
    case Done(v) => f(v)
    case Cont(e, k) =>
      Cont(e, s => k(s).flatMap {
        case (Done(v), s) => f(v) match {
          case Cont(None, k) => k(s)
          case i             => Try((i, s))
        }
        case (i, s) => Try((i.flatMap(f), s))
      })
  }

  def map[B](f: A => B): Iteratee[Elt, B] = this match {
    case Done(v) => Done(f(v))
    case Cont(e, k) => Cont(e, s => k(s).map {
      case (i, s) => (i.map(f), s)
    })
  }

  def andThen[B](i: => Iteratee[Elt, B]): Iteratee[Elt, B] =
    this.flatMap(_ => i)

  def withFilter(f: A => Boolean): Iteratee[Elt, A] =
    this match {
      case Done(v) =>
        if (f(v))
          this
        else
          throwError(new Exception(f"predicate does not hold in $toString"))
      case Cont(e, k) =>
        Cont(e, in => k(in).map { case (i, s) => (i.withFilter(f), s) })
    }

  def recover[B >: A](handler: Exception => B): Iteratee[Elt, B] = this match {
    case d @ Done(_)      => d
    case Cont(Some(e), _) => Done(handler(e))
    case Cont(None, k) => Cont(None, s => k(s).flatMap {
      case (i, s) => Try((i.recover(handler), s))
    })
  }

  def recoverWith[B >: A](handler: Exception => Iteratee[Elt, B]): Iteratee[Elt, B] = this match {
    case d @ Done(_)      => d
    case Cont(Some(e), _) => handler(e)
    case Cont(None, k) => Cont(None, s => k(s).flatMap {
      case (i, s) => Try((i.recoverWith(handler), s))
    })
  }

  override def toString = f"${clazz.value}(${name.value})"

}

final case class Done[Elt, A](v: A)(implicit name: sourcecode.Enclosing) extends Iteratee[Elt, A]

final case class Cont[Elt, A](error: Option[Exception], k: K[Elt, A])(implicit name: sourcecode.Enclosing) extends Iteratee[Elt, A]
