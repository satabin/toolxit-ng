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

import scala.language.higherKinds

/** An `Iteratee` consumes some input and produces some output.
 *
 *  @author Lucas Satabin
 */
sealed abstract class Iteratee[In, +Out] {

  def flatMap[Out1](f: Out => Iteratee[In, Out1]): Iteratee[In, Out1] =
    this match {
      case Done(v, Eoi) =>
        f(v)
      case Done(v, e) =>
        f(v) match {
          case Done(v, _)  => Done(v, e)
          case Cont(k)     => k(e)
          case Error(t, e) => Error(t, e)
        }
      case Error(t, e) =>
        Error(t, e)
      case Cont(k) =>
        Cont(in => k(in).flatMap(f))
    }

  def map[Out1](f: Out => Out1): Iteratee[In, Out1] =
    flatMap(a => Done(f(a), Chunk(Nil)))

  def withFilter(f: Out => Boolean): Iteratee[In, Out] =
    this match {
      case Done(v, e) =>
        if (f(v))
          this
        else
          Error(new Exception("predicate does not hold"), e)
      case Error(_, _) =>
        this
      case Cont(k) =>
        Cont(in => k(in).withFilter(f))
    }
}

final case class Done[In, Out](a: Out, e: Input[In]) extends Iteratee[In, Out]

case class Error[In](t: Throwable, e: Input[In]) extends Iteratee[In, Nothing]

case class Cont[In, Out](k: Input[In] => Iteratee[In, Out]) extends Iteratee[In, Out]

/** A bunch of useful basic iteratees.
 *
 *  @author Lucas Satabin
 */
abstract class Iteratees[Elt] {

  /** The iteratee that is done with the specified result. */
  def done[Res](v: Res): Iteratee[Elt, Res] =
    Done(v, Chunk(Nil))

  /** The iteratee that consumes no input and produces nothing */
  val noop: Iteratee[Elt, Unit] =
    Done((), Chunk(Nil))

  /** Consumes one element from the input. */
  val take: Iteratee[Elt, Option[Elt]] = Cont {
    case Chunk(e :: rest) => Done(Some(e), Chunk(rest))
    case Eoi              => Done(None, Eoi)
    case Chunk(Nil)       => take
  }

  /** Consumes one element from the input.
   *  If the end of input was reached, throw the specified exception.
   */
  def take(t: => Throwable): Iteratee[Elt, Elt] = Cont {
    case Chunk(e :: rest) => Done(e, Chunk(rest))
    case Eoi              => Error(t, Eoi)
    case Chunk(Nil)       => take(t)
  }

  /** Consumes elements from the input as long as the predicate holds.
   *  The list of consumed document is returned.
   */
  def takeWhile(p: Elt => Boolean): Iteratee[Elt, List[Elt]] = Cont {
    case Chunk(l) =>
      l.takeWhile(p) match {
        case Nil  => takeWhile(p).map(rest => l ++ rest)
        case rest => Done(l, Chunk(rest))
      }
    case Eoi => Done(Nil, Eoi)
  }

  /** Peeks one element from the input without consuming it. */
  val peek: Iteratee[Elt, Option[Elt]] = Cont {
    case in @ Chunk(e :: _) => Done(Some(e), in)
    case Eoi                => Done(None, Eoi)
    case Chunk(Nil)         => peek
  }

  /** Peeks up to `n` elements from the input without consuming them. */
  def peek(n: Int): Iteratee[Elt, List[Elt]] = Cont {
    case Chunk(l) if l.size >= n =>
      Done(l.take(n), Chunk(l))
    case Chunk(l1) => peek(n - l1.size).map(l2 => l1 ++ l2)
    case Eoi       => Done(Nil, Eoi)
  }

  /** Consumes the next element from the input, without returning it. */
  val swallow: Iteratee[Elt, Unit] = Cont {
    case Chunk(_ :: l) => Done((), Chunk(l))
    case in @ Eoi      => Done((), in)
    case Chunk(Nil)    => swallow
  }

  /** Consumes the next `n` elements from the input, without returning them. */
  def swallow(n: Int): Iteratee[Elt, Unit] = Cont {
    case Chunk(l) if l.size >= n => Done((), Chunk(l.drop(n)))
    case Chunk(l)                => swallow(n - l.size)
    case Eoi                     => Done((), Eoi)
  }

  /** Drops elements from input until the predicate gets falsified. */
  def dropWhile(p: Elt => Boolean): Iteratee[Elt, Unit] = Cont {
    case Chunk(l) =>
      l.dropWhile(p) match {
        case Nil => dropWhile(p)
        case l   => Done(Unit, Chunk(l))
      }
    case Eoi => Done(Unit, Eoi)
  }

  /** Pushes back one element in front of the input. */
  def pushback(e: Elt): Iteratee[Elt, Unit] = Cont {
    case Chunk(l) => Done((), Chunk(e :: l))
    case _        => Done((), Chunk(List(e)))
  }

  /** Pushes back several elements in front of the input in reverse order. */
  def pushback(el: List[Elt]): Iteratee[Elt, Unit] = Cont {
    case Chunk(l) => Done((), Chunk(el reverse_::: l))
    case _        => Done((), Chunk(el.reverse))
  }

  /** Throws a recoverable error. */
  def throwError(t: Throwable): Iteratee[Elt, Nothing] =
    Error(t, Chunk(Nil))

  /** Throws a fatal error. */
  def throwFatal(t: Throwable): Iteratee[Elt, Nothing] =
    Error(t, Eoi)

  /** Indicates whether the input is finished. Does not consume any input. */
  val finished: Iteratee[Elt, Boolean] = Cont {
    case chunk @ Chunk(_) => Done(false, chunk)
    case Eoi              => Done(true, Eoi)
  }

  /** A backtracking-free non deterministic choice. The first iteratees that finishes is the right one. */
  def choice[Out](it1: Iteratee[Elt, Out], it2: Iteratee[Elt, Out]): Iteratee[Elt, Out] =
    (it1, it2) match {
      case (Done(_, _), _)  => it1
      case (_, Done(_, _))  => it2
      case (Error(_, _), _) => it2
      case (_, Error(_, _)) => it1
      case (Cont(k1), Cont(k2)) => Cont { in =>
        val i1 = k1(in)
        val i2 = k2(in)
        choice(i1, i2)
      }
    }

}
