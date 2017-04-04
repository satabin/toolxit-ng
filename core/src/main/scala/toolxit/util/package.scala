package toolxit

import scala.annotation.tailrec

import scala.util.{
  Try,
  Failure
}

/** Scala port of the iterator library presented here: http://okmij.org/ftp/Haskell/Iteratee/IterateeM.hs. */
package object util {

  type Enumerator[Elt, A] = Iteratee[Elt, A] => Try[Iteratee[Elt, A]]

  type Enumeratee[EltO, EltI, A] = Iteratee[EltI, A] => Iteratee[EltO, Iteratee[EltI, A]]

  type K[Elt, +A] = Stream[Elt] => Try[(Iteratee[Elt, A], Stream[Elt])]

  @inline
  val exnEos: Exception =
    new Exception("End of stream")

  @inline
  val exnDivergent: Exception =
    new Exception("Divergent iteratee")

  @inline
  def throwError[Elt](exn: Exception): Iteratee[Elt, Nothing] =
    Cont(Some(exn), s => Try((throwError(exn), s)))

  @inline
  def throwRecoverableError[Elt, A](exn: Exception, k: K[Elt, A]): Iteratee[Elt, A] =
    Cont(Some(exn), k)

  /** Runs the iteratee and returns the result wrapped in the monad. */
  def run[Elt, A](it: Iteratee[Elt, A]): Try[A] = it match {
    case Done(v) => Try(v)
    case Cont(None, k) => k(Eos(None)).flatMap {
      case (Done(v), s)           => Try(v)
      case (cont @ Cont(e, _), s) => Failure(e.getOrElse(exnDivergent))
    }
    case Cont(Some(e), _) => Failure(e)
  }

}
