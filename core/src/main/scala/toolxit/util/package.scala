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
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

import scala.language.higherKinds

import scala.util.{
  Try,
  Success,
  Failure
}

import scala.concurrent.{
  Future,
  ExecutionContext
}

package object util {

  type Enumerator[In, Monad[+_], Out] = Step[In, Monad, Out] => Iteratee[In, Monad, Out]

  type Identity[+T] = T

  implicit object MonadicTry extends Monadic[Try] {
    def unit[T](v: T): Try[T] = Success(v)
    def error[T](t: Throwable): Try[T] = Failure(t)
    def bind[T, U](f: T => Try[U], t: Try[T]): Try[U] = t.flatMap(f)
  }

  implicit class MonadicFuture(val ec: ExecutionContext) extends Monadic[Future] {
    implicit def _ec = ec
    def unit[T](v: T): Future[T] = Future.successful(v)
    def error[T](t: Throwable): Future[T] = Future.failed(t)
    def bind[T, U](f: T => Future[U], t: Future[T]): Future[U] = t.flatMap(f)
  }

  implicit object MonadicIdentity extends Monadic[Identity] {
    def unit[T](v: T): Identity[T] = v
    def error[T](t: Throwable): Identity[T] = throw t
    def bind[T, U](f: T => Identity[U], t: Identity[T]): Identity[U] = f(t)
  }

}
