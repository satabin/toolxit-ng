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
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package mouth

import util._
import glue._

trait TeXGlues {
  this: TeXMouth =>

  val internalGlue: Processor[Glue] =
    read.flatMap {
      case ControlSequenceToken(SkipDef(name), _) =>
        for (() <- swallow)
          yield env.skip(name)
      case Primitives.GlueParameter(p) =>
        for (() <- swallow)
          yield env.glueParameter(p)
      case Primitives.InternalGlue("lastskip") =>
        for (() <- swallow)
          yield env.glues("lastskip")
      case t @ Primitives.InternalGlue("skip") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
        } yield env.skip(b)
    }

  lazy val internalMuglue: Processor[Muglue] = ???

  val plus = keyword("plus", true)
  val minus = keyword("minus", true)

  val stretch: Processor[Amount] =
    for {
      () <- spaces
      p <- plus
      s <- if (p) amount else done(ZeroAmount)
    } yield s

  val shrink: Processor[Amount] =
    for {
      () <- spaces
      m <- minus
      s <- if (m) amount else done(ZeroAmount)
    } yield s

  val amount: Processor[Amount] =
    for {
      sign <- signs()
      t <- read
      a <- t match {
        case StartsFactor() =>
          for {
            f <- factor
            () <- spaces
            t <- read
            a <- t match {
              case CharacterToken('f' | 'F', _) => filUnit.map(FillAmount(sign * f, _))
              case _                            => unitOfMeasure.map(toDim => DimenAmount(toDim(sign * f)))
            }
          } yield a
        case _ => dimen.map(d => DimenAmount(sign * d))
      }
    } yield a

  val filUnit: Processor[Int] =
    for {
      _ <- keyword("fil", false)
      lvl <- ls(1)
    } yield lvl

  def ls(lvl: Int): Processor[Int] =
    for {
      t <- read
      i <- t match {
        case CharacterToken('l' | 'L', _) =>
          if (lvl < 3)
            for {
              () <- swallow
              i <- ls(lvl + 1)
            } yield i
          else
            throwError[Token](new TeXMouthException("", t.pos))
        case _ =>
          done(lvl)
      }
    } yield i

  val glue: Processor[Glue] =
    for {
      sign <- signs()
      t <- read
      g <- t match {
        case StartsInternalGlue() => internalGlue.map(sign * _)
        case _ =>
          for {
            va <- dimen
            st <- stretch
            sh <- shrink
          } yield Glue(sign * va, st, sh)
      }
    } yield g

  // extractors

  object StartsInternalGlue {
    def unapply(token: Token): Boolean = token match {
      case ControlSequenceToken(SkipDef(_), _) => true
      case Primitives.GlueParameter(_)         => true
      case Primitives.InternalGlue(_)          => true
      case _                                   => false
    }
  }

  object StartsInternalMuglue {
    def unapply(token: Token): Boolean =
      false
  }

  object SkipDef {
    def unapply(name: String): Option[Byte] = env.css(name) match {
      case Some(TeXGlue(_, num)) => Some(num)
      case _                     => None
    }
  }

}
