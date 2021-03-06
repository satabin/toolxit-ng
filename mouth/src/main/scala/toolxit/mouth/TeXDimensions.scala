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
import dimen._

trait TeXDimensions {
  this: TeXMouth =>

  val optTrue = keyword("true", true)

  val internalUnit: Processor[Double => Dimension] = {
    val emx = keyword(Seq("em", "ex"))
    for {
      t <- read
      u <- t match {
        case StartsInternalInteger() =>
          for (int <- internalInteger)
            yield (d: Double) => d * int.sp
        case StartsInternalDimen() =>
          for (dim <- internalDimension)
            yield (d: Double) => d * dim
        case StartsInternalGlue() =>
          for (glu <- internalGlue)
            yield (d: Double) => d * glu.value
        case _ =>
          for {
            u <- emx
            _ <- optSpace
          } yield env.font() match {
            case Some((fn, mag)) =>
              val metrics = env.fontManager.font(fn, mag).get
              u match {
                case "em" => (d: Double) => d * metrics.quadWidth
                case "ex" => (d: Double) => d * metrics.xHeight
              }
            case None =>
              (_: Double) => ZeroDimen
          }
      }
    } yield u
  }

  private def decimalConstant(integerPart: Int): Processor[Double] =
    for {
      // swallow the comma or period
      () <- swallow
      fractionalPart <- integerConstant(0)
    } yield f"$integerPart.$fractionalPart".toDouble

  val unitOfMeasure: Processor[Double => Dimension] = {
    val physicalUnit = keyword(Seq("pt", "pc", "in", "bp", "cm", "mm", "dd", "cc", "sp"))
    for {
      () <- spaces
      t <- read
      u <- t match {
        case StartsInternalUnit() =>
          internalUnit
        case _ =>
          for {
            t <- optTrue
            u <- physicalUnit
            _ <- optSpace
          } yield {
            val f = u match {
              case "pt" => Dimension.ofPoint _
              case "pc" => Dimension.ofPica _
              case "in" => Dimension.ofInch _
              case "bp" => Dimension.ofBigPoint _
              case "cm" => Dimension.ofCentimeter _
              case "mm" => Dimension.ofMillimeter _
              case "dd" => Dimension.ofDidotPoint _
              case "cc" => Dimension.ofCicero _
              case "sp" => Dimension.ofScaledPoint _
            }
            // apply magnification if `true' is given
            if (t) (d: Double) => f(d * 1000 / env.integerParameter("mag")) else f
          }
      }
    } yield u
  }

  val internalDimension: Processor[Dimension] =
    for {
      t <- read
      d <- t match {
        case ControlSequenceToken(DimenDef(name), _) =>
          for (() <- swallow)
            yield env.dimen(name)

        case Primitives.DimensionParameter(name) =>
          for (() <- swallow)
            yield env.dimensionParameter(name)

        case Primitives.SpecialDimension(name) =>
          for (() <- swallow)
            yield env.dimensions.getOrElse(name, ZeroDimen)

        case tok @ Primitives.InternalDimension("dimen") =>
          for {
            () <- swallow
            d <- bit8(tok.pos)
          } yield env.dimen(d)

        case tok @ Primitives.InternalDimension("ht") =>
          for {
            () <- swallow
            d <- bit8(tok.pos)
          } yield env.ht.getOrElse(d, ZeroDimen)

        case tok @ Primitives.InternalDimension("wd") =>
          for {
            () <- swallow
            d <- bit8(tok.pos)
          } yield env.wd.getOrElse(d, ZeroDimen)

        case tok @ Primitives.InternalDimension("dp") =>
          for {
            () <- swallow
            d <- bit8(tok.pos)
          } yield env.dp.getOrElse(d, ZeroDimen)

        case tok @ Primitives.InternalDimension("fontdimen") =>
          for {
            () <- swallow
            d <- number
            (fn, mag) <- font
          } yield {
            val metrics = env.fontManager.font(fn, mag).get
            metrics(d)
          }

        case Primitives.InternalDimension(name) =>
          for (() <- swallow)
            yield env.dimensions.getOrElse(name, ZeroDimen)

      }
    } yield d

  val factor: Processor[Double] =
    for {
      tok <- read
      d <- tok match {
        case StartsInternalInteger() => internalInteger.map(_.toDouble)
        case CharacterToken(c, Category.OTHER_CHARACTER) if c.isDigit =>
          // an integer may well be a decimal number if followed b a period or comma
          for {
            i <- integerConstant(0)
            next <- read
            d <- next match {
              case CharacterToken('.' | ',', Category.OTHER_CHARACTER) =>
                // this is a decimal constant after all
                decimalConstant(i)
              case _ =>
                // nope, finally it was just an integer, we can parse the unit
                done(i.toDouble)
            }
          } yield d
        case CharacterToken('.' | ',', Category.OTHER_CHARACTER) =>
          decimalConstant(0)
      }
    } yield d

  val normalDimen: Processor[Dimension] =
    for {
      tok <- read
      d <- tok match {
        case StartsInternalDimen() => internalDimension
        case StartsNormalInteger() =>
          // normal integer + unit
          for {
            f <- factor
            toDim <- unitOfMeasure
          } yield toDim(f)
      }
    } yield d

  lazy val normalMudimen: Processor[Dimension] =
    ???

  val dimen: Processor[Dimension] =
    for {
      sign <- signs()
      t <- read
      d <- t match {
        case StartsNormalDimen()  => normalDimen
        case StartsInternalGlue() => internalGlue.map(_.value)
        case _                    => throwError[Token](new TeXMouthException(f"Dimension expected but got $t", t.pos))
      }
    } yield sign * d

  val mudimen: Processor[Dimension] =
    for {
      sign <- signs()
      t <- read
      md <- t match {
        case StartsNormalMudimen() => normalMudimen
      }
    } yield md

  // extractors

  object StartsNormalDimen {
    def unapply(token: Token): Boolean =
      token match {
        case StartsInternalDimen() => true
        case StartsNormalInteger() => true
        case CharacterToken('.' | ',', Category.OTHER_CHARACTER) => true
        case _ => false
      }
  }

  object StartsNormalMudimen {
    def unapply(token: Token): Boolean =
      token match {
        case StartsNormalInteger() => true
        case CharacterToken('.' | ',', Category.OTHER_CHARACTER) => true
        case _ => false
      }
  }

  object StartsPhysicalUnit {
    def unapply(token: Token): Boolean =
      token match {
        case CharacterToken('p' | 'P' | 'i' | 'I' | 'b' | 'B' | 'c' | 'C' | 'm' | 'M' | 'd' | 'D' | 's' | 'S', _) => true
        case _ => false
      }
  }

  object StartsInternalUnit {
    def unapply(token: Token): Boolean =
      token match {
        case CharacterToken('e' | 'E', _) => true
        case StartsInternalInteger()      => true
        case StartsInternalDimen()        => true
        case StartsInternalGlue()         => true
        case _                            => false
      }
  }

  object StartsInternalDimen {
    def unapply(token: Token): Boolean =
      token match {
        case Primitives.DimensionParameter(_)     => true
        case Primitives.SpecialDimension(_)       => true
        case Primitives.InternalDimension(_)      => true
        case ControlSequenceToken(DimenDef(_), _) => true
        case _                                    => false
      }
  }

  object StartsFactor {
    def unapply(token: Token): Boolean = token match {
      case StartsInternalInteger() => true
      case CharacterToken('.' | ',', Category.OTHER_CHARACTER) => true
      case CharacterToken(c, Category.OTHER_CHARACTER) => c.isDigit
      case _ => false
    }
  }

  object DimenDef {
    def unapply(name: String): Option[Byte] = env.css(name) match {
      case Some(TeXDimension(_, num)) => Some(num)
      case _                          => None
    }
  }

}
