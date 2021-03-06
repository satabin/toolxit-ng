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
import font._
import box._

trait TeXAssignments {
  this: TeXMouth =>

  object StartsAssignment {
    def unapply(token: Token): Boolean =
      StartsSimpleAssignment.unapply(token) || StartsGlobalAssignment.unapply(token)
  }

  object StartsSimpleAssignment {
    def unapply(token: Token): Boolean = token match {
      case Primitives.IntegerParameter(_) =>
        true
      case Primitives.DimensionParameter(_) =>
        true
      case Primitives.GlueParameter(_) =>
        true
      case Primitives.MuglueParameter(_) =>
        true
      case Primitives.TokenParameter(_) =>
        true
      case CountdefToken(_) =>
        true
      case DimendefToken(_) =>
        true
      case FontdefToken(_, _) =>
        true
      case ToksdefToken(_) =>
        true
      case SkipdefToken(_) =>
        true
      case Primitive("count" | "dimen" | "advance" | "multiply" | "divide" | "chardef" | "mathchardef" | "countdef" | "dimendef" | "let" | "futurelet" | "read" | "font" | "nullfont" | "setbox" | "toksdef" | "toks" | "glue") =>
        true
      case Primitives.Font("textfont" | "scriptfont" | "scriptscriptfont") =>
        true
      case Primitives.Codename(_) =>
        true
      case _ =>
        false
    }
  }

  object StartsGlobalAssignment {
    def unapply(token: Token): Boolean = token match {
      case Primitive("fontdimen" | "hyphenchar" | "skewchar" | "hyphenation" | "patterns") => true
      case Primitives.InteractionMode(_) => true
      case Primitives.SpecialInteger(_) => true
      case Primitives.SpecialDimension(_) => true
      case Primitives.InternalDimension("ht" | "wd" | "dp") => true
      case _ => false
    }
  }

  val equals: Processor[Unit] =
    for {
      () <- spaces
      e <- read
      () <- e match {
        case CharacterToken('=', Category.OTHER_CHARACTER) => swallow
        case _ => noop
      }
    } yield ()

  val by = keyword("by", true)

  val to = keyword("to", false)

  val at = keyword("at", true)

  val scaled = keyword("scaled", true)

  val spread = keyword("spread", false)

  val controlsequence: Processor[String] =
    raw.flatMap {
      case ControlSequenceToken(name, _) => swallow.andThen(done(name))
      case tok                           => throwError(new TeXMouthException(f"Expected control sequence but got $tok.", tok.pos))
    }

  val atClause: Processor[Option[Either[Dimension, Double]]] =
    for {
      () <- spaces
      t <- read
      cl <- t match {
        case CharacterToken('a' | 'A', _) =>
          // potential at clause
          for {
            t <- at
            cl <- if (t) dimen.map(d => Some(Left(d))) else done(None)
          } yield cl
        case CharacterToken('s' | 'S', _) =>
          // potential scaled clause
          for {
            t <- scaled
            cl <- if (t) number.map(n => Some(Right(n.toDouble / 1000))) else done(None)
          } yield cl
        case _ =>
          done(None)
      }
    } yield cl

  def simpleAssignment(global: Boolean): Processor[Assignment] =
    read.flatMap {
      // chardef
      case tok @ Primitive("chardef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          c <- char(tok.pos)
        } yield CharacterDefinition(cs, CharacterToken(c, env.category(c)), global)

      // mathchardef
      case tok @ Primitive("mathchardef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          code <- bit15(tok.pos)
        } yield MathCharacterDefinition(cs, code, global)

      // countdef
      case tok @ Primitive("countdef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          i <- bit8(tok.pos)
        } yield CounterDefinition(cs, i, global)

      // dimendef
      case tok @ Primitive("dimendef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          i <- bit8(tok.pos)
        } yield DimensionDefinition(cs, i, global)

      // toksdef
      case tok @ Primitive("toksdef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          i <- bit8(tok.pos)
        } yield TokensDefinition(cs, i, global)

      // let and futurelet
      case tok @ Primitive("let") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          _ <- optSpace
          t <- raw
          () <- swallow
        } yield LetAssignment(cs, t, global)

      case Primitive("futurelet") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          _ <- optSpace
          t1 <- raw
          () <- swallow
          t2 <- raw
          () <- pushback(t1)
        } yield LetAssignment(cs, t2, global)

      // integer variables
      case Primitives.IntegerParameter(name) =>
        for {
          () <- swallow
          () <- equals
          i <- number
        } yield IntegerParameterAssignment(name, SetOp(i), global)

      case CountdefToken(cnt) =>
        for {
          () <- swallow
          () <- equals
          i <- number
        } yield CounterAssignment(cnt, SetOp(i), global)

      case tok @ ControlSequenceToken("count", _) =>
        for {
          () <- swallow
          cnt <- bit8(tok.pos)
          () <- equals
          i <- number
        } yield CounterAssignment(cnt, SetOp(i), global)

      case tok @ ControlSequenceToken("dimen", _) =>
        for {
          () <- swallow
          dim <- bit8(tok.pos)
          () <- equals
          d <- dimen
        } yield DimensionAssignment(dim, SetOp(d), global)

      case Primitives.DimensionParameter(name) =>
        for {
          () <- swallow
          () <- equals
          d <- dimen
        } yield DimensionParameterAssignment(name, SetOp(d), global)

      case DimendefToken(dim) =>
        for {
          () <- swallow
          () <- equals
          d <- dimen
        } yield DimensionAssignment(dim, SetOp(d), global)

      case tok @ ControlSequenceToken("skip", _) =>
        for {
          () <- swallow
          gl <- bit8(tok.pos)
          () <- equals
          g <- glue
        } yield GlueAssignment(gl, SetOp(g), global)

      case Primitives.GlueParameter(name) =>
        for {
          () <- swallow
          () <- equals
          g <- glue
        } yield GlueParameterAssignment(name, SetOp(g), global)

      case SkipdefToken(gl) =>
        for {
          () <- swallow
          () <- equals
          g <- glue
        } yield GlueAssignment(gl, SetOp(g), global)

      case Primitives.TokenParameter(name) =>
        for {
          () <- swallow
          () <- equals
          () <- filler
          t <- read
          toks <- t match {
            case StartsTokenVariable() => tokens
            case _                     => generalText(false)
          }
        } yield TokensParameterAssignment(name, toks, global)

      case ToksdefToken(n) =>
        for {
          () <- swallow
          () <- equals
          () <- filler
          t <- read
          toks <- t match {
            case StartsTokenVariable() => tokens
            case _                     => generalText(false)
          }
        } yield TokensAssignment(n, toks, global)

      case t @ Primitive("toks") =>
        for {
          () <- swallow
          n <- bit8(t.pos)
          () <- equals
          () <- filler
          t <- read
          toks <- t match {
            case StartsTokenVariable() => tokens
            case _                     => generalText(false)
          }
        } yield TokensAssignment(n, toks, global)

      case FontdefToken(fname, mag) =>
        for {
          () <- swallow
        } yield CurrentFontAssignment(fname, mag, global)

      case ControlSequenceToken("nullfont", _) =>
        for {
          () <- swallow
        } yield CurrentFontAssignment("nullfont", None, global)

      // family assignment
      case t @ FontRange("textfont") =>
        for {
          () <- swallow
          n <- catNumber(t.pos)
          () <- equals
          (fn, mag) <- font
        } yield TextFontAssignment(n, fn, mag, global)

      case t @ FontRange("scriptfont") =>
        for {
          () <- swallow
          n <- catNumber(t.pos)
          () <- equals
          (fn, mag) <- font
        } yield ScriptFontAssignment(n, fn, mag, global)

      case t @ FontRange("scriptscriptfont") =>
        for {
          () <- swallow
          n <- catNumber(t.pos)
          () <- equals
          (fn, mag) <- font
        } yield ScriptScriptFontAssignment(n, fn, mag, global)

      case ControlSequenceToken("font", _) =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          fname <- filename(new StringBuilder)
          at <- atClause
        } yield FontAssignment(cs, fname, at, global)

      // arithmetic
      case ControlSequenceToken("advance", _) =>
        for {
          () <- swallow
          adv <- advance(global)
        } yield adv

      case ControlSequenceToken("multiply", _) =>
        for {
          () <- swallow
          mul <- multiply(global)
        } yield mul

      case ControlSequenceToken("divide", _) =>
        for {
          () <- swallow
          div <- divide(global)
        } yield div

      // codenames
      case tok @ Primitives.Codename("catcode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          cat <- catNumber(tok.pos)
        } yield CategoryAssignment(c, Category.withValue(cat), global)

      case tok @ Primitives.Codename("mathcode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          code <- bit15(tok.pos)
        } yield MathCodeAssignment(c, code, global)

      case tok @ Primitives.Codename("delcode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          code <- bit24(tok.pos)
        } yield DelimiterCodeAssignment(c, code, global)

      case tok @ Primitives.Codename("uccode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          uc <- char(tok.pos)
        } yield UccodeAssignment(c, uc, global)

      case tok @ Primitives.Codename("lccode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          lc <- char(tok.pos)
        } yield LccodeAssignment(c, lc, global)

      case tok @ Primitives.Codename("sfcode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          sf <- bit15(tok.pos)
        } yield SfcodeAssignment(c, sf, global)

      // read
      case tok @ ControlSequenceToken("read", _) =>
        for {
          () <- swallow
          i <- catNumber(tok.pos)
          _ <- to
          () <- spaces
          cs <- controlsequence
        } yield Read(i, cs, global)

      case tok @ ControlSequenceToken("setbox", _) =>
        for {
          () <- swallow
          n <- bit8(tok.pos)
          () <- equals
          () <- filler
          b <- boxAssignment(n)
        } yield b

    }

  val globalAssignment: Processor[Assignment] =
    read.flatMap {
      case Primitive("fontdimen") =>
        for {
          () <- swallow
          n <- number
          (fn, mag) <- font
          () <- equals
          d <- dimen
        } yield FontDimensionAssignment(n, fn, mag, d)

      case Primitive("hyphenchar") =>
        for {
          () <- swallow
          (fn, mag) <- font
          () <- equals
          n <- number
        } yield HyphenationCharacterAssignment(fn, mag, n.toChar)

      case Primitive("skewchar") =>
        for {
          () <- swallow
          (fn, mag) <- font
          () <- equals
          n <- number
        } yield SkewCharacterAssignment(fn, mag, n.toChar)

      case Primitive("hyphenation") =>
        for {
          () <- swallow
          exceptions <- generalText(true)
        } yield ???

      case t @ Primitive("patterns") =>
        if (env.ini)
          for {
            () <- swallow
            exceptions <- generalText(true)
          } yield ???
        else
          throwError[Token](new TeXMouthException(f"Patterns can be loaded only by INITEX", t.pos))

      case t @ Primitive("ht") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
          () <- equals
          d <- dimen
        } yield HtAssignment(b, d)

      case t @ Primitive("wd") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
          () <- equals
          d <- dimen
        } yield WdAssignment(b, d)

      case t @ Primitive("dp") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
          () <- equals
          d <- dimen
        } yield DpAssignment(b, d)

      case Primitives.InternalDimension(name) =>
        for (() <- swallow)
          yield InteractionModeAssignment(InteractionMode.withName(name))

      case Primitives.SpecialInteger(name) =>
        for {
          () <- swallow
          () <- equals
          n <- number
        } yield SpecialIntegerAssignment(name, n)

      case Primitives.SpecialDimension(name) =>
        for {
          () <- swallow
          () <- equals
          d <- dimen
        } yield SpecialDimensionAssignment(name, d)

    }

  def assignment(global: Boolean): Processor[Assignment] =
    for {
      t <- read
      asgn <- t match {
        case StartsSimpleAssignment() => simpleAssignment(global)
        case StartsGlobalAssignment() => globalAssignment
        case _                        => throwError[Token](new TeXMouthException("Assignment expected", t.pos))
      }
      () <- env.afterAssignment match {
        case Some(t) =>
          env.afterAssignment = None
          pushback(t)
        case None =>
          noop
      }
    } yield asgn

  def advance(global: Boolean): Processor[Assignment] =
    for {
      t <- read
      asgn <- t match {
        case Primitives.IntegerParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield IntegerParameterAssignment(name, Advance(i), global)
        case tok @ CountdefToken(cnt) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Advance(i), global)
        case tok @ ControlSequenceToken("count", _) =>
          for {
            () <- swallow
            cnt <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Advance(i), global)

        case Primitives.DimensionParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- dimen
          } yield DimensionParameterAssignment(name, Advance(i), global)
        case tok @ DimendefToken(dim) =>
          for {
            () <- swallow
            _ <- by
            i <- dimen
          } yield DimensionAssignment(dim, Advance(i), global)
        case tok @ ControlSequenceToken("dimen", _) =>
          for {
            () <- swallow
            dim <- bit8(tok.pos)
            _ <- by
            i <- dimen
          } yield DimensionAssignment(dim, Advance(i), global)

        case Primitives.GlueParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- glue
          } yield GlueParameterAssignment(name, Advance(i), global)
        case tok @ SkipdefToken(gl) =>
          for {
            () <- swallow
            _ <- by
            i <- glue
          } yield GlueAssignment(gl, Advance(i), global)
        case tok @ ControlSequenceToken("skip", _) =>
          for {
            () <- swallow
            gl <- bit8(tok.pos)
            _ <- by
            i <- glue
          } yield GlueAssignment(gl, Advance(i), global)

        case tok =>
          throwError[Token](new TeXMouthException("variable expected", tok.pos))
      }
    } yield asgn

  def multiply(global: Boolean): Processor[Assignment] =
    for {
      t <- read
      asgn <- t match {
        case Primitives.IntegerParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield IntegerParameterAssignment(name, Multiply(i), global)
        case tok @ CountdefToken(cnt) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Multiply(i), global)
        case tok @ ControlSequenceToken("count", _) =>
          for {
            () <- swallow
            cnt <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Multiply(i), global)

        case Primitives.DimensionParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield DimensionParameterAssignment(name, Multiply(i), global)
        case tok @ DimendefToken(dim) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield DimensionAssignment(dim, Multiply(i), global)
        case tok @ ControlSequenceToken("dimen", _) =>
          for {
            () <- swallow
            dim <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield DimensionAssignment(dim, Multiply(i), global)

        case Primitives.GlueParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield GlueParameterAssignment(name, Multiply(i), global)
        case tok @ SkipdefToken(gl) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield GlueAssignment(gl, Multiply(i), global)
        case tok @ ControlSequenceToken("skip", _) =>
          for {
            () <- swallow
            gl <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield GlueAssignment(gl, Multiply(i), global)

        case tok =>
          throwError[Token](new TeXMouthException("variable expected", tok.pos))
      }
    } yield asgn

  def divide(global: Boolean): Processor[Assignment] =
    for {
      t <- read
      asgn <- t match {
        case Primitives.IntegerParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield IntegerParameterAssignment(name, Divide(i), global)
        case tok @ CountdefToken(cnt) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Divide(i), global)
        case tok @ ControlSequenceToken("count", _) =>
          for {
            () <- swallow
            cnt <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, Divide(i), global)

        case Primitives.DimensionParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield DimensionParameterAssignment(name, Divide(i), global)
        case tok @ DimendefToken(dim) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield DimensionAssignment(dim, Divide(i), global)
        case tok @ ControlSequenceToken("dimen", _) =>
          for {
            () <- swallow
            dim <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield DimensionAssignment(dim, Divide(i), global)

        case Primitives.GlueParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield GlueParameterAssignment(name, Divide(i), global)
        case tok @ SkipdefToken(gl) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield GlueAssignment(gl, Divide(i), global)
        case tok @ ControlSequenceToken("skip", _) =>
          for {
            () <- swallow
            gl <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield GlueAssignment(gl, Divide(i), global)

        case tok =>
          throwError[Token](new TeXMouthException("variable expected", tok.pos))
      }
    } yield asgn

  private def boxAssignment(n: Byte): Processor[Assignment] =
    read.flatMap {
      case t @ Primitives.Box("box") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
        } yield BoxAssignment(n, BoxRegister(b, false))

      case t @ Primitives.Box("copy") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
        } yield BoxAssignment(n, BoxRegister(b, true))

      case t @ Primitives.Box("lastbox") =>
        if (env.mode == Mode.MathMode)
          throwError(new TeXMouthException("", t.pos))
        else
          for (() <- swallow)
            yield BoxAssignment(n, LastBox)

      case t @ Primitives.Box("vsplit") =>
        for {
          () <- swallow
          b <- bit8(t.pos)
          _ <- to
          d <- dimen
        } yield BoxAssignment(n, VSplit(b, d))

      case Primitives.Box("hbox") =>
        for {
          () <- swallow
          spec <- boxSpecification
          t <- read
          () <- t match {
            case ExplicitOrImplicit(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
              for (() <- swallow)
                yield env.enterMode(Mode.RestrictedHorizontalMode)
            case _ =>
              throwError[Token](new TeXMouthException("Implicit or explicit beginning of group expected", t.pos))
          }
        } yield StartHBoxAssignment(n, spec)

      case Primitives.Box("vbox") =>
        for {
          () <- swallow
          spec <- boxSpecification
          t <- read
          () <- t match {
            case ExplicitOrImplicit(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
              for (() <- swallow)
                yield env.enterMode(Mode.InternalVerticalMode)
            case _ =>
              throwError[Token](new TeXMouthException("Implicit or explicit beginning of group expected", t.pos))
          }
        } yield StartVBoxAssignment(n, spec)

      case Primitives.Box("vtop") =>
        for {
          () <- swallow
          spec <- boxSpecification
          t <- read
          () <- t match {
            case ExplicitOrImplicit(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
              for (() <- swallow)
                yield env.enterMode(Mode.InternalVerticalMode)
            case _ =>
              throwError[Token](new TeXMouthException("Implicit or explicit beginning of group expected", t.pos))
          }
        } yield StartVTopAssignment(n, spec)

    }

  private val boxSpecification: Processor[Option[Specification]] =
    for {
      () <- spaces
      t <- read
      s <- t match {
        case CharacterToken('t' | 'T', _) =>
          for {
            t <- to
            s <- if (t)
              for {
                d <- dimen
                () <- filler
              } yield Some(To(d))
            else
              done(None)
          } yield s
        case CharacterToken('s' | 'S', _) =>
          for {
            t <- spread
            s <- if (t)
              for {
                d <- dimen
                () <- filler
              } yield Some(Spread(d))
            else
              done(None)
          } yield s
        case _ =>
          for (() <- filler)
            yield None
      }
    } yield s

  object CountdefToken {
    def unapply(token: Token): Option[Byte] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXCounter(_, c)) => Some(c)
          case _                      => None
        }
      case _ => None
    }
  }

  object DimendefToken {
    def unapply(token: Token): Option[Byte] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXDimension(_, d)) => Some(d)
          case _                        => None
        }
      case _ => None
    }
  }

  object FontdefToken {
    def unapply(token: Token): Option[(String, Option[Either[Dimension, Double]])] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXFont(_, fn, mag)) => Some(fn -> mag)
          case _                         => None
        }
      case _ => None
    }
  }

  object ToksdefToken {
    def unapply(token: Token): Option[Byte] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXTokenList(_, number)) => Some(number)
          case _                             => None
        }
      case _ => None
    }
  }

  object SkipdefToken {
    def unapply(token: Token): Option[Byte] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXGlue(_, number)) => Some(number)
          case _                        => None
        }
      case _ => None
    }
  }

}
