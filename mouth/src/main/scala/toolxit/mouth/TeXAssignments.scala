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

trait TeXAssignments {
  this: TeXMouth =>

  object StartsAssignment {
    def unapply(token: Token): Boolean = token match {
      case Primitives.IntegerParameter(_) =>
        true
      case CountdefToken(_) =>
        true
      case DimendefToken(_) =>
        true
      case FontdefToken(_, _) =>
        true
      case Primitive("count" | "dimen" | "advance" | "multiply" | "divide" | "chardef" | "countdef" | "dimendef" | "let" | "futurelet" | "read" | "font" | "nullfont") =>
        true
      case Primitives.Font("textfont" | "scriptfont" | "scriptscriptfont") =>
        true
      case Primitives.Codename(_) =>
        true
      case _ =>
        false
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

  def simpleAssignment(global: Boolean): Processor[Command] =
    read.flatMap {
      // chardef
      case tok @ Primitive("chardef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          c <- char(tok.pos)
        } yield CharacterDefinition(cs, CharacterToken(c, env.category(c)), global)

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
        } yield IntegerParameterAssignment(name, i, AssignmentMode.Set, global)

      case CountdefToken(cnt) =>
        for {
          () <- swallow
          () <- equals
          i <- number
        } yield CounterAssignment(cnt, i, AssignmentMode.Set, global)

      case tok @ ControlSequenceToken("count", _) =>
        for {
          () <- swallow
          cnt <- bit8(tok.pos)
          () <- equals
          i <- number
        } yield CounterAssignment(cnt, i, AssignmentMode.Set, global)

      case tok @ ControlSequenceToken("dimen", _) =>
        for {
          () <- swallow
          dim <- bit8(tok.pos)
          () <- equals
          d <- dimen
        } yield DimensionAssignment(dim, d.sps, AssignmentMode.Set, global)

      case DimendefToken(dim) =>
        for {
          () <- swallow
          () <- equals
          d <- dimen
        } yield DimensionAssignment(dim, d.sps, AssignmentMode.Set, global)

      case FontdefToken(fname, mag) =>
        for {
          () <- swallow
        } yield CurrentFontAssignment(fname, mag, global)

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
          adv <- arithmetic(AssignmentMode.Advance, global)
        } yield adv

      case ControlSequenceToken("multiply", _) =>
        for {
          () <- swallow
          adv <- arithmetic(AssignmentMode.Multiply, global)
        } yield adv

      case ControlSequenceToken("divide", _) =>
        for {
          () <- swallow
          adv <- arithmetic(AssignmentMode.Divide, global)
        } yield adv

      // codenames
      case tok @ Primitives.Codename("catcode") =>
        for {
          () <- swallow
          c <- char(tok.pos)
          () <- equals
          cat <- catNumber(tok.pos)
        } yield CategoryAssignment(c, Category.withValue(cat), global)

      // read
      case tok @ ControlSequenceToken("read", _) =>
        for {
          () <- swallow
          i <- catNumber(tok.pos)
          _ <- to
          () <- spaces
          cs <- controlsequence
        } yield Read(i, cs, global)

    }

  def arithmetic(mode: AssignmentMode, global: Boolean): Processor[Assignment] =
    for {
      t <- read
      asgn <- t match {
        case Primitives.IntegerParameter(name) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield IntegerParameterAssignment(name, i, mode, global)
        case tok @ CountdefToken(cnt) =>
          for {
            () <- swallow
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, i, mode, global)
        case tok @ ControlSequenceToken("count", _) =>
          for {
            () <- swallow
            cnt <- bit8(tok.pos)
            _ <- by
            i <- number
          } yield CounterAssignment(cnt, i, mode, global)

        case tok @ DimendefToken(dim) =>
          for {
            () <- swallow
            _ <- by
            i <- if (mode == AssignmentMode.Set) dimen.map(_.sps) else number
          } yield DimensionAssignment(dim, i, mode, global)
        case tok @ ControlSequenceToken("dimen", _) =>
          for {
            () <- swallow
            dim <- bit8(tok.pos)
            _ <- by
            i <- if (mode == AssignmentMode.Set) dimen.map(_.sps) else number
          } yield DimensionAssignment(dim, i, mode, global)

        case tok =>
          throwError[Token](new TeXMouthException("variable expected", tok.pos))
      }
    } yield asgn

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

}
