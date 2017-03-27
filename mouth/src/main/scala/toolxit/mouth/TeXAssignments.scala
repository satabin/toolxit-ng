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

trait TeXAssignments {
  this: TeXMouth =>

  object StartsAssignment {
    def unapply(token: Token): Boolean = token match {
      case Primitives.IntegerParameter(_) =>
        true
      case CountdefToken(_) =>
        true
      case Primitive("count" | "advance" | "multiply" | "divide" | "chardef" | "let" | "futurelet" | "read") =>
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

  val controlsequence: Processor[String] =
    raw.flatMap {
      case ControlSequenceToken(name, _) => swallow.andThen(done(name))
      case tok                           => throwError(new TeXMouthException(f"Expected control sequence but got $tok.", tok.pos))
    }

  def simpleAssignment(global: Boolean): Processor[Command] =
    read.flatMap {
      // chardef
      case tok @ Primitive("chardef") =>
        for {
          () <- swallow
          cs <- controlsequence
          () <- equals
          c <- char(tok.pos)
        } yield CharacterDefinition(cs, c, global)

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
          () <- to
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
            () <- by
            i <- number
          } yield IntegerParameterAssignment(name, i, mode, global)
        case tok @ CountdefToken(cnt) =>
          for {
            () <- swallow
            () <- by
            i <- number
          } yield CounterAssignment(cnt, i, mode, global)
        case tok @ ControlSequenceToken("count", _) =>
          for {
            () <- swallow
            cnt <- bit8(tok.pos)
            () <- by
            i <- number
          } yield CounterAssignment(cnt, i, mode, global)

        case tok =>
          throwError[Token](new TeXMouthException("variable expected", tok.pos))
      }
    } yield asgn

  object CountdefToken {
    def unapply(token: Token): Option[Byte] = token match {
      case ControlSequenceToken(name, _) =>
        env.css(name) match {
          case Some(TeXInteger(_, c)) => Some(c)
          case _                      => None
        }
      case _ => None
    }
  }

}
