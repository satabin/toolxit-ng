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
      case ControlSequenceToken("count" | "advance" | "multiply" | "divide", _) =>
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

  def simpleAssignment(global: Boolean): Processor[Assignment] =
    read.flatMap {
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
        } yield CategoryAssignment(c, cat, global)

    }

  def arithmetic(mode: AssignmentMode, global: Boolean): Processor[Assignment] =
    for {
      t <- raw
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
