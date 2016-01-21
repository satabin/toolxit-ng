/*
* Copyright (c) 2015 Lucas Satabin
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

import dimen._

import util._

import scala.collection.mutable.{
  Map,
  Stack
}

import scala.annotation.tailrec

/** The TeX environment tracks the following elements:
 *   - defined macros,
 *   - character category codes,
 *   - counters
 *
 *  Environments have a lexical scope, this means that a new sub environment
 *  is created whenever one encounters a character token of category `BEGINNING_OF_GROUP`
 *  and this sub environment is discarded on the first character token of category
 *  `END_OF_GROUP`.
 *  This hierarchy mechanism is fully managed by this class by calling the methods
 *  `enterGroup` and `leaveGroup`.
 *
 *  @groupdesc Globals These declarations represent the global state of the environment
 *  @groupdesc Registers These are the various available registers
 *  @groupdesc Extractors Useful extractor using the current environment
 *
 *  @author Lucas Satabin
 *
 */
class TeXEnvironment(_jobname: String) {

  // defines the registeres that get stacked when entering groups
  // registers allow for local definitions
  private class TeXRegisters(val parent: Option[TeXRegisters]) {

    // the map from character to category code
    val categories = Map.empty[Char, Category.Value]

    // the map from cs name to its internal representation
    val controlSequences = Map.empty[String, ControlSequence]

    // local values of the different registers
    val counters = Map.empty[Byte, Int]
    // all dimension are stored as an integer multiple of one sp
    // the biggest dimension accepted by TeX is 2^30sp, so an integer
    // is sufficient to store it.
    val dimensions = Map.empty[Byte, Dimension]
    // glues and muglues are the triple (dimension, stretch, shrink)
    val glues = Map.empty[Byte, Glue]
    val muglues = Map.empty[Byte, Muglue]
    // TODO other register types

  }

  /** The root registers of this instance */
  private val root: TeXRegisters =
    new TeXRegisters(None)

  /** The currently stacked local registers */
  private var locals: TeXRegisters =
    root

  /** The job name, typically, the name of the command that launched this process.
   *
   *  @group Globals
   */
  val jobname: String =
    _jobname

  /** The current reading state.
   *
   *  @group Globals
   */
  var state: ReadingState.Value =
    ReadingState.N

  /** The current parsing mode.
   *
   *  @group Globals
   */
  var mode: Mode.Value =
    Mode.VerticalMode

  /** The current expansion state.
   *
   *  @group Globals
   */
  var expanding: Boolean =
    true

  /** The stack of opened inputs. The current read input stream is on the top.
   *
   *  @group Globals
   */
  val inputs: Stack[LineReader] =
    Stack.empty[LineReader]

  /** Indicates whether an `\endinput` control sequence has been encountered.
   *
   *  @group Globals
   */
  var endinputEncountered: Boolean =
    false

  /** Indicates whether an END_OF_LINE character has been encountered.
   *
   *  @group Globals
   */
  var endOfLineEncountered: Boolean =
    false

  /** The last token position.
   *
   *  @group Globals
   */
  var lastPosition: Position =
    NoPosition

  /** Enters a new group and returns the new environment local to this group.
   *
   *  @group Globals
   */
  def enterGroup: Unit =
    locals = new TeXRegisters(Some(locals))

  /** Leaves a group and returns the environment corresponding to the parent group.
   *
   *  @group Globals
   */
  def leaveGroup: Unit =
    locals = locals.parent.get

  // generic lookup function that starts from the register on top in the stack
  // and goes up in the hierarchy until the root is reached
  @tailrec
  private def lookupRegister[Key, Value](key: Key, map: TeXRegisters => Map[Key, Value], registers: TeXRegisters): Option[Value] =
    map(registers).get(key) match {
      case Some(v) =>
        Some(v)
      case None =>
        registers.parent match {
          case Some(parent) =>
            lookupRegister(key, map, parent)
          case None =>
            None
        }
    }

  /** Exposes category management functions.
   *
   *  @group Registers
   */
  object category {

    /** Returns the category of the given character in the current environment.
     *  This category may vary over time, so this method must be called every time
     *  one needs to determine the category of a character.
     */
    def apply(char: Char): Category.Value =
      lookupRegister(char, (_.categories), locals) match {
        case Some(c) =>
          c
        case None =>
          // if not specified otherwise, UTF-8 letters are in category `letter`
          if (char.isLetter)
            Category.LETTER
          else
            Category.OTHER_CHARACTER
      }

    /** Sets the category of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, category: Category.Value): Unit =
      locals.categories(char) = category

  }

  /** Exposes control sequence management functions.
   *
   *  @group Registers
   */
  object css {

    /** Exposes global control sequence management functions. */
    object global {
      /** Finds and returns the control sequence definition identified by its name.
       *  If the control sequence is not found, returns `None`.
       */
      def apply(name: String): Option[ControlSequence] =
        root.controlSequences.get(name)

      /** Adds or replace the global control sequence identified by the given name
       *  with the new control sequence definition. This control sequence definition
       *  is global and so will be available in any context.
       */
      def update(name: String, cs: ControlSequence): Unit =
        root.controlSequences(name) = cs

    }

    /** Finds and returns the control sequence definition identified by its name.
     *  If the control sequence is not found in the given context, returns `None`.
     */
    def apply(name: String): Option[ControlSequence] =
      lookupRegister(name, (_.controlSequences), locals)

    /** Indicates whether the given name is a macro declared as `\outer` */
    def isOuter(name: String): Boolean =
      apply(name).collect {
        case TeXMacro(_, _, _, _, outer) => outer
      }.getOrElse(false)

    /** Indicates whether the current environment contains a definition with the given name */
    def contains(name: String): Boolean =
      apply(name).isDefined

    /** Adds or replace the control sequence identified by the given name
     *  with the new control sequence definition. This control sequence definition
     *  is scoped to  the current group only, and will be discarded when leaving the group.
     */
    def update(name: String, cs: ControlSequence) =
      locals.controlSequences(name) = cs
  }

  /** A register is either in this environment or in one parent.
   *
   *  @group Registers
   */
  trait Register[T] {

    def apply(number: Byte): Option[T]

    def update(number: Byte, value: T): Unit

  }

  /** Exposes count register management functions.
   *
   *  @group Registers
   */
  object count extends Register[Int] {

    /** Finds and returns the count register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(number: Byte): Option[Int] =
      lookupRegister(number, (_.counters), locals)

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Int) =
      locals.counters(number) = value
  }

  /** Exposes dimension register management functions.
   *
   *  @group Registers
   */
  object dimen extends Register[Dimension] {

    /** Finds and returns the dimension register value identified by its register number
     *  in the current environment.
     *  The default value of a dimension register is `0 pt`.
     */
    def apply(number: Byte): Option[Dimension] =
      lookupRegister(number, (_.dimensions), locals)

    /** Sets the value of the dimension register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Dimension) =
      locals.dimensions(number) = value
  }

  /** Exposes glue register management functions.
   *
   *  @group Registers
   */
  object skip extends Register[Glue] {

    /** Finds and returns the glue register value identified by its register number
     *  in the current environment.
     *  The default value of a glue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Option[Glue] =
      lookupRegister(number, (_.glues), locals)

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Glue): Unit =
      locals.glues(number) = value
  }

  /** Exposes muglue register management functions.
   *
   *  @group Registers
   */
  object muskip extends Register[Muglue] {

    /** Finds and returns the muglue register value identified by its register number
     *  in the current environment.
     *  The default value of a muglue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Option[Muglue] =
      lookupRegister(number, (_.muglues), locals)

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Muglue) =
      locals.muglues(number) = value
  }

  /** The current escape character.
   *
   *  @group Globals
   */
  var escapechar: Char =
    '\\'

  // ==== bunch of useful extractors ====

  /** Extractor to determine whether a character is an ignored character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object IGNORED_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.IGNORED_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a beginning of group character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object BEGINNING_OF_GROUP {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.BEGINNING_OF_GROUP)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an end of group character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object END_OF_GROUP {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.END_OF_GROUP)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a math shift character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object MATH_SHIFT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.MATH_SHIFT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an alignment tab character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object ALIGNMENT_TAB {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ALIGNMENT_TAB)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an end of line character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object END_OF_LINE {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.END_OF_LINE)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a parameter character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object PARAMETER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.PARAMETER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a superscript character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object SUPERSCRIPT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SUPERSCRIPT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a subscript character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object SUBSCRIPT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SUBSCRIPT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an escape character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object ESCAPE_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ESCAPE_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a space character in the current
   *  environment.
   *
   *  @group Extractors
   */
  object SPACE {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SPACE)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a letter in the current
   *  environment.
   *
   *  @group Extractors
   */
  object LETTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.LETTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an other character  in the current
   *  environment.
   *
   *  @group Extractors
   */
  object OTHER_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.OTHER_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an active character  in the current
   *  environment.
   *
   *  @group Extractors
   */
  object ACTIVE_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ACTIVE_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a comment character  in the current
   *  environment.
   *
   *  @group Extractors
   */
  object COMMENT_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.COMMENT_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an invalid character  in the current
   *  environment.
   *
   *  @group Extractors
   */
  object INVALID_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.INVALID_CHARACTER)
        Some(c)
      else
        None
  }

  // ==== internals ====

  // the different flags used to tune the expansion mechanism

  private var flags: Long = 0x0000000000000000l
  private val DEBUG_POSITION = 0x0000000000000001l

  def debugPositions: Boolean =
    (flags & DEBUG_POSITION) != 0x0

  def debugPositions_=(b: Boolean): Unit =
    if(b)
      flags = flags | DEBUG_POSITION
    else
      flags = flags ^ DEBUG_POSITION

  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.INVALID_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER

}

object TeXEnvironment {

  /** Creates a new root environment */
  def apply(jobname: String): TeXEnvironment =
    new TeXEnvironment(jobname)

}
