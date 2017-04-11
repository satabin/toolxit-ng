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
import glue._
import font._

import util._

import scala.collection.mutable.Map

import scala.annotation.tailrec

import java.io.LineNumberReader

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
class TeXEnvironment private (val ini: Boolean, val jobname: String, finders: List[FontFinder]) {

  // defines the registeres that get stacked when entering groups
  // registers allow for local definitions
  private class TeXRegisters(val parent: Option[TeXRegisters]) {

    // the map from character to category code
    val categories = Map.empty[Char, Category]

    // the map from character to math code
    val mathcodes = Map.empty[Char, Int]

    // the map from character to delimiter code
    val delcodes = Map.empty[Char, Int]

    // the map from cahracter to its uppercase character
    val uccodes = Map.empty[Char, Char]

    // the map from cahracter to its lowercase character
    val lccodes = Map.empty[Char, Char]

    // the map from cs name to its internal representation
    val controlSequences = Map.empty[String, ControlSequence]

    // the names of all control sequences up to the root
    def allConreolSequences: Set[String] = parent.map(_.allConreolSequences).getOrElse(Set.empty[String]) ++ controlSequences.keySet

    // local values of the different registers
    val counters = Map.empty[Byte, Int]
    // all dimension are stored as an integer multiple of one sp
    // the biggest dimension accepted by TeX is 2^30sp, so an integer
    // is sufficient to store it.
    val dimensions = Map.empty[Byte, Dimension]
    // glues and muglues are the triple (dimension, stretch, shrink)
    val glues = Map.empty[Byte, Glue]
    val muglues = Map.empty[Byte, Muglue]
    // token lists
    val tokens = Map.empty[Byte, List[Token]]

    var currentFont: Option[(String, Option[Either[Dimension, Double]])] = None
    val textfonts = Map.empty[Byte, (String, Option[Either[Dimension, Double]])]
    val scriptfonts = Map.empty[Byte, (String, Option[Either[Dimension, Double]])]
    val scriptscriptfonts = Map.empty[Byte, (String, Option[Either[Dimension, Double]])]

    val integerParameters = Map.empty[String, Int]
    val dimensionParameters = Map.empty[String, Dimension]
    val glueParameters = Map.empty[String, Glue]
    val muglueParameters = Map.empty[String, Muglue]
    val tokenParameters = Map.empty[String, List[Token]]

    // stack of aftergroup tokens
    var afterGroups: List[Token] = Nil

  }

  val fontManager = new FontManager(finders)

  /** The root registers of this instance */
  private val root: TeXRegisters =
    new TeXRegisters(None)

  /** The currently stacked local registers */
  private var locals: TeXRegisters =
    root

  /** The current reading state.
   *
   *  @group Globals
   */
  var state: ReadingState.Value =
    ReadingState.N

  // the stack of modes
  private var modes = List1[Mode](Mode.VerticalMode)

  /** The current parsing mode.
   *
   *  @group Globals
   */
  def mode: Mode =
    modes.head

  /** Enters the given mode. */
  def enterMode(mode: Mode): Unit =
    modes ::= mode

  /** Leaves the current mode, restoring the previous one.
   *  If a mode was left, returns it. If no mode was to leave, return `None` and does nothing.
   */
  def leaveMode(): Option[Mode] =
    modes match {
      case More(m, rest) =>
        // We can never leave the initial vertical mode
        modes = rest
        Some(m)
      case _ =>
        None
    }

  /** The current expansion state.
   *
   *  @group Globals
   */
  var expanding: Boolean =
    true

  /** Whether we are reading replacement text for macros.
   *
   *  @group Globals
   */
  var inReplacement: Boolean =
    false

  private var inputs: List[(LineNumberReader, Option[String], Option[(String, Int)])] =
    List.empty[(LineNumberReader, Option[String], Option[(String, Int)])]

  /** Pushes a new (possibly unnamed) input from which to read next characters from now on.
   *  Once the new input is finished, got back to reading the old input where we stopped.
   */
  def pushInput(reader: LineNumberReader, name: Option[String], line: Option[(String, Int)]): Unit =
    inputs = (reader, name, line) :: inputs

  /** Pops the current input from which to read. */
  def popInput(): Option[(LineNumberReader, Option[String], Option[(String, Int)])] = inputs match {
    case h :: t =>
      inputs = t
      Some(h)
    case _ =>
      None
  }

  private var feedbackList: List[Token] = Nil

  /** This is quite ugly but some commands required the stomach to
   *  work on unexpanded token sequences, and feed them again to the mouth.
   *  There is no way an iteratee can do this through the iteratee right now,
   *  so pushing them back in the environment makes the [[mouth.TeXMouth mouth]]
   *  aware of these tokens to feed again before reading from the [[eyes.TeXEyes eyes]].
   *
   *  The tokens must be in reverse order.
   */
  def pushReadAgain(tokens: List[Token]): Unit = {
    assert(feedbackList.isEmpty)
    feedbackList = tokens
  }

  def popReadAgain(): List[Token] =
    feedbackList match {
      case Nil => Nil
      case l =>
        feedbackList = Nil
        l
    }

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
   *  It returns the group to pushback (in reverse order) when leavin this group.
   *
   *  @group Globals
   */
  def leaveGroup: List[Token] = {
    val ag = locals.afterGroups
    locals = locals.parent.get
    ag
  }

  /** Token to push back after the next assignment. */
  var afterAssignment: Option[Token] = None

  def pushAfterGroup(token: Token): Unit =
    locals.afterGroups = token :: locals.afterGroups

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
    def apply(char: Char): Category =
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
    def update(char: Char, category: Category): Unit =
      locals.categories(char) = category

    def update(char: Char, global: Boolean, category: Category): Unit = {
      val cats = if (global) root.categories else locals.categories
      cats(char) = category
    }

  }

  /** Exposes math code management functions.
   *
   *  @group Registers
   */
  object mathcode {

    /** Returns the math code of the given character in the current environment. */
    def apply(char: Char): Int =
      lookupRegister(char, (_.mathcodes), locals) match {
        case Some(c) =>
          c
        case None =>
          if (char.isLetter)
            0x7100 + char
          else if (char.isDigit)
            0x7000 + char
          else
            char
      }

    /** Sets the math code of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, code: Int): Unit =
      locals.mathcodes(char) = code

    def update(char: Char, global: Boolean, code: Int): Unit = {
      val codes = if (global) root.mathcodes else locals.mathcodes
      codes(char) = code
    }

  }

  /** Exposes delimiter code management functions.
   *
   *  @group Registers
   */
  object delcode {

    /** Returns the delimiter code of the given character in the current environment. */
    def apply(char: Char): Int =
      lookupRegister(char, (_.delcodes), locals) match {
        case Some(c) =>
          c
        case None =>
          -1
      }

    /** Sets the delimiter code of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, code: Int): Unit =
      locals.delcodes(char) = code

    def update(char: Char, global: Boolean, code: Int): Unit = {
      val codes = if (global) root.delcodes else locals.delcodes
      codes(char) = code
    }

  }

  /** Exposes uccode management functions.
   *
   *  @group Registers
   */
  object uccode {

    /** Returns the uppercase character of the given character in the current environment. */
    def apply(char: Char): Char =
      lookupRegister(char, (_.uccodes), locals) match {
        case Some(c) =>
          c
        case None =>
          // if not specified otherwise, UTF-8 letters are in category `letter`
          if (char.isLetter)
            char.toUpper
          else
            char
      }

    /** Sets the uppercase of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, uc: Char): Unit =
      locals.uccodes(char) = uc

    def update(char: Char, global: Boolean, uc: Char): Unit = {
      val uccodes = if (global) root.uccodes else locals.uccodes
      uccodes(char) = uc
    }

  }

  /** Exposes lccode management functions.
   *
   *  @group Registers
   */
  object lccode {

    /** Returns the lowercase character of the given character in the current environment. */
    def apply(char: Char): Char =
      lookupRegister(char, (_.lccodes), locals) match {
        case Some(c) =>
          c
        case None =>
          // if not specified otherwise, UTF-8 letters are in category `letter`
          if (char.isLetter)
            char.toLower
          else
            char
      }

    /** Sets the lowercase of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, lc: Char): Unit =
      locals.lccodes(char) = lc

    def update(char: Char, global: Boolean, lc: Char): Unit = {
      val lccodes = if (global) root.lccodes else locals.lccodes
      lccodes(char) = lc
    }

  }

  /** Exposes control sequence management functions.
   *
   *  @group Registers
   */
  object css {

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

    def update(name: String, global: Boolean, cs: ControlSequence): Unit = {
      val css = if (global) root.controlSequences else locals.controlSequences
      css(name) = cs
    }

    /** Returns the name of all control sequences defined in the current context. */
    def names: Set[String] =
      locals.allConreolSequences

  }

  /** Exposes count register management functions.
   *
   *  @group Registers
   */
  object count {

    /** Finds and returns the count register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(number: Byte): Int =
      lookupRegister(number, (_.counters), locals).getOrElse(0)

    /** Sets the value of the count register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Int) =
      locals.counters(number) = value

    def update(number: Byte, mode: AssignmentMode, global: Boolean, value: Int): Unit = {
      val cnts = if (global) root.counters else locals.counters
      mode match {
        case AssignmentMode.Set      => cnts(number) = value
        case AssignmentMode.Advance  => cnts(number) = cnts.getOrElse(number, 0) + value
        case AssignmentMode.Multiply => cnts(number) = cnts.getOrElse(number, 0) * value
        case AssignmentMode.Divide   => cnts(number) = cnts.getOrElse(number, 0) / value
      }
    }

  }

  /** Exposes integer parameter register management functions.
   *
   *  @group Registers
   */
  object integerParameter {

    /** Finds and returns the integer parameter register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(name: String): Int =
      lookupRegister(name, (_.integerParameters), locals).getOrElse(0)

    /** Sets the value of the integer parameter register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(name: String, value: Int) =
      locals.integerParameters(name) = value

    def update(name: String, mode: AssignmentMode, global: Boolean, value: Int): Unit = {
      val params = if (global) root.integerParameters else locals.integerParameters
      mode match {
        case AssignmentMode.Set      => params(name) = value
        case AssignmentMode.Advance  => params(name) = params.getOrElse(name, 0) + value
        case AssignmentMode.Multiply => params(name) = params.getOrElse(name, 0) * value
        case AssignmentMode.Divide   => params(name) = params.getOrElse(name, 0) / value
      }
    }
  }

  /** Exposes dimension register management functions.
   *
   *  @group Registers
   */
  object dimen {

    /** Finds and returns the dimension register value identified by its register number
     *  in the current environment.
     *  The default value of a dimension register is `0 pt`.
     */
    def apply(number: Byte): Dimension =
      lookupRegister(number, (_.dimensions), locals).getOrElse(ZeroDimen)

    /** Sets the value of the dimension register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Dimension) =
      locals.dimensions(number) = value

    def update(number: Byte, mode: AssignmentMode, global: Boolean, value: Int): Unit = {
      val dims = if (global) root.dimensions else locals.dimensions
      mode match {
        case AssignmentMode.Set      => dims(number) = Dimension(value)
        case AssignmentMode.Advance  => dims(number) = dims.getOrElse(number, ZeroDimen) + value
        case AssignmentMode.Multiply => dims(number) = dims.getOrElse(number, ZeroDimen) * value
        case AssignmentMode.Divide   => dims(number) = dims.getOrElse(number, ZeroDimen) / value
      }
    }
  }

  /** Exposes dimension parameter register management functions.
   *
   *  @group Registers
   */
  object dimensionParameter {

    /** Finds and returns the dimension parameter register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(name: String): Dimension =
      lookupRegister(name, (_.dimensionParameters), locals).getOrElse(ZeroDimen)

    /** Sets the value of the dimension parameter register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(name: String, value: Dimension) =
      locals.dimensionParameters(name) = value

    def update(name: String, mode: AssignmentMode, global: Boolean, value: Int): Unit = {
      val params = if (global) root.dimensionParameters else locals.dimensionParameters
      mode match {
        case AssignmentMode.Set      => params(name) = Dimension(value)
        case AssignmentMode.Advance  => params(name) = params.getOrElse(name, ZeroDimen) + value
        case AssignmentMode.Multiply => params(name) = params.getOrElse(name, ZeroDimen) * value
        case AssignmentMode.Divide   => params(name) = params.getOrElse(name, ZeroDimen) / value
      }
    }
  }

  /** Exposes glue register management functions.
   *
   *  @group Registers
   */
  object skip {

    /** Finds and returns the glue register value identified by its register number
     *  in the current environment.
     *  The default value of a glue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Glue =
      lookupRegister(number, (_.glues), locals).getOrElse(ZeroGlue)

    /** Sets the value of the count register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Glue): Unit =
      locals.glues(number) = value

    def update(number: Byte, global: Boolean, value: Glue): Unit = {
      val glues = if (global) root.glues else locals.glues
      glues(number) = value
    }
  }

  /** Exposes glue parameter register management functions.
   *
   *  @group Registers
   */
  object glueParameter {

    /** Finds and returns the glue parameter register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(name: String): Glue =
      lookupRegister(name, (_.glueParameters), locals).getOrElse(ZeroGlue)

    /** Sets the value of the glue parameter register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(name: String, value: Glue) =
      locals.glueParameters(name) = value

    def update(name: String, global: Boolean, value: Glue): Unit = {
      val params = if (global) root.glueParameters else locals.glueParameters
      params(name) = value
    }
  }

  /** Exposes muglue register management functions.
   *
   *  @group Registers
   */
  object muskip {

    /** Finds and returns the muglue register value identified by its register number
     *  in the current environment.
     *  The default value of a muglue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Muglue =
      lookupRegister(number, (_.muglues), locals).getOrElse(ZeroMuglue)

    /** Sets the value of the count register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Muglue) =
      locals.muglues(number) = value

    def update(number: Byte, global: Boolean, value: Muglue): Unit = {
      val muglues = if (global) root.muglues else locals.muglues
      muglues(number) = value
    }
  }
  /** Exposes muglue parameter register management functions.
   *
   *  @group Registers
   */
  object muglueParameter {

    /** Finds and returns the muglue parameter register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(name: String): Muglue =
      lookupRegister(name, (_.muglueParameters), locals).getOrElse(ZeroMuglue)

    /** Sets the value of the muglue parameter register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(name: String, value: Muglue) =
      locals.muglueParameters(name) = value

    def update(name: String, global: Boolean, value: Muglue): Unit = {
      val params = if (global) root.muglueParameters else locals.muglueParameters
      params(name) = value
    }
  }

  /** Exposes tokens register management functions.
   *
   *  @group Registers
   */
  object toks {

    /** Finds and returns the token register value identified by its register number
     *  in the current environment.
     *  The default value of a tokens register is `Nil`.
     */
    def apply(number: Byte): List[Token] =
      lookupRegister(number, (_.tokens), locals).getOrElse(Nil)

    /** Sets the value of the tokens register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: List[Token]) =
      locals.tokens(number) = value

    def update(number: Byte, global: Boolean, value: List[Token]): Unit = {
      val toks = if (global) root.tokens else locals.tokens
      toks(number) = value
    }

  }

  /** Exposes tokens register management functions.
   *
   *  @group Registers
   */
  object tokenParameter {

    /** Finds and returns the token parameter value identified by its register number
     *  in the current environment.
     *  The default value of a tokens register is `Nil`.
     */
    def apply(name: String): List[Token] =
      lookupRegister(name, (_.tokenParameters), locals).getOrElse(Nil)

    /** Sets the value of the token parameter in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(name: String, value: List[Token]) =
      locals.tokenParameters(name) = value

    def update(name: String, global: Boolean, value: List[Token]): Unit = {
      val toks = if (global) root.tokenParameters else locals.tokenParameters
      toks(name) = value
    }

  }

  /** Exposes text font register management functions.
   *
   *  @group Registers
   */
  object textfont {

    /** Finds and returns the textfont register value identified by its register number
     *  in the current environment.
     */
    def apply(number: Byte): Option[(String, Option[Either[Dimension, Double]])] =
      lookupRegister(number, (_.textfonts), locals)

    /** Sets the value of the textfont register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: (String, Option[Either[Dimension, Double]])) =
      locals.textfonts(number) = value

    def update(number: Byte, global: Boolean, value: (String, Option[Either[Dimension, Double]])): Unit = {
      val fonts = if (global) root.textfonts else locals.textfonts
      fonts(number) = value
    }

  }

  /** Exposes scriptfont register management functions.
   *
   *  @group Registers
   */
  object scriptfont {

    /** Finds and returns the scriptfont register value identified by its register number
     *  in the current environment.
     */
    def apply(number: Byte): Option[(String, Option[Either[Dimension, Double]])] =
      lookupRegister(number, (_.scriptfonts), locals)

    /** Sets the value of the scriptfont register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: (String, Option[Either[Dimension, Double]])) =
      locals.scriptfonts(number) = value

    def update(number: Byte, global: Boolean, value: (String, Option[Either[Dimension, Double]])): Unit = {
      val fonts = if (global) root.scriptfonts else locals.scriptfonts
      fonts(number) = value
    }

  }

  /** Exposes scriptscriptfont register management functions.
   *
   *  @group Registers
   */
  object scriptscriptfont {

    /** Finds and returns the scriptscriptfont register value identified by its register number
     *  in the current environment.
     */
    def apply(number: Byte): Option[(String, Option[Either[Dimension, Double]])] =
      lookupRegister(number, (_.scriptscriptfonts), locals)

    /** Sets the value of the scriptscriptfont register in the current environment.
     *  This value will be reset to the previous value when leaving the current group.
     */
    def update(number: Byte, value: (String, Option[Either[Dimension, Double]])) =
      locals.scriptscriptfonts(number) = value

    def update(number: Byte, global: Boolean, value: (String, Option[Either[Dimension, Double]])): Unit = {
      val fonts = if (global) root.scriptscriptfonts else locals.scriptscriptfonts
      fonts(number) = value
    }

  }

  /** Exposes current font register management functions.
   *
   *  @group Registers
   */
  object font {

    def apply(): Option[(String, Option[Either[Dimension, Double]])] = {
      @tailrec
      def lookup(registers: TeXRegisters): Option[(String, Option[Either[Dimension, Double]])] =
        registers.currentFont match {
          case Some(f) => Some(f)
          case None =>
            registers.parent match {
              case Some(p) => lookup(p)
              case None    => None
            }
        }
      lookup(locals)
    }

    def update(value: (String, Option[Either[Dimension, Double]])): Unit =
      locals.currentFont = Some(value)

    def update(global: Boolean, value: (String, Option[Either[Dimension, Double]])): Unit =
      if (global)
        root.currentFont = Some(value)
      else
        locals.currentFont = Some(value)

  }

  /** The current escape character.
   *
   *  @group Globals
   */
  var escapechar: Char =
    '\\'

  /** The current interaction mode.
   *
   *  @group Globals
   */
  var interactionmode: InteractionMode =
    InteractionMode.ErrorStopMode

  /** The special integers.
   *
   *  @group Globals
   */
  val integers = Map.empty[String, Int]

  /** The special dimensions.
   *
   *  @group Globals
   */
  val dimensions = Map.empty[String, Dimension]

  /** The ht dimensions.
   *
   *  @group Globals
   */
  val ht = Map.empty[Byte, Dimension]

  /** The wd dimensions.
   *
   *  @group Globals
   */
  val wd = Map.empty[Byte, Dimension]

  /** The dp dimensions.
   *
   *  @group Globals
   */
  val dp = Map.empty[Byte, Dimension]

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
    if (b)
      flags = flags | DEBUG_POSITION
    else
      flags = flags ^ DEBUG_POSITION

  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.IGNORED_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER
  category(127) = Category.INVALID_CHARACTER
  // integer parameters
  integerParameter("mag") = 1000
}

object TeXEnvironment {

  /** Creates a new root environment */
  def apply(jobname: String, finders: List[FontFinder]): TeXEnvironment =
    new TeXEnvironment(false, jobname, finders)

}
