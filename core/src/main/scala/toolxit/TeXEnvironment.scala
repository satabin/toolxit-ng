/*
* This file is part of the ToolXiT project.
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

import scala.collection.mutable.Map

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
 *  @author Lucas Satabin
 *
 */
abstract class TeXEnvironment {
  self =>

  /** The root environment of this instance */
  def root: TeXEnvironment

  /** The job name, typically, the name of the command that launched this process */
  val jobname: String

  /** The current parsing mode */
  var mode: Mode.Value

  /** Enters a new group and returns the new environment local to this group. */
  def enterGroup: TeXEnvironment =
    new SubTeXEnvironment(this)

  /** Leaves a group and returns the environment corresponding to the parent group. */
  def leaveGroup: TeXEnvironment

  /** Exposes category management functions. */
  val category: Category

  trait Category {
    /** Returns the category of the given character in the current environment.
     *  This category may vary over time, so this method must be called every time
     *  one needs to determine the category of a character.
     */
    def apply(char: Char): Category.Value

    /** Sets the category of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, category: Category.Value): Unit =
      categories(char) = category

  }

  /** Exposes control sequence management functions. */
  val css: Css

  trait Css {

    /** Exposes global control sequence management functions. */
    val global: Global

    trait Global {
      /** Finds and returns the control sequence definition identified by its name.
       *  If the control sequence is not found, returns `None`.
       */
      def apply(name: String): Option[ControlSequence]

      /** Adds or replace the global control sequence identified by the given name
       *  with the new control sequence definition. This control sequence definition
       *  is global and so will be available in any context.
       */
      def update(name: String, cs: ControlSequence): Unit =
        root.css(name) = cs

    }

    /** Finds and returns the control sequence definition identified by its name.
     *  If the control sequence is not found in the given context, returns `None`.
     */
    def apply(name: String): Option[ControlSequence]

    /** Indicates whether the current environment contains a definition with the given name */
    def contains(name: String): Boolean =
      apply(name).isDefined

    /** Adds or replace the control sequence identified by the given name
     *  with the new control sequence definition. This control sequence definition
     *  is scoped to  the current group only, and will be discarded when leaving the group.
     */
    def update(name: String, cs: ControlSequence) =
      controlSequences(name) = cs
  }

  // A register is either in this environment or in one parent
  trait Register[T] {

    def apply(number: Byte): Option[T]

    def update(number: Byte, value: T): Unit

  }

  /** Exposes count register management functions. */
  val count: Count

  trait Count extends Register[Int] {

    /** Finds and returns the count register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(number: Byte): Option[Int]

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Int) =
      counters(number) = value
  }

  /** Exposes dimension register management functions. */
  val dimen: Dimen

  trait Dimen extends Register[Dimension] {

    /** Finds and returns the dimension register value identified by its register number
     *  in the current environment.
     *  The default value of a dimension register is `0 pt`.
     */
    def apply(number: Byte): Option[Dimension]

    /** Sets the value of the dimension register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Dimension) =
      dimensions(number) = value
  }

  /** Exposes glue register management functions. */
  val skip: Skip

  trait Skip extends Register[Glue] {

    /** Finds and returns the glue register value identified by its register number
     *  in the current environment.
     *  The default value of a glue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Option[Glue]

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Glue): Unit =
      glues(number) = value
  }

  /** Exposes muglue register management functions. */
  val muskip: Muskip

  trait Muskip extends Register[Muglue] {

    /** Finds and returns the muglue register value identified by its register number
     *  in the current environment.
     *  The default value of a muglue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Option[Muglue]

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Muglue) =
      muglues(number) = value
  }

  /** The current escape character */
  var escapechar: Char

  /** Returns the meaning of the given token in the current environment */
  def meaning(token: Token): String = token match {
    case CharacterToken(c, Category.BEGINNING_OF_GROUP) =>
      "begin-group character " + c
    case CharacterToken(c, Category.END_OF_GROUP) =>
      "end-group character" + c
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      "the character " + c
    case CharacterToken(c, Category.LETTER) =>
      "the letter " + c
    case CharacterToken(c, Category.PARAMETER) =>
      "macro parameter character " + c
    case ControlSequenceToken(name, _) if Primitives.all.contains(name) =>
      escapechar + name
    case ControlSequenceToken(name, _) =>
      val esc = escapechar
      css(name) match {
        case Some(cs) =>
          cs match {
            case TeXInteger(_, number) =>
              esc + "count" + number
            case TeXChar(_, number) =>
              esc + "char\"" + number
            case TeXMathChar(_, number) =>
              esc + "mathchar\"" + number
            case TeXDimension(_, number) =>
              esc + "dimen" + number
            case TeXGlue(_, number) =>
              esc + "skip" + number
            case TeXMuglue(_, number) =>
              esc + "muskip" + number
            case TeXMacro(name, parameters, repl, long, outer) =>
              val params = parameters map {
                case Left(ParameterToken(n)) =>
                  "#" + n
                case Right(tokens) =>
                  tokens.map(toString).mkString
              }
              "macro:" + params + "->" + repl.map(toString).mkString
            case TeXTokenList(_, number) =>
              esc + "toks" + number
            case TeXFont(_, number) =>
              ???
            case TeXPrimitive(name) =>
              esc + name
          }
        case None =>
          // unknown control sequence in this environment, undefined
          "undefined"
      }
  }

  /*** Indicates whether this token should be expanded in the current environment */
  def expandable(token: Token): Boolean = token match {
    case ControlSequenceToken(name, _) =>
      css.contains(name) || Primitives.expandablePrimitives.contains(name)
    case _ =>
      false
  }

  /** Makes a string out of a parsed token */
  def toString(token: Token): String = token match {
    case ControlSequenceToken(name, true) =>
      // an active character is printed without escape character
      name
    case ControlSequenceToken(name, false) =>
      escapechar + name
    case CharacterToken(c, _) =>
      c.toString
    case _ =>
      throw new TeXInternalException("should never happen")
  }

  def toString(tokens: List[Token]): String =
    tokens.foldLeft("") { (acc, token) =>
      acc + toString(token)
    }

  // ==== bunch of useful extractors ====

  /** Extractor to determine whether a character is an ignored character in the current
   *  environment
   */
  object IGNORED_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.IGNORED_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a beginning of group character in the current
   *  environment
   */
  object BEGINNING_OF_GROUP {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.BEGINNING_OF_GROUP)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an end of group character in the current
   *  environment
   */
  object END_OF_GROUP {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.END_OF_GROUP)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a math shift character in the current
   *  environment
   */
  object MATH_SHIFT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.MATH_SHIFT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an alignment tab character in the current
   *  environment
   */
  object ALIGNMENT_TAB {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ALIGNMENT_TAB)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an end of line character in the current
   *  environment
   */
  object END_OF_LINE {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.END_OF_LINE)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a parameter character in the current
   *  environment
   */
  object PARAMETER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.PARAMETER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a superscript character in the current
   *  environment
   */
  object SUPERSCRIPT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SUPERSCRIPT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a subscript character in the current
   *  environment
   */
  object SUBSCRIPT {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SUBSCRIPT)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an escape character in the current
   *  environment
   */
  object ESCAPE_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ESCAPE_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a space character in the current
   *  environment
   */
  object SPACE {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.SPACE)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a letter in the current
   *  environment
   */
  object LETTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.LETTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an other character  in the current
   *  environment
   */
  object OTHER_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.OTHER_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an active character  in the current
   *  environment
   */
  object ACTIVE_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.ACTIVE_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is a comment character  in the current
   *  environment
   */
  object COMMENT_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.COMMENT_CHARACTER)
        Some(c)
      else
        None
  }

  /** Extractor to determine whether a character is an invalid character  in the current
   *  environment
   */
  object INVALID_CHARACTER {
    def unapply(c: Char): Option[Char] =
      if (category(c) == Category.INVALID_CHARACTER)
        Some(c)
      else
        None
  }

  // ==== internals ====

  // the map from character to category code
  protected val categories = Map.empty[Char, Category.Value]

  // the map from cs name to its internal representation
  protected val controlSequences = Map.empty[String, ControlSequence]

  // local values of the different registers
  protected[this] val counters = Map.empty[Byte, Int]
  // all dimension are stored as an integer multiple of one sp
  // the biggest dimension accepted by TeX is 2^30sp, so an integer
  // is sufficient to store it.
  protected[this] val dimensions = Map.empty[Byte, Dimension]
  // glues and muglues are the triple (dimension, stretch, shrink)
  protected[this] val glues = Map.empty[Byte, Glue]
  protected[this] val muglues = Map.empty[Byte, Muglue]
  // TODO other register types

}

object TeXEnvironment {

  /** Creates a new root environment */
  def apply(jobname: String): TeXEnvironment =
    new RootTeXEnvironment(jobname)

}

private class SubTeXEnvironment(parent: TeXEnvironment) extends TeXEnvironment {

  val root: TeXEnvironment =
    parent.root

  val jobname: String =
    root.jobname

  def mode: Mode.Value =
    root.mode

  def mode_=(m: Mode.Value): Unit =
    root.mode = m

  def leaveGroup: TeXEnvironment =
    parent

  object category extends Category {

    def apply(char: Char): Category.Value =
      categories.get(char) match {
        case Some(cat) => cat
        case None =>
          parent.category(char)
      }

  }

  object css extends Css {

    object global extends Global {

      def apply(name: String): Option[ControlSequence] =
        root.css(name)

    }

    def apply(name: String): Option[ControlSequence] =
      controlSequences.get(name).orElse(parent.css(name))

  }

  object count extends Count {

    def apply(number: Byte): Option[Int] =
      counters.get(number).orElse(parent.count(number))

  }

  object dimen extends Dimen {

    def apply(number: Byte): Option[Dimension] =
      dimensions.get(number).orElse(parent.dimen(number))

  }

  object skip extends Skip {

    def apply(number: Byte): Option[Glue] =
      glues.get(number).orElse(parent.skip(number))

  }

  object muskip extends Muskip {

    def apply(number: Byte): Option[Muglue] =
      muglues.get(number).orElse(parent.muskip(number))

  }

  def escapechar: Char =
    _escapechar match {
      case Some(c) =>
        c
      case None =>
        parent.escapechar
    }

  /** Sets the current escape character */
  def escapechar_=(c: Char): Unit =
    _escapechar = Some(c)

  // ==== internals ====

  // the internal parameters
  private[this] var _escapechar: Option[Char] = None

}

private class RootTeXEnvironment(override val jobname: String) extends TeXEnvironment {

  val root =
    this

  var mode =
    Mode.VerticalMode

  var escapechar =
    92.toChar

  def leaveGroup: TeXEnvironment =
    throw new TeXInternalException("Cannot leave group when in top-level environment")

  object category extends Category {

    def apply(char: Char): Category.Value =

      categories.get(char) match {
        case Some(cat) => cat
        case None =>
          // if not specified otherwise, UTF-8 letters are in category `letter`
          if (char.isLetter)
            Category.LETTER
          else
            Category.OTHER_CHARACTER
      }

  }

  object css extends Css {

    object global extends Global {

      def apply(name: String): Option[ControlSequence] =
        controlSequences.get(name)

    }

    def apply(name: String): Option[ControlSequence] =
      controlSequences.get(name)

  }

  object count extends Count {

    def apply(number: Byte): Option[Int] =
      counters.get(number)

  }

  object dimen extends Dimen {

    def apply(number: Byte): Option[Dimension] =
      dimensions.get(number)

  }

  object skip extends Skip {

    def apply(number: Byte): Option[Glue] =
      glues.get(number)

  }

  object muskip extends Muskip {

    def apply(number: Byte): Option[Muglue] =
      muglues.get(number)

  }

  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.INVALID_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER

  // add all primitive control sequence names
  for (name <- Primitives.all)
    css(name) = TeXPrimitive(name)

}
