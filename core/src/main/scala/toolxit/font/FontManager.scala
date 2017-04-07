/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing = Value val software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package font

import dimen._

import scala.util.Try

import scala.annotation.tailrec

import scala.collection.mutable.Map

/** The font manager is responsible for loading, caching, and updating
 *  fonts.
 *
 *  Fonts are referred to by name and magnification. Two font whose design size after
 *  applying magnification are equals are considered equal and modfying one impacts
 *  the other one.
 *
 *  @param finders The list of [[FontFinder]]s used to load fonts. They are tried in order.
 */
class FontManager(finders: List[FontFinder]) {

  private val loaded = Map.empty[String, FontMetrics]

  private val scaled = Map.empty[String, FontMetrics]

  /** Finds the font, loading it if necessary, and scaling it. */
  def font(name: String, magnification: Option[Either[Dimension, Double]]): Try[FontMetrics] = Try {
    val font = loaded.getOrElseUpdate(name, load(name))
    magnification match {
      case Some(Left(dim)) =>
        if (dim == font.designSize)
          font
        else
          scaled.getOrElseUpdate(f"$name at $dim", font.at(dim))
      case Some(Right(scale)) =>
        if (scale == 1.0)
          font
        else
          scaled.getOrElseUpdate(f"$name at ${scale * font.designSize}", font.at(scale * font.designSize))
      case None =>
        font
    }
  }

  /** Builds and returns the display name for the given magnified font. */
  def fontname(name: String, magnification: Option[Either[Dimension, Double]]): Try[String] = Try {
    val font = loaded.getOrElseUpdate(name, load(name))
    magnification match {
      case Some(Left(dim)) =>
        if (dim == font.designSize)
          name
        else
          f"$name at $dim"
      case Some(Right(scale)) =>
        if (scale == 1.0)
          name
        else
          f"$name at ${scale * font.designSize}"
      case None =>
        name
    }
  }

  /** Updates the given font parameter with the provided value. */
  def update(name: String, magnification: Option[Either[Dimension, Double]], parameter: Int, value: Dimension): Try[Unit] =
    for {
      f <- font(name, magnification)
      n <- fontname(name, magnification)
    } yield {
      val f1 = f.updated(parameter, value)
      if (n == name)
        loaded(name) = f1
      else
        scaled(n) = f1
    }

  private def load(name: String): FontMetrics = {
    @tailrec
    def loop(finders: List[FontFinder]): FontMetrics = finders match {
      case finder :: rest => finder.find(name) match {
        case Some(font) => font
        case None       => loop(rest)
      }
      case Nil => throw new FontNotFoundException(name)
    }
    loop(finders)
  }

}

case class FontNotFoundException(name: String) extends Exception(f"Font not found $name", null, false, false)
