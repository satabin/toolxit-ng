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
package dimen

/** A TeX dimension with a value and a unit.
 *
 *  @author Lucas Satabin
 *
 */
case class Dimension(sps: Int) extends Ordered[Dimension] {

  def toNumber: Int = 0

  def compare(that: Dimension): Int =
    this.sps - that.sps

}

object Dimension {
  // all dimensions are stored in sp (max 2^30 sp)
  // pt -> point
  // pc -> pica (1 pc = 12 pt)
  // in -> inch (1 in = 72.27 pt)
  // bp -> big point (72 bp = 1 in)
  // cm -> centimeter (2.54 cm = 1 in)
  // mm -> millimeter (10 mm = 1 cm)
  // dd -> didot point (1157 dd = 1238 pt)
  // cc -> cicero (1 cc = 12 dd)
  // sp -> scaled point (65536 sp = 1 pt)

  def ofPoint(point: Float): Dimension = Dimension((65536f * point).toInt)

  def ofPica(pica: Float): Dimension = ofPoint(12f * pica)

  def ofInch(in: Float): Dimension = ofPoint(72.27f * in)

  def ofBigPoint(bp: Float): Dimension = ofInch(bp / 72f)

  def ofCentimeter(cm: Float): Dimension = ofInch(cm / 2.54f)

  def ofMillimeter(mm: Float): Dimension = ofCentimeter(mm / 10f)

  def ofDidotPoint(dd: Float): Dimension = ofPoint(dd * 1238f / 1157f)

  def ofCicero(cc: Float): Dimension = ofDidotPoint(cc * 12f)

  def \(name: String) = 0

}

case class FilDimension(factor: Float, fil: FilUnit)
