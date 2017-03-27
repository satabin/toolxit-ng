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
*
*/
package toolxit
package util

final class TrieNode(strings: Seq[String]) {

  // the table of distinct lowercase characters in the strings
  private val chars = strings.flatMap(s => s.toLowerCase.distinct).distinct.mkString

  private val children = if (chars.nonEmpty) {
    val arr = Array.ofDim[TrieNode](chars.size)
    for ((idx, ss) <- strings.filter(!_.isEmpty).groupBy(s => chars.indexOf(s(0).toLower)))
      arr(idx) = new TrieNode(ss.map(_.substring(1)))
    arr
  } else {
    Array.empty[TrieNode]
  }

  // this node is an accepting one if there are no children or at least one is empty
  val word = children.isEmpty || strings.exists(_.isEmpty)

  def query(c: Char): Option[TrieNode] =
    if (children.isEmpty) {
      None
    } else {
      val idx = chars.indexOf(c.toLower)
      if (idx == -1) {
        None
      } else {
        Some(children(idx))
      }
    }

}
