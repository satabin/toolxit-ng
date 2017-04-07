/*
* Copyright (c) 2017 Lucas Satabin
*
* Licensed under the Apache License Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package font
package tfm

import java.nio.file.{
  Path,
  Files
}
import java.io.FileInputStream

import scodec._
import bits._

class TfmFinder(dir: Path) extends FontFinder {

  def find(name: String): Option[FontMetrics] = {
    val path = dir.resolve(f"$name.tfm")
    if (Files.exists(path)) {
      val stream = new FileInputStream(path.toFile)
      try {
        val bv = BitVector.fromInputStream(stream)
        TfmCodec.file.decodeValue(bv) match {
          case Attempt.Successful(tfm) => Some(tfm)
          case Attempt.Failure(err)    => throw new Exception(err.messageWithContext)
        }
      } finally {
        if (stream != null)
          stream.close()
      }
    } else {
      None
    }
  }

}
