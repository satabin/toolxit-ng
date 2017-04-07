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
package tfm

import dimen._

import scodec._
import bits._
import codecs._

import shapeless._

import java.nio.charset.Charset

/** Codec for `.tfm` files.
 *  Based on http://texdoc.net/texmf-dist/doc/generic/knuth/texware/tftopl.pdf
 */
object TfmCodec {

  private val fixWord: Codec[Double] =
    int32.xmap({
      i => i * math.pow(2, -20)
    }, {
      d => math.round(d * math.pow(2, 20)).toInt
    })

  private val uint6 = uint(6)

  private val charInfoWord: Codec[CharInfoWord] =
    (uint8 :: uint4 :: uint4 :: uint6 :: uint2.xmap[Tag](i => Tag.withValue(i.toByte), _.value) :: uint8).as[CharInfoWord]

  private val header =
    ("lf" | uint16) ::
      ("lh" | uint16) ::
      ("bc" | uint16) ::
      ("ec" | uint16) ::
      ("nw" | uint16) ::
      ("nh" | uint16) ::
      ("nd" | uint16) ::
      ("ni" | uint16) ::
      ("nl" | uint16) ::
      ("nk" | uint16) ::
      ("nc" | uint16) ::
      ("np" | uint16)

  private def bcpl(name: String, size: Int) =
    for {
      sz <- uint8.emap(sz => if (sz < size) Attempt.successful(sz) else Attempt.failure(Err(f"$name size must be less than $size bytes")))
      scheme <- fixedSizeBytes(sz, string(Charset.forName("US-ASCII")))
      // ignore the rest
      _ <- ignore((size - sz - 1) * 8)
    } yield Some(scheme)

  private val codingScheme =
    bcpl("encoding scheme", 40)

  private val fontIdentifier =
    bcpl("font identifier", 20)

  private def formatFace(k: Byte): String = {
    val s = k % 2
    val b = k / 2
    val mbl = "MBL"(b % 3)
    val ri = "RI"(s)
    val rce = "RCE"(b / 3)
    f"$mbl$ri$rce"
  }

  private val face =
    for {
      _ <- ignore(24)
      b <- byte
    } yield Some(if (b >= 18 || b < 0) Right(b) else Left(formatFace(b)))

  /** Decodes an entire `.tfm` file. */
  val file: Decoder[TfmFontMetrics] = header.flatMap {
    case lf :: lh :: bc :: ec :: nw :: nh :: nd :: ni :: nl :: nk :: nc :: np :: HNil =>
      for {
        checkSum <- int32
        size <- fixWord
        codingScheme <- if (lh >= 12) codingScheme.decodeOnly else provide(None)
        fontIdentifier <- if (lh >= 17) fontIdentifier.decodeOnly else provide(None)
        face <- if (lh >= 18) face.decodeOnly else provide(None)
        // ignore the rest of the header
        _ <- if (lh >= 19) ignore((lh - 18) * 8) else provide(())
        charInfo <- fixedSizeBits((ec - bc + 1) * 32, vector(charInfoWord))
        width <- fixedSizeBits(nw * 32, vector(fixWord))
        height <- fixedSizeBits(nh * 32, vector(fixWord))
        depth <- fixedSizeBits(nd * 32, vector(fixWord))
        italic <- fixedSizeBits(ni * 32, vector(fixWord))
        ligKern <- fixedSizeBits(nl * 32, vector(int32))
        kern <- fixedSizeBits(nk * 32, vector(fixWord))
        exten <- fixedSizeBits(nc * 32, vector(int32))
        param <- fixedSizeBits(np * 32, vector(fixWord))
      } yield TfmFontMetrics(checkSum, size.pt, codingScheme, fontIdentifier, face, bc, charInfo, width, height, depth, italic, ligKern, kern, exten, param)
  }

}
