package org.bykn.bosatsu.parser

import scala.collection.mutable.BitSet

object BitSetUtil {
  type Tpe = BitSet

  @inline final val isScalaJs = true
  @inline final val isScalaJvm = false

  @inline def isSet(b: BitSet, idx: Int): Boolean =
    b(idx)

  def bitSetFor(charArray: Array[Char]): BitSet = {
    var idx = 0
    val bs = new BitSet(charArray(charArray.length - 1).toInt + 1)
    while (idx < charArray.length) {
      bs += charArray(idx).toInt
      idx += 1
    }

    bs
  }
}
