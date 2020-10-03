package org.bykn.bosatsu.parser

import java.util.BitSet

object BitSetUtil {
  type Tpe = BitSet

  @inline final val isScalaJs = false
  @inline final val isScalaJvm = true

  @inline def isSet(b: BitSet, idx: Int): Boolean =
    b.get(idx)

  def bitSetFor(charArray: Array[Char]): BitSet = {
    var idx = 0
    val bs = new BitSet(charArray(charArray.length - 1).toInt + 1)
    while (idx < charArray.length) {
      bs.set(charArray(idx).toInt)
      idx += 1
    }

    bs
  }
}
