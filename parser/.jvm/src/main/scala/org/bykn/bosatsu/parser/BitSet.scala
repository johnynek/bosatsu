package org.bykn.bosatsu.parser

import java.util.BitSet

object BitSetUtil {
  type Tpe = BitSet

  @inline final val isScalaJs = false
  @inline final val isScalaJvm = true

  @inline def isSet(b: BitSet, idx: Int): Boolean =
    (idx == 0) || ((idx > 0) && b.get(idx))

  def bitSetFor(charArray: Array[Char]): BitSet = {
    val min = charArray(0).toInt
    val bs = new BitSet(charArray(charArray.length - 1).toInt + 1 - min)
    var idx = 1
    while (idx < charArray.length) {
      bs.set(charArray(idx).toInt - min)
      idx += 1
    }

    bs
  }
}
