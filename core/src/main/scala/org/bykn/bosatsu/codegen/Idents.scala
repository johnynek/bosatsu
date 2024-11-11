package org.bykn.bosatsu.codegen

object Idents {

  private[this] val base62Items =
    (('0' to '9') ++ ('A' to 'Z') ++ ('a' to 'z')).toSet

  private def toBase62(c: Char): String =
    if (base62Items(c)) c.toString
    else if (c == '_') "__"
    else {
      def toChar(i0: Int): Char =
        if (i0 < 0) {
          // $COVERAGE-OFF$
          sys.error(s"invalid in: $i0")
          // $COVERAGE-ON$
        } else if (i0 < 10) (i0 + '0'.toInt).toChar
        else if (i0 < 36) (i0 - 10 + 'A'.toInt).toChar
        else if (i0 < 62) (i0 - 36 + 'a'.toInt).toChar
        else {
          // $COVERAGE-OFF$
          sys.error(s"invalid int: $i0")
          // $COVERAGE-ON$
        }

      def toString(i: Int): String =
        if (i < 62) toChar(i).toString
        else {
          val i0 = i % 62
          val i1 = i / 62
          toString(i1) + toChar(i0)
        }

      "_" + toString(c.toInt) + "_"
    }

  def escape(prefix: String, str: CharSequence): String = {
    val bldr = new java.lang.StringBuilder
    var idx = 0
    val len = str.length
    bldr.append(prefix)
    while (idx < len) {
      bldr.append(toBase62(str.charAt(idx)))
      idx += 1
    }
    bldr.toString()
  }

  private def unBase62(
      str: String,
      offset: Int,
      bldr: java.lang.StringBuilder
  ): Int = {
    var idx = offset
    var num = 0

    while (idx < str.length) {
      val c = str.charAt(idx)
      idx += 1
      if (c == '_') {
        if (idx == offset + 1) {
          // this is a literal _
          bldr.append('_')
        }
        else {
          // done, this is the trailing _
          bldr.append(num.toChar)
        }
        return (idx - offset)
      } else {
        val base =
          if (c <= '9') '0'.toInt
          else if (c <= 'Z') ('A'.toInt - 10)
          else ('a'.toInt - 36)

        num = num * 62 + c.toInt - base
      }
    }
    return -1
  }

  def unescape(prefix: String, str: String): Option[String] =
    if (str.startsWith(prefix)) {
      val bldr = new java.lang.StringBuilder()
      var idx = prefix.length
      while (idx < str.length) {
        val c = str.charAt(idx)
        idx += 1
        if (c == '_') {
          val res = unBase62(str, idx, bldr)
          if (res < 1) return None
          else {
            // this tells us how many characters we read
            idx += res
          }
        } else {
          // this character is literally encoded
          bldr.append(c)
        }
      }

      Some(bldr.toString())
    } else {
      None
    }
}