package dev.bosatsu.codegen

object Idents {

  private val firstChars =
    (('a' to 'z') ++ ('A' to 'Z')).toArray

  private val base62ItemsArray =
    (firstChars ++ ('0' to '9')).toArray

  private val base62Items = base62ItemsArray.toSet

  // these are all the strings that escape as themselves
  val allSimpleIdents: LazyList[String] = {
    val front = firstChars.to(LazyList).map(_.toString)
    val inners = base62ItemsArray.to(LazyList).map(_.toString)

    lazy val tails: LazyList[String] = inners #::: (for {
      h <- inners
      t <- tails
    } yield h + t)

    front #::: (for {
      f <- front
      t <- tails
    } yield f + t)
  }

  private val offset0: Int = '0'.toInt
  private val offsetA: Int = 'A'.toInt - 10
  private val offseta: Int = 'a'.toInt - 36

  private def toBase62(
      c: Char,
      bldr: java.lang.StringBuilder
  ): java.lang.StringBuilder =
    if (base62Items(c)) bldr.append(c)
    else if (c == '_') bldr.append("__")
    else {
      def toChar(i0: Int): Char =
        (i0 + (
          if (i0 < 36) {
            if (i0 < 10) offset0
            else offsetA
          } else offseta
        )).toChar

      def toString(i: Int): Unit =
        if (i < 62) {
          val _ = bldr.append(toChar(i))
        } else {
          val i1 = i / 62
          val i0 = i % 62
          // this isn't tail recursion, but it's okay
          // because the int can't be that big so we can
          // only divide by 62 a few times
          toString(i1)
          val _ = bldr.append(toChar(i0))
        }

      bldr.append('_')
      toString(c.toInt)
      bldr.append('_')
    }

  def escape(prefix: String, str: CharSequence): String = {
    val bldr = new java.lang.StringBuilder
    var idx = 0
    val len = str.length
    bldr.append(prefix)
    while (idx < len) {
      toBase62(str.charAt(idx), bldr): Unit
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
        } else {
          // done, this is the trailing _
          bldr.append(num.toChar)
        }
        return (idx - offset)
      } else {
        val base =
          if (c <= 'Z') {
            if (c <= '9') offset0
            else offsetA
          } else offseta

        num = num * 62 + c.toInt - base
      }
    }
    return -1
  }

  def unescape(prefix: String, str: String): Option[String] =
    if (str.startsWith(prefix)) {
      val bldr = new java.lang.StringBuilder()
      var idx = prefix.length
      val len = str.length
      while (idx < len) {
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
          bldr.append(c): Unit
        }
      }

      Some(bldr.toString())
    } else {
      None
    }
}
