package dev.bosatsu

import cats.parse.{Parser0 => P0, Parser => P, Accumulator, Appender}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]
  protected def allowEscapedNewlineContinuation: Boolean = false

  private val encodeTable = decodeTable.iterator.map { case (v, k) =>
    (k, s"\\$v")
  }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  val escapedToken: P[Int] = {
    def parseIntStr(p: P[Any], base: Int): P[Int] =
      p.string.map(java.lang.Integer.parseInt(_, base))

    val escapes = P.charIn(decodeTable.keys.toSeq).map(decodeTable(_).toInt)

    val oct = P.charIn('0' to '7')
    val octP = P.char('o') *> parseIntStr(oct ~ oct, 8)

    val hex = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') *> parseIntStr(hex2, 16)

    val hex4 = hex2 ~ hex2
    val u4 = P.char('u') *> parseIntStr(hex4, 16)
    val hex8 = hex4 ~ hex4
    val u8 = P.char('U') *> parseIntStr(hex8, 16)

    // do the oneOf in a guess order of likelihood
    val after = P.oneOf(escapes :: u4 :: hexP :: u8 :: octP :: Nil)
    P.char('\\') *> after
  }

  val utf16Codepoint: P[Int] = {
    // see: https://en.wikipedia.org/wiki/UTF-16
    val first = P.anyChar.map { c =>
      val ci = c.toInt
      if (ci < 0xd800 || ci >= 0xe000) Right(ci)
      else Left(ci)
    }

    val second: P[Int => Int] =
      P.charWhere { c =>
        val ci = c.toInt
        (0xdc00 <= ci) && (ci <= 0xdfff)
      }.map { low =>
        val lowOff = low - 0xdc00 + 0x10000

        { high =>
          val highPart = (high - 0xd800) * 0x400
          highPart + lowOff
        }
      }

    P.select(first)(second)
  }

  val codePointAccumulator: Accumulator[Int, String] =
    new Accumulator[Int, String] {
      def newAppender(first: Int): Appender[Int, String] =
        new Appender[Int, String] {
          val strbuilder = new java.lang.StringBuilder
          strbuilder.appendCodePoint(first)

          def append(item: Int) = {
            strbuilder.appendCodePoint(item)
            this
          }
          def finish(): String = strbuilder.toString
        }
    }

  private val codePointOptionAccumulator: Accumulator[Option[Int], String] =
    new Accumulator[Option[Int], String] {
      def newAppender(first: Option[Int]): Appender[Option[Int], String] =
        new Appender[Option[Int], String] {
          val strbuilder = new java.lang.StringBuilder
          if (first.isDefined) {
            strbuilder.appendCodePoint(first.get): Unit
          }

          def append(item: Option[Int]) = {
            // Performance: this avoids Option.foreach closure allocation/indirection.
            if (item.isDefined) {
              strbuilder.appendCodePoint(item.get): Unit
            }
            this
          }
          def finish(): String = strbuilder.toString
        }
    }

  /** String content without the delimiter
    */
  def undelimitedString1(endP: P[Unit]): P[String] = {
    val continuation =
      (P.char('\\').soft ~ P.char('\n')).as(None)
    val escapedOrLiteral =
      escapedToken
        .map(Some(_))
        .orElse((!endP).with1 *> utf16Codepoint.map(Some(_)))

    (if (allowEscapedNewlineContinuation) continuation.orElse(escapedOrLiteral)
     else escapedOrLiteral)
      .repAs(using codePointOptionAccumulator)
  }

  def codepoint(startP: P[Any], endP: P[Any]): P[Int] =
    startP *>
      escapedToken.orElse((!endP).with1 *> utf16Codepoint) <*
      endP

  def escapedString(q: Char): P[String] = {
    val end: P[Unit] = P.char(q)
    end *> undelimitedString1(end).orElse(P.pure("")) <* end
  }

  def interpolatedString[A, B](
      quoteChar: Char,
      istart: P[A => B],
      interp: P0[A],
      iend: P[Unit]
  ): P[List[Either[B, (Region, String)]]] = {
    val strQuote = P.char(quoteChar)

    val strLit: P[String] = undelimitedString1(strQuote.orElse(istart.void))
    val notStr: P[B] = (istart ~ interp ~ iend).map { case ((fn, a), _) =>
      fn(a)
    }

    val either: P[Either[B, (Region, String)]] =
      ((P.index.with1 ~ strLit ~ P.index)
        .map { case ((s, str), l) => Right((Region(s, l), str)) })
        .orElse(notStr.map(Left(_)))

    (strQuote ~ either.rep0 ~ strQuote).map { case ((_, lst), _) => lst }
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape =
      if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o'   => loop(decodeNum(idx + 2, 2, 8))
                  case 'x'   => loop(decodeNum(idx + 2, 2, 16))
                  case 'u'   => loop(decodeNum(idx + 2, 4, 16))
                  case 'U'   => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }
}

object StringUtil extends GenericStringUtil {
  override protected val allowEscapedNewlineContinuation: Boolean = true
  // Here are the rules for escaping in python/bosatsu
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('$', '$'), // for interpolation
      ('`', '`'),
      ('a', 7.toChar), // bell
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t'),
      ('v', 11.toChar)
    ) // vertical tab

  def codePoints(s: String): List[Int] = {
    // .codePoints isn't available in scalajs
    var idx = 0
    val bldr = List.newBuilder[Int]
    while (idx < s.length) {
      val cp = s.codePointAt(idx)
      idx += Character.charCount(cp)
      bldr += cp
    }

    bldr.result()
  }

  def fromCodePoints(it: Iterable[Int]): String = {
    val bldr = new java.lang.StringBuilder
    it.foreach(bldr.appendCodePoint(_))
    bldr.toString
  }
}

object JsonStringUtil extends GenericStringUtil {
  // Here are the rules for escaping in json
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar), // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )
}
