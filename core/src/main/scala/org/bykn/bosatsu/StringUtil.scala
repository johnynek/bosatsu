package org.bykn.bosatsu

import org.bykn.bosatsu.parser.{Parser => P, Parser1 => P1}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
   }.toArray

  val escapedToken: P1[Unit] = {
    val escapes = P.charIn(decodeTable.keys.toSeq)

    val oct = P.charIn('0' to '7')
    val octP = P.char('o') ~ oct ~ oct

    val hex = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') ~ hex2

    val hex4 = hex2 ~ hex2
    val u4 = P.char('u') ~ hex4
    val hex8 = hex4 ~ hex4
    val u8 = P.char('U') ~ hex8

    val after = P.oneOf1[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
    (P.char('\\') ~ after).void
  }

  /**
   * String content without the delimiter
   */
  def undelimitedString1(endP: P1[Unit]): P1[String] =
    escapedToken.backtrack.orElse1((!endP).with1 ~ P.anyChar)
      .rep1
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_) => P.fail
        }
      }

  def escapedString(q: Char): P1[String] = {
    val end: P1[Unit] = P.char(q)
    end *> undelimitedString1(end).orElse(P.pure("")) <* end
  }

  def interpolatedString[A](quoteChar: Char, istart: P1[Unit], interp: P[A], iend: P1[Unit]): P1[List[Either[A, (Region, String)]]] = {
    val strQuote = P.char(quoteChar)

    val strLit: P1[String] = undelimitedString1(strQuote.orElse1(istart))
    val notStr: P1[A] = (istart ~ interp ~ iend).map { case ((_, a), _) => a }

    val either: P1[Either[A, (Region, String)]] =
      ((P.index.with1 ~ strLit ~ P.index).map { case ((s, str), l) => Right((Region(s, l), str)) })
        .orElse1(notStr.map(Left(_)))

    (strQuote ~ either.rep ~ strQuote).map { case ((_, lst), _) => lst }
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape = if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else encodeTable.get(c) match {
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
      }
      else if (idx < 0) {
        // error from decodeNum
        idx
      }
      else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        }
        else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          }
          else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o' => loop(decodeNum(idx + 2, 2, 8))
                  case 'x' => loop(decodeNum(idx + 2, 2, 16))
                  case 'u' => loop(decodeNum(idx + 2, 4, 16))
                  case 'U' => loop(decodeNum(idx + 2, 8, 16))
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
      ('v', 11.toChar)) // vertical tab
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
      ('t', '\t'))
}
