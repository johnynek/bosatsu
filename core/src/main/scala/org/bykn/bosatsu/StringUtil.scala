package org.bykn.bosatsu

import fastparse.all._

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
   }.toArray

  private val escapeString: P[Unit] = {
    val escapes = CharIn(decodeTable.keys.toSeq)
    val oct = CharIn('0' until '8')
    val hex = CharIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val octP = P("o" ~ oct ~ oct)
    val hexP = P("x" ~ hex ~ hex)
    val u4 = P("u" ~ hex.rep(4))
    val u8 = P("U" ~ hex.rep(8))
    val after = escapes | octP | hexP | u4 | u8
    P("\\" ~ after)
  }

  def escapedString(q: Char): P[String] = {
    val qstr = q.toString
    val char = P(escapeString | (!qstr ~ AnyChar))
    P(qstr ~ char.rep().! ~ qstr)
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => PassWith(str1)
          case Left(_) => Fail
        }
      }
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
    def loop(idx: Int): Option[Int] =
      if (idx >= str.length) None
      else if (idx < 0) Some(~idx) // error from decodeNum
      else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        }
        else {
          val nextIdx = idx + 1
          if (nextIdx >= str.length) Some(idx)
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
                  case _ => Some(idx)
                }
            }
          }
        }
      }

    loop(0) match {
      case None => Right(sb.toString)
      case Some(err) => Left(err)
    }
  }
}

object StringUtil extends GenericStringUtil {
  // Here are the rules for escaping in python/bosatsu
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
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
