package org.bykn.bosatsu

import fastparse.all._
import java.math.{BigInteger, BigDecimal}
import org.typelevel.paiges.Doc

/**
 * A simple JSON ast for output
 */
sealed abstract class Json {
  def toDoc: Doc

  def render: String = toDoc.render(80)
}

object Json {
  import Doc.text

  sealed abstract class NumberKind {
    def asString: String = {
      import NumberKind._

      this match {
        case IntKind(i) => i.toString
        case LongKind(l) => l.toString
        case BigIntKind(b) => b.toString
        case DoubleKind(d) => d.toString
        case BigDecimalKind(b) => b.toString
        case GiantDecimalKind(d) => d
      }
    }
  }

  object NumberKind {
    sealed abstract class IntegerKind extends NumberKind {
      def toBigInteger: BigInteger =
        this match {
          case IntKind(i) => BigInteger.valueOf(i.toLong)
          case LongKind(l) => BigInteger.valueOf(l)
          case BigIntKind(b) => b
        }
    }
    sealed abstract class FloatingKind extends NumberKind
    final case class IntKind(toInt: Int) extends IntegerKind
    final case class LongKind(toLong: Long) extends IntegerKind
    final case class BigIntKind(toBigInt: BigInteger) extends IntegerKind

    final case class DoubleKind(toDouble: Double) extends FloatingKind
    final case class BigDecimalKind(toBigDecimal: BigDecimal) extends FloatingKind
    final case class GiantDecimalKind(giant: String) extends FloatingKind

    private[this] val digits = ('0' to '9').toSet

    def apply(str: String): NumberKind = {

      @annotation.tailrec
      def allDigits(str: String, at: Int): Boolean =
        (str.length <= at) || (digits(str(at)) && allDigits(str, at + 1))

      val isInteger = allDigits(str, 0) || ((str(0) == '-') && allDigits(str, 1))
      if (isInteger) {
        val len = str.length
        if (len < 10) {
          // all 9 digit int strings fit in an Int
          IntKind(str.toInt)
        }
        else if (len < 12) {
          // some of these fit in an Int
          try {
            IntKind(str.toInt)
          }
          catch {
            case (_: NumberFormatException) => LongKind(str.toLong)
          }
        }
        else if (len < 19) {
          // all these fit in a long
          LongKind(str.toLong)
        }
        else if (len < 21) {
          // some of these fit in a long
          try {
            LongKind(str.toLong)
          }
          catch {
            case (_: NumberFormatException) => BigIntKind(new BigInteger(str))
          }
        }
        else BigIntKind(new BigInteger(str))
      }
      else {
        @inline def bd =
          try BigDecimalKind(new BigDecimal(str))
          catch {
            case (_: NumberFormatException) => GiantDecimalKind(str)
          }

        try {
          val d = str.toDouble
          val dstr = d.toString
          if (!d.isInfinite && (dstr == str || (dstr.toDouble == d))) DoubleKind(d)
          else bd
        }
        catch {
          case (_: NumberFormatException) => bd
        }
      }
    }
  }

  final case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(JsonStringUtil.escape('"', str)))
  }
  final case class JNumberStr(asString: String) extends Json {
    override def render = asString
    def toDoc = text(asString)

    lazy val numberKind: NumberKind = NumberKind(asString)
  }
  object JBool {
    final case object True extends Json {
      override val render = "true"
      val toDoc = text(render)
    }
    final case object False extends Json {
      override val render = "false"
      val toDoc = text(render)
    }

    def apply(bool: Boolean): Json =
      if (bool) True else False

    private[this] val someTrue = Some(true)
    private[this] val someFalse = Some(false)

    def unapply(j: Json): Option[Boolean] =
      j match {
        case True => someTrue
        case False => someFalse
        case _ => None
      }

  }

  final case object JNull extends Json {
    override val render = "null"
    val toDoc = text(render)
  }
  final case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = {
      val parts = Doc.intercalate(Doc.comma, toVector.map { j => (Doc.line + j.toDoc).grouped })
      "[" +: ((parts :+ " ]").nested(2))
    }
  }
  // we use a List here to preserve the order in which items
  // were given to us
  final case class JObject(items: List[(String, Json)]) extends Json {
    val toMap: Map[String, Json] = items.toMap
    val keys: List[String] = items.map(_._1).distinct

    def toDoc = {
      val kvs = keys.map { k =>
        val j = toMap(k)
        JString(k).toDoc + text(":") + ((Doc.lineOrSpace + j.toDoc).nested(2))
      }
      val parts = Doc.intercalate(Doc.comma + Doc.line, kvs).grouped
      parts.bracketBy(text("{"), text("}"))
    }

    /**
     * Return a JObject with each key at most once, but in the order of this
     */
    def normalize: JObject = JObject(keys.map { k => (k, toMap(k)) })
  }

  /**
   * This doesn't have to be super fast (but is fairly fast) since we use it in places
   * where speed won't matter: feeding it into a program that will convert it to bosatsu
   * structured data
   */
  val parser: P[Json] = {
    val recurse = P(parser)
    val pnull = P("null").map(_ => JNull)
    val pbool = P("true").map(_ => JBool.True) | P("false").map(_ => JBool.False)
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val numStr = Parser.jsonNumber.map(JNumberStr(_))
    val whitespace: P[Unit] = CharIn(" \t\r\n")
    val whitespaces0 = whitespace.rep()

    val listSep = P(whitespaces0 ~ "," ~/ whitespaces0)
    val list = P("[" ~/ whitespaces0 ~ recurse.rep(sep = listSep) ~ whitespaces0 ~ "]")
      .map { vs => JArray(vs.toVector) }

    val kv = justStr ~ whitespaces0 ~ P(":") ~/ whitespaces0 ~ recurse
    val obj = P("{" ~/ whitespaces0 ~ kv.rep(sep = listSep) ~ whitespaces0 ~ "}")
      .map { vs => JObject(vs.toList) }

    pnull | pbool | str | numStr | list | obj
  }
}
