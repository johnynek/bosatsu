package org.bykn.bosatsu

import fastparse.all._
import java.math.{BigInteger, BigDecimal}
import org.typelevel.paiges.Doc
import cats.Eq

/**
 * A simple JSON ast for output
 */
sealed abstract class Json {
  def toDoc: Doc

  def render: String
}

object Json {
  import Doc.text

  final case class JString(str: String) extends Json {
    override def render = "\"%s\"".format(JsonStringUtil.escape('"', str))
    def toDoc = text(render)
  }
  final case class JNumberStr(asString: String) extends Json {
    override def render = asString
    def toDoc = text(asString)

    def toBigInteger: Option[BigInteger] =
      try Some(new BigDecimal(asString).toBigIntegerExact)
      catch {
        case (_: ArithmeticException) => None
      }
  }

  object JBigInteger {
    // optimize the common case
    private def allDigits(str: String): Boolean = {
      var idx = 0
      while (idx < str.length) {
        val c = str(idx)
        if (c < '0' || '9' < c) return false
        idx = idx + 1
      }
      true
    }
    def unapply(j: Json): Option[BigInteger] =
      j match {
        case num@JNumberStr(str) =>
          if (allDigits(str)) Some(new BigInteger(str))
          else num.toBigInteger
        case _ => None
      }
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

    def render = toDoc.render(80)
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

    def render = toDoc.render(80)
  }

  /**
   * this checks for semantic equivalence:
   * 1. we use BigDecimal to compare JNumberStr
   * 2. we normalize objects
   */
  implicit val eqJson: Eq[Json] =
    new Eq[Json] {
      def eqv(a: Json, b: Json) =
        (a, b) match {
          case (JNull, JNull) => true
          case (JBool.True, JBool.True) => true
          case (JBool.False, JBool.False) => true
          case (JString(sa), JString(sb)) => sa == sb
          case (JNumberStr(sa), JNumberStr(sb)) =>
            new BigDecimal(sa).compareTo(new BigDecimal(sb)) == 0
          case (JArray(itemsa), JArray(itemsb)) =>
            (itemsa.size == itemsb.size) &&
              itemsa.iterator
                .zip(itemsb.iterator)
                .forall { case (a, b) => eqv(a, b) }
          case (oa@JObject(_), ob@JObject(_)) =>
            val na = oa.normalize
            val nb = ob.normalize
            (na.toMap.keySet == nb.toMap.keySet) &&
              na.keys.forall { k =>
                eqv(na.toMap(k), nb.toMap(k))
              }
          case (_, _) => false
        }
    }

  /**
   * This doesn't have to be super fast (but is fairly fast) since we use it in places
   * where speed won't matter: feeding it into a program that will convert it to bosatsu
   * structured data
   */
  val parser: P[Json] = {
    val recurse = P(parser)
    val pnull = P("null").map(_ => JNull)
    val bool = P("true").map(_ => JBool.True) | P("false").map(_ => JBool.False)
    val justStr = JsonStringUtil.escapedString('"')
    val str = justStr.map(JString(_))
    val num = Parser.JsonNumber.parser.map(JNumberStr(_))
    val whitespace: P[Unit] = CharIn(" \t\r\n")
    val whitespaces0 = whitespace.rep()

    val listSep = P(whitespaces0 ~ "," ~/ whitespaces0)
    val list = P("[" ~/ whitespaces0 ~ recurse.rep(sep = listSep) ~ whitespaces0 ~ "]")
      .map { vs => JArray(vs.toVector) }

    val kv = justStr ~ whitespaces0 ~ P(":") ~/ whitespaces0 ~ recurse
    val obj = P("{" ~/ whitespaces0 ~ kv.rep(sep = listSep) ~ whitespaces0 ~ "}")
      .map { vs => JObject(vs.toList) }

    pnull | bool | str | num | list | obj
  }
}
