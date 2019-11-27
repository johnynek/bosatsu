package org.bykn.bosatsu

import fastparse.all._
import org.typelevel.paiges.Doc

/**
 * A simple JSON ast for output
 */
sealed abstract class Json {
  def toDoc: Doc

  def render: String = toDoc.render(80)
}

object Json {
  sealed abstract class Num extends Json

  import Doc.{text, str}

  final case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(JsonStringUtil.escape('"', str)))
  }
  final case class JNumber(toDouble: Double) extends Num {
    override def render = toDouble.toString
    def toDoc = str(toDouble)
  }
  final case class JNumberStr(asString: String) extends Num {
    override def render = asString
    def toDoc = text(asString)
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
    val numStr = Parser.jsonNumber.map { str =>
      try {
        // this isn't great... but we don't need speed
        val d = str.toDouble
        if (d.toString == str) JNumber(d)
        else JNumberStr(str)
      }
      catch {
        case _: NumberFormatException => JNumberStr(str)
      }
    }

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
