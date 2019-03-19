package org.bykn.bosatsu

import org.typelevel.paiges.Doc

/**
 * A simple JSON ast for output
 */
sealed abstract class Json {
  def toDoc: Doc

  def render: String = toDoc.render(80)
}

object Json {
  import Doc.{text, str}

  case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(JsonStringUtil.escape('"', str)))
  }
  case class JNumber(toDouble: Double) extends Json {
    def toDoc = str(toDouble)
  }
  case class JNumberStr(asString: String) extends Json {
    def toDoc = text(asString)
  }
  case class JBool(toBoolean: Boolean) extends Json {
    def toDoc = str(toBoolean)
  }
  case object JNull extends Json {
    def toDoc = text("null")
  }
  case class JArray(toVector: Vector[Json]) extends Json {
    def toDoc = {
      val parts = Doc.intercalate(Doc.comma, toVector.map { j => (Doc.line + j.toDoc).grouped })
      "[" +: ((parts :+ " ]").nested(2))
    }
  }
  // we use a List here to preserve the order in which items
  // were given to us
  case class JObject(items: List[(String, Json)]) extends Json {
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
}

