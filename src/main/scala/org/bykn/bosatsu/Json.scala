package org.bykn.bosatsu

import org.typelevel.paiges.Doc

/**
 * A simple JSON ast for output
 */
sealed abstract class Json {
  def toDoc: Doc
}

object Json {
  import Doc.{text, str}

  def escape(str: String): String =
    str.flatMap {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '"' => "\""
      case other => other.toString
    }

  case class JString(str: String) extends Json {
    def toDoc = text("\"%s\"".format(escape(str)))
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
  case class JObject(toMap: Map[String, Json]) extends Json {
    def toDoc = {
      val kvs = toMap.map { case (s, j) =>
        JString(s).toDoc + text(":") + ((Doc.lineOrSpace + j.toDoc).nested(2))
      }
      val parts = Doc.intercalate(Doc.comma + Doc.line, kvs).grouped
      parts.bracketBy(text("{"), text("}"))
    }
  }
}

