package dev.bosatsu.edn

import cats.parse.{Parser => P, Parser0 => P0}
import dev.bosatsu.Parser
import org.typelevel.paiges.{Doc, Document}

enum Edn {
  case ENil
  case EBool(value: Boolean)
  case EString(value: String)
  case ESymbol(value: String)
  case EKeyword(value: String)
  case EList(items: List[Edn])
  case EVector(items: List[Edn])
  case EMap(items: List[(Edn, Edn)])
}

object Edn {
  private def isTerminator(c: Char): Boolean =
    c.isWhitespace ||
      c == ',' || c == '(' || c == ')' || c == '[' || c == ']' ||
      c == '{' || c == '}' || c == '"' || c == ';'

  private val ws0: P0[Unit] =
    P.charIn(" \n\r\t,").rep0.void

  private val token: P[String] =
    P.charsWhile(ch => !isTerminator(ch) && ch != ':')

  private val keyword: P[Edn] =
    (P.char(':') *> token).map(EKeyword(_))

  private val symbolOrConst: P[Edn] =
    token.map {
      case "nil"   => ENil
      case "true"  => EBool(true)
      case "false" => EBool(false)
      case other   => ESymbol(other)
    }

  private val str: P[Edn] =
    Parser.escapedString('"').map(EString(_))

  val parser: P[Edn] =
    P.recursive[Edn] { recurse =>
      val item: P[Edn] = recurse <* ws0
      val items0: P0[List[Edn]] = ws0 *> item.rep0

      val list: P[Edn] =
        (P.char('(') *> items0 <* P.char(')')).map(EList(_))
      val vec: P[Edn] =
        (P.char('[') *> items0 <* P.char(']')).map(EVector(_))
      val mapLike: P[Edn] =
        (P.char('{') *> items0 <* P.char('}')).mapFilter { ls =>
          if ((ls.length % 2) == 0) {
            val pairs = ls.grouped(2).collect {
              case k :: v :: Nil => (k, v)
            }.toList
            Some(EMap(pairs))
          } else None
        }

      list
        .orElse(vec)
        .orElse(mapLike)
        .orElse(keyword)
        .orElse(str)
        .orElse(symbolOrConst)
    }

  def parseAll(input: String): Either[P.Error, Edn] =
    parser.parseAll(input)

  private def block(open: Char, close: Char, docs: List[Doc]): Doc =
    docs match {
      case Nil =>
        Doc.char(open) + Doc.char(close)
      case _ =>
        (Doc.char(open) + Doc.intercalate(Doc.line, docs) + Doc.char(close))
          .nested(2)
          .grouped
    }

  private def escapeEdnString(str: String): String = {
    val sb = new java.lang.StringBuilder(str.length)
    str.foreach {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case c if Character.isISOControl(c) =>
        sb.append(f"\\u${c.toInt}%04x")
      case c =>
        sb.append(c)
    }
    sb.toString
  }

  private def renderString(str: String): Doc =
    Doc.char('"') + Doc.text(escapeEdnString(str)) + Doc.char('"')

  private def docOf(edn: Edn): Doc =
    edn match {
      case _: ENil.type   => Doc.text("nil")
      case EBool(true)    => Doc.text("true")
      case EBool(false)   => Doc.text("false")
      case EString(value) => renderString(value)
      case ESymbol(value) => Doc.text(value)
      case EKeyword(value) =>
        Doc.char(':') + Doc.text(value)
      case EList(items) =>
        block('(', ')', items.map(toDoc))
      case EVector(items) =>
        block('[', ']', items.map(toDoc))
      case EMap(items) =>
        val docs = items.map { case (k, v) =>
          docOf(k) + Doc.space + docOf(v)
        }
        block('{', '}', docs)
    }

  given documentEdn: Document[Edn] =
    Document.instance(docOf)

  def toDoc(edn: Edn): Doc =
    Document[Edn].document(edn)
}
