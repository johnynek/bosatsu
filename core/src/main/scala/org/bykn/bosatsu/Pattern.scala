package org.bykn.bosatsu

import Parser.{ Combinators, lowerIdent, upperIdent }
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

// TODO, in the future, we could recursively have patterns in the args
case class Pattern[T](typeName: T, bindings: List[Option[String]]) {
  def map[U](fn: T => U): Pattern[U] =
    Pattern(fn(typeName), bindings)
}

object Pattern {
  implicit val document: Document[Pattern[String]] =
    Document.instance[Pattern[String]] {
      case Pattern(n, Nil) => Doc.text(n)
      case Pattern(n, nonEmpty) =>
        def bind(o: Option[String]): Doc = o.fold(Doc.char('_'))(Doc.text)
        Doc.text(n) +
          Doc.char('(') + Doc.intercalate(Doc.text(", "), nonEmpty.map(bind)) + Doc.char(')')
    }
  val parser: P[Pattern[String]] = {
    val item = lowerIdent.map(Some(_)) | P("_").map(_ => None)
    P(upperIdent ~ (item.listN(1).parens).?)
      .map {
        case (n, None) => Pattern(n, Nil)
        case (n, Some(ls)) => Pattern(n, ls)
      }
  }
}

