package org.bykn.bosatsu

import fastparse.all._

import Parser.{maybeSpacesAndLines, spacesAndLines}
import org.typelevel.paiges.{Doc, Document}

/**
 * Represents the list construction sublanguage
 */
sealed abstract class ListLang[A]
object ListLang {
  sealed abstract class SpliceOrItem[A] {
    def value: A
  }
  object SpliceOrItem {
    case class Splice[A](value: A) extends SpliceOrItem[A]
    case class Item[A](value: A) extends SpliceOrItem[A]

    def parser[A](pa: P[A]): P[SpliceOrItem[A]] =
      P("*" ~ pa).map(Splice(_)) | pa.map(Item(_))

    implicit def document[A](implicit A: Document[A]): Document[SpliceOrItem[A]] =
      Document.instance[SpliceOrItem[A]] {
        case Splice(a) => Doc.char('*') + A.document(a)
        case Item(a) => A.document(a)
      }
  }

  case class Cons[A](items: List[SpliceOrItem[A]]) extends ListLang[A]
  case class Comprehension[A](expr: SpliceOrItem[A], binding: A, in: A, filter: Option[A]) extends ListLang[A]

  def parser[A](pa: P[A]): P[ListLang[A]] = {
    val sia = SpliceOrItem.parser(pa) ~ maybeSpacesAndLines
    val comma = P("," ~ maybeSpacesAndLines)
    val cons = sia
      .rep(sep = comma)
      .map { tail => { a: SpliceOrItem[A] => Cons(a :: tail.toList) } }
      .opaque("ConsListTail")

    val filterExpr = P("if" ~ spacesAndLines ~/ pa).?
    val comp = P(
      "for" ~ spacesAndLines ~ pa ~ spacesAndLines ~
      "in" ~ spacesAndLines ~ pa ~ maybeSpacesAndLines ~
      filterExpr)
        .map { case (b, i, f) =>
          { e: SpliceOrItem[A] => Comprehension(e, b, i, f) }
        }
        .opaque("ListComprehension")

    val inner = (comma ~ cons) | comp

    P("[" ~ maybeSpacesAndLines ~ (sia ~ inner.?).? ~ maybeSpacesAndLines ~ "]")
      .map {
        case None => Cons(Nil)
        case Some((a, None)) => Cons(a :: Nil)
        case Some((a, Some(rest))) => rest(a)
      }
  }

  implicit def document[A](implicit A: Document[A]): Document[ListLang[A]] =
    Document.instance[ListLang[A]] {
      case Cons(items) =>
        Doc.char('[') + Doc.intercalate(Doc.text(", "),
          items.map(SpliceOrItem.document(A).document(_))) +
          Doc.char(']')
      case Comprehension(e, b, i, f) =>
        val filt = f match {
          case None => Doc.empty
          case Some(e) => Doc.text(" if ") + A.document(e)
        }
        Doc.char('[') + SpliceOrItem.document(A).document(e) + Doc.text(" for ") +
          A.document(b) + Doc.text(" in ") +
          A.document(i) + filt +
          Doc.char(']')
    }
}

