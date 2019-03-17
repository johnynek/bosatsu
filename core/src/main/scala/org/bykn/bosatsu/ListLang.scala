package org.bykn.bosatsu

import fastparse.all._

import Parser.{maybeSpacesAndLines, spacesAndLines, Combinators}
import org.typelevel.paiges.{Doc, Document}

/**
 * Represents the list construction sublanguage
 * A is the expression type, B is the pattern type for bindings
 */
sealed abstract class ListLang[A, +B]
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

  case class Cons[A](items: List[SpliceOrItem[A]]) extends ListLang[A, Nothing]
  case class Comprehension[A, B](expr: SpliceOrItem[A], binding: B, in: A, filter: Option[A]) extends ListLang[A, B]

  def parser[A, B](pa: P[A], pbind: P[B]): P[ListLang[A, B]] = {
    val sia = SpliceOrItem.parser(pa)
    // construct the tail of a list, so we will finally have at least one item
    val consTail = sia.nonEmptyListOfWs(maybeSpacesAndLines, 1).?
      .map { tail =>
        val listTail = tail match {
          case None => Nil
          case Some(ne) => ne.toList
        }

        { a: SpliceOrItem[A] => Cons(a :: listTail) }
      }
      .opaque("ConsListTail")

    val filterExpr = P("if" ~ spacesAndLines ~ pa)
    // TODO, we don't know if the parsers absorb trailing spaces, they probably
    // shouldn't but currently it looks like they are
    val comp = P(
      "for" ~ spacesAndLines ~/ pbind ~ maybeSpacesAndLines ~
      "in" ~ spacesAndLines ~ pa ~ (maybeSpacesAndLines ~ filterExpr).?)
        .map { case (b, i, f) =>
          { e: SpliceOrItem[A] => Comprehension(e, b, i, f) }
        }
        .opaque("ListComprehension")

    val commaCons = ("," ~ maybeSpacesAndLines ~ consTail)
    val inner = commaCons | (spacesAndLines ~ (commaCons | comp))

    P("[" ~ maybeSpacesAndLines ~ (sia ~ inner.?).? ~ maybeSpacesAndLines ~ "]")
      .map {
        case None => Cons(Nil)
        case Some((a, None)) => Cons(a :: Nil)
        case Some((a, Some(rest))) => rest(a)
      }
  }

  implicit def document[A, B](implicit A: Document[A], B: Document[B]): Document[ListLang[A, B]] =
    Document.instance[ListLang[A, B]] {
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
          B.document(b) + Doc.text(" in ") +
          A.document(i) + filt +
          Doc.char(']')
    }
}

