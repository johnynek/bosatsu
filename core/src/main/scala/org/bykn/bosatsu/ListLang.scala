package org.bykn.bosatsu

import fastparse.all._

import Parser.{maybeSpacesAndLines, spacesAndLines, Combinators}
import org.typelevel.paiges.{Doc, Document}

/**
 * Represents the list construction sublanguage
 * A is the expression type, B is the pattern type for bindings
 */
sealed abstract class ListLang[F[_], A, +B]
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

  case class KVPair[A](key: A, value: A)

  object KVPair {
    private[this] val sep: Doc = Doc.text(": ")

    def parser[A](p: P[A]): P[KVPair[A]] =
      P(p ~ maybeSpacesAndLines ~ ":" ~ maybeSpacesAndLines ~ p)
        .map { case (k, v) => KVPair(k, v) }

    implicit def document[A](implicit A: Document[A]): Document[KVPair[A]] =
      Document.instance[KVPair[A]] {
        case KVPair(k, v) => A.document(k) + sep + A.document(v)
      }
  }

  case class Cons[F[_], A](items: List[F[A]]) extends ListLang[F, A, Nothing]
  case class Comprehension[F[_], A, B](expr: F[A], binding: B, in: A, filter: Option[A]) extends ListLang[F, A, B]

  def parser[A, B](pa: P[A], pbind: P[B]): P[ListLang[SpliceOrItem, A, B]] =
    genParser(P("["), SpliceOrItem.parser(pa), pa, pbind, P("]"))

  def dictParser[A, B](pa: P[A], pbind: P[B]): P[ListLang[KVPair, A, B]] =
    genParser(P("{"), KVPair.parser(pa), pa, pbind, P("}"))

  def genParser[F[_], A, B](left: P[Unit], fa: P[F[A]], pa: P[A], pbind: P[B], right: P[Unit]): P[ListLang[F, A, B]] = {
    val sia = fa
    // construct the tail of a list, so we will finally have at least one item
    val consTail = sia.nonEmptyListOfWs(maybeSpacesAndLines).?
      .map { tail =>
        val listTail = tail match {
          case None => Nil
          case Some(ne) => ne.toList
        }

        { a: F[A] => Cons(a :: listTail) }
      }
      .opaque("ConsListTail")

    val filterExpr = P("if" ~ spacesAndLines ~/ pa)
    // TODO, we don't know if the parsers absorb trailing spaces, they probably
    // shouldn't but currently it looks like they are
    val comp = P(
      "for" ~ spacesAndLines ~/ pbind ~ maybeSpacesAndLines ~
      "in" ~ spacesAndLines ~/ pa ~ (maybeSpacesAndLines ~ filterExpr).?)
        .map { case (b, i, f) =>
          { e: F[A] => Comprehension(e, b, i, f) }
        }
        .opaque("ListComprehension")

    val commaCons = ("," ~ maybeSpacesAndLines ~ consTail)
    val inner = commaCons | (spacesAndLines ~ (commaCons | comp))

    P(left ~/ maybeSpacesAndLines ~ (sia ~ inner.?).? ~ maybeSpacesAndLines ~ right)
      .map {
        case None => Cons(Nil)
        case Some((a, None)) => Cons(a :: Nil)
        case Some((a, Some(rest))) => rest(a)
      }
  }

  def genDocument[F[_], A, B](left: Doc, right: Doc)(implicit F: Document[F[A]], A: Document[A], B: Document[B]): Document[ListLang[F, A, B]] =
    Document.instance[ListLang[F, A, B]] {
      case Cons(items) =>
        left + Doc.intercalate(Doc.text(", "),
          items.map(F.document(_))) +
          right
      case Comprehension(e, b, i, f) =>
        val filt = f match {
          case None => Doc.empty
          case Some(e) => Doc.text(" if ") + A.document(e)
        }
        left + F.document(e) + Doc.text(" for ") +
          B.document(b) + Doc.text(" in ") +
          A.document(i) + filt +
          right
    }

  implicit def document[A, B](implicit A: Document[A], B: Document[B]): Document[ListLang[SpliceOrItem, A, B]] =
    genDocument[SpliceOrItem, A, B](Doc.char('['), Doc.char(']'))

  implicit def documentDict[A, B](implicit A: Document[A], B: Document[B]): Document[ListLang[KVPair, A, B]] =
    genDocument[KVPair, A, B](Doc.char('{'), Doc.char('}'))
}

