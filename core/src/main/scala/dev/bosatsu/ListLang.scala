package dev.bosatsu

import cats.{Apply, Functor}
import Parser.{maybeSpacesAndCommentLines, spacesAndCommentLines, Combinators}
import org.typelevel.paiges.{Doc, Document}
import cats.parse.{Parser => P}

import cats.implicits._

/** Represents the list construction sublanguage A is the expression type, B is
  * the pattern type for bindings
  */
sealed abstract class ListLang[F[_], A, +B]
object ListLang {
  sealed abstract class SpliceOrItem[A] {
    def value: A

    def map[B](fn: A => B): SpliceOrItem[B]
    def traverse[F[_]: Functor, B](fn: A => F[B]): F[SpliceOrItem[B]]

  }
  object SpliceOrItem {
    case class Splice[A](value: A) extends SpliceOrItem[A] {
      def map[B](fn: A => B): Splice[B] = Splice(fn(value))
      def traverse[F[_]: Functor, B](fn: A => F[B]): F[SpliceOrItem[B]] =
        Functor[F].map(fn(value))(Splice(_))
    }
    case class Item[A](value: A) extends SpliceOrItem[A] {
      def map[B](fn: A => B): Item[B] = Item(fn(value))
      def traverse[F[_]: Functor, B](fn: A => F[B]): F[SpliceOrItem[B]] =
        Functor[F].map(fn(value))(Item(_))
    }

    def parser[A](pa: P[A]): P[SpliceOrItem[A]] =
      (P.char('*') *> pa)
        .map(Splice(_))
        .orElse(pa.map(Item(_)))

    implicit def document[A](implicit
        A: Document[A]
    ): Document[SpliceOrItem[A]] =
      Document.instance[SpliceOrItem[A]] {
        case Splice(a) => Doc.char('*') + A.document(a)
        case Item(a)   => A.document(a)
      }
  }

  case class KVPair[A](key: A, value: A) {
    def map[B](fn: A => B): KVPair[B] = KVPair(fn(key), fn(value))
    def traverse[F[_]: Apply, B](fn: A => F[B]): F[KVPair[B]] =
      Apply[F].map2(fn(key), fn(value))(KVPair(_, _))
  }

  object KVPair {
    private val sep: Doc = Doc.text(": ")

    def parser[A](p: P[A]): P[KVPair[A]] =
      ((p <* maybeSpacesAndCommentLines <* P.char(
        ':'
      ) <* maybeSpacesAndCommentLines) ~ p)
        .map { case (k, v) => KVPair(k, v) }

    implicit def document[A](implicit A: Document[A]): Document[KVPair[A]] =
      Document.instance[KVPair[A]] { case KVPair(k, v) =>
        A.document(k) + sep + A.document(v)
      }
  }

  case class Cons[F[_], A](items: List[F[A]]) extends ListLang[F, A, Nothing]
  case class Comprehension[F[_], A, B](
      expr: F[A],
      binding: B,
      in: A,
      filter: Option[A]
  ) extends ListLang[F, A, B]

  def parser[A, B](
      pa: P[A],
      psrc: P[A],
      pbind: P[B]
  ): P[ListLang[SpliceOrItem, A, B]] =
    genParser(P.char('['), SpliceOrItem.parser(pa), psrc, pbind, P.char(']'))

  def dictParser[A, B](
      pa: P[A],
      psrc: P[A],
      pbind: P[B]
  ): P[ListLang[KVPair, A, B]] =
    genParser(P.char('{'), KVPair.parser(pa), psrc, pbind, P.char('}'))

  def genParser[F[_], A, B](
      left: P[Unit],
      fa: P[F[A]],
      pa: P[A],
      pbind: P[B],
      right: P[Unit]
  ): P[ListLang[F, A, B]] = {
    // construct the tail of a list, so we will finally have at least one item
    val consTail =
      fa.nonEmptyListOfWs(maybeSpacesAndCommentLines).?.map { tail =>
        val listTail = tail match {
          case None     => Nil
          case Some(ne) => ne.toList
        }

        { (a: F[A]) => Cons(a :: listTail) }
      }

    val filterExpr = P.string("if") *> spacesAndCommentLines *> pa

    val comp =
      (
        P.string(
          "for"
        ) *> spacesAndCommentLines *> pbind <* maybeSpacesAndCommentLines,
        P.string(
          "in"
        ) *> spacesAndCommentLines *> pa <* maybeSpacesAndCommentLines,
        filterExpr.?
      )
        .mapN((b, i, f) => (e: F[A]) => Comprehension(e, b, i, f))

    val commaCons = P.char(',') *> maybeSpacesAndCommentLines *> consTail
    val inner =
      commaCons.orElse(spacesAndCommentLines.soft *> commaCons.orElse(comp))

    (left *> maybeSpacesAndCommentLines *> (fa ~ inner.?).? <* maybeSpacesAndCommentLines <* right)
      .map {
        case None                  => Cons(Nil)
        case Some((a, None))       => Cons(a :: Nil)
        case Some((a, Some(rest))) => rest(a)
      }
  }

  def genDocument[F[_], A, B](left: Doc, right: Doc)(implicit
      F: Document[F[A]],
      A: Document[A],
      B: Document[B]
  ): Document[ListLang[F, A, B]] =
    Document.instance[ListLang[F, A, B]] {
      case Cons(items) =>
        left + Doc.intercalate(Doc.text(", "), items.map(F.document(_))) +
          right
      case Comprehension(e, b, i, f) =>
        val filt = f match {
          case None    => Doc.empty
          case Some(e) => Doc.text(" if ") + A.document(e)
        }
        left + F.document(e) + Doc.text(" for ") +
          B.document(b) + Doc.text(" in ") +
          A.document(i) + filt +
          right
    }

  implicit def document[A, B](implicit
      A: Document[A],
      B: Document[B]
  ): Document[ListLang[SpliceOrItem, A, B]] =
    genDocument[SpliceOrItem, A, B](Doc.char('['), Doc.char(']'))

  implicit def documentDict[A, B](implicit
      A: Document[A],
      B: Document[B]
  ): Document[ListLang[KVPair, A, B]] =
    genDocument[KVPair, A, B](Doc.char('{'), Doc.char('}'))
}
