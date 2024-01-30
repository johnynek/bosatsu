package org.bykn.bosatsu

import cats.{Foldable, Functor}
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P}
import org.typelevel.paiges.{Doc, Document}

import Parser.{spaces, Combinators}

sealed abstract class ImportedName[+T] {
  def originalName: Identifier
  def localName: Identifier
  def tag: T
  def isRenamed: Boolean = originalName != localName

  def map[U](fn: T => U): ImportedName[U] =
    this match {
      case ImportedName.OriginalName(n, t) =>
        ImportedName.OriginalName(n, fn(t))
      case ImportedName.Renamed(o, l, t) =>
        ImportedName.Renamed(o, l, fn(t))
    }

  def traverse[F[_], U](
      fn: T => F[U]
  )(implicit F: Functor[F]): F[ImportedName[U]] =
    this match {
      case ImportedName.OriginalName(n, t) =>
        F.map(fn(t))(ImportedName.OriginalName(n, _))
      case ImportedName.Renamed(o, l, t) =>
        F.map(fn(t))(ImportedName.Renamed(o, l, _))
    }
}

object ImportedName {
  case class OriginalName[T](originalName: Identifier, tag: T)
      extends ImportedName[T] {
    def localName = originalName
  }
  case class Renamed[T](originalName: Identifier, localName: Identifier, tag: T)
      extends ImportedName[T]

  implicit val document: Document[ImportedName[Unit]] =
    Document.instance[ImportedName[Unit]] {
      case ImportedName.OriginalName(nm, _) => Document[Identifier].document(nm)
      case ImportedName.Renamed(from, to, _) =>
        Document[Identifier].document(from) + Doc.text(" as ") +
          Document[Identifier].document(to)
    }

  val parser: P[ImportedName[Unit]] = {
    def basedOn(of: P[Identifier]): P[ImportedName[Unit]] =
      (of ~ (spaces.soft *> P.string("as") *> spaces *> of).?)
        .map {
          case (from, Some(to)) => ImportedName.Renamed(from, to, ())
          case (orig, None)     => ImportedName.OriginalName(orig, ())
        }

    basedOn(Identifier.bindableParser)
      .orElse(basedOn(Identifier.consParser))
  }
}

case class Import[A, B](pack: A, items: NonEmptyList[ImportedName[B]]) {
  def resolveToGlobal: Map[Identifier, (A, Identifier)] =
    items.foldLeft(Map.empty[Identifier, (A, Identifier)]) {
      case (m0, impName) =>
        m0.updated(impName.localName, (pack, impName.originalName))
    }
}

object Import {
  implicit val document: Document[Import[PackageName, Unit]] =
    Document.instance[Import[PackageName, Unit]] { case Import(pname, items) =>
      val itemDocs = items.toList.map(Document[ImportedName[Unit]].document _)

      Doc.text("from") + Doc.space + Document[PackageName].document(
        pname
      ) + Doc.space + Doc.text("import") +
        // TODO: use paiges to pack this in nicely using .group or something
        Doc.space + Doc.intercalate(Doc.text(", "), itemDocs)
    }

  val parser: P[Import[PackageName, Unit]] = {
    val pyimps = ImportedName.parser.itemsMaybeParens.map(_._2)

    (
      (P.string("from") ~ spaces).backtrack *> PackageName.parser <* spaces,
      P.string("import") *> spaces *> pyimps
    )
      .mapN(Import(_, _))
  }

  /** This only keeps the last name if there are duplicate local names checking
    * for duplicate local names should be done at another layer
    */
  def locals[F[_]: Foldable, A, B, C](
      imp: Import[A, F[B]]
  )(pn: PartialFunction[B, C]): Map[Identifier, C] = {
    val fn = pn.lift
    imp.items.foldLeft(Map.empty[Identifier, C]) { case (m0, impName) =>
      impName.tag.foldLeft(m0) { (m1, b) =>
        fn(b) match {
          case None    => m1
          case Some(c) => m1.updated(impName.localName, c)
        }
      }
    }
  }
}

/** There are all the distinct imported names and the original ImportedName
  */
case class ImportMap[A, B](toMap: Map[Identifier, (A, ImportedName[B])]) {
  def apply(name: Identifier): Option[(A, ImportedName[B])] =
    toMap.get(name)

  def +(that: (A, ImportedName[B])): ImportMap[A, B] =
    ImportMap(toMap.updated(that._2.localName, that))
}

object ImportMap {
  def empty[A, B]: ImportMap[A, B] = ImportMap(Map.empty)
  // Return the list of collisions in local names along with a map
  // with the last name overwriting the import
  def fromImports[A, B](
      is: List[Import[A, B]]
  ): (List[(A, ImportedName[B])], ImportMap[A, B]) =
    is.iterator
      .flatMap { case Import(p, is) => is.toList.iterator.map((p, _)) }
      .foldLeft((List.empty[(A, ImportedName[B])], ImportMap.empty[A, B])) {
        case ((dups, imap), pim @ (_, im)) =>
          val dups1 = imap(im.localName) match {
            case Some(nm) => nm :: dups
            case None     => dups
          }

          (dups1, imap + pim)
      }
}
