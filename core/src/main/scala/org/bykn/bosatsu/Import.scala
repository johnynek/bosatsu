package org.bykn.bosatsu

import cats.data.NonEmptyList
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}

import Parser.{lowerIdent, upperIdent, spaces, maybeSpace, Combinators}

sealed abstract class ImportedName[T] {
  def originalName: String
  def localName: String
  def tag: T
  def setTag(t: T): ImportedName[T]
  def isRenamed: Boolean = originalName != localName
}

object ImportedName {
  case class OriginalName[T](originalName: String, tag: T) extends ImportedName[T] {
    def localName = originalName
    def setTag(t: T): ImportedName[T] = OriginalName(originalName, t)
  }
  case class Renamed[T](originalName: String, localName: String, tag: T) extends ImportedName[T] {
    def setTag(t: T): ImportedName[T] = Renamed(originalName, localName, t)
  }

  implicit val document: Document[ImportedName[Unit]] =
    Document.instance[ImportedName[Unit]] {
      case ImportedName.OriginalName(nm, _) => Doc.text(nm)
      case ImportedName.Renamed(from, to, _) => Doc.text(from) + Doc.text(" as ") + Doc.text(to)
    }

  val parser: P[ImportedName[Unit]] = {
    def basedOn(of: P[String]): P[ImportedName[Unit]] =
      P(of ~ (spaces ~ "as" ~ spaces ~ of).?).map {
        case (from, Some(to)) => ImportedName.Renamed(from, to, ())
        case (orig, None) => ImportedName.OriginalName(orig, ())
      }

    basedOn(lowerIdent) | basedOn(upperIdent)
  }
}

case class Import[A, B](pack: A, items: NonEmptyList[ImportedName[B]])

object Import {
  implicit val document: Document[Import[PackageName, Unit]] =
    Document.instance[Import[PackageName, Unit]] { case Import(pname, items) =>
      Doc.text("import ") + Document[PackageName].document(pname) + Doc.space +
        // TODO: use paiges to pack this in nicely using .group or something
        Doc.char('[') + Doc.intercalate(Doc.text(", "), items.toList.map(Document[ImportedName[Unit]].document _)) + Doc.char(']')
    }

  val parser: P[Import[PackageName, Unit]] = {
    P("import" ~ spaces ~/ PackageName.parser ~ maybeSpace ~
      ImportedName.parser.nonEmptyListSyntax).map { case (pname, imported) =>
        Import(pname, imported)
      }
  }
}

/**
 * There are all the distinct imported names and the original ImportedName
 */
case class ImportMap[A, B](toMap: Map[String, (A, ImportedName[B])]) {
  def apply(name: String): Option[(A, ImportedName[B])] =
    toMap.get(name)

  def +(that: (A, ImportedName[B])): ImportMap[A, B] =
    ImportMap(toMap.updated(that._2.localName, that))
}

object ImportMap {
  def empty[A, B]: ImportMap[A, B] = ImportMap(Map.empty)
  // Return the list of collisions in local names along with a map
  // with the last name overwriting the import
  def fromImports[A, B](is: List[Import[A, B]]): (List[(A, ImportedName[B])], ImportMap[A, B]) =
    is.iterator
      .flatMap { case Import(p, is) => is.toList.iterator.map((p, _)) }
      .foldLeft((List.empty[(A, ImportedName[B])], ImportMap.empty[A, B])) {
        case ((dups, imap), pim@(pack, im)) =>
          val dups1 = imap(im.localName) match {
            case Some(nm) => nm :: dups
            case None => dups
          }

          (dups1, imap + pim)
        }
}
