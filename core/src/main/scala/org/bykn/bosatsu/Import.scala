package org.bykn.bosatsu

import cats.Foldable
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import cats.syntax.all._
import org.typelevel.paiges.{Doc, Document}
import Parser.{spaces, Combinators}

case class Import[A, B](pack: A, items: NonEmptyList[ImportedName[B]]) {
  def resolveToGlobal: Map[Identifier, (A, Identifier)] =
    items.foldLeft(Map.empty[Identifier, (A, Identifier)]) {
      case (m0, impName) =>
        m0.updated(impName.localName, (pack, impName.originalName))
    }

  def mapFilter[C](
      fn: (A, ImportedName[B]) => Option[ImportedName[C]]
  ): Option[Import[A, C]] =
    NonEmptyList.fromList(items.toList.flatMap(in => fn(pack, in))) match {
      case Some(i1) => Some(Import(pack, i1))
      case None     => None
    }
}

object Import {
  implicit val document: Document[Import[PackageName, Unit]] =
    Document.instance[Import[PackageName, Unit]] { case Import(pname, items) =>
      val itemDocs = items.toList.map(Document[ImportedName[Unit]].document)

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
