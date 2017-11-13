package org.bykn.edgemar

import cats.data.NonEmptyList
import fastparse.all._
import org.typelevel.paiges.{Doc, Document}
import Parser.{lowerIdent, upperIdent, spaces, maybeSpace, Combinators}

case class PackageName(parts: NonEmptyList[String]) {
  def asString: String = parts.toList.mkString("/")
}

object PackageName {
  implicit val document: Document[PackageName] =
    Document.instance[PackageName] { pn => Doc.text(pn.asString) }

  implicit val parser: P[PackageName] =
    P(upperIdent ~ ("/" ~ upperIdent).rep()).map { case (head, tail) =>
      PackageName(NonEmptyList(head, tail.toList))
    }
}

sealed abstract class ImportedName
object ImportedName {
  case class OriginalName(asString: String) extends ImportedName
  case class Renamed(remote: String, local: String) extends ImportedName

  implicit val document: Document[ImportedName] =
    Document.instance[ImportedName] {
      case ImportedName.OriginalName(nm) => Doc.text(nm)
      case ImportedName.Renamed(from, to) => Doc.text(from) + Doc.text(" as ") + Doc.text(to)
    }

  val parser: P[ImportedName] = {
    def basedOn(of: P[String]): P[ImportedName] =
      P(of ~ (spaces ~ "as" ~ spaces ~ of).?).map {
        case (from, Some(to)) => ImportedName.Renamed(from, to)
        case (orig, None) => ImportedName.OriginalName(orig)
      }

    basedOn(lowerIdent) | basedOn(upperIdent)
  }
}

case class Import(packageName: PackageName, items: NonEmptyList[ImportedName])

object Import {
  implicit val document: Document[Import] =
    Document.instance[Import] { case Import(pname, items) =>
      Doc.text("import ") + Document[PackageName].document(pname) + Doc.space +
        // TODO: use paiges to pack this in nicely using .group or something
        Doc.char('[') + Doc.intercalate(Doc.text(", "), items.toList.map(Document[ImportedName].document _)) + Doc.char(']')
    }

  val parser: P[Import] = {
    P("import" ~ spaces ~/ PackageName.parser ~ maybeSpace ~
      ImportedName.parser.nonEmptyListSyntax).map { case (pname, imported) =>
        Import(pname, imported)
      }
  }
}

sealed abstract class ExportedName
object ExportedName {
  case class Binding(name: String) extends ExportedName
  case class TypeName(name: String) extends ExportedName
  case class Constructor(name: String) extends ExportedName

  private[this] val consDoc = Doc.text("()")

  implicit val document: Document[ExportedName] =
    Document.instance[ExportedName] {
      case Binding(n) => Doc.text(n)
      case TypeName(n) => Doc.text(n)
      case Constructor(n) => Doc.text(n) + consDoc
    }

  val parser: P[ExportedName] =
    lowerIdent.map(Binding(_)) |
      P(upperIdent ~ "()".!.?).map {
        case (n, None) => TypeName(n)
        case (n, Some(_)) => Constructor(n)
      }
}

case class Package(name: PackageName, imports: List[Import], exports: List[ExportedName], body: Statement)

object Package {
  implicit val document: Document[Package] =
    Document.instance[Package] { case Package(name, imports, exports, body) =>
      val p = Doc.text("package ") + Document[PackageName].document(name) + Doc.line
      val i = imports match {
        case Nil => Doc.empty
        case nonEmptyImports =>
          Doc.intercalate(Doc.line, nonEmptyImports.map(Document[Import].document _)) + Doc.line
      }
      val e = exports match {
        case Nil => Doc.empty
        case nonEmptyExports =>
          Doc.text("export ") + Doc.text("[ ") +
          Doc.intercalate(Doc.text(", "), nonEmptyExports.map(Document[ExportedName].document _)) + Doc.text(" ]") + Doc.line
      }
      val b = Document[Statement].document(body)
      // add an extra line between each group
      Doc.intercalate(Doc.line, List(p, i, e, b))
    }

  val parser: P[Package] = {
    val pname = Padding.parser(P("package" ~ spaces ~ PackageName.parser)).map(_.padded)
    val im = Padding.parser(Import.parser).map(_.padded).rep().map(_.toList)
    val ex = Padding.parser(P("export" ~ maybeSpace ~ ExportedName.parser.nonEmptyListSyntax)).map(_.padded)
    val body = Padding.parser(Statement.parser).map(_.padded)
    (pname ~ im ~ Parser.nonEmptyListToList(ex) ~ body).map { case (p, i, e, b) =>
      Package(p, i, e, b)
    }
  }
}
