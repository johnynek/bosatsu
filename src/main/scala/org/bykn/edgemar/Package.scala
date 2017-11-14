package org.bykn.edgemar

import cats.data.{NonEmptyList, Validated, ValidatedNel, ReaderT}
import cats.implicits._
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

// TODO: attach a tag so we have ImportedName[T], which is initially maybe
// Unit, but later becomes Expr after we can fully populate the DAG
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

case class Import[A](pack: A, items: NonEmptyList[ImportedName])

object Import {
  implicit val document: Document[Import[PackageName]] =
    Document.instance[Import[PackageName]] { case Import(pname, items) =>
      Doc.text("import ") + Document[PackageName].document(pname) + Doc.space +
        // TODO: use paiges to pack this in nicely using .group or something
        Doc.char('[') + Doc.intercalate(Doc.text(", "), items.toList.map(Document[ImportedName].document _)) + Doc.char(']')
    }

  val parser: P[Import[PackageName]] = {
    P("import" ~ spaces ~/ PackageName.parser ~ maybeSpace ~
      ImportedName.parser.nonEmptyListSyntax).map { case (pname, imported) =>
        Import(pname, imported)
      }
  }
}

// TODO: attach a tag so we have ExportedName[T], which is initially maybe
// Unit, but later becomes Either[Expr[_], DefinedType] after we can fully populate the DAG
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

case class Package[A](name: PackageName, imports: List[Import[A]], exports: List[ExportedName], body: Statement)

object Package {

  implicit val document: Document[Package[PackageName]] =
    Document.instance[Package[PackageName]] { case Package(name, imports, exports, body) =>
      val p = Doc.text("package ") + Document[PackageName].document(name) + Doc.line
      val i = imports match {
        case Nil => Doc.empty
        case nonEmptyImports =>
          Doc.intercalate(Doc.line, nonEmptyImports.map(Document[Import[PackageName]].document _)) + Doc.line
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

  val parser: P[Package[PackageName]] = {
    val pname = Padding.parser(P("package" ~ spaces ~ PackageName.parser)).map(_.padded)
    val im = Padding.parser(Import.parser).map(_.padded).rep().map(_.toList)
    val ex = Padding.parser(P("export" ~ maybeSpace ~ ExportedName.parser.nonEmptyListSyntax)).map(_.padded)
    val body = Padding.parser(Statement.parser).map(_.padded)
    (pname ~ im ~ Parser.nonEmptyListToList(ex) ~ body).map { case (p, i, e, b) =>
      Package(p, i, e, b)
    }
  }
}

case class PackageMap[A](toMap: Map[PackageName, Package[A]])

object PackageMap {
  def build(ps: Iterable[Package[PackageName]]): ValidatedNel[PackageError.DuplicatePackages, PackageMap[PackageName]] = {

    def toPackage(it: Iterable[Package[PackageName]]): ValidatedNel[PackageError.DuplicatePackages, Package[PackageName]] =
      it.toList match {
        case Nil => sys.error("unreachable, groupBy never produces an empty list")
        case h :: Nil => Validated.valid(h)
        case h :: h1 :: tail => Validated.invalidNel(PackageError.DuplicatePackages(h, NonEmptyList(h1, tail)))
      }

    ps.groupBy(_.name).traverse(toPackage _).map(PackageMap(_))
  }

  def resolve(map: PackageMap[PackageName]): ValidatedNel[PackageError, PackageMap[Fix[Package]]] = {
    def getPackage(i: Import[PackageName], from: Package[PackageName]): ValidatedNel[PackageError, Import[Package[PackageName]]] =
      map.toMap.get(i.pack) match {
        case None => Validated.invalidNel(PackageError.UnknownImport(i.pack, from))
        case Some(pack) => Validated.valid(Import(pack, i.items))
      }

    def step(p: Package[PackageName]): ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], Package[Fix[Package]]] = {
      val edeps = ReaderT.ask[Either[NonEmptyList[PackageError], ?], List[PackageName]]
        .flatMapF {
          case nonE@(h :: tail) if nonE.contains(p.name) =>
            Left(NonEmptyList.of(PackageError.CircularDependency(p, NonEmptyList(h, tail))))
          case _ =>
            val deps = p.imports.traverse(getPackage(_, p)) // the packages p depends on
            deps.toEither
        }

      edeps
        .flatMap { deps =>
          deps.traverse { i =>
            step(i.pack)
              .local[List[PackageName]](p.name :: _) // add this package into the path of all the deps
              .map { p => Import(Fix(p), i.items) }
          }
          .map { imports =>
            Package(p.name, imports, p.exports, p.body)
          }
        }
    }

    type M = Map[PackageName, Package[Fix[Package]]]
    val r: ReaderT[Either[NonEmptyList[PackageError], ?], List[PackageName], M] =
      map.toMap.traverse(step)
    val m: Either[NonEmptyList[PackageError], M] = r.run(Nil)
    m.map(PackageMap(_)).toValidated
  }

  def resolveAll(ps: Iterable[Package[PackageName]]): ValidatedNel[PackageError, PackageMap[Fix[Package]]] =
    build(ps).andThen(resolve(_))
}

sealed abstract class PackageError
object PackageError {
  case class UnknownExport(ex: ExportedName, in: Program[Declaration, Statement]) extends PackageError
  case class UnknownImport(pack: PackageName, from: Package[PackageName]) extends PackageError
  case class DuplicatePackages(head: Package[PackageName], collisions: NonEmptyList[Package[PackageName]]) extends PackageError
  case class CircularDependency(from: Package[PackageName], path: NonEmptyList[PackageName]) extends PackageError
}
