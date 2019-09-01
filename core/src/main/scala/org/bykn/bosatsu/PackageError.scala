package org.bykn.bosatsu

import cats.data.{Chain, NonEmptyList, Writer}
import org.typelevel.paiges.{Doc, Document}
import rankn.{Infer, Type}

sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)]): String

  protected def getMapSrc(sourceMap: Map[PackageName, (LocationMap, String)], pack: PackageName): (LocationMap, String) =
    sourceMap.get(pack) match {
      case None => (LocationMap(""), "<unknown source>")
      case Some(found) => found
    }
}

object PackageError {
  def showTypes(pack: PackageName, tpes: List[Type]): Map[Type, String] =
    TypeRef.fromTypes(Some(pack), tpes).map { case (k, v) =>
      (k, v.toDoc.render(80))
    }.toMap

  def nearest[A](ident: Identifier, existing: Map[Identifier, A], count: Int): List[(Identifier, A)] =
    existing
      .iterator
      .map { case (i, a) =>
        val d = EditDistance(ident.asString.toIterable, i.asString.toIterable)
        (i, d, a)
      }
      .filter(_._2 < ident.asString.length) // don't show things that require total edits
      .toList
      .sortBy { case (_, d, _) => d }
      .distinct
      .take(count)
      .map { case (i, _, a) => (i, a) }

  case class UnknownExport[A](ex: ExportedName[A],
    in: PackageName,
    lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = getMapSrc(sourceMap, in)
      val header =
        s"in $sourceName unknown export ${ex.name}"
      val candidateMap: Map[Identifier, Region] =
        lets.map { case (n, _, expr) => (n, HasRegion.region(expr)) }.toMap
      val candidates =
        nearest(ex.name, candidateMap, 3)
          .map { case (n, r) =>
            val pos = lm.toLineCol(r.start).map { case (l, c) => s" at line: ${l + 1}, column: ${c + 1}" }.getOrElse("")
            s"${n.asString}$pos"
          }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion =
        if (candidates.nonEmpty) "\n" + s"perhaps you meant:$candstr"
        else ""
      header + suggestion
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, from: Package[PackageName, A, B, C]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (_, sourceName) = getMapSrc(sourceMap, from.name)
      s"in $sourceName package ${from.name.asString} imports unknown package ${pack.asString}"
    }
  }

  case class DuplicatedImport(duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) =
      duplicates.sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          val (_, sourceName) = getMapSrc(sourceMap, pack)
          s"duplicate import in $sourceName package ${pack.asString} imports ${imp.originalName} as ${imp.localName}"
        }
        .mkString("\n")
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName[A, B](
    in: PackageName,
    importing: Package.Inferred,
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
        val ls = importing
          .program
          .lets

        val (_, sourceName) = getMapSrc(sourceMap, in)
        val letMap = ls.iterator.map { case (n, _, _) => (n: Identifier, ()) }.toMap
        letMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${importing.name.asString} has ${iname.originalName.sourceCodeRepr} but it is not exported. Add to exports"
            case None =>
              val near = nearest(iname.originalName, letMap, 3)
                .map { case (n, _) => n.sourceCodeRepr }
                .mkString(" Nearest: ", ", ", "")
              s"in $sourceName package: ${importing.name.asString} does not have name ${iname.originalName.sourceCodeRepr}.$near"
          }
      }
    }

  case class UnknownImportFromInterface[A, B](
    in: PackageName,
    importing: Package.Interface,
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {

        val (_, sourceName) = getMapSrc(sourceMap, in)
        val exportMap = importing.exports.map { e => (e.name, ()) }.toMap
        val near = nearest(iname.originalName, exportMap, 3)
          .map { case (n, _) => n.asString }
          .mkString(" Nearest: ", ", ", "")
        s"in $sourceName package: ${importing.name} does not have name ${iname.originalName}.$near"
      }
    }

  case class CircularDependency[A, B, C](from: Package[PackageName, A, B, C], path: NonEmptyList[PackageName]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val packs = from.name :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = getMapSrc(sourceMap, p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }

  case class CircularType[A](from: PackageName, path: NonEmptyList[rankn.DefinedType[A]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      s"circular types in ${from.asString} " + path.toList.reverse.map(_.name.ident.asString).mkString(" -> ")
    }
  }

  case class VarianceInferenceFailure(from: PackageName, failed: NonEmptyList[rankn.DefinedType[Unit]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      s"failed to infer variance in ${from.asString} of " + failed.toList.map(_.name.ident.asString).sorted.mkString(", ")
    }
  }

  case class TypeErrorIn(tpeErr: Infer.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val teMessage = tpeErr match {
        case Infer.Error.NotUnifiable(t0, t1, r0, r1) =>
          val context0 =
            if (r0 == r1) " " // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0).getOrElse(r0.toString) // we should highlight the whole region
              s"\n$m\n"
            }
          val context1 =
            lm.showRegion(r1).getOrElse(r1.toString) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          s"type error: expected type ${tmap(t0)}${context0}to be the same as type ${tmap(t1)}\n$context1"
        case Infer.Error.VarNotInScope((pack, name), scope, region) =>
          val ctx = lm.showRegion(region).getOrElse(region.toString)
          val candidates: List[String] =
            nearest(name, scope.map { case ((_, n), _) => (n, ()) }, 3)
              .map { case (n, _) => n.asString }

          val cmessage =
            if (candidates.nonEmpty) candidates.mkString("\nClosest: ", ", ", ".\n")
            else ""
          val qname = "\"" + name.sourceCodeRepr + "\""
          s"name $qname unknown.$cmessage\n$ctx"
        case Infer.Error.SubsumptionCheckFailure(t0, t1, r0, r1, tvs) =>
          val context0 =
            if (r0 == r1) " " // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0).getOrElse(r0.toString) // we should highlight the whole region
              s"\n$m\n"
            }
          val context1 =
            lm.showRegion(r1).getOrElse(r1.toString) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          s"type ${tmap(t0)}${context0}does not subsume type ${tmap(t1)}\n$context1"
        case uc@Infer.Error.UnknownConstructor((p, n), region, _) =>
          val near = nearest(n, uc.knownConstructors.map { case (_, n) => (n, ()) }.toMap, 3)
            .map { case (n, _) => n.asString }

          val nearStr =
            if (near.isEmpty) ""
            else near.mkString(", nearest: ", ", ", "")

          val context =
            lm.showRegion(region).getOrElse(region.toString) // we should highlight the whole region

          s"unknown constructor ${n.asString}$nearStr" + "\n" + context
        case err => err.message
      }
      // TODO use the sourceMap/regiouns in Infer.Error
      s"in file: $sourceName, package ${pack.asString}, $teMessage"
    }
  }

  case class SourceConverterErrorIn(err: SourceConverter.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val msg = {
        val context =
          lm.showRegion(err.region).getOrElse(err.region.toString) // we should highlight the whole region

        err.message + "\n" + context
      }
      s"in file: $sourceName, package ${pack.asString}, $msg"
    }
  }

  case class TotalityCheckError(pack: PackageName, err: TotalityCheck.ExprError[Declaration]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val region = err.matchExpr.tag.region
      val context1 =
        lm.showRegion(region).getOrElse(region.toString) // we should highlight the whole region
      val teMessage = err match {
        case TotalityCheck.NonTotalMatch(_, missing) =>
          import Identifier.Constructor
          val allTypes = missing.traverse(_.traverseType { t => Writer(Chain.one(t), ()) })
            .run._1.toList.distinct
          val showT = showTypes(pack, allTypes)

          val doc = Pattern.compiledDocument(Document.instance[Type] { t =>
            Doc.text(showT(t))
          })
          def showPat(p: Pattern[(PackageName, Constructor), Type]): String =
            doc.document(p).render(80)

          "non-total match, missing: " +
            (Doc.intercalate(Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_)))).render(80)
        case TotalityCheck.InvalidPattern(_, err) =>
          import TotalityCheck._
          err match {
            case ArityMismatch((_, n), _, _, exp, found) =>
              s"arity mismatch: ${n.asString} expected $exp parameters, found $found"
            case UnknownConstructor((_, n), _, _) =>
              s"unknown constructor: ${n.asString}"
            case MultipleSplicesInPattern(_, _) =>
              "multiple splices in pattern, only one per match allowed"
          }
      }
      // TODO use the sourceMap/regions in Infer.Error
      s"in file: $sourceName, package ${pack.asString}\n$context1\n$teMessage"
    }
  }

  case class RecursionError(pack: PackageName, err: DefRecursionCheck.RecursionError) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)]) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val ctx = lm.showRegion(err.region).getOrElse(err.region.toString) // we should highlight the whole region
      val errMessage = err.message
      // TODO use the sourceMap/regions in RecursionError
      s"in file: $sourceName, package ${pack.asString}, $errMessage\n$ctx\n"
    }
  }
}
