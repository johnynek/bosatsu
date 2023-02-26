package org.bykn.bosatsu

import cats.data.{Chain, NonEmptyList, Writer, NonEmptyMap}
import org.typelevel.paiges.{Doc, Document}

import rankn._
import LocationMap.Colorize

sealed abstract class PackageError {
  def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize): String

  protected def getMapSrc(sourceMap: Map[PackageName, (LocationMap, String)], pack: PackageName): (LocationMap, String) =
    sourceMap.get(pack) match {
      case None => (LocationMap(""), "<unknown source>")
      case Some(found) => found
    }
}

object PackageError {
  def showTypes(pack: PackageName, tpes: List[Type]): Map[Type, Doc] = {
    // TODO: we should use the imports in each package to talk about
    // types in ways that are local to that package
    require(pack ne null)
    tpes
      .iterator
      .map { t =>
        (t, Type.fullyResolvedDocument.document(t))
      }
      .toMap
  }

  def nearest[A](ident: Identifier, existing: Iterable[(Identifier, A)], count: Int): List[(Identifier, A)] =
    existing
      .iterator
      .map { case (i, a) =>
        val d = EditDistance.string(ident.asString, i.asString)
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
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, in)
      val header =
        s"in $sourceName unknown export ${ex.name.sourceCodeRepr}"
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

  case class PrivateTypeEscape[A](ex: ExportedName[A],
    exType: Type,
    in: PackageName,
    privateType: Type.Const) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (_, sourceName) = getMapSrc(sourceMap, in)
      val pt = Type.TyConst(privateType)
      val tpeMap = showTypes(in, exType :: pt :: Nil)
      val first = s"in $sourceName export ${ex.name.sourceCodeRepr} of type ${tpeMap(exType).render(80)}"
      if (exType == pt) {
        s"$first has an unexported (private) type."
      }
      else {
        s"$first references an unexported (private) type ${tpeMap(pt).render(80)}."
      }
    }
  }

  case class UnknownImportPackage[A, B, C](pack: PackageName, fromName: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (_, sourceName) = getMapSrc(sourceMap, fromName)
      s"in $sourceName package ${fromName.asString} imports unknown package ${pack.asString}"
    }
  }

  case class DuplicatedImport(duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) =
      duplicates.sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          val (_, sourceName) = getMapSrc(sourceMap, pack)
          s"duplicate import in $sourceName package ${pack.asString} imports ${imp.originalName.sourceCodeRepr} as ${imp.localName.sourceCodeRepr}"
        }
        .mkString("\n")
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName[A, B](
    in: PackageName,
    importedPackage: PackageName,
    letMap: Map[Identifier, Unit],
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
        val ipname = importedPackage

        val (_, sourceName) = getMapSrc(sourceMap, in)
        letMap
          .get(iname.originalName) match {
            case Some(_) =>
              s"in $sourceName package: ${ipname.asString} has ${iname.originalName.sourceCodeRepr} but it is not exported. Add to exports"
            case None =>
              val near = nearest(iname.originalName, letMap, 3)
                .map { case (n, _) => n.sourceCodeRepr }
                .mkString(" Nearest: ", ", ", "")
              s"in $sourceName package: ${ipname.asString} does not have name ${iname.originalName.sourceCodeRepr}.$near"
          }
      }
    }

  case class UnknownImportFromInterface[A, B](
    in: PackageName,
    importingName: PackageName,
    exportNames: List[Identifier],
    iname: ImportedName[A],
    exports: List[ExportedName[B]]) extends PackageError {
      def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {

        val (_, sourceName) = getMapSrc(sourceMap, in)
        val exportMap = exportNames.map { e => (e, ()) }.toMap
        val near = nearest(iname.originalName, exportMap, 3)
          .map { case (n, _) => n.asString }
          .mkString(" Nearest: ", ", ", "")
        s"in $sourceName package: ${importingName} does not have name ${iname.originalName}.$near"
      }
    }

  case class CircularDependency[A, B, C](from: PackageName, path: NonEmptyList[PackageName]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val packs = from :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = getMapSrc(sourceMap, p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }

  case class CircularType[A](from: PackageName, path: NonEmptyList[rankn.DefinedType[A]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      s"circular types in ${from.asString} " + path.toList.reverse.map(_.name.ident.asString).mkString(" -> ")
    }
  }

  case class VarianceInferenceFailure(from: PackageName, failed: NonEmptyList[rankn.DefinedType[Unit]]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      s"failed to infer variance in ${from.asString} of " + failed.toList.map(_.name.ident.asString).sorted.mkString(", ")
    }
  }

  case class TypeErrorIn(tpeErr: Infer.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val teMessage = tpeErr match {
        case Infer.Error.NotUnifiable(t0, t1, r0, r1) =>
          val context0 =
            if (r0 == r1) Doc.space // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0, 2, errColor).getOrElse(Doc.str(r0)) // we should highlight the whole region
              Doc.hardLine + m + Doc.hardLine
            }
          val context1 =
            lm.showRegion(r1, 2, errColor).getOrElse(Doc.str(r1)) // we should highlight the whole region

          val fnHint =
            (t0, t1) match {
              case (Type.Fun(_, _), Type.Fun(_, _)) =>
                // both are functions
                Doc.empty
              case (Type.Fun(_, _), _) | (_, Type.Fun(_, _)) =>
                Doc.text("hint: this often happens when you apply the wrong number of arguments to a function.") + Doc.hardLine
              case _ =>
                Doc.empty
            }

          val tmap = showTypes(pack, List(t0, t1))
          val doc = Doc.text("type error: expected type ") + tmap(t0) +
            context0 + Doc.text("to be the same as type ") + tmap(t1) +
            Doc.hardLine + fnHint + context1

          doc.render(80)
        case Infer.Error.VarNotInScope((_, name), scope, region) =>
          val ctx = lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))
          val candidates: List[String] =
            nearest(name, scope.map { case ((_, n), _) => (n, ()) }, 3)
              .map { case (n, _) => n.asString }

          val cmessage =
            if (candidates.nonEmpty) candidates.mkString("\nClosest: ", ", ", ".\n")
            else ""
          val qname = "\"" + name.sourceCodeRepr + "\""
          (Doc.text("name ") + Doc.text(qname) + Doc.text(" unknown.") + Doc.text(cmessage) + Doc.hardLine +
            ctx).render(80)
        case Infer.Error.SubsumptionCheckFailure(t0, t1, r0, r1, _) =>
          val context0 =
            if (r0 == r1) Doc.space // sometimes the region of the error is the same on right and left
            else {
              val m = lm.showRegion(r0, 2, errColor).getOrElse(Doc.str(r0)) // we should highlight the whole region
              Doc.hardLine + m + Doc.hardLine
            }
          val context1 =
            lm.showRegion(r1, 2, errColor).getOrElse(Doc.str(r1)) // we should highlight the whole region

          val tmap = showTypes(pack, List(t0, t1))
          val doc = Doc.text("type ") + tmap(t0) + context0 +
            Doc.text("does not subsume type ") + tmap(t1) + Doc.hardLine +
            context1

          doc.render(80)
        case uc@Infer.Error.UnknownConstructor((_, n), region, _) =>
          val near = nearest(n, uc.knownConstructors.map { case (_, n) => (n, ()) }.toMap, 3)
            .map { case (n, _) => n.asString }

          val nearStr =
            if (near.isEmpty) ""
            else near.mkString(", nearest: ", ", ", "")

          val context =
            lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region

          val doc = Doc.text("unknown constructor ") + Doc.text(n.asString) +
            Doc.text(nearStr) + Doc.hardLine + context
          doc.render(80)
        case Infer.Error.KindInvariantViolation(tpe, right, region, mess) =>
          val applied = Type.TyApply(tpe, right)
          val tmap = showTypes(pack, applied :: Nil)
          val context =
            lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region
          val doc = Doc.text("kind error: ") + Doc.text(mess) + Doc.text(" for type the left of ") +
            tmap(applied) + Doc.hardLine +
            context

          doc.render(80)
        case Infer.Error.KindSubsumptionCheckFailure(leftK, leftT, rightK, rightT, leftR, rightR) =>
          val applied = Type.TyApply(leftT, rightT)
          val tmap = showTypes(pack, applied :: leftT :: rightT :: Nil)
          val context =
            if (leftR == rightR)
              lm.showRegion(leftR, 2, errColor).getOrElse(Doc.str(leftR)) // we should highlight the whole region
            else { 
              lm.showRegion(leftR, 2, errColor).getOrElse(Doc.str(leftR)) + Doc.hardLine + Doc.hardLine + // we should highlight the whole region
                lm.showRegion(rightR, 2, errColor).getOrElse(Doc.str(rightR))
            }
          val doc = Doc.text("kind error: ") + Doc.text("the type: ") + tmap(applied) +
            Doc.text(" is invalid because the left ") + tmap(leftT) + Doc.text(" has kind ") + Kind.toDoc(leftK) +
            Doc.text(" and the right ") + tmap(rightT) + Doc.text(" has kind ") + Kind.toDoc(rightK) +
            Doc.text(s" but left does not subsume right:") +
            Doc.hardLine +
            context

          doc.render(80)
        case err => err.message
      }
      // TODO use the sourceMap/regiouns in Infer.Error
      s"in file: $sourceName, package ${pack.asString}, $teMessage"
    }
  }

  case class SourceConverterErrorIn(err: SourceConverter.Error, pack: PackageName) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val msg = {
        val context =
          lm.showRegion(err.region, 2, errColor)
            .getOrElse(Doc.str(err.region)) // we should highlight the whole region

        Doc.text(err.message) + Doc.hardLine + context
      }
      val doc = Doc.text("in file: ") + Doc.text(sourceName) + Doc.text(", package ") + Doc.text(pack.asString) +
        Doc.text(", ") + msg

      doc.render(80)
    }
  }

  case class TotalityCheckError(pack: PackageName, err: TotalityCheck.ExprError[Declaration]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val region = err.matchExpr.tag.region
      val context1 =
        lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region
      val teMessage = err match {
        case TotalityCheck.NonTotalMatch(_, missing) =>
          val allTypes = missing.traverse(_.traverseType { t => Writer(Chain.one(t), ()) })
            .run._1.toList.distinct
          val showT = showTypes(pack, allTypes)

          val doc = Pattern.compiledDocument(Document.instance[Type] { t =>
            showT(t)
          })

          Doc.text("non-total match, missing: ") +
            (Doc.intercalate(Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_))))
        case TotalityCheck.UnreachableBranches(_, unreachableBranches) =>
          val allTypes = unreachableBranches.traverse(_.traverseType { t => Writer(Chain.one(t), ()) })
            .run._1.toList.distinct
          val showT = showTypes(pack, allTypes)

          val doc = Pattern.compiledDocument(Document.instance[Type] { t =>
            showT(t)
          })

          Doc.text("unreachable branches: ") +
            (Doc.intercalate(Doc.char(',') + Doc.lineOrSpace,
              unreachableBranches.toList.map(doc.document(_))))
        case TotalityCheck.InvalidPattern(_, err) =>
          import TotalityCheck._
          err match {
            case ArityMismatch((_, n), _, _, exp, found) =>
              Doc.text(s"arity mismatch: ${n.asString} expected $exp parameters, found $found")
            case UnknownConstructor((_, n), _, _) =>
              Doc.text(s"unknown constructor: ${n.asString}")
            case InvalidStrPat(pat, _) =>
              Doc.text(s"invalid string pattern: ") +
                Document[Pattern.Parsed].document(pat) +
                Doc.text(" (adjacent bindings aren't allowed)")
            case MultipleSplicesInPattern(_, _) =>
              // TODO: get printing of compiled patterns working well
              //val docp = Document[Pattern.Parsed].document(Pattern.ListPat(pat)) +
              Doc.text("multiple splices in pattern, only one per match allowed")
          }
      }
      // TODO use the sourceMap/regions in Infer.Error
      val doc = Doc.text(s"in file: $sourceName, package ${pack.asString}") + Doc.hardLine +
        context1 + Doc.hardLine + teMessage

      doc.render(80)
    }
  }

  case class UnusedLetError(pack: PackageName, errs: NonEmptyList[(Identifier.Bindable, Region)]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val docs = errs
        .sortBy(_._2)
        .map { case (bn, region) =>
          val rdoc = lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region)) // we should highlight the whole region
          val message = Doc.text("unused let binding: " + bn.sourceCodeRepr)
          message + Doc.hardLine + rdoc
        }

      val packDoc = Doc.text(s"in file: $sourceName, package ${pack.asString}:")
      val line2 = Doc.hardLine + Doc.hardLine
      (packDoc + (line2 + Doc.intercalate(line2, docs.toList)).nested(2)).render(80)
    }
  }

  case class RecursionError(pack: PackageName, err: DefRecursionCheck.RecursionError) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val (lm, sourceName) = getMapSrc(sourceMap, pack)
      val ctx = lm.showRegion(err.region, 2, errColor)
        .getOrElse(Doc.str(err.region)) // we should highlight the whole region
      val errMessage = err.message
      // TODO use the sourceMap/regions in RecursionError
      val doc = Doc.text(s"in file: $sourceName, package ${pack.asString}, $errMessage") +
        Doc.hardLine + ctx + Doc.hardLine

      doc.render(80)
    }
  }

  case class DuplicatedPackageError(dups: NonEmptyMap[PackageName, (String, NonEmptyList[String])]) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      val packDoc = Doc.text("package ")
      val dupInDoc = Doc.text(" duplicated in ")
      val dupMessages = dups
        .toSortedMap
        .map { case (pname, (one, nelist)) =>
          val dupsrcs = Doc.intercalate(Doc.comma + Doc.lineOrSpace,
            (one :: nelist.toList)
              .sorted
              .map(Doc.text(_))
            )
            .nested(4)
          packDoc + Doc.text(pname.asString) + dupInDoc + dupsrcs
        }

      Doc.intercalate(Doc.line, dupMessages).render(80)
    }
  }

  case class KindInferenceError(pack: PackageName, kindError: KindFormula.Error) extends PackageError {
    def message(sourceMap: Map[PackageName, (LocationMap, String)], errColor: Colorize) = {
      // TODO actually make this look sane
      s"in package ${pack.asString}: KindError: $kindError"
    }
  }
}