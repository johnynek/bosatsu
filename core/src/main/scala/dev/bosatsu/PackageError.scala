package dev.bosatsu

import cats.data.{Chain, NonEmptyList, Writer, NonEmptyMap}
import cats.syntax.all._
import org.typelevel.paiges.{Doc, Document}

import rankn._
import LocationMap.Colorize

sealed abstract class PackageError {
  def message(
      sourceMap: Map[PackageName, (LocationMap, String)],
      errColor: Colorize
  ): String
}

object PackageError {
  def showTypes(pack: PackageName, tpes: List[Type]): Map[Type, Doc] = {
    // TODO: we should use the imports in each package to talk about (https://github.com/johnynek/bosatsu/issues/4)
    // types in ways that are local to that package
    Require(pack ne null)
    tpes.iterator.map { t =>
      (t, Type.fullyResolvedDocument.document(t))
    }.toMap
  }

  def nearest[A](
      ident: Identifier,
      existing: Iterable[(Identifier, A)],
      count: Int
  ): List[(Identifier, A)] = {
    val istr = ident.asString
    val prefixes =
      existing.iterator.filter { case (i, _) =>
        i.asString.startsWith(istr)
      }.toList

    val close = existing.iterator
      .map { case (i, a) =>
        val d = EditDistance.string(istr, i.asString)
        (i, d, a)
      }
      .filter { case (i, d, _) =>
        // don't show things that require total edits
        (d < istr.length) && (d < i.asString.length)
      }
      .toList
      .sortBy { case (_, d, _) => d }
      .take(count)
      .map { case (i, _, a) => (i, a) }

    (prefixes.sortBy(_._1) ::: close).distinct
  }

  private val emptyLocMap = LocationMap("")

  type SourceMap = Map[PackageName, (LocationMap, String)]
  implicit class SourceMapMethods(private val sm: SourceMap) extends AnyVal {
    def headLine(packageName: PackageName, region: Option[Region]): Doc = {
      val (lm, sourceName) = getMapSrc(packageName)
      val suffix = (region.flatMap(r => lm.toLineCol(r.start))) match {
        case Some((line, col)) => s":${line + 1}:${col + 1}"
        case None              => ""
      }
      Doc.text(s"in file: $sourceName$suffix, package ${packageName.asString}")
    }

    def getMapSrc(pack: PackageName): (LocationMap, String) =
      sm.get(pack) match {
        case None        => (emptyLocMap, "<unknown source>")
        case Some(found) => found
      }
  }

  case class UnknownExport[A](
      ex: ExportedName[A],
      in: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, sourceName) = sourceMap.getMapSrc(in)
      val header =
        s"in $sourceName unknown export ${ex.name.sourceCodeRepr}"
      val candidateMap: Map[Identifier, Region] =
        lets.map { case (n, _, expr) => (n, HasRegion.region(expr)) }.toMap
      val candidates =
        nearest(ex.name, candidateMap, 3)
          .map { case (n, r) =>
            val pos = lm
              .toLineCol(r.start)
              .map { case (l, c) => s":${l + 1}:${c + 1}" }
              .getOrElse("")
            s"${n.asString}$pos"
          }
      val candstr = candidates.mkString("\n\t", "\n\t", "\n")
      val suggestion =
        if (candidates.nonEmpty) "\n" + s"perhaps you meant:$candstr"
        else ""
      header + suggestion
    }
  }

  case class PrivateTypeEscape[A](
      ex: ExportedName[A],
      exType: Type,
      in: PackageName,
      privateType: Type.Const
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (_, sourceName) = sourceMap.getMapSrc(in)
      val pt = Type.TyConst(privateType)
      val tpeMap = showTypes(in, exType :: pt :: Nil)
      val first =
        s"in $sourceName export ${ex.name.sourceCodeRepr} of type ${tpeMap(exType).render(80)}"
      if (exType == (pt: Type)) {
        s"$first has an unexported (private) type."
      } else {
        s"$first references an unexported (private) type ${tpeMap(pt).render(80)}."
      }
    }
  }

  case class UnknownImportPackage[A, B, C](
      pack: PackageName,
      fromName: PackageName
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (_, sourceName) = sourceMap.getMapSrc(fromName)
      s"in $sourceName package ${fromName.asString} imports unknown package ${pack.asString}"
    }
  }

  case class DuplicatedImport(
      inPackage: PackageName,
      duplicates: NonEmptyList[(PackageName, ImportedName[Unit])]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (_, sourceName) = sourceMap.getMapSrc(inPackage)
      val first =
        s"duplicate import in $sourceName package ${inPackage.asString}"

      duplicates
        .sortBy(_._2.localName)
        .toList
        .iterator
        .map { case (pack, imp) =>
          if (imp.isRenamed) {
            s"\tfrom ${pack.asString} import ${imp.originalName.sourceCodeRepr} as ${imp.localName.sourceCodeRepr}"
          } else {
            s"\tfrom ${pack.asString} import ${imp.originalName.sourceCodeRepr}"
          }
        }
        .mkString(first + "\n", "\n", "\n")
    }
  }

  // We could check if we forgot to export the name in the package and give that error
  case class UnknownImportName[A, B](
      in: PackageName,
      importedPackage: PackageName,
      letMap: Map[Identifier, Unit],
      iname: ImportedName[A],
      exports: List[ExportedName[B]]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val ipname = importedPackage

      val (_, sourceName) = sourceMap.getMapSrc(in)
      letMap
        .get(iname.originalName) match {
        case Some(_) =>
          s"in $sourceName package: ${ipname.asString} has ${iname.originalName.sourceCodeRepr} but it is not exported. Add to exports"
        case None =>
          val cands = nearest(iname.originalName, letMap, 3)
            .map { case (n, _) => n.sourceCodeRepr }

          val near =
            if (cands.nonEmpty) cands.mkString(" Nearest: ", ", ", "")
            else ""

          s"in $sourceName package: ${ipname.asString} does not have name ${iname.originalName.sourceCodeRepr}.$near"
      }
    }
  }

  case class UnknownImportFromInterface[A, B](
      in: PackageName,
      importingName: PackageName,
      exportNames: List[Identifier],
      iname: ImportedName[A],
      exports: List[ExportedName[B]]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {

      val exportMap = exportNames.map(e => (e, ())).toMap

      val candidates =
        nearest(iname.originalName, exportMap, 3)
          .map(ident => Doc.text(ident._1.sourceCodeRepr))

      val near = Doc.text(" Nearest: ") +
        (Doc
          .intercalate(Doc.text(",") + Doc.line, candidates)
          .nested(4)
          .grouped)

      (sourceMap.headLine(importingName, None) + Doc.hardLine + Doc.text(
        s"does not have name ${iname.originalName}."
      ) + near).render(80)
    }
  }

  case class CircularDependency[A, B, C](
      from: PackageName,
      path: NonEmptyList[PackageName]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val packs = from :: (path.toList)
      val msg = packs.map { p =>
        val (_, src) = sourceMap.getMapSrc(p)
        s"${p.asString} in $src"
      }
      val tab = "\n\t"
      s"circular package dependency:\n${msg.mkString(tab)}"
    }
  }

  case class VarianceInferenceFailure(
      from: PackageName,
      failed: NonEmptyList[rankn.DefinedType[Unit]]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) =
      s"failed to infer variance in ${from.asString} of " + failed.toList
        .map(_.name.ident.asString)
        .sorted
        .mkString(", ")
  }

  case class TypeErrorIn(
      tpeErr: Infer.Error,
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, Expr[Declaration])],
      externals: Map[Identifier.Bindable, (Type, Region)],
      letNameRegions: Map[Identifier.Bindable, Region]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      def singleToDoc(tpeErr: Infer.Error.Single): Doc = {
        val (teMessage, region) = tpeErr match {
          case Infer.Error.NotUnifiable(t0, t1, r0, r1) =>
            val context0 =
              if (r0 === r1)
                Doc.space // sometimes the region of the error is the same on right and left
              else {
                val m = lm
                  .showRegion(r0, 2, errColor)
                  .getOrElse(
                    Doc.str(r0)
                  ) // we should highlight the whole region
                Doc.hardLine + m + Doc.hardLine
              }
            val context1 =
              lm.showRegion(r1, 2, errColor)
                .getOrElse(Doc.str(r1)) // we should highlight the whole region

            val fnHint =
              (t0, t1) match {
                case (
                      Type.RootConst(Type.FnType(_, leftSize)),
                      Type.RootConst(Type.FnType(_, rightSize))
                    ) =>
                  // both are functions
                  def args(n: Int) =
                    if (n == 1) "one argument" else s"$n arguments"
                  Doc.text(
                    s"hint: the first type is a function with ${args(leftSize)} and the second is a function with ${args(rightSize)}."
                  ) + Doc.hardLine
                case (Type.Fun(_, _), _) | (_, Type.Fun(_, _)) =>
                  Doc.text(
                    "hint: this often happens when you apply the wrong number of arguments to a function."
                  ) + Doc.hardLine
                case _ =>
                  Doc.empty
              }

            val tmap = showTypes(pack, List(t0, t1))
            val doc = Doc.text("type error: expected type ") + tmap(t0) +
              context0 + Doc.text("to be the same as type ") + tmap(t1) +
              Doc.hardLine + fnHint + context1

            (doc, Some(r0))
          case Infer.Error.VarNotInScope((_, name), scope, region) =>
            val ctx =
              lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))

            val qname = "\"" + name.sourceCodeRepr + "\""
            val laterTopLevelDef =
              name match {
                case b: Identifier.Bindable =>
                  letNameRegions.get(b).filter(_.start > region.start)
                case _ =>
                  None
              }

            laterTopLevelDef match {
              case Some(defRegion) =>
                val defPos = lm
                  .toLineCol(defRegion.start)
                  .map { case (line, col) => s"${line + 1}:${col + 1}" }
                  .getOrElse(defRegion.toString)
                val defCtx =
                  lm.showRegion(defRegion, 2, errColor).getOrElse(Doc.str(
                    defRegion
                  ))

                (
                  Doc.text("name ") + Doc.text(qname) + Doc.text(
                    " is used before it is defined."
                  ) + Doc.hardLine +
                    Doc.text(
                      s"this use site uses ${name.sourceCodeRepr}, but ${name.sourceCodeRepr} is defined later in this file at $defPos."
                    ) + Doc.hardLine +
                    Doc.text(
                      s"move ${name.sourceCodeRepr} before its first use site."
                    ) + Doc.hardLine +
                    Doc.text("Use site:") + Doc.hardLine + ctx + Doc.hardLine +
                    Doc.text("Definition site:") + Doc.hardLine + defCtx,
                  Some(region)
                )
              case None =>
                val names =
                  (scope.map { case ((_, n), _) => n }.toList ::: lets.map(
                    _._1
                  )).distinct

                val candidates: List[String] =
                  nearest(name, names.map((_, ())), 3)
                    .map { case (n, _) => n.asString }

                val cmessage =
                  if (candidates.nonEmpty)
                    candidates.mkString("\nClosest: ", ", ", ".\n")
                  else ""

                (
                  Doc.text("name ") + Doc.text(qname) + Doc.text(
                    " unknown."
                  ) + Doc
                    .text(cmessage) + Doc.hardLine +
                    ctx,
                  Some(region)
                )
            }
          case Infer.Error.SubsumptionCheckFailure(t0, t1, r0, r1, _) =>
            val context0 =
              if (r0 === r1)
                Doc.space // sometimes the region of the error is the same on right and left
              else {
                val m = lm
                  .showRegion(r0, 2, errColor)
                  .getOrElse(
                    Doc.str(r0)
                  ) // we should highlight the whole region
                Doc.hardLine + m + Doc.hardLine
              }
            val context1 =
              lm.showRegion(r1, 2, errColor)
                .getOrElse(Doc.str(r1)) // we should highlight the whole region

            val tmap = showTypes(pack, List(t0, t1))
            val doc = Doc.text("type ") + tmap(t0) + context0 +
              Doc.text("does not subsume type ") + tmap(t1) + Doc.hardLine +
              context1

            (doc, Some(r0))
          case uc @ Infer.Error.UnknownConstructor((_, n), region, _) =>
            val near = nearest(
              n,
              uc.knownConstructors.map { case (_, n) => (n, ()) }.toMap,
              3
            )
              .map { case (n, _) => n.asString }

            val nearStr =
              if (near.isEmpty) ""
              else near.mkString(", nearest: ", ", ", "")

            val context =
              lm.showRegion(region, 2, errColor)
                .getOrElse(
                  Doc.str(region)
                ) // we should highlight the whole region

            val doc = Doc.text("unknown constructor ") + Doc.text(n.asString) +
              Doc.text(nearStr) + Doc.hardLine + context
            (doc, Some(region))
          case Infer.Error.KindCannotTyApply(applied, region) =>
            val tmap = showTypes(pack, applied :: Nil)
            val context =
              lm.showRegion(region, 2, errColor)
                .getOrElse(
                  Doc.str(region)
                ) // we should highlight the whole region
            val doc = Doc.text("kind error: for kind of the left of ") +
              tmap(applied) + Doc.text(
                " is *. Cannot apply to kind *."
              ) + Doc.hardLine +
              context

            (doc, Some(region))
          case Infer.Error.KindExpectedType(tpe, kind, region) =>
            val tmap = showTypes(pack, tpe :: Nil)
            val context =
              lm.showRegion(region, 2, errColor)
                .getOrElse(
                  Doc.str(region)
                ) // we should highlight the whole region
            val doc = Doc.text("expected type ") +
              tmap(tpe) + Doc.text(
                " to have kind *, which is to say be a valid value, but it is kind "
              ) + Kind.toDoc(kind) + Doc.hardLine +
              context

            (doc, Some(region))
          case Infer.Error.KindInvalidApply(applied, leftK, rightK, region) =>
            val leftT = applied.on
            val rightT = applied.arg
            val tmap = showTypes(pack, applied :: leftT :: rightT :: Nil)
            val context =
              lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))
            val doc = Doc.text("kind error: ") + Doc.text("the type: ") + tmap(
              applied
            ) +
              Doc.text(" is invalid because the left ") + tmap(leftT) + Doc
                .text(" has kind ") + Kind.toDoc(leftK) +
              Doc.text(" and the right ") + tmap(rightT) + Doc.text(
                " has kind "
              ) + Kind.toDoc(rightK) +
              Doc.text(s" but left cannot accept the kind of the right:") +
              Doc.hardLine +
              context

            (doc, Some(region))
          case Infer.Error.KindMismatch(
                meta,
                metaK,
                rightT,
                rightK,
                metaR,
                rightR
              ) =>
            val tmap = showTypes(pack, meta :: rightT :: Nil)
            val context0 =
              lm.showRegion(metaR, 2, errColor)
                .getOrElse(
                  Doc.str(metaR)
                ) // we should highlight the whole region
            val context1 =
              if (metaR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine +
                  lm.showRegion(rightR, 2, errColor)
                    .getOrElse(
                      Doc.str(rightR)
                    ) + // we should highlight the whole region
                  Doc.hardLine
              } else {
                Doc.empty
              }

            val doc =
              Doc.text("kind error: ") + Doc.text("the type: ") + tmap(meta) +
                Doc.text(" of kind: ") + Kind.toDoc(metaK) + Doc.text(
                  " at: "
                ) + Doc.hardLine +
                context0 + Doc.hardLine + Doc.hardLine +
                Doc.text("cannot be unified with the type ") + tmap(rightT) +
                Doc.text(" of kind: ") + Kind.toDoc(rightK) + context1 +
                Doc.hardLine +
                Doc.text("because the first kind does not subsume the second.")

            (doc, Some(metaR))
          case Infer.Error.UnexpectedMeta(meta, in, metaR, rightR) =>
            val tymeta = Type.TyMeta(meta)
            val tmap = showTypes(pack, tymeta :: in :: Nil)
            val context0 =
              lm.showRegion(metaR, 2, errColor)
                .getOrElse(
                  Doc.str(metaR)
                ) // we should highlight the whole region
            val context1 =
              if (metaR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine +
                  lm.showRegion(rightR, 2, errColor)
                    .getOrElse(
                      Doc.str(rightR)
                    ) + // we should highlight the whole region
                  Doc.hardLine
              } else {
                Doc.empty
              }

            val doc =
              Doc.text("Unexpected unknown: the type: ") + tmap(tymeta) +
                Doc.text(" of kind: ") + Kind.toDoc(meta.kind) + Doc.text(
                  " at: "
                ) + Doc.hardLine +
                context0 + Doc.hardLine + Doc.hardLine +
                Doc.text("inside the type ") + tmap(in) + context1 +
                Doc.hardLine +
                Doc.text(
                  "this sometimes happens when a function arg has been omitted, or an illegal recursive type or function."
                )

            (doc, Some(metaR))
          case Infer.Error.NotPolymorphicEnough(tpe, _, _, region) =>
            val tmap = showTypes(pack, tpe :: Nil)
            val context =
              lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))

            (
              Doc.text("the type ") + tmap(tpe) + Doc.text(
                " is not polymorphic enough"
              ) + Doc.hardLine + context,
              Some(region)
            )
          case Infer.Error.ArityMismatch(leftA, leftR, rightA, rightR) =>
            val context0 =
              lm.showRegion(leftR, 2, errColor).getOrElse(Doc.str(leftR))
            val context1 =
              if (leftR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine +
                  lm.showRegion(rightR, 2, errColor).getOrElse(Doc.str(rightR))
              } else {
                Doc.empty
              }

            def args(n: Int) =
              if (n == 1) "one argument" else s"$n arguments"

            (
              Doc.text(
                s"function with ${args(leftA)} at:"
              ) + Doc.hardLine + context0 +
                Doc.text(
                  s" does not match function with ${args(rightA)}"
                ) + context1,
              Some(leftR)
            )
          case Infer.Error.ArityTooLarge(found, max, region) =>
            val context =
              lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))

            (
              Doc.text(
                s"function with $found arguments is too large. Maximum function argument count is $max."
              ) + Doc.hardLine + context,
              Some(region)
            )
          case Infer.Error.UnexpectedBound(bound, _, reg, _) =>
            val tyvar = Type.TyVar(bound)
            val tmap = showTypes(pack, tyvar :: Nil)
            val context =
              lm.showRegion(reg, 2, errColor).getOrElse(Doc.str(reg))

            (
              Doc.text("unexpected bound: ") + tmap(
                tyvar
              ) + Doc.hardLine + context,
              Some(reg)
            )
          case Infer.Error.UnionPatternBindMismatch(_, names, region) =>
            val context =
              lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))

            val uniqueSets = graph.Tree.distinctBy(names)(_.toSet)
            val uniqs = Doc.intercalate(
              Doc.char(',') + Doc.line,
              uniqueSets.toList.map { names =>
                Doc.text(
                  names.iterator.map(_.sourceCodeRepr).mkString("[", ", ", "]")
                )
              }
            )
            (
              Doc.text("not all union elements bind the same names: ") +
                (Doc.line + uniqs + context).nested(4).grouped,
              Some(region)
            )
          case Infer.Error.UnknownDefined(const, reg) =>
            val context =
              lm.showRegion(reg, 2, errColor).getOrElse(Doc.str(reg))

            (
              Doc.text(
                s"Use of unimported type. Add `from ${const.packageName.asString} import ${const.name.asString}`"
              ) + Doc.hardLine + context,
              Some(reg)
            )
          case ie: Infer.Error.InternalError =>
            val context =
              lm.showRegion(ie.region, 2, errColor)
                .getOrElse(Doc.str(ie.region))
            (Doc.text(ie.message) + Doc.hardLine + context, Some(ie.region))

        }
        val h = sourceMap.headLine(pack, region)
        (h + Doc.hardLine + teMessage)
      }

      val finalDoc = tpeErr match {
        case s: Infer.Error.Single         => singleToDoc(s)
        case c @ Infer.Error.Combine(_, _) =>
          val twoLines = Doc.hardLine + Doc.hardLine
          c.flatten.iterator
            .map(singleToDoc)
            .reduce((a, b) => a + (twoLines + b))
      }
      finalDoc.render(80)
    }
  }

  case class SourceConverterErrorsIn(
      region: Region,
      errs: NonEmptyList[SourceConverter.Error],
      pack: PackageName
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val context =
        lm.showRegion(region, 2, errColor)
          .getOrElse(
            Doc.str(region)
          ) // we should highlight the whole region
      val headDoc = sourceMap.headLine(pack, Some(region))

      val (missing, notMissing) = errs.toList.partitionMap {
        case ma: SourceConverter.MissingArg => Left(ma)
        case notMa                          => Right(notMa)
      }
      val mdocs = missing
        .groupBy(ma => (ma.name, ma.syntax))
        .toList
        .sortBy { case ((name, _), _) => name }
        .map { case ((_, syn), mas) =>
          val allMissing = mas.map(_.missing)

          val missingDoc = Doc.intercalate(
            Doc.comma + Doc.space,
            allMissing.sorted.map(m => Doc.text(m.asString))
          )

          val fieldStr =
            if (allMissing.lengthCompare(1) == 0) "field" else "fields"

          val hint =
            syn match {
              case SourceConverter.ConstructorSyntax.Pat(_) =>
                Doc.line + Doc.text(
                  "if you want to ignore those fields, add a ... to signify ignoring missing."
                )
              case _ =>
                // we can't ignore fields when constructing
                Doc.empty
            }
          (Doc.text(s"missing $fieldStr: ") + missingDoc + Doc.line + Doc.text(
            "in"
          ) +
            Doc.line + syn.toDoc + hint).nested(4)
        }

      val mdoc = Doc.intercalate(Doc.hardLine, mdocs)
      val notMDoc = Doc.intercalate(
        Doc.hardLine,
        notMissing.map(se => Doc.text(se.message))
      )
      val msg = if (missing.nonEmpty) {
        if (notMissing.nonEmpty) {
          mdoc + Doc.hardLine + notMDoc
        } else mdoc
      } else {
        notMDoc
      }

      val doc = headDoc + Doc.hardLine + msg + Doc.hardLine + context

      doc.render(80)
    }
  }

  case class TotalityCheckError(
      pack: PackageName,
      err: TotalityCheck.ExprError[Declaration]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val region = err.matchExpr.tag.region
      val context1 =
        lm.showRegion(region, 2, errColor)
          .getOrElse(Doc.str(region)) // we should highlight the whole region
      val teMessage = err match {
        case TotalityCheck.NonTotalMatch(_, missing) =>
          val allTypes = missing
            .traverse(_.traverseType(t => Writer(Chain.one(t), ())))
            .run
            ._1
            .toList
            .distinct
          val showT = showTypes(pack, allTypes)

          given Document[Type] = Document.instance(showT)
          val doc = Pattern.compiledDocument[Type]

          Doc.text("non-total match, missing: ") +
            (Doc.intercalate(
              Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_))
            ))
        case TotalityCheck.UnreachableBranches(_, unreachableBranches) =>
          val allTypes = unreachableBranches
            .traverse(_.traverseType(t => Writer(Chain.one(t), ())))
            .run
            ._1
            .toList
            .distinct
          val showT = showTypes(pack, allTypes)

          given Document[Type] = Document.instance(showT)
          val doc = Pattern.compiledDocument[Type]

          Doc.text("unreachable branches: ") +
            (Doc.intercalate(
              Doc.char(',') + Doc.lineOrSpace,
              unreachableBranches.toList.map(doc.document(_))
            ))
        case TotalityCheck.InvalidPattern(_, err) =>
          import TotalityCheck._
          err match {
            case ArityMismatch((_, n), _, _, exp, found) =>
              Doc.text(
                s"arity mismatch: ${n.asString} expected $exp parameters, found $found"
              )
            case UnknownConstructor((_, n), _, _) =>
              Doc.text(s"unknown constructor: ${n.asString}")
            case InvalidStrPat(pat, _) =>
              Doc.text(s"invalid string pattern: ") +
                Document[Pattern.Parsed].document(pat) +
                Doc.text(" (adjacent string bindings aren't allowed)")
            case MultipleSplicesInPattern(_, _) =>
              // TODO: get printing of compiled patterns working well (https://github.com/johnynek/bosatsu/issues/4)
              // val docp = Document[Pattern.Parsed].document(Pattern.ListPat(pat)) +
              Doc.text(
                "multiple splices in pattern, only one per match allowed"
              )
          }
      }
      val prefix = sourceMap.headLine(pack, Some(region))
      val doc = prefix + Doc.hardLine +
        context1 + Doc.hardLine + teMessage

      doc.render(80)
    }
  }

  case class UnusedLetError(
      pack: PackageName,
      errs: NonEmptyList[(Identifier.Bindable, Region)]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val docs = errs
        .sortBy(_._2)
        .map { case (bn, region) =>
          val rdoc = lm
            .showRegion(region, 2, errColor)
            .getOrElse(Doc.str(region)) // we should highlight the whole region
          val message = Doc.text("unused let binding: " + bn.sourceCodeRepr)
          message + Doc.hardLine + rdoc
        }

      val packDoc = sourceMap.headLine(pack, Some(errs.head._2))
      val line2 = Doc.hardLine + Doc.hardLine
      (packDoc + (line2 + Doc.intercalate(line2, docs.toList)).nested(2))
        .render(80)
    }
  }

  case class RecursionError(
      pack: PackageName,
      err: DefRecursionCheck.RecursionError
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val ctx = lm
        .showRegion(err.region, 2, errColor)
        .getOrElse(Doc.str(err.region)) // we should highlight the whole region
      val errMessage = err.message
      // TODO use the sourceMap/regions in RecursionError (https://github.com/johnynek/bosatsu/issues/4)
      val packDoc = sourceMap.headLine(pack, Some(err.region))
      val doc = packDoc + Doc.hardLine + Doc.text(errMessage) +
        Doc.hardLine + ctx + Doc.hardLine

      doc.render(80)
    }
  }

  case class DuplicatedPackageError(
      dups: NonEmptyMap[PackageName, (String, NonEmptyList[String])]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val packDoc = Doc.text("package ")
      val dupInDoc = Doc.text(" duplicated in ")
      val dupMessages = dups.toSortedMap
        .map { case (pname, (one, nelist)) =>
          val dupsrcs = Doc
            .intercalate(
              Doc.comma + Doc.lineOrSpace,
              (one :: nelist.toList).sorted
                .map(Doc.text(_))
            )
            .nested(4)
          packDoc + Doc.text(pname.asString) + dupInDoc + dupsrcs
        }

      Doc.intercalate(Doc.line, dupMessages).render(80)
    }
  }

  case class KindInferenceError(
      pack: PackageName,
      kindError: KindFormula.Error,
      regions: Map[Type.Const.Defined, Region]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val region = regions(kindError.dt.toTypeConst)
      val ctx = lm
        .showRegion(region, 2, errColor)
        .getOrElse(Doc.str(region)) // we should highlight the whole region
      val prefix = sourceMap.headLine(pack, Some(region))
      val message = kindError match {
        case KindFormula.Error.Unsatisfiable(_, _, _, _) =>
          // TODO: would be good to give a more precise problem, e.g. which (https://github.com/johnynek/bosatsu/issues/4)
          // type parameters are the problem.
          Doc.text("could not solve for valid variances")
        case KindFormula.Error.FromShapeError(se) =>
          se match {
            case Shape.UnificationError(_, cons, left, right) =>
              Doc.text("shape error: expected ") + Shape.shapeDoc(left) + Doc
                .text(" and ") + Shape.shapeDoc(right) +
                Doc.text(
                  s" to match in the constructor ${cons.name.sourceCodeRepr}"
                ) + Doc.hardLine
            case Shape.ShapeMismatch(_, cons, outer, tyApp, right) =>
              val tmap = showTypes(pack, outer :: tyApp :: Nil)
              val typeDoc =
                if (outer != (tyApp: Type))
                  (tmap(outer) + Doc.text(" at application ") + tmap(tyApp))
                else tmap(outer)

              Doc.text("shape error: expected ") + Shape.shapeDoc(right) + Doc
                .text(" -> ?") + Doc.text(" but found * ") +
                Doc.text(
                  s"in the constructor ${cons.name.sourceCodeRepr} inside type "
                ) +
                typeDoc +
                Doc.hardLine
            case Shape.FinishFailure(dt, left, right) =>
              val tdoc =
                showTypes(pack, dt.toTypeTyConst :: Nil)(dt.toTypeTyConst)
              Doc.text("in type ") + tdoc + Doc.text(
                " could not unify shapes: "
              ) + Shape.shapeDoc(left) + Doc.text(" and ") +
                Shape.shapeDoc(right)
            case Shape.ShapeLoop(dt, tpe, _) =>
              val tpe2 = tpe match {
                case Left(ap) => ap
                case Right(v) => Type.TyVar(v)
              }
              val tdocs = showTypes(pack, dt.toTypeTyConst :: tpe2 :: Nil)

              Doc.text("in type ") + tdocs(dt.toTypeTyConst) + Doc.text(
                " cyclic dependency encountered in "
              ) +
                tdocs(tpe2)
            case Shape.UnboundVar(dt, cfn, v) =>
              val tpe2 = Type.TyVar(v)
              val tdocs = showTypes(pack, dt.toTypeTyConst :: tpe2 :: Nil)

              val cfnMsg =
                if (dt.isStruct) Doc.empty
                else {
                  Doc.text(s" in constructor ${cfn.name.sourceCodeRepr} ")
                }
              Doc.text("in type ") + tdocs(dt.toTypeTyConst) +
                Doc.text(" unbound type variable ") + tdocs(tpe2) + cfnMsg
            case Shape.UnknownConst(dt, cfn, c) =>
              val tpe2 = Type.TyConst(c)
              val tdocs = showTypes(pack, dt.toTypeTyConst :: tpe2 :: Nil)

              val cfnMsg =
                if (dt.isStruct) Doc.empty
                else {
                  Doc.text(s" in constructor ${cfn.name.sourceCodeRepr} ")
                }
              Doc.text("in type ") + tdocs(dt.toTypeTyConst) +
                Doc.text(" unknown type ") + tdocs(tpe2) + cfnMsg
          }
      }
      (prefix + Doc.hardLine + message + Doc.hardLine + ctx).render(80)
    }
  }

  case class UnusedImport(
      inPack: PackageName,
      badImports: NonEmptyList[Import[PackageName, Unit]]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val prefix = sourceMap.headLine(inPack, None)
      val di = (Doc.hardLine + Doc.intercalate(
        Doc.hardLine,
        badImports.toList.map(Document[Import[PackageName, Unit]].document(_))
      ))
        .nested(2)

      val imports =
        if (badImports.tail.lengthCompare(0) == 0) "import" else "imports"
      (prefix + Doc.hardLine + Doc.text(
        s"unused $imports of:"
      ) + di + Doc.hardLine).render(80)
    }
  }

  case class UnusedLets(
      inPack: PackageName,
      unusedLets: NonEmptyList[
        (Identifier.Bindable, RecursionKind, TypedExpr[Any], Region)
      ]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) =
      UnusedLetError(
        inPack,
        unusedLets.map { case (b, _, _, r) => (b, r) }
      ).message(sourceMap, errColor)
  }
}
