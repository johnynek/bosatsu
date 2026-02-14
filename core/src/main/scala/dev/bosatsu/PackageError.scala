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
  def showTypes(
      pack: PackageName,
      tpes: List[Type],
      localTypeNames: Set[TypeName] = Set.empty
  ): Map[Type, Doc] = {
    // TODO: we should use the imports in each package to talk about (https://github.com/johnynek/bosatsu/issues/4)
    // types in ways that are local to that package
    Require(pack ne null)
    TypeRenderer.documents(tpes, TypeRenderer.Context(pack, localTypeNames), 80)
  }

  def nearest[A](
      ident: Identifier,
      existing: Iterable[(Identifier, A)],
      count: Int
  ): List[(Identifier, A)] =
    NameSuggestion
      .nearest(
        ident,
        existing.iterator.map { case (i, a) =>
          NameSuggestion.Candidate(i, a)
        }.toList,
        count
      )
      .map(c => (c.ident, c.value))

  private def quoted(ident: Identifier): Doc =
    Doc.char('`') + Doc.text(ident.sourceCodeRepr) + Doc.char('`')

  private def suggestedName(
      ident: Identifier,
      label: Option[String]
  ): Doc =
    label match {
      case None          => quoted(ident)
      case Some(lblText) => Doc.text(lblText) + Doc.space + quoted(ident)
    }

  private def didYouMeanDoc(
      suggestions: List[(Identifier, Option[String])]
  ): Doc =
    suggestions match {
      case Nil => Doc.empty
      case (ident, label) :: Nil =>
        Doc.hardLine + Doc.text("Did you mean ") + suggestedName(
          ident,
          label
        ) + Doc.char('?')
      case many =>
        val docs = many.map { case (ident, label) =>
          suggestedName(ident, label)
        }
        Doc.hardLine + Doc.text("Did you mean one of: ") +
          Doc
            .intercalate(Doc.text(",") + Doc.lineOrSpace, docs)
            .grouped + Doc.char('?')
    }

  private def nearestConstructorsDoc(
      suggestions: List[Identifier.Constructor]
  ): Doc =
    suggestions match {
      case Nil => Doc.empty
      case one :: Nil =>
        Doc.hardLine + Doc.text("Did you mean constructor ") + quoted(
          one
        ) + Doc.char('?')
      case many =>
        Doc.hardLine + Doc.text("Nearest constructors in scope: ") +
          Doc
            .intercalate(
              Doc.text(",") + Doc.lineOrSpace,
              many.map(quoted)
            )
            .grouped + Doc.char('.')
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
            s"${n.sourceCodeRepr}$pos"
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

      val near =
        if (candidates.isEmpty) {
          Doc.empty
        } else {
          Doc.text(" Nearest: ") +
            Doc
              .intercalate(Doc.text(",") + Doc.line, candidates)
              .nested(4)
              .grouped
        }

      (sourceMap.headLine(importingName, None) + Doc.hardLine + Doc.text(
        s"does not have name ${iname.originalName.sourceCodeRepr}."
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
        .map(_.name.ident.sourceCodeRepr)
        .sorted
        .mkString(", ")
  }

  case class TypeErrorIn(
      tpeErr: Infer.Error,
      pack: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, Expr[Declaration])],
      externals: Map[Identifier.Bindable, (Type, Region)],
      letNameRegions: Map[Identifier.Bindable, Region],
      localTypeNames: Set[TypeName]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) = {
      val (lm, _) = sourceMap.getMapSrc(pack)
      val renderCtx = TypeRenderer.Context(pack, localTypeNames)

      def contextDoc(region: Region): Doc =
        lm.showRegion(region, 2, errColor).getOrElse(Doc.str(region))

      def renderedTypeKey(tpe: Type): String =
        TypeRenderer.render(tpe, renderCtx, 80)

      def orient(
          left: (Type, Region),
          right: (Type, Region),
          direction: Infer.Error.Direction
      ): ((Type, Region), (Type, Region)) =
        direction match {
          case Infer.Error.Direction.ExpectLeft  => (left, right)
          case Infer.Error.Direction.ExpectRight => (right, left)
          case Infer.Error.Direction.Unknown     => (left, right)
        }

      def baseMismatch(
          tpeErr: Infer.Error.TypeError
      ): Option[((Type, Region), (Type, Region), Infer.Error.Direction)] =
        tpeErr match {
          case Infer.Error.NotUnifiable(t0, t1, r0, r1, direction) =>
            Some(((t0, r0), (t1, r1), direction))
          case Infer.Error.SubsumptionCheckFailure(
                t0,
                t1,
                r0,
                r1,
                _,
                direction
              ) =>
            Some(((t0, r0), (t1, r1), direction))
          case Infer.Error.ContextualTypeError(_, direction, cause) =>
            baseMismatch(cause).map { case (left, right, causeDirection) =>
              val finalDirection =
                causeDirection match {
                  case Infer.Error.Direction.Unknown => direction
                  case found                         => found
                }
              (left, right, finalDirection)
            }
          case _ =>
            None
        }

      def expectedFound(
          tpeErr: Infer.Error.TypeError
      ): Option[((Type, Region), (Type, Region))] =
        baseMismatch(tpeErr).map { case (left, right, direction) =>
          orient(left, right, direction)
        }

      def mismatchEvidenceRegion(
          tpeErr: Infer.Error.Single
      ): Option[Region] =
        tpeErr match {
          case te: Infer.Error.TypeError =>
            expectedFound(te).map(_._1._2)
          case _ =>
            None
        }

      def dedupKey(
          tpeErr: Infer.Error.Single
      ): Option[(String, Int, String, String)] =
        tpeErr match {
          case c @ Infer.Error.ContextualTypeError(site, _, _) =>
            val (expectedKey, foundKey) =
              expectedFound(c)
                .map { case ((exp, _), (found, _)) =>
                  (renderedTypeKey(exp), renderedTypeKey(found))
                }
                .getOrElse(("", ""))
            Some((s"context:$site", site.hashCode, expectedKey, foundKey))
          case e @ Infer.Error.NotUnifiable(_, _, r0, r1, _) =>
            val (expectedKey, foundKey) =
              expectedFound(e)
                .map { case ((exp, _), (found, _)) =>
                  (renderedTypeKey(exp), renderedTypeKey(found))
                }
                .getOrElse(("", ""))
            Some((
              "not-unifiable",
              math.min(r0.start, r1.start),
              expectedKey,
              foundKey
            ))
          case e @ Infer.Error.SubsumptionCheckFailure(_, _, r0, r1, _, _) =>
            val (expectedKey, foundKey) =
              expectedFound(e)
                .map { case ((exp, _), (found, _)) =>
                  (renderedTypeKey(exp), renderedTypeKey(found))
                }
                .getOrElse(("", ""))
            Some((
              "subsume",
              math.min(r0.start, r1.start),
              expectedKey,
              foundKey
            ))
          case _ =>
            None
        }

      def dedupSingles(
          singles: List[Infer.Error.Single]
      ): List[(Infer.Error.Single, List[Region])] = {
        val keyToIdx = scala.collection.mutable.Map
          .empty[(String, Int, String, String), Int]
        val acc =
          scala.collection.mutable.ArrayBuffer
            .empty[(Infer.Error.Single, List[Region])]

        singles.foreach { single =>
          val evidence = mismatchEvidenceRegion(single).toList
          dedupKey(single) match {
            case Some(key) =>
              keyToIdx.get(key) match {
                case Some(idx) =>
                  val (keep, regions) = acc(idx)
                  acc.update(idx, (keep, regions ::: evidence))
                case None      =>
                  keyToIdx.update(key, acc.size)
                  acc.append((single, evidence))
              }
            case None      =>
              acc.append((single, evidence))
          }
        }

        acc.toList
      }

      def occurrenceDoc(label: String, count: Int): Doc =
        if (count > 1)
          Doc.hardLine + Doc.text(s"This unknown $label appears $count times.")
        else Doc.empty

      def isUseBeforeDef(name: Identifier, region: Region): Boolean =
        name match {
          case b: Identifier.Bindable =>
            letNameRegions.get(b).exists(_.start > region.start)
          case _ =>
            false
        }

      def singleToDoc(
          tpeErr: Infer.Error.Single,
          occurrences: Int,
          evidenceRegions: List[Region]
      ): Doc = {
        val (teMessage, region) = tpeErr match {
          case c @ Infer.Error.ContextualTypeError(site, _, cause) =>
            site match {
              case appSite: Infer.Error.MismatchSite.AppArg =>
                val expectedType = appSite.expectedArgType
                val expectedFromCause =
                  expectedFound(c)
                val expected =
                  expectedFromCause
                    .map(_._1)
                    .getOrElse((expectedType, appSite.functionRegion))
                val found =
                  expectedFromCause
                    .map(_._2)
                    .orElse {
                      baseMismatch(cause).flatMap {
                        case ((left, _), (right, rightRegion), _)
                            if (left == expectedType) && (right =!= expectedType) =>
                          Some((right, rightRegion))
                        case ((left, leftRegion), (right, _), _)
                            if (right == expectedType) && (left =!= expectedType) =>
                          Some((left, leftRegion))
                        case _ =>
                          None
                      }
                    }
                    .getOrElse((expectedType, appSite.argumentRegion))

                val tmap = showTypes(
                  pack,
                  List(expected._1, found._1, appSite.functionType),
                  localTypeNames
                )
                val fnLabel = appSite.functionName
                  .map { full =>
                    val localPrefix = s"${pack.asString}::"
                    if (full.startsWith(localPrefix))
                      full.substring(localPrefix.length)
                    else full
                  }
                  .getOrElse("function")
                val fnContext =
                  if (appSite.functionRegion =!= appSite.argumentRegion) {
                    Doc.hardLine + Doc.text("function site:") + Doc.hardLine +
                      contextDoc(appSite.functionRegion)
                  } else {
                    Doc.empty
                  }

                (
                  Doc.text(
                    s"type mismatch in call to $fnLabel, argument ${appSite.argIndex + 1} of ${appSite.argCount}:"
                  ) + Doc.hardLine +
                    Doc.text("expected: ") + tmap(expected._1) + Doc.hardLine +
                    Doc.text("found: ") + tmap(found._1) + Doc.hardLine +
                    Doc.text("function type: ") + tmap(appSite.functionType) +
                    Doc.hardLine + Doc.text("argument site:") + Doc.hardLine +
                    contextDoc(appSite.argumentRegion) + fnContext,
                  Some(appSite.argumentRegion)
                )

              case patSite: Infer.Error.MismatchSite.MatchPattern =>
                val tmap = showTypes(
                  pack,
                  List(
                    patSite.expectedScrutineeType,
                    patSite.foundPatternType
                  ),
                  localTypeNames
                )
                given Document[Type] =
                  Document.instance(t =>
                    TypeRenderer.document(t, renderCtx, 80)
                  )
                val patternDoc =
                  Pattern.compiledDocument[Type]
                    .document(patSite.pattern)
                    .render(80)
                val scrutineeContext =
                  if (patSite.scrutineeRegion =!= patSite.patternRegion) {
                    Doc.hardLine + Doc.text("scrutinee site:") + Doc.hardLine +
                      contextDoc(patSite.scrutineeRegion)
                  } else {
                    Doc.empty
                  }

                (
                  Doc.text("pattern type mismatch:") + Doc.hardLine +
                    Doc.text("pattern: ") + Doc.text(patternDoc) + Doc.hardLine +
                    Doc.text("expected scrutinee type: ") + tmap(
                      patSite.expectedScrutineeType
                    ) + Doc.hardLine +
                    Doc.text("found pattern type: ") + tmap(
                      patSite.foundPatternType
                    ) + Doc.hardLine +
                    Doc.text("pattern site:") + Doc.hardLine +
                    contextDoc(patSite.patternRegion) + scrutineeContext,
                  Some(patSite.patternRegion)
                )
            }

          case Infer.Error.NotUnifiable(t0, t1, r0, r1, direction) =>
            val ((expectedType, expectedRegion), (foundType, foundRegion)) =
              orient((t0, r0), (t1, r1), direction)
            val context0 =
              if (expectedRegion === foundRegion)
                Doc.space // sometimes the region of the error is the same on right and left
              else {
                val m = contextDoc(expectedRegion)
                Doc.hardLine + m + Doc.hardLine
              }
            val context1 = contextDoc(foundRegion)

            val fnHint =
              (expectedType, foundType) match {
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

            val tmap = showTypes(pack, List(expectedType, foundType), localTypeNames)
            val evidenceDocs =
              evidenceRegions.distinct.sortBy(_.start).map(contextDoc)
            val evidenceDoc =
              if (evidenceDocs.lengthCompare(1) > 0) {
                Doc.text("evidence sites:") + Doc.hardLine +
                  Doc.intercalate(Doc.hardLine + Doc.hardLine, evidenceDocs)
              } else {
                context1
              }
            val doc = Doc.text("type error: expected type ") + tmap(expectedType) +
              context0 + Doc.text("to be the same as type ") + tmap(foundType) +
              Doc.hardLine + fnHint + evidenceDoc

            (doc, Some(expectedRegion))

          case Infer.Error.VarNotInScope((_, name), scope, region) =>
            val ctx = contextDoc(region)

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
                val defCtx = contextDoc(defRegion)

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
                val inScopeCandidates = scope.iterator.map { case ((p, n), _) =>
                  val pri =
                    p match {
                      case None                   => NameSuggestion.ScopePriority.Local
                      case Some(pn) if pn == pack => NameSuggestion.ScopePriority.SamePackage
                      case Some(_)                => NameSuggestion.ScopePriority.Imported
                    }
                  NameSuggestion.Candidate(n, pri, pri)
                }
                val letCandidates = lets.iterator.map { case (n, _, _) =>
                  NameSuggestion.Candidate(
                    n,
                    NameSuggestion.ScopePriority.Local,
                    NameSuggestion.ScopePriority.Local
                  )
                }
                val allCandidates =
                  inScopeCandidates.toList ::: letCandidates.toList
                val candidates = NameSuggestion
                  .nearest(name, allCandidates, 3)
                  .map { cand =>
                    val label =
                      cand.value match {
                        case NameSuggestion.ScopePriority.Local |
                            NameSuggestion.ScopePriority.SamePackage =>
                          Some("local value")
                        case NameSuggestion.ScopePriority.Imported =>
                          None
                      }
                    (cand.ident, label)
                  }
                (
                  Doc.text("Unknown name ") + quoted(name) + Doc.char('.') +
                    didYouMeanDoc(candidates) + occurrenceDoc(
                      "name",
                      occurrences
                    ) + Doc.hardLine + ctx,
                  Some(region)
                )
            }

          case Infer.Error.SubsumptionCheckFailure(
                t0,
                t1,
                r0,
                r1,
                _,
                direction
              ) =>
            val ((expectedType, expectedRegion), (foundType, foundRegion)) =
              orient((t0, r0), (t1, r1), direction)
            val context0 =
              if (foundRegion === expectedRegion)
                Doc.space // sometimes the region of the error is the same on right and left
              else {
                val m = contextDoc(foundRegion)
                Doc.hardLine + m + Doc.hardLine
              }
            val context1 = contextDoc(expectedRegion)

            val tmap = showTypes(pack, List(expectedType, foundType), localTypeNames)
            val evidenceDocs =
              evidenceRegions.distinct.sortBy(_.start).map(contextDoc)
            val evidenceDoc =
              if (evidenceDocs.lengthCompare(1) > 0) {
                Doc.text("evidence sites:") + Doc.hardLine +
                  Doc.intercalate(Doc.hardLine + Doc.hardLine, evidenceDocs)
              } else {
                context1
              }
            val doc = Doc.text("type ") + tmap(foundType) + context0 +
              Doc.text("does not subsume expected type ") + tmap(expectedType) + Doc.hardLine +
              evidenceDoc

            (doc, Some(foundRegion))

          case uc @ Infer.Error.UnknownConstructor((_, n), region, _) =>
            val near = NameSuggestion
              .nearest(
                n,
                uc.knownConstructors.map { case (p, c) =>
                  val pri =
                    if (p == pack) NameSuggestion.ScopePriority.SamePackage
                    else NameSuggestion.ScopePriority.Imported
                  NameSuggestion.Candidate(c, c, pri)
                },
                3
              )
              .map(_.value)

            val context = contextDoc(region)

            val doc =
              Doc.text("Unknown constructor ") + quoted(n) + Doc.char('.') +
                nearestConstructorsDoc(near) + occurrenceDoc(
                  "constructor",
                  occurrences
                ) + Doc.hardLine + context
            (doc, Some(region))

          case Infer.Error.KindCannotTyApply(applied, region) =>
            val tmap = showTypes(pack, applied :: Nil, localTypeNames)
            val context = contextDoc(region)
            val doc = Doc.text("kind error: for kind of the left of ") +
              tmap(applied) + Doc.text(
                " is *. Cannot apply to kind *."
              ) + Doc.hardLine +
              context

            (doc, Some(region))

          case Infer.Error.KindExpectedType(tpe, kind, region) =>
            val tmap = showTypes(pack, tpe :: Nil, localTypeNames)
            val context = contextDoc(region)
            val doc = Doc.text("expected type ") +
              tmap(tpe) + Doc.text(
                " to have kind *, which is to say be a valid value, but it is kind "
              ) + Kind.toDoc(kind) + Doc.hardLine +
              context

            (doc, Some(region))

          case Infer.Error.KindInvalidApply(applied, leftK, rightK, region) =>
            val leftT = applied.on
            val rightT = applied.arg
            val tmap = showTypes(
              pack,
              applied :: leftT :: rightT :: Nil,
              localTypeNames
            )
            val context = contextDoc(region)
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
            val tmap = showTypes(pack, meta :: rightT :: Nil, localTypeNames)
            val context0 = contextDoc(metaR)
            val context1 =
              if (metaR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine +
                  contextDoc(rightR) +
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
            val tmap = showTypes(pack, tymeta :: in :: Nil, localTypeNames)
            val context0 = contextDoc(metaR)
            val context1 =
              if (metaR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine +
                  contextDoc(rightR) +
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
            val tmap = showTypes(pack, tpe :: Nil, localTypeNames)
            val context = contextDoc(region)

            (
              Doc.text("the type ") + tmap(tpe) + Doc.text(
                " is not polymorphic enough"
              ) + Doc.hardLine + context,
              Some(region)
            )

          case Infer.Error.ArityMismatch(leftA, leftR, rightA, rightR) =>
            val context0 = contextDoc(leftR)
            val context1 =
              if (leftR =!= rightR) {
                Doc.text(" at: ") + Doc.hardLine + contextDoc(rightR)
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
            val context = contextDoc(region)

            (
              Doc.text(
                s"function with $found arguments is too large. Maximum function argument count is $max."
              ) + Doc.hardLine + context,
              Some(region)
            )

          case Infer.Error.UnexpectedBound(bound, _, reg, _) =>
            val tyvar = Type.TyVar(bound)
            val tmap = showTypes(pack, tyvar :: Nil, localTypeNames)
            val context = contextDoc(reg)

            (
              Doc.text("unexpected bound: ") + tmap(
                tyvar
              ) + Doc.hardLine + context,
              Some(reg)
            )

          case Infer.Error.UnionPatternBindMismatch(_, names, region) =>
            val context = contextDoc(region)

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
            val context = contextDoc(reg)

            (
              Doc.text(
                s"Use of unimported type. Add `from ${const.packageName.asString} import ${const.name.ident.sourceCodeRepr}`"
              ) + Doc.hardLine + context,
              Some(reg)
            )

          case ie: Infer.Error.InternalError =>
            val context = contextDoc(ie.region)
            (Doc.text(ie.message) + Doc.hardLine + context, Some(ie.region))

        }
        val h = sourceMap.headLine(pack, region)
        h + Doc.hardLine + teMessage
      }

      def aggregateSingles(
          errs: List[(Infer.Error.Single, List[Region])]
      ): List[(Infer.Error.Single, Int, List[Region])] = {
        val bldr = scala.collection.mutable.ArrayBuffer
          .empty[(Infer.Error.Single, Int, List[Region])]
        val indexOfKey = scala.collection.mutable.Map.empty[(Int, Identifier), Int]

        def add(err: Infer.Error.Single, evidence: List[Region]): Unit =
          bldr.append((err, 1, evidence))

        errs.foreach { case (err, evidence) =>
          val keyOpt =
            err match {
              case Infer.Error.VarNotInScope((_, n), _, region)
                  if !isUseBeforeDef(n, region) =>
                Some((0, n))
              case Infer.Error.UnknownConstructor((_, n), _, _) =>
                Some((1, n))
              case _ =>
                None
            }

          keyOpt match {
            case Some(key) =>
              indexOfKey.get(key) match {
                case Some(idx) =>
                  val (first, count, currentEvidence) = bldr(idx)
                  bldr.update(idx, (first, count + 1, currentEvidence ::: evidence))
                case None =>
                  indexOfKey.update(key, bldr.size)
                  add(err, evidence)
              }
            case None =>
              add(err, evidence)
          }
        }
        bldr.toList
      }

      val finalDoc = tpeErr match {
        case s: Infer.Error.Single =>
          singleToDoc(
            s,
            occurrences = 1,
            evidenceRegions = mismatchEvidenceRegion(s).toList
          )
        case c @ Infer.Error.Combine(_, _) =>
          val twoLines = Doc.hardLine + Doc.hardLine
          aggregateSingles(dedupSingles(c.flatten.iterator.toList))
            .map { case (single, occurrences, evidenceRegions) =>
              singleToDoc(single, occurrences, evidenceRegions)
            }
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
            allMissing.sorted.map(m => Doc.text(m.sourceCodeRepr))
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
          val guardHint =
            if (err.matchExpr.branches.exists(_.guard.nonEmpty)) {
              Doc.hardLine + Doc.text(
                "guarded branches do not count toward totality; add an unguarded fallback case"
              )
            } else Doc.empty

          Doc.text("non-total match, missing: ") +
            (Doc.intercalate(
              Doc.char(',') + Doc.lineOrSpace,
              missing.toList.map(doc.document(_))
            )) + guardHint
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
                s"arity mismatch: ${n.sourceCodeRepr} expected $exp parameters, found $found"
              )
            case UnknownConstructor((_, n), _, env) =>
              val near = NameSuggestion
                .nearest(
                  n,
                  env.typeConstructors.keysIterator.map { case (p, c) =>
                    val pri =
                      if (p == pack) NameSuggestion.ScopePriority.SamePackage
                      else NameSuggestion.ScopePriority.Imported
                    NameSuggestion.Candidate(c, c, pri)
                  }.toList,
                  3
                )
                .map(_.value)
              Doc.text("Unknown constructor ") + quoted(n) + Doc.char('.') +
                nearestConstructorsDoc(near)
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

  private def unusedValueMessage(
      pack: PackageName,
      errs: NonEmptyList[(Identifier.Bindable, Region)],
      sourceMap: Map[PackageName, (LocationMap, String)],
      errColor: Colorize,
      hints: List[String]
  ): String = {
    val (lm, _) = sourceMap.getMapSrc(pack)
    val sorted = errs.sortBy(_._2)
    val unusedDocs = sorted.map { case (bn, region) =>
      val rdoc = lm
        .showRegion(region, 2, errColor)
        .getOrElse(Doc.str(region)) // we should highlight the whole region
      val message = Doc.text(s"unused value '${bn.sourceCodeRepr}'")
      message + Doc.hardLine + rdoc
    }

    val maybeCount =
      if (sorted.tail.isEmpty) None
      else Some(Doc.text(s"found ${sorted.length} unused values."))

    val maybeHints =
      if (hints.isEmpty) None
      else {
        Some(
          Doc.text("How to resolve:") + Doc.hardLine +
            Doc.intercalate(
              Doc.hardLine,
              hints.map { hint =>
                Doc.text(s"- $hint")
              }
            )
        )
      }

    val bodyBlocks = unusedDocs.toList ::: List(maybeCount, maybeHints).flatten
    val line2 = Doc.hardLine + Doc.hardLine
    val packDoc = sourceMap.headLine(pack, Some(sorted.head._2))
    (packDoc + (line2 + Doc.intercalate(line2, bodyBlocks)).nested(2)).render(80)
  }

  case class UnusedLetError(
      pack: PackageName,
      errs: NonEmptyList[(Identifier.Bindable, Region)]
  ) extends PackageError {
    def message(
        sourceMap: Map[PackageName, (LocationMap, String)],
        errColor: Colorize
    ) =
      unusedValueMessage(
        pack,
        errs,
        sourceMap,
        errColor,
        List(
          "use the value in an expression that contributes to the result",
          "if intentional, ignore it with `_` (for example: `_ = <expr>`)"
        )
      )
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
      unusedValueMessage(
        inPack,
        unusedLets.map { case (b, _, _, r) => (b, r) },
        sourceMap,
        errColor,
        if (unusedLets.tail.isEmpty) {
          val name = unusedLets.head._1.sourceCodeRepr
          List(
            s"add '$name' to exports",
            "rebind it as `_ = <expr>`",
            "use it from `tests` (reachable from the final value of type Bosatsu::Test)",
            "use it from `main` (reachable from the final non-test value)"
          )
        } else {
          List(
            "add needed values to exports",
            "rebind intentionally unused values as `_ = <expr>`",
            "use them from `tests` (reachable from the final value of type Bosatsu::Test)",
            "use them from `main` (reachable from the final non-test value)"
          )
        }
      )
  }
}
