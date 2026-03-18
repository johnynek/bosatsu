package dev.bosatsu.tool

import cats.data.{Ior, NonEmptyList, Validated, ValidatedNel}
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}
import dev.bosatsu.{
  CompileOptions,
  Declaration,
  Expr,
  Identifier,
  Package,
  PackageCustoms,
  PackageError,
  PackageMap,
  PackageName,
  Par,
  PlatformIO,
  RecursionKind,
  ShadowedBindingTypeCheck,
  SourceConverter,
  TotalityCheck,
  TypeName,
  UnusedLetCheck
}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.rankn.Infer
import org.typelevel.paiges.Doc
import scala.util.control.NonFatal

import cats.syntax.all._

object CompilerApi {
  private final case class DiagnosticCategory(
      singularLabel: String,
      pluralLabel: String,
      order: Int
  )
  private object DiagnosticCategory {
    val UnusedValue =
      DiagnosticCategory("unused value", "unused values", 0)
    val TypeError =
      DiagnosticCategory("type error", "type errors", 1)
    val RecursionError =
      DiagnosticCategory("recursion error", "recursion errors", 2)
    val TotalityError =
      DiagnosticCategory("totality error", "totality errors", 3)
    val SourceConversionError =
      DiagnosticCategory("source conversion error", "source conversion errors", 4)
    val ImportError =
      DiagnosticCategory("import error", "import errors", 5)
    val PackageError =
      DiagnosticCategory("package error", "package errors", 6)

    val ShadowedBinding =
      DiagnosticCategory("shadowed binding", "shadowed bindings", 1)
    val UnreachableBranch =
      DiagnosticCategory("unreachable branch", "unreachable branches", 2)
    val UnusedImport =
      DiagnosticCategory("unused import", "unused imports", 3)
  }

  private enum DiagnosticKind derives CanEqual {
    case Error
    case Warning
  }

  private final case class RenderedDiagnostic(
      category: DiagnosticCategory,
      title: String,
      summaryCount: Int,
      body: Doc
  )

  private final case class TypeCheckDiagnostics[Path](
      packages: Option[PackageMap.Inferred],
      sourcePaths: NonEmptyList[(Path, PackageName)],
      sourceMap: PackageMap.SourceMap,
      orderedDiagnostics: List[PackageError],
      hardDiagnostics: List[PackageError],
      lintDiagnostics: List[PackageError]
  )

  private def pluralizedLabel(
      count: Int,
      singular: String,
      plural: String
  ): String =
    if (count == 1) s"1 $singular" else s"$count $plural"

  private def formatDiagnosticTitle(
      category: DiagnosticCategory,
      count: Int
  ): String =
    if (count == 1) category.singularLabel
    else s"$count ${category.pluralLabel}"

  private def typeErrorCount(err: PackageError.TypeErrorIn): Int =
    err.tpeErr match {
      case _: Infer.Error.Single => 1
      case c: Infer.Error.Combine => c.flatten.length.toInt
    }

  private def classifyError(
      err: PackageError,
      body: Doc
  ): RenderedDiagnostic =
    err match {
      case e: PackageError.UnusedLetError =>
        val count = e.errs.length
        val title =
          if (count == 1) s"unused value '${e.errs.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedDiagnostic(DiagnosticCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedLets =>
        val count = e.unusedLets.length
        val title =
          if (count == 1) s"unused value '${e.unusedLets.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedDiagnostic(DiagnosticCategory.UnusedValue, title, count, body)
      case e: PackageError.TypeErrorIn =>
        val count = typeErrorCount(e)
        RenderedDiagnostic(
          DiagnosticCategory.TypeError,
          formatDiagnosticTitle(DiagnosticCategory.TypeError, count),
          count,
          body
        )
      case _: PackageError.KindInferenceError =>
        RenderedDiagnostic(
          DiagnosticCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.ShadowedBindingTypeError =>
        RenderedDiagnostic(
          DiagnosticCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.PrivateTypeEscape[?] =>
        RenderedDiagnostic(
          DiagnosticCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.RecursionError =>
        RenderedDiagnostic(
          DiagnosticCategory.RecursionError,
          "recursion error",
          1,
          body
        )
      case _: PackageError.TotalityCheckError =>
        RenderedDiagnostic(
          DiagnosticCategory.TotalityError,
          "totality error",
          1,
          body
        )
      case e: PackageError.SourceConverterErrorsIn =>
        val count = e.errs.length
        RenderedDiagnostic(
          DiagnosticCategory.SourceConversionError,
          formatDiagnosticTitle(DiagnosticCategory.SourceConversionError, count),
          count,
          body
        )
      case e: PackageError.UnusedImport =>
        val count = e.badImports.length
        RenderedDiagnostic(
          DiagnosticCategory.ImportError,
          formatDiagnosticTitle(DiagnosticCategory.ImportError, count),
          count,
          body
        )
      case e: PackageError.DuplicatedImport =>
        val count = e.duplicates.length
        RenderedDiagnostic(
          DiagnosticCategory.ImportError,
          formatDiagnosticTitle(DiagnosticCategory.ImportError, count),
          count,
          body
        )
      case _: PackageError.UnknownImportPackage[?, ?, ?] =>
        RenderedDiagnostic(
          DiagnosticCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownImportName[?, ?] =>
        RenderedDiagnostic(
          DiagnosticCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownImportFromInterface[?, ?] =>
        RenderedDiagnostic(
          DiagnosticCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownExport[?] =>
        RenderedDiagnostic(
          DiagnosticCategory.PackageError,
          "package error",
          1,
          body
        )
      case _: PackageError.CircularDependency[?, ?, ?] =>
        RenderedDiagnostic(
          DiagnosticCategory.PackageError,
          "package error",
          1,
          body
        )
      case e: PackageError.VarianceInferenceFailure =>
        val count = e.failed.length
        RenderedDiagnostic(
          DiagnosticCategory.PackageError,
          formatDiagnosticTitle(DiagnosticCategory.PackageError, count),
          count,
          body
        )
      case e: PackageError.DuplicatedPackageError =>
        val count = e.dups.length
        RenderedDiagnostic(
          DiagnosticCategory.PackageError,
          formatDiagnosticTitle(DiagnosticCategory.PackageError, count),
          count,
          body
        )
    }

  private def classifyWarning(
      err: PackageError,
      body: Doc
  ): RenderedDiagnostic =
    err match {
      case e: PackageError.UnusedLetError =>
        val count = e.errs.length
        val title =
          if (count == 1) s"unused value '${e.errs.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedDiagnostic(DiagnosticCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedLets =>
        val count = e.unusedLets.length
        val title =
          if (count == 1) s"unused value '${e.unusedLets.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedDiagnostic(DiagnosticCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedImport =>
        val count = e.badImports.length
        RenderedDiagnostic(
          DiagnosticCategory.UnusedImport,
          formatDiagnosticTitle(DiagnosticCategory.UnusedImport, count),
          count,
          body
        )
      case e: PackageError.ShadowedBindingTypeError =>
        RenderedDiagnostic(
          DiagnosticCategory.ShadowedBinding,
          s"shadowed binding '${e.err.name.sourceCodeRepr}' changes type",
          1,
          body
        )
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.UnreachableBranches(_, branches)
          ) =>
        val count = branches.length
        RenderedDiagnostic(
          DiagnosticCategory.UnreachableBranch,
          formatDiagnosticTitle(DiagnosticCategory.UnreachableBranch, count),
          count,
          body
        )
      case _ =>
        classifyError(err, body)
    }

  private def classifyDiagnostic(
      err: PackageError,
      body: Doc,
      kind: DiagnosticKind
  ): RenderedDiagnostic =
    kind match {
      case DiagnosticKind.Error   => classifyError(err, body)
      case DiagnosticKind.Warning => classifyWarning(err, body)
    }

  private def renderDiagnostic(
      idx: Int,
      diagnostic: RenderedDiagnostic
  ): Doc =
    Doc.text(s"$idx. ${diagnostic.title}") +
      (Doc.hardLine + diagnostic.body).nested(2)

  private def renderDiagnosticSummary(
      diagnostics: List[RenderedDiagnostic],
      kind: DiagnosticKind
  ): Doc = {
    val grouped = diagnostics
      .groupMapReduce(_.category)(_.summaryCount)(_ + _)
      .toList
      .sortBy { case (category, _) => category.order }
    val totalDiagnostics = grouped.iterator.map(_._2).sum
    val parts = grouped.map { case (category, count) =>
      Doc.text(pluralizedLabel(count, category.singularLabel, category.pluralLabel))
    }
    val summaryWord =
      kind match {
        case DiagnosticKind.Error =>
          if (totalDiagnostics == 1) "error" else "errors"
        case DiagnosticKind.Warning =>
          if (totalDiagnostics == 1) "warning" else "warnings"
      }
    Doc.text(s"$totalDiagnostics $summaryWord: ") +
      Doc.intercalate(Doc.text(", "), parts)
  }

  private def renderMultiDiagnosticMessage(
      diagnostics: List[RenderedDiagnostic],
      kind: DiagnosticKind
  ): Doc = {
    val divider = "-" * 72
    val dividerDoc = Doc.text(divider)
    val entrySeparator =
      Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + Doc.hardLine
    val entries = diagnostics.zipWithIndex.map { case (diag, idx) =>
      renderDiagnostic(idx + 1, diag)
    }
    val entriesDoc = Doc.intercalate(entrySeparator, entries)
    val summaryDoc = renderDiagnosticSummary(diagnostics, kind)
    entriesDoc + Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + summaryDoc
  }

  private def fromParse[F[_], Path, A](
      platformIO: PlatformIO[F, Path],
      v: ValidatedNel[PathParseError[Path], A],
      color: Colorize
  ): F[A] =
    v match {
      case Validated.Valid(a) =>
        platformIO.moduleIOMonad.pure(a)
      case Validated.Invalid(errs) =>
        platformIO.moduleIOMonad.raiseError(ParseErrors(errs, color))
    }

  private def distinctErrors(errors: List[PackageError]): List[PackageError] =
    errors.distinct

  private def partitionDiagnostics(
      errors: List[PackageError]
  ): (List[PackageError], List[PackageError]) =
    errors.partitionMap { err =>
      if (PackageError.isPostponable(err)) Right(err)
      else Left(err)
    }

  private def localTypeNames(pack: Package.Inferred): Set[TypeName] =
    pack.types.definedTypes.keysIterator.collect {
      case (packageName, tn) if packageName == pack.name => tn
    }.toSet

  private def replayTypedLintDiagnostics(
      pack: Package.Inferred
  ): List[PackageError] = {
    val shadowedBindings =
      try {
        ShadowedBindingTypeCheck
          .checkLets(pack.name, pack.lets) match {
          case Validated.Valid(_)     =>
            Nil
          case Validated.Invalid(errs) =>
            val localNames = localTypeNames(pack)
            errs.toNonEmptyList.toList.map { err =>
              PackageError.ShadowedBindingTypeError(
                pack.name,
                err,
                localNames
              ): PackageError
            }
        }
      } catch {
        case NonFatal(_) =>
          Nil
      }

    val totalityLint =
      try {
        pack.lets.traverse_ { case (_, _, expr) =>
          TotalityCheck(pack.types).checkExpr(expr)
        } match {
          case Validated.Valid(_)   =>
            Nil
          case Validated.Invalid(errs) =>
            errs
              .toNonEmptyList
              .map(err => PackageError.TotalityCheckError(pack.name, err): PackageError)
              .toList
              .filter(PackageError.isPostponable)
        }
      } catch {
        // Optimized packages can carry normalized declaration tags that are no
        // longer accepted by TotalityCheck's source-oriented validator. In that
        // case we keep the rest of the replayed lint set and skip totality
        // replay for this package.
        case NonFatal(_) =>
          Nil
      }

    (shadowedBindings ::: totalityLint)
      .filter(PackageError.isPostponable)
  }

  private def replayUnusedLets(
      packName: PackageName,
      lets: List[(Identifier.Bindable, RecursionKind, Expr[Declaration])]
  ): List[PackageError] =
    lets
      .traverse_ { case (_, _, expr) =>
        UnusedLetCheck.check(expr)
      } match {
      case Validated.Valid(_)     =>
        Nil
      case Validated.Invalid(errs) =>
        List(PackageError.UnusedLetError(packName, errs.toNonEmptyList))
    }

  private def replaySourceLintDiagnostics(
      pack: Package.Inferred,
      parsed: Package.Parsed
  ): List[PackageError] = {
    // Cached inferred packages can drop the source-level statement/region
    // detail needed for unused-let replay, so reusing the parsed source here
    // keeps warm-cache warnings aligned with cold compiles.
    val customs =
      PackageCustoms.lintDiagnostics(pack, Some(parsed.program))

    val unusedLets =
      SourceConverter
        .toProgram(
          parsed.name,
          pack.imports.map(imp => imp.copy(pack = imp.pack.name)),
          parsed.program
        ) match {
        case Ior.Left(_)         =>
          Nil
        case Ior.Right(program0) =>
          replayUnusedLets(parsed.name, program0.lets)
        case Ior.Both(_, program0) =>
          replayUnusedLets(parsed.name, program0.lets)
      }

    (customs ::: unusedLets).filter(PackageError.isPostponable)
  }

  private def replayLintDiagnostics[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      sourceFiles: NonEmptyList[PackageResolver.SourceFile[IO, Path]],
      packs: PackageMap.Inferred,
      allowedPackages: Set[PackageName],
      errColor: Colorize
  ): IO[List[PackageError]] = {
    import platformIO.moduleIOMonad

    val packMap = packs.toMap

    sourceFiles.toList
      .filter(sf => allowedPackages(sf.packageName))
      .traverse { sourceFile =>
        packMap.get(sourceFile.packageName) match {
          case None       =>
            moduleIOMonad.pure(Nil)
          case Some(pack) =>
            sourceFile.loadParsed
              .flatMap(parsed0 => fromParse(platformIO, parsed0, errColor))
              .map(parsed =>
                replaySourceLintDiagnostics(pack, parsed) :::
                  replayTypedLintDiagnostics(pack)
              )
        }
      }
      .map(_.flatten)
  }

  private def diagnosticDoc(
      sourceMap: PackageMap.SourceMap,
      diagnostics: NonEmptyList[PackageError],
      color: Colorize,
      kind: DiagnosticKind
  ): Doc = {
    val rendered = diagnostics.toList.distinct
      .map { err =>
        val message = err.message(sourceMap, color)
        classifyDiagnostic(err, Doc.text(message), kind)
      }

    kind match {
      case DiagnosticKind.Error =>
        rendered match {
          case one :: Nil => one.body
          case many       => renderMultiDiagnosticMessage(many, kind)
        }
      case DiagnosticKind.Warning =>
        renderMultiDiagnosticMessage(rendered, kind)
    }
  }

  private def WarningDoc(
      sourceMap: PackageMap.SourceMap,
      warnings: NonEmptyList[PackageError],
      color: Colorize
  ): Doc =
    diagnosticDoc(sourceMap, warnings, color, DiagnosticKind.Warning)

  private def typeCheckDiagnostics[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: NonEmptyList[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path],
      compileOptions: CompileOptions,
      replaySuccessfulLint: Boolean,
      compileCacheDirOpt: Option[Path]
  )(implicit
      ec: Par.EC
  ): IO[TypeCheckDiagnostics[Path]] = {
    import platformIO.{canPromiseF, moduleIOMonad, parallelF}

    for {
      ins <- packRes.loadSourceFiles(inputs)(platformIO)
      sourceFiles <- fromParse(platformIO, ins, errColor)
      sources = sourceFiles.map { source =>
        source.toSourceUnit(
          platformIO.pathToString(source.path),
          source.loadParsed.flatMap(parsed => fromParse(platformIO, parsed, errColor))
        )
      }
      cache: InferCache[IO] = compileCacheDirOpt match {
        case Some(cacheDir) => CompileCache.filesystem(cacheDir, platformIO)
        case None           => InferCache.noop[IO]
      }
      checked <- PackageMap.typeCheckSources[IO, String](
        sources,
        ifs,
        "predef",
        compileOptions,
        cache,
        InferPhases.default
      )
      _ <- cache.statsSnapshot.traverse_(platformIO.println)
      sourceMap = PackageMap.buildSourceMapFromSources(sources)
      pathToName = sourceFiles.map(source => (source.path, source.packageName))
      sourcePackageNames = pathToName.iterator.map(_._2).toSet
      (compileDiagnostics, compiled) = checked match {
        case Ior.Left(errs)      => (errs.toList, None)
        case Ior.Right(packs)    => (Nil, Some(packs))
        case Ior.Both(errs, pm)  => (errs.toList, Some(pm))
      }
      (hardDiagnostics0, compileLintDiagnostics) =
        partitionDiagnostics(compileDiagnostics)
      replayedLintDiagnostics <-
        compiled match {
          case Some(_) if !replaySuccessfulLint =>
            moduleIOMonad.pure(Nil)
          case Some(packs) if hardDiagnostics0.isEmpty =>
            replayLintDiagnostics(
              platformIO,
              sourceFiles,
              packs,
              sourcePackageNames,
              errColor
            )
          case _ =>
            moduleIOMonad.pure(Nil)
        }
      lintDiagnostics =
        distinctErrors(replayedLintDiagnostics ::: compileLintDiagnostics)
      hardDiagnostics = distinctErrors(hardDiagnostics0)
      orderedDiagnostics =
        if (hardDiagnostics.nonEmpty) distinctErrors(compileDiagnostics)
        else lintDiagnostics
    } yield TypeCheckDiagnostics(
      packages = compiled,
      sourcePaths = pathToName,
      sourceMap = sourceMap,
      orderedDiagnostics = orderedDiagnostics,
      hardDiagnostics = hardDiagnostics,
      lintDiagnostics = lintDiagnostics
    )
  }

  /** like typecheck, but a no-op for empty lists
    */
  private def typeCheck0[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: List[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path],
      compileOptions: CompileOptions,
      compileCacheDirOpt: Option[Path]
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    NonEmptyList.fromList(inputs) match {
      case None =>
        // we should still return the predef
        // if it is not in ifs
        val useInternalPredef =
          !ifs.exists { (p: Package.Interface) =>
            p.name == PackageName.PredefName
          }

        if (useInternalPredef) {
          platformIO.moduleIOMonad.pure(
            (
              PackageMap.fromIterable(
                PackageMap.predefCompiledForMode(compileOptions.mode) :: Nil
              ),
              Nil
            )
          )
        } else {
          platformIO.moduleIOMonad.pure((PackageMap.empty, Nil))
        }
      case Some(nel) =>
        typeCheck(
          platformIO,
          nel,
          ifs,
          errColor,
          packRes,
          compileOptions,
          compileCacheDirOpt
        )
          .map { case (m, ps) => (m, ps.toList) }
    }
  }

  def typeCheck[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: NonEmptyList[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path],
      compileOptions: CompileOptions,
      compileCacheDirOpt: Option[Path] = None
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Inferred, NonEmptyList[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    for {
      diagnostics <- typeCheckDiagnostics(
        platformIO,
        inputs,
        ifs,
        errColor,
        packRes,
        compileOptions,
        replaySuccessfulLint = false,
        compileCacheDirOpt
      )
      result <-
        diagnostics.packages match {
          case Some(packs) if diagnostics.orderedDiagnostics.isEmpty =>
            moduleIOMonad.pure((packs, diagnostics.sourcePaths))
          case _ =>
            moduleIOMonad.raiseError(
              PackageErrors(
                diagnostics.sourceMap,
                NonEmptyList.fromListUnsafe(diagnostics.orderedDiagnostics),
                errColor
              )
            )
        }
    } yield result
  }

  def typeCheckWithLintMode[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      inputs: NonEmptyList[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path],
      compileOptions: CompileOptions,
      lintMode: LintMode,
      compileCacheDirOpt: Option[Path] = None
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Inferred, NonEmptyList[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    def compiledOrError(
        diagnostics: TypeCheckDiagnostics[Path]
    ): IO[(PackageMap.Inferred, NonEmptyList[(Path, PackageName)])] =
      diagnostics.packages match {
        case Some(packs) =>
          moduleIOMonad.pure((packs, diagnostics.sourcePaths))
        case None        =>
          moduleIOMonad.raiseError(
            CliException.Basic(
              "internal compiler error: missing compiled packages after successful typecheck"
            )
          )
      }

    def warningsIfAny(
        diagnostics: TypeCheckDiagnostics[Path]
    ): IO[Unit] =
      NonEmptyList
        .fromList(diagnostics.lintDiagnostics)
        .traverse_(warnings =>
          platformIO.writeError(WarningDoc(diagnostics.sourceMap, warnings, errColor))
        )

    def hardFailure(
        diagnostics: TypeCheckDiagnostics[Path],
        errors: List[PackageError]
    ): IO[(PackageMap.Inferred, NonEmptyList[(Path, PackageName)])] =
      moduleIOMonad.raiseError(
        PackageErrors(
          diagnostics.sourceMap,
          NonEmptyList.fromListUnsafe(errors),
          errColor
        )
      )

    for {
      diagnostics <- typeCheckDiagnostics(
        platformIO,
        inputs,
        ifs,
        errColor,
        packRes,
        compileOptions,
        replaySuccessfulLint = true,
        compileCacheDirOpt
      )
      result <- lintMode match {
        case LintMode.Strict =>
          NonEmptyList.fromList(diagnostics.orderedDiagnostics) match {
            case Some(errs) =>
              moduleIOMonad.raiseError(
                PackageErrors(diagnostics.sourceMap, errs, errColor)
              )
            case None       =>
              compiledOrError(diagnostics)
          }
        case LintMode.Warn   =>
          diagnostics.hardDiagnostics match {
            case _ :: _ =>
              (
                NonEmptyList.fromList(diagnostics.lintDiagnostics) match {
                  case Some(warnings) =>
                    moduleIOMonad.raiseError(
                      WarningAndPackageErrors(
                        diagnostics.sourceMap,
                        warnings,
                        NonEmptyList.fromListUnsafe(diagnostics.hardDiagnostics),
                        errColor
                      )
                    )
                  case None           =>
                    hardFailure(diagnostics, diagnostics.hardDiagnostics)
                }
              )
            case Nil    =>
              warningsIfAny(diagnostics) *>
                compiledOrError(diagnostics)
          }
        case LintMode.Lax    =>
          diagnostics.hardDiagnostics match {
            case _ :: _ => hardFailure(diagnostics, diagnostics.hardDiagnostics)
            case Nil    => compiledOrError(diagnostics)
          }
      }
    } yield result
  }

  def buildPackMap[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      srcs: List[Path],
      deps: List[Path],
      errColor: Colorize,
      packRes: PackageResolver[IO, Path],
      compileOptions: CompileOptions,
      compileCacheDirOpt: Option[Path] = None
  )(implicit
      ec: Par.EC
  ): IO[(PackageMap.Typed[Any], List[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    for {
      packs <- platformIO.readPackages(deps)
      ifaces = packs.map(Package.interfaceOf(_))
      packsList <- typeCheck0(
        platformIO,
        srcs,
        ifaces,
        errColor,
        packRes,
        compileOptions,
        compileCacheDirOpt
      )
      (thesePacks, lst) = packsList
      packMap = packs.foldLeft(PackageMap.toAnyTyped(thesePacks))(_ + _)
    } yield (packMap, lst)
  }

  private def PackageErrors(
      sourceMap: PackageMap.SourceMap,
      errors: NonEmptyList[PackageError],
      color: Colorize
  ): CliException & Exception = {
    val errDoc = diagnosticDoc(sourceMap, errors, color, DiagnosticKind.Error)
    val messageString = errDoc.render(80)

    CliException(messageString, errDoc)
  }

  private def WarningAndPackageErrors(
      sourceMap: PackageMap.SourceMap,
      warnings: NonEmptyList[PackageError],
      errors: NonEmptyList[PackageError],
      color: Colorize
  ): CliException & Exception = {
    val warnDoc = WarningDoc(sourceMap, warnings, color)
    val errDoc = diagnosticDoc(sourceMap, errors, color, DiagnosticKind.Error)
    val fullDoc = warnDoc + Doc.hardLine + Doc.hardLine + errDoc
    val messageString = fullDoc.render(80)

    CliException(messageString, fullDoc)
  }

  private def ParseErrors[Path](
      errors: NonEmptyList[PathParseError[Path]],
      color: Colorize
  ): CliException & Exception = {

    val messages: List[String] =
      errors.toList.flatMap {
        case PathParseError.ParseFailure(pf, path) =>
          // we should never be partial here
          val (r, c) = pf.locations.toLineCol(pf.position).get
          val ctx = pf.showContext(color)
          List(
            s"failed to parse $path:${r + 1}:${c + 1}",
            ctx.render(80)
          )
        case PathParseError.FileError(path, err) =>
          err match {
            case e
                if e.getClass.getName == "java.nio.file.NoSuchFileException" =>
              // This class isn't present in scalajs, use the String
              List(s"file not found: $path")
            case _ =>
              List(
                s"failed to parse $path",
                err.getMessage,
                err.getClass.toString
              )
          }
      }

    val messageString: String = messages.mkString("\n")
    val errDoc = Doc.intercalate(Doc.hardLine, messages.map(Doc.text(_)))

    CliException(messageString, errDoc)
  }
}
