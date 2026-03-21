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

import cats.syntax.all._

object CompilerApi {
  private enum DiagnosticKind derives CanEqual {
    case Error
    case Warning
  }

  private enum ErrorCategory(
      val singularLabel: String,
      val pluralLabel: String,
      val order: Int
  ) {
    def title(count: Int): String =
      if (count == 1) singularLabel else s"$count $pluralLabel"

    // Errors and lint render in separate blocks, so each block gets its own
    // category ordering instead of trying to share a single cross-kind index.
    case UnusedValue
        extends ErrorCategory("unused value", "unused values", 0)
    case TypeError extends ErrorCategory("type error", "type errors", 1)
    case RecursionError
        extends ErrorCategory("recursion error", "recursion errors", 2)
    case RecursionForm
        extends ErrorCategory("recursion form", "recursion forms", 3)
    case TotalityError
        extends ErrorCategory("totality error", "totality errors", 4)
    case SourceConversionError
        extends ErrorCategory(
          "source conversion error",
          "source conversion errors",
          5
        )
    case ImportError extends ErrorCategory("import error", "import errors", 6)
    case PackageError
        extends ErrorCategory("package error", "package errors", 7)
  }

  private enum LintCategory(
      val singularLabel: String,
      val pluralLabel: String,
      val order: Int
  ) {
    def title(count: Int): String =
      if (count == 1) singularLabel else s"$count $pluralLabel"

    case UnusedValue
        extends LintCategory("unused value", "unused values", 0)
    case ShadowedBinding
        extends LintCategory("shadowed binding", "shadowed bindings", 1)
    case UnreachableBranch
        extends LintCategory("unreachable branch", "unreachable branches", 2)
    case RecursionForm
        extends LintCategory("recursion form", "recursion forms", 3)
    case TodoUsage extends LintCategory("todo usage", "todo usages", 4)
    case UnusedImport extends LintCategory("unused import", "unused imports", 5)
  }

  private sealed trait RenderedDiagnostic {
    def title: String
    def summaryCount: Int
    def body: Doc
  }

  private final case class RenderedError(
      category: ErrorCategory,
      title: String,
      summaryCount: Int,
      body: Doc
  ) extends RenderedDiagnostic

  private final case class RenderedLint(
      category: LintCategory,
      title: String,
      summaryCount: Int,
      body: Doc
  ) extends RenderedDiagnostic

  private final case class TypeCheckDiagnostics[Path](
      packages: Option[PackageMap.Compiled],
      sourcePaths: NonEmptyList[(Path, PackageName)],
      sourceMap: PackageMap.SourceMap,
      orderedDiagnostics: List[PackageError],
      hardDiagnostics: List[PackageError],
      lintDiagnostics: List[PackageError]
  )

  private final case class PartitionedErrors(
      postponable: List[PackageError],
      errors: List[PackageError]
  )

  private def pluralizedLabel(
      count: Int,
      singular: String,
      plural: String
  ): String =
    if (count == 1) s"1 $singular" else s"$count $plural"

  private def renderedError(
      category: ErrorCategory,
      count: Int,
      body: Doc
  ): RenderedError =
    RenderedError(category, category.title(count), count, body)

  private def renderedLint(
      category: LintCategory,
      count: Int,
      body: Doc
  ): RenderedLint =
    RenderedLint(category, category.title(count), count, body)

  private def typeErrorCount(err: PackageError.TypeErrorIn): Int =
    err.tpeErr match {
      case _: Infer.Error.Single => 1
      case c: Infer.Error.Combine => c.flatten.length.toInt
    }

  private def classifyError(
      err: PackageError,
      body: Doc
  ): RenderedError =
    err match {
      case e: PackageError.UnusedLetError =>
        val count = e.errs.length
        val title =
          if (count == 1) s"unused value '${e.errs.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedError(ErrorCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedLets =>
        val count = e.unusedLets.length
        val title =
          if (count == 1) s"unused value '${e.unusedLets.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedError(ErrorCategory.UnusedValue, title, count, body)
      case e: PackageError.TypeErrorIn =>
        val count = typeErrorCount(e)
        renderedError(ErrorCategory.TypeError, count, body)
      case _: PackageError.KindInferenceError =>
        renderedError(ErrorCategory.TypeError, 1, body)
      case _: PackageError.ShadowedBindingTypeError =>
        renderedError(ErrorCategory.TypeError, 1, body)
      case _: PackageError.PrivateTypeEscape[?] =>
        renderedError(ErrorCategory.TypeError, 1, body)
      case _: PackageError.RecursionError =>
        renderedError(ErrorCategory.RecursionError, 1, body)
      case _: PackageError.RecursionLint =>
        renderedError(ErrorCategory.RecursionForm, 1, body)
      case _: PackageError.TotalityCheckError[?] =>
        renderedError(ErrorCategory.TotalityError, 1, body)
      case e: PackageError.SourceConverterErrorsIn =>
        val count = e.errs.length
        renderedError(ErrorCategory.SourceConversionError, count, body)
      case e: PackageError.UnusedImport =>
        val count = e.badImports.length
        renderedError(ErrorCategory.ImportError, count, body)
      case e: PackageError.DuplicatedImport =>
        val count = e.duplicates.length
        renderedError(ErrorCategory.ImportError, count, body)
      case _: PackageError.UnknownImportPackage[?, ?, ?] =>
        renderedError(ErrorCategory.ImportError, 1, body)
      case _: PackageError.UnknownImportName[?, ?] =>
        renderedError(ErrorCategory.ImportError, 1, body)
      case _: PackageError.UnknownImportFromInterface[?, ?] =>
        renderedError(ErrorCategory.ImportError, 1, body)
      case _: PackageError.UnknownExport[?] =>
        renderedError(ErrorCategory.PackageError, 1, body)
      case _: PackageError.CircularDependency[?, ?, ?] =>
        renderedError(ErrorCategory.PackageError, 1, body)
      case e: PackageError.VarianceInferenceFailure =>
        val count = e.failed.length
        renderedError(ErrorCategory.PackageError, count, body)
      case e: PackageError.DuplicatedPackageError =>
        val count = e.dups.length
        renderedError(ErrorCategory.PackageError, count, body)
      case e: PackageError.TodoUsage =>
        renderedError(ErrorCategory.PackageError, e.regions.length, body)
    }

  // This classifier assumes `err` is already known to be postponable. Passing
  // a general hard error here is a bug in the lint partitioning flow.
  private def classifyWarning(
      err: PackageError,
      body: Doc
  ): RenderedLint =
    err match {
      case e: PackageError.UnusedLetError =>
        val count = e.errs.length
        val title =
          if (count == 1) s"unused value '${e.errs.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedLint(LintCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedLets =>
        val count = e.unusedLets.length
        val title =
          if (count == 1) s"unused value '${e.unusedLets.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedLint(LintCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedImport =>
        val count = e.badImports.length
        renderedLint(LintCategory.UnusedImport, count, body)
      case e: PackageError.ShadowedBindingTypeError =>
        RenderedLint(
          LintCategory.ShadowedBinding,
          s"shadowed binding '${e.err.name.sourceCodeRepr}' changes type",
          1,
          body
        )
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.UnreachableBranches(_, branches)
          ) =>
        val count = branches.length
        renderedLint(LintCategory.UnreachableBranch, count, body)
      case e: PackageError.TodoUsage =>
        renderedLint(LintCategory.TodoUsage, e.regions.length, body)
      case _: PackageError.RecursionLint =>
        renderedLint(LintCategory.RecursionForm, 1, body)
      case _ =>
        sys.error(s"unexpected non-lint warning: $err")
    }

  private def renderDiagnostic(
      idx: Int,
      diagnostic: RenderedDiagnostic
  ): Doc =
    Doc.text(s"$idx. ${diagnostic.title}") +
      (Doc.hardLine + diagnostic.body).nested(2)

  private def renderErrorSummary(
      diagnostics: List[RenderedError]
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
      if (totalDiagnostics == 1) "error" else "errors"
    Doc.text(s"$totalDiagnostics $summaryWord: ") +
      Doc.intercalate(Doc.text(", "), parts)
  }

  private def renderLintSummary(
      diagnostics: List[RenderedLint]
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
      if (totalDiagnostics == 1) "warning" else "warnings"
    Doc.text(s"$totalDiagnostics $summaryWord: ") +
      Doc.intercalate(Doc.text(", "), parts)
  }

  private def renderMultiErrorMessage(
      diagnostics: List[RenderedError]
  ): Doc = {
    val dividerDoc = Doc.char('-') * 72
    val entrySeparator =
      Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + Doc.hardLine
    val entries = diagnostics.zipWithIndex.map { case (diag, idx) =>
      renderDiagnostic(idx + 1, diag)
    }
    val entriesDoc = Doc.intercalate(entrySeparator, entries)
    val summaryDoc = renderErrorSummary(diagnostics)
    entriesDoc + Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + summaryDoc
  }

  private def renderMultiWarningMessage(
      diagnostics: List[RenderedLint]
  ): Doc = {
    val dividerDoc = Doc.char('-') * 72
    val entrySeparator =
      Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + Doc.hardLine
    val entries = diagnostics.zipWithIndex.map { case (diag, idx) =>
      renderDiagnostic(idx + 1, diag)
    }
    val entriesDoc = Doc.intercalate(entrySeparator, entries)
    val summaryDoc = renderLintSummary(diagnostics)
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

  private def partitionDiagnostics(
      errors: List[PackageError]
  ): PartitionedErrors = {
    val (postponable, hardErrors) =
      errors.partition(PackageError.isPostponable)
    PartitionedErrors(postponable, hardErrors)
  }

  private def localTypeNames(pack: Package.Compiled): Set[TypeName] =
    pack.types.definedTypes.keysIterator.collect {
      case (packageName, tn) if packageName == pack.name => tn
    }.toSet

  private def replayTypedLintDiagnostics(
      pack: Package.Compiled
  ): List[PackageError] = {
    val recursionLints =
      Package
        .recursionLints(pack)
        .map(lint => PackageError.RecursionLint(pack.name, lint): PackageError)

    val shadowedBindings =
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

    val totalityLint =
      pack.lets.traverse_ { case (_, _, expr) =>
        TotalityCheck(pack.types).checkExprReplay(expr)
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

    (recursionLints ::: shadowedBindings ::: totalityLint)
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
      pack: Package.Compiled,
      parsed: Package.Parsed,
      includeTodoUsageWarnings: Boolean
  ): List[PackageError] = {
    // Cached compiled packages can drop the source-level statement/region
    // detail needed for unused-let replay, so reusing the parsed source here
    // keeps warm-cache warnings aligned with cold compiles.
    val sourceImports = pack.imports
    val roots =
      Package.testRootBindables(pack).toList ::: Package
        .mainValue(pack)
        .map(_._1)
        .toList
    val resolvedImports =
      sourceImports.map(imp => imp.copy(pack = imp.pack.name))

    SourceConverter
      .toProgram(
        parsed.name,
        resolvedImports,
        parsed.program
      ) match {
      case Ior.Left(_)         =>
        Nil
      case Ior.Right(program0) =>
        (
          PackageCustoms.lintDiagnosticsFromSource(
            pack.name,
            roots,
            sourceImports,
            parsed.exports,
            program0
          ) :::
            (if (includeTodoUsageWarnings)
               PackageCustoms.todoUsageLintFromSource(pack.name, program0)
             else Nil) :::
            replayUnusedLets(parsed.name, program0.lets)
        ).filter(PackageError.isPostponable)
      case Ior.Both(_, program0) =>
        (
          PackageCustoms.lintDiagnosticsFromSource(
            pack.name,
            roots,
            sourceImports,
            parsed.exports,
            program0
          ) :::
            (if (includeTodoUsageWarnings)
               PackageCustoms.todoUsageLintFromSource(pack.name, program0)
             else Nil) :::
            replayUnusedLets(parsed.name, program0.lets)
        ).filter(PackageError.isPostponable)
    }
  }

  private def replayLintDiagnostics[IO[_], Path](
      platformIO: PlatformIO[IO, Path],
      sourceFiles: NonEmptyList[PackageResolver.SourceFile[IO, Path]],
      packs: PackageMap.Compiled,
      allowedPackages: Set[PackageName],
      includeTodoUsageWarnings: Boolean,
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
                replaySourceLintDiagnostics(
                  pack,
                  parsed,
                  includeTodoUsageWarnings
                ) :::
                  replayTypedLintDiagnostics(pack)
              )
        }
      }
      .map(_.flatten)
  }

  private def phasesForLintMode(
      lintMode: LintMode
  ): InferPhases = {
    val base = InferPhases.default
    def lintModeCacheKey(lm: LintMode): String =
      lm match {
        case LintMode.Strict          => "strict"
        case LintMode.Warn | LintMode.Lax => "warn_or_lax"
      }

    // Strict runs must not reuse artifacts that only existed because warn/lax
    // allowed postponable diagnostics through, but warn and lax can share the
    // same compiled artifact cache because they accept the same code.
    new InferPhases {
      val id: String =
        s"${base.id}:lint:${lintModeCacheKey(lintMode)}"

      def dependencyInterface(pack: Package.Typed[Any]): Package.Interface =
        base.dependencyInterface(pack)

      def finishPackage(
          pack: Package.Inferred,
          depIfaces: scala.collection.immutable.SortedMap[
            dev.bosatsu.PackageName,
            Package.Interface
          ],
          compileOptions: CompileOptions
      ): Package.Inferred =
        base.finishPackage(pack, depIfaces, compileOptions)
    }
  }

  private def dedupeDiagnostics(
      sourceMap: PackageMap.SourceMap,
      diagnostics: List[PackageError]
  ): List[PackageError] = {
    val seen = scala.collection.mutable.HashSet.empty[(String, String)]
    diagnostics.filter { err =>
      val key = (err.getClass.getName, err.message(sourceMap, Colorize.None))
      seen.add(key)
    }
  }

  private def diagnosticDoc(
      sourceMap: PackageMap.SourceMap,
      diagnostics: NonEmptyList[PackageError],
      color: Colorize,
      kind: DiagnosticKind
  ): Doc =
    kind match {
      case DiagnosticKind.Error =>
        val rendered = dedupeDiagnostics(sourceMap, diagnostics.toList)
          .map { err =>
            val message = err.message(sourceMap, color)
            classifyError(err, Doc.text(message))
          }

        rendered match {
          case one :: Nil => one.body
          case many       => renderMultiErrorMessage(many)
        }
      case DiagnosticKind.Warning =>
        val rendered = dedupeDiagnostics(sourceMap, diagnostics.toList)
          .map { err =>
            val message = err.message(sourceMap, color)
            classifyWarning(err, Doc.text(message))
          }

        renderMultiWarningMessage(rendered)
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
      lintMode: LintMode,
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
      effectiveSources =
        PackageMap.effectivePredefSources(
          sources,
          ifs,
          "predef",
          compileOptions.mode
        )
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
        phasesForLintMode(lintMode)
      )
      _ <- cache.statsSnapshot.traverse_(platformIO.println)
      sourceMap = PackageMap.buildSourceMapFromSources(sources)
      pathToName = sourceFiles.map(source => (source.path, source.packageName))
      sourcePackageNames = pathToName.iterator.map(_._2).toSet
      // Only the built-in type-check predef should trigger the warn-only todo
      // lint. An explicit `Bosatsu/Predef` interface can define its own `todo`.
      includeTodoUsageWarnings =
        lintMode == LintMode.Warn &&
          compileOptions.mode == CompileOptions.Mode.TypeCheckOnly &&
          effectiveSources.usesInternalPredefSource
      (compileDiagnostics, compiled) = checked match {
        case Ior.Left(errs)      => (errs.toList, None)
        case Ior.Right(packs)    => (Nil, Some(packs))
        case Ior.Both(errs, pm)  => (errs.toList, Some(pm))
      }
      partitionedDiagnostics = partitionDiagnostics(compileDiagnostics)
      hardDiagnostics0 = partitionedDiagnostics.errors
      compileLintDiagnostics = partitionedDiagnostics.postponable
      replayedLintDiagnostics <-
        compiled match {
          case Some(_) if !replaySuccessfulLint =>
            moduleIOMonad.pure(Nil)
          case Some(packs) =>
            replayLintDiagnostics(
              platformIO,
              sourceFiles,
              packs,
              sourcePackageNames,
              includeTodoUsageWarnings,
              errColor
            )
          case _ =>
            moduleIOMonad.pure(Nil)
        }
      lintDiagnostics =
        dedupeDiagnostics(
          sourceMap,
          replayedLintDiagnostics ::: compileLintDiagnostics
        )
      hardDiagnostics = dedupeDiagnostics(sourceMap, hardDiagnostics0)
      orderedDiagnostics =
        if (hardDiagnostics.nonEmpty)
          dedupeDiagnostics(sourceMap, compileDiagnostics)
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
  ): IO[(PackageMap.Compiled, List[(Path, PackageName)])] = {
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
  ): IO[(PackageMap.Compiled, NonEmptyList[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    for {
      diagnostics <- typeCheckDiagnostics(
        platformIO,
        inputs,
        ifs,
        errColor,
        packRes,
        compileOptions,
        LintMode.Strict,
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
  ): IO[(PackageMap.Compiled, NonEmptyList[(Path, PackageName)])] = {
    import platformIO.moduleIOMonad

    def compiledOrError(
        diagnostics: TypeCheckDiagnostics[Path]
    ): IO[(PackageMap.Compiled, NonEmptyList[(Path, PackageName)])] =
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
    ): IO[(PackageMap.Compiled, NonEmptyList[(Path, PackageName)])] =
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
        lintMode,
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
