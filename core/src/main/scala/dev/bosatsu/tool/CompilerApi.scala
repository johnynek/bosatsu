package dev.bosatsu.tool

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import dev.bosatsu.cache.{CompileCache, InferCache, InferPhases}
import dev.bosatsu.{
  CompileOptions,
  Package,
  PackageError,
  PackageMap,
  PackageName,
  Par,
  PlatformIO
}
import dev.bosatsu.LocationMap.Colorize
import dev.bosatsu.rankn.Infer
import org.typelevel.paiges.Doc

import dev.bosatsu.IorMethods.IorExtension

import cats.syntax.all._

object CompilerApi {
  private enum ErrorCategory(
      val singularLabel: String,
      val pluralLabel: String,
      val order: Int
  ) {
    case UnusedValue extends ErrorCategory("unused value", "unused values", 0)
    case TypeError extends ErrorCategory("type error", "type errors", 1)
    case RecursionError extends ErrorCategory(
          "recursion error",
          "recursion errors",
          2
        )
    case TotalityError extends ErrorCategory(
          "totality error",
          "totality errors",
          3
        )
    case SourceConversionError extends ErrorCategory(
          "source conversion error",
          "source conversion errors",
          4
        )
    case ImportError extends ErrorCategory("import error", "import errors", 5)
    case PackageError extends ErrorCategory(
          "package error",
          "package errors",
          6
        )
  }

  private final case class RenderedDiagnostic(
      category: ErrorCategory,
      title: String,
      summaryCount: Int,
      body: Doc
  )

  private def pluralizedLabel(
      count: Int,
      singular: String,
      plural: String
  ): String =
    if (count == 1) s"1 $singular" else s"$count $plural"

  private def formatDiagnosticTitle(
      category: ErrorCategory,
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
        RenderedDiagnostic(ErrorCategory.UnusedValue, title, count, body)
      case e: PackageError.UnusedLets =>
        val count = e.unusedLets.length
        val title =
          if (count == 1) s"unused value '${e.unusedLets.head._1.sourceCodeRepr}'"
          else s"$count unused values"
        RenderedDiagnostic(ErrorCategory.UnusedValue, title, count, body)
      case e: PackageError.TypeErrorIn =>
        val count = typeErrorCount(e)
        RenderedDiagnostic(
          ErrorCategory.TypeError,
          formatDiagnosticTitle(ErrorCategory.TypeError, count),
          count,
          body
        )
      case _: PackageError.KindInferenceError =>
        RenderedDiagnostic(
          ErrorCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.ShadowedBindingTypeError =>
        RenderedDiagnostic(
          ErrorCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.PrivateTypeEscape[?] =>
        RenderedDiagnostic(
          ErrorCategory.TypeError,
          "type error",
          1,
          body
        )
      case _: PackageError.RecursionError =>
        RenderedDiagnostic(
          ErrorCategory.RecursionError,
          "recursion error",
          1,
          body
        )
      case _: PackageError.TotalityCheckError[?] =>
        RenderedDiagnostic(
          ErrorCategory.TotalityError,
          "totality error",
          1,
          body
        )
      case e: PackageError.SourceConverterErrorsIn =>
        val count = e.errs.length
        RenderedDiagnostic(
          ErrorCategory.SourceConversionError,
          formatDiagnosticTitle(ErrorCategory.SourceConversionError, count),
          count,
          body
        )
      case e: PackageError.UnusedImport =>
        val count = e.badImports.length
        RenderedDiagnostic(
          ErrorCategory.ImportError,
          formatDiagnosticTitle(ErrorCategory.ImportError, count),
          count,
          body
        )
      case e: PackageError.DuplicatedImport =>
        val count = e.duplicates.length
        RenderedDiagnostic(
          ErrorCategory.ImportError,
          formatDiagnosticTitle(ErrorCategory.ImportError, count),
          count,
          body
        )
      case _: PackageError.UnknownImportPackage[?, ?, ?] =>
        RenderedDiagnostic(
          ErrorCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownImportName[?, ?] =>
        RenderedDiagnostic(
          ErrorCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownImportFromInterface[?, ?] =>
        RenderedDiagnostic(
          ErrorCategory.ImportError,
          "import error",
          1,
          body
        )
      case _: PackageError.UnknownExport[?] =>
        RenderedDiagnostic(
          ErrorCategory.PackageError,
          "package error",
          1,
          body
        )
      case _: PackageError.CircularDependency[?, ?, ?] =>
        RenderedDiagnostic(
          ErrorCategory.PackageError,
          "package error",
          1,
          body
        )
      case e: PackageError.VarianceInferenceFailure =>
        val count = e.failed.length
        RenderedDiagnostic(
          ErrorCategory.PackageError,
          formatDiagnosticTitle(ErrorCategory.PackageError, count),
          count,
          body
        )
      case e: PackageError.DuplicatedPackageError =>
        val count = e.dups.length
        RenderedDiagnostic(
          ErrorCategory.PackageError,
          formatDiagnosticTitle(ErrorCategory.PackageError, count),
          count,
          body
        )
    }

  private def renderDiagnostic(
      idx: Int,
      diagnostic: RenderedDiagnostic
  ): Doc =
    Doc.text(s"$idx. ${diagnostic.title}") +
      (Doc.hardLine + diagnostic.body).nested(2)

  private def renderDiagnosticSummary(
      diagnostics: List[RenderedDiagnostic]
  ): Doc = {
    val grouped = diagnostics
      .groupMapReduce(_.category)(_.summaryCount)(_ + _)
      .toList
      .sortBy { case (category, _) => category.order }
    val totalErrors = grouped.iterator.map(_._2).sum
    val parts = grouped.map { case (category, count) =>
      Doc.text(pluralizedLabel(count, category.singularLabel, category.pluralLabel))
    }
    val errorWord = if (totalErrors == 1) "error" else "errors"
    Doc.text(s"$totalErrors $errorWord: ") +
      Doc.intercalate(Doc.text(", "), parts)
  }

  private def renderMultiErrorMessage(
      diagnostics: List[RenderedDiagnostic]
  ): Doc = {
    val divider = "-" * 72
    val dividerDoc = Doc.text(divider)
    val entrySeparator =
      Doc.hardLine + Doc.hardLine + dividerDoc + Doc.hardLine + Doc.hardLine
    val entries = diagnostics.zipWithIndex.map { case (diag, idx) =>
      renderDiagnostic(idx + 1, diag)
    }
    val entriesDoc = Doc.intercalate(entrySeparator, entries)
    val summaryDoc = renderDiagnosticSummary(diagnostics)
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
      // TODO, we could use applicative, to report both duplicate packages and the other
      // errors
      res <-
        checked.strictToValidated match {
          case Validated.Valid(p) =>
            val pathToName: NonEmptyList[(Path, PackageName)] =
              sourceFiles.map(source => (source.path, source.packageName))
            moduleIOMonad.pure((p, pathToName))
          case Validated.Invalid(errs) =>
            val sourceMap = PackageMap.buildSourceMapFromSources(sources)
            moduleIOMonad.raiseError(
              PackageErrors(sourceMap, errs, errColor)
            )
        }
    } yield res
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
    val diagnostics =
      errors.toList.distinct
        .map { err =>
          val message = err.message(sourceMap, color)
          classifyError(err, Doc.text(message))
        }

    val errDoc: Doc =
      diagnostics match {
        case one :: Nil => one.body
        case many       =>
          renderMultiErrorMessage(many)
      }
    val messageString = errDoc.render(80)

    CliException(messageString, errDoc)
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
