package dev.bosatsu.tool_command

import cats.data.Validated
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{CompileOptions, LocationMap, PackageSignatures, Par, PlatformIO}
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.tool.{
  CliException,
  CompilerApi,
  InterfaceLabels,
  LibraryInspection,
  Output,
  PackageBindings,
  PackageResolver,
  ToolchainManifest
}

object CompilePackagesCommand {
  private final case class ScopedInputs[Path](
      scopeMap: Path,
      dependencyContext: Path,
      toolchainManifest: Path
  )

  private def scopedCompilation[F[_], Path](
      platform: PlatformIO[F, Path],
      sources: cats.data.NonEmptyList[PackageResolver.SourceFile[F, Path]],
      directInterfaces: List[Path],
      labels: Map[Path, String],
      inputs: ScopedInputs[Path],
      labelMap: Option[Path],
      sourcesOnDisk: List[Path],
      packagesOut: Path,
      sigOut: Path,
      strict: Boolean,
      color: LocationMap.Colorize
  )(using Par.EC): F[Output[Path]] = {
    import platform.{moduleIOMonad, pathOrdering, showPath}
    given CanPromise[F] = platform.canPromiseF

    for {
      rawScopeMap <- PackageBindings.read(platform, inputs.scopeMap, directInterfaces)
      scopeMap = rawScopeMap.copy(imports = rawScopeMap.imports.map { case (identity, imported) =>
        val label = labels.get(imported.signature.path).orElse(imported.signature.label)
        identity -> imported.copy(signature = imported.signature.copy(label = label))
      })
      dependencyContext <- LibraryInspection.readDependencyContext(platform, inputs.dependencyContext)
      toolchain <- ToolchainManifest.read(platform, inputs.toolchainManifest)
      inputPaths = sourcesOnDisk ::: directInterfaces ::: labelMap.toList ::: List(
        inputs.scopeMap, inputs.dependencyContext, inputs.toolchainManifest
      ) ::: dependencyContext.libraries.map(_.path) :::
        scopeMap.imports.valuesIterator.map(_.publicIndexPath).toList
      _ <- OutputPathChecks.checkOutputFiles(platform, inputPaths, List(packagesOut, sigOut))
      imported <- LibraryInspection.loadImportedInputs(
        platform, scopeMap, dependencyContext, toolchain
      )
      importedPaths = scopeMap.imports.valuesIterator.map(_.signature.path).toList
      directPaths = directInterfaces.filterNot(path =>
        importedPaths.exists(imported => pathOrdering.compare(path, imported) == 0)
      )
      directBundles <- directPaths.traverse { path =>
        platform.readInterfaceBundle(path).adaptError { case error =>
          CliException.Basic(showPath.show(path) + s": cannot read signature: ${error.getMessage}")
        }.map { bundle =>
          PackageSignatures.Input(path, labels.get(path)) -> bundle
        }
      }
      plan <- platform.canPromiseF.compute(
        CompilerApi.prepareScopedCompilation(
          sources, scopeMap, imported, directBundles, builtinPredef = true
        )
      ).flatMap(moduleIOMonad.fromTry)
      group <- CompilerApi.compileScopedWithSignatures(
        platform,
        sources,
        plan,
        strict,
        color,
        CompileOptions.Default,
        None
      )
    } yield Output.PackageGroup(group, packagesOut, sigOut): Output[Path]
  }

  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}
    Opts.subcommand("compile-packages", "compile a group of explicit sources for a build") {
      val sources = Opts.options[Path]("src", "Bosatsu source file (repeatable)")
      val directInterfaces = Opts.options[Path]("direct-interface", "dependency signature (repeatable)").orEmpty
      val labelMap = Opts.option[Path]("interface-labels", "JSON map of declared signature files to labels").orNone
      val strict = Opts.flag("error-unused-interfaces", "reject any unused direct interface file").orFalse
      val packagesOut = Opts.option[Path]("packages-out", "compiled implementation bundle")
      val sigOut = Opts.option[Path]("sig-out", "public package signature")
      val scopeMap = Opts.option[Path]("scope-map", "checked member-relative imported bindings").orNone
      val dependencyContext =
        Opts.option[Path]("dependency-context", "version-1 declared library artifact context").orNone
      val toolchainManifest =
        Opts.option[Path]("toolchain-manifest", "selected toolchain and core manifest").orNone
      val scopedInputs: Opts[Option[ScopedInputs[Path]]] =
        (scopeMap, dependencyContext, toolchainManifest).tupled.mapValidated {
          case (None, None, None) => Validated.valid(None)
          case (Some(scopes), Some(context), Some(manifest)) =>
            Validated.valid(Some(ScopedInputs(scopes, context, manifest)))
          case _ => Validated.invalidNel(
            "scoped compilation requires --scope-map, --dependency-context, and --toolchain-manifest together"
          )
        }

      (sources, directInterfaces, labelMap, strict, packagesOut, sigOut, scopedInputs)
        .mapN { (srcs, deps, labelsPath, errorUnused, packagesPath, signaturePath, scoped) =>
          platform.withEC {
            val color = LocationMap.Colorize.None
            val sourcePaths = srcs.distinctBy(platform.pathToString).sortBy(platform.pathToString)
            val sourcePathList = sourcePaths.toList
            val paths = deps.distinct.sortBy(platform.pathToString)
            for {
              loaded <- CompilerApi.loadSources(
                platform, sourcePaths, PackageResolver.ExplicitOnly(), color
              )
              _ <- moduleIOMonad.raiseWhen(loaded.exists(_.packageName.isPredef))(
                CliException.Basic("Bosatsu/Predef is implicit in compile-packages")
              )
              labels <- labelsPath.fold(moduleIOMonad.pure(Map.empty[Path, String]))(
                InterfaceLabels.read(platform, _, paths)
              )
              output <- scoped match {
                case Some(inputs) =>
                  scopedCompilation(
                    platform,
                    loaded,
                    paths,
                    labels,
                    inputs,
                    labelsPath,
                    sourcePathList,
                    packagesPath,
                    signaturePath,
                    errorUnused,
                    color
                  )
                case None =>
                  for {
                    _ <- OutputPathChecks.checkOutputFiles(
                      platform,
                      sourcePathList ::: paths ::: labelsPath.toList,
                      List(packagesPath, signaturePath)
                    )
                    group <- CompilerApi.compileWithSignatures(
                      platform,
                      loaded,
                      paths.map(path => path -> labels.get(path)),
                      Nil,
                      useExplicitPredef = false,
                      errorUnusedInterfaces = errorUnused,
                      color,
                      CompileOptions.Default,
                      None
                    )
                  } yield Output.PackageGroup(group, packagesPath, signaturePath): Output[Path]
              }
            } yield output
          }
        }
    }
  }
}
