package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.tool.{
  CliException,
  CommandSupport,
  CommonOpts,
  CompilerApi,
  Output,
  PackageResolver,
  PathGen,
  ShowEdn,
  ShowSelection
}
import dev.bosatsu.{
  CompileOptions,
  Package,
  PackageMap,
  PackageName,
  Par,
  PlatformIO
}

object ShowCommand {
  private def loadAndCompile[F[_], Path](
      platformIO: PlatformIO[F, Path],
      srcs: PathGen[F, Path],
      ifaces: PathGen[F, Path],
      includes: PathGen[F, Path],
      packageResolver: PackageResolver[F, Path],
      publicDependencies: List[Path],
      privateDependencies: List[Path],
      errColor: LocationMap.Colorize
  )(implicit
      ec: Par.EC
  ): F[(List[Package.Interface], List[Package.Typed[Any]])] = {
    import platformIO.moduleIOMonad

    val readIfaces =
      for {
        ifacePaths <- ifaces.read
        parsedIfaces <- platformIO.readInterfaces(ifacePaths)
      } yield parsedIfaces

    val readPacks =
      for {
        includePaths <- includes.read
        includePacks <- platformIO.readPackages(includePaths)
      } yield includePacks

    (
      srcs.read,
      readIfaces,
      readPacks,
      CommandSupport.readDepLibraries(
        platformIO,
        publicDependencies,
        privateDependencies
      )
    ).flatMapN {
      case (Nil, ifacesList, packs, depLibs) =>
        val depIfs = CommandSupport.dependencyInterfaces(depLibs)
        val depPackages = CommandSupport.dependencyPackages(depLibs)
        val allPacks = packs ::: depPackages
        CommandSupport
          .ensureDistinctPackages(platformIO, allPacks, "show dependencies")
          .as((ifacesList ::: depIfs, allPacks))

      case (h :: t, ifacesList, packs, depLibs) =>
        val depIfs = CommandSupport.dependencyInterfaces(depLibs)
        val depPackages = CommandSupport.dependencyPackages(depLibs)
        val existingPacks = packs ::: depPackages
        val packIfs = existingPacks.map(Package.interfaceOf(_))

        for {
          packPath <- CompilerApi.typeCheck(
            platformIO,
            NonEmptyList(h, t),
            ifacesList ::: depIfs ::: packIfs,
            errColor,
            packageResolver,
            CompileOptions.Default
          )
          allPacks =
            (PackageMap.fromIterable(existingPacks) ++ packPath._1.toMap.map(
              _._2
            )).toMap.values.toList
          _ <-
            CommandSupport.ensureDistinctPackages(
              platformIO,
              allPacks,
              "show dependencies"
            )
        } yield (ifacesList ::: depIfs, allPacks)
    }
  }

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize
    import ShowSelection.{typeArgument, valueArgument}

    val showOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.interfacePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      Opts
        .options[PackageName](
          "package",
          help = "package names to show in full"
        )
        .orEmpty,
      Opts
        .options[ShowSelection.TypeSelector](
          "type",
          help = "type names to show (package::Type)"
        )
        .orEmpty,
      Opts
        .options[ShowSelection.ValueSelector](
          "value",
          help = "value names to show (package::value)"
        )
        .orEmpty,
      Opts
        .flag(
          "json",
          help = "emit JSON instead of EDN for easier machine parsing"
        )
        .orFalse,
      commonOpts.outputPathOpt.orNone,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          ifaces,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          packages,
          types,
          values,
          jsonOut,
          output,
          errColor
      ) =>
        val request = ShowSelection.Request(packages, types, values)
        platformIO.withEC {
          for {
            (interfaces, packs0) <- loadAndCompile(
              platformIO,
              srcs,
              ifaces,
              includes,
              packageResolver,
              publicDependencies,
              privateDependencies,
              errColor
            )
            packs1 = packs0.filterNot(_.name == PackageName.PredefName)
            packs <- moduleIOMonad.fromEither(
              ShowSelection
                .selectPackages(packs1, request)
                .leftMap(CliException.Basic(_))
            )
            selectedInterfaces = ShowSelection.selectInterfaces(interfaces, request)
          } yield (
            if (jsonOut)
              Output.JsonOutput(
                ShowEdn.showJson(packs, selectedInterfaces),
                output
              )
            else
              Output.ShowOutput(
                packs,
                selectedInterfaces,
                output
              ): Output[Path]
          )
        }
    }

    Opts.subcommand(
      "show",
      "show fully type-checked packages (EDN by default; JSON with --json)"
    )(showOpt)
  }
}
