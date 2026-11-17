package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.codegen.CompilationSource
import dev.bosatsu.tool.{
  CliException,
  CommandSupport,
  CommonOpts,
  CompilerApi,
  Output,
  PackageResolver,
  PathGen,
  ShowEdn,
  ShowSupport,
  ShowSelection
}
import dev.bosatsu.{
  CompileOptions,
  MatchlessFromTypedExpr,
  Package,
  PackageMap,
  PackageName,
  Par,
  PlatformIO,
  TypeValidator
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
      compileCacheDirOpt: Option[Path],
      compileOptions: CompileOptions,
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
            compileOptions,
            compileCacheDirOpt
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
    import ShowSupport.{
      matchlessPassArgument,
      showIrArgument,
      typedPassArgument
    }

    val irOpt =
      Opts
        .option[Output.ShowIr](
          "ir",
          help = "which IR to render: typedexpr or matchless"
        )
        .withDefault(Output.ShowIr.TypedExpr)

    val disableTypedPassOpt =
      Opts
        .options[CompileOptions.TypedPass](
          "disable-typed-pass",
          help =
            "disable a typed pass: loop-recur-lowering, normalize, discard-unused"
        )
        .orEmpty
        .map(_.toSet)

    val disableMatchlessPassOpt =
      Opts
        .options[dev.bosatsu.Matchless.Pass](
          "disable-matchless-pass",
          help =
            "disable a Matchless pass: hoist-invariant-loop-lets, reuse-constructors, global-inlining"
        )
        .orEmpty
        .map(_.toSet)

    def defsInOriginalOrder[A](
        pack: Package.Typed[Any],
        lets: List[(dev.bosatsu.Identifier.Bindable, A)]
    ): List[(dev.bosatsu.Identifier.Bindable, A)] = {
      val byName = lets.toMap
      pack.lets.flatMap { case (name, _, _) =>
        byName.get(name).map(name -> _)
      }
    }

    def matchlessShowValue(
        request: ShowSupport.Request,
        allPacks: List[Package.Typed[Any]],
        selectedPacks: List[Package.Typed[Any]]
    )(implicit ec: Par.EC): Output.ShowValue.Matchless = {
      val localPassOptions = request.matchlessPassOptions.localPassOptions

      if (request.matchlessPassOptions.enableGlobalInlining) {
        val namespace =
          CompilationSource
            .namespace(PackageMap.fromIterable(allPacks))
            .treeShake(ShowSupport.matchlessRoots(selectedPacks))
        val compiled =
          namespace.compiledWithMatchlessOptions(
            localPassOptions,
            enableGlobalInlining = true
          )

        ShowSupport.matchlessShowValue(
          selectedPacks,
          request,
          pack => {
            val scope = namespace.depFor(namespace.rootKey, pack.name)
            val lets =
              compiled
                .get(scope)
                .flatMap(_.get(pack.name))
                .getOrElse(Nil)
            defsInOriginalOrder(pack, lets)
          }
        )
      } else {
        val compiled =
          MatchlessFromTypedExpr.compile(
            (),
            PackageMap.fromIterable(selectedPacks),
            localPassOptions
          )

        ShowSupport.matchlessShowValue(
          selectedPacks,
          request,
          pack => defsInOriginalOrder(pack, compiled.getOrElse(pack.name, Nil))
        )
      }
    }

    val showOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.interfacePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.compileCacheDirOpt,
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
          "externals",
          help = "show only package names and external value declarations"
        )
        .orFalse,
      Opts
        .flag(
          "json",
          help = "emit JSON instead of EDN for easier machine parsing"
        )
        .orFalse,
      Opts
        .flag(
          "validate-typedexpr",
          help =
            "run the TypedExpr TypeValidator on the shown packages before rendering; fails on invalid TypedExpr IR"
        )
        .orFalse,
      irOpt,
      disableTypedPassOpt,
      disableMatchlessPassOpt,
      Opts
        .flag(
          "no-opt",
          help =
            "disable all typed passes to inspect the unoptimized typed pipeline"
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
          compileCacheDirOpt,
          packages,
          types,
          values,
          externalsOnly,
          jsonOut,
          validateTypedExpr,
          ir,
          disabledTypedPasses,
          disabledMatchlessPasses,
          noOpt,
          output,
          errColor
      ) =>
        val selection =
          ShowSelection.Request(packages, types, values, externalsOnly)
        val showRequest =
          ShowSupport
            .request(
              selection,
              ir,
              noOpt,
              disabledTypedPasses,
              disabledMatchlessPasses,
              packageNamesOnly = false,
              validateTypedExpr = validateTypedExpr
            )
            .toEither
            .leftMap(errs => CliException.Basic(errs.toList.mkString("\n")))
        platformIO.withEC {
          for {
            request <- moduleIOMonad.fromEither(showRequest)
            (interfaces, packs0) <- loadAndCompile(
              platformIO,
              srcs,
              ifaces,
              includes,
              packageResolver,
              publicDependencies,
              privateDependencies,
              compileCacheDirOpt,
              request.compileOptions,
              errColor
            )
            packs1 = packs0.filterNot(_.name == PackageName.PredefName)
            packs <- moduleIOMonad.fromEither(
              ShowSelection
                .selectPackages(packs1, request.selection)
                .leftMap(CliException.Basic(_))
            )
            _ <-
              if (request.validateTypedExpr)
                moduleIOMonad.fromEither(
                  TypeValidator
                    .validationFailureMessage(
                      "show typedexpr",
                      TypeValidator.validatePackagesInEnv(
                        packs,
                        packs0,
                        "show typedexpr"
                      )
                    )
                    .toLeft(())
                    .leftMap(CliException.Basic(_))
                )
              else moduleIOMonad.unit
            selectedInterfaces =
              ShowSelection.selectInterfaces(interfaces, request.selection)
            showValue =
              request.ir match {
                case Output.ShowIr.TypedExpr =>
                  ShowSupport.typedShowValue(packs, selectedInterfaces, request)
                case Output.ShowIr.Matchless =>
                  matchlessShowValue(request, packs0, packs)
              }
          } yield (
            if (jsonOut)
              Output.JsonOutput(
                ShowEdn.showJson(showValue),
                output
              )
            else
              Output.ShowOutput(
                showValue,
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
