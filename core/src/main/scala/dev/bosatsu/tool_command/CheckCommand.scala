package dev.bosatsu.tool_command

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.tool.{
  CommandSupport,
  CommonOpts,
  CompilerApi,
  Output,
  PackageResolver,
  PathGen
}
import dev.bosatsu.{CompileOptions, PackageMap, PackageName, Par, PlatformIO}

object CheckCommand {
  private def compile[F[_], Path](
      platformIO: PlatformIO[F, Path],
      srcs: PathGen[F, Path],
      ifaces: PathGen[F, Path],
      packageResolver: PackageResolver[F, Path],
      publicDependencies: List[Path],
      privateDependencies: List[Path],
      compileCacheDirOpt: Option[Path],
      errColor: LocationMap.Colorize
  )(implicit
      ec: Par.EC
  ): F[PackageMap.Compiled] = {
    import platformIO.moduleIOMonad

    def inNel: F[NonEmptyList[Path]] =
      srcs.read.flatMap { ins =>
        NonEmptyList.fromList(ins) match {
          case Some(nel) => moduleIOMonad.pure(nel)
          case None      =>
            CommandSupport.noInputs(platformIO, "check")
        }
      }

    for {
      dependencies <- CommandSupport.readDepArtifacts(
        platformIO,
        publicDependencies,
        privateDependencies
      )
      ifacePaths <- ifaces.read
      inputs <- inNel
      packs <- if (ifacePaths.isEmpty && dependencies._1.isEmpty && dependencies._2.isEmpty)
        CompilerApi.typeCheck(platformIO, inputs, Nil,
          errColor, packageResolver, CompileOptions.NoOptimize, compileCacheDirOpt).map(_._1)
      else {
        val libraries = (dependencies._1 ::: dependencies._2).map { case (path, lib) =>
          path -> proto.Interfaces(lib.arg.exportedIfaces)
        }
        for {
          sources <- CompilerApi.loadSources(platformIO, inputs, packageResolver, errColor)
          resPacks <- CompilerApi.compileWithSignatures(
            platformIO, sources, ifacePaths.map(_ -> None), libraries,
            useExplicitPredef = true, errorUnusedInterfaces = false, errColor, CompileOptions.NoOptimize, compileCacheDirOpt
          )
        } yield PackageMap.fromIterable(resPacks)
      }
    } yield packs
  }

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    val checkOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.interfacePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.compileCacheDirOpt,
      commonOpts.outputPathOpt.orNone,
      commonOpts.interfaceOutputPathOpt.orNone,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          ifaces,
          packageResolver,
          publicDependencies,
          privateDependencies,
          compileCacheDirOpt,
          output,
          interfaceOutput,
          errColor
      ) =>
        platformIO.withEC {
          compile(
            platformIO,
            srcs,
            ifaces,
            packageResolver,
            publicDependencies,
            privateDependencies,
            compileCacheDirOpt,
            errColor
          ).map { packs =>
            val packList =
              packs.toMap.iterator
                .map { case (_, p) => p }
                // TODO currently we recompile predef in every run, so every interface includes (https://github.com/johnynek/bosatsu/issues/414)
                // predef, we filter that out
                .filter(_.name != PackageName.PredefName)
                .toList
                .sortBy(_.name)

            (Output.CompileOut(packList, interfaceOutput, output): Output[Path])
          }
        }
    }

    Opts.subcommand("check", "type check a set of packages")(checkOpt)
  }
}
