package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.tool.{CommandSupport, CommonOpts, CompilerApi, Output, PackageResolver, PathGen}
import dev.bosatsu.{PackageMap, PackageName, Par, PlatformIO}

object CheckCommand {
  private def compile[F[_], Path](
      platformIO: PlatformIO[F, Path],
      srcs: PathGen[F, Path],
      ifaces: PathGen[F, Path],
      packageResolver: PackageResolver[F, Path],
      publicDependencies: List[Path],
      privateDependencies: List[Path],
      errColor: LocationMap.Colorize
  )(implicit
      ec: Par.EC
  ): F[PackageMap.Inferred] = {
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
      dependencies <- CommandSupport.readDepLibraries(
        platformIO,
        publicDependencies,
        privateDependencies
      )
      ifacePaths <- ifaces.read
      interfaces <- platformIO.readInterfaces(ifacePaths)
      inputs <- inNel
      packPath <- CompilerApi.typeCheck(
        platformIO,
        inputs,
        interfaces ::: CommandSupport.dependencyInterfaces(dependencies),
        errColor,
        packageResolver
      )
    } yield packPath._1
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
      commonOpts.noSearchPackageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
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
