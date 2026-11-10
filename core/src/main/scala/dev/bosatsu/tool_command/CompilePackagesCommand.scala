package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{CompileOptions, LocationMap, PackageSignatures, PlatformIO}
import dev.bosatsu.tool.{CliException, CompilerApi, Output, PackageResolver}

object CompilePackagesCommand {
  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg, showPath}
    Opts.subcommand("compile-packages", "compile a group of explicit sources for a build") {
      (
        Opts.options[Path]("src", "Bosatsu source file (repeatable)"),
        Opts.options[Path]("direct-interface", "dependency signature (repeatable)").orEmpty,
        Opts.option[Path]("packages-out", "compiled implementation bundle"),
        Opts.option[Path]("sig-out", "public package signature")
      ).mapN { (srcs, deps, packagesOut, sigOut) =>
        platform.withEC {
          CompilerApi.typeCheckWithInterfaces(
            platform,
            srcs.distinctBy(platform.pathToString).sortBy(platform.pathToString),
            sources => {
              val requirements = sources.toList.map { src =>
                (src.packageName, (src.imports.map(_.pack) ::: src.exposes.flatten).distinct)
              }
              if (requirements.exists(_._1.isPredef))
                moduleIOMonad.raiseError(
                  CliException.Basic("Bosatsu/Predef is implicit in compile-packages")
                )
              else
                deps.distinct.sortBy(platform.pathToString).traverse { path =>
                  platform.readInterfaceBundle(path).adaptError { case e =>
                    CliException.Basic(show"cannot read signature $path: ${e.getMessage}")
                  }.map(path -> _)
                }.flatMap { bundles =>
                  platform.canPromiseF
                    .compute(PackageSignatures.decodeRequired(bundles, requirements))
                    .flatMap(moduleIOMonad.fromTry)
                    .map(PackageSignatures.predef :: _)
                }
            },
            LocationMap.Colorize.None,
            PackageResolver.ExplicitOnly(),
            CompileOptions.Default,
            None
          ).map { case (compiled, sources) =>
            val members = sources.toList.map(_._2).sorted.map(compiled.toMap)
            Output.PackageGroup(members, packagesOut, sigOut): Output[Path]
          }
        }
      }
    }
  }
}
