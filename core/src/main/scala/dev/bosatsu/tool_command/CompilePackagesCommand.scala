package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{CompileOptions, LocationMap, PlatformIO}
import dev.bosatsu.tool.{CliException, CompilerApi, Output, PackageResolver}

object CompilePackagesCommand {
  def opts[F[_], Path](platform: PlatformIO[F, Path]): Opts[F[Output[Path]]] = {
    import platform.{moduleIOMonad, pathArg}
    Opts.subcommand("compile-packages", "compile a group of explicit sources for a build") {
      (
        Opts.options[Path]("src", "Bosatsu source file (repeatable)"),
        Opts.options[Path]("direct-interface", "dependency signature (repeatable)").orEmpty,
        Opts.option[Path]("packages-out", "compiled implementation bundle"),
        Opts.option[Path]("sig-out", "public package signature")
      ).mapN { (srcs, deps, packagesOut, sigOut) =>
        platform.withEC {
          val color = LocationMap.Colorize.None
          for {
            sources <- CompilerApi.loadSources(
              platform, srcs.distinctBy(platform.pathToString).sortBy(platform.pathToString),
              PackageResolver.ExplicitOnly(), color
            )
            _ <- moduleIOMonad.raiseWhen(sources.exists(_.packageName.isPredef))(
              CliException.Basic("Bosatsu/Predef is implicit in compile-packages")
            )
            members <- CompilerApi.compileWithSignatures(
              platform, sources, deps.distinct.sortBy(platform.pathToString), Nil,
              useExplicitPredef = false, color, CompileOptions.Default, None
            )
          } yield Output.PackageGroup(members, packagesOut, sigOut): Output[Path]
        }
      }
    }
  }
}
