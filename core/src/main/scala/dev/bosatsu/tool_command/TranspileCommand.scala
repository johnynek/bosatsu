package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.codegen
import dev.bosatsu.tool.{CommonOpts, Output}
import dev.bosatsu.PlatformIO

object TranspileCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    val transOpt =
      codegen.python.PythonTranspiler
        .opts(platformIO)
        .orElse(codegen.clang.ClangTranspiler.opts(platformIO))

    val transpileOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.compileCacheDirOpt,
      Colorize.optsConsoleDefault,
      transOpt
    ).mapN {
      (
          srcs,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          compileCacheDirOpt,
          errColor,
          generator
      ) =>
        platformIO.withEC {
          for {
            (packs, _) <- RuntimeCommandSupport.packMap(
              platformIO,
              srcs,
              includes,
              packageResolver,
              publicDependencies,
              privateDependencies,
              compileCacheDirOpt,
              "transpile",
              Nil,
              errColor
            )
            data <- generator.renderAll(packs)
          } yield (Output.TranspileOut(data): Output[Path])
        }
    }

    Opts.subcommand("transpile", "transpile bosatsu into another language")(
      transpileOpt
    )
  }
}
