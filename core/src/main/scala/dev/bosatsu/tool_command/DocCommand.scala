package dev.bosatsu.tool_command

import cats.data.NonEmptyList
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.codegen.Transpiler
import dev.bosatsu.tool.{
  CommandSupport,
  CommonOpts,
  CompilerApi,
  MarkdownDoc,
  Output
}
import dev.bosatsu.{CompileOptions, Package, PackageName, PlatformIO}

object DocCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.{moduleIOMonad, pathArg}
    import LocationMap.Colorize

    val docOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.interfacePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      Transpiler.outDir[Path],
      Opts
        .flag(
          "include_predef",
          help = "include Bosatsu/Predef in generated docs"
        )
        .orFalse,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          ifaces,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          outdir,
          includePredef,
          color
      ) =>
        platformIO.withEC {
          for {
            dependencies <- CommandSupport.readDepLibraries(
              platformIO,
              publicDependencies,
              privateDependencies
            )
            ifacePaths <- ifaces.read
            parsedIfaces <- platformIO.readInterfaces(ifacePaths)
            includePaths <- includes.read
            includePacks <- platformIO.readPackages(includePaths)
            inputSources <- srcs.read
            inputNel <- NonEmptyList.fromList(inputSources) match {
              case Some(nel) => moduleIOMonad.pure(nel)
              case None      => CommandSupport.noInputs(platformIO, "doc")
            }
            checked <- CompilerApi.typeCheck(
              platformIO,
              inputNel,
              parsedIfaces :::
                CommandSupport.dependencyInterfaces(dependencies) :::
                includePacks.map(Package.interfaceOf(_)),
              color,
              packageResolver,
              CompileOptions.Default
            )
            (compiled, sourcePaths) = checked
            compiledPacks = {
              val packs0 = compiled.toMap.values.toList
              if (includePredef) packs0
              else packs0.filterNot(_.name == PackageName.PredefName)
            }
            docs <- MarkdownDoc.generate(
              platformIO,
              compiledPacks,
              sourcePaths.toList,
              outdir,
              color
            )
          } yield (Output.TranspileOut(docs): Output[Path])
        }
    }

    Opts.subcommand(
      "doc",
      "generate markdown docs for exported package APIs from source inputs"
    )(docOpt)
  }
}
