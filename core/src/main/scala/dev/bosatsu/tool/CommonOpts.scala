package dev.bosatsu.tool

import com.monovore.decline.Opts
import dev.bosatsu.{PackageName, PlatformIO}
import dev.bosatsu.Identifier.Bindable

final class CommonOpts[F[_], P](platformIO: PlatformIO[F, P]) {
  import platformIO.pathArg
  import MainIdentifier.argument

  val outputPathOpt: Opts[P] =
    Opts.option[P]("output", help = "output path")

  val interfaceOutputPathOpt: Opts[P] =
    Opts.option[P]("interface_out", help = "interface output path")

  val sourcePathOpts: Opts[PathGen[F, P]] =
    PathGen.opts("input", help = "input source files", ".bosatsu")(platformIO)

  val interfacePathOpts: Opts[PathGen[F, P]] =
    PathGen.opts("interface", help = "interface files", ".bosatsig")(platformIO)

  val includePathOpts: Opts[PathGen[F, P]] =
    PathGen.opts(
      "include",
      help = "compiled packages to include files",
      ".bosatsu_package"
    )(platformIO)

  val publicDependencyOpts: Opts[List[P]] =
    Opts
      .options[P](
        "pub_dep",
        help = "public dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
      )
      .orEmpty

  val privateDependencyOpts: Opts[List[P]] =
    Opts
      .options[P](
        "priv_dep",
        help = "private dependency library (.bosatsu_lib or .bosatsu_ifacelib)"
      )
      .orEmpty

  val packageResolverOpts: Opts[PackageResolver[F, P]] =
    PackageResolver.opts(platformIO)

  val noSearchPackageResolverOpts: Opts[PackageResolver[F, P]] =
    PackageResolver.noSearchOpts(platformIO)

  val mainIdentifierOpt: Opts[MainIdentifier[P]] =
    MainIdentifier.opts(
      Opts.option[(PackageName, Option[Bindable])](
        "main",
        help =
          "main value to evaluate (package name or full identifier to a value)"
      ),
      Opts.option[P](
        "main_file",
        help = "file containing the main package to evaluate"
      )
    )

  val testIdentifiersOpt: Opts[List[MainIdentifier[P]]] =
    MainIdentifier.list(
      Opts
        .options[PackageName](
          "test_package",
          help = "package for which to run tests"
        )
        .map(_.map((_, None)))
        .orEmpty,
      Opts
        .options[P](
          "test_file",
          help = "file containing the package for which to run tests"
        )
        .orEmpty
    )
}
