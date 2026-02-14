package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, PackageName, PlatformIO, Predef}
import dev.bosatsu.library.LibraryEvaluation
import dev.bosatsu.tool.{CommonOpts, Output}

object TestCommand {
  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    val testOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.testIdentifiersOpt,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          testPacks,
          errColor
      ) =>
        platformIO.withEC {
          for {
            (packs, nameMap) <- RuntimeCommandSupport.packMap(
              platformIO,
              srcs,
              includes,
              packageResolver,
              publicDependencies,
              privateDependencies,
              "test",
              testPacks,
              errColor
            )
            testPackIdents <-
              testPacks.traverse(_.getMain(nameMap)(platformIO))
          } yield {
            val testPackNames: List[PackageName] = testPackIdents.map(_._1)
            val testIt: Iterator[PackageName] =
              if (testPacks.isEmpty) {
                // if there are no given files or packages to test, assume
                // we test all the files
                nameMap.iterator.map(_._2)
              } else {
                // otherwise we have a specific list packages/files to test
                testPackNames.iterator
              }

            val testPackages: List[PackageName] =
              testIt.toList.sorted.distinct
            val ev =
              LibraryEvaluation.fromPackageMap(packs, Predef.jvmExternals)
            val res0 = testPackages.map(p => (p, ev.evalTest(p)))
            val res =
              if (testPacks.isEmpty) res0.filter { case (_, testRes) =>
                testRes.isDefined
              }
              else res0

            (Output.TestOutput(res, errColor): Output[Path])
          }
        }
    }

    Opts.subcommand("test", "test a set of bosatsu modules")(testOpt)
  }
}
