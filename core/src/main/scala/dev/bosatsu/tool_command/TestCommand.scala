package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.{LocationMap, PackageName, PlatformIO, Predef}
import dev.bosatsu.library.LibraryEvaluation
import dev.bosatsu.tool.{CliException, CommonOpts, Output}

object TestCommand {
  private def testDiscoveryError(
      err: dev.bosatsu.Package.TestDiscoveryError
  ): String =
    err match {
      case dev.bosatsu.Package.TestDiscoveryError.PlainTestAfterProgTest(
            packageName,
            progTest,
            plainTestsAfter
          ) =>
        val plainTestStr =
          plainTestsAfter.toList.map(_.sourceCodeRepr).mkString(", ")
        s"${packageName.asString}: found top-level Test value(s) after ProgTest ${progTest.sourceCodeRepr}: $plainTestStr"
    }

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
      commonOpts.compileCacheDirOpt,
      commonOpts.testIdentifiersOpt,
      Colorize.optsConsoleDefault,
      Opts
        .flag(
          "quiet",
          help = "only print failure details and final test summary"
        )
        .orFalse
    ).mapN {
      (
          srcs,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          compileCacheDirOpt,
          testPacks,
          errColor,
          quiet
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
              compileCacheDirOpt,
              "test",
              testPacks,
              errColor
            )
            testPackIdents <- testPacks.traverse(_.getMain(nameMap)(platformIO))
            out <- {
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
              val discovered = testPackages.map(p => (p, ev.evalTest(p)))
              val discoveryErrors =
                discovered.collect { case (_, Left(err)) =>
                  testDiscoveryError(err)
                }

              if (discoveryErrors.nonEmpty) {
                moduleIOMonad.raiseError[Output[Path]](
                  CliException.Basic(
                    "invalid test discovery:\n" + discoveryErrors.mkString("\n")
                  )
                )
              } else {
                val res0 = discovered.collect { case (p, Right(testResult)) =>
                  (p, testResult)
                }
                val res =
                  if (testPacks.isEmpty) res0.filter { case (_, testRes) =>
                    testRes.isDefined
                  }
                  else res0

                moduleIOMonad.pure(
                  Output.TestOutput(res, errColor, quiet): Output[Path]
                )
              }
            }
          } yield out
        }
    }

    Opts.subcommand("test", "test a set of bosatsu modules")(testOpt)
  }
}
