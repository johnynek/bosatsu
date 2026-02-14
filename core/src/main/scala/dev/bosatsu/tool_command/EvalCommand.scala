package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.Predef
import dev.bosatsu.library.LibraryEvaluation
import dev.bosatsu.tool.{
  CommonOpts,
  MainIdentifier,
  Output,
  PackageResolver,
  PathGen
}
import dev.bosatsu.{Par, PlatformIO}

object EvalCommand {
  def runEval[F[_], Path](
      platformIO: PlatformIO[F, Path],
      srcs: PathGen[F, Path],
      includes: PathGen[F, Path],
      packageResolver: PackageResolver[F, Path],
      publicDependencies: List[Path],
      privateDependencies: List[Path],
      commandName: String,
      mainPackage: MainIdentifier[Path],
      errColor: LocationMap.Colorize
  )(implicit
      ec: Par.EC
  ): F[(LibraryEvaluation[Unit], Output.EvaluationResult)] = {
    import platformIO.moduleIOMonad

    for {
      (packs, names) <- RuntimeCommandSupport.packMap(
        platformIO,
        srcs,
        includes,
        packageResolver,
        publicDependencies,
        privateDependencies,
        commandName,
        mainPackage :: Nil,
        errColor
      )
      (mainPackageName, value) <- mainPackage.getMain(names)(platformIO)
      ev = LibraryEvaluation.fromPackageMap(packs, Predef.jvmExternals)
      (eval, tpe) <- moduleIOMonad.fromEither {
        value match {
          case None        => ev.evaluateMainValue(mainPackageName)
          case Some(ident) => ev.evaluateNameValue(mainPackageName, ident)
        }
      }
      memoizedEval = eval.memoize
      toDocFn = ev.valueToDoc.toDoc(tpe)
      evalDoc = memoizedEval.map { v =>
        toDocFn(v) match {
          case Right(d) => d
          // $COVERAGE-OFF$ unreachable due to being well typed
          case Left(err) =>
            sys.error(show"got illtyped error: $err")
          // $COVERAGE-ON$
        }
      }
    } yield (ev, Output.EvaluationResult(eval, tpe, evalDoc))
  }

  def opts[F[_], Path](
      platformIO: PlatformIO[F, Path],
      commonOpts: CommonOpts[F, Path]
  ): Opts[F[Output[Path]]] = {
    import platformIO.moduleIOMonad
    import LocationMap.Colorize

    val evalOpt = (
      commonOpts.sourcePathOpts,
      commonOpts.includePathOpts,
      commonOpts.packageResolverOpts,
      commonOpts.publicDependencyOpts,
      commonOpts.privateDependencyOpts,
      commonOpts.mainIdentifierOpt,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          mainPackage,
          errColor
      ) =>
        platformIO.withEC {
          runEval(
            platformIO,
            srcs,
            includes,
            packageResolver,
            publicDependencies,
            privateDependencies,
            "eval",
            mainPackage,
            errColor
          ).map { case (_, result) => (result: Output[Path]) }
        }
    }

    Opts.subcommand("eval", "evaluate an expression and print the output")(
      evalOpt
    )
  }
}
