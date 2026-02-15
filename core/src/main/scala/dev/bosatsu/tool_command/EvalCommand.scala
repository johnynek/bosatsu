package dev.bosatsu.tool_command

import cats.{Eval}
import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.Predef
import dev.bosatsu.library.LibraryEvaluation
import dev.bosatsu.tool.{
  CliException,
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
        (value match {
          case None        => ev.evaluateMainValue(mainPackageName)
          case Some(ident) => ev.evaluateNameValue(mainPackageName, ident)
        }).leftMap { ex =>
          CliException.Basic(Option(ex.getMessage).getOrElse(ex.toString))
        }
      }
      memoizedEval = eval.memoize
      toDocFn = ev.valueToDoc.toDoc(tpe)
      renderedDoc <- moduleIOMonad.fromEither {
        toDocFn(memoizedEval.value).leftMap { err =>
          CliException.Basic(show"unable to render value: $err")
        }
      }
      evalDoc = Eval.now(renderedDoc)
    } yield (ev, Output.EvaluationResult(memoizedEval, tpe, evalDoc))
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
