package dev.bosatsu.tool_command

import cats.syntax.all._
import com.monovore.decline.Opts
import dev.bosatsu.LocationMap
import dev.bosatsu.Predef
import dev.bosatsu.PredefImpl
import dev.bosatsu.library.LibraryEvaluation
import dev.bosatsu.tool.{
  CliException,
  CommonOpts,
  MainIdentifier,
  Output,
  PackageResolver,
  PathGen
}
import dev.bosatsu.{PackageName, Par, PlatformIO, TypeName, rankn}
import org.typelevel.paiges.Doc

object EvalCommand {
  private val progMainType: rankn.Type =
    rankn.Type.TyConst(
      rankn.Type.Const.Defined(
        PackageName.parts("Bosatsu", "Prog"),
        TypeName("Main")
      )
    )

  private def runOutput[Path](
      result: Output.EvaluationResult,
      args: List[String]
  ): Either[Exception & CliException, Output[Path]] =
    if (result.tpe == progMainType) {
      val run = result.value.map(PredefImpl.runProgMain(_, args))
      Right(Output.RunMainResult(run))
    } else {
      val actual = rankn.Type.fullyResolvedDocument.document(result.tpe).render(80)
      Left(
        CliException.Basic(
          show"--run requires a Bosatsu/Prog::Main value, found type: $actual"
        )
      )
    }

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
      ev = LibraryEvaluation.fromPackageMap(packs, Predef.evalExternals)
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
      evalDoc = memoizedEval.map { v =>
        toDocFn(v) match {
          case Right(d) => d
          case Left(_) =>
            Doc.text(
              "Could not render the value. The value does not appear to be the correct type. This should be impossible. Report this as a bug."
            )
        }
      }
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
      Opts
        .flag(
          "run",
          help =
            "run the selected value as Bosatsu/Prog::Main and return its exit code"
        )
        .orFalse,
      Opts.arguments[String]("arg").orEmpty,
      Colorize.optsConsoleDefault
    ).mapN {
      (
          srcs,
          includes,
          packageResolver,
          publicDependencies,
          privateDependencies,
          mainPackage,
          runMain,
          runArgs,
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
          ).flatMap { case (_, result) =>
            if (runMain) {
              moduleIOMonad.fromEither(runOutput(result, runArgs))
            } else if (runArgs.nonEmpty) {
              moduleIOMonad.raiseError(
                CliException.Basic("trailing args require --run")
              )
            } else {
              moduleIOMonad.pure(result: Output[Path])
            }
          }
        }
    }

    Opts.subcommand("eval", "evaluate an expression and print the output")(
      evalOpt
    )
  }
}
