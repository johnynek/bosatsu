package dev.bosatsu.codegen.clang

import cats.data.{NonEmptyList, Validated}
import com.monovore.decline.{Argument, Opts}
import java.util.regex.{Pattern => RegexPat}
import dev.bosatsu.codegen.{Transpiler, CompilationNamespace}
import dev.bosatsu.{
  BuildInfo,
  Identifier,
  Json,
  PackageName,
  Par,
  PlatformIO,
  TypeName
}
import dev.bosatsu.tool.{CliException, ExitCode}
import dev.bosatsu.rankn.Type
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

import Identifier.Bindable

import cats.syntax.all._
import cats.Applicative

case object ClangTranspiler extends Transpiler {

  sealed abstract class EmitMode {
    def apply[K](
        cs: CompilationNamespace[K],
        roots: Set[(PackageName, Identifier)]
    ): CompilationNamespace[K]
  }
  object EmitMode {
    case object Shake extends EmitMode {
      def apply[K](
          cs: CompilationNamespace[K],
          roots: Set[(PackageName, Identifier)]
      ): CompilationNamespace[K] =
        cs.treeShake(roots)
    }
    case object All extends EmitMode {
      def apply[K](
          cs: CompilationNamespace[K],
          roots: Set[(PackageName, Identifier)]
      ): CompilationNamespace[K] =
        cs
    }

    implicit val argumentEmitMode: Argument[EmitMode] =
      new Argument[EmitMode] {
        def defaultMetavar: String = "emitmode"
        def read(string: String) =
          string match {
            case "shake" => Validated.valid(Shake)
            case "all"   => Validated.valid(All)
            case other   =>
              Validated.invalidNel(s"expected (shake|all) got $other")
          }
      }

    val opts: Opts[EmitMode] =
      Opts
        .option[EmitMode]("emitmode", "emit mode: shake|all, default = all")
        .withDefault(All)
  }
  sealed abstract class Mode[F[_]](val name: String)
  object Mode {
    case class Main[F[_]](pack: F[PackageName]) extends Mode[F]("main")
    case class Test[F[_]](
        filter: Option[PackageName => Boolean],
        filterRegexes: NonEmptyList[String],
        execute: Boolean
    ) extends Mode[F]("test") {
      def values[K](
          ns: CompilationNamespace[K]
      ): List[(PackageName, Bindable)] =
        (filter match {
          case None =>
            ns.testValues.toList
          case Some(k) =>
            ns.testValues.iterator.filter { case (p, _) => k(p) }.toList
        }).sortBy(_._1)
    }

    def testOpts[F[_]](executeOpts: Opts[Boolean]): Opts[Mode.Test[F]] =
      (
        Opts
          .options[String](
            "filter",
            help = "regular expression to filter package names"
          )
          .orNone,
        executeOpts
      ).tupled
        .mapValidated {
          case (None, e) =>
            Validated.valid(Test(None, NonEmptyList.one(".*"), e))
          case (Some(res), e) =>
            Try(res.map(RegexPat.compile(_))) match {
              case Success(pats) =>
                Validated.valid(
                  Test(
                    Some(pn => pats.exists(_.matcher(pn.asString).matches())),
                    res,
                    e
                  )
                )
              case Failure(e) =>
                Validated.invalidNel(
                  s"could not parse pattern: $res\n\n${e.getMessage}"
                )
            }
        }

    def opts[F[_]: Applicative]: Opts[Mode[F]] =
      Opts
        .option[PackageName]("main", "the package to use as an entry point")
        .map(pn => Main(Applicative[F].pure(pn)))
        .orElse(
          Opts.flag("test", "compile the tests") *> testOpts[F](
            Opts
              .flag(
                "execute",
                help = "run the test immediately after compiling"
              )
              .orFalse
          )
        )
  }
  case class GenExternalsMode(generate: Boolean)
  object GenExternalsMode {
    val opts: Opts[GenExternalsMode] =
      Opts
        .flag("gen_ext_headers", "generate externals header files")
        .as(GenExternalsMode(true))
        .orElse(Opts(GenExternalsMode(false)))
  }

  case class Output[F[_], P](cOut: P, exeOut: Option[(P, F[CcConf])])
  object Output {
    def ccConfOpt[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[CcConf]] = {
      import platformIO.{moduleIOMonad, pathArg, showPath}

      def parseCCFile(confPath: P): F[CcConf] =
        for {
          json <- platformIO.parseUtf8(confPath, Json.parserFile)
          ccConf <- CcConf.parse(json) match {
            case Right(cc)            => moduleIOMonad.pure(cc)
            case Left((str, j, path)) =>
              moduleIOMonad.raiseError(
                CliException.Basic(
                  show"when parsing $confPath got error: $str at $path to json: $j"
                )
              )
          }
        } yield ccConf

      val defaultCcConf = for {
        rootOpt <- platformIO.gitTopLevel
        root <- platformIO.getOrError(
          rootOpt,
          "could not find .git directory to locate default cc_conf"
        )
        gitSha <- platformIO.getOrError(
          BuildInfo.gitHeadCommit,
          s"compiler version ${BuildInfo.version} was built without a git-sha"
        )
        confPath = platformIO.resolve(
          root,
          ".bosatsuc" :: gitSha :: "cc_conf.json" :: Nil
        )
        _ <- platformIO.fsDataType(confPath).flatMap {
          case Some(PlatformIO.FSDataType.File) => moduleIOMonad.unit
          case res @ (None | Some(PlatformIO.FSDataType.Dir)) =>
            moduleIOMonad.raiseError[Unit](
              CliException.Basic(
                show"expected a CcConf json file at $confPath but found: ${res.toString}.\n\n" +
                  "Perhaps you need to run `bosatsu c-runtime install` (or `make install` in c_runtime)"
              )
            )
        }
        ccConf <- parseCCFile(confPath)
      } yield ccConf

      Opts
        .option[P](
          "cc_conf",
          help =
            "path to cc_conf.json file which configures c compilation on this platform"
        )
        .orNone
        .map {
          case Some(p) => parseCCFile(p)
          case None    => defaultCcConf
        }
    }

    def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[Output[F, P]] = {
      import platformIO.pathArg

      val cOpt = Opts
        .option[P]("output", help = "name of output c code file.", short = "o")
        .orElse {
          Opts("output.c")
            .mapValidated(platformIO.path(_))
        }

      val exeOpt =
        (
          Opts.option[P](
            "exe_out",
            help = "if set, compile the c code to an executable",
            short = "e"
          ),
          ccConfOpt[F, P](platformIO)
        ).tupled

      (cOpt, exeOpt.orNone).mapN(Output(_, _))
    }
  }

  case class Arguments[F[_], P](
      mode: Mode[F],
      emit: EmitMode,
      generateExternals: GenExternalsMode,
      output: Output[F, P],
      outDir: P,
      platformIO: PlatformIO[F, P]
  )
  type Args[F[_], P] = Arguments[F, P]

  def justOptsGivenModeOutput[F[_], M, P](
      modeOutOpts: Opts[M],
      platformIO: PlatformIO[F, P]
  )(
      extract: M => (Mode[F], Output[F, P])
  ): Opts[(M, Transpiler.Optioned[F, P])] =
    (
      modeOutOpts,
      EmitMode.opts,
      GenExternalsMode.opts,
      Transpiler.outDir(using platformIO.pathArg)
    ).mapN { (mOut, e, g, outDir) =>
      (
        mOut,
        Transpiler.optioned(this) {
          val (m, out) = extract(mOut)
          Arguments(m, e, g, out, outDir, platformIO)
        }
      )
    }

  def justOptsGivenMode[F[_], M, P](
      modeOpts: Opts[M],
      platformIO: PlatformIO[F, P]
  )(extractMode: M => Mode[F]): Opts[(M, Transpiler.Optioned[F, P])] =
    justOptsGivenModeOutput(
      modeOpts.product(Output.opts(platformIO)),
      platformIO
    ) { case (m, o) => (extractMode(m), o) }
      .map { case ((m, _), opt) => (m, opt) }

  def opts[F[_], P](
      platformIO: PlatformIO[F, P]
  ): Opts[Transpiler.Optioned[F, P]] =
    Opts.subcommand("c", "generate c code") {
      justOptsGivenMode(
        Mode.opts[F](using platformIO.moduleIOMonad),
        platformIO
      )(m => m).map(_._2)
    }

  private def spacePackList(ps: Iterable[PackageName]): Doc =
    (Doc.line + Doc.intercalate(
      Doc.comma + Doc.line,
      ps.map(p => Doc.text(p.asString))
    ))
      .nested(4)
      .grouped

  case class CircularPackagesFound(loop: NonEmptyList[(Any, PackageName)])
      extends Exception("circular deps in packages")
      with CliException {
    def errDoc: Doc =
      (Doc.text("circular dependencies found in packages:") + spacePackList(
        loop.toList.map(_._2) // ??? TODO report the key in a sane way
      ))
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  case object TestExecutionFailed
      extends Exception("test execution failed")
      with CliException {
    def errDoc: Doc = Doc.empty
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
    override def report[F[_], P](platform: PlatformIO[F, P]): F[ExitCode] =
      platform.moduleIOMonad.pure(exitCode)
  }

  case class InvalidMainValue(pack: PackageName, message: String)
      extends Exception(s"invalid main ${pack.asString}: $message.")
      with CliException {
    def errDoc = Doc.text(getMessage())
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  case class NoTestsFound(packs: List[PackageName], regex: NonEmptyList[String])
      extends Exception(show"no tests found in $packs with regex $regex")
      with CliException {
    def errDoc: Doc =
      (Doc.text("no tests found in:") + spacePackList(packs) + Doc.hardLine +
        Doc.text("using regexes:") +
        (Doc.line + Doc
          .intercalate(Doc.line, regex.toList.map(Doc.text(_)))
          .nested(4)).grouped)

    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  /** given the type (and possible the value) we return a C function name that
    * takes this value and the C args and returns an exit code which the main
    * function can return:
    *
    * int run_main(BValue main_value, int argc, char** args)
    */
  def validMain(tpe: Type): Either[String, Code.Ident] = {
    val ProgMain =
      Type.Const.Defined(PackageName.parts("Bosatsu", "Prog"), TypeName("Main"))
    tpe match {
      case Type.TyConst(ProgMain) =>
        Right(Code.Ident("bsts_Bosatsu_Prog_run_main"))
      case notMain =>
        Left(
          s"unknown type for main: ${Type.fullyResolvedDocument.document(notMain).render(80)}"
        )
    }
  }

  def renderAll[F[_], P, S](
      ns: CompilationNamespace[S],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]] = {
    import args.platformIO._

    // we have to render the code in sorted order
    NonEmptyList.fromList(ns.topoSort.loopNodes) match {
      case Some(loop) => moduleIOMonad.raiseError(CircularPackagesFound(loop))
      case None       =>
        val docAndEffect: F[Either[ClangGen.Error, (Doc, F[Unit])]] =
          args.mode match {
            case Mode.Main(fp) =>
              fp.flatMap { p =>
                (ns.mainValues(validMain(_).isRight).get(p) match {
                  case Some((b, t)) =>
                    validMain(t) match {
                      case Right(mainRun) =>
                        val ns1 = args.emit(ns, Set((p, b)))
                        val clangGen = new ClangGen(ns1)
                        moduleIOMonad.pure(
                          clangGen
                            .renderMain(p, b, mainRun)
                            .map((_, moduleIOMonad.unit))
                        )
                      case Left(invalid) =>
                        moduleIOMonad
                          .raiseError[Either[ClangGen.Error, (Doc, F[Unit])]](
                            InvalidMainValue(p, invalid)
                          )
                    }
                  case None =>
                    moduleIOMonad
                      .raiseError[Either[ClangGen.Error, (Doc, F[Unit])]](
                        InvalidMainValue(p, "empty package")
                      )
                })
              }
            case test @ Mode.Test(_, re, execute) =>
              val tvs = test.values(ns)
              (if (tvs.isEmpty) {
                 moduleIOMonad.raiseError(
                   NoTestsFound(ns.rootPackages.toList, re)
                 )
               } else {
                 val ns1 = args.emit(ns, tvs.toSet)
                 val clangGen = new ClangGen(ns1)
                 val r = clangGen.renderTests(values = tvs.toList.sorted)

                 val exeIO = args.output match {
                   case Output(_, Some((exeName, _))) if execute =>
                     val exePath = args.platformIO.resolve(args.outDir, exeName)
                     args.platformIO
                       .system(args.platformIO.showPath.show(exePath), Nil)
                       .handleErrorWith {
                         case _: CliException =>
                           moduleIOMonad.raiseError(TestExecutionFailed)
                       }
                   case _ =>
                     moduleIOMonad.unit
                 }

                 moduleIOMonad.pure(r.map((_, exeIO)))
               })
          }

        docAndEffect.flatMap {
          case Left(err) =>
            moduleIOMonad.raiseError(
              CliException(
                s"clang gen error: ${err.display.render(80)}",
                err = err.display
              )
            )

          case Right((doc, effect)) =>
            val outputName = resolve(args.outDir, args.output.cOut)
            val externalHeaders =
              if (args.generateExternals.generate) {
                (new ClangGen(ns)).generateExternalsStub.iterator.map {
                  case (n, d) =>
                    resolve(args.outDir, n) -> d
                }.toList
              } else Nil

            args.output.exeOut match {
              case None =>
                moduleIOMonad.pure(
                  (outputName -> doc) :: externalHeaders
                )
              case Some((exe, fcc)) =>
                for {
                  ccConf <- fcc
                  // we have to write the c code before we can compile
                  _ <- args.platformIO.writeDoc(outputName, doc)
                  _ <- ccConf.compile(outputName, resolve(args.outDir, exe))(
                    args.platformIO
                  )
                  _ <-
                    effect // now that we have compiled, we can run the effect
                } yield externalHeaders
            }
        }
    }
  }
}
