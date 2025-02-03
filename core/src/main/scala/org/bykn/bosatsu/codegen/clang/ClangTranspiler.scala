package org.bykn.bosatsu.codegen.clang

import cats.data.{NonEmptyList, Validated}
import com.monovore.decline.{Argument, Opts}
import java.util.regex.{Pattern => RegexPat}
import org.bykn.bosatsu.codegen.Transpiler
import org.bykn.bosatsu.{
  BuildInfo,
  Identifier,
  Json,
  MatchlessFromTypedExpr,
  Package,
  PackageName,
  PackageMap,
  Par,
  PlatformIO,
  TypedExpr,
  TypeName
}
import org.bykn.bosatsu.tool.{CliException, ExitCode}
import org.bykn.bosatsu.rankn.Type
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

import Identifier.Bindable

import cats.syntax.all._

case object ClangTranspiler extends Transpiler {

  sealed abstract class EmitMode {
    def apply[A](
        pm: PackageMap.Typed[A],
        roots: Set[(PackageName, Identifier)]
    ): PackageMap.Typed[A]
  }
  object EmitMode {
    case object Shake extends EmitMode {
      def apply[A](
          pm: PackageMap.Typed[A],
          roots: Set[(PackageName, Identifier)]
      ): PackageMap.Typed[A] =
        PackageMap.treeShake(pm, roots)
    }
    case object All extends EmitMode {
      def apply[A](
          pm: PackageMap.Typed[A],
          roots: Set[(PackageName, Identifier)]
      ): PackageMap.Typed[A] = pm
    }

    implicit val argumentEmitMode: Argument[EmitMode] =
      new Argument[EmitMode] {
        def defaultMetavar: String = "emitmode"
        def read(string: String) =
          string match {
            case "shake" => Validated.valid(Shake)
            case "all"   => Validated.valid(All)
            case other =>
              Validated.invalidNel(s"expected (shake|all) got $other")
          }
      }

    val opts: Opts[EmitMode] =
      Opts
        .option[EmitMode]("emitmode", "emit mode: shake|all, default = all")
        .withDefault(All)
  }
  sealed abstract class Mode(val name: String)
  object Mode {
    case class Main(pack: PackageName) extends Mode("main")
    case class Test(
        filter: Option[PackageName => Boolean],
        filterRegexes: NonEmptyList[String]
    ) extends Mode("test") {
      def values(p: PackageMap.Typed[Any]): List[(PackageName, Bindable)] =
        (filter match {
          case None =>
            p.testValues.toList
          case Some(k) =>
            p.testValues.iterator.filter { case (p, _) => k(p) }.toList
        }).sortBy(_._1)
    }

    val opts: Opts[Mode] =
      Opts
        .option[PackageName]("main", "the package to use as an entry point")
        .map(Main(_))
        .orElse(
          Opts.flag("test", "compile the tests") *>
            (Opts
              .options[String](
                "filter",
                "regular expression to filter package names"
              )
              .orNone)
              .mapValidated {
                case None => Validated.valid(Test(None, NonEmptyList.one(".*")))
                case Some(res) =>
                  Try(res.map(RegexPat.compile(_))) match {
                    case Success(pats) =>
                      Validated.valid(
                        Test(
                          Some(pn =>
                            pats.exists(_.matcher(pn.asString).matches())
                          ),
                          res
                        )
                      )
                    case Failure(e) =>
                      Validated.invalidNel(
                        s"could not parse patterns: $res\n\n${e.getMessage}"
                      )
                  }
              }
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
    def opts[F[_], P](platformIO: PlatformIO[F, P]): Opts[Output[F, P]] = {
      import platformIO.{moduleIOMonad, pathArg, showPath}

      val cOpt = Opts
        .option[P]("output", "o", "name of output c code file.")
        .orElse {
          Opts("output.c")
            .mapValidated(platformIO.path(_))
        }

      def parseCCFile(confPath: P): F[CcConf] =
        for {
          json <- platformIO.parseUtf8(confPath, Json.parserFile)
          ccConf <- CcConf.parse(json) match {
            case Right(cc) => moduleIOMonad.pure(cc)
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
                  "Perhaps you need to `make install` the c_runtime"
              )
            )
        }
        ccConf <- parseCCFile(confPath)
      } yield ccConf

      val exeOpt =
        (
          Opts.option[P](
            "exe_out",
            help = "if set, compile the c code to an executable",
            short = "e"
          ),
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
        ).tupled

      (cOpt, exeOpt.orNone).mapN(Output(_, _))
    }
  }

  case class Arguments[F[_], P](
      mode: Mode,
      emit: EmitMode,
      generateExternals: GenExternalsMode,
      output: Output[F, P],
      platformIO: PlatformIO[F, P]
  )
  type Args[F[_], P] = Arguments[F, P]

  def opts[F[_], P](
      platformIO: PlatformIO[F, P]
  ): Opts[Transpiler.Optioned[F, P]] =
    Opts.subcommand("c", "generate c code") {
      (
        Mode.opts,
        EmitMode.opts,
        GenExternalsMode.opts,
        Output.opts[F, P](platformIO)
      ).mapN { (m, e, g, out) =>
        Transpiler.optioned(this)(Arguments(m, e, g, out, platformIO))
      }
    }

  case class GenError(error: ClangGen.Error)
      extends Exception(s"clang gen error: ${error.display.render(80)}")
      with CliException {
    def errDoc: Doc = error.display
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  private def spacePackList(ps: Iterable[PackageName]): Doc =
    (Doc.line + Doc.intercalate(
      Doc.comma + Doc.line,
      ps.map(p => Doc.text(p.asString))
    ))
      .nested(4)
      .grouped

  case class CircularPackagesFound(loop: NonEmptyList[PackageName])
      extends Exception("circular deps in packages")
      with CliException {
    def errDoc: Doc =
      (Doc.text("circular dependencies found in packages:") + spacePackList(
        loop.toList
      ))
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
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

  def externalsFor(pm: PackageMap.Typed[Any]): ClangGen.ExternalResolver =
    ClangGen.ExternalResolver.stdExternals(pm)

  /** given the type (and possible the value) we return a C function name that
    * takes this value and the C args and returns an exit code which the main
    * function can return:
    *
    * int run_main(BValue main_value, int argc, char** args)
    */
  def validMain[A](te: TypedExpr[A]): Either[String, Code.Ident] = {
    val ProgMain =
      Type.Const.Defined(PackageName.parts("Bosatsu", "Prog"), TypeName("Main"))
    te.getType match {
      case Type.TyConst(ProgMain) =>
        Right(Code.Ident("bsts_Bosatsu_Prog_run_main"))
      case notMain =>
        Left(
          s"unknown type for main: ${Type.fullyResolvedDocument.document(notMain).render(80)}"
        )
    }
  }

  def renderAll[F[_], P](
      outDir: P,
      pm: PackageMap.Typed[Any],
      args: Args[F, P]
  )(implicit ec: Par.EC): F[List[(P, Doc)]] = {
    import args.platformIO._

    // we have to render the code in sorted order
    val sorted = pm.topoSort
    NonEmptyList.fromList(sorted.loopNodes) match {
      case Some(loop) => moduleIOMonad.raiseError(CircularPackagesFound(loop))
      case None =>
        val ext = externalsFor(pm)
        val doc = args.mode match {
          case Mode.Main(p) =>
            pm.toMap.get(p).flatMap(Package.mainValue(_)) match {
              case Some((b, _, t)) =>
                validMain(t) match {
                  case Right(mainRun) =>
                    val pm1 = args.emit(pm, Set((p, b)))
                    val matchlessMap = MatchlessFromTypedExpr.compile((), pm1)
                    val sortedEnv = cats
                      .Functor[Vector]
                      .compose[NonEmptyList]
                      .map(sorted.layers)(pn => pn -> matchlessMap(pn))

                    ClangGen.renderMain(
                      sortedEnv = sortedEnv,
                      externals = ext,
                      value = (p, b, mainRun)
                    )
                  case Left(invalid) =>
                    return moduleIOMonad.raiseError(
                      InvalidMainValue(p, invalid)
                    )
                }
              case None =>
                return moduleIOMonad.raiseError(
                  InvalidMainValue(p, "empty package")
                )
            }
          case test @ Mode.Test(_, re) =>
            test.values(pm) match {
              case Nil =>
                return moduleIOMonad.raiseError(
                  NoTestsFound(pm.toMap.keySet.toList.sorted, re)
                )
              case nonEmpty =>
                val pm1 = args.emit(pm, nonEmpty.toSet)
                val matchlessMap = MatchlessFromTypedExpr.compile((), pm1)
                val sortedEnv = cats
                  .Functor[Vector]
                  .compose[NonEmptyList]
                  .map(sorted.layers)(pn => pn -> matchlessMap(pn))

                ClangGen.renderTests(
                  sortedEnv = sortedEnv,
                  externals = ext,
                  values = nonEmpty
                )
            }
        }

        doc match {
          case Left(err) => moduleIOMonad.raiseError(GenError(err))
          case Right(doc) =>
            val outputName = resolve(outDir, args.output.cOut)
            val externalHeaders =
              if (args.generateExternals.generate) {
                ext.generateExternalsStub.iterator.map { case (n, d) =>
                  resolve(outDir, n) -> d
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
                  _ <- ccConf.compile(outputName, resolve(outDir, exe))(
                    args.platformIO
                  )
                } yield externalHeaders
            }
        }
    }
  }
}
