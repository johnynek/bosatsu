package dev.bosatsu.codegen.clang

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.parse.{Parser => CP}
import com.monovore.decline.{Argument, Opts}
import java.util.regex.{Pattern => RegexPat}
import dev.bosatsu.codegen.{Transpiler, CompilationNamespace}
import dev.bosatsu.{
  BuildInfo,
  Identifier,
  Json,
  NameSuggestion,
  Package,
  PackageName,
  Par,
  PlatformIO,
  TypeName
}
import dev.bosatsu.tool.{CliException, ExitCode}
import dev.bosatsu.rankn.Type
import org.typelevel.paiges.Doc
import scala.util.{Failure, Success, Try}

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
        .option[EmitMode]("emitmode", "emit mode: shake|all, default = shake")
        .withDefault(Shake)
  }
  sealed abstract class Mode[F[_]](val name: String)
  object Mode {
    case class Main[F[_]](pack: F[PackageName]) extends Mode[F]("main")
    object Test {
      type ValueSelector = (PackageName, Identifier.Bindable)

      sealed trait SelectionMode {
        def sourceFilter: Option[PackageName => Boolean]
        def filterRegexes: NonEmptyList[String]
      }
      object SelectionMode {
        final case class ByFilter(
            filterRegexes: NonEmptyList[String],
            filter: Option[PackageName => Boolean]
        ) extends SelectionMode {
          def sourceFilter: Option[PackageName => Boolean] = filter
        }

        final case class ByValue(
            packageName: PackageName,
            bindable: Identifier.Bindable
        ) extends SelectionMode {
          def sourceFilter: Option[PackageName => Boolean] = Some(_ == packageName)
          def filterRegexes: NonEmptyList[String] =
            NonEmptyList.one(show"${packageName.asString}::${bindable.sourceCodeRepr}")
        }
      }
    }

    case class Test[F[_]](
        selection: Test.SelectionMode,
        execute: Boolean,
        quiet: Boolean
    ) extends Mode[F]("test") {
      def sourceFilter: Option[PackageName => Boolean] = selection.sourceFilter
      def filterRegexes: NonEmptyList[String] = selection.filterRegexes

      def values[K](
          ns: CompilationNamespace[K]
      ): Either[Exception & CliException, List[(
        PackageName,
        Package.TestEntry[Any]
      )]] =
        selection match {
          case Test.SelectionMode.ByFilter(_, filter) =>
            val filtered = (filter match {
              case None =>
                ns.testEntries.toList
              case Some(k) =>
                ns.testEntries.iterator.filter { case (p, _) => k(p) }.toList
            }).sortBy(_._1)

            val errors =
              filtered.collect { case (pn, Left(err)) => (pn, err) }
            NonEmptyList.fromList(errors) match {
              case Some(errs) =>
                Left(InvalidTestDiscovery(errs))
              case None       =>
                Right(filtered.collect { case (pn, Right(entry)) => (pn, entry) })
            }
          case Test.SelectionMode.ByValue(packageName, bindable) =>
            val knownPacks = ns.rootPackages.toList.sorted
            if (!ns.rootPackages.contains(packageName)) {
              Left(
                InvalidTestValueSelection(
                  packageName,
                  bindable,
                  unknownTestPackMsg(packageName, bindable, knownPacks)
                )
              )
            } else {
              val exported = ns.exportedValues(packageName).getOrElse(Map.empty)
              val exportedTestValues = exported.iterator.collect {
                case (name, tpe) if isTestValueType(tpe) => name
              }.toList
              exported.get(bindable) match {
                case None =>
                  if (exportedTestValues.isEmpty) {
                    Left(
                      InvalidTestValueSelection(
                        packageName,
                        bindable,
                        noExportedTestValuesMsg(packageName, bindable)
                      )
                    )
                  } else {
                    val exportedTests = exportedTestValues.map(_.sourceCodeRepr).sorted
                    Left(
                      InvalidTestValueSelection(
                        packageName,
                        bindable,
                        testValueNotExportedMsg(
                          packageName,
                          bindable,
                          exportedTests
                        )
                      )
                    )
                  }
                case Some(tpe) if !isTestValueType(tpe) =>
                  Left(
                    InvalidTestValueSelection(
                      packageName,
                      bindable,
                      exportedValueNotTestMsg(packageName, bindable, tpe)
                    )
                  )
                case Some(_) =>
                  ns.exportedTestEntry(packageName, bindable) match {
                    case Some(entry) =>
                      Right((packageName, entry) :: Nil)
                    case None        =>
                      Left(
                        InvalidTestValueSelection(
                          packageName,
                          bindable,
                          exportedValueNotTopLevelTestMsg(packageName, bindable)
                        )
                      )
                  }
              }
            }
        }
    }

    private implicit val testValueArgument: Argument[Test.ValueSelector] =
      new Argument[Test.ValueSelector] {
        def defaultMetavar: String = "valueIdent"

        def read(
            string: String
        ): ValidatedNel[String, Test.ValueSelector] =
          (PackageName.parser ~ (CP.string("::") *> Identifier.parser))
            .parseAll(string) match {
            case Right((pack, bindable: Identifier.Bindable)) =>
              Validated.valid((pack, bindable))
            case Right((pack, cons: Identifier.Constructor))  =>
              Validated.invalidNel(
                show"${pack.asString}::${cons.asString} is a constructor or type name, not a value. A top-level value is required."
              )
            case _ =>
              Validated.invalidNel(
                s"could not parse $string as package::value. Must be package::value, e.g. Foo/Bar::bippy."
              )
          }
      }

    private def testFilterSelectionOpts: Opts[Test.SelectionMode.ByFilter] =
      Opts
        .options[String](
          "filter",
          help = "regular expression to filter package names"
        )
        .orNone
        .mapValidated {
          case None      =>
            Validated.valid(
              Test.SelectionMode.ByFilter(
                NonEmptyList.one(".*"),
                None
              )
            )
          case Some(res) =>
            Try(res.map(RegexPat.compile(_))) match {
              case Success(pats) =>
                Validated.valid(
                  Test.SelectionMode.ByFilter(
                    res,
                    Some(pn => pats.exists(_.matcher(pn.asString).matches()))
                  )
                )
              case Failure(e)    =>
                Validated.invalidNel(
                  s"could not parse pattern: $res\n\n${e.getMessage}"
                )
            }
        }

    private def testSelectionOpts: Opts[Test.SelectionMode] =
      Opts
        .option[Test.ValueSelector](
          "value",
          help = "single exported test value to run (package::value)"
        )
        .map { case (packageName, bindable) =>
          Test.SelectionMode.ByValue(packageName, bindable)
        }
        .orElse(testFilterSelectionOpts)

    def testFilterOpts: Opts[Option[PackageName => Boolean]] =
      testFilterSelectionOpts.map(_.sourceFilter)

    def testOpts[F[_]](executeOpts: Opts[Boolean]): Opts[Mode.Test[F]] =
      (
        testSelectionOpts,
        executeOpts,
        Opts
          .flag(
            "quiet",
            help = "only print failure details and final test summary"
          )
          .orFalse
      ).mapN(Test(_, _, _))

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

  case class Output[F[_], P](
      cOut: P,
      cOutRelativeToOutDir: Boolean,
      exeOut: Option[(P, F[CcConf])],
      exeOutRelativeToOutDir: Boolean = true,
      ccFlags: List[String],
      ccLibs: List[String]
  )
  object Output {
    def ccConfOpt[F[_], P](platformIO: PlatformIO[F, P]): Opts[F[CcConf]] = {
      import platformIO.{moduleIOMonad, pathArg, showPath}

      def validateExplicitCcConfPath(confPath: P): F[Unit] =
        platformIO.fsDataType(confPath).flatMap {
          case Some(PlatformIO.FSDataType.File) =>
            moduleIOMonad.unit
          case None =>
            moduleIOMonad.raiseError(
              CliException.Basic(
                show"cc_conf file not found: $confPath\n" +
                  "Run `bosatsu c-runtime install` or provide a valid `--cc_conf` path."
              )
            )
          case Some(PlatformIO.FSDataType.Dir) =>
            moduleIOMonad.raiseError(
              CliException.Basic(
                show"cc_conf path is not a file: $confPath"
              )
            )
        }

      def parseCCFile(confPath: P): F[CcConf] =
        for {
          _ <- validateExplicitCcConfPath(confPath)
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
          CliException.Basic(
            "could not find a .git entry to locate default cc_conf.\n" +
              "Pass --cc_conf <path/to/cc_conf.json>, or run from a git checkout with .git available."
          )
        )
        gitSha <- platformIO.getOrError(
          BuildInfo.gitHeadCommit,
          CliException.Basic(
            s"compiler version ${BuildInfo.version} was built without a git-sha.\n" +
              "Pass --cc_conf <path/to/cc_conf.json>, or run `bosatsu c-runtime install --git_sha <sha>` and use the generated cc_conf."
          )
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
                show"missing default c runtime config before C compilation.\n" +
                  show"runtime hash: $gitSha\n" +
                  "expected artifact: cc_conf.json\n" +
                  show"expected path: $confPath\n" +
                  show"found: ${res.toString}.\n\n" +
                  "Run `bosatsu c-runtime install` (or `make install` in c_runtime)."
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
      val ccFlagsOpt =
        Opts
          .options[String](
            "cc_flag",
            help =
              "additional compiler/linker flags passed before source files (repeatable)"
          )
          .orEmpty
          .map(_.toList)
      val ccLibsOpt =
        Opts
          .options[String](
            "cc_lib",
            help =
              "additional linker arguments passed after source files (repeatable)"
          )
          .orEmpty
          .map(_.toList)

      (cOpt, exeOpt.orNone, ccFlagsOpt, ccLibsOpt).mapN {
        (cOut, exeOut, ccFlags, ccLibs) =>
          Output(
            cOut = cOut,
            cOutRelativeToOutDir = true,
            exeOut = exeOut,
            exeOutRelativeToOutDir = true,
            ccFlags = ccFlags,
            ccLibs = ccLibs
          )
      }
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
      extends Exception(message)
      with CliException {
    def errDoc = Doc.text(getMessage())
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  case class InvalidTestValueSelection(
      packageName: PackageName,
      valueName: Identifier.Bindable,
      message: String
  ) extends Exception(message)
      with CliException {
    def errDoc = Doc.text(getMessage())
    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  private def packageIdent(pack: PackageName): Identifier =
    Identifier.Synthetic(pack.asString)

  private def nearestPackage(
      query: PackageName,
      known: List[PackageName]
  ): Option[PackageName] =
    NameSuggestion
      .best(
        packageIdent(query),
        known.map { pack =>
          NameSuggestion.Candidate(packageIdent(pack), pack)
        }
      )
      .map(_.value)

  private def packageCountMsg(count: Int): String = {
    val packWord = if (count == 1) "package" else "packages"
    s"($count $packWord available.)"
  }

  private def packageSuggestionsForRegexes(
      regexes: NonEmptyList[String],
      knownPacks: List[PackageName]
  ): List[PackageName] =
    regexes.toList
      .flatMap(regex => PackageName.parse(regex).toList)
      .flatMap(query => nearestPackage(query, knownPacks).toList)
      .distinct
      .sorted

  private def unknownMainPackMsg(
      mainPack: PackageName,
      knownPacks: List[PackageName]
  ): String = {
    val suggestion =
      nearestPackage(mainPack, knownPacks)
        .fold("")(pack => s"\nDid you mean: ${pack.asString} ?")
    s"invalid main package `${mainPack.asString}`: unknown package.$suggestion\n${packageCountMsg(knownPacks.size)}"
  }

  private def invalidMainPackMsg(mainPack: PackageName, detail: String): String =
    s"invalid main package `${mainPack.asString}`: $detail"

  private def invalidTestValueMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable,
      detail: String
  ): String =
    s"invalid test value `${packageName.asString}::${bindable.sourceCodeRepr}`: $detail"

  private def unknownTestPackMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable,
      knownPacks: List[PackageName]
  ): String = {
    val suggestion =
      nearestPackage(packageName, knownPacks)
        .fold("")(pack => s"\nDid you mean: ${pack.asString} ?")
    invalidTestValueMsg(
      packageName,
      bindable,
      s"unknown package.$suggestion\n${packageCountMsg(knownPacks.size)}"
    )
  }

  private def noExportedTestValuesMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable
  ): String =
    invalidTestValueMsg(
      packageName,
      bindable,
      s"${packageName.asString} has no exported test values. Export a value with type Bosatsu/Predef::Test or Bosatsu/Prog::ProgTest."
    )

  private def testValueNotExportedMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable,
      exportedTestValues: List[String]
  ): String = {
    val detail =
      if (exportedTestValues.isEmpty) {
        s"value is not exported by ${packageName.asString}."
      } else {
        val rendered = exportedTestValues.mkString(", ")
        s"value is not exported by ${packageName.asString}. exported test values: [$rendered]"
      }
    invalidTestValueMsg(packageName, bindable, detail)
  }

  private def exportedValueNotTestMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable,
      tpe: Type
  ): String = {
    val actualType = Type.fullyResolvedDocument.document(tpe).render(80)
    invalidTestValueMsg(
      packageName,
      bindable,
      s"exported value is not a test value. Expected Bosatsu/Predef::Test or Bosatsu/Prog::ProgTest, found: $actualType"
    )
  }

  private def exportedValueNotTopLevelTestMsg(
      packageName: PackageName,
      bindable: Identifier.Bindable
  ): String =
    invalidTestValueMsg(
      packageName,
      bindable,
      "exported value is not a top-level test value in this package."
    )

  private def isTestValueType(tpe: Type): Boolean =
    tpe.sameAs(Type.TestType) || tpe.sameAs(Type.ProgTestType)

  private def testDiscoveryErrMsg(
      err: Package.TestDiscoveryError
  ): String =
    err match {
      case Package.TestDiscoveryError.PlainTestAfterProgTest(
            packageName,
            progTest,
            plainTestsAfter
          ) =>
        val plainTestStr =
          plainTestsAfter.toList.map(_.sourceCodeRepr).mkString(", ")
        s"${packageName.asString}: found top-level Test value(s) after ProgTest ${progTest.sourceCodeRepr}: $plainTestStr"
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

  private def noPackagesMatchedDoc(
      regexes: NonEmptyList[String],
      knownPacks0: List[PackageName]
  ): Doc = {
    val knownPacks = knownPacks0.distinct.sorted
    val suggestions = packageSuggestionsForRegexes(regexes, knownPacks)
    val regexHeader =
      if (regexes.tail.isEmpty) Doc.text("no packages found matching regex:")
      else Doc.text("no packages found matching regexes:")
    val regexListDoc =
      Doc.intercalate(Doc.line, regexes.toList.map(Doc.text(_)))

    val suggestionDocs =
      suggestions match {
        case Nil =>
          Nil
        case suggested :: Nil =>
          (Doc.text("Did you mean: ") + Doc.text(suggested.asString) + Doc.text(
            " ?"
          )) :: Nil
        case many =>
          val listed = Doc.intercalate(Doc.line, many.map(pack => Doc.text(pack.asString)))
          (Doc.text("Did you mean one of:") + (Doc.line + listed).nested(2))
            .grouped :: Nil
      }

    Doc.intercalate(
      Doc.hardLine,
      (regexHeader + (Doc.line + regexListDoc).nested(2)).grouped ::
        (suggestionDocs :+ Doc.text(packageCountMsg(knownPacks.size)))
    )
  }

  case class NoPackagesMatchedFilter(
      regexes: NonEmptyList[String],
      knownPacks0: List[PackageName]
  ) extends Exception(noPackagesMatchedDoc(regexes, knownPacks0).render(200))
      with CliException {
    def errDoc: Doc = noPackagesMatchedDoc(regexes, knownPacks0)

    def stdOutDoc: Doc = Doc.empty
    def exitCode: ExitCode = ExitCode.Error
  }

  case class InvalidTestDiscovery(
      errors: NonEmptyList[(PackageName, Package.TestDiscoveryError)]
  ) extends Exception("invalid test discovery")
      with CliException {
    def errDoc: Doc =
      (Doc.text("invalid test discovery:") +
        (Doc.line + Doc
          .intercalate(
            Doc.line,
            errors.toList.map { case (_, err) =>
              Doc.text(testDiscoveryErrMsg(err))
            }
          )
          .nested(2)).grouped)
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
                val mains = ns.mainValues(validMain(_).isRight)
                (mains.get(p) match {
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
                            InvalidMainValue(
                              p,
                              invalidMainPackMsg(p, invalid)
                            )
                          )
                    }
                  case None =>
                    val knownPacks = ns.rootPackages.toList.sorted

                    val mainPacks = mains.keys.toList.sorted
                    val mainMsg =
                      if (mainPacks.nonEmpty) {
                        s"packages with valid main: ${mainPacks.map(_.asString).mkString(", ")}"
                      } else {
                        "no packages contain a value of type Bosatsu/Prog::Main"
                      }

                    val message =
                      if (!ns.rootPackages.contains(p)) {
                        unknownMainPackMsg(p, knownPacks)
                      } else {
                        invalidMainPackMsg(
                          p,
                          s"no value of type Bosatsu/Prog::Main in ${p.asString}. $mainMsg"
                        )
                      }
                    moduleIOMonad
                      .raiseError[Either[ClangGen.Error, (Doc, F[Unit])]](
                        InvalidMainValue(p, message)
                      )
                })
              }
            case test @ Mode.Test(_, execute, quiet) =>
              test.values(ns) match {
                case Left(err) =>
                  moduleIOMonad.raiseError(err)
                case Right(tvs)   =>
                  if (tvs.isEmpty) {
                    moduleIOMonad.raiseError(
                      NoTestsFound(ns.rootPackages.toList, test.filterRegexes)
                    )
                  } else {
                    val roots = tvs.iterator.map { case (p, entry) =>
                      (p, entry.bindable: Identifier)
                    }.toSet
                    val ns1 = args.emit(ns, roots)
                    val clangGen = new ClangGen(ns1)
                    val r = clangGen.renderTests(values = tvs)

                    val exeIO = args.output match {
                      case Output(
                            _,
                            _,
                            Some((exeName, _)),
                            exeOutRelativeToOutDir,
                            _,
                            _
                          ) if execute =>
                        val exePath =
                          if (exeOutRelativeToOutDir)
                            args.platformIO.resolve(args.outDir, exeName)
                          else exeName
                        val exeArgs = if (quiet) "--quiet" :: Nil else Nil
                        args.platformIO
                          .system(args.platformIO.showPath.show(exePath), exeArgs)
                          .handleErrorWith {
                            case _: CliException =>
                              moduleIOMonad.raiseError(TestExecutionFailed)
                            case other =>
                              moduleIOMonad.raiseError(other)
                          }
                      case _ =>
                        moduleIOMonad.unit
                    }

                    moduleIOMonad.pure(r.map((_, exeIO)))
                  }
              }
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
            val outputName =
              if (args.output.cOutRelativeToOutDir)
                resolve(args.outDir, args.output.cOut)
              else args.output.cOut
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
                val exeOutName =
                  // build can compile in a temp outdir while still targeting a cwd-relative exe path.
                  if (args.output.exeOutRelativeToOutDir)
                    resolve(args.outDir, exe)
                  else exe
                for {
                  ccConf <- fcc
                  // we have to write the c code before we can compile
                  _ <- args.platformIO.writeDoc(outputName, doc)
                  _ <- ccConf.compile(
                    outputName,
                    exeOutName,
                    extraFlags = args.output.ccFlags,
                    extraLibs = args.output.ccLibs
                  )(args.platformIO)
                  _ <-
                    effect // now that we have compiled, we can run the effect
                } yield externalHeaders
            }
        }
    }
  }
}
