package org.bykn.bosatsu

import cats.arrow.FunctionK
import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.{Eval, Traverse, MonadError}
import com.monovore.decline.{Argument, Command, Help, Opts}
import fastparse.all.P
import org.typelevel.paiges.Doc
import scala.util.{ Failure, Success, Try }

import LocationMap.Colorize

import cats.implicits._

/**
 * This is an implementation of the CLI tool where Path is abstracted.
 * The idea is to allow it to be testable and usable in scalajs where
 * we don't have file-IO
 */
abstract class MainModule[IO[_]](implicit val moduleIOMonad: MonadError[IO, Throwable]) {
  type Path

  implicit def pathArg: Argument[Path]

  def readPath(p: Path): IO[String]

  def readPackages(paths: List[Path]): IO[List[Package.Typed[Unit]]]

  def readInterfaces(paths: List[Path]): IO[List[Package.Interface]]

  /**
   * given an ordered list of prefered roots, if a packFile starts
   * with one of these roots, return a PackageName based on the rest
   */
  def pathPackage(roots: List[Path], packFile: Path): Option[PackageName]

  //////////////////////////////
  // Below here are concrete and should not use override
  //////////////////////////////

  final def run(args: List[String]): Either[Help, IO[Output]] =
    MainCommand.command
      .parse(args.toList)
      .map(_.run)

  sealed abstract class Output
  object Output {
    case class TestOutput(tests: List[(PackageName, Option[Test])]) extends Output
    case class EvaluationResult(value: Eval[Value], tpe: rankn.Type) extends Output
    case class JsonOutput(json: Json, output: Path) extends Output
    case class CompileOut(packList: List[Package.Typed[Any]], ifout: Option[Path], output: Path) extends Output
  }

  sealed abstract class MainCommand {
    def run: IO[Output]
  }

  object MainCommand {
    def parseInputs[F[_]: Traverse](paths: F[Path], pathToPack: Path => Option[PackageName]): IO[ValidatedNel[ParseError, F[((Path, LocationMap), Package.Parsed)]]] =
      // we use IO(traverse) so we can accumulate all the errors in parallel easily
      // if do this with parseFile returning an IO, we need to do IO.Par[Validated[...]]
      // and use the composed applicative... too much work for the same result
      paths.traverse { path =>
        val defaultPack = pathToPack(path)
        parseFile(Package.parser(defaultPack), path).map(_.map { case (lm, parsed) =>
          ((path, lm), parsed)
        })
      }
      .map(_.sequence)

    sealed trait ParseError {
      def showContext(errColor: Colorize): Option[Doc] =
        this match {
          case ParseError.PartialParse(err, _) =>
            err.showContext(errColor)
          case ParseError.ParseFailure(err, _) =>
            err.showContext(errColor)
          case ParseError.FileError(_, _) =>
            None
        }
    }

    object ParseError {
       case class PartialParse[A](error: Parser.Error.PartialParse[A], path: Path) extends ParseError
       case class ParseFailure(error: Parser.Error.ParseFailure, path: Path) extends ParseError
       case class FileError(readPath: Path, error: Throwable) extends ParseError
    }

    def parseFile[A](p: P[A], path: Path): IO[ValidatedNel[ParseError, (LocationMap, A)]] =
      readPath(path)
        .attempt
        .map {
          case Right(str) => Parser.parse(p, str).leftMap { nel =>
            nel.map {
              case pp@Parser.Error.PartialParse(_, _, _) => ParseError.PartialParse(pp, path)
              case pf@Parser.Error.ParseFailure(_, _) => ParseError.ParseFailure(pf, path)
            }
          }
          case Left(err) => Validated.invalidNel(ParseError.FileError(path, err))
        }

    /**
     * like typecheck, but a no-op for empty lists
     */
    def typeCheck0(
      inputs: List[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]
      ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
      NonEmptyList.fromList(inputs) match {
        case None =>
          // we should still return the predef
          // if it is not in ifs
          val useInternalPredef =
            !ifs.contains { p: Package.Interface => p.name == PackageName.PredefName }

          if (useInternalPredef) {
            moduleIOMonad.pure((PackageMap.fromIterable(Predef.predefCompiled :: Nil), Nil))
          }
          else {
            moduleIOMonad.pure((PackageMap.empty, Nil))
          }
        case Some(nel) => typeCheck(nel, ifs, errColor, pathToPack)
      }

    def typeCheck(
      inputs: NonEmptyList[Path],
      ifs: List[Package.Interface],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]
      ): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
      parseInputs(inputs, pathToPack)
        .flatMap { ins =>
          moduleIOMonad.fromTry {
            // Now we have completed all IO, here we do all the checks we need for correctness
            toTry(ins, errColor)
              .flatMap { packs =>
                val map = PackageMap.buildSourceMap(packs)
                val liftError = Lambda[FunctionK[ValidatedNel[PackageError, ?], Try]](fromPackageError(map, _, errColor))
                // TODO, we could use applicative, to report both duplicate packages and the other
                // errors
                PackageMap.typeCheckParsed(packs, ifs, "predef", liftError)(checkDuplicatePackages(_)(_._1.toString))
                  .map { p =>
                    val pathToName: List[(Path, PackageName)] =
                      packs.map { case ((path, _), p) => (path, p.name) }.toList
                    (p, pathToName)
                  }
              }
          }
        }

    def buildPackMap(
      srcs: List[Path],
      deps: List[Path],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]): IO[(PackageMap.Typed[Any], List[(Path, PackageName)])] =
      readPackages(deps)
        .flatMap { packs =>
          val ifaces = packs.map(Package.interfaceOf(_))
          typeCheck0(srcs, ifaces, errColor, pathToPack)
            .map { case (thesePacks, lst) =>
              (packs.foldLeft(PackageMap.toAnyTyped(thesePacks))(_ + _), lst)
            }
        }

    /**
     * This allows us to use either a path or packagename to select
     * the main file
     */
    sealed abstract class MainIdentifier {
      def getMain(ps: List[(Path, PackageName)]): IO[PackageName]
    }
    object MainIdentifier {
      case class FromPackage(mainPackage: PackageName) extends MainIdentifier {
        def getMain(ps: List[(Path, PackageName)]): IO[PackageName] = moduleIOMonad.pure(mainPackage)
      }
      case class FromFile(mainFile: Path) extends MainIdentifier {
        def getMain(ps: List[(Path, PackageName)]): IO[PackageName] =
          ps.collectFirst { case (path, pn) if path == mainFile => pn } match {
            case None => moduleIOMonad.raiseError(new Exception(s"could not find file $mainFile in parsed sources"))
            case Some(p) => moduleIOMonad.pure(p)
          }
      }

      def opts(pnOpts: Opts[PackageName], fileOpts: Opts[Path]): Opts[MainIdentifier] =
        pnOpts.map(FromPackage(_)).orElse(fileOpts.map(FromFile(_)))
    }

    case class Evaluate(
      inputs: List[Path],
      mainPackage: MainIdentifier,
      deps: List[Path],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]) extends MainCommand {
      def run =
        buildPackMap(inputs.toList, deps, errColor, pathToPack)
          .flatMap { case (packs, names) =>
            mainPackage
              .getMain(names)
              .flatMap { mainPackage =>
                val ev = Evaluation(packs, Predef.jvmExternals)
                ev.evaluateLast(mainPackage) match {
                  case None => moduleIOMonad.raiseError(new Exception("found no main expression"))
                  case Some((eval, tpe)) =>
                    moduleIOMonad.pure(Output.EvaluationResult(eval, tpe))
                }
              }
          }
    }

    case class ToJson(
      inputs: List[Path],
      deps: List[Path],
      mainPackage: MainIdentifier,
      output: Path,
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]) extends MainCommand {
      def checkEmpty =
        if (inputs.isEmpty && deps.isEmpty) moduleIOMonad.raiseError(new Exception("no test sources or test dependencies"))
        else moduleIOMonad.unit

      def run = checkEmpty *> buildPackMap(inputs.toList, deps, errColor, pathToPack)
        .flatMap { case (packs, files) =>
          mainPackage.getMain(files)
            .flatMap { mainPackage =>
              val ev = Evaluation(packs, Predef.jvmExternals)
              ev.evaluateLast(mainPackage) match {
                case None =>
                  moduleIOMonad.raiseError(new Exception("found no main expression"))
                case Some((eval, scheme)) =>
                  val res = eval.value
                  ev.toJson(res, scheme) match {
                    case None =>
                      moduleIOMonad.raiseError(new Exception(
                        s"cannot convert type to Json: $scheme"))
                    case Some(j) =>
                      moduleIOMonad.pure(Output.JsonOutput(j, output))
                  }
              }
            }
        }
    }
    case class TypeCheck(
      inputs: NonEmptyList[Path],
      ifaces: List[Path],
      output: Path,
      ifout: Option[Path],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]) extends MainCommand {
      def run =
        readInterfaces(ifaces)
          .flatMap(typeCheck(inputs, _, errColor, pathToPack))
          .map { case (packs, _) =>
            val packList =
              packs.toMap
                .iterator
                .map { case (_, p) => p }
                // TODO currently we recompile predef in every run, so every interface includes
                // predef, we filter that out
                .filter(_.name != PackageName.PredefName)
                .toList
                .sortBy(_.name)

            Output.CompileOut(packList, ifout, output)
          }
    }

    case class RunTests(
      tests: List[Path],
      testPacks: List[PackageName],
      dependencies: List[Path],
      errColor: Colorize,
      pathToPack: Path => Option[PackageName]) extends MainCommand {
      def run = {
        if (tests.isEmpty && dependencies.isEmpty) {
          moduleIOMonad.raiseError(new Exception("no test sources or test dependencies"))
        }
        else {
          val typeChecked =
            for {
              deps <- readPackages(dependencies)
              ifaces = deps.map(Package.interfaceOf(_))
              pn <- typeCheck0(tests, ifaces, errColor, pathToPack)
              (packs0, nameMap) = pn
              // add the dependencies into the package map
              packs = deps.foldLeft(PackageMap.toAnyTyped(packs0))(_ + _)
            } yield (packs, nameMap)

          typeChecked.map { case (packs, nameMap) =>
            val testSet = tests.toSet
            val testPackages: List[PackageName] =
              (nameMap.iterator.collect { case (p, name) if testSet(p) => name } ++
                testPacks.iterator).toList.sorted.distinct
            val ev = Evaluation(packs, Predef.jvmExternals)
            Output.TestOutput(testPackages.map { p => (p, ev.evalTest(p)) })
          }
        }
      }
    }

    def errors(msgs: List[String]): Try[Nothing] =
      Failure(new Exception(msgs.mkString("\n######\n")))

    def toTry[A](v: ValidatedNel[ParseError, A], color: Colorize): Try[A] =
      v match {
        case Validated.Valid(a) => Success(a)
        case Validated.Invalid(errs) =>
          val msgs = errs.toList.flatMap {
            case ParseError.PartialParse(pp, path) =>
              // we should never be partial here
              val (r, c) = pp.locations.toLineCol(pp.position).get
              val ctx = pp.locations.showContext(pp.position, 2, color).get
              List(s"failed to parse completely $path at line ${r + 1}, column ${c + 1}",
                  ctx.render(80))
            case ParseError.ParseFailure(pf, path) =>
              // we should never be partial here
              val (r, c) = pf.locations.toLineCol(pf.position).get
              val ctx = pf.locations.showContext(pf.position, 2, color).get
              List(s"failed to parse $path at line ${r + 1}, column ${c + 1}",
                  ctx.render(80))
            case ParseError.FileError(path, err) =>
              List(s"failed to parse $path",
                  err.getMessage,
                  err.getClass.toString)
          }
        errors(msgs)
      }

    def fromPackageError[A](
      sourceMap: Map[PackageName, (LocationMap, String)],
      v: ValidatedNel[PackageError, A],
      errColor: Colorize): Try[A] =
        v match {
          case Validated.Invalid(errs) => errors(errs.map(_.message(sourceMap, errColor)).toList)
          case Validated.Valid(a) => Success(a)
        }

    def checkDuplicatePackages[A](dups: Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])])(fn: A => String): Try[Unit] =
      if (dups.isEmpty) Success(())
      else
        errors(
          dups.iterator.map { case (pname, ((src, _), nelist)) =>
            val dupsrcs = (fn(src) :: nelist.map { case (s, _) => fn(s) }.toList).sorted.mkString(", ")
            s"package ${pname.asString} duplicated in $dupsrcs"
          }.toList
        )

    val opts: Opts[MainCommand] = {
      implicit val argPack: Argument[PackageName] =
        new Argument[PackageName] {
          def defaultMetavar: String = "packageName"
          def read(string: String): ValidatedNel[String, PackageName] =
            PackageName.parse(string) match {
              case Some(pn) => Validated.valid(pn)
              case None => Validated.invalidNel(s"could not parse $string as a package name. Must be capitalized strings separated by /")
            }
        }

      def toList[A](neo: Opts[NonEmptyList[A]]): Opts[List[A]] = {
        neo.orNone.map {
          case None => Nil
          case Some(ne) => ne.toList
        }
      }

      implicit val argColor: Argument[Colorize] =
        new Argument[Colorize] {
          def defaultMetavar: String = "color"
          def read(str: String): ValidatedNel[String, Colorize] =
            str.toLowerCase match {
              case "none" => Validated.valid(Colorize.none)
              case "ansi" => Validated.valid(Colorize.Console.red)
              case "html" => Validated.valid(Colorize.HmtlFont.red)
              case other => Validated.invalidNel(s"unknown colorize: $other, expected: none, ansi or html")
            }
        }

      val ins = Opts.options[Path]("input", help = "input files")
      val ifaces = toList(Opts.options[Path]("interface", help = "interface files"))
      val includes = toList(Opts.options[Path]("include", help = "compiled packages to include files"))

      val colorOpt = Opts.option[Colorize]("color", help = "colorize mode: none, ansi or html")
        .orElse(Opts(Colorize.Console.red))

      val mainP =
          MainIdentifier.opts(
            Opts.option[PackageName]("main", help = "main package to evaluate"),
            Opts.option[Path]("main_file", help = "file containing the main package to evaluate"))
      val testP = toList(Opts.options[PackageName]("test_package", help = "package for which to run tests"))
      val outputPath = Opts.option[Path]("output", help = "output path")
      val interfaceOutputPath = Opts.option[Path]("interface_out", help = "interface output path")

      val pathToPack: Opts[Path => Option[PackageName]] =
        toList(
          Opts.options[Path](
            "package_root",
            help = "for implicit package names, consider these paths as roots"))
          .map { paths =>
            { p: Path => pathPackage(paths, p) }
          }

      val evalOpt = (toList(ins), mainP, includes, colorOpt, pathToPack)
        .mapN(Evaluate(_, _, _, _, _))
      val toJsonOpt = (toList(ins), includes, mainP, outputPath, colorOpt, pathToPack)
        .mapN(ToJson(_, _, _, _, _, _))
      val typeCheckOpt = (ins, ifaces, outputPath, interfaceOutputPath.orNone, colorOpt, pathToPack)
        .mapN(TypeCheck(_, _, _, _, _, _))
      val testOpt = (toList(ins), testP, includes, colorOpt, pathToPack)
        .mapN(RunTests(_, _, _, _, _))

      Opts.subcommand("eval", "evaluate an expression and print the output")(evalOpt)
        .orElse(Opts.subcommand("write-json", "evaluate a data expression into json")(toJsonOpt))
        .orElse(Opts.subcommand("type-check", "type check a set of packages")(typeCheckOpt))
        .orElse(Opts.subcommand("test", "test a set of bosatsu modules")(testOpt))
    }

    def command: Command[MainCommand] =
      Command("bosatsu", "a total and functional programming language")(opts)
  }

}
