package org.bykn.bosatsu

import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.effect.IO
import cats.Traverse
import com.monovore.decline.{Argument, Command, Opts}
import fastparse.all.P
import java.nio.file.{Files, Path}
import org.typelevel.paiges.Doc
import scala.util.{ Failure, Success, Try }

import cats.implicits._

sealed abstract class MainCommand {
  def run: IO[Unit]
}

object MainCommand {
  def parseInputs[F[_]: Traverse](paths: F[Path]): IO[ValidatedNel[ParseError, F[((Path, LocationMap), Package.Parsed)]]] =
    IO(paths.traverse { path =>
      parseFile(Package.parser, path).map { case (lm, parsed) =>
        ((path, lm), parsed)
      }
    })

  sealed trait ParseError {
    def showContext: Option[String] =
      this match {
        case ParseError.PartialParse(err, _) =>
          err.showContext
        case ParseError.ParseFailure(err, _) =>
          err.showContext
        case ParseError.FileError(_, _) =>
          None
      }
  }

  object ParseError {
     case class PartialParse[A](error: Parser.Error.PartialParse[A], path: Path) extends ParseError
     case class ParseFailure(error: Parser.Error.ParseFailure, path: Path) extends ParseError
     case class FileError(readPath: Path, error: Throwable) extends ParseError
  }

  // This is side effecting, so should only be called inside an IO
  private def parseFile[A](p: P[A], path: Path): ValidatedNel[ParseError, (LocationMap, A)] =
    Try(new String(Files.readAllBytes(path), "utf-8")) match {
      case Success(str) => Parser.parse(p, str).leftMap { nel =>
        nel.map {
          case pp@Parser.Error.PartialParse(_, _, _) => ParseError.PartialParse(pp, path)
          case pf@Parser.Error.ParseFailure(_, _) => ParseError.ParseFailure(pf, path)
        }
      }
      case Failure(err) => Validated.invalidNel(ParseError.FileError(path, err))
    }

  /**
   * like typecheck, but a no-op for empty lists
   */
  def typeCheck0(inputs: List[Path], ifs: List[Package.Interface]): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
    NonEmptyList.fromList(inputs) match {
      case None =>
        // we should still return the predef
        // if it is not in ifs
        val useInternalPredef =
          !ifs.contains { p: Package.Interface => p.name == Predef.Name }

        if (useInternalPredef) {
          IO.pure((PackageMap.fromIterable(Predef.predefCompiled :: Nil), Nil))
        }
        else {
          IO.pure((PackageMap.empty, Nil))
        }
      case Some(nel) => typeCheck(nel, ifs)
    }

  def typeCheck(inputs: NonEmptyList[Path], ifs: List[Package.Interface]): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
    parseInputs(inputs)
      .flatMap { ins =>
        // if we have passed in a use supplied predef, don't use the internal one
        val useInternalPredef = !ifs.contains { p: Package.Interface => p.name == Predef.Name }
        IO.fromTry {
          // Now we have completed all IO, here we do all the checks we need for correctness
          for {
            packs <- toTry(ins)
            parsed =
              if (useInternalPredef) Predef.withPredefA(("predef", LocationMap("")), packs.toList)
              else Predef.withPredefImportsA(packs.toList)
            (dups, resPacks) = PackageMap.resolveThenInfer(parsed, ifs)
            _ <- checkDuplicatePackages(dups)(_._1.toString)
            map = PackageMap.buildSourceMap(packs)
            p <- fromPackageError(map, resPacks)
            pathToName: List[(Path, PackageName)] = packs.map { case ((path, _), p) => (path, p.name) }.toList
          } yield (p, pathToName)
        }
      }

  def buildPackMap(srcs: List[Path], deps: List[Path]): IO[PackageMap.Typed[Any]] =
    ProtoConverter
      .readPackages(deps)
      .flatMap { packs =>
        val ifaces = packs.map(Package.interfaceOf(_))
        typeCheck0(srcs, ifaces)
          .map { case (thesePacks, _) =>
            packs.foldLeft(PackageMap.toAnyTyped(thesePacks))(_ + _)
          }
      }

  case class Evaluate(inputs: List[Path], mainPackage: PackageName, deps: List[Path]) extends MainCommand {
    def run =
      buildPackMap(inputs.toList, deps)
        .flatMap { packs =>
          val ev = Evaluation(packs, Predef.jvmExternals)
          ev.evaluateLast(mainPackage) match {
            case None => IO.raiseError(new Exception("found no main expression"))
            case Some((eval, scheme)) =>
              IO {
                val res = eval.value
                println(s"$res: $scheme")
              }
          }
        }
  }
  case class ToJson(inputs: List[Path], deps: List[Path], mainPackage: PackageName, output: Path) extends MainCommand {
    def checkEmpty =
      if (inputs.isEmpty && deps.isEmpty) IO.raiseError(new Exception("no test sources or test dependencies"))
      else IO.unit

    def run = checkEmpty *> buildPackMap(inputs.toList, deps)
      .flatMap { packs =>
        val ev = Evaluation(packs, Predef.jvmExternals)
        ev.evaluateLast(mainPackage) match {
          case None =>
            IO.raiseError(new Exception("found no main expression"))
          case Some((eval, scheme)) =>
            val res = eval.value
            ev.toJson(res, scheme) match {
              case None =>
                IO.raiseError(new Exception(
                  s"cannot convert type to Json: $scheme"))
              case Some(j) =>
                CodeGenWrite.writeDoc(output, j.toDoc)
            }
        }
      }
  }
  case class TypeCheck(inputs: NonEmptyList[Path], ifaces: List[Path], output: Path, ifout: Option[Path]) extends MainCommand {
    def run =
      ProtoConverter
        .readInterfaces(ifaces)
        .flatMap(typeCheck(inputs, _))
        .flatMap { case (packs, _) =>
          val packList =
            packs.toMap
              .iterator
              .map { case (_, p) => p }
              // TODO currently we recompile predef in every run, so every interface includes
              // predef, we filter that out
              .filter(_.name != Predef.packageName)
              .toList
              .sortBy(_.name)

          val ifres = ifout match {
            case None =>
              IO.unit
            case Some(ifacePath) =>
              val ifs = packList.map(Package.interfaceOf(_))
              ProtoConverter.writeInterfaces(ifs, ifacePath)
          }
          val out = ProtoConverter.writePackages(packList, output)

          ifres *> out
      }
  }

  case class Compile(inputs: NonEmptyList[Path], compileRoot: Path) extends MainCommand {
    def run =
      typeCheck(inputs, Nil).flatMap { case (packs, _) =>
        CodeGenWrite.write(compileRoot, packs, Predef.jvmExternals) *>
          IO(println(s"wrote ${packs.toMap.size} packages"))
      }
  }

  case class RunTests(tests: List[Path], testPacks: List[PackageName], dependencies: List[Path]) extends MainCommand {
    def run = {
      if (tests.isEmpty && dependencies.isEmpty) {
        IO.raiseError(new Exception("no test sources or test dependencies"))
      }
      else {
        val typeChecked =
          for {
            deps <- ProtoConverter.readPackages(dependencies)
            ifaces = deps.map(Package.interfaceOf(_))
            pn <- typeCheck0(tests, ifaces)
            (packs0, nameMap) = pn
            // add the dependencies into the package map
            packs = deps.foldLeft(PackageMap.toAnyTyped(packs0))(_ + _)
          } yield (packs, nameMap)

        typeChecked.flatMap { case (packs, nameMap) =>
          val testSet = tests.toSet
          val testPackages: List[PackageName] =
            (nameMap.iterator.collect { case (p, name) if testSet(p) => name } ++
              testPacks.iterator).toList.sorted.distinct
          val ev = Evaluation(packs, Predef.jvmExternals)
          val resMap = testPackages.map { p => (p, ev.evalTest(p)) }
          val noTests = resMap.collect { case (p, None) => p }.toList
          val results = resMap.collect { case (p, Some(t)) => (p, Test.report(t)) }.toList.sortBy(_._1)

          val failures = results.iterator.map { case (_, (_, f, _)) => f }.sum
          val success = noTests.isEmpty && (failures == 0)
          val docRes: Doc =
            Doc.intercalate(Doc.line,
              results.map { case (p, (_, _, d)) =>
                Doc.text(p.asString) + Doc.char(':') + (Doc.lineOrSpace + d).nested(2)
              })

          if (success) IO(println(docRes.render(80)))
          else {
            val missingDoc =
              if (noTests.isEmpty) Nil
              else {
                val prefix = Doc.text("packages with missing tests: ")
                val missingDoc = Doc.intercalate(Doc.lineOrSpace, noTests.sorted.map { p => Doc.text(p.asString) })
                (prefix + missingDoc.nested(2)) :: Nil
              }

            val fullOut = Doc.intercalate(Doc.line + Doc.text("#######") + Doc.line, docRes :: missingDoc)

            val failureStr =
              if (failures == 1) "1 test failure"
              else s"$failures test failures"

            val missingCount = noTests.size
            val excepMessage =
              if (missingCount > 0) {
                val packString = if (missingCount == 1) "package" else "packages"
                s"$failureStr and $missingCount $packString with no tests found"
              }
              else failureStr

            IO(println(fullOut.render(80))) *> IO.raiseError(new Exception(excepMessage))
          }
        }
      }
    }
  }

  def errors(msgs: List[String]): Try[Nothing] =
    Failure(new Exception(msgs.mkString("\n######\n")))

  def toTry[A](v: ValidatedNel[ParseError, A]): Try[A] =
    v match {
      case Validated.Valid(a) => Success(a)
      case Validated.Invalid(errs) =>
        val msgs = errs.toList.flatMap {
          case ParseError.PartialParse(pp, path) =>
            // we should never be partial here
            val (r, c) = pp.locations.toLineCol(pp.position).get
            val ctx = pp.locations.showContext(pp.position).get
            List(s"failed to parse completely $path at line ${r + 1}, column ${c + 1}",
                ctx.toString)
          case ParseError.ParseFailure(pf, path) =>
            // we should never be partial here
            val (r, c) = pf.locations.toLineCol(pf.position).get
            val ctx = pf.locations.showContext(pf.position).get
            List(s"failed to parse $path at line ${r + 1}, column ${c + 1}",
                ctx.toString)
          case ParseError.FileError(path, err) =>
            List(s"failed to parse $path",
                err.getMessage,
                err.getClass.toString)
        }
      errors(msgs)
    }

  def fromPackageError[A](sourceMap: Map[PackageName, (LocationMap, String)], v: ValidatedNel[PackageError, A]): Try[A] =
    v match {
      case Validated.Invalid(errs) => errors(errs.map(_.message(sourceMap)).toList)
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

    val ins = Opts.options[Path]("input", help = "input files")
    val ifaces = toList(Opts.options[Path]("interface", help = "interface files"))
    val includes = toList(Opts.options[Path]("include", help = "compiled packages to include files"))

    val mainP = Opts.option[PackageName]("main", help = "main package to evaluate")
    val testP = toList(Opts.options[PackageName]("test_package", help = "package for which to run tests"))
    val outputPath = Opts.option[Path]("output", help = "output path")
    val interfaceOutputPath = Opts.option[Path]("interface_out", help = "interface output path")
    val compileRoot = Opts.option[Path]("compile_root", help = "root directory to write java output")

    val evalOpt = (toList(ins), mainP, includes).mapN(Evaluate(_, _, _))
    val toJsonOpt = (toList(ins), includes, mainP, outputPath).mapN(ToJson(_, _, _, _))
    val typeCheckOpt = (ins, ifaces, outputPath, interfaceOutputPath.orNone).mapN(TypeCheck(_, _, _, _))
    val compileOpt = (ins, compileRoot).mapN(Compile(_, _))
    val testOpt = (toList(ins), testP, includes).mapN(RunTests(_, _, _))

    Opts.subcommand("eval", "evaluate an expression and print the output")(evalOpt)
      .orElse(Opts.subcommand("write-json", "evaluate a data expression into json")(toJsonOpt))
      .orElse(Opts.subcommand("type-check", "type check a set of packages")(typeCheckOpt))
      .orElse(Opts.subcommand("compile", "compile bosatsu to Java code")(compileOpt))
      .orElse(Opts.subcommand("test", "test a set of bosatsu modules")(testOpt))
  }
}

object Main {
  def command: Command[MainCommand] =
    Command("bosatsu", "a total and functional programming language")(MainCommand.opts)

  def main(args: Array[String]): Unit =
    command.parse(args.toList) match {
      case Right(cmd) =>
        Try(cmd.run.unsafeRunSync) match {
          case Failure(err) =>
            // TODO use some verbosity flag to modulate this
            //err.printStackTrace
            System.err.println(err.getMessage)
            System.exit(1)
          case Success(()) =>
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
