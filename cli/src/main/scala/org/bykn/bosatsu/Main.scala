package org.bykn.bosatsu

import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.effect.IO
import cats.{Eval, Traverse}
import com.monovore.decline.{Argument, Command, Opts}
import fastparse.all.P
import java.nio.file.{Files, Path}
import org.typelevel.paiges.Doc
import scala.util.{ Failure, Success, Try }

import cats.implicits._

object Foo {
  def times(i: java.lang.Integer): java.lang.Integer =
    java.lang.Integer.valueOf(i.intValue + 42)
}

object Std {
  //fold = ffi scala org.bykn.bosatsu.Std.fold List[a] -> b -> (b -> a -> b) -> b
  @annotation.tailrec
  final def fold(list: Any, bv: Any, fn: Any): Any = {
    list match {
      case (0, _) =>
        // Empty
        bv
      case (1, head :: tail :: Nil) =>
        val fnT = fn.asInstanceOf[Fn[Any, Fn[Any, Any]]]
        fold(tail, fnT(bv)(head), fn)
      case _ => sys.error(s"unexpected: $list")
    }
  }

  def unitValue: AnyRef = (0, Nil)

  def print(i: Any): Any =
    Eval.always { println(i.toString); unitValue }

  def flatMap(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].flatMap { v =>
      fn.asInstanceOf[Fn[Any, Eval[Any]]](v)
    }
  def mapAction(act: Any, fn: Any): Any =
    act.asInstanceOf[Eval[Any]].map { v =>
      fn.asInstanceOf[Fn[Any, Any]](v)
    }

  def toAction(a: Any): Any = Eval.now(a)
  def runAction(a: Any): Any = a.asInstanceOf[Eval[Any]].value
}

sealed abstract class MainCommand {
  def run: IO[List[String]]
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

  def typeCheck(inputs: NonEmptyList[Path], ifacePaths: List[Path]): IO[(PackageMap.Inferred, List[(Path, PackageName)])] =
    for {
      ins <- parseInputs(inputs)
      ifs <- ProtoConverter.readInterfaces(ifacePaths)
      res <- IO.fromTry(
        // Now we have completed all IO, here we do all the checks we need for correctness
        for {
          packs <- toTry(ins)
          (dups, resPacks) = PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), packs.toList), ifs)
          _ <- checkDuplicatePackages(dups)(_._1.toString)
          map = PackageMap.buildSourceMap(packs)
          p <- fromPackageError(map, resPacks)
          pathToName: List[(Path, PackageName)] = packs.map { case ((path, _), p) => (path, p.name) }.toList
        } yield (p, pathToName))
    } yield res

  case class Evaluate(inputs: NonEmptyList[Path], mainPackage: PackageName) extends MainCommand {
    def run =
      typeCheck(inputs, Nil).flatMap { case (packs, _) =>
        val ev = Evaluation(packs, Predef.jvmExternals)
        ev.evaluateLast(mainPackage) match {
          case None => IO.raiseError(new Exception("found no main expression"))
          case Some((eval, scheme)) =>
            val res = eval.value
            IO.pure(List(s"$res: $scheme"))
        }
      }
  }
  case class ToJson(inputs: NonEmptyList[Path], ifaces: List[Path], mainPackage: PackageName, output: Path) extends MainCommand {
    def run =
      typeCheck(inputs, Nil).flatMap { case (packs, _) =>
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
                CodeGenWrite.writeDoc(output, j.toDoc).as(Nil)
            }
        }
      }
  }
  case class TypeCheck(inputs: NonEmptyList[Path], ifaces: List[Path], output: Path, ifout: Option[Path]) extends MainCommand {
    def run =
      typeCheck(inputs, ifaces).flatMap { case (packs, _) =>
        val ifres = ifout match {
          case None =>
            IO.unit
          case Some(p) =>
            val ifs0 = packs.toMap.iterator.map { case (_, p) => Package.interfaceOf(p) }.toList
            // TODO currently we recompile predef in every run, so every interface includes
            // predef, we filter that out
            val ifs = ifs0.filterNot(_.name == Predef.packageName)
            ProtoConverter.writeInterfaces(ifs, p)
        }

        val out =
          CodeGenWrite.writeDoc(output, Doc.text(s"checked ${packs.toMap.size} packages"))
            .as(Nil)

        ifres *> out
      }
  }

  case class Compile(inputs: NonEmptyList[Path], compileRoot: Path) extends MainCommand {
    def run =
      typeCheck(inputs, Nil).flatMap { case (packs, _) =>
        CodeGenWrite.write(compileRoot, packs, Predef.jvmExternals)
          .as(List(s"wrote ${packs.toMap.size} packages"))
      }
  }

  case class RunTests(tests: List[Path], testPacks: List[PackageName], dependencies: List[Path]) extends MainCommand {
    def run = {
      val files = NonEmptyList.fromList(tests ::: dependencies) match {
        case None =>
          IO.raiseError(new Exception("no test sources or test dependencies"))
        case Some(ne) => IO.pure(ne)
      }

      files.flatMap(typeCheck(_, Nil)).flatMap { case (packs, nameMap) =>
        val testSet = tests.toList.toSet
        val testPackages: List[PackageName] =
          (nameMap.iterator.collect { case (p, name) if testSet(p) => name } ++
            testPacks.iterator).toList.sorted.distinct
        val ev = Evaluation(packs, Predef.jvmExternals)
        val resMap = testPackages.map { p => (p, ev.evalTest(p)) }
        val noTests = resMap.collect { case (p, None) => p }.toList
        val results = resMap.collect { case (p, Some(t)) => (p, Test.report(t)) }.toList.sortBy(_._1)

        val success = noTests.isEmpty && results.forall { case (_, (_, f, _)) => f == 0 }
        def stdOut: List[String] =
          results.map { case (p, (_, _, d)) =>
            val res = Doc.text(p.asString) + Doc.char(':') + (Doc.lineOrSpace + d).nested(2)
            res.render(80)
          }
        if (success) IO.pure(stdOut)
        else {
          val missingDoc =
            if (noTests.isEmpty) Doc.empty
            else {
              val prefix = Doc.text("packages with missing tests: ")
              val missingDoc = Doc.intercalate(Doc.lineOrSpace, noTests.sorted.map { p => Doc.text(p.asString) })
              (prefix + missingDoc.nested(2))
            }

          IO.raiseError(new Exception(missingDoc.render(80)))
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
    val deps = toList(Opts.options[Path]("test_deps", help = "test dependencies"))
    val ifaces = toList(Opts.options[Path]("interface", help = "interface files"))

    val mainP = Opts.option[PackageName]("main", help = "main package to evaluate")
    val testP = toList(Opts.options[PackageName]("test_package", help = "package for which to run tests"))
    val outputPath = Opts.option[Path]("output", help = "output path")
    val interfaceOutputPath = Opts.option[Path]("interface_out", help = "interface output path")
    val compileRoot = Opts.option[Path]("compile_root", help = "root directory to write java output")

    val evalOpt = (ins, mainP).mapN(Evaluate(_, _))
    val toJsonOpt = (ins, ifaces, mainP, outputPath).mapN(ToJson(_, _, _, _))
    val typeCheckOpt = (ins, ifaces, outputPath, interfaceOutputPath.orNone).mapN(TypeCheck(_, _, _, _))
    val compileOpt = (ins, compileRoot).mapN(Compile(_, _))
    val testOpt = (toList(ins), testP, deps).mapN(RunTests(_, _, _))

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
            System.err.println(err.getMessage)
            System.exit(1)
          case Success(lines) =>
            lines.foreach(println)
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
