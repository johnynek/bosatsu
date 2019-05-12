package org.bykn.bosatsu

import cats.{Eval, Traverse}
import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.implicits._
import com.monovore.decline._
import fastparse.all._
import java.nio.file.{Files, Path}
import org.typelevel.paiges.Doc
import scala.util.Try
import scala.util.{ Failure, Success, Try }

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

sealed abstract class MainResult[+A] {
  def map[B](fn: A => B): MainResult[B]
  def flatMap[B](fn: A => MainResult[B]): MainResult[B]
}
object MainResult {
  case class Error(code: Int, errLines: List[String], stdOut: List[String] = Nil) extends MainResult[Nothing] {
    def map[B](fn: Nothing => B) = this
    def flatMap[B](fn: Nothing => MainResult[B]) = this
  }
  case class Success[A](result: A) extends MainResult[A] {
    def map[B](fn: A => B) = Success(fn(result))
    def flatMap[B](fn: A => MainResult[B]) = fn(result)
  }

  def fromTry[A](t: Try[A]): MainResult[A] =
    t match {
      case scala.util.Success(a) =>
        MainResult.Success(a)
      case scala.util.Failure(err) =>
        MainResult.Error(1, List(err.toString, err.getMessage))
    }

  def product[A, B](a: MainResult[A], b: MainResult[B])(errs: (Int, Int) => Int): MainResult[(A, B)] =
    (a, b) match {
      case (Error(a, la, sa), Error(b, lb, sb)) => Error(errs(a, b), la ::: lb, sa ::: sb)
      case (e@Error(_, _, _), _) => e
      case (_, e@Error(_, _, _)) => e
      case (Success(a), Success(b)) => Success((a, b))
    }
}

sealed abstract class MainCommand {
  def run(): MainResult[List[String]]
}

object MainCommand {
  def parseInputs[F[_]: Traverse](paths: F[Path]): ValidatedNel[ParseError, F[((Path, LocationMap), Package.Parsed)]] =
    paths.traverse { path =>
      parseFile(Package.parser, path).map { case (lm, parsed) =>
        ((path, lm), parsed)
      }
    }

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

  def parseFile[A](p: P[A], path: Path): ValidatedNel[ParseError, (LocationMap, A)] =
    Try(new String(Files.readAllBytes(path), "utf-8")) match {
      case Success(str) => Parser.parse(p, str).leftMap { nel =>
        nel.map {
          case pp@Parser.Error.PartialParse(_, _, _) => ParseError.PartialParse(pp, path)
          case pf@Parser.Error.ParseFailure(_, _) => ParseError.ParseFailure(pf, path)
        }
      }
      case Failure(err) => Validated.invalidNel(ParseError.FileError(path, err))
    }

  def typeCheck(inputs: NonEmptyList[Path]): MainResult[(PackageMap.Inferred, List[(Path, PackageName)])] = {
    val ins = parseInputs(inputs)

    toResult(ins)
      .flatMap { packs =>
        val pathToName: List[(Path, PackageName)] = packs.map { case ((path, _), p) => (path, p.name) }.toList
        val (dups, resPacks) = PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), packs.toList))
        val checkD = checkDuplicatePackages(dups)(_._1.toString)
        val map = PackageMap.buildSourceMap(packs)
        val checkPacks = fromPackageError(map, resPacks)
        MainResult.product(checkD, checkPacks)(_ max _)
          .map { case (_, p) => (p, pathToName) }
      }
  }

  case class Evaluate(inputs: NonEmptyList[Path], mainPackage: PackageName) extends MainCommand {
    def run() =
      typeCheck(inputs).flatMap { case (packs, _) =>
        val ev = Evaluation(packs, Predef.jvmExternals)
        ev.evaluateLast(mainPackage) match {
          case None => MainResult.Error(1, List("found no main expression"))
          case Some((eval, scheme)) =>
            val res = eval.value
            MainResult.Success(List(s"$res: $scheme"))
        }
      }
  }
  case class ToJson(inputs: NonEmptyList[Path], signatures: List[Path], mainPackage: PackageName, output: Path) extends MainCommand {
    def run() =
      typeCheck(inputs).flatMap { case (packs, _) =>
        val ev = Evaluation(packs, Predef.jvmExternals)
        ev.evaluateLast(mainPackage) match {
          case None => MainResult.Error(1, List("found no main expression"))
          case Some((eval, scheme)) =>
            val res = eval.value
            ev.toJson(res, scheme) match {
              case None =>
                MainResult.Error(1, List(s"cannot convert type to Json: $scheme"))
              case Some(j) =>
                MainResult.fromTry(CodeGenWrite.writeDoc(output, j.toDoc))
                  .map { _ => Nil }
            }
        }
      }
  }
  case class TypeCheck(inputs: NonEmptyList[Path], signatures: List[Path], output: Path) extends MainCommand {
    def run() =
      typeCheck(inputs).flatMap { case (packs, _) =>
        MainResult.fromTry(CodeGenWrite.writeDoc(output, Doc.text(s"checked ${packs.toMap.size} packages")))
          .map(_ => Nil)
      }
  }
  case class Compile(inputs: NonEmptyList[Path], compileRoot: Path) extends MainCommand {
    def run() =
      typeCheck(inputs).flatMap { case (packs, _) =>
        MainResult.fromTry(CodeGenWrite.write(compileRoot, packs, Predef.jvmExternals))
          .map { _ => List(s"wrote ${packs.toMap.size} packages") }
      }
  }

  case class RunTests(tests: List[Path], testPacks: List[PackageName], dependencies: List[Path]) extends MainCommand {
    def run() = {
      val files = NonEmptyList.fromList(tests ::: dependencies) match {
        case None =>
          MainResult.Error(1, "no test sources or test dependencies" :: Nil, Nil)
        case Some(ne) => MainResult.Success(ne)
      }

      files.flatMap(typeCheck(_)).flatMap { case (packs, nameMap) =>
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
        if (success) MainResult.Success(stdOut)
        else {
          val missingDoc =
            if (noTests.isEmpty) Doc.empty
            else {
              val prefix = Doc.text("packages with missing tests: ")
              val missingDoc = Doc.intercalate(Doc.lineOrSpace, noTests.sorted.map { p => Doc.text(p.asString) })
              (prefix + missingDoc.nested(2))
            }

          MainResult.Error(1, missingDoc.render(80) :: Nil, stdOut)
        }
      }
    }
  }

  def toResult[A](v: ValidatedNel[ParseError, A]): MainResult[A] =
    v match {
      case Validated.Valid(a) => MainResult.Success(a)
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
      MainResult.Error(1, msgs)
    }

  def fromPackageError[A](sourceMap: Map[PackageName, (LocationMap, String)], v: ValidatedNel[PackageError, A]): MainResult[A] =
    v match {
      case Validated.Invalid(errs) => MainResult.Error(1, errs.map(_.message(sourceMap)).toList)
      case Validated.Valid(a) => MainResult.Success(a)
    }

  def checkDuplicatePackages[A](dups: Map[PackageName, ((A, Package.Parsed), NonEmptyList[(A, Package.Parsed)])])(fn: A => String): MainResult[Unit] =
    if (dups.isEmpty) MainResult.Success(())
    else
      MainResult.Error(1,
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
    val sigs = toList(Opts.options[Path]("signature", help = "signature files"))

    val mainP = Opts.option[PackageName]("main", help = "main package to evaluate")
    val testP = toList(Opts.options[PackageName]("test_package", help = "package for which to run tests"))
    val outputPath = Opts.option[Path]("output", help = "output path")
    val compileRoot = Opts.option[Path]("compile_root", help = "root directory to write java output")

    val evalOpt = (ins, mainP).mapN(Evaluate(_, _))
    val toJsonOpt = (ins, sigs, mainP, outputPath).mapN(ToJson(_, _, _, _))
    val typeCheckOpt = (ins, sigs, outputPath).mapN(TypeCheck(_, _, _))
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
        cmd.run() match {
          case MainResult.Error(code, errs, stdout) =>
            errs.foreach(System.err.println)
            stdout.foreach(println)
            System.exit(code)
          case MainResult.Success(lines) =>
            lines.foreach(println)
            System.exit(0)
        }
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
