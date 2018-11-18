package org.bykn.bosatsu

import better.files.{File, FileMonitor}
import cats.{Eval, Id}
import cats.data.{Validated, ValidatedNel, NonEmptyList}
import cats.implicits._
import com.monovore.decline._
import java.nio.file.Path
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import org.typelevel.paiges.Doc
import scala.util.Try
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import org.scalatra.ScalatraServlet


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

  def waitReturn(a: Any): Any = {
    println(s"a waitReturn ${a.toString}")
    a
  }

  import java.net.Socket
  import java.io.OutputStreamWriter

  def expensiveFunction(a: Any, b: Any): Any = {
    val socket = new Socket("127.0.0.1", 5555)
    val osw = new OutputStreamWriter(socket.getOutputStream(), "UTF-8")
    val str = s"logging a: '${a.toString}' b: '${b.toString}'\n"
    osw.write(str, 0, str.length)
    osw.flush()
    socket.close()
    a
  }
}

sealed abstract class MainResult[+A] {
  def map[B](fn: A => B): MainResult[B]
  def flatMap[B](fn: A => MainResult[B]): MainResult[B]
  def intermediate: Boolean
}
object MainResult {
  case class Error(code: Int, errLines: List[String], stdOut: List[String] = Nil, intermediate: Boolean = false) extends MainResult[Nothing] {
    def map[B](fn: Nothing => B) = this
    def flatMap[B](fn: Nothing => MainResult[B]) = this
  }
  case class Success[A](result: A, intermediate: Boolean = false, cache: Map[NormalExpression, (Eval[Any], Scheme)] = Map()) extends MainResult[A] {
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
      case (Error(a, la, sa, ia), Error(b, lb, sb, ib)) => Error(errs(a, b), la ::: lb, sa ::: sb, ia && ib)
      case (e@Error(_, _, _, _), _) => e
      case (_, e@Error(_, _, _, _)) => e
      case (Success(a, ia, ca), Success(b, ib, cb)) => Success((a, b), ia && ib, ca ++ cb)
    }
}

sealed abstract class MainCommand {
  def run(): LinkedBlockingQueue[MainResult[List[String]]]
}

object JettyLauncher { // this is my entry object as specified in sbt project definition
  def startServer() : Unit = {
    val server = new Server(8080)
    val context = new WebAppContext()
    context setContextPath "/"
    context.setResourceBase("src/main/webapp")
    context.addEventListener(new ScalatraListener)
    context.addServlet(classOf[DefaultServlet], "/")

    server.setHandler(context)

    server.start
    // server.join
  }
}

class ReactiveBosatsuServlet extends ScalatraServlet {

  get("/") {
    "hello!"
  }

}

object MainCommand {

  def typeCheck(inputs: NonEmptyList[Path], externals: List[Path]): MainResult[(Externals, PackageMap.Inferred, List[(Path, PackageName)])] = {
    val ins = parseInputs(inputs)
    val exts = readExternals(externals)

    toResult(ins.product(exts))
      .flatMap { case (packs, exts) =>
        val pathToName: List[(Path, PackageName)] = packs.map { case ((path, _), p) => (path, p.name) }.toList
        val (dups, resPacks) = PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), packs.toList))
        val checkD = checkDuplicatePackages(dups)(_._1.toString)
        val map = packs.map { case ((path, lm), pack) => (pack.name, (lm, path.toString)) }.toList.toMap
        val checkPacks = fromPackageError(map, resPacks)
        MainResult.product(checkD, checkPacks)(_ max _)
          .map { case (_, p) => (exts, p, pathToName) }
      }
  }

  def blockingQueue(mainResult: MainResult[List[String]]) = {
    val bq: LinkedBlockingQueue[MainResult[List[String]]] = new LinkedBlockingQueue()
    bq.put(mainResult)
    bq
  }

  case class Evaluate(inputs: NonEmptyList[Path], externals: List[Path], mainPackage: PackageName, cache: Map[NormalExpression, (Eval[Any], Scheme)] = Map()) extends MainCommand {
    def run() = {
      blockingQueue(typeCheck(inputs, externals).flatMap { case (ext, packs, _) =>
        val ev = Evaluation(packs, Predef.jvmExternals ++ ext)
        ev.evaluateLast(mainPackage, cache) match {
          case None => MainResult.Error(1, List("found no main expression"))
          case Some((eval, scheme, ne, cache)) =>
            val res = eval.value
            MainResult.Success(List(s"res: ($res)", s"scheme: $scheme", s"expression: $ne"), false, cache)
        }
      })
    }
  }
  case class Revaluate(inputs: NonEmptyList[Path], externals: List[Path], mainPackage: PackageName) extends MainCommand {

    val cache: LinkedBlockingQueue[Map[NormalExpression, (Eval[Any], Scheme)]] = new LinkedBlockingQueue()
    JettyLauncher.startServer()

    def combinedCache: Map[NormalExpression, (Eval[Any], Scheme)] = cache.toArray
      .map(_.asInstanceOf[Map[NormalExpression, (Eval[Any], Scheme)]])
      .foldLeft(Map[NormalExpression, (Eval[Any], Scheme)]())(_ ++ _)

    def result = Evaluate(inputs, externals, mainPackage, combinedCache).run.take match {
      case e @ MainResult.Error(_,_,_,_) => e.copy(intermediate = true)
      case s @ MainResult.Success(_,_,c) => {
        cache.put(c)
        s.copy(intermediate = true)
      }
    }

    def run() = {
      import scala.concurrent.ExecutionContext.Implicits.global

      val bq = blockingQueue(result)
      (inputs ++ externals).toList.foreach { p =>
        val watcher = new FileMonitor(p, recursive=false) {
          override def onModify(file: File, count: Int): Unit = {
            Thread.sleep(10)
            bq.put(result)
          }
        }
        watcher.start
      }
      bq
    }
  }
  case class ToJson(inputs: NonEmptyList[Path], externals: List[Path], signatures: List[Path], mainPackage: PackageName, output: Path) extends MainCommand {
    def run() =
      blockingQueue(typeCheck(inputs, externals).flatMap { case (ext, packs, _) =>
        val ev = Evaluation(packs, Predef.jvmExternals ++ ext)
        ev.evaluateLast(mainPackage, Map()) match {
          case None => MainResult.Error(1, List("found no main expression"))
          case Some((eval, scheme, _, _)) =>
            val res = eval.value
            ev.toJson(res, scheme) match {
              case None =>
                MainResult.Error(1, List(s"cannot convert type to Json: $scheme"))
              case Some(j) =>
                MainResult.fromTry(CodeGen.writeDoc(output, j.toDoc))
                  .map { _ => Nil }
            }
        }
      })
  }
  case class TypeCheck(inputs: NonEmptyList[Path], signatures: List[Path], output: Path) extends MainCommand {
    def run() =
      blockingQueue(typeCheck(inputs, Nil).flatMap { case (_, packs, _) =>
        MainResult.fromTry(CodeGen.writeDoc(output, Doc.text(s"checked ${packs.toMap.size} packages")))
          .map(_ => Nil)
      })
  }
  case class Compile(inputs: NonEmptyList[Path], externals: List[Path], compileRoot: Path) extends MainCommand {
    def run() =
      blockingQueue(typeCheck(inputs, externals).flatMap { case (ext, packs, _) =>
        MainResult.fromTry(CodeGen.write(compileRoot, packs, Predef.jvmExternals ++ ext))
          .map { _ => List(s"wrote ${packs.toMap.size} packages") }
      })
  }
  case class Normalize(inputs: NonEmptyList[Path], externals: List[Path], mainPackage: PackageName) extends MainCommand {
    def run() = blockingQueue(typeCheck(inputs, externals).flatMap { case (ext, packs, _) =>
      val norm = Normalization(packs)
      norm.normalizeLast(mainPackage) match {
        case None => MainResult.Error(1, List("found no main expression"))
        case Some(expr) =>
          MainResult.Success(List(s"${expr.tag._3}"))
      }
    })
  }

  case class RunTests(tests: NonEmptyList[Path], dependencies: List[Path], externals: List[Path]) extends MainCommand {
    def run() =
      blockingQueue(typeCheck(NonEmptyList(tests.head, tests.tail ::: dependencies), externals).flatMap { case (ext, packs, nameMap) =>
        val testSet = tests.toList.toSet
        val testPackages: List[PackageName] = nameMap.collect { case (p, name) if testSet(p) => name }
        val ev = Evaluation(packs, Predef.jvmExternals ++ ext)
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
          val missingDoc = Doc.text("packages with missing tests: ") +
            Doc.intercalate(Doc.lineOrSpace, noTests.sorted.map { p => Doc.text(p.asString) }).nested(2)

          MainResult.Error(1, missingDoc.render(80) :: Nil, stdOut)
        }
      })
  }

  def readExternals(epaths: List[Path]): ValidatedNel[Parser.Error, Externals] =
    epaths match {
      case Nil => Validated.valid(Externals.empty)
      case epaths =>
        epaths.traverse(Parser.parseFile(Externals.parser, _))
          .map(_.toList.map(_._2).reduce(_ ++ _))
    }

  def parseInputs(paths: NonEmptyList[Path]): ValidatedNel[Parser.Error, NonEmptyList[((Path, LocationMap), Package.Parsed)]] =
    paths.traverse { path =>
      Parser.parseFile(Package.parser, path).map { case (lm, parsed) =>
        ((path, lm), parsed)
      }
    }

  def toResult[A](v: ValidatedNel[Parser.Error, A]): MainResult[A] =
    v match {
      case Validated.Valid(a) => MainResult.Success(a)
      case Validated.Invalid(errs) =>
        val msgs = errs.toList.flatMap {
          case Parser.Error.PartialParse(_, pos, map, Some(path)) =>
            // we should never be partial here
            val (r, c) = map.toLineCol(pos).get
            val ctx = map.showContext(pos).get
            List(s"failed to parse completely $path at line ${r + 1}, column ${c + 1}",
                ctx.toString)
          case Parser.Error.ParseFailure(pos, map, Some(path)) =>
            // we should never be partial here
            val (r, c) = map.toLineCol(pos).get
            val ctx = map.showContext(pos).get
            List(s"failed to parse $path at line ${r + 1}, column ${c + 1}",
                ctx.toString)
          case Parser.Error.FileError(path, err) =>
            List(s"failed to parse $path",
                err.getMessage,
                err.getClass.toString)
          case other =>
            List(s"unexpected error $other")
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

    def toList[A](o: Option[NonEmptyList[A]]): List[A] =
      o match {
        case None => Nil
        case Some(ne) => ne.toList
      }

    val ins = Opts.options[Path]("input", help = "input files")
    val deps = Opts.options[Path]("test_deps", help = "test dependencies").orNone.map(toList(_))
    val extern = Opts.options[Path]("external", help = "input files").orNone.map(toList(_))
    val sigs = Opts.options[Path]("signature", help = "signature files").orNone.map(toList(_))

    val mainP = Opts.option[PackageName]("main", help = "main package to evaluate")
    val outputPath = Opts.option[Path]("output", help = "output path")
    val compileRoot = Opts.option[Path]("compile_root", help = "root directory to write java output")

    val evalOpt = (ins, extern, mainP).mapN(Evaluate(_, _, _))
    val revalOpt = (ins, extern, mainP).mapN(Revaluate(_, _, _))
    val normOpt = (ins, extern, mainP).mapN(Normalize(_, _, _))
    val toJsonOpt = (ins, extern, sigs, mainP, outputPath).mapN(ToJson(_, _, _, _, _))
    val typeCheckOpt = (ins, sigs, outputPath).mapN(TypeCheck(_, _, _))
    val compileOpt = (ins, extern, compileRoot).mapN(Compile(_, _, _))
    val testOpt = (ins, deps, extern).mapN(RunTests(_, _, _))

    Opts.subcommand("eval", "evaluate an expression and print the output")(evalOpt)
      .orElse(Opts.subcommand("reval", "re-evaluate an expression on file change and print the output")(revalOpt))
      .orElse(Opts.subcommand("normalize", "apply a beta-normalization to the program")(normOpt))
      .orElse(Opts.subcommand("write-json", "evaluate a data expression into json")(toJsonOpt))
      .orElse(Opts.subcommand("type-check", "type check a set of packages")(typeCheckOpt))
      .orElse(Opts.subcommand("compile", "compile bosatsu to Java code")(compileOpt))
      .orElse(Opts.subcommand("test", "test a set of bosatsu modules")(testOpt))
  }
}

object Main {
  def command: Command[MainCommand] =
    Command("bosatsu", "a total and functional programming language")(MainCommand.opts)

  def runLoop(resultQueue: LinkedBlockingQueue[MainResult[List[String]]]): Unit = resultQueue.take match {
    case MainResult.Error(code, errs, stdout, intermediate) =>
      errs.foreach(System.err.println)
      stdout.foreach(println)
      if(intermediate) runLoop(resultQueue) else System.exit(code)
    case MainResult.Success(lines, intermediate, _) =>
      lines.foreach(println)
      if(intermediate) runLoop(resultQueue) else System.exit(0)
  }

  def main(args: Array[String]): Unit =
    command.parse(args.toList) match {
      case Right(cmd) =>
        runLoop(cmd.run())
      case Left(help) =>
        System.err.println(help.toString)
        System.exit(1)
    }
}
