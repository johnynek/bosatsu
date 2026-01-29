package dev.bosatsu

import cats.data.{Chain, Ior, Validated, NonEmptyList}
import java.nio.file.{Files, Paths}
import dev.bosatsu.rankn._
import dev.bosatsu.tool.Output
import munit.Assertions.{assertEquals, fail}
import IorMethods.IorExtension

import cats.syntax.all._

object TestUtils {

  def parsedTypeEnvOf(
      pack: PackageName,
      str: String
  ): ParsedTypeEnv[Option[Kind.Arg]] = {

    val stmt = statementsOf(str)
    val prog = SourceConverter.toProgram(pack, Nil, stmt) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => sys.error(err.toString)
    }
    prog.types._2
  }

  val predefParsedTypeEnv: ParsedTypeEnv[Option[Kind.Arg]] = {
    val p = Package.predefPackage
    val prog = SourceConverter.toProgram(p.name, Nil, p.program) match {
      case Ior.Right(prog)   => prog
      case Ior.Both(_, prog) => prog
      case Ior.Left(err)     => sys.error(err.toString)
    }
    prog.types._2
  }

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Option[Kind.Arg]] =
    TypeEnv.fromParsed(parsedTypeEnvOf(pack, str))

  def statementsOf(str: String): List[Statement] =
    Parser.unsafeParse(Statement.parser, str)

  /** Make sure no illegal final types escaped into a TypedExpr
    */
  def assertValid[A](te: TypedExpr[A]): Unit = {
    def checkType(t: Type, bound: Set[Type.Var.Bound]): Type =
      t match {
        case t @ Type.TyVar(Type.Var.Skolem(_, _, _, _)) =>
          sys.error(s"illegal skolem ($t) escape in ${te.repr}")
        case Type.TyVar(Type.Var.Bound(_)) => t
        case t @ Type.TyMeta(_)            =>
          sys.error(s"illegal meta ($t) escape in ${te.repr}")
        case Type.TyApply(left, right) =>
          Type.TyApply(
            checkType(left, bound).asInstanceOf[Type.Rho],
            checkType(right, bound)
          )
        case q: Type.Quantified =>
          q.copy(in =
            checkType(q.in, bound ++ q.vars.toList.map(_._1))
              .asInstanceOf[Type.Rho]
          )
        case Type.TyConst(_) => t
      }
    te.traverseType[cats.Id](checkType(_, Set.empty))
    val tp = te.getType
    lazy val teStr = Type.fullyResolvedDocument.document(tp).render(80)
    scala.Predef.require(
      Type.freeTyVars(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )

    scala.Predef.require(
      Type.metaTvs(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}"
    )
  }

  val testPackage: PackageName = PackageName.parts("Test")

  def checkLast[A](
      statement: String
  )(fn: TypedExpr[Declaration] => A): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBody(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid(program) =>
        // make sure all the TypedExpr are valid
        program.lets.foreach { case (_, _, te) => assertValid(te) }
        fn(program.lets.last._3)
    }
  }

  def checkPackageMap[A](
      statement: String
  )(
      fn: PackageMap.Typed[Declaration] => A
  ): A = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBody(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        val lm = LocationMap(statement)
        val packMap = Map((testPackage, (lm, statement)))
        val msg = errs.toList
          .map { err =>
            err.message(packMap, LocationMap.Colorize.None)
          }
          .mkString("", "\n==========\n", "\n")
        sys.error("inference failure: " + msg)
      case Validated.Valid(program) =>
        // make sure all the TypedExpr are valid
        program.lets.foreach { case (_, _, te) => assertValid(te) }
        val pack: Package.Typed[Declaration] =
          Package(testPackage, Nil, Nil, (program, ImportMap.empty))
        val pm: PackageMap.Typed[Declaration] =
          PackageMap.empty + pack + PackageMap.predefCompiled
        fn(pm)
    }
  }

  def checkMatchless[A](
      statement: String
  )(
      fn: Map[
        PackageName,
        List[(Identifier.Bindable, Matchless.Expr[Unit])]
      ] => A
  ): A =
    checkPackageMap(statement) { pm =>
      Par.withEC {
        val comp = MatchlessFromTypedExpr.compile((), pm)
        fn(comp)
      }
    }

  def compileFile(path: String, rest: String*)(implicit
      ec: Par.EC
  ): PackageMap.Typed[Any] = {
    def toS(s: String): String =
      new String(Files.readAllBytes(Paths.get(s)), "UTF-8")

    val packNEL =
      NonEmptyList(path, rest.toList)
        .map { s =>
          val str = toS(s)
          val pack = Parser.unsafeParse(Package.parser(None), str)
          (("", LocationMap(str)), pack)
        }

    val res = PackageMap.typeCheckParsed(packNEL, Nil, "")
    res.left match {
      case Some(err) => sys.error(err.toString)
      case None      => ()
    }

    res.right.get
  }

  def makeInputArgs(files: List[(Chain[String], Any)]): List[String] =
    ("--package_root" :: "" :: Nil) ::: files.flatMap { case (idx, _) =>
      "--input" :: idx.iterator.mkString("/") :: Nil
    }

  private type ErrorOr[A] = Either[Throwable, A]
  private val module = MemoryMain[ErrorOr]

  def evalTest(packages: List[String], mainPackS: String, expected: Value) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "eval" :: "--main" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(Output.EvaluationResult(got, _, gotDoc)) =>
        val gv = got.value
        assertEquals(
          gv,
          expected,
          s"${gotDoc.value.render(80)}\n\n$gv != $expected"
        )
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(msg) => fail(msg)
          case None      => fail(s"got an exception: $err")
        }
    }
  }

  def evalTestJson(
      packages: List[String],
      mainPackS: String,
      expected: Json
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "json" :: "write" :: "--main" :: mainPackS :: "--output" :: "-1" :: makeInputArgs(
        files
      )
    ) match {
      case Right(Output.JsonOutput(got, _)) =>
        assertEquals(got, expected, s"$got != $expected")
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def runBosatsuTest(
      packages: List[String],
      mainPackS: String,
      assertionCount: Int
  ) = {
    val files = packages.zipWithIndex.map(_.swap).map { case (idx, content) =>
      Chain.one(s"Package$idx") -> content
    }

    module.runWith(files)(
      "test" :: "--test_package" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(Output.TestOutput(results, _)) =>
        results.collect { case (_, Some(t)) => t.value } match {
          case t :: Nil =>
            assertEquals(
              t.assertions,
              assertionCount,
              s"${t.assertions} != $assertionCount"
            )
            val Test.Report(_, failcount, message) =
              Test.report(t, LocationMap.Colorize.None)
            assertEquals(t.failures.map(_.assertions).getOrElse(0), failcount)
            if (failcount > 0) fail(message.render(80))
            else ()
          case other =>
            fail(s"expected exactly one test result, got: $other")
        }
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        module.mainExceptionToString(err) match {
          case Some(err) =>
            fail(err)
          case None =>
            err.printStackTrace
            fail(err.toString)
        }
    }
  }

  def testInferred(
      packages: List[String],
      mainPackS: String,
      inferredHandler: (PackageMap.Inferred, PackageName) => Unit
  )(implicit ec: Par.EC) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          System.err.println(
            p.showContext(LocationMap.Colorize.None).render(80)
          )
        }
        sys.error("failed to parse") // errs.toString)
    }

    val fullParsed =
      PackageMap
        .withPredefA(("predef", LocationMap("")), parsedPaths)
        .map { case ((path, _), p) => (path, p) }

    PackageMap
      .resolveThenInfer(fullParsed, Nil)
      .strictToValidated match {
      case Validated.Valid(packMap) =>
        inferredHandler(packMap, mainPack)

      case Validated.Invalid(errs) =>
        val tes = errs.toList
          .collect { case te: PackageError.TypeErrorIn =>
            te.tpeErr.toString
          }
          .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }
  }

  def evalFail(
      packages: List[String]
  )(errFn: PartialFunction[PackageError, Unit])(implicit ec: Par.EC) = {

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs)     => vs
      case Validated.Invalid(errs) =>
        sys.error(s"parse fail: ${errs}")
    }

    // use parallelism to typecheck
    val withPre =
      PackageMap.withPredefA(("predef", LocationMap("")), parsedPaths)

    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }
    PackageMap.resolveThenInfer(withPrePaths, Nil).left match {
      case None =>
        fail("expected to fail type checking")

      case Some(errs) if errs.collect(errFn).nonEmpty =>
        // make sure we can print the messages:
        val sm = PackageMap.buildSourceMap(withPre)
        errs.toList.foreach(_.message(sm, LocationMap.Colorize.None))
        assert(true)
      case Some(errs) =>
        fail(s"failed, but no type errors: $errs")
    }
  }

}
