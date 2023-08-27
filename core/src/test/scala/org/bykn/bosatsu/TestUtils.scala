package org.bykn.bosatsu

import cats.data.{Ior, Validated}
import cats.implicits._
import org.bykn.bosatsu.rankn._
import org.scalatest.{Assertion, Assertions}

import Assertions.{succeed, fail}
import IorMethods.IorExtension

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
    def checkType(t: Type): Type =
      t match {
        case t @ Type.TyVar(Type.Var.Skolem(_, _, _)) =>
          sys.error(s"illegal skolem ($t) escape in ${te.repr}")
        case Type.TyVar(Type.Var.Bound(_)) => t
        case t @ Type.TyMeta(_) =>
          sys.error(s"illegal meta ($t) escape in ${te.repr}")
        case Type.TyApply(left, right) =>
          Type.TyApply(checkType(left), checkType(right))
        case Type.ForAll(args, in) =>
          Type.ForAll(args, checkType(in).asInstanceOf[Type.Rho])
        case Type.TyConst(_) => t
      }
    te.traverseType[cats.Id](checkType)
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

  def checkLast(
      statement: String
  )(fn: TypedExpr[Declaration] => Assertion): Assertion = {
    val stmts = Parser.unsafeParse(Statement.parser, statement)
    Package.inferBody(testPackage, Nil, stmts).strictToValidated match {
      case Validated.Invalid(errs) =>
        fail(
          "inference failure: " + errs.toList
            .map(_.message(Map.empty, LocationMap.Colorize.None))
            .mkString("\n")
        )
      case Validated.Valid(program) =>
        // make sure all the TypedExpr are valid
        program.lets.foreach { case (_, _, te) => assertValid(te) }
        fn(program.lets.last._3)
    }
  }

  def makeInputArgs(files: List[(Int, Any)]): List[String] =
    ("--package_root" :: Int.MaxValue.toString :: Nil) ::: files.flatMap {
      case (idx, _) => "--input" :: idx.toString :: Nil
    }

  private val module = new MemoryMain[Either[Throwable, *], Int]({ idx =>
    if (idx == Int.MaxValue) Nil
    else List(s"Package$idx")
  })

  def evalTest(packages: List[String], mainPackS: String, expected: Value) = {
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)(
      "eval" :: "--main" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(module.Output.EvaluationResult(got, _, gotDoc)) =>
        val gv = got.value
        assert(
          gv == expected,
          s"${gotDoc.value.render(80)}\n\n$gv != $expected"
        )
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def evalTestJson(
      packages: List[String],
      mainPackS: String,
      expected: Json
  ) = {
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)(
      "json" :: "write" :: "--main" :: mainPackS :: "--output" :: "-1" :: makeInputArgs(
        files
      )
    ) match {
      case Right(module.Output.JsonOutput(got, _)) =>
        assert(got == expected, s"$got != $expected")
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
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)(
      "test" :: "--test_package" :: mainPackS :: makeInputArgs(files)
    ) match {
      case Right(module.Output.TestOutput(results, _)) =>
        results.collect { case (_, Some(t)) => t.value } match {
          case t :: Nil =>
            assert(
              t.assertions == assertionCount,
              s"${t.assertions} != $assertionCount"
            )
            val (_, failcount, message) =
              Test.report(t, LocationMap.Colorize.None)
            assert(t.failures.map(_.assertions).getOrElse(0) == failcount)
            if (failcount > 0) fail(message.render(80))
            else succeed
          case other =>
            fail(s"expected exactly one test result, got: $other")
        }
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(pe: module.MainException.PackageErrors) =>
        fail(pe.messages.mkString("\n"))
      case Left(err) =>
        err.printStackTrace
        fail(s"got an exception: $err")
    }
  }

  def testInferred(
      packages: List[String],
      mainPackS: String,
      inferredHandler: (PackageMap.Inferred, PackageName) => Assertion
  )(implicit ec: Par.EC) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
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
          .collect { case PackageError.TypeErrorIn(te, _) =>
            te.toString
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
      case Validated.Valid(vs) => vs
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
