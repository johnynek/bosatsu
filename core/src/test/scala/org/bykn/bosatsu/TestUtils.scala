package org.bykn.bosatsu

import cats.data.Validated
import org.bykn.bosatsu.rankn._
import org.scalatest.{Assertion, Assertions}

import fastparse.all.Parsed

import Assertions.{succeed, fail}
import cats.implicits._
import cats.Eval

import fastparse.all._

object TestUtils {
  import TestParseUtils.region

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Unit] = {

    val tpeFn: Identifier.Constructor => Type.Const =
      { tpe => Type.Const.Defined(pack, TypeName(tpe)) }

    val consFn: Identifier.Constructor => (PackageName, Identifier.Constructor) =
      { cons => (pack, cons) }

    val stmt = statementsOf(pack, str)
    val srcConv = SourceConverter(pack, Nil, Statement.definitionsOf(stmt))
    val cats.data.Ior.Right(prog) = srcConv.toProgram(stmt)
    TypeEnv.fromParsed(prog.types._2)
  }

  def statementsOf(pack: PackageName, str: String): List[Statement] =
    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        stmt
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }

  /**
   * Make sure no illegal final types escaped into a TypedExpr
   */
  def assertValid[A](te: TypedExpr[A]): Unit = {
    def checkType(t: Type): Type =
      t match {
        case t@Type.TyVar(Type.Var.Skolem(_, _)) =>
          sys.error(s"illegal skolem ($t) escape in ${te.repr}")
        case Type.TyVar(Type.Var.Bound(_)) => t
        case t@Type.TyMeta(_) =>
          sys.error(s"illegal meta ($t) escape in ${te.repr}")
        case Type.TyApply(left, right) =>
          Type.TyApply(checkType(left), checkType(right))
        case Type.ForAll(args, in) =>
          Type.ForAll(args, checkType(in).asInstanceOf[Type.Rho])
        case Type.TyConst(_) => t
      }
    te.traverseType[cats.Id](checkType)
    val tp = te.getType
    lazy val teStr = TypeRef.fromTypes(None, tp :: Nil)(tp).toDoc.render(80)
    scala.Predef.require(Type.freeTyVars(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}")

    scala.Predef.require(Type.metaTvs(tp :: Nil).isEmpty,
      s"illegal inferred type: $teStr in: ${te.repr}")
  }

  def checkLast(statement: String)(fn: TypedExpr[Declaration] => Assertion): Assertion =
    Statement.parser.parse(statement) match {
      case Parsed.Success(stmts, _) =>
        Package.inferBody(PackageName.parts("Test"), Nil, stmts) match {
          case Validated.Invalid(errs) =>
            fail("inference failure: " + errs.toList.map(_.message(Map.empty, LocationMap.Colorize.none)).mkString("\n"))
          case Validated.Valid(program) =>
            // make sure all the TypedExpr are valid
            program.lets.foreach { case (_, _, te) => assertValid(te) }
            fn(program.lets.last._3)
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $statement: $exp at $idx with trace: ${extra.traced.trace}")
    }

  def makeInputArgs(files: List[(Int, Any)]): List[String] =
    ("--package_root" :: Int.MaxValue.toString :: Nil) ::: files.flatMap { case (idx, _) => "--input" :: idx.toString :: Nil }

  private val module = new MemoryMain[Either[Throwable, ?], Int]({ idx =>
    if (idx == Int.MaxValue) Nil
    else List(s"Package$idx")
  })

  def evalTest(packages: List[String], mainPackS: String, expected: Value) = {
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)("eval" :: "--main" :: mainPackS :: makeInputArgs(files)) match {
      case Right(module.Output.EvaluationResult(got, _)) =>
        assert(got.value == expected, s"${got.value} != $expected")
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def evalTestJson(packages: List[String], mainPackS: String, expected: Json) = {
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)("write-json" :: "--main" :: mainPackS :: "--output" :: "-1" :: makeInputArgs(files)) match {
      case Right(module.Output.JsonOutput(got, _)) =>
        assert(got == expected, s"$got != $expected")
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def runBosatsuTest(packages: List[String], mainPackS: String, assertionCount: Int) = {
    val files = packages.zipWithIndex.map(_.swap)

    module.runWith(files)("test" :: "--test_package" :: mainPackS :: makeInputArgs(files)) match {
      case Right(module.Output.TestOutput(results)) =>
        results.collect { case (_, Some(t)) => t } match {
          case t :: Nil =>
            assert(t.assertions == assertionCount, s"${t.assertions} != $assertionCount")
            val (suc, failcount, message) = Test.report(t)
            assert(t.failures.map(_.assertions).getOrElse(0) == failcount)
            if (failcount > 0) fail(message.render(80))
            else succeed
          case other =>
            fail(s"expected exactly one test result, got: $other")
        }
      case Right(other) =>
        fail(s"got an unexpected success: $other")
      case Left(err) =>
        fail(s"got an exception: $err")
    }
  }

  def testInferred(packages: List[String], mainPackS: String, inferredHandler: (PackageMap.Inferred, PackageName) => Assertion ) = {
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
          p.showContext(LocationMap.Colorize.none).foreach(System.err.println)
        }
        sys.error("failed to parse") //errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths), Nil) match {
      case (dups, Validated.Valid(packMap)) if dups.isEmpty =>
        inferredHandler(packMap, mainPack)

      case (other, Validated.Invalid(errs)) =>
        val tes = errs.toList.collect {
          case PackageError.TypeErrorIn(te, _) =>
            te.message
        }
        .mkString("\n")
        fail(tes + "\n" + errs.toString)
    }
  }
  sealed abstract class NormalTestMode[A] {
    def expected: A
  }
  object NormalTestMode {
    case class TagMode(expected: Normalization.NormalExpressionTag, serialized: Option[String] = None) extends NormalTestMode[Normalization.NormalExpressionTag]
    case class ExpressionMode(expected: NormalExpression, serialized: Option[String] = None) extends NormalTestMode[NormalExpression]
    case class ChildrenMode(expected: Set[NormalExpression]) extends NormalTestMode[Set[NormalExpression]]
  }

  def normalizeTest[A](packages: List[String], mainPackS: String, expectedMode: NormalTestMode[A]) = {
    def inferredHandler(infPackMap: PackageMap.Inferred, mainPack: PackageName): Assertion = {
      val normPackMap = NormalizePackageMap(infPackMap).hashKey(ne => (ne, ne.serialize))
      (for {
        pack <- normPackMap.toMap.get(mainPack)
        exprs = pack.program.lets.map { case (_, rec, e) => e }
        fleft = exprs.map(_.size.toInt)
        fright = exprs.map(_.foldRight(Eval.now(0)) { case (_, m) => m.map(_ + 1) }.value)
        expr <- exprs.lastOption
        tag = expr.tag
        ser = tag._2.ne._2
        ne = tag._2.ne._1
        children = tag._2.children.map(_._1)
      } yield {
        assert(fleft == fright, s"folds didn't match. left: $fleft, right: $fright")
        expectedMode match {
          case NormalTestMode.TagMode(expected, expectedSerialiazed) =>
            expectedSerialiazed.foreach ( s =>
              assert(ser == s, s"serialization error. expected '$s' got '$ser'")
            )
            assert(ne == expected.ne, s"ne error. expected '${expected.ne}' got '$ne'" )
            assert(children == expected.children, s"children error. expected '${expected.children}' got '$children'" )
            succeed
          case NormalTestMode.ExpressionMode(expected, expectedSerialiazed) =>
            expectedSerialiazed.foreach( s =>
              assert(ser == s, s"serialization error. expected '$s' got '$ser'")
            )
            assert(ne == expected, s"ne error. expected '${expected}' got '$ne'" )
            succeed
          case NormalTestMode.ChildrenMode(expected) =>
            assert(children == expected, s"children error. expected '${expected}' got '$children'" )
            succeed
        }
      }
      ).getOrElse(fail("There should be a last expression"))
    }

    testInferred(packages, mainPackS, inferredHandler(_,_))
  }

  def normalTagTest(packages: List[String], mainPackS: String, expected: Normalization.NormalExpressionTag, expectedSerialiazed: Option[String] = None) =
    normalizeTest(packages, mainPackS, NormalTestMode.TagMode(expected, expectedSerialiazed))
  def normalExpressionTest(packages: List[String], mainPackS: String, expected: NormalExpression, expectedSerialiazed: Option[String] = None) =
    normalizeTest(packages, mainPackS, NormalTestMode.ExpressionMode(expected, expectedSerialiazed))
  def normalChildrenTest(packages: List[String], mainPackS: String, expected: Set[NormalExpression]) =
    normalizeTest(packages, mainPackS, NormalTestMode.ChildrenMode(expected))

  def evalFail(packages: List[String], mainPackS: String, extern: Externals = Externals.empty)(errFn: PartialFunction[PackageError, Unit]) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser(None), pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        sys.error(errs.toString)
    }

    val withPre = Predef.withPredefA(("predef", LocationMap("")), parsedPaths)
    PackageMap.resolveThenInfer(withPre, Nil) match {
      case (_, Validated.Valid(_)) =>
        fail("expected to fail type checking")

      case (sm, Validated.Invalid(errs)) if errs.collect(errFn).nonEmpty =>
        // make sure we can print the messages:
        val sm = PackageMap.buildSourceMap(withPre)
        errs.toList.foreach(_.message(sm, LocationMap.Colorize.none))
        assert(true)
      case (_, Validated.Invalid(errs)) =>
          fail(s"failed, but no type errors: $errs")
    }
  }


}
