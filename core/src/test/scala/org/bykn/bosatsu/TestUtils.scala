package org.bykn.bosatsu

import org.bykn.bosatsu.rankn._
import cats.data.Validated
import org.scalatest.{Assertion, Assertions}

import fastparse.all.Parsed

import Assertions.{succeed, fail}
import cats.implicits._

object TestUtils {
  import TestParseUtils.region

  def typeEnvOf(pack: PackageName, str: String): TypeEnv[Unit] = {

    val tpeFn: Identifier.Constructor => Type.Const =
      { tpe => Type.Const.Defined(pack, TypeName(tpe)) }

    val consFn: Identifier.Constructor => (PackageName, Identifier.Constructor) =
      { cons => (pack, cons) }

    val stmt = statementOf(pack, str)
    val prog = Program.fromStatement(
      Predef.packageName,
      tpeFn,
      consFn,
      stmt)
    TypeEnv.fromParsed(prog.types)
  }

  def statementOf(pack: PackageName, str: String): Statement = {

    val tpeFn: Identifier.Constructor => Type.Const =
      { tpe => Type.Const.Defined(pack, TypeName(tpe)) }

    val consFn: Identifier.Constructor => (PackageName, Identifier.Constructor) =
      { cons => (pack, cons) }

    Statement.parser.parse(str) match {
      case Parsed.Success(stmt, idx) =>
        assert(idx == str.length)
        stmt
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx in region ${region(str, idx)} with trace: ${extra.traced.trace}")
    }
  }

  def checkLast(statement: String)(fn: TypedExpr[Declaration] => Assertion): Assertion =
    Statement.parser.parse(statement) match {
      case Parsed.Success(stmt, _) =>
        Package.inferBody(PackageName.parts("Test"), Nil, stmt) match {
          case Validated.Invalid(errs) =>
            fail("inference failure: " + errs.toList.map(_.message(Map.empty)).mkString("\n"))
          case Validated.Valid((_, lets)) =>
            fn(lets.last._3)
        }
      case Parsed.Failure(exp, idx, extra) =>
        fail(s"failed to parse: $statement: $exp at $idx with trace: ${extra.traced.trace}")
    }

  sealed abstract class EvaluationMode[A] {
    def expected: A
  }
  object EvaluationMode {
    case class JsonMode(expected: Json) extends EvaluationMode[Json]
    case class EvalMode(expected: Evaluation.Value) extends EvaluationMode[Evaluation.Value]
    case class TestMode(expected: Int) extends EvaluationMode[Int]
  }

  def evalTest(packages: List[String], mainPackS: String, expected: Evaluation.Value, extern: Externals = Externals.empty) =
    evalTestMode(packages, mainPackS, EvaluationMode.EvalMode(expected), extern)

  def evalTestJson(packages: List[String], mainPackS: String, expected: Json, extern: Externals = Externals.empty) =
    evalTestMode(packages, mainPackS, EvaluationMode.JsonMode(expected), extern)

  def runBosatsuTest(packages: List[String], mainPackS: String, assertionCount: Int, extern: Externals = Externals.empty) =
    evalTestMode(packages, mainPackS, EvaluationMode.TestMode(assertionCount), extern)

  def evalTestMode[A](packages: List[String], mainPackS: String, expected: EvaluationMode[A], extern: Externals = Externals.empty) = {
    def inferredHandler(packMap: PackageMap.Inferred, mainPack: PackageName): Assertion = {
      val ev = Evaluation(packMap, Predef.jvmExternals ++ extern)
      ev.evaluateLast(mainPack) match {
        case None => fail("found no main expression")
        case Some((eval, schm)) =>
          val typeStr = TypeRef.fromTypes(Some(mainPack), schm :: Nil)(schm).toDoc.render(80)
          expected match {
            case EvaluationMode.EvalMode(exp) =>
              val left = eval.value
              assert(left == exp,
                s"failed: for type: $typeStr, ${left} != $exp")
              succeed
            case EvaluationMode.JsonMode(json) =>
              val leftJson = ev.toJson(eval.value, schm)
              assert(leftJson == Some(json), s"type: $typeStr, $leftJson != $json")
              succeed
            case EvaluationMode.TestMode(cnt) =>
              ev.evalTest(mainPack) match {
                case None => fail(s"$mainPack had no tests evaluted")
                case Some(t) =>
                  assert(t.assertions == cnt)
                  val (suc, failcount, message) = Test.report(t)
                  assert(t.failures.map(_.assertions).getOrElse(0) == failcount)
                  if (failcount > 0) fail(message.render(80))
                  else succeed
              }
          }
      }
    }
    testInferred(packages, mainPackS, inferredHandler(_, _))
  }

  def testInferred(packages: List[String], mainPackS: String, inferredHandler: (PackageMap.Inferred, PackageName) => Assertion ) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        errs.toList.foreach { p =>
          p.showContext.foreach(System.err.println)
        }
        sys.error("failed to parse") //errs.toString)
    }

    PackageMap.resolveThenInfer(Predef.withPredefA(("predef", LocationMap("")), parsedPaths)) match {
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
    case class TagMode(expected: Normalization.NormalExpressionTag) extends NormalTestMode[Normalization.NormalExpressionTag]
    case class ExpressionMode(expected: NormalExpression) extends NormalTestMode[NormalExpression]
    case class ChildrenMode(expected: Set[NormalExpression]) extends NormalTestMode[Set[NormalExpression]]
  }

  def normalizeTest[A](packages: List[String], mainPackS: String, expectedMode: NormalTestMode[A]) = {
    def inferredHandler(infPackMap: PackageMap.Inferred, mainPack: PackageName): Assertion = {
      val normPackMap = NormalizePackageMap(infPackMap).normalizePackageMap
      (for {
        pack <- normPackMap.toMap.get(mainPack)
        (name, rec, expr) <- pack.program.lets.lastOption
      } yield {
        expectedMode match {
          case NormalTestMode.TagMode(expected) =>
            assert(expr.tag._2.ne == expected.ne, s"ne error. expected '${expected.ne}' got '${expr.tag._2.ne}'" )
            assert(expr.tag._2.children == expected.children, s"children error. expected '${expected.children}' got '${expr.tag._2.children}'" )
            succeed
          case NormalTestMode.ExpressionMode(expected) =>
            assert(expr.tag._2.ne == expected, s"ne error. expected '${expected}' got '${expr.tag._2.ne}'" )
            succeed
          case NormalTestMode.ChildrenMode(expected) =>
            assert(expr.tag._2.children == expected, s"children error. expected '${expected}' got '${expr.tag._2.children}'" )
            succeed
        }
      }
      ).getOrElse(fail("There should be a last expression"))
    }

    testInferred(packages, mainPackS, inferredHandler(_,_))
  }

  def normalTagTest(packages: List[String], mainPackS: String, expected: Normalization.NormalExpressionTag) =
    normalizeTest(packages, mainPackS, NormalTestMode.TagMode(expected))
  def normalExpressionTest(packages: List[String], mainPackS: String, expected: NormalExpression) =
    normalizeTest(packages, mainPackS, NormalTestMode.ExpressionMode(expected))
  def normalChildrenTest(packages: List[String], mainPackS: String, expected: Set[NormalExpression]) =
    normalizeTest(packages, mainPackS, NormalTestMode.ChildrenMode(expected))

  def evalFail(packages: List[String], mainPackS: String, extern: Externals = Externals.empty)(errFn: PartialFunction[PackageError, Unit]) = {
    val mainPack = PackageName.parse(mainPackS).get

    val parsed = packages.zipWithIndex.traverse { case (pack, i) =>
      Parser.parse(Package.parser, pack).map { case (lm, parsed) =>
        ((i.toString, lm), parsed)
      }
    }

    val parsedPaths = parsed match {
      case Validated.Valid(vs) => vs
      case Validated.Invalid(errs) =>
        sys.error(errs.toString)
    }

    val withPre = Predef.withPredefA(("predef", LocationMap("")), parsedPaths)
    PackageMap.resolveThenInfer(withPre) match {
      case (_, Validated.Valid(_)) =>
        fail("expected to fail type checking")

      case (sm, Validated.Invalid(errs)) if errs.collect(errFn).nonEmpty =>
        // make sure we can print the messages:
        val sm = PackageMap.buildSourceMap(withPre)
        errs.toList.foreach(_.message(sm))
        assert(true)
      case (_, Validated.Invalid(errs)) =>
          fail(s"failed, but no type errors: $errs")
    }
  }


}
