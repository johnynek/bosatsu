package dev.bosatsu

import cats.Show
import cats.data.Validated
import Identifier.{Constructor, Name}
import TestUtils.checkEnvExpr

class TypedTotalityTest extends munit.FunSuite {
  private val pack = TestUtils.testPackage

  private def findLetExpr(
      lets: List[(Identifier.Bindable, RecursionKind, TypedExpr[Declaration])],
      name: String
  ): TypedExpr[Declaration] = {
    val target = Name(name)
    lets.collectFirst { case (`target`, _, expr) => expr } match {
      case Some(expr) => expr
      case None       => fail(s"missing let: $name in ${lets.map(_._1).mkString(", ")}")
    }
  }

  private def assertTotal(statement: String, letName: String): Unit =
    checkEnvExpr(statement) { (env, lets) =>
      val expr = findLetExpr(lets, letName)
      TotalityCheck(env).checkExpr(expr) match {
        case Validated.Valid(())    => ()
        case Validated.Invalid(errs) => fail(errs.toList.mkString(", "))
      }
    }

  private def assertNonTotal(statement: String, letName: String): Unit =
    checkEnvExpr(statement) { (env, lets) =>
      val expr = findLetExpr(lets, letName)
      TotalityCheck(env).checkExpr(expr) match {
        case Validated.Invalid(errs) =>
          val hasNonTotal = errs.exists {
            case TotalityCheck.NonTotalMatch(_, _) => true
            case _                                 => false
          }
          assert(hasNonTotal, errs.toList.mkString(", "))
        case Validated.Valid(()) =>
          fail("expected non-total match error")
      }
    }

  private def assertMatchesAlwaysTrue(statement: String): Unit =
    val parsed = Parser.parse(Package.parser, statement) match {
      case Validated.Valid((lm, parsed)) =>
        ((0.toString, lm), parsed) :: Nil
      case Validated.Invalid(errs) =>
        fail(s"parse fail: $errs")
    }

    val withPre = PackageMap.withPredefA(("predef", LocationMap("")), parsed)
    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }
    given Show[String] = Show.fromToString
    val errs =
      Par.noParallelism {
        PackageMap
          .resolveThenInfer(withPrePaths, Nil, CompileOptions.Default)
          .left
          .getOrElse(fail("expected always-true `matches` error"))
      }

    val hasAlwaysTrue = errs.exists {
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.MatchesAlwaysTrue(_)
          ) =>
        true
      case _ => false
    }
    val hasUnreachable = errs.exists {
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.UnreachableBranches(_, _)
          ) =>
        true
      case _ => false
    }
    assert(hasAlwaysTrue, errs.toList.mkString(", "))
    assert(!hasUnreachable, errs.toList.mkString(", "))

  test("Result[Never, Value] with only Ok branch is total") {
    assertTotal(
      """#
enum Never: Never(next: Never)
enum Value: V
enum Result[e, r]: Err(err: e), Ok(ok: r)

def only_ok(x: Result[Never, Value]) -> Value:
  Ok(v) = x
  v
""",
      "only_ok"
    )
  }

  test("unreachable detection remains shadowing based") {
    checkEnvExpr("""#
enum Value: V
enum Result[e, r]: Err(err: e), Ok(ok: r)

def has_unreachable(y: Result[Value, Value]) -> Value:
  match y:
    case _:
      V
    case Ok(_):
      V
""") { (env, lets) =>
      val expr = findLetExpr(lets, "has_unreachable")
      val okPat =
        Pattern.PositionalStruct(
          (pack, Constructor("Ok")),
          Pattern.WildCard :: Nil
        )
      TotalityCheck(env).checkExpr(expr) match {
        case Validated.Invalid(errs) =>
          val hasUnreachableOk = errs.exists {
            case TotalityCheck.UnreachableBranches(_, pats) =>
              pats.toList.contains(okPat)
            case _ => false
          }
          assert(hasUnreachableOk, errs.toList.mkString(", "))
        case Validated.Valid(()) =>
          fail("expected unreachable branch error")
      }
    }
  }

  test("definitively uninhabited scrutinee is vacuously total") {
    assertTotal(
      """#
enum Never: A(next: Never), B(next: Never)

def vacuous(z: Never) -> Never:
  A(a) = z
  a
""",
      "vacuous"
    )
  }

  test("unit matches totality errors report always-true matches") {
    assertMatchesAlwaysTrue(
      """package TotalUnit
        |
        |x = ()
        |
        |y = x matches ()
        |""".stripMargin
    )
  }

  test("struct matches totality errors report always-true matches") {
    assertMatchesAlwaysTrue(
      """package TotalStruct
        |
        |struct Foo
        |
        |x = Foo
        |
        |y = x matches Foo
        |""".stripMargin
    )
  }

  test("forall scrutinee with phantom branch parameter is not total") {
    assertNonTotal(
      """#
enum Result[e, r]: Err(err: r), Ok(ok: r)

def is_good(res: forall e. Result[e, r]) -> r:
  Ok(ok) = res
  ok
""",
      "is_good"
    )
  }

  test("forall scrutinee can prove totality when existential branch carries e") {
    assertTotal(
      """#
enum Result[e, r]: Err(err: e), Ok(ok: r)

def is_good(res: forall e. Result[e, r]) -> r:
  Ok(ok) = res
  ok
""",
      "is_good"
    )
  }

  test("deep non-match nesting is stack safe") {
    checkEnvExpr("""#
enum Value: V
enum Result[e, r]: Err(err: e), Ok(ok: r)

def deep_total(x: Result[Value, Value]) -> Value:
  match x:
    case Err(_):
      V
    case Ok(v):
      v
""") { (env, lets) =>
      val expr0 = findLetExpr(lets, "deep_total")
      val tpe = expr0.getType
      val depth = 20000

      var expr = expr0
      var idx = 0
      while (idx < depth) {
        expr = TypedExpr.Annotation(expr, tpe, None)
        idx += 1
      }

      TotalityCheck(env).checkExpr(expr) match {
        case Validated.Valid(())    => ()
        case Validated.Invalid(errs) => fail(errs.toList.mkString(", "))
      }
    }
  }
}
