package dev.bosatsu

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

  test("forall scrutinee pattern binding typechecks") {
    checkEnvExpr("""#
enum Result[e, r]: Err(err: r), Ok(ok: r)

def is_good(res: forall e. Result[e, r]) -> r:
  Ok(ok) = res
  ok
""") { (_, lets) =>
      val hasIsGood = lets.exists(_._1 == Name("is_good"))
      assert(hasIsGood)
    }
  }
}
