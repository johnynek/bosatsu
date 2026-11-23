package dev.bosatsu

import cats.Show
import cats.data.{Ior, Validated}
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

  private def assertNoMatchesAlwaysTrue(statement: String): Unit =
    val parsed = Parser.parse(Package.parser, statement) match {
      case Validated.Valid((lm, parsed)) =>
        ((0.toString, lm), parsed) :: Nil
      case Validated.Invalid(errs) =>
        fail(s"parse fail: $errs")
    }

    val withPre = PackageMap.withPredefA(("predef", LocationMap("")), parsed)
    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }
    given Show[String] = Show.fromToString
    val res =
      Par.noParallelism {
        PackageMap.resolveThenInfer(withPrePaths, Nil, CompileOptions.Default)
      }

    val errs = res match {
      case Ior.Left(es)     => es.toList
      case Ior.Both(es, _)  => es.toList
      case Ior.Right(_) => Nil
    }
    val hasAlwaysTrue = errs.exists {
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.MatchesAlwaysTrue(_)
          ) =>
        true
      case _ => false
    }
    assert(!hasAlwaysTrue, errs.mkString(", "))

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

  test("guarded irrefutable matches do not report always-true matches") {
    assertNoMatchesAlwaysTrue(
      """package GuardedTotal
        |
        |def pred(x): True
        |
        |main = 42 matches y if pred(y)
        |""".stripMargin
    )
  }

  test("if True guarded matches still report always-true matches") {
    assertMatchesAlwaysTrue(
      """package GuardedTrue
        |
        |main = 42 matches y if True
        |""".stripMargin
    )
  }

  test("effectively-trivial match branch guards participate like unguarded branches") {
    assertTotal(
      """main = match 1:
        |  case x if x matches y:
        |    y
        |""".stripMargin,
      "main"
    )
  }

  test("nontrivial match branch guards remain guarded for totality") {
    assertNonTotal(
      """def keep_zero(i: Int) -> Bool:
        |  i.eq_Int(0)
        |
        |main = match 1:
        |  case x if x matches y if keep_zero(y):
        |    y
        |""".stripMargin,
      "main"
    )
  }

  test("irrefutable conditional matches still produce a totality diagnostic") {
    val statement =
      """package ConditionalIrrefutable
        |
        |main = if 42 matches y:
        |  y
        |else:
        |  0
        |""".stripMargin

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
          .getOrElse(fail("expected totality error"))
      }

    val hasTotalityDiagnostic = errs.exists {
      case PackageError.TotalityCheckError(
            _,
            TotalityCheck.MatchesAlwaysTrue(_) | TotalityCheck.UnreachableBranches(_, _)
          ) =>
        true
      case _ => false
    }

    assert(hasTotalityDiagnostic, errs.toList.mkString(", "))
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

  test("constructor existential named like the scrutinee variable is not captured") {
    // Type.normalize renames `forall b` to `forall a`, the name of Step's
    // existential. Step(Base(1)) has type forall b. T[b], so HasT is reachable.
    assertNonTotal(
      """#
enum T[x]: Base(v: x), Step[a](t: T[a])
enum Q: HasT(t: forall b. T[b]), Other

def f(q: Q) -> Int:
  match q:
    case Other: 0
""",
      "f"
    )
  }

  test("nested field quantifier named like the scrutinee variable is not captured") {
    // Both `forall b. D[b]` and B's field `exists y. D[y]` normalize their
    // binder to `a`. B(A(1)) has type D[b] for every b, so HasD is reachable.
    assertNonTotal(
      """#
enum D[x]: A(v: x), B(w: exists y. D[y])
enum Q: HasD(d: forall b. D[b]), Other

def f(q: Q) -> Int:
  match q:
    case Other: 0
""",
      "f"
    )
  }

  test("constructor existential named like an outer variable in a type argument is not captured") {
    // `exists b. Box[b]` normalizes to `exists a. Box[a]`, and the outer `a`
    // is substituted for MkBox's `x` next to its existential `a`.
    // HasBox(MkBox(1, x -> x)) is a value (with a = Never), so HasBox is
    // reachable.
    assertNonTotal(
      """#
enum Never: Never(next: Never)
enum Box[x]: MkBox[a](h: x, k: a -> Never)
enum Q: HasBox(b: exists b. Box[b]), Other

def f(q: Q) -> Int:
  match q:
    case Other: 0
""",
      "f"
    )
  }

  test("totality check finishes on a type whose expansion opens many variables") {
    // Every expansion of P opens four existentials, giving 16 data-type keys and
    // up to 16! paths for a search that only stops at keys on its current
    // path. The least-fixed-point solver needs 16 equations.
    assertTotal(
      """#
enum P[w, x, y, z]: Base(w: w, x: x, y: y, z: z), Step[a, b, c, d](p: P[a, b, c, d])

def f(p: P[Int, Int, Int, Int]) -> Int:
  match p:
    case Base(_, _, _, _): 0
    case Step(_): 1
""",
      "f"
    )
  }

  test("a branch reachable through a contravariant wrapper is not pruned") {
    // Rec occurs covariantly in its own definition (under two contravariant
    // parameters), so it compiles, and it is empty. But
    // absurd(D(W1(r))) = absurd(r), absurd(D(W2(f))) = f(W1(1)) is a value of
    // Rec -> Never, so (None, Some(absurd)) reaches the missing branch.
    assertNonTotal(
      """#
enum Never: Never(next: Never)
enum Wrapper[x, y]: W1(v: y), W2(f: x -> Never)
struct Rec(w: Wrapper[Wrapper[Rec, Int], Rec])

def g(s: (Option[Wrapper[Wrapper[Rec, Int], Rec]], Option[Rec -> Never])) -> Int:
  match s:
    case (_, None): 0
""",
      "g"
    )
  }

  test("recursion under two function arguments is solved through the compiler") {
    // DoubleNeg occurs to the left of two arrows in its own definition, which
    // the compiler accepts. It has no value, so HasP can be left out.
    assertTotal(
      """#
enum Never: Never(next: Never)
struct DoubleNeg(f: (DoubleNeg -> Never) -> Never)
enum Q: HasP(p: DoubleNeg), HasInt(i: Int)

def h(q: Q) -> Int:
  match q:
    case HasInt(i): i
""",
      "h"
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
