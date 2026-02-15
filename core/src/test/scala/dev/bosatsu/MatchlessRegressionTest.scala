package dev.bosatsu

import cats.Eval

class MatchlessRegressionTest extends munit.FunSuite {
  private def nestedLetMut(depth: Int): Matchless.Expr[Unit] =
    (0 until depth).foldLeft[Matchless.Expr[Unit]](Matchless.MakeStruct(0)) {
      case (acc, idx) =>
        Matchless.LetMut(Matchless.LocalAnonMut(idx.toLong), acc)
    }

  private def nestedLet(depth: Int): Matchless.Expr[Unit] =
    (0 until depth).foldLeft[Matchless.Expr[Unit]](Matchless.MakeStruct(0)) {
      case (acc, idx) =>
        Matchless.Let(
          Identifier.synthetic(s"issue1652_$idx"),
          Matchless.MakeStruct(0),
          acc
        )
    }

  private def nestedLetMutBool(depth: Int): Matchless.BoolExpr[Unit] =
    (0 until depth).foldLeft[Matchless.BoolExpr[Unit]](Matchless.TrueConst) {
      case (acc, idx) =>
        Matchless.LetMutBool(Matchless.LocalAnonMut(idx.toLong), acc)
    }

  private def assertReuseConstructorsNoStackOverflow(
      expr: Matchless.Expr[Unit]
  ): Unit =
    try {
      Matchless.reuseConstructors(expr)
      ()
    } catch {
      case _: StackOverflowError =>
        fail(
          "reuseConstructors should not overflow on deeply nested expressions"
        )
    }

  private def countWhileExprs(e: Matchless.Expr[Unit]): Int =
    e match {
      case Matchless.WhileExpr(cond, effectExpr, _) =>
        1 + countBoolWhileExprs(cond) + countWhileExprs(effectExpr)
      case Matchless.Lambda(captures, _, _, body) =>
        captures.map(countWhileExprs).sum + countWhileExprs(body)
      case Matchless.App(fn, args) =>
        countWhileExprs(fn) + args.toList.map(countWhileExprs).sum
      case Matchless.Let(_, expr, in) =>
        countWhileExprs(expr) + countWhileExprs(in)
      case Matchless.LetMut(_, in) =>
        countWhileExprs(in)
      case Matchless.If(cond, t, f) =>
        countBoolWhileExprs(cond) + countWhileExprs(t) + countWhileExprs(f)
      case Matchless.Always(cond, thenExpr) =>
        countBoolWhileExprs(cond) + countWhileExprs(thenExpr)
      case Matchless.PrevNat(of) =>
        countWhileExprs(of)
      case _ =>
        0
    }

  private def countBoolWhileExprs(b: Matchless.BoolExpr[Unit]): Int =
    b match {
      case Matchless.EqualsLit(expr, _) =>
        countWhileExprs(expr)
      case Matchless.EqualsNat(expr, _) =>
        countWhileExprs(expr)
      case Matchless.And(left, right) =>
        countBoolWhileExprs(left) + countBoolWhileExprs(right)
      case Matchless.CheckVariant(expr, _, _, _) =>
        countWhileExprs(expr)
      case Matchless.SetMut(_, expr) =>
        countWhileExprs(expr)
      case Matchless.LetBool(_, value, in) =>
        countWhileExprs(value) + countBoolWhileExprs(in)
      case Matchless.LetMutBool(_, in) =>
        countBoolWhileExprs(in)
      case Matchless.TrueConst =>
        0
    }

  test("matrix match does not duplicate large terminal fallback") {
    TestUtils.checkMatchless("""
enum L:
  E
  M(tail: L)

enum Nat:
  Z
  S(prev: Nat)

def branch_blowup(args: L) -> Nat:
  match args:
    case M(M(E)): Z
    case _:
      (
        def loop(lst: L, acc: Nat) -> Nat:
          recur lst:
            case E: acc
            case M(tail): loop(tail, S(acc))
        loop(args, Z)
      )
""") { binds =>
      val byName = binds(TestUtils.testPackage).toMap
      val expr = byName(Identifier.Name("branch_blowup"))
      val whileCount = countWhileExprs(expr)
      assertEquals(whileCount, 1, expr.toString)
    }
  }

  test(
    "issue 1652: deep LetMut chain with MakeStruct(0) does not overflow reuseConstructors"
  ) {
    assertReuseConstructorsNoStackOverflow(nestedLetMut(20000))
  }

  test(
    "issue 1652: deep Let chain with MakeStruct(0) does not overflow reuseConstructors"
  ) {
    assertReuseConstructorsNoStackOverflow(nestedLet(20000))
  }

  test(
    "issue 1652: deeply nested LetMutBool condition does not overflow reuseConstructors"
  ) {
    val expr =
      Matchless.If(
        nestedLetMutBool(20000),
        Matchless.MakeStruct(0),
        Matchless.MakeStruct(0)
      )
    assertReuseConstructorsNoStackOverflow(expr)
  }

  test("MatchlessToValue evaluates static If conditions without dynamic branching") {
    val trueIf =
      Matchless.If(
        Matchless.TrueConst,
        Matchless.Literal(Lit(1)),
        Matchless.Literal(Lit(2))
      )
    val falseIf =
      Matchless.If(
        Matchless.EqualsLit(Matchless.Literal(Lit(1)), Lit(2)),
        Matchless.Literal(Lit(3)),
        Matchless.Literal(Lit(4))
      )

    val evaluated =
      MatchlessToValue
        .traverse(Vector(trueIf, falseIf))((_, _, _) => Eval.now(Value.UnitValue))
        .map(_.value)

    assertEquals(evaluated, Vector(Value.VInt(1), Value.VInt(4)))
  }
}
