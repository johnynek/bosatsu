package dev.bosatsu

class MatchlessRegressionTest extends munit.FunSuite {
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
      case Matchless.MatchString(arg, _, _, _) =>
        countWhileExprs(arg)
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
}
