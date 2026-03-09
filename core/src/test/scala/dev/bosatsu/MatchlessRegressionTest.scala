package dev.bosatsu

import cats.Eval
import cats.data.NonEmptyList
import dev.bosatsu.rankn.DataRepr
import dev.bosatsu.rankn.Type

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

  private def nestedTypedLet(depth: Int): TypedExpr[Unit] = {
    val intT = Type.IntType
    val base = TypedExpr.Literal(Lit.fromInt(0), intT, ())

    (0 until depth).foldRight(base: TypedExpr[Unit]) { (idx, in) =>
      val n = Identifier.synthetic(s"typed_let_$idx")
      TypedExpr.Let(
        n,
        TypedExpr.Literal(Lit.fromInt(idx), intT, ()),
        in,
        RecursionKind.NonRecursive,
        ()
      )
    }
  }

  private def assertMatchlessFromLetNoStackOverflow(
      te: TypedExpr[Unit],
      stackBytes: Long
  ): Unit = {
    val variantOf: (PackageName, Identifier.Constructor) => Option[DataRepr] = {
      case (PackageName.PredefName, Identifier.Constructor("EmptyList")) =>
        Some(DataRepr.Enum(0, 0, List(0, 1)))
      case (PackageName.PredefName, Identifier.Constructor("NonEmptyList")) =>
        Some(DataRepr.Enum(1, 2, List(0, 1)))
      case _ =>
        Some(DataRepr.Struct(0))
    }

    @volatile var failure: Option[Throwable] = None

    val thread = new Thread(
      null,
      new Runnable {
        def run(): Unit =
          try {
            Matchless.fromLet(
              (),
              Identifier.Name("out"),
              RecursionKind.NonRecursive,
              te
            )(variantOf): Unit
            ()
          } catch {
            case t: Throwable =>
              failure = Some(t)
          }
      },
      "matchless-fromLet-small-stack",
      stackBytes
    )

    thread.start()
    thread.join()

    failure match {
      case Some(_: StackOverflowError) =>
        fail("Matchless.fromLet should not overflow on deeply nested TypedExpr lets")
      case Some(other) =>
        fail(s"unexpected failure compiling deep TypedExpr let-chain: $other")
      case None =>
        ()
    }
  }

  private def assertReuseConstructorsNoStackOverflow(
      expr: Matchless.Expr[Unit]
  ): Unit =
    try {
      Matchless.reuseConstructors(expr): Unit
      ()
    } catch {
      case _: StackOverflowError =>
        fail(
          "reuseConstructors should not overflow on deeply nested expressions"
        )
    }

  private def assertMatchlessToValueNoStackOverflow(
      expr: Matchless.Expr[Unit]
  ): Unit =
    try {
      val evaluated =
        MatchlessToValue
          .traverse(Vector(expr))((_, _, _) => Eval.now(Value.UnitValue))
          .map(_.value)
      assertEquals(evaluated, Vector(Value.UnitValue))
    } catch {
      case _: StackOverflowError =>
        fail(
          "MatchlessToValue should not overflow on deeply nested expressions"
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
      case Matchless.CheckVariantSet(expr, _, _, _) =>
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

  private def countSelfRecursiveCalls(expr: Matchless.Expr[Unit]): Int = {
    def loopBool(
        b: Matchless.BoolExpr[Unit],
        activeRecNames: Set[Identifier.Bindable]
    ): Int =
      b match {
        case Matchless.EqualsLit(expr, _) =>
          loopExpr(expr, activeRecNames)
        case Matchless.EqualsNat(expr, _) =>
          loopExpr(expr, activeRecNames)
        case Matchless.And(left, right) =>
          loopBool(left, activeRecNames) + loopBool(right, activeRecNames)
        case Matchless.CheckVariant(expr, _, _, _) =>
          loopExpr(expr, activeRecNames)
        case Matchless.CheckVariantSet(expr, _, _, _) =>
          loopExpr(expr, activeRecNames)
        case Matchless.SetMut(_, expr) =>
          loopExpr(expr, activeRecNames)
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value, activeRecNames) + loopBool(in, activeRecNames)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in, activeRecNames)
        case Matchless.TrueConst =>
          0
      }

    def loopExpr(
        e: Matchless.Expr[Unit],
        activeRecNames: Set[Identifier.Bindable]
    ): Int =
      e match {
        case Matchless.Lambda(captures, recursiveName, args, body) =>
          val argNames = args.toList.toSet
          val recNamesInLambda = activeRecNames -- argNames
          val recNamesInBody =
            recursiveName match {
              case Some(fnName) if !argNames(fnName) => recNamesInLambda + fnName
              case _                                  => recNamesInLambda
            }
          captures.map(loopExpr(_, recNamesInLambda)).sum +
            loopExpr(body, recNamesInBody)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond, activeRecNames) + loopExpr(effectExpr, activeRecNames)
        case Matchless.App(fn, args) =>
          val headCallsSelf = fn match {
            case Matchless.Local(fnName) if activeRecNames(fnName) => 1
            case _                                                 => 0
          }
          headCallsSelf + loopExpr(fn, activeRecNames) + args.toList
            .map(loopExpr(_, activeRecNames))
            .sum
        case Matchless.Let(Right(name), value, in) =>
          loopExpr(value, activeRecNames) + loopExpr(in, activeRecNames - name)
        case Matchless.Let(_, value, in) =>
          loopExpr(value, activeRecNames) + loopExpr(in, activeRecNames)
        case Matchless.LetMut(_, in) =>
          loopExpr(in, activeRecNames)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond, activeRecNames) +
            loopExpr(thenExpr, activeRecNames) +
            loopExpr(elseExpr, activeRecNames)
        case Matchless.SwitchVariant(on, _, cases, default) =>
          loopExpr(on, activeRecNames) + cases.iterator.map {
            case (_, branch) =>
              loopExpr(branch, activeRecNames)
          }.sum + default.fold(0)(loopExpr(_, activeRecNames))
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond, activeRecNames) + loopExpr(thenExpr, activeRecNames)
        case Matchless.PrevNat(of) =>
          loopExpr(of, activeRecNames)
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopExpr(arg, activeRecNames)
        case Matchless.GetStructElement(arg, _, _) =>
          loopExpr(arg, activeRecNames)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.ClosureSlot(_) | Matchless.LocalAnon(_) |
            Matchless.LocalAnonMut(_) | Matchless.Literal(_) |
            Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          0
      }

    loopExpr(expr, Set.empty)
  }

  test("polymorphic recursion lowers to while in Matchless without self-calls") {
    TestUtils.checkMatchless("""
struct Box[a](box: a)

enum Nat: Z, S(n: Nat)

def box_more[a](n: Nat, b: Box[a]) -> Nat:
  recur n:
    case Z: Z
    case S(n): box_more(n, Box(b))
""") { binds =>
      val byName = binds(TestUtils.testPackage).toMap
      val expr = byName(Identifier.Name("box_more"))
      val whileCount = countWhileExprs(expr)
      assertEquals(whileCount, 1, expr.toString)
      val selfCallCount = countSelfRecursiveCalls(expr)
      assertEquals(selfCallCount, 0, expr.toString)
    }
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

  test(
    "issue 1942: deeply nested LetMutBool condition does not overflow MatchlessToValue"
  ) {
    val expr =
      Matchless.If(
        nestedLetMutBool(20000),
        Matchless.MakeStruct(0),
        Matchless.MakeStruct(0)
      )
    assertMatchlessToValueNoStackOverflow(expr)
  }

  test(
    "issue 1942: deep LetMut chain with MakeStruct(0) does not overflow MatchlessToValue"
  ) {
    assertMatchlessToValueNoStackOverflow(nestedLetMut(20000))
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

  test("MatchlessToValue evaluates CheckVariantSet guards") {
    val famArities = 0 :: 0 :: 0 :: 0 :: 0 :: Nil
    val arg = Identifier.Name("v")
    val enumGuard: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(arg),
        Matchless.If(
          Matchless.CheckVariantSet(
            Matchless.Local(arg),
            NonEmptyList.of(0, 2, 4),
            0,
            famArities
          ),
          Matchless.Literal(Lit(1)),
          Matchless.Literal(Lit(0))
        )
      )

    val evalExprs = Vector(
      Matchless.App(
        enumGuard,
        NonEmptyList.one(Matchless.MakeEnum(0, 0, famArities))
      ),
      Matchless.App(
        enumGuard,
        NonEmptyList.one(Matchless.MakeEnum(1, 0, famArities))
      )
    )

    val evaluated =
      MatchlessToValue
        .traverse(evalExprs)((_, _, _) => Eval.now(Value.UnitValue))
        .map(_.value)
    assertEquals(evaluated, Vector(Value.VInt(1), Value.VInt(0)))
  }

  Platform.onJvm(
    test("deep TypedExpr non-rec let chains lower to Matchless without stack overflow") {
      val depth = sys.props.get("repro.typedLetDepth").fold(10000)(_.toInt)
      val stackBytes = sys.props.get("repro.stackBytes").fold(96L * 1024L)(_.toLong)
      assertMatchlessFromLetNoStackOverflow(nestedTypedLet(depth), stackBytes)
    }
  )

  test("SwitchVariant.toIfElse preserves MatchlessToValue semantics") {
    val famArities = 0 :: 0 :: 0 :: 0 :: 0 :: Nil
    val arg = Identifier.Name("v")
    val switchBody: Matchless.SwitchVariant[Unit] =
      Matchless.SwitchVariant(
        Matchless.Local(arg),
        famArities,
        NonEmptyList.of(
          0 -> Matchless.Literal(Lit(10)),
          2 -> Matchless.Literal(Lit(20)),
          3 -> Matchless.Literal(Lit(30)),
          4 -> Matchless.Literal(Lit(40))
        ),
        Some(Matchless.Literal(Lit(99)))
      )
    val switchExpr: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(arg),
        switchBody
      )
    val ifElseExpr: Matchless.Expr[Unit] =
      Matchless.Lambda(
        Nil,
        None,
        NonEmptyList.one(arg),
        switchBody.toIfElse
      )

    val evalExprs = Vector(
      Matchless.App(
        switchExpr,
        NonEmptyList.one(Matchless.MakeEnum(2, 0, famArities))
      ),
      Matchless.App(
        ifElseExpr,
        NonEmptyList.one(Matchless.MakeEnum(2, 0, famArities))
      ),
      Matchless.App(
        switchExpr,
        NonEmptyList.one(Matchless.MakeEnum(1, 0, famArities))
      ),
      Matchless.App(
        ifElseExpr,
        NonEmptyList.one(Matchless.MakeEnum(1, 0, famArities))
      ),
      Matchless.App(
        switchExpr,
        NonEmptyList.one(Matchless.MakeEnum(4, 0, famArities))
      ),
      Matchless.App(
        ifElseExpr,
        NonEmptyList.one(Matchless.MakeEnum(4, 0, famArities))
      )
    )

    val evaluated =
      MatchlessToValue
        .traverse(evalExprs)((_, _, _) => Eval.now(Value.UnitValue))
        .map(_.value)
    assertEquals(
      evaluated,
      Vector(
        Value.VInt(20),
        Value.VInt(20),
        Value.VInt(99),
        Value.VInt(99),
        Value.VInt(40),
        Value.VInt(40)
      )
    )
  }
}
