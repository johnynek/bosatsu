package dev.bosatsu

import cats.Order

class Issue1633Test extends munit.FunSuite with ParTest {
  private val reproSource = """
package MyLib/ReproMin8

export ParseStep, main

def drop_ws(s: String) -> String:
  recur s:
    case " ${tail}":
      drop_ws(tail)
    case "\n${tail}":
      drop_ws(tail)
    case "\t${tail}":
      drop_ws(tail)
    case "\r${tail}":
      drop_ws(tail)
    case _:
      s

enum ParseStep:
  ParseDone(result: Option[String])

def parse_value(input: String) -> ParseStep:
  rest = drop_ws(input)
  match rest:
    case "null${_}":
      ParseDone(Some("ok"))
    case "\"${_}":
      ParseDone(None)
    case _:
      ParseDone(None)

tests = TestSuite("ReproMin8 tests", [
  Assertion(parse_value("null") matches ParseDone(Some("ok")), "no ws"),
  Assertion(parse_value(" null") matches ParseDone(Some("ok")), "leading ws"),
])

main = parse_value(" null")
"""

  private val reproPackage = PackageName.parts("MyLib", "ReproMin8")
  private val parseValueName = Identifier.Name("parse_value")
  private val mainName = Identifier.Name("main")
  private val capturedBName = Identifier.Name("b")

  private def withRepro[A](
      fn: (PackageMap.Inferred, PackageName) => A
  ): A = {
    var out: Option[A] = None
    TestUtils.testInferred(
      List(reproSource),
      reproPackage.asString,
      (pm, pn) => out = Some(fn(pm, pn))
    )
    out match {
      case Some(value) => value
      case None        => fail("failed to compute issue 1633 repro result")
    }
  }

  private def typedHasLoop[A](te: TypedExpr[A]): Boolean =
    te match {
      case TypedExpr.Generic(_, in) =>
        typedHasLoop(in)
      case TypedExpr.Annotation(in, _, _) =>
        typedHasLoop(in)
      case TypedExpr.AnnotatedLambda(_, in, _) =>
        typedHasLoop(in)
      case TypedExpr.App(fn, args, _, _) =>
        typedHasLoop(fn) || args.exists(typedHasLoop(_))
      case TypedExpr.Let(_, arg, in, _, _) =>
        typedHasLoop(arg) || typedHasLoop(in)
      case TypedExpr.Loop(_, _, _) =>
        true
      case TypedExpr.Recur(args, _, _) =>
        args.exists(typedHasLoop(_))
      case TypedExpr.Match(arg, branches, _) =>
        typedHasLoop(arg) || branches.exists { branch =>
          branch.guard.exists(typedHasLoop(_)) || typedHasLoop(branch.expr)
        }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        false
    }

  private def typedHasRecur[A](te: TypedExpr[A]): Boolean =
    te match {
      case TypedExpr.Generic(_, in) =>
        typedHasRecur(in)
      case TypedExpr.Annotation(in, _, _) =>
        typedHasRecur(in)
      case TypedExpr.AnnotatedLambda(_, in, _) =>
        typedHasRecur(in)
      case TypedExpr.App(fn, args, _, _) =>
        typedHasRecur(fn) || args.exists(typedHasRecur(_))
      case TypedExpr.Let(_, arg, in, _, _) =>
        typedHasRecur(arg) || typedHasRecur(in)
      case TypedExpr.Loop(args, in, _) =>
        args.exists { case (_, arg) => typedHasRecur(arg) } || typedHasRecur(in)
      case TypedExpr.Recur(_, _, _) =>
        true
      case TypedExpr.Match(arg, branches, _) =>
        typedHasRecur(arg) || branches.exists { branch =>
          branch.guard.exists(typedHasRecur(_)) || typedHasRecur(branch.expr)
        }
      case TypedExpr.Local(_, _, _) | TypedExpr.Global(_, _, _, _) |
          TypedExpr.Literal(_, _, _) =>
        false
    }

  private def hasClosureSlotApply[A](expr: Matchless.Expr[A]): Boolean = {
    def loopExpr(e: Matchless.Expr[A]): Boolean =
      e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(loopExpr) || loopExpr(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond) || loopExpr(effectExpr)
        case Matchless.App(Matchless.ClosureSlot(_), _) =>
          true
        case Matchless.App(fn, args) =>
          loopExpr(fn) || args.exists(loopExpr)
        case Matchless.Let(_, value, in) =>
          loopExpr(value) || loopExpr(in)
        case Matchless.LetMut(_, in) =>
          loopExpr(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond) || loopExpr(thenExpr) || loopExpr(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond) || loopExpr(thenExpr)
        case Matchless.PrevNat(of) =>
          loopExpr(of)
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loopExpr(arg)
        case Matchless.GetStructElement(arg, _, _) =>
          loopExpr(arg)
        case Matchless.Local(_) | Matchless.Global(_, _, _) |
            Matchless.ClosureSlot(_) | Matchless.LocalAnon(_) |
            Matchless.LocalAnonMut(_) | Matchless.Literal(_) |
            Matchless.MakeEnum(_, _, _) | Matchless.MakeStruct(_) |
            Matchless.ZeroNat | Matchless.SuccNat =>
          false
      }

    def loopBool(b: Matchless.BoolExpr[A]): Boolean =
      b match {
        case Matchless.EqualsLit(expr, _) =>
          loopExpr(expr)
        case Matchless.EqualsNat(expr, _) =>
          loopExpr(expr)
        case Matchless.And(left, right) =>
          loopBool(left) || loopBool(right)
        case Matchless.CheckVariant(expr, _, _, _) =>
          loopExpr(expr)
        case Matchless.SetMut(_, expr) =>
          loopExpr(expr)
        case Matchless.TrueConst =>
          false
        case Matchless.LetBool(_, value, in) =>
          loopExpr(value) || loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
      }

    loopExpr(expr)
  }

  test(
    "issue 1633: loop+string-match lowering avoids captured non-function apply"
  ) {
    withRepro { (pm, _) =>
      val pack = pm.toMap.getOrElse(
        reproPackage,
        fail(s"missing inferred package: ${reproPackage.asString}")
      )
      val parseValueTyped = pack.lets.find(_._1 == parseValueName) match {
        case Some((_, _, te)) => te
        case None             =>
          fail(s"missing ${parseValueName.sourceCodeRepr} in typed lets")
      }
      assert(typedHasLoop(parseValueTyped), parseValueTyped.repr.render(100))
      assert(typedHasRecur(parseValueTyped), parseValueTyped.repr.render(100))

      given Order[Unit] = Order.fromOrdering
      val compiled = MatchlessFromTypedExpr.compile((), pm)
      val parseValueMatchless = compiled
        .getOrElse(
          reproPackage,
          fail(s"missing compiled package: ${reproPackage.asString}")
        )
        .find(_._1 == parseValueName) match {
        case Some((_, expr)) => expr
        case None            =>
          fail(s"missing ${parseValueName.sourceCodeRepr} in matchless lets")
      }
      parseValueMatchless match {
        case Matchless.Let(
              Right(`capturedBName`),
              _,
              Matchless.Lambda(captures, _, _, _)
            ) =>
          assertEquals(captures, Matchless.Local(capturedBName) :: Nil)
        case other =>
          fail(s"unexpected parse_value lowering shape: $other")
      }

      assertEquals(
        hasClosureSlotApply(parseValueMatchless),
        false,
        parseValueMatchless.toString
      )
    }
  }

  test("issue 1633: eval main should produce successful parse payload") {
    withRepro { (pm, mainPack) =>
      val pack = pm.toMap.getOrElse(
        reproPackage,
        fail(s"missing inferred package: ${reproPackage.asString}")
      )
      assert(
        pack.lets.exists(_._1 == mainName),
        s"missing ${mainName.sourceCodeRepr}"
      )

      val ev = library.LibraryEvaluation.fromPackageMap(pm, Predef.jvmExternals)
      val (mainEval, _) =
        ev.evaluateMainValue(mainPack).fold(err => fail(err.toString), identity)

      val expected = Value.VOption.some(Value.Str("ok"))
      assertEquals(mainEval.value, expected)
    }
  }
}
