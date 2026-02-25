package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension

class MatchlessInterfaceTest extends munit.FunSuite {

  private def typeCheck(
      src: String,
      ifaces: List[Package.Interface]
  ): PackageMap.Inferred = {
    val pack = Parser.unsafeParse(Package.parser(None), src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, ifaces, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
    }
  }

  private def exprSourceInfos(
      expr: Matchless.Expr[Unit]
  ): List[Matchless.SourceInfo] = {
    val nested =
      expr match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.flatMap(exprSourceInfos) ::: exprSourceInfos(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          boolSourceInfos(cond) ::: exprSourceInfos(effectExpr)
        case Matchless.App(fn, args) =>
          exprSourceInfos(fn) ::: args.toList.flatMap(exprSourceInfos)
        case Matchless.Let(_, value, in) =>
          exprSourceInfos(value) ::: exprSourceInfos(in)
        case Matchless.LetMut(_, in) =>
          exprSourceInfos(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          boolSourceInfos(cond) ::: exprSourceInfos(
            thenExpr
          ) ::: exprSourceInfos(elseExpr)
        case Matchless.Always(cond, thenExpr) =>
          boolSourceInfos(cond) ::: exprSourceInfos(thenExpr)
        case Matchless.PrevNat(of) =>
          exprSourceInfos(of)
        case _ =>
          Nil
      }

    expr.sourceInfo :: nested
  }

  private def boolSourceInfos(
      boolExpr: Matchless.BoolExpr[Unit]
  ): List[Matchless.SourceInfo] = {
    val nested =
      boolExpr match {
        case Matchless.EqualsLit(expr, _) =>
          exprSourceInfos(expr)
        case Matchless.EqualsNat(expr, _) =>
          exprSourceInfos(expr)
        case Matchless.And(left, right) =>
          boolSourceInfos(left) ::: boolSourceInfos(right)
        case Matchless.CheckVariant(expr, _, _, _) =>
          exprSourceInfos(expr)
        case Matchless.SetMut(_, expr) =>
          exprSourceInfos(expr)
        case Matchless.LetBool(_, value, in) =>
          exprSourceInfos(value) ::: boolSourceInfos(in)
        case Matchless.LetMutBool(_, in) =>
          boolSourceInfos(in)
        case Matchless.TrueConst =>
          Nil
      }

    boolExpr.sourceInfo :: nested
  }

  test("Matchless can compile constructors from imported interfaces") {
    val natSrc =
      """package Bosatsu/Num/Nat
        |
        |export Nat()
        |
        |enum Nat: Zero, Succ(prev: Nat)
        |""".stripMargin

    val natPm = typeCheck(natSrc, Nil)
    val natPack =
      natPm.toMap(PackageName.parts("Bosatsu", "Num", "Nat"))
    val natIface = Package.interfaceOf(natPack)

    val fibSrc =
      """package My/Fib
        |
        |from Bosatsu/Num/Nat import Nat, Zero, Succ
        |
        |def pred_or_zero(n: Nat) -> Nat:
        |  match n:
        |    case Zero: Zero
        |    case Succ(n): n
        |""".stripMargin

    val fibPm = typeCheck(fibSrc, natIface :: Nil)

    Par.withEC {
      given Order[Unit] = Order.fromOrdering
      val compiled = MatchlessFromTypedExpr.compile((), fibPm)
      assert(compiled.contains(PackageName.parts("My", "Fib")))
      val infos = compiled(PackageName.parts("My", "Fib")).flatMap {
        case (_, expr) => exprSourceInfos(expr)
      }
      assert(infos.nonEmpty)
      assert(
        infos.forall(_.packageHashIdent == Package.emptySourceHashIdent),
        infos.toString
      )
    }
  }
}
