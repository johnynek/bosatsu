package dev.bosatsu

import cats.data.NonEmptyList

import Identifier.Bindable

class MatchlessApplyArgsTest extends munit.FunSuite {
  import scala.language.implicitConversions

  given [A]: Conversion[Matchless.SourceInfo => A, A] with
    def apply(fn: Matchless.SourceInfo => A): A =
      fn(Matchless.SourceInfo.empty)

  test(
    "Matchless.recoverTopLevelLambda beta-reduces let-bound lambda aliases"
  ) {
    val fnName1 = Identifier.Name("fn1")
    val fnName2 = Identifier.Name("fn2")
    val arg1 = Identifier.Name("arg1")
    val arg2 = Identifier.Name("arg2")

    val lam1: Matchless.Expr[Unit] =
      Matchless.Lambda(captures = Nil, recursiveName = None, args = NonEmptyList.one(arg1), body = Matchless.Literal(Lit(0), Matchless.SourceInfo.empty), Matchless.SourceInfo.empty)
    val lam2: Matchless.Expr[Unit] =
      Matchless.Lambda(captures = Nil, recursiveName = None, args = NonEmptyList.one(arg2), body = Matchless.Literal(Lit(0), Matchless.SourceInfo.empty), Matchless.SourceInfo.empty)

    val branch1: Matchless.Expr[Unit] =
      Matchless.Let(Right(fnName1), lam1, Matchless.Local(fnName1, Matchless.SourceInfo.empty), Matchless.SourceInfo.empty)
    val branch2: Matchless.Expr[Unit] =
      Matchless.Let(Right(fnName2), lam2, Matchless.Local(fnName2, Matchless.SourceInfo.empty), Matchless.SourceInfo.empty)
    val expr: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, branch1, branch2, Matchless.SourceInfo.empty)

    Matchless.recoverTopLevelLambda(expr) match {
      case Matchless.Lambda(Nil, None, args, body, _) =>
        val topArg: Matchless.Expr[Unit] = Matchless.Local(args.head, Matchless.SourceInfo.empty)
        def assertReducedAliasCall(
            branch: Matchless.Expr[Unit],
            fnName: Bindable,
            lamExpr: Matchless.Expr[Unit],
            lamArg: Bindable
        ): Unit =
          branch match {
            case Matchless.Let(Right(`fnName`), `lamExpr`, Matchless.Let(Right(tmp), `topArg`, Matchless.Let(Right(`lamArg`), Matchless.Local(tmpRef, _), Matchless.Literal(lit, _), _), _), _) =>
              assertEquals(lit, Lit(0))
              assertEquals(tmp, tmpRef)
            case Matchless.Let(Right(`fnName`), `lamExpr`, trailing, _)
                if trailing == Matchless.App(
                  Matchless.Local(fnName, Matchless.SourceInfo.empty),
                  NonEmptyList.one(topArg),
                  Matchless.SourceInfo.empty
                ) =>
              fail(
                s"expected beta-reduced let-bound lambda alias, found trailing apply: $branch"
              )
            case other =>
              fail(
                s"expected let-bound alias call to beta-reduce, found: $other"
              )
          }

        body match {
          case Matchless.If(Matchless.TrueConst, tBranch, fBranch, _) =>
            assertReducedAliasCall(tBranch, fnName1, lam1, arg1)
            assertReducedAliasCall(fBranch, fnName2, lam2, arg2)
          case other =>
            fail(s"expected recovered branch if, found: $other")
        }
      case other =>
        fail(s"expected recovered lambda, found: $other")
    }
  }
}
