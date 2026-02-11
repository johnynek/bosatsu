package dev.bosatsu

import cats.data.NonEmptyList

import Identifier.Bindable

class MatchlessApplyArgsTest extends munit.FunSuite {
  test("Matchless.recoverTopLevelLambda beta-reduces let-bound lambda aliases") {
    val fnName1 = Identifier.Name("fn1")
    val fnName2 = Identifier.Name("fn2")
    val arg1 = Identifier.Name("arg1")
    val arg2 = Identifier.Name("arg2")

    val lam1: Matchless.Expr[Unit] =
      Matchless.Lambda(
        captures = Nil,
        recursiveName = None,
        args = NonEmptyList.one(arg1),
        body = Matchless.Literal(Lit(0))
      )
    val lam2: Matchless.Expr[Unit] =
      Matchless.Lambda(
        captures = Nil,
        recursiveName = None,
        args = NonEmptyList.one(arg2),
        body = Matchless.Literal(Lit(0))
      )

    val branch1: Matchless.Expr[Unit] =
      Matchless.Let(Right(fnName1), lam1, Matchless.Local(fnName1))
    val branch2: Matchless.Expr[Unit] =
      Matchless.Let(Right(fnName2), lam2, Matchless.Local(fnName2))
    val expr: Matchless.Expr[Unit] =
      Matchless.If(Matchless.TrueConst, branch1, branch2)

    Matchless.recoverTopLevelLambda(expr) match {
      case Matchless.Lambda(Nil, None, args, body) =>
        val topArg: Matchless.Expr[Unit] = Matchless.Local(args.head)
        def assertReducedAliasCall(
            branch: Matchless.Expr[Unit],
            fnName: Bindable,
            lamExpr: Matchless.Expr[Unit],
            lamArg: Bindable
        ): Unit =
          branch match {
            case Matchless.Let(
                  Right(`fnName`),
                  `lamExpr`,
                  Matchless.Let(
                    Right(tmp),
                    `topArg`,
                    Matchless.Let(
                      Right(`lamArg`),
                      Matchless.Local(tmpRef),
                      Matchless.Literal(lit)
                    )
                  )
                ) =>
              assertEquals(lit, Lit(0))
              assertEquals(tmp, tmpRef)
            case Matchless.Let(
                  Right(`fnName`),
                  `lamExpr`,
                  trailing
                ) if trailing == Matchless.App(
                  Matchless.Local(fnName),
                  NonEmptyList.one(topArg)
                ) =>
              fail(s"expected beta-reduced let-bound lambda alias, found trailing apply: $branch")
            case other =>
              fail(s"expected let-bound alias call to beta-reduce, found: $other")
          }

        body match {
          case Matchless.If(Matchless.TrueConst, tBranch, fBranch) =>
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
