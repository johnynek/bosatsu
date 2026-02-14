package dev.bosatsu

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class ExprTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)

  val genExpr: Gen[Expr[Int]] = Generators.Exprs.gen(Gen.choose(0, 99), 4)

  implicit val arbExpr: Arbitrary[Expr[Int]] = Arbitrary(genExpr)

  test("replaceTag replaces") {
    forAll { (s: Expr[Int], i: Int) =>
      assertEquals(s.replaceTag(i).tag, i)
    }
  }

  test("e.notFree(n) is true if n is not free in e") {
    forAll(genExpr, Generators.bindIdentGen) { (expr, n) =>
      assertEquals(expr.notFree(n), !expr.freeVarsDup.contains(n))
    }
  }

  test("if we bind a free variable, it's no longer free") {
    forAll(
      genExpr,
      Generators.bindIdentGen,
      Generators.genCompiledPattern(4),
      genExpr
    ) { (expr, n, pat, bind) =>
      if (expr.notFree(n)) ()
      else {
        // n is free in expr
        assert(Expr.Lambda(NonEmptyList.one((n, None)), expr, 0).notFree(n))
        if (bind.notFree(n)) {
          assert(
            Expr.Let(n, bind, expr, RecursionKind.NonRecursive, 0).notFree(n)
          )
          assert(
            Expr
              .Match(
                bind,
                NonEmptyList.one(
                  Expr.Branch(Pattern.Named(n, pat), None, expr)
                ),
                0
              )
              .notFree(n)
          )
        }
      }
    }
  }

  test("allNames is a superset of freeVars") {
    forAll(genExpr) { e =>
      val allNames = Expr.allNames(e)
      assert(e.freeVarsDup.forall(allNames))
    }
  }

  test("let flattening makes sense") {
    forAll(genExpr) {
      case let @ Expr.Let(n, b, i, r, tag) =>
        val (lets, res) = let.flatten
        assertEquals(Expr.lets(lets.toList, res), let)

        given Eq[Expr[Int]] =
          // Safe: Expr is an immutable AST with structural equals.
          Eq.fromUniversalEquals
        if (i === res) {
          assertEquals(lets.length, 1)
          assertEquals(lets.head, (n, r, b, tag))
        } else {
          i match {
            case l1 @ Expr.Let(_, _, _, _, _) =>
              val (lets1, res1) = l1.flatten
              assertEquals(res, res1)
              assertEquals((n, r, b, tag) :: lets1, lets)
            case other =>
              fail(s"expected Let: $other")
          }
        }
      case _ => ()
    }
  }
}
