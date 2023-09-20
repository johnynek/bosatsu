package org.bykn.bosatsu

import cats.data.NonEmptyList
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Gen}

class ExprTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 5000)

  val genExpr: Gen[Expr[Int]] = Generators.Exprs.gen(Gen.choose(0, 99), 4)

  implicit val arbExpr: Arbitrary[Expr[Int]] = Arbitrary(genExpr)

  test("replaceTag replaces") {
    forAll { (s: Expr[Int], i: Int) =>
      assert(s.replaceTag(i).tag == i)
    }
  }

  test("e.notFree(n) is true if n is not free in e") {
    forAll(genExpr, Generators.bindIdentGen) { (expr, n) =>
      assert(expr.notFree(n) == !expr.freeVarsDup.contains(n))
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
              .Match(bind, NonEmptyList.one((Pattern.Named(n, pat), expr)), 0)
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
        assert(Expr.lets(lets.toList, res) == let)

        if (i == res) {
          assert(lets.length == 1)
          assert(lets.head == (n, r, b, tag))
        } else {
          i match {
            case l1 @ Expr.Let(_, _, _, _, _) =>
              val (lets1, res1) = l1.flatten
              assert(res == res1)
              assert((n, r, b, tag) :: lets1 == lets)
            case other =>
              fail(s"expected Let: $other")
          }
        }
      case _ => ()
    }
  }
}
