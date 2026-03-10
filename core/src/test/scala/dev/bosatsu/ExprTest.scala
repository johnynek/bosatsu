package dev.bosatsu

import cats.{Applicative, Eq}
import cats.data.{Chain, NonEmptyList, Writer}
import cats.syntax.all._
import dev.bosatsu.rankn.Type
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

class ExprTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 5000 else 500
    )

  val genExpr: Gen[Expr[Int]] = Generators.Exprs.gen(Gen.choose(0, 99), 4)

  implicit val arbExpr: Arbitrary[Expr[Int]] = Arbitrary(genExpr)

  private def traverseTypeOracle[T, F[_]](
      expr: Expr[T],
      bound: Set[Type.Var.Bound]
  )(
      fn: (Type, Set[Type.Var.Bound]) => F[Type]
  )(implicit F: Applicative[F]): F[Expr[T]] =
    expr match {
      case Expr.Annotation(e, tpe, a) =>
        (traverseTypeOracle[T, F](e, bound)(fn), fn(tpe, bound))
          .mapN(Expr.Annotation(_, _, a))
      case v: Expr.Name[T]   => F.pure(v)
      case Expr.App(f, args, t) =>
        (
          traverseTypeOracle[T, F](f, bound)(fn),
          args.traverse(traverseTypeOracle[T, F](_, bound)(fn))
        )
          .mapN(Expr.App(_, _, t))
      case Expr.Generic(bs, in) =>
        val bound1 = bound ++ bs.toList.iterator.map(_._1)
        traverseTypeOracle[T, F](in, bound1)(fn).map(Expr.Generic(bs, _))
      case Expr.Lambda(args, expr, t) =>
        (
          args.traverse { case (n, optT) =>
            optT.traverse(fn(_, bound)).map((n, _))
          },
          traverseTypeOracle[T, F](expr, bound)(fn)
        ).mapN(Expr.Lambda(_, _, t))
      case Expr.Let(arg, exp, in, rec, tag) =>
        (
          traverseTypeOracle[T, F](exp, bound)(fn),
          traverseTypeOracle[T, F](in, bound)(fn)
        )
          .mapN(Expr.Let(arg, _, _, rec, tag))
      case l @ Expr.Literal(_, _)   => F.pure(l)
      case Expr.Match(arg, branches, tag) =>
        val argB = traverseTypeOracle[T, F](arg, bound)(fn)
        type B = Expr.Branch[T]
        def branchFn(b: B): F[B] =
          (
            b.pattern.traverseType(fn(_, bound)),
            b.guard.traverse(traverseTypeOracle[T, F](_, bound)(fn)),
            traverseTypeOracle[T, F](b.expr, bound)(fn)
          ).mapN { (pat, guard, expr) =>
            Expr.Branch(pat, guard, expr)(using b.patternRegion)
          }
        val branchB = branches.traverse(branchFn)
        (argB, branchB).mapN(Expr.Match(_, _, tag))
    }

  private def freeBoundTyVarsViaTraverseType[A](expr: Expr[A]): List[Type.Var.Bound] = {
    val w = traverseTypeOracle(expr, Set.empty) { (t, bound) =>
      val frees = Chain.fromSeq(Type.freeBoundTyVars(t :: Nil))
      Writer(frees.filterNot(bound), t)
    }
    w.written.iterator.toList.distinct
  }

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
                  Expr.Branch(Pattern.Named(n, pat), None, expr)(using
                    Region.empty
                  )
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

  test("freeBoundTyVars matches traverseType oracle") {
    forAll(genExpr) { e =>
      assertEquals(
        Expr.freeBoundTyVars(e),
        freeBoundTyVarsViaTraverseType(e)
      )
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
