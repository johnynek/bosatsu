package dev.bosatsu

import cats.{Applicative, Eq, Id}
import cats.data.{Chain, NonEmptyList, Writer}
import cats.syntax.all._
import dev.bosatsu.rankn.Type
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scala.concurrent.duration.DurationInt

class ExprTest extends munit.ScalaCheckSuite {
  override val munitTimeout = 2.minutes

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

  private def deepRightApp2Chain(depth: Int): Expr[Int] = {
    var cursor: Expr[Int] = Expr.Local(Identifier.Name("z"), -1)
    var idx = depth - 1
    while (idx >= 0) {
      val fn = Expr.Local(Identifier.Name(s"f$idx"), idx)
      val arg = Expr.Local(Identifier.Name(s"x$idx"), idx)
      cursor = Expr.App(fn, NonEmptyList.of(arg, cursor), idx)
      idx = idx - 1
    }
    cursor
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

  test("Expr flattenApp2/rebuildApp2 round trips right-deep binary app chains") {
    val app = deepRightApp2Chain(64) match {
      case app: Expr.App[Int] => app
      case other              => fail(s"expected App, got $other")
    }
    val (steps, last) = Expr.flattenApp2(app).getOrElse(
      fail("expected flattenApp2 to recognize right-deep app2 chain")
    )
    assertEquals(Expr.rebuildApp2(steps, last), app)
  }

  Platform.onJvm(
    test("Expr recursive app utilities are stack safe on right-deep binary app chains") {
      val depth = sys.props.get("repro.exprApp2Depth").fold(2000)(_.toInt)
      val stackBytes = sys.props.get("repro.stackBytes").fold(96L * 1024L)(_.toLong)

      @volatile var failure: Option[Throwable] = None

      val thread = new Thread(
        null,
        new Runnable {
          def run(): Unit =
            try {
              val expr = deepRightApp2Chain(depth)
              val expectedVars = (2 * depth) + 1

              assertEquals(expr.freeVarsDup.length, expectedVars)
              assertEquals(Expr.allNames(expr).size, expectedVars)
              assertEquals(expr.globals, Set.empty[Expr.Global[Int]])
              assertEquals(expr.eraseTags.freeVarsDup.length, expectedVars)

              val tv = Type.Var.Bound("a")
              val annotated = Expr.Annotation(expr, Type.TyVar(tv), depth)
              val (sks, expr1) =
                Expr.skolemizeVars[Id, Int](
                  NonEmptyList.one((tv, Kind.Type)),
                  annotated
                ) { (b, k) =>
                  Type.Var.Skolem(b.name, k, existential = false, id = 0L)
                }
              assertEquals(sks.length, 1)
              assertEquals(expr1.freeVarsDup.length, expectedVars)
            } catch {
              case t: Throwable =>
                failure = Some(t)
            }
        },
        "expr-app2-small-stack",
        stackBytes
      )

      thread.start()
      thread.join()

      failure match {
        case Some(so: StackOverflowError) =>
          val trace = so.getStackTrace.iterator.take(40).mkString("\n")
          fail(
            s"Expr recursive app utilities overflowed on right-deep app2 chains (depth=$depth, stackBytes=$stackBytes)\n$trace"
          )
        case Some(other) =>
          val trace = other.getStackTrace.iterator.take(40).mkString("\n")
          fail(s"unexpected failure: $other\n$trace")
        case None =>
          ()
      }
    }
  )

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
