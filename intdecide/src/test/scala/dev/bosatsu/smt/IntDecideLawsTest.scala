package dev.bosatsu.smt

import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}

import IntDecide.{Reason, Result}
import LinearBoundsGens.{byZeroChoices, evalBool}

/** The laws in the doc comment of `IntDecide.implies`, checked without z3:
  *   - soundness on a grid of values,
  *   - every counterexample really is one,
  *   - with every Int variable bounded, the grid covers every value, so it
  *     decides the goal, and `implies` must agree with it exactly.
  *
  * IntDecideZ3Test in core checks the same procedure against z3 on unbounded
  * goals.
  */
class IntDecideLawsTest extends munit.ScalaCheckSuite {
  import SmtExpr.*
  import IntDecideGens.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (TestPlatform.isJvm) 400 else 60)

  private def implies(goal: BoolExpr, facts: List[BoolExpr]): Result =
    IntDecide.implies(goal, facts, IntDecide.DefaultWorkLimit)

  private def describe(facts: List[BoolExpr], goal: BoolExpr): String =
    s"facts: $facts\ngoal: $goal"

  // Every variable of the facts and goal has a value, and the facts hold
  // there and the goal doesn't, under every interpretation of division by
  // zero.
  private def realCounterexample(
      m: IntDecide.Model,
      facts: List[BoolExpr],
      goal: BoolExpr
  ): Prop = {
    val (ints, bools) =
      (goal :: facts).map(names).foldLeft((Set.empty[String], Set.empty[String])) {
        case ((a, b), (c, d)) => (a ++ c, b ++ d)
      }
    val refutes = byZeroChoices.forall { bz =>
      val env = envOf(m, bz)
      facts.forall(evalBool(_, env)) && !evalBool(goal, env)
    }
    Prop.all(
      ints.forall(m.ints.contains) :| s"model $m is missing an Int variable of $ints",
      bools.forall(m.bools.contains) :| s"model $m is missing a Bool variable of $bools",
      refutes :| s"model $m does not refute\n${describe(facts, goal)}"
    )
  }

  property("in the fragment, with every Int variable bounded, implies agrees with the grid") {
    forAll(caseGen(nonlinear = false)) { case (facts0, goal) =>
      val facts = boxFacts ::: facts0
      (implies(goal, facts), counterexample(grid, facts, goal)) match {
        case (Result.Proved, None)         => Prop.passed
        case (Result.Proved, Some(env))    =>
          Prop.falsified :| s"proved, but $env is a counterexample\n${describe(facts, goal)}"
        case (Result.Refuted(m), Some(_))  => realCounterexample(m, facts, goal)
        case (Result.Refuted(m), None)     =>
          Prop.falsified :| s"refuted by $m, but the grid has no counterexample\n${describe(facts, goal)}"
        case (Result.Unknown(r), _)        =>
          Prop.falsified :| s"unknown ($r) in the fragment\n${describe(facts, goal)}"
      }
    }
  }

  property("in the fragment, implies proves or refutes, and is right") {
    forAll(caseGen(nonlinear = false)) { case (facts, goal) =>
      implies(goal, facts) match {
        case Result.Proved     =>
          val bad = counterexample(grid, facts, goal)
          bad.isEmpty :| s"proved, but $bad is a counterexample\n${describe(facts, goal)}"
        case Result.Refuted(m) => realCounterexample(m, facts, goal)
        case Result.Unknown(r) =>
          Prop.falsified :| s"unknown ($r) in the fragment\n${describe(facts, goal)}"
      }
    }
  }

  property("outside the fragment, proofs are sound and counterexamples real") {
    // fewer cases: each grid check runs under every interpretation of
    // division by zero
    forAll(caseGen(nonlinear = true)) { case (facts, goal) =>
      implies(goal, facts) match {
        case Result.Proved     =>
          val bad = counterexample(gridAllByZero, facts, goal)
          bad.isEmpty :| s"proved, but $bad is a counterexample\n${describe(facts, goal)}"
        case Result.Refuted(m) => realCounterexample(m, facts, goal)
        case Result.Unknown(r) =>
          (r != Reason.UnverifiedCounterexample) :| s"unverified counterexample\n${describe(facts, goal)}"
      }
    }
  }

  property("adding facts never stops a proof") {
    forAll(caseGen(nonlinear = false), factGen(nonlinear = false)) { case ((facts, goal), extra) =>
      (implies(goal, facts) == Result.Proved) ==>
        (implies(goal, extra :: facts) == Result.Proved) :| describe(extra :: facts, goal)
    }
  }

  // ---------------------------------------------------------------------------
  // What the issue found LinearBounds and pathImplies can't prove

  private val n = iv("n")
  private val k = iv("k")
  private val step = iv("step")
  private val cap = iv("cap")
  private val done = bv("done")
  private val fast = bv("fast")

  private def assertProved(goal: BoolExpr, facts: BoolExpr*)(using munit.Location): Unit =
    assertEquals(implies(goal, facts.toList), Result.Proved)

  // proved, where the fast path alone can't
  private def assertProvedNew(goal: BoolExpr, facts: BoolExpr*)(using munit.Location): Unit = {
    assert(!pathImplies(goal, facts), "pathImplies proves it, so this checks nothing new")
    assertProved(goal, facts*)
  }

  private def assertRefuted(goal: BoolExpr, facts: BoolExpr*)(using munit.Location): IntDecide.Model =
    implies(goal, facts.toList) match {
      case Result.Refuted(m) =>
        assert(facts.forall(f => IntDecide.evalBool(f, m).contains(true)), s"facts fail at $m")
        assertEquals(IntDecide.evalBool(goal, m), Some(false))
        m
      case other => fail(s"expected a counterexample, got $other")
    }

  test("facts relating two variables") {
    // n > k gives n - k - 1 >= 0
    assertProvedNew(Gte(Sub(Vector(Sub(Vector(n, k)), c(1))), c(0)), Gt(n, k))
    // step <= cap and cap <= n give n - step >= 0
    assertProvedNew(Gte(Sub(Vector(n, step)), c(0)), Lte(step, cap), Lte(cap, n))
  }

  test("a fallthrough fact with an or") {
    // `case _ if (n <= 0) and done` missed, and then done
    assertProvedNew(
      Gte(Sub(Vector(n, c(1))), c(0)),
      Not(And(Vector(Lte(n, c(0)), done))),
      done
    )
  }

  test("an ite whose condition is not a constant") {
    // `if fast: n.sub(2) else: n.sub(1)` is < n
    val next = Ite[SmtSort.IntSort](fast, Sub(Vector(n, c(2))), Sub(Vector(n, c(1))))
    assertProvedNew(Lt(next, n), Gt(n, c(0)))
  }

  test("a case split on a Bool variable") {
    assertProvedNew(
      Gte(n, c(1)),
      Implies(fast, Gt(n, c(5))),
      Implies(Not(fast), Gt(n, c(0)))
    )
  }

  test("div and mod by negative constants, with Bosatsu's semantics") {
    // n div -2 is floor(n / -2), so for n >= 1 it is <= -1
    assertProved(Lt(bosatsuDiv(n, c(-2)), c(0)), Gte(n, c(1)))
    // n mod_Int -3 is in [-2, 0]
    assertProved(
      And(Vector(Lte(bosatsuMod(n, c(-3)), c(0)), Gte(bosatsuMod(n, c(-3)), c(-2)))),
      BoolConst.True
    )
    // n == 3 * (n div 3) + n mod_Int 3
    assertProved(
      EqInt(n, Add(Vector(Mul(Vector(c(3), bosatsuDiv(n, c(3)))), bosatsuMod(n, c(3))))),
      BoolConst.True
    )
  }

  test("an integer-only fact: 2n == 1 has no solution") {
    assertProved(BoolConst.False, EqInt(Mul(Vector(c(2), n)), c(1)))
  }

  test("counterexamples") {
    val m = assertRefuted(Lt(Sub(Vector(n, c(1))), k), Gt(n, c(0)))
    assert(m.ints.contains("n") && m.ints.contains("k"), m.toString)
    // the earlier branch missed because done is false
    val m1 = assertRefuted(Gte(Sub(Vector(n, c(1))), c(0)), Not(And(Vector(Lte(n, c(0)), done))))
    assertEquals(m1.bools.get("done"), Some(false))
    assert(m1.ints.get("n").exists(_ <= 0), m1.toString)
  }

  test("products of variables are unknowns: sound proofs, unknown otherwise") {
    val nk = Mul(Vector(n, k))
    // the same term is the same unknown
    assertEquals(implies(Lt(Sub(Vector(nk, c(1))), nk), Nil), Result.Proved)
    // n * k > 0 doesn't follow from n > 0, and n = 1, k = 0 shows it
    val m = assertRefuted(Gt(nk, c(0)), Gt(n, c(0)))
    assertEquals(m.ints.get("k"), Some(BigInt(0)))
    // n * n >= 0 is true, but not linear
    assertEquals(
      implies(Gte(Mul(Vector(n, n)), c(0)), Nil),
      Result.Unknown(Reason.OutsideFragment)
    )
  }

  test("division by a variable or zero is an unknown") {
    assertEquals(implies(Lte(Div(n, c(0)), Div(n, c(0))), Nil), Result.Proved)
    implies(Gte(Div(n, k), c(0)), List(Gt(n, c(0)), Gt(k, c(0)))) match {
      case Result.Unknown(Reason.OutsideFragment) => ()
      case other                                  => fail(s"expected unknown, got $other")
    }
  }

  test("terms with no arguments and uninterpreted functions are unknowns") {
    val f = App[SmtSort.IntSort]("f", Vector(n))
    assertEquals(implies(EqInt(f, f), Nil), Result.Proved)
    assertEquals(implies(Gt(f, c(0)), Nil), Result.Unknown(Reason.OutsideFragment))
    assertEquals(implies(Gt(Add(Vector()), c(0)), Nil), Result.Unknown(Reason.OutsideFragment))
    val p = App[SmtSort.BoolSort]("p", Vector(n))
    assertEquals(implies(Or(Vector(p, Not(p))), Nil), Result.Proved)
    assertEquals(implies(p, Nil), Result.Unknown(Reason.OutsideFragment))
  }

  test("a work limit that runs out gives Unknown") {
    val goal = Lt(Ite[SmtSort.IntSort](fast, Sub(Vector(n, c(2))), Sub(Vector(n, c(1)))), n)
    assertEquals(
      IntDecide.implies(goal, List(Gt(n, c(0))), 5),
      Result.Unknown(Reason.WorkLimit)
    )
  }

  test("Reason messages") {
    Reason.values.foreach(r => assert(r.message.nonEmpty))
  }
}
