package dev.bosatsu.smt

import dev.bosatsu.Platform
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}
import org.scalacheck.rng.Seed

import IntDecide.Result
import LinearBoundsGens.evalBool

/** IntDecide replaced z3 in the recursion check, so the two must never
  * contradict each other: IntDecide must never prove a goal z3 refutes (z3
  * finds the facts and the negated goal sat), and never refute one z3 proves
  * (unsat). Either may fail to answer, and the embedded z3 sometimes does (it
  * traps on some valid queries), but in the fragment IntDecide decides, with a
  * limit large enough (`IntDecideGens.decide`), it must always answer, so its
  * giving up fails these properties too.
  *
  * Three kinds of goals, from IntDecideGens:
  *   - refutable: facts built to hold at a point where the goal fails, so the
  *     answer must be a counterexample
  *   - provable: goals built to follow from the facts, so the answer must be a
  *     proof
  *   - any: small random goals and facts, either way
  *
  * When z3 can't answer, a counterexample is still checked on the facts and
  * goal, and a proof on a grid of values.
  */
class IntDecideZ3Test extends munit.ScalaCheckSuite {
  import IntDecideGens.*

  // z3 is slower under Scala.js, and the JVM run checks the same code
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isJvm) 200 else 30)

  /** What a case is built to be. */
  private enum Expect derives CanEqual {
    case Proof, Counterexample, Either
  }

  private def checkAgainstZ3(
      facts: List[SmtExpr.BoolExpr],
      goal: SmtExpr.BoolExpr,
      ints: Iterable[String],
      bools: Iterable[String],
      expect: Expect
  ): Prop = {
    val decided = decide(goal, facts)
    val z3 = Z3Oracle.implies(goal, facts, ints, bools)
    val clue =
      s"facts: $facts\ngoal: $goal\nIntDecide: $decided\nz3: $z3\nexpected: $expect"
    // z3's answer, when it has one: Some(true) is unsat (proved)
    val z3Proved: Option[Boolean] = z3.toOption.flatten

    def real(m: IntDecide.Model): Prop = {
      val env = envOf(m, LinearBoundsGens.byZeroChoices.head)
      (facts.forall(evalBool(_, env)) && !evalBool(
        goal,
        env
      )) :| s"not a counterexample\n$clue"
    }

    decided match {
      case Result.Proved =>
        if (z3Proved.contains(false))
          Prop.falsified :| s"proved, but z3 found a counterexample\n$clue"
        else if (expect == Expect.Counterexample)
          Prop.falsified :| s"proved a goal built to fail\n$clue"
        else if (z3Proved.isEmpty) {
          val bad = counterexample(grid, facts, goal)
          bad.isEmpty :| s"z3 can't answer, and the grid has a counterexample $bad\n$clue"
        } else Prop.passed
      case Result.Refuted(m) =>
        if (z3Proved.contains(true))
          Prop.falsified :| s"refuted, but z3 proved it\n$clue"
        else if (expect == Expect.Proof)
          Prop.falsified :| s"refuted a goal built to follow\n$clue"
        else real(m)
      case Result.Unknown(reason) =>
        Prop.falsified :| s"gave up ($reason) on a goal in the fragment\n$clue"
    }
  }

  property(
    "goals built to fail at a point: IntDecide refutes them, and z3 never proves them"
  ) {
    forAll(refutableCaseGen) { case (facts, goal, _) =>
      checkAgainstZ3(facts, goal, intNames, boolNames, Expect.Counterexample)
    }
  }

  property(
    "goals built to follow from the facts: IntDecide proves them, and z3 never refutes them"
  ) {
    forAllNoShrink(provableCaseGen) { case (facts, goal) =>
      checkAgainstZ3(facts, goal, intNames, boolNames, Expect.Proof)
    }
  }

  property("small random goals: IntDecide and z3 never contradict each other") {
    forAll(caseGen(nonlinear = false)) { case (facts, goal) =>
      checkAgainstZ3(facts, goal, intNames, boolNames, Expect.Either)
    }
  }

  property(
    "small random goals with every Int variable bounded: no contradiction"
  ) {
    forAll(caseGen(nonlinear = false)) { case (facts, goal) =>
      checkAgainstZ3(
        boxFacts ::: facts,
        goal,
        intNames,
        boolNames,
        Expect.Either
      )
    }
  }

  property("the LinearBounds goals: no contradiction") {
    // linear, with div and mod by negative constants read as SMT-LIB's
    forAll(LinearBoundsGens.z3CaseGen) { case (facts, goal) =>
      checkAgainstZ3(facts, goal, LinearBoundsGens.varNames, Nil, Expect.Either)
    }
  }

  test("the random goals go both ways") {
    val cases = (0 until 200).flatMap(i =>
      caseGen(nonlinear = false)(Gen.Parameters.default, Seed(i.toLong))
    )
    val results = cases.map { case (facts, goal) =>
      IntDecide.implies(goal, facts, IntDecide.DefaultWorkLimit)
    }
    assert(results.count(_ == Result.Proved) >= 20, results.toString)
    assert(
      results.count { case Result.Refuted(_) => true; case _ => false } >= 20,
      results.toString
    )
  }
}
