package dev.bosatsu.smt

import dev.bosatsu.Platform
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.rng.Seed

import IntDecide.Result
import LinearBoundsGens.evalBool

/** IntDecide replaced z3 in the recursion check. In the fragment it decides,
  * it must agree with z3 in both directions:
  *   - proved: IntDecide proves a goal exactly when z3 finds the facts and
  *     the negated goal unsat
  *   - refuted: when IntDecide gives a counterexample, z3 finds them sat, and
  *     the counterexample really makes the facts true and the goal false
  *
  * Cases where z3 says unknown or fails are checked on a grid instead (see
  * IntDecideLawsTest for the exact grid check on bounded goals).
  */
class IntDecideZ3Test extends munit.ScalaCheckSuite {
  import IntDecideGens.*

  // z3 is slower under Scala.js, and the JVM run checks the same code
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isJvm) 200 else 30)

  private def agreesWithZ3(
      facts: List[SmtExpr.BoolExpr],
      goal: SmtExpr.BoolExpr,
      ints: Iterable[String],
      bools: Iterable[String]
  ): Prop = {
    val decided = IntDecide.implies(goal, facts, IntDecide.DefaultWorkLimit)
    val z3 = Z3Oracle.implies(goal, facts, ints, bools)
    val clue = s"facts: $facts\ngoal: $goal\nIntDecide: $decided\nz3: $z3"
    def real(m: IntDecide.Model): Prop = {
      val env = envOf(m, LinearBoundsGens.byZeroChoices.head)
      (facts.forall(evalBool(_, env)) && !evalBool(goal, env)) :| s"not a counterexample\n$clue"
    }
    (decided, z3) match {
      case (Result.Proved, Right(Some(true)))      => Prop.passed
      case (Result.Refuted(m), Right(Some(false))) => real(m)
      case (Result.Proved, Right(None) | Left(_))  =>
        val bad = counterexample(grid, facts, goal)
        bad.isEmpty :| s"z3 can't decide, and the grid has a counterexample $bad\n$clue"
      case (Result.Refuted(m), Right(None) | Left(_)) => real(m)
      case _                                       => Prop.falsified :| s"disagree\n$clue"
    }
  }

  property("IntDecide agrees with z3 on goals the recursion check builds") {
    forAll(caseGen(nonlinear = false)) { case (facts, goal) =>
      agreesWithZ3(facts, goal, intNames, boolNames)
    }
  }

  property("IntDecide agrees with z3 on the LinearBounds goals") {
    // linear, with div and mod by negative constants read as SMT-LIB's
    forAll(LinearBoundsGens.z3CaseGen) { case (facts, goal) =>
      agreesWithZ3(facts, goal, LinearBoundsGens.varNames, Nil)
    }
  }

  property("IntDecide agrees with z3 on goals with every Int variable bounded") {
    forAll(caseGen(nonlinear = false)) { case (facts, goal) =>
      agreesWithZ3(boxFacts ::: facts, goal, intNames, boolNames)
    }
  }

  test("both directions happen: the generators give provable and refutable goals") {
    val params = Gen.Parameters.default
    val cases = (0 until 200).flatMap(i => caseGen(nonlinear = false)(params, Seed(i.toLong)))
    val results = cases.map { case (facts, goal) => IntDecide.implies(goal, facts, IntDecide.DefaultWorkLimit) }
    assert(results.count(_ == Result.Proved) >= 20, results.toString)
    assert(results.count { case Result.Refuted(_) => true; case _ => false } >= 20, results.toString)
  }
}
