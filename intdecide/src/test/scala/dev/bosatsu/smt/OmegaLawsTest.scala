package dev.bosatsu.smt

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.{forAll, propBoolean}
import scala.collection.immutable.IntMap

import Omega.{Constraint, Form, Result}

/** The laws in the doc comments of Omega. `solve` is checked against brute
  * force on problems whose variables are all bounded, where brute force over
  * the box decides the problem, so Sat and Unsat are both checked exactly.
  */
class OmegaLawsTest extends munit.ScalaCheckSuite {
  import IntDecideGens.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (TestPlatform.isJvm) 1000 else 150)

  private val bigBudget: Long = 10000000L

  private def solve(cs: List[Constraint]): Result =
    Omega.solve(cs, Omega.Budget(bigBudget))

  private val envGen: Gen[Map[Int, BigInt]] =
    Gen
      .listOfN(omegaVars.size, LinearBoundsGens.bigIntGen)
      .map(vs => omegaVars.zip(vs).toMap)

  // ---------------------------------------------------------------------------
  // Form

  property("Form.eval is a homomorphism") {
    forAll(omegaFormGen, omegaFormGen, LinearBoundsGens.bigIntGen, envGen) {
      (a, b, k, env) =>
        Prop.all(
          ((a + b).eval(env) == a.eval(env) + b.eval(env)) :| "+",
          ((a - b).eval(env) == a.eval(env) - b.eval(env)) :| "-",
          ((a * k).eval(env) == a.eval(env) * k) :| "*",
          ((-a).eval(env) == -a.eval(env)) :| "negate",
          (a.plus(k).eval(env) == a.eval(env) + k) :| "plus"
        )
    }
  }

  property("Form.substitute evaluates the form at the substituted value") {
    forAll(omegaFormGen, omegaFormGen, Gen.oneOf(omegaVars), envGen) {
      (a, by, v, env) =>
        val sub = a.substitute(v, by)
        Prop.all(
          (sub.eval(env) == a.eval(env.updated(v, by.eval(env)))) :| "eval",
          (by.coeff(v) != 0 || sub.coeff(v) == 0) :| "v is gone"
        )
    }
  }

  property("Forms are canonical: equal exactly when equal as functions") {
    forAll(omegaFormGen, omegaFormGen) { (a, b) =>
      Prop.all(
        ((a - a) == Form.zero) :| "a - a",
        ((a + b) == (b + a)) :| "commutative",
        (Form.of(a.const, a.coeffs) == a) :| "of",
        (!a.coeffs.valuesIterator.contains(BigInt(0))) :| "no zero coefficient"
      )
    }
  }

  property(
    "Form.tightened has the same integer solutions, with coefficients of gcd 1"
  ) {
    forAll(omegaFormGen, Gen.choose(1, 6), Gen.chooseNum(-5, 5), envGen) {
      (f0, k, d, env) =>
        // a common factor and an offset it doesn't divide
        val f = (f0 * BigInt(k)).plus(BigInt(d))
        val t = f.tightened
        Prop.all(
          ((t.eval(env) >= 0) == (f.eval(env) >= 0)) :| s"$f and $t at $env",
          (t.isConstant || (t.coeffs.valuesIterator
            .foldLeft(BigInt(0))(_.gcd(_)) == 1)) :| s"$t"
        )
    }
  }

  property("floorDiv and ceilDiv bracket the quotient") {
    forAll(LinearBoundsGens.bigIntGen, LinearBoundsGens.positiveBigIntGen) {
      (a, k) =>
        val q = Omega.floorDiv(a, k)
        val r = Omega.ceilDiv(a, k)
        Prop.all(
          ((k * q <= a) && (a < k * (q + 1))) :| s"floor a=$a k=$k q=$q",
          ((k * (r - 1) < a) && (a <= k * r)) :| s"ceil a=$a k=$k r=$r"
        )
    }
  }

  // ---------------------------------------------------------------------------
  // solve

  property("solve is exact on bounded problems") {
    forAll(constraintsGen) { cs0 =>
      val cs = boxConstraints ::: cs0
      (solve(cs), omegaSolution(cs)) match {
        case (Result.Sat(m), _) =>
          Prop.all(
            cs.forall(_.holds(m)) :| s"model $m does not satisfy $cs0",
            cs.flatMap(_.vars)
              .forall(m.contains) :| s"model $m is missing variables"
          )
        case (Result.Unsat, None)        => Prop.passed
        case (Result.Unsat, Some(point)) =>
          Prop.falsified :| s"unsat, but $point solves $cs0"
        case (Result.GaveUp, _) => Prop.falsified :| s"gave up on $cs0"
      }
    }
  }

  property(
    "solve is exact on bounded problems that need the dark shadow and splinters"
  ) {
    forAll(inexactConstraintsGen) { cs0 =>
      val cs = boxConstraints ::: cs0
      (solve(cs), omegaSolution(cs)) match {
        case (Result.Sat(m), _) =>
          cs.forall(_.holds(m)) :| s"model $m does not satisfy $cs0"
        case (Result.Unsat, None)        => Prop.passed
        case (Result.Unsat, Some(point)) =>
          Prop.falsified :| s"unsat, but $point solves $cs0"
        case (Result.GaveUp, _) => Prop.falsified :| s"gave up on $cs0"
      }
    }
  }

  property(
    "solve on unbounded problems: models satisfy, and unsat has no solution near 0"
  ) {
    forAll(constraintsGen) { cs =>
      solve(cs) match {
        case Result.Sat(m) =>
          (cs.forall(_.holds(m)) && cs
            .flatMap(_.vars)
            .forall(m.contains)) :| s"model $m for $cs"
        case Result.Unsat =>
          omegaSolution(
            cs
          ).isEmpty :| s"unsat, but ${omegaSolution(cs)} solves $cs"
        case Result.GaveUp =>
          Prop.falsified :| s"gave up on $cs"
      }
    }
  }

  property(
    "a model of a solvable problem with the constraints shuffled is a model too"
  ) {
    forAll(constraintsGen) { cs =>
      val forward = solve(cs)
      val backward = solve(cs.reverse)
      (forward, backward) match {
        case (Result.Sat(_), Result.Sat(_)) | (Result.Unsat, Result.Unsat) =>
          Prop.passed
        case _ => Prop.falsified :| s"$forward but reversed $backward for $cs"
      }
    }
  }

  test(
    "quotients and remainders of the same variables don't blow up the projection"
  ) {
    // From IntDecide on a goal provableCaseGen built. Variables 0, 1 and 2
    // are its Int variables, and each of its seven div and mod terms adds a
    // quotient q and remainder r with n == k * q + r and 0 <= r <= k - 1.
    // The pairs of bounds on a quotient have coefficients k, so only the Int
    // variables counted as exact, and eliminating them first multiplied the
    // constraints (400 lower times 183 upper bounds after six steps): no
    // budget sufficed. Counting the pairs on a quotient used nowhere else as
    // exact (pairExact) solves it with under 1,000.
    def f(const: Int, coeffs: (Int, Int)*): Form =
      Form.of(BigInt(const), coeffs.map { case (v, k) => (v, BigInt(k)) })
    def divMod(n: Form, k: Int, q: Int, r: Int): List[Constraint] =
      List(
        Constraint.Eq(n - f(0, q -> k, r -> 1)),
        Constraint.Geq(f(0, r -> 1)),
        Constraint.Geq(f(k - 1, r -> -1))
      )
    val cs =
      divMod(f(0, 0 -> -1), 5, 3, 4) ::: divMod(f(0, 0 -> 1), 4, 5, 6) :::
        divMod(f(0, 2 -> -1), 4, 7, 8) ::: divMod(f(0, 1 -> 1), 4, 10, 11) :::
        divMod(f(0, 0 -> 1), 7, 12, 13) ::: divMod(f(0, 1 -> -1), 3, 14, 15) :::
        divMod(f(0, 2 -> 1), 5, 16, 17) ::: List(
          Constraint.Eq(f(0, 1 -> -1, 9 -> 1)),
          Constraint.Geq(f(0, 3 -> 1, 5 -> -1)),
          Constraint.Geq(f(3, 1 -> 1, 2 -> -1)),
          Constraint.Geq(f(0, 0 -> 2, 2 -> -1)),
          Constraint.Eq(f(0, 0 -> 1, 2 -> -1, 11 -> 1)),
          Constraint.Geq(f(-1, 0 -> 1, 2 -> -1, 8 -> 1))
        )
    // Unsat: v3 >= v5 is floor(-v0 / 5) >= floor(v0 / 4), so v0 <= 0; then
    // v2 == v0 + v11 <= 2 * v0 with v11 >= 0 makes v0, v11 and v2 all 0,
    // and v8 >= 1 fails, since v8 is (-v2) mod 4.
    assertEquals(Omega.solve(cs, Omega.Budget(10000L)), Result.Unsat)
  }

  test("a budget that runs out gives GaveUp") {
    val cs = boxConstraints ::: List(
      Constraint.Geq(
        Form.of(BigInt(-27), List(0 -> BigInt(11), 1 -> BigInt(13)))
      )
    )
    assertEquals(Omega.solve(cs, Omega.Budget(3)), Result.GaveUp)
  }

  test(
    "running out during the splinters stops at once, however many there are"
  ) {
    // 0 <= a * x - b * y <= 1 with a = 10^30 + 1, b = 10^30: the dark shadow
    // of x is empty, and the lower bound has about 10^30 splinters. The
    // budget runs out at the first one (or earlier), and the rest must not be
    // tried one by one.
    val a = BigInt(10).pow(30) + 1
    val b = BigInt(10).pow(30)
    val f = Form.of(BigInt(0), List(0 -> a, 1 -> -b))
    val slab = List(Constraint.Geq(f), Constraint.Geq((-f).plus(BigInt(1))))
    assertEquals(Omega.solve(slab, Omega.Budget(10)), Result.GaveUp)
    // with room, the first splinter, a * x == b * y, has x == y == 0
    assertEquals(
      solve(slab),
      Result.Sat(IntMap(0 -> BigInt(0), 1 -> BigInt(0)))
    )
  }

  // ---------------------------------------------------------------------------
  // Regressions for each kind of step

  private def geq(c: Int, terms: (Int, Int)*): Constraint =
    Constraint.Geq(
      Form.of(BigInt(c), terms.map { case (v, a) => (v, BigInt(a)) })
    )
  private def eq(c: Int, terms: (Int, Int)*): Constraint =
    Constraint.Eq(
      Form.of(BigInt(c), terms.map { case (v, a) => (v, BigInt(a)) })
    )

  private def assertSat(cs: List[Constraint])(using munit.Location): Unit =
    solve(cs) match {
      case Result.Sat(m) => assert(cs.forall(_.holds(m)), s"model $m")
      case other         => fail(s"expected a solution, got $other")
    }

  test(
    "Pugh's example: 27 <= 11x + 13y <= 45 and -10 <= 7x - 9y <= 4 has real but no integer solutions"
  ) {
    val cs = List(
      geq(-27, 0 -> 11, 1 -> 13),
      geq(45, 0 -> -11, 1 -> -13),
      geq(10, 0 -> 7, 1 -> -9),
      geq(4, 0 -> -7, 1 -> 9)
    )
    assertEquals(solve(cs), Result.Unsat)
  }

  test("a solution only a splinter finds: 0 <= 3x - 2y <= 1") {
    // Eliminating x pairs 3x >= 2y with 3x <= 2y + 1: the real shadow is
    // 3 >= 0, but the dark shadow needs 3 >= 2 * 2. The splinter
    // 3x - 2y == 0 has x == y == 0.
    assertSat(List(geq(0, 0 -> 3, 1 -> -2), geq(1, 0 -> -3, 1 -> 2)))
    // and with y pinned so that only 3x - 2y == 1 works: x == 1, y == 1
    assertSat(
      List(geq(0, 0 -> 3, 1 -> -2), geq(1, 0 -> -3, 1 -> 2), eq(-1, 1 -> 1))
    )
  }

  test("equalities without a unit coefficient") {
    assertSat(List(eq(-1, 0 -> 3, 1 -> 5)))
    assertSat(List(eq(-7, 0 -> 4, 1 -> 6, 2 -> 9)))
    assertEquals(solve(List(eq(-3, 0 -> 6, 1 -> 10))), Result.Unsat)
    // 3x + 5y == 1 forces x == 2 mod 5: x in [3, 6] gives x == 2 + 5k, none
    assertEquals(
      solve(List(eq(-1, 0 -> 3, 1 -> 5), geq(-3, 0 -> 1), geq(6, 0 -> -1))),
      Result.Unsat
    )
  }

  test("opposite inequalities that meet are an equality") {
    assertSat(List(geq(-4, 0 -> 2, 1 -> 2), geq(4, 0 -> -2, 1 -> -2)))
    // 2x + 2y == 3 after tightening: 2x + 2y >= 3 becomes x + y >= 2, and
    // 2x + 2y <= 3 becomes x + y <= 1
    assertEquals(
      solve(List(geq(-3, 0 -> 2, 1 -> 2), geq(3, 0 -> -2, 1 -> -2))),
      Result.Unsat
    )
  }

  test("constant constraints") {
    assertEquals(
      solve(List(Constraint.Geq(Form.constant(BigInt(-1))))),
      Result.Unsat
    )
    assertEquals(
      solve(List(Constraint.Eq(Form.constant(BigInt(1))))),
      Result.Unsat
    )
    assertEquals(solve(Nil), Result.Sat(IntMap.empty))
  }
}
