package dev.bosatsu.smt

import org.scalacheck.Prop
import org.scalacheck.Prop.{forAll, propBoolean}

/** LinearBounds replaces z3 for goals it can prove, so every "proved" must
  * agree with z3. The main properties only count cases the prover proves
  * (cases it declines are discarded), so each successful case is a real
  * agreement check, and a generator that stops producing provable cases fails
  * the test instead of passing vacuously.
  */
class LinearBoundsTest extends munit.ScalaCheckSuite {
  import SmtExpr.*
  import SmtCommand.*
  import LinearBoundsGens.*

  // z3 is slower under Scala.js, and the JVM run checks the same code
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (dev.bosatsu.Platform.isJvm) 300 else 50)
      .withMaxDiscardRatio(20)

  private val liveRunner: Z3Api.RunSmt2 = Z3Oracle.runner

  private def z3Script(goal: BoolExpr, facts: List[BoolExpr]): SmtScript =
    Z3Oracle.script(goal, facts, varNames, Nil)

  /** z3's answer for "facts imply goal", as Z3Oracle.implies gives it. */
  private def z3Answer(goal: BoolExpr, facts: List[BoolExpr]): Either[String, Option[Boolean]] =
    Z3Oracle.implies(goal, facts, varNames, Nil)

  // for cases known not to trip the embedded z3
  private def z3Implies(goal: BoolExpr, facts: List[BoolExpr]): Option[Boolean] =
    z3Answer(goal, facts) match {
      case Right(ans) => ans
      case Left(err)  =>
        fail(s"z3 failed: $err\n${SmtLibRender.renderScript(z3Script(goal, facts))}")
    }

  private def agreesWithZ3(goal: BoolExpr, facts: List[BoolExpr]): Prop =
    z3Answer(goal, facts) match {
      case Right(Some(true))  => Prop.passed
      case Right(Some(false)) =>
        val smt2 = SmtLibRender.renderScript(z3Script(goal, facts))
        val raw = liveRunner(smt2.replace("(check-sat)", "(check-sat)\n(get-model)"))
        Prop.falsified :| s"proved, but z3 found a counterexample\nfacts: $facts\ngoal: $goal\n$smt2\nz3: $raw"
      case Right(None) => Prop.undecided
      case Left(_)     =>
        // The embedded z3 traps on some valid queries, for example
        // z > -1, z <= 2, not ((z div 4) <= z). Check those on the grid
        // instead, so a trap never counts as agreement.
        val bad = gridCounterexample(facts, goal)
        bad.isEmpty :| s"z3 trapped, and the grid has a counterexample ${bad.map(_.vars)}\nfacts: $facts\ngoal: $goal"
    }

  property("every goal LinearBounds proves, z3 proves too") {
    forAll(z3CaseGen) { case (facts, goal) =>
      LinearBounds.implies(goal, facts) ==> agreesWithZ3(goal, facts)
    }
  }

  property("every goal pathImplies proves, z3 proves too") {
    forAll(z3CaseGen) { case (facts, goal) =>
      val facts1 = facts.map(normalizeBoolForSolver)
      pathImplies(normalizeBoolForSolver(goal), facts1) ==> agreesWithZ3(goal, facts)
    }
  }

  test("euclid matches SMT-LIB div and mod") {
    for {
      n <- -9 to 9
      d <- List(-4, -3, -2, -1, 1, 2, 3, 4)
    } {
      val (q, r) = euclid(BigInt(n), BigInt(d))
      assertEquals(BigInt(d) * q + r, BigInt(n))
      assert(r >= 0 && r < BigInt(d).abs, s"n=$n d=$d q=$q r=$r")
    }
  }

  property("every goal LinearBounds proves holds at every point of a grid") {
    // covers what the z3 properties can't: products, and division by zero or
    // by a variable
    forAll(anyCaseGen) { case (facts, goal) =>
      LinearBounds.implies(goal, facts) ==> {
        val bad = gridCounterexample(facts, goal)
        bad.isEmpty :| s"counterexample ${bad.map(_.vars)}\nfacts: $facts\ngoal: $goal"
      }
    }
  }

  property("adding facts never stops LinearBounds proving a goal") {
    forAll(anyCaseGen, factGen(z3Safe = false)) { (factsGoal, extra) =>
      val (facts, goal) = factsGoal
      LinearBounds.implies(goal, facts) ==> LinearBounds.implies(goal, extra :: facts)
    }
  }

  // just enough of SMT-LIB to read the queries below
  private def parse(s: String): BoolExpr = {
    def int(e: SExpr): IntExpr =
      e match {
        case SExpr.Atom(a) if a.headOption.exists(ch => ch.isDigit) => IntConst(BigInt(a))
        case SExpr.Atom(a)                                           => v(a)
        case SExpr.List(Vector(SExpr.Atom("-"), a))                  => Sub(Vector(int(a)))
        case SExpr.List(Vector(SExpr.Atom("-"), a, b))               => Sub(Vector(int(a), int(b)))
        case SExpr.List(Vector(SExpr.Atom("+"), a, b))               => Add(Vector(int(a), int(b)))
        case SExpr.List(Vector(SExpr.Atom("div"), a, b))             => Div(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom("mod"), a, b))             => Mod(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom("ite"), p, a, b))          => Ite[SmtSort.IntSort](bool(p), int(a), int(b))
        case other                                                   => sys.error(s"bad int $other")
      }
    def bool(e: SExpr): BoolExpr =
      e match {
        case SExpr.Atom(name)                            => Var[SmtSort.BoolSort](name)
        case SExpr.List(Vector(SExpr.Atom("<"), a, b))   => Lt(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom("<="), a, b))  => Lte(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom(">"), a, b))   => Gt(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom(">="), a, b))  => Gte(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom("="), a, b))   => EqInt(int(a), int(b))
        case SExpr.List(Vector(SExpr.Atom("not"), a))    => Not(bool(a))
        case SExpr.List(SExpr.Atom("and") +: args)       => And(args.map(bool))
        case SExpr.List(SExpr.Atom("or") +: args)        => Or(args.map(bool))
        case other                                       => sys.error(s"bad bool $other")
      }
    SExprParser.parseAll(s) match {
      case Right(Vector(e)) => bool(e)
      case other            => sys.error(s"could not parse $s: $other")
    }
  }

  // the queries `bosatsu check` sent to z3 for test_workspace (fact, goal)
  private val workspaceQueries: List[(String, String)] = List(
    "(and (not (< idx_0 0)) (not (= idx_0 0)) (and (not (< idx_0 0)) (not (= idx_0 0))) (< idx_0 s_3))" ->
      "(>= (- idx_0 1) 0)",
    "(> curr_0 0)" -> "(>= (ite (= 10 0) 0 (div curr_0 10)) 0)",
    "(and (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) even_1)" ->
      "(>= (ite (= 2 0) 0 (div (- n_0 1) 2)) 0)",
    "(and (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) even_1)" ->
      "(< (ite (= 2 0) 0 (div (- n_0 1) 2)) n_0)",
    "(> curr_0 0)" -> "(< (ite (= 10 0) 0 (div curr_0 10)) curr_0)",
    "(and (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) (not even_1) (not even_1))" ->
      "(>= (ite (= 2 0) 0 (div n_0 2)) 0)",
    "(and (not (or (< n_0 0) (= n_0 0))) (not (or (< n_0 0) (= n_0 0))) (not even_1) (not even_1))" ->
      "(< (ite (= 2 0) 0 (div n_0 2)) n_0)",
    "(and (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0))))" ->
      "(>= (ite (= 2 0) 0 (div i_0 2)) 0)",
    "(and (not (or (< i_0 0) (= i_0 0))) (not (or (< i_0 0) (= i_0 0))))" ->
      "(< (ite (= 2 0) 0 (div i_0 2)) i_0)"
  )

  test("proves every query test_workspace sent to z3") {
    workspaceQueries.foreach { case (fact, goal) =>
      assert(pathImplies(parse(goal), List(parse(fact))), s"$fact => $goal")
    }
  }

  private def z3ProvesWithDecls(goal: BoolExpr, facts: List[BoolExpr], ints: List[String], bools: List[String]): Boolean = {
    val script = SmtScript(
      Vector(SetLogic.QF_LIA) ++
        ints.map(DeclareConst(_, SmtSort.IntS)) ++
        bools.map(DeclareConst(_, SmtSort.BoolS)) ++
        Vector(Assert(And(facts.toVector)), Assert(Not(goal)), CheckSat)
    )
    Z3Api.run(script, parseModel = false, liveRunner) match {
      case Right(res) => res.status == Z3Api.Status.Unsat
      case Left(err)  => fail(err.message)
    }
  }

  test("z3 agrees with every workspace query") {
    workspaceQueries.foreach { case (fact, goal) =>
      val ints = List("idx_0", "s_1", "s_2", "s_3", "curr_0", "n_0", "i_0", "acc_1")
      assert(
        z3ProvesWithDecls(parse(goal), List(parse(fact)), ints, List("even_1")),
        s"$fact => $goal"
      )
    }
  }

  test("proves decreasing division and decrement families, and z3 agrees") {
    val n = v("x")
    for {
      k <- 2 to 7
      d <- 0 to 3
    } {
      val cases = List(
        // x > d: (x - d) div k is a smaller natural
        List(Gt(n, c(d))) -> Lt(Div(Sub(Vector(n, c(d))), c(k)), n),
        List(Gt(n, c(d))) -> Gte(Div(Sub(Vector(n, c(d))), c(k)), c(0)),
        // x >= 0 and x != 0
        List(Not(Lt(n, c(0))), Not(EqInt(n, c(0)))) -> Gte(Sub(Vector(n, c(1))), c(0)),
        List(Gt(n, c(0))) -> Lt(Div(n, c(k)), n),
        List(Gte(n, c(0))) -> Lt(Mod(n, c(k)), c(k))
      )
      cases.foreach { case (facts, goal) =>
        assert(LinearBounds.implies(goal, facts), s"$facts => $goal")
        assertEquals(z3Implies(goal, facts), Some(true), s"$facts => $goal")
      }
    }
  }

  test("does not prove false goals near the boundary") {
    val n = v("x")
    val cases = List(
      // x >= 0 allows x = 0, where x - 1 < 0
      List(Gte(n, c(0))) -> Gte(Sub(Vector(n, c(1))), c(0)),
      // x div 1 == x, not smaller
      List(Gt(n, c(0))) -> Lt(Div(n, c(1)), n),
      // x = 0 allows 0 div 2 == 0 == x
      List(Gte(n, c(0))) -> Lt(Div(n, c(2)), n),
      // negative divisors flip the sign
      List(Gt(n, c(0))) -> Gte(Div(n, c(-2)), c(0))
    )
    cases.foreach { case (facts, goal) =>
      assert(!LinearBounds.implies(goal, facts), s"$facts => $goal")
      assertEquals(z3Implies(goal, facts), Some(false), s"$facts => $goal")
    }
  }

  test("does not read div or mod by a negative constant") {
    // These hold in SMT-LIB, but not for Bosatsu's div and mod_Int: the
    // recursion check once lowered those straight to SMT-LIB div and mod, and
    // 5.mod_Int(-3) == -1, so the last goal fails at x = 5, and recursing on
    // it would loop forever.
    val n = v("x")
    def oldLowering(k: Int) = Ite(EqInt(c(k), c(0)), n, Mod(n, c(k)))
    val cases = List(
      List(Gte(n, c(0))) -> Gte(Mod(n, c(-3)), c(0)),
      List(Gte(n, c(0))) -> Lt(Mod(n, c(-3)), c(3)),
      List(Gt(n, c(0))) -> Lte(Div(n, c(-2)), c(0)),
      List(Gt(n, c(2))) -> Lt(Sub(Vector(n, oldLowering(-3), c(1))), n)
    )
    cases.foreach { case (facts, goal) =>
      assert(!LinearBounds.implies(goal, facts), s"$facts => $goal")
    }
    // the same goal with a positive divisor is proved
    assert(LinearBounds.implies(Lt(Sub(Vector(n, oldLowering(3), c(1))), n), List(Gt(n, c(2)))))
  }

  test("reads Bosatsu's div and mod_Int by a negative constant as the lowering builds them") {
    val n = v("x")
    val facts = List(Gt(n, c(2)))
    // x.mod_Int(-3) is in [-2, 0]: x - m - 1 can be x + 1, while x + m - 1 < x
    val m = bosatsuMod(n, c(-3))
    assert(!LinearBounds.implies(Lt(Sub(Vector(n, m, c(1))), n), facts))
    assertEquals(z3Implies(Lt(Sub(Vector(n, m, c(1))), n), facts), Some(false))
    val good = List(
      Lt(Sub(Vector(Add(Vector(n, m)), c(1))), n),
      Gte(Sub(Vector(Add(Vector(n, m)), c(1))), c(0)),
      Lte(m, c(0)),
      Gt(m, c(-3))
    )
    good.foreach { goal =>
      assert(LinearBounds.implies(goal, facts), s"$goal")
      assertEquals(z3Implies(goal, facts), Some(true), s"$goal")
    }
    // x.div(-2) is -ceil(x / 2): at most -1 for x > 0
    val q = bosatsuDiv(n, c(-2))
    assert(LinearBounds.implies(Lte(q, c(-1)), List(Gt(n, c(0)))))
    assertEquals(z3Implies(Lte(q, c(-1)), List(Gt(n, c(0)))), Some(true))
    assert(!LinearBounds.implies(Lte(q, c(-2)), List(Gt(n, c(0)))))
    assertEquals(z3Implies(Lte(q, c(-2)), List(Gt(n, c(0)))), Some(false))
  }

  test("nested div by 1 takes linear time") {
    // bounds once read the numerator twice for k == 1, so 40 levels took 2^40 steps
    val n = v("x")
    val nested = (1 to 40).foldLeft(n: IntExpr)((e, _) => Div(e, c(1)))
    assert(LinearBounds.implies(Lte(nested, n), List(Gte(n, c(0)))))
  }

  test("does not prove goals that depend on division by zero") {
    // SMT-LIB leaves x div 0 and x mod 0 unspecified. The embedded z3 traps
    // on these, so they are checked here and in the grid property instead.
    val n = v("x")
    val cases = List(
      List(Gt(n, c(0))) -> Gte(Div(n, c(0)), c(0)),
      List(Gt(n, c(0))) -> Gte(Mod(n, c(0)), c(0)),
      List(Gt(n, c(0))) -> Lt(Div(n, c(0)), n)
    )
    cases.foreach { case (facts, goal) =>
      assert(!LinearBounds.implies(goal, facts), s"$facts => $goal")
    }
  }

  test("a one-argument Sub means its argument, as rendered for z3") {
    // Sub(Vector(y)) renders as `y`, so this goal is y div 5 - x div 4 > y + 2,
    // which fails at y = 7, x = 0. Reading it as -y would wrongly prove it.
    val (x, y, z) = (v("x"), v("y"), v("z"))
    val facts = List(
      And(Vector(Lte(c(6), Add(Vector(Mul(Vector(c(1), y)), c(-1)))), Lte(z, c(5)))),
      Lte(Add(Vector(Mul(Vector(c(1), x)), c(-1))), c(1))
    )
    val goal = Gt(Sub(Vector(Div(y, c(5)), Div(x, c(4)))), Add(Vector(Sub(Vector(y)), c(2))))
    assert(!LinearBounds.implies(goal, facts))
    assertEquals(z3Implies(goal, facts), Some(false), SmtLibRender.renderScript(z3Script(goal, facts)))
  }

  test("a case z3 once reported as disproved is proved by both") {
    // facts: y - 1 >= 6, z <= 5, x - 1 <= 1
    // goal: y div 5 - x div 4 > 2 - y
    val (x, y, z) = (v("x"), v("y"), v("z"))
    val facts = List(
      And(Vector(Lte(c(6), Add(Vector(Mul(Vector(c(1), y)), c(-1)))), Lte(z, c(5)))),
      Lte(Add(Vector(Mul(Vector(c(1), x)), c(-1))), c(1))
    )
    val goal = Gt(Sub(Vector(Div(y, c(5)), Div(x, c(4)))), Sub(Vector(c(2), y)))
    assert(LinearBounds.implies(goal, facts))
    assertEquals(z3Implies(goal, facts), Some(true), SmtLibRender.renderScript(z3Script(goal, facts)))
  }

  test("contradictory facts prove anything, as in z3") {
    val n = v("x")
    val facts = List(Gt(n, c(3)), Lt(n, c(2)))
    assert(LinearBounds.implies(BoolConst.False, facts))
    assertEquals(z3Implies(BoolConst.False, facts), Some(true))
    val excluded = List(Gte(n, c(0)), Lte(n, c(0)), Not(EqInt(n, c(0))))
    assert(LinearBounds.implies(BoolConst.False, excluded))
    assertEquals(z3Implies(BoolConst.False, excluded), Some(true))
  }
}
