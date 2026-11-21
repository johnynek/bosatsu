package dev.bosatsu.smt

import dev.bosatsu.scalawasiz3.{Z3Platform, Z3Result}
import org.scalacheck.{Gen, Prop}
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

  // z3 is slower under Scala.js, and the JVM run checks the same code
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (dev.bosatsu.Platform.isJvm) 300 else 50)
      .withMaxDiscardRatio(20)

  private val z3Solver = Z3Platform.create()
  private val liveRunner: Z3Api.RunSmt2 = { smt2 =>
    z3Solver.runSmt2(smt2) match {
      case Z3Result.Success(stdout, stderr, _) =>
        Right(Z3Api.SolverOutput(stdout, stderr))
      case Z3Result.Failure(msg, _, stdout, stderr, _) =>
        Left(Z3Api.RunError.ExecutionFailure(msg, stdout, stderr))
    }
  }

  private val varNames: Vector[String] = Vector("x", "y", "z")

  private def v(name: String): IntExpr = Var[SmtSort.IntSort](name)
  private def c(n: Int): IntExpr = IntConst(BigInt(n))

  private val varGen: Gen[IntExpr] = Gen.oneOf(varNames).map(v)
  private val constGen: Gen[IntExpr] = Gen.chooseNum(-6, 6).map(c)
  // The embedded z3 traps on division or mod by zero, by a variable (even
  // when guarded), and on some nonlinear (QF_NIA) inputs, so the z3
  // properties use linear arithmetic with nonzero constant divisors. The grid
  // property also covers products, and zero and variable divisors.
  private def divisorGen(z3Safe: Boolean): Gen[Int] =
    if (z3Safe) Gen.frequency(6 -> Gen.chooseNum(1, 5), 2 -> Gen.chooseNum(-4, -1))
    else Gen.frequency(6 -> Gen.chooseNum(1, 5), 1 -> Gen.const(0), 2 -> Gen.chooseNum(-4, -1))

  private def cmpGen(l: Gen[IntExpr], r: Gen[IntExpr]): Gen[BoolExpr] =
    for {
      a <- l
      b <- r
      mk <- Gen.oneOf[(IntExpr, IntExpr) => BoolExpr](
        Lt(_, _),
        Lte(_, _),
        Gt(_, _),
        Gte(_, _),
        EqInt(_, _)
      )
    } yield mk(a, b)

  // Every Int construct LinearBounds handles, plus ones it must ignore.
  private def intExprGen(depth: Int, z3Safe: Boolean): Gen[IntExpr] = {
    val leaf = Gen.frequency(3 -> varGen, 2 -> constGen)
    if (depth <= 0) leaf
    else {
      val rec = intExprGen(depth - 1, z3Safe)
      val nonlinear = if (z3Safe) 0 else 1
      Gen.frequency(
        6 -> leaf,
        2 -> Gen.zip(rec, rec).map { case (a, b) => Add(Vector(a, b)) },
        1 -> Gen.choose(2, 4).flatMap(Gen.listOfN(_, rec)).map(as => Add(as.toVector)),
        2 -> Gen.zip(rec, rec).map { case (a, b) => Sub(Vector(a, b)) },
        // rendered as the argument itself, not a negation
        1 -> rec.map(a => Sub(Vector(a))),
        1 -> Gen.choose(2, 4).flatMap(Gen.listOfN(_, rec)).map(as => Sub(as.toVector)),
        2 -> Gen.zip(Gen.chooseNum(-3, 3), rec).map { case (k, a) => Mul(Vector(c(k), a)) },
        nonlinear -> Gen.zip(rec, rec).map { case (a, b) => Mul(Vector(a, b)) },
        3 -> Gen.zip(rec, divisorGen(z3Safe)).map { case (a, k) => Div(a, c(k)) },
        nonlinear -> Gen.zip(rec, rec).map { case (a, b) => Div(a, b) },
        2 -> Gen.zip(rec, divisorGen(z3Safe)).map { case (a, k) => Mod(a, c(k)) },
        nonlinear -> Gen.zip(rec, rec).map { case (a, b) => Mod(a, b) },
        1 -> Gen.zip(constIteCond, rec, rec).map { case (p, a, b) => Ite[SmtSort.IntSort](p, a, b) },
        1 -> Gen.zip(cmpGen(varGen, constGen), rec, rec).map { case (p, a, b) =>
          Ite[SmtSort.IntSort](p, a, b)
        }
      )
    }
  }

  // conditions with no variables, like the `(= 2 0)` guards on div
  private val constIteCond: Gen[BoolExpr] =
    Gen.oneOf(cmpGen(constGen, constGen), Gen.oneOf(true, false).map(BoolConst(_)))

  // a * v + d compared with a constant, in every orientation
  private val singleVarFactGen: Gen[BoolExpr] = {
    val lin = for {
      x <- varGen
      a <- Gen.oneOf(1, 1, 1, -1, 2, -2, 3)
      d <- Gen.chooseNum(-3, 3)
      form <- Gen.oneOf(0, 1, 2)
    } yield form match {
      case 0 => x
      case 1 => Add(Vector(Mul(Vector(c(a), x)), c(d)))
      case _ => Sub(Vector(Mul(Vector(c(a), x)), c(d)))
    }
    val cmp = Gen.oneOf(cmpGen(lin, constGen), cmpGen(constGen, lin))
    Gen.frequency(
      6 -> cmp,
      3 -> cmp.map(Not(_)),
      3 -> Gen.zip(varGen, constGen).map { case (x, k) => Not(EqInt(x, k)) },
      2 -> Gen.zip(varGen, constGen).map { case (x, k) =>
        Not(Or(Vector(Lt(x, k), EqInt(x, k))))
      },
      1 -> Gen.zip(varGen, constGen).map { case (x, k) =>
        Or(Vector(Lt(x, k), EqInt(x, k)))
      },
      1 -> cmp.map(f => Not(Not(f)))
    )
  }

  private def noiseFactGen(z3Safe: Boolean): Gen[BoolExpr] =
    Gen.frequency(
      3 -> cmpGen(varGen, varGen),
      2 -> cmpGen(intExprGen(2, z3Safe), intExprGen(1, z3Safe)),
      1 -> Gen.const(BoolConst.True),
      1 -> Gen.const(BoolConst.False)
    )

  private def factGen(z3Safe: Boolean): Gen[BoolExpr] =
    Gen.frequency(
      8 -> singleVarFactGen,
      2 -> noiseFactGen(z3Safe),
      1 -> Gen.zip(singleVarFactGen, singleVarFactGen).map { case (a, b) => And(Vector(a, b)) }
    )

  // most facts bound a variable, so a good share of goals are provable
  private def factsGen(z3Safe: Boolean): Gen[List[BoolExpr]] =
    Gen.choose(1, 6).flatMap(Gen.listOfN(_, factGen(z3Safe)))

  private val templateGoalGen: Gen[BoolExpr] =
    for {
      x <- varGen
      k <- Gen.chooseNum(1, 6)
      d <- Gen.chooseNum(0, 3)
      shape <- Gen.choose(0, 5)
    } yield shape match {
      case 0 => Lt(Div(Sub(Vector(x, c(d))), c(k)), x)
      case 1 => Gte(Div(Sub(Vector(x, c(d))), c(k)), c(0))
      case 2 => Lt(Mod(x, c(k)), c(k))
      case 3 => Lte(Div(x, c(k)), x)
      case 4 => Gte(Sub(Vector(x, c(d))), c(0))
      case _ => Lt(Sub(Vector(x, c(d + 1))), x)
    }

  private def goalGen(z3Safe: Boolean): Gen[BoolExpr] = {
    val cmp = cmpGen(intExprGen(2, z3Safe), intExprGen(2, z3Safe))
    val atom = Gen.frequency(5 -> cmp, 4 -> templateGoalGen)
    Gen.frequency(
      8 -> atom,
      2 -> atom.map(Not(_)),
      1 -> Gen.zip(atom, atom).map { case (a, b) => And(Vector(a, b)) },
      1 -> Gen.zip(atom, atom).map { case (a, b) => Or(Vector(a, b)) },
      1 -> Gen.zip(atom, atom).map { case (a, b) => Not(And(Vector(a, b))) },
      1 -> Gen.zip(atom, atom).map { case (a, b) => Not(Or(Vector(a, b))) },
      1 -> Gen.oneOf(true, false).map(BoolConst(_))
    )
  }

  private def caseGen(z3Safe: Boolean): Gen[(List[BoolExpr], BoolExpr)] =
    Gen.zip(factsGen(z3Safe), goalGen(z3Safe))
  private val z3CaseGen = caseGen(z3Safe = true)
  private val anyCaseGen = caseGen(z3Safe = false)

  // QF_LIA does not allow products of terms or division by a term
  private def isNonlinear(e: SmtExpr[?]): Boolean = {
    def isConst(i: IntExpr) = LinearBounds.linear(i).exists(_.coeffs.isEmpty)
    e match {
      case Mul(args) =>
        args.count(a => !isConst(a)) > 1 || args.exists(isNonlinear)
      case Div(n, d) => !isConst(d) || isNonlinear(n) || isNonlinear(d)
      case Mod(n, d) => !isConst(d) || isNonlinear(n) || isNonlinear(d)
      case Add(args)          => args.exists(isNonlinear)
      case Sub(args)          => args.exists(isNonlinear)
      case Ite(p, a, b)       => isNonlinear(p) || isNonlinear(a) || isNonlinear(b)
      case Lt(a, b)           => isNonlinear(a) || isNonlinear(b)
      case Lte(a, b)          => isNonlinear(a) || isNonlinear(b)
      case Gt(a, b)           => isNonlinear(a) || isNonlinear(b)
      case Gte(a, b)          => isNonlinear(a) || isNonlinear(b)
      case EqInt(a, b)        => isNonlinear(a) || isNonlinear(b)
      case Not(a)             => isNonlinear(a)
      case And(args)          => args.exists(isNonlinear)
      case Or(args)           => args.exists(isNonlinear)
      case _                  => false
    }
  }

  private def z3Script(goal: BoolExpr, facts: List[BoolExpr]): SmtScript = {
    val pathCond = normalizeBoolForSolver(And(facts.toVector))
    val goal1 = normalizeBoolForSolver(goal)
    val logic =
      if (isNonlinear(pathCond) || isNonlinear(goal1)) SetLogic("QF_NIA")
      else SetLogic.QF_LIA
    SmtScript(
      Vector(logic) ++
        varNames.map(DeclareConst(_, SmtSort.IntS)) ++
        Vector(Assert(pathCond), Assert(Not(goal1)), CheckSat)
    )
  }

  /** z3's answer for "facts imply goal": Right(Some(true)) if facts and not
    * goal is unsat, Right(Some(false)) if sat, Right(None) if z3 says unknown,
    * and Left if z3 failed.
    */
  private def z3Answer(goal: BoolExpr, facts: List[BoolExpr]): Either[String, Option[Boolean]] =
    Z3Api.run(z3Script(goal, facts), parseModel = false, liveRunner) match {
      case Right(res) =>
        Right(res.status match {
          case Z3Api.Status.Unsat   => Some(true)
          case Z3Api.Status.Sat     => Some(false)
          case Z3Api.Status.Unknown => None
        })
      case Left(err) => Left(err.message)
    }

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

  // SMT-LIB semantics: for d != 0, n = d * q + r with 0 <= r < |d|.
  private def euclid(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val q0 = n / d.abs
    val q1 = if ((n % d.abs != 0) && (n < 0)) q0 - 1 else q0
    val q = if (d > 0) q1 else -q1
    (q, n - d * q)
  }

  // SMT-LIB leaves division and mod by zero unspecified (any function of the
  // numerator), so a proved goal must hold for every one of these choices.
  private val byZeroChoices: List[BigInt => BigInt] =
    List(_ => BigInt(0), _ => BigInt(1), _ => BigInt(-1), _ => BigInt(7), _ => BigInt(-7), n => n, n => -n)

  private final case class Env(vars: Map[String, BigInt], byZero: BigInt => BigInt)

  private def evalInt(e: IntExpr, env: Env): BigInt =
    e match {
      case IntConst(n) => n
      case Var(name)   => env.vars(name)
      case Add(args)   => args.iterator.map(evalInt(_, env)).sum
      case Sub(args)   =>
        args.toList match {
          case Nil          => BigInt(0)
          // SmtToSExpr renders a one-argument `-` as the argument itself
          case one :: Nil   => evalInt(one, env)
          case head :: rest => rest.foldLeft(evalInt(head, env))(_ - evalInt(_, env))
        }
      case Mul(args) => args.iterator.map(evalInt(_, env)).product
      case Div(n, d) =>
        val (nv, dv) = (evalInt(n, env), evalInt(d, env))
        if (dv == 0) env.byZero(nv) else euclid(nv, dv)._1
      case Mod(n, d) =>
        val (nv, dv) = (evalInt(n, env), evalInt(d, env))
        if (dv == 0) env.byZero(nv) else euclid(nv, dv)._2
      case Ite(p, a, b) => if (evalBool(p, env)) evalInt(a, env) else evalInt(b, env)
      case other        => sys.error(s"unexpected int expression: $other")
    }

  private def evalBool(e: BoolExpr, env: Env): Boolean =
    e match {
      case BoolConst(b)  => b
      case Lt(a, b)      => evalInt(a, env) < evalInt(b, env)
      case Lte(a, b)     => evalInt(a, env) <= evalInt(b, env)
      case Gt(a, b)      => evalInt(a, env) > evalInt(b, env)
      case Gte(a, b)     => evalInt(a, env) >= evalInt(b, env)
      case EqInt(a, b)   => evalInt(a, env) == evalInt(b, env)
      case Not(a)        => !evalBool(a, env)
      case And(args)     => args.forall(evalBool(_, env))
      case Or(args)      => args.exists(evalBool(_, env))
      case other         => sys.error(s"unexpected bool expression: $other")
    }

  private val grid: List[Env] =
    (for {
      x <- -6 to 6
      y <- -6 to 6
      z <- -6 to 6
      byZero <- byZeroChoices
    } yield Env(Map("x" -> BigInt(x), "y" -> BigInt(y), "z" -> BigInt(z)), byZero)).toList

  private def gridCounterexample(facts: List[BoolExpr], goal: BoolExpr): Option[Env] =
    grid.find(env => facts.forall(evalBool(_, env)) && !evalBool(goal, env))

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
