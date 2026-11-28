package dev.bosatsu.smt

import org.scalacheck.Gen

import LinearBoundsGens.{Env, byZeroChoices, evalBool, evalInt}

/** Generators for the IntDecide and Omega tests: goals and facts over three
  * Int and two Bool variables, with everything the recursion check builds.
  */
object IntDecideGens {
  import SmtExpr.*

  // ---------------------------------------------------------------------------
  // Omega problems

  val omegaVars: Vector[Int] = Vector(0, 1, 2)

  // small coefficients are common, larger ones make inexact eliminations
  val coeffGen: Gen[BigInt] =
    Gen.frequency(
      6 -> Gen.oneOf(-2, -1, 1, 2),
      3 -> Gen.oneOf(-7, -6, -5, -4, -3, 3, 4, 5, 6, 7),
      1 -> Gen.chooseNum(-30, 30)
    ).map(BigInt(_))

  val omegaFormGen: Gen[Omega.Form] =
    for {
      c <- Gen.chooseNum(-25, 25).map(BigInt(_))
      n <- Gen.choose(1, 3)
      vs <- Gen.pick(n, omegaVars)
      cs <- Gen.listOfN(n, coeffGen)
    } yield Omega.Form.of(c, vs.toList.zip(cs))

  val constraintGen: Gen[Omega.Constraint] =
    Gen.frequency(
      4 -> omegaFormGen.map(Omega.Constraint.Geq(_)),
      1 -> omegaFormGen.map(Omega.Constraint.Eq(_))
    )

  val constraintsGen: Gen[List[Omega.Constraint]] =
    Gen.choose(1, 6).flatMap(Gen.listOfN(_, constraintGen))

  // Thin slabs `0 <= f <= w` with no coefficient 1 or -1: eliminations are
  // rarely exact, and the dark shadow is often empty while integer
  // solutions remain, so the splinters are needed.
  val inexactConstraintsGen: Gen[List[Omega.Constraint]] = {
    val big = Gen.oneOf(-9, -8, -7, -6, -5, -4, -3, -2, 2, 3, 4, 5, 6, 7, 8, 9).map(BigInt(_))
    val slab = for {
      c <- Gen.chooseNum(-20, 20).map(BigInt(_))
      a <- big
      b <- big
      vs <- Gen.pick(2, omegaVars)
      w <- Gen.chooseNum(0, 3).map(BigInt(_))
    } yield {
      val f = Omega.Form.of(c, vs.toList.zip(List(a, b)))
      List(Omega.Constraint.Geq(f), Omega.Constraint.Geq((-f).plus(w)))
    }
    Gen.choose(1, 2).flatMap(Gen.listOfN(_, slab)).map(_.flatten)
  }

  val omegaBound: Int = 4

  /** `-omegaBound <= v <= omegaBound` for every variable, so brute force
    * over the box decides the problem.
    */
  val boxConstraints: List[Omega.Constraint] =
    omegaVars.toList.flatMap { v =>
      val x = Omega.Form.variable(v)
      List(
        Omega.Constraint.Geq(x.plus(BigInt(omegaBound))),
        Omega.Constraint.Geq((-x).plus(BigInt(omegaBound)))
      )
    }

  val omegaBox: List[Map[Int, BigInt]] =
    (for {
      a <- -omegaBound to omegaBound
      b <- -omegaBound to omegaBound
      c <- -omegaBound to omegaBound
    } yield Map(0 -> BigInt(a), 1 -> BigInt(b), 2 -> BigInt(c))).toList

  def omegaSolution(cs: List[Omega.Constraint]): Option[Map[Int, BigInt]] =
    omegaBox.find(env => cs.forall(_.holds(env)))

  // ---------------------------------------------------------------------------
  // Goals and facts

  val intNames: Vector[String] = Vector("x", "y", "z")
  val boolNames: Vector[String] = Vector("p", "q")

  def iv(name: String): IntExpr = Var[SmtSort.IntSort](name)
  def bv(name: String): BoolExpr = Var[SmtSort.BoolSort](name)
  def c(n: Int): IntExpr = IntConst(BigInt(n))

  val intVarGen: Gen[IntExpr] = Gen.oneOf(intNames).map(iv)
  val boolVarGen: Gen[BoolExpr] = Gen.oneOf(boolNames).map(bv)
  val smallConstGen: Gen[IntExpr] = Gen.chooseNum(-6, 6).map(c)
  val nonzeroGen: Gen[Int] = Gen.oneOf(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 7)

  /** A comparison of two Int terms. */
  def cmpGen(ints: Gen[IntExpr]): Gen[BoolExpr] =
    LinearBoundsGens.cmpGen(ints, ints)

  /** Linear Int terms, as the recursion check builds them: Bosatsu's div and
    * mod by any nonzero constant, SMT-LIB's by a positive one, and `ite` on
    * conditions with variables.
    *
    * With `nonlinear`, also products of variables and division by variables
    * and zero, which IntDecide reads as unknowns.
    */
  def intTermGen(depth: Int, nonlinear: Boolean): Gen[IntExpr] = {
    val leaf = Gen.frequency(4 -> intVarGen, 2 -> smallConstGen)
    if (depth <= 0) leaf
    else {
      val rec = intTermGen(depth - 1, nonlinear)
      val nl = if (nonlinear) 1 else 0
      Gen.frequency(
        6 -> leaf,
        3 -> Gen.zip(rec, rec).map { case (a, b) => Add(Vector(a, b)) },
        3 -> Gen.zip(rec, rec).map { case (a, b) => Sub(Vector(a, b)) },
        1 -> Gen.choose(1, 3).flatMap(Gen.listOfN(_, rec)).map(as => Sub(as.toVector)),
        2 -> Gen.zip(Gen.chooseNum(-3, 3), rec).map { case (k, a) => Mul(Vector(c(k), a)) },
        2 -> Gen.zip(rec, nonzeroGen).map { case (a, k) => bosatsuDiv(a, c(k)) },
        2 -> Gen.zip(rec, nonzeroGen).map { case (a, k) => bosatsuMod(a, c(k)) },
        1 -> Gen.zip(rec, Gen.choose(1, 5)).map { case (a, k) => Div(a, c(k)) },
        1 -> Gen.zip(rec, Gen.choose(1, 5)).map { case (a, k) => Mod(a, c(k)) },
        2 -> Gen.zip(condGen(1, intVarGen), rec, rec).map { case (p, a, b) =>
          Ite[SmtSort.IntSort](p, a, b)
        },
        nl -> Gen.zip(rec, rec).map { case (a, b) => Mul(Vector(a, b)) },
        nl -> Gen.zip(rec, rec).map { case (a, b) => bosatsuDiv(a, b) },
        nl -> Gen.zip(rec, rec).map { case (a, b) => Div(a, b) },
        nl -> Gen.zip(rec, rec).map { case (a, b) => Mod(a, b) }
      )
    }
  }

  /** Booleans over comparisons of `ints` and the Bool variables, with every
    * connective.
    */
  def condGen(depth: Int, ints: Gen[IntExpr]): Gen[BoolExpr] = {
    val leaf = Gen.frequency(
      5 -> cmpGen(ints),
      2 -> boolVarGen,
      1 -> Gen.oneOf(true, false).map(BoolConst(_))
    )
    if (depth <= 0) leaf
    else {
      val rec = condGen(depth - 1, ints)
      Gen.frequency(
        4 -> leaf,
        2 -> rec.map(Not(_)),
        2 -> Gen.choose(2, 3).flatMap(Gen.listOfN(_, rec)).map(as => And(as.toVector)),
        2 -> Gen.choose(2, 3).flatMap(Gen.listOfN(_, rec)).map(as => Or(as.toVector)),
        1 -> Gen.zip(rec, rec).map { case (a, b) => Implies(a, b) },
        1 -> Gen.zip(rec, rec).map { case (a, b) => Xor(a, b) },
        1 -> Gen.zip(rec, rec).map { case (a, b) => EqBool(a, b) },
        1 -> Gen.zip(rec, rec, rec).map { case (p, a, b) => Ite[SmtSort.BoolSort](p, a, b) }
      )
    }
  }

  // facts that bound one variable, the way the recursion check's path
  // conditions do, so a good share of goals are provable
  val boundFactGen: Gen[BoolExpr] =
    for {
      x <- intVarGen
      k <- smallConstGen
      mk <- Gen.oneOf[(IntExpr, IntExpr) => BoolExpr](
        Lt(_, _),
        Lte(_, _),
        Gt(_, _),
        Gte(_, _),
        (a, b) => Not(EqInt(a, b))
      )
    } yield mk(x, k)

  def factGen(nonlinear: Boolean): Gen[BoolExpr] =
    Gen.frequency(
      4 -> boundFactGen,
      // relating two or more variables
      3 -> cmpGen(intTermGen(1, nonlinear)),
      // a match fallthrough: `not (n <= 0 and done)`
      2 -> Gen.zip(boundFactGen, boolVarGen).map { case (f, b) => Not(And(Vector(f, b))) },
      2 -> condGen(2, intTermGen(1, nonlinear)),
      1 -> boolVarGen.map(Not(_))
    )

  def factsGen(nonlinear: Boolean): Gen[List[BoolExpr]] =
    Gen.choose(0, 5).flatMap(Gen.listOfN(_, factGen(nonlinear)))

  // the obligations of a recursive call: next >= 0 and next < current
  def goalGen(nonlinear: Boolean): Gen[BoolExpr] = {
    val term = intTermGen(2, nonlinear)
    Gen.frequency(
      3 -> Gen.zip(term, intVarGen).map { case (n, x) => Lt(n, x) },
      2 -> term.map(n => Gte(n, c(0))),
      3 -> cmpGen(term),
      2 -> condGen(2, intTermGen(1, nonlinear))
    )
  }

  def caseGen(nonlinear: Boolean): Gen[(List[BoolExpr], BoolExpr)] =
    Gen.zip(factsGen(nonlinear), goalGen(nonlinear))

  // ---------------------------------------------------------------------------
  // Grids

  val gridBound: Int = 4

  /** `-gridBound <= v <= gridBound` for every Int variable. */
  val boxFacts: List[BoolExpr] =
    intNames.toList.flatMap(n => List(Gte(iv(n), c(-gridBound)), Lte(iv(n), c(gridBound))))

  private def envsWith(byZero: List[BigInt => BigInt]): List[Env] =
    (for {
      x <- -gridBound to gridBound
      y <- -gridBound to gridBound
      z <- -gridBound to gridBound
      p <- List(true, false)
      q <- List(true, false)
      bz <- byZero
    } yield Env(
      Map("x" -> BigInt(x), "y" -> BigInt(y), "z" -> BigInt(z)),
      Map("p" -> p, "q" -> q),
      bz
    )).toList

  /** Every point of the box, for terms without division by zero. */
  val grid: List[Env] = envsWith(byZeroChoices.take(1))

  /** Every point of the box under each interpretation of division by zero. */
  val gridAllByZero: List[Env] = envsWith(byZeroChoices)

  def counterexample(
      envs: List[Env],
      facts: List[BoolExpr],
      goal: BoolExpr
  ): Option[Env] =
    envs.find(env => facts.forall(evalBool(_, env)) && !evalBool(goal, env))

  /** A limit large enough for the goals built here.
    *
    * `IntDecide.DefaultWorkLimit` is sized for the recursion check's
    * obligations, which are far smaller than many goals built here, and a few
    * of those need more than it. The third law of `IntDecide.implies` is
    * about a limit large enough; for the goals built here, this is one by
    * far: of 900,000 sampled, two needed more than the default, and neither
    * ten times it.
    */
  val largeWorkLimit: Long = 100L * IntDecide.DefaultWorkLimit

  /** `IntDecide.implies` with the default limit, then with `largeWorkLimit`
    * if the default is reached: the result the laws are about.
    */
  def decide(goal: BoolExpr, facts: List[BoolExpr]): IntDecide.Result =
    IntDecide.implies(goal, facts, IntDecide.DefaultWorkLimit) match {
      case IntDecide.Result.Unknown(IntDecide.Reason.WorkLimit) =>
        IntDecide.implies(goal, facts, largeWorkLimit)
      case other                                                => other
    }

  /** The model as an evaluation environment, under one interpretation of
    * division by zero.
    */
  def envOf(m: IntDecide.Model, byZero: BigInt => BigInt): Env =
    Env(
      m.ints.toMap.withDefaultValue(BigInt(0)),
      m.bools.toMap.withDefaultValue(false),
      byZero
    )

  /** The Int and Bool variable names of an expression. */
  def names(e: BoolExpr): (Set[String], Set[String]) = {
    def ints(i: IntExpr): (Set[String], Set[String]) =
      i match {
        case Var(n)        => (Set(n), Set.empty)
        case IntConst(_)   => (Set.empty, Set.empty)
        case Add(as)       => merge(as.map(ints))
        case Sub(as)       => merge(as.map(ints))
        case Mul(as)       => merge(as.map(ints))
        case Div(a, b)     => merge(Vector(ints(a), ints(b)))
        case Mod(a, b)     => merge(Vector(ints(a), ints(b)))
        case Ite(p, a, b)  => merge(Vector(bools(p), ints(a), ints(b)))
        case other         => sys.error(s"unexpected int expression: $other")
      }
    def bools(b: BoolExpr): (Set[String], Set[String]) =
      b match {
        case Var(n)         => (Set.empty, Set(n))
        case BoolConst(_)   => (Set.empty, Set.empty)
        case Lt(l, r)       => merge(Vector(ints(l), ints(r)))
        case Lte(l, r)      => merge(Vector(ints(l), ints(r)))
        case Gt(l, r)       => merge(Vector(ints(l), ints(r)))
        case Gte(l, r)      => merge(Vector(ints(l), ints(r)))
        case EqInt(l, r)    => merge(Vector(ints(l), ints(r)))
        case EqBool(l, r)   => merge(Vector(bools(l), bools(r)))
        case Xor(l, r)      => merge(Vector(bools(l), bools(r)))
        case Implies(l, r)  => merge(Vector(bools(l), bools(r)))
        case Not(a)         => bools(a)
        case And(as)        => merge(as.map(bools))
        case Or(as)         => merge(as.map(bools))
        case Ite(p, a, b)   => merge(Vector(bools(p), bools(a), bools(b)))
        case other          => sys.error(s"unexpected bool expression: $other")
      }
    def merge(parts: Vector[(Set[String], Set[String])]): (Set[String], Set[String]) =
      parts.foldLeft((Set.empty[String], Set.empty[String])) { case ((a, b), (c, d)) =>
        (a ++ c, b ++ d)
      }
    bools(e)
  }

  // ---------------------------------------------------------------------------
  // Goals known to be refuted (a witness point) or proved (they follow)

  /** A point: values for every Int and Bool variable. */
  val pointGen: Gen[Env] =
    for {
      xs <- Gen.listOfN(
        intNames.size,
        Gen.frequency(6 -> Gen.chooseNum(-12, 12), 1 -> Gen.chooseNum(-1000, 1000))
      )
      bs <- Gen.listOfN(boolNames.size, Gen.oneOf(true, false))
    } yield Env(
      intNames.zip(xs.map(BigInt(_))).toMap,
      boolNames.zip(bs).toMap,
      byZeroChoices.head
    )

  /** e if it holds at env, else its negation. */
  def holdingAt(env: Env, e: BoolExpr): BoolExpr =
    if (evalBool(e, env)) e else Not(e)

  /** e if it fails at env, else its negation. */
  def failingAt(env: Env, e: BoolExpr): BoolExpr =
    if (evalBool(e, env)) Not(e) else e

  /** `sum(k_i * v_i)` over one to three Int variables, coefficients not 0. */
  val linearTermGen: Gen[IntExpr] =
    for {
      n <- Gen.choose(1, 3)
      vs <- Gen.pick(n, intNames)
      ks <- Gen.listOfN(n, Gen.oneOf(-7, -5, -3, -2, -1, 1, 2, 3, 4, 6))
    } yield Add(vs.toVector.zip(ks).map { case (v, k) => Mul(Vector(c(k), iv(v))) })

  /** `lo <= t <= hi` for a linear term t, with env inside: a thin slab, so
    * a few of them leave few integer points, often only near env.
    */
  def slabAt(env: Env): Gen[List[BoolExpr]] =
    for {
      t <- linearTermGen
      below <- Gen.choose(0, 2)
      above <- Gen.choose(0, 2)
    } yield {
      val v = evalInt(t, env)
      List(Gte(t, IntConst(v - below)), Lte(t, IntConst(v + above)))
    }

  /** Facts that all hold at a point and a goal that fails there, so the goal
    * never follows: the point is a counterexample. The facts include thin
    * slabs around the point, which leave few integer solutions.
    */
  val refutableCaseGen: Gen[(List[BoolExpr], BoolExpr, Env)] =
    for {
      env <- pointGen
      loose <- Gen.choose(0, 4).flatMap(Gen.listOfN(_, factGen(nonlinear = false)))
      slabs <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, slabAt(env)))
      goal <- goalGen(nonlinear = false)
    } yield (loose.map(holdingAt(env, _)) ::: slabs.flatten, failingAt(env, goal), env)

  /** An Int term with the same value as t everywhere. */
  def equivalentInt(t: IntExpr): Gen[IntExpr] =
    t match {
      case Add(args) if args.nonEmpty =>
        Gen.sequence[Vector[IntExpr], IntExpr](args.map(equivalentInt)).flatMap { as =>
          Gen.oneOf(Add(as), Add(as.reverse), Sub(Vector(Add(as :+ c(3)), c(3))))
        }
      case Sub(Vector(a, b)) =>
        Gen.zip(equivalentInt(a), equivalentInt(b)).flatMap { case (a1, b1) =>
          Gen.oneOf(Sub(Vector(a1, b1)), Add(Vector(a1, Mul(Vector(c(-1), b1)))))
        }
      case Mul(Vector(IntConst(k), a)) =>
        equivalentInt(a).flatMap { a1 =>
          Gen.oneOf(
            Mul(Vector(IntConst(k), a1)),
            Mul(Vector(a1, IntConst(k))),
            Add(Vector(Mul(Vector(IntConst(k - 1), a1)), a1))
          )
        }
      case Ite(p, a, b) =>
        Gen.zip(equivalent(p), equivalentInt(a), equivalentInt(b)).flatMap { case (p1, a1, b1) =>
          Gen.oneOf(Ite[SmtSort.IntSort](p1, a1, b1), Ite[SmtSort.IntSort](Not(p1), b1, a1))
        }
      case Div(n, d) => equivalentInt(n).map(Div(_, d))
      case Mod(n, d) => equivalentInt(n).map(Mod(_, d))
      case other     => Gen.oneOf(other, Add(Vector(other, c(0))))
    }

  /** A Bool term with the same value as e everywhere, built by identities
    * that hold over the integers.
    */
  def equivalent(e: BoolExpr): Gen[BoolExpr] = {
    def ints(l: IntExpr, r: IntExpr) = Gen.zip(equivalentInt(l), equivalentInt(r))
    def two(l: BoolExpr, r: BoolExpr) = Gen.zip(equivalent(l), equivalent(r))
    e match {
      case Lt(l, r) =>
        ints(l, r).flatMap { case (a, b) =>
          Gen.oneOf(Gt(b, a), Lte(Add(Vector(a, c(1))), b), Not(Gte(a, b)), Lt(Sub(Vector(a, b)), c(0)))
        }
      case Lte(l, r) =>
        ints(l, r).flatMap { case (a, b) =>
          Gen.oneOf(Gte(b, a), Not(Gt(a, b)), Lt(a, Add(Vector(b, c(1)))))
        }
      case Gt(l, r) => equivalent(Lt(r, l))
      case Gte(l, r) => equivalent(Lte(r, l))
      case EqInt(l, r) =>
        ints(l, r).flatMap { case (a, b) =>
          Gen.oneOf(
            EqInt(b, a),
            And(Vector(Lte(a, b), Gte(a, b))),
            EqInt(Sub(Vector(a, b)), c(0)),
            Not(Or(Vector(Lt(a, b), Gt(a, b))))
          )
        }
      case Not(a) =>
        equivalent(a).flatMap(a1 => Gen.oneOf(Not(a1), Implies(a1, BoolConst.False)))
      case And(as) =>
        Gen.sequence[Vector[BoolExpr], BoolExpr](as.map(equivalent)).flatMap { as1 =>
          Gen.oneOf(And(as1), And(as1.reverse), Not(Or(as1.map(Not(_)))))
        }
      case Or(as) =>
        Gen.sequence[Vector[BoolExpr], BoolExpr](as.map(equivalent)).flatMap { as1 =>
          Gen.oneOf(Or(as1), Or(as1.reverse), Not(And(as1.map(Not(_)))))
        }
      case Implies(a, b) =>
        two(a, b).flatMap { case (a1, b1) => Gen.oneOf(Or(Vector(Not(a1), b1)), Implies(Not(b1), Not(a1))) }
      case Xor(a, b) =>
        two(a, b).flatMap { case (a1, b1) => Gen.oneOf(Not(EqBool(a1, b1)), Xor(b1, a1)) }
      case EqBool(a, b) =>
        two(a, b).flatMap { case (a1, b1) =>
          Gen.oneOf(And(Vector(Implies(a1, b1), Implies(b1, a1))), Not(Xor(a1, b1)))
        }
      case Ite(p, a, b) =>
        Gen.zip(equivalent(p), equivalent(a), equivalent(b)).flatMap { case (p1, a1, b1) =>
          Gen.oneOf(
            And(Vector(Implies(p1, a1), Implies(Not(p1), b1))),
            Or(Vector(And(Vector(p1, a1)), And(Vector(Not(p1), b1))))
          )
        }
      case other => Gen.oneOf(other, Not(Not(other)))
    }
  }

  private val provableCond: Gen[BoolExpr] = condGen(2, intTermGen(1, nonlinear = false))

  // Each strategy gives facts the goal follows from.
  private val provableCores: Gen[(List[BoolExpr], BoolExpr)] = {
    // the goal is a fact restated
    val restated = provableCond.flatMap(f => equivalent(f).map(g => (List(f), g)))
    // a nonnegative combination of linear facts, as Fourier-Motzkin finds
    val combined =
      for {
        n <- Gen.choose(2, 4)
        ts <- Gen.listOfN(n, linearTermGen)
        ks <- Gen.listOfN(n, Gen.chooseNum(-8, 8))
        ls <- Gen.listOfN(n, Gen.choose(0, 3))
        slack <- Gen.choose(0, 2)
      } yield {
        val sum = Add(ts.zip(ls).map { case (t, l) => Mul(Vector(c(l), t)) }.toVector)
        val bound = ls.zip(ks).map { case (l, k) => l * k }.sum - slack
        (ts.zip(ks).map { case (t, k) => Gte(t, c(k)) }, Gte(sum, c(bound)))
      }
    // k * t >= m means t >= ceil(m / k), which needs t to be an integer
    val tightened =
      for {
        t <- linearTermGen
        k <- Gen.choose(2, 5)
        m <- Gen.chooseNum(-20, 20)
        up <- Gen.oneOf(true, false)
      } yield {
        val kt = Mul(Vector(c(k), t))
        if (up) (List(Gte(kt, c(m))), Gte(t, IntConst(Omega.ceilDiv(BigInt(m), BigInt(k)))))
        else (List(Lte(kt, c(m))), Lte(t, IntConst(Omega.floorDiv(BigInt(m), BigInt(k)))))
      }
    // a multiple of k strictly between two consecutive multiples of k
    val noIntegerPoint =
      for {
        t <- linearTermGen
        k <- Gen.choose(2, 5)
        m <- Gen.chooseNum(-10, 10)
      } yield {
        val kt = Mul(Vector(c(k), t))
        (List(Gte(kt, c(k * m + 1)), Lte(kt, c(k * m + k - 1))), BoolConst.False)
      }
    // case splits
    val split =
      Gen.zip(provableCond, provableCond, provableCond).map { case (a, b, g) =>
        (List(Or(Vector(a, b)), Implies(a, g), Implies(b, g)), g)
      }
    val boolSplit =
      Gen.zip(boolVarGen, provableCond).map { case (p, g) =>
        (List(Implies(p, g), Implies(Not(p), g)), g)
      }
    // t1 + a <= t2 and t2 + b <= t3 give t1 + a + b <= t3
    val chain =
      for {
        t1 <- linearTermGen
        t2 <- linearTermGen
        t3 <- linearTermGen
        a <- Gen.chooseNum(-3, 3)
        b <- Gen.chooseNum(-3, 3)
      } yield (
        List(Lte(Add(Vector(t1, c(a))), t2), Lte(Add(Vector(t2, c(b))), t3)),
        Lte(Add(Vector(t1, c(a + b))), t3)
      )
    // Bosatsu's div and mod_Int by a nonzero constant are floor division
    val divMod =
      for {
        t <- intTermGen(1, nonlinear = false)
        k <- nonzeroGen
        shape <- Gen.choose(0, 2)
      } yield {
        val q = bosatsuDiv(t, c(k))
        val r = bosatsuMod(t, c(k))
        val kq = Mul(Vector(c(k), q))
        val goal = shape match {
          case 0 => EqInt(t, Add(Vector(kq, r)))
          case 1 =>
            if (k > 0) And(Vector(Gte(r, c(0)), Lt(r, c(k))))
            else And(Vector(Lte(r, c(0)), Gt(r, c(k))))
          case _ =>
            // floor(t / k) == q: k * q <= t < k * q + k for k > 0, and the
            // other way around for k < 0
            if (k > 0) And(Vector(Lte(kq, t), Lt(t, Add(Vector(kq, c(k))))))
            else And(Vector(Gte(kq, t), Gt(t, Add(Vector(kq, c(k))))))
        }
        (Nil, goal)
      }
    // bounded thin slabs the grid shows have no integer point
    val boundedEmpty =
      Gen
        .choose(2, 3)
        .flatMap(n => Gen.listOfN(n, pointGen.flatMap(slabAt)))
        .map(slabs => boxFacts ::: slabs.flatten)
        .retryUntil(facts => counterexample(grid, facts, BoolConst.False).isEmpty, 200)
        .map(facts => (facts, BoolConst.False))

    Gen.frequency(
      3 -> restated,
      3 -> combined,
      2 -> tightened,
      1 -> noIntegerPoint,
      2 -> split,
      1 -> boolSplit,
      2 -> chain,
      2 -> divMod,
      2 -> boundedEmpty
    )
  }

  /** Facts and a goal that follows from them: one of the constructions
    * above, with unrelated facts added (more facts never stop a goal
    * following) and the goal sometimes restated.
    *
    * Check these with `forAllNoShrink`: shrinking drops facts, and the goal
    * may not follow from fewer.
    */
  val provableCaseGen: Gen[(List[BoolExpr], BoolExpr)] =
    for {
      core <- provableCores
      before <- Gen.choose(0, 2).flatMap(Gen.listOfN(_, factGen(nonlinear = false)))
      after <- Gen.choose(0, 2).flatMap(Gen.listOfN(_, factGen(nonlinear = false)))
      goal <- Gen.frequency(2 -> Gen.const(core._2), 1 -> equivalent(core._2))
    } yield (before ::: core._1 ::: after, goal)
}
