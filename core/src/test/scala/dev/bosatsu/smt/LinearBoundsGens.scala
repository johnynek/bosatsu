package dev.bosatsu.smt

import org.scalacheck.Gen

import LinearBounds.{Box, Interval, Lin}

/** Generators and an SMT-LIB evaluator for the LinearBounds tests. */
object LinearBoundsGens {
  import SmtExpr.*

  // ---------------------------------------------------------------------------
  // Integers

  // Mostly small values, so overlaps and cancellations are common, plus some
  // well beyond Long.
  val bigIntGen: Gen[BigInt] =
    Gen.frequency(
      6 -> Gen.chooseNum(-10, 10).map(BigInt(_)),
      3 -> Gen.chooseNum(-1000, 1000).map(BigInt(_)),
      2 -> Gen.chooseNum(Long.MinValue, Long.MaxValue).map(BigInt(_)),
      1 -> Gen.zip(Gen.chooseNum(Long.MinValue, Long.MaxValue), Gen.chooseNum(1, 64)).map {
        case (l, s) => BigInt(l) << s
      },
      1 -> Gen.oneOf(BigInt(0), BigInt(1), BigInt(-1))
    )

  val positiveBigIntGen: Gen[BigInt] =
    bigIntGen.map(b => if (b > 0) b else -b + 1)

  // ---------------------------------------------------------------------------
  // Lin

  // a small pool, so Lins share variables and terms cancel
  val linVarNames: Vector[String] = Vector("a", "b", "c", "d")

  // Built directly, including zero coefficients that Lin.of must drop.
  val linDirectGen: Gen[Lin] =
    for {
      c <- bigIntGen
      n <- Gen.choose(0, 4)
      terms <- Gen.listOfN(
        n,
        Gen.zip(Gen.oneOf(linVarNames), Gen.frequency(5 -> bigIntGen, 1 -> Gen.const(BigInt(0))))
      )
    } yield Lin.of(c, terms.toMap)

  // Built through the operations, so their results are generated too.
  private def linOpsGen(depth: Int): Gen[Lin] =
    if (depth <= 0)
      Gen.frequency(
        4 -> linDirectGen,
        1 -> bigIntGen.map(Lin.constant),
        1 -> Gen.oneOf(linVarNames).map(Lin.variable),
        1 -> Gen.const(Lin.zero)
      )
    else {
      val rec = linOpsGen(depth - 1)
      Gen.frequency(
        4 -> rec,
        2 -> Gen.zip(rec, rec).map { case (a, b) => a + b },
        2 -> Gen.zip(rec, rec).map { case (a, b) => a - b },
        1 -> rec.map(-_),
        2 -> Gen.zip(rec, bigIntGen).map { case (a, k) => a * k },
        1 -> Gen.zip(rec, bigIntGen).map { case (a, k) => a.plus(k) },
        1 -> rec.map(a => a - a)
      )
    }

  val linGen: Gen[Lin] = linOpsGen(3)

  val linEnvGen: Gen[Map[String, BigInt]] =
    Gen.listOfN(linVarNames.size, bigIntGen).map(vs => linVarNames.zip(vs).toMap)

  // ---------------------------------------------------------------------------
  // Interval

  // Bounds close together, so overlaps, touching ends, points and empty
  // intervals are common, with some large values.
  val intervalEndGen: Gen[BigInt] =
    Gen.frequency(6 -> Gen.chooseNum(-8, 8).map(BigInt(_)), 1 -> bigIntGen)

  private val optEndGen: Gen[Option[BigInt]] =
    Gen.frequency(1 -> Gen.const(None), 3 -> intervalEndGen.map(Some(_)))

  // About half of the finite ones are empty.
  val intervalGen: Gen[Interval] =
    Gen.frequency(
      8 -> Gen.zip(optEndGen, optEndGen).map { case (l, h) => Interval(l, h) },
      2 -> Gen.zip(intervalEndGen, intervalEndGen).map { case (a, b) =>
        Interval(Some(a.min(b)), Some(a.max(b)))
      },
      1 -> intervalEndGen.map(Interval.point),
      1 -> Gen.const(Interval.all),
      1 -> Gen.const(Interval.empty)
    )

  val nonEmptyIntervalGen: Gen[Interval] = intervalGen.retryUntil(!_.isEmpty)

  /** Members of a nonempty interval, often at or next to its bounds. */
  def memberGen(i: Interval): Gen[BigInt] = {
    val offset = Gen.frequency(3 -> Gen.chooseNum(0, 3).map(BigInt(_)), 1 -> positiveBigIntGen)
    (i.lo, i.hi) match {
      case (Some(l), Some(h)) =>
        require(l <= h, s"empty interval $i")
        Gen.frequency(2 -> Gen.const(l), 2 -> Gen.const(h), 4 -> Gen.choose(l, h))
      case (Some(l), None) => offset.map(l + _)
      case (None, Some(h)) => offset.map(h - _)
      case (None, None)    => bigIntGen
    }
  }

  /** For intervals, membership can only change at a bound. So two intervals
    * contain the same integers exactly when they agree at every bound, every
    * bound plus or minus one, and a point beyond all bounds on each side.
    */
  def probes(is: Interval*): List[BigInt] = {
    val ends = is.toList.flatMap(i => i.lo.toList ::: i.hi.toList)
    val far = ends.map(_.abs).foldLeft(BigInt(0))(_.max(_)) * 2 + 10
    (far :: -far :: ends.flatMap(e => List(e - 1, e, e + 1))).distinct
  }

  /** True when a and b contain the same integers. */
  def sameSet(a: Interval, b: Interval): Boolean =
    probes(a, b).forall(x => a.contains(x) == b.contains(x))

  // ---------------------------------------------------------------------------
  // Box

  val boxVarNames: Vector[String] = Vector("x", "y", "z")

  // Nonempty intervals for some variables, and a few excluded values, chosen
  // so each interval keeps a member.
  val boxGen: Gen[Box] = {
    val small = Gen.chooseNum(-8, 8).map(BigInt(_))
    val ivGen: Gen[Interval] =
      Gen.frequency(
        4 -> Gen.zip(small, Gen.chooseNum(0, 8)).map { case (l, w) => Interval(Some(l), Some(l + w)) },
        1 -> small.map(l => Interval(Some(l), None)),
        1 -> small.map(h => Interval(None, Some(h))),
        1 -> Gen.const(Interval.all)
      )
    def excludedGen(i: Interval): Gen[Set[BigInt]] = {
      val wide = (i.lo, i.hi) match {
        case (Some(l), Some(h)) => h - l >= 4
        case _                  => true
      }
      if (!wide) Gen.const(Set.empty)
      else {
        val near = (i.lo.toList.flatMap(l => List(l, l + 1)) ::: i.hi.toList.flatMap(h => List(h, h - 1)))
        val candidates = if (near.isEmpty) List(BigInt(0), BigInt(1)) else near
        Gen.frequency(2 -> Gen.const(Set.empty[BigInt]), 1 -> Gen.someOf(candidates).map(_.toSet.take(2)))
      }
    }
    Gen
      .sequence[List[Option[(String, Interval, Set[BigInt])]], Option[(String, Interval, Set[BigInt])]](
        boxVarNames.toList.map { v =>
          Gen.frequency(
            1 -> Gen.const(None),
            4 -> ivGen.flatMap(i => excludedGen(i).map(ex => Some((v, i, ex))))
          )
        }
      )
      .map { entries =>
        val es = entries.flatten
        Box(
          es.map { case (v, i, _) => (v, i) }.toMap,
          es.collect { case (v, _, ex) if ex.nonEmpty => (v, ex) }.toMap
        )
      }
  }

  /** An env (over boxVarNames) with every variable in its interval, maybe
    * excluded.
    */
  def envNearBoxGen(b: Box): Gen[Map[String, BigInt]] =
    Gen
      .sequence[List[(String, BigInt)], (String, BigInt)](
        boxVarNames.toList.map(v => memberGen(b.interval(v)).map((v, _)))
      )
      .map(_.toMap)

  /** An env with each variable at a probe point of its interval or next to an
    * excluded value: where membership in the box can change.
    */
  def envProbeBoxGen(b: Box): Gen[Map[String, BigInt]] =
    Gen
      .sequence[List[(String, BigInt)], (String, BigInt)](
        boxVarNames.toList.map { v =>
          val near = b.excluded.getOrElse(v, Set.empty[BigInt]).toList.flatMap(x => List(x - 1, x, x + 1))
          Gen.oneOf(probes(b.interval(v)) ::: near).map((v, _))
        }
      )
      .map(_.toMap)

  /** An env in the box. */
  def envInBoxGen(b: Box): Gen[Map[String, BigInt]] =
    envNearBoxGen(b).retryUntil(b.contains)

  /** An env in the box with one variable moved to a probe point of its
    * interval or next to an excluded value. The other variables stay inside,
    * so membership turns on that one variable.
    */
  def envEdgeOfBoxGen(b: Box): Gen[Map[String, BigInt]] =
    for {
      env <- envInBoxGen(b)
      v <- Gen.oneOf(boxVarNames)
      near = b.excluded.getOrElse(v, Set.empty[BigInt]).toList.flatMap(x => List(x - 1, x, x + 1))
      x <- Gen.oneOf(probes(b.interval(v)) ::: near)
    } yield env.updated(v, x)

  val boxEnvGen: Gen[(Box, Map[String, BigInt])] =
    boxGen.flatMap(b => envInBoxGen(b).map((b, _)))

  /** Lins over the box variables with small coefficients, so facts built from
    * them often hold in a box.
    */
  val boxLinGen: Gen[Lin] =
    for {
      c <- Gen.chooseNum(-10, 10).map(BigInt(_))
      n <- Gen.choose(0, 2)
      terms <- Gen.listOfN(
        n,
        Gen.zip(Gen.oneOf(boxVarNames), Gen.oneOf(-3, -2, -1, 1, 2, 3).map(BigInt(_)))
      )
    } yield Lin.of(c, terms.toMap)

  val relGen: Gen[LinearBounds.Rel] =
    Gen.oneOf(LinearBounds.Rel.Le, LinearBounds.Rel.Ge, LinearBounds.Rel.Eq, LinearBounds.Rel.Ne)

  // ---------------------------------------------------------------------------
  // SMT expressions

  val varNames: Vector[String] = boxVarNames

  def v(name: String): IntExpr = Var[SmtSort.IntSort](name)
  def c(n: Int): IntExpr = IntConst(BigInt(n))

  val varGen: Gen[IntExpr] = Gen.oneOf(varNames).map(v)
  val constGen: Gen[IntExpr] = Gen.chooseNum(-6, 6).map(c)

  // The embedded z3 traps on division or mod by zero, by a variable (even
  // when guarded), and on some nonlinear (QF_NIA) inputs, so the z3
  // properties use linear arithmetic with nonzero constant divisors. The grid
  // property also covers products, and zero and variable divisors.
  def divisorGen(z3Safe: Boolean): Gen[Int] =
    if (z3Safe) Gen.frequency(6 -> Gen.chooseNum(1, 5), 2 -> Gen.chooseNum(-4, -1))
    else Gen.frequency(6 -> Gen.chooseNum(1, 5), 1 -> Gen.const(0), 2 -> Gen.chooseNum(-4, -1))

  def cmpGen(l: Gen[IntExpr], r: Gen[IntExpr]): Gen[BoolExpr] =
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
  def intExprGen(depth: Int, z3Safe: Boolean): Gen[IntExpr] = {
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
  lazy val constIteCond: Gen[BoolExpr] =
    Gen.oneOf(cmpGen(constGen, constGen), Gen.oneOf(true, false).map(BoolConst(_)))

  /** Booleans over comparisons of the given Int expressions. */
  def boolExprGen(depth: Int, ints: Gen[IntExpr]): Gen[BoolExpr] = {
    val leaf = Gen.frequency(4 -> cmpGen(ints, ints), 1 -> Gen.oneOf(true, false).map(BoolConst(_)))
    if (depth <= 0) leaf
    else {
      val rec = boolExprGen(depth - 1, ints)
      Gen.frequency(
        3 -> leaf,
        2 -> rec.map(Not(_)),
        1 -> Gen.choose(0, 3).flatMap(Gen.listOfN(_, rec)).map(as => And(as.toVector)),
        1 -> Gen.choose(0, 3).flatMap(Gen.listOfN(_, rec)).map(as => Or(as.toVector))
      )
    }
  }

  // a * v + d compared with a constant, in every orientation
  val singleVarFactGen: Gen[BoolExpr] = {
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

  def noiseFactGen(z3Safe: Boolean): Gen[BoolExpr] =
    Gen.frequency(
      3 -> cmpGen(varGen, varGen),
      2 -> cmpGen(intExprGen(2, z3Safe), intExprGen(1, z3Safe)),
      1 -> Gen.const(BoolConst.True),
      1 -> Gen.const(BoolConst.False)
    )

  def factGen(z3Safe: Boolean): Gen[BoolExpr] =
    Gen.frequency(
      8 -> singleVarFactGen,
      2 -> noiseFactGen(z3Safe),
      1 -> Gen.zip(singleVarFactGen, singleVarFactGen).map { case (a, b) => And(Vector(a, b)) }
    )

  // most facts bound a variable, so a good share of goals are provable
  def factsGen(z3Safe: Boolean): Gen[List[BoolExpr]] =
    Gen.choose(1, 6).flatMap(Gen.listOfN(_, factGen(z3Safe)))

  val templateGoalGen: Gen[BoolExpr] =
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

  def goalGen(z3Safe: Boolean): Gen[BoolExpr] = {
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

  def caseGen(z3Safe: Boolean): Gen[(List[BoolExpr], BoolExpr)] =
    Gen.zip(factsGen(z3Safe), goalGen(z3Safe))
  val z3CaseGen: Gen[(List[BoolExpr], BoolExpr)] = caseGen(z3Safe = true)
  val anyCaseGen: Gen[(List[BoolExpr], BoolExpr)] = caseGen(z3Safe = false)

  // ---------------------------------------------------------------------------
  // Evaluation with SMT-LIB semantics

  // For d != 0, n = d * q + r with 0 <= r < |d|.
  def euclid(n: BigInt, d: BigInt): (BigInt, BigInt) = {
    val q0 = n / d.abs
    val q1 = if ((n % d.abs != 0) && (n < 0)) q0 - 1 else q0
    val q = if (d > 0) q1 else -q1
    (q, n - d * q)
  }

  // SMT-LIB leaves division and mod by zero unspecified (any function of the
  // numerator), so a law must hold for every one of these choices.
  val byZeroChoices: List[BigInt => BigInt] =
    List(_ => BigInt(0), _ => BigInt(1), _ => BigInt(-1), _ => BigInt(7), _ => BigInt(-7), n => n, n => -n)

  final case class Env(vars: Map[String, BigInt], byZero: BigInt => BigInt)

  def evalInt(e: IntExpr, env: Env): BigInt =
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

  def evalBool(e: BoolExpr, env: Env): Boolean =
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

  /** Every point of [-6, 6]^3, under each interpretation of division by zero. */
  val grid: List[Env] =
    (for {
      x <- -6 to 6
      y <- -6 to 6
      z <- -6 to 6
      byZero <- byZeroChoices
    } yield Env(Map("x" -> BigInt(x), "y" -> BigInt(y), "z" -> BigInt(z)), byZero)).toList

  def gridCounterexample(facts: List[BoolExpr], goal: BoolExpr): Option[Env] =
    grid.find(env => facts.forall(evalBool(_, env)) && !evalBool(goal, env))
}
