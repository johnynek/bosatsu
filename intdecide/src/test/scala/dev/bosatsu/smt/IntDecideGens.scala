package dev.bosatsu.smt

import org.scalacheck.Gen

import LinearBoundsGens.{Env, byZeroChoices, evalBool}

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
}
