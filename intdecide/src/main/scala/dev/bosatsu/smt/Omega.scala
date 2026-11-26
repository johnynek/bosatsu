package dev.bosatsu.smt

import cats.syntax.all.*
import scala.collection.immutable.IntMap

/** Decides whether a conjunction of linear equalities and inequalities over
  * the integers has a solution, and finds one when it does. This is Pugh's
  * Omega test ("The Omega test: a fast and practical integer programming
  * algorithm for dependence analysis", 1991), which is exact over the
  * integers, unlike Fourier-Motzkin elimination over the rationals.
  *
  * Variables are numbered by Int. A problem goes through these steps, each of
  * which keeps the set of integer solutions (projected onto the variables
  * still in the problem):
  *
  *   1. Normalize every constraint: divide by the gcd of its coefficients.
  *      An equality whose constant the gcd doesn't divide has no integer
  *      solution; an inequality's constant is rounded down, which keeps the
  *      same integer solutions.
  *   2. Eliminate equalities. One with a coefficient of 1 or -1 is solved for
  *      that variable, which is substituted everywhere. Otherwise a
  *      unimodular change of variables shrinks the smallest coefficient,
  *      until one is 1 or -1.
  *   3. With only inequalities left, keep the tightest of each set of
  *      parallel ones, and turn each opposite pair that meets into an
  *      equality (back to step 2).
  *   4. Eliminate a variable. One bounded on one side only is dropped with
  *      its constraints. Otherwise each pair of a lower and an upper bound
  *      gives one constraint without the variable (Fourier-Motzkin). When
  *      every pair has a coefficient of 1 on one side this projection is
  *      exact. When it isn't, the "dark shadow" (pairs that leave room for an
  *      integer between them) is solved, and if it has no solution, so are
  *      the "splinters": the original problem with the variable pinned close
  *      to one of its lower bounds. Together these find every solution.
  *
  * Solutions are rebuilt on the way back out: an eliminated variable gets a
  * value from its definition or from its bounds at the values found for the
  * rest.
  *
  * Every step spends from a `Budget`, so the work is bounded: running out
  * gives `GaveUp`, never a wrong answer. The laws below are checked by
  * OmegaLawsTest.
  */
private[smt] object Omega {

  /** The linear function `const + sum(coeffs(v) * v)` over integer variables.
    *
    * Canonical: no coefficient is zero (the constructor is private, and every
    * operation keeps this), so two Forms are equal exactly when they are
    * equal as functions.
    *
    * Laws, for all Forms `a`, `b`, integers `k` and environments `env`:
    *   - `eval` is a homomorphism: `(a + b).eval(env) == a.eval(env) +
    *     b.eval(env)`, `(a * k).eval(env) == a.eval(env) * k`,
    *     `(a - b).eval(env) == a.eval(env) - b.eval(env)` and
    *     `a.plus(k).eval(env) == a.eval(env) + k`
    *   - `a.substitute(v, b).eval(env) == a.eval(env.updated(v,
    *     b.eval(env)))`, and the result doesn't mention `v` unless `b` does
    *   - `a == b` exactly when `a.eval(env) == b.eval(env)` for every `env`
    */
  final case class Form private (const: BigInt, coeffs: IntMap[BigInt])
      derives CanEqual {

    /** The value at env. */
    def eval(env: Int => BigInt): BigInt =
      coeffs.foldLeft(const) { case (acc, (v, c)) => acc + c * env(v) }

    def isConstant: Boolean = coeffs.isEmpty

    /** The coefficient of v, zero if v is not mentioned. */
    def coeff(v: Int): BigInt = coeffs.getOrElse(v, Form.zeroInt)

    def vars: Iterable[Int] = coeffs.keys

    def +(that: Form): Form =
      new Form(
        const + that.const,
        that.coeffs.foldLeft(coeffs) { case (acc, (v, c)) =>
          val c1 = acc.getOrElse(v, Form.zeroInt) + c
          if (c1 == 0) acc - v else acc.updated(v, c1)
        }
      )

    def *(k: BigInt): Form =
      if (k == 0) Form.zero
      else new Form(const * k, coeffs.transform((_, c) => c * k))

    def unary_- : Form = this * Form.minusOne

    def -(that: Form): Form = this + (-that)

    def plus(k: BigInt): Form = new Form(const + k, coeffs)

    /** This form without its v term. */
    def without(v: Int): Form = new Form(const, coeffs - v)

    /** This form with `by` in place of v. */
    def substitute(v: Int, by: Form): Form =
      coeffs.get(v) match {
        case Some(a) => without(v) + (by * a)
        case None    => this
      }

    // Every coefficient and the constant divided by g, which divides all of
    // them; the coefficients stay nonzero.
    private[Omega] def divideExactly(g: BigInt): Form =
      new Form(const / g, coeffs.transform((_, c) => c / g))

    // The coefficients divided by g, which divides all of them, and the
    // constant rounded down.
    private[Omega] def divideFloor(g: BigInt): Form =
      new Form(floorDiv(const, g), coeffs.transform((_, c) => c / g))
  }

  object Form {
    private[Omega] val zeroInt: BigInt = BigInt(0)
    private[Omega] val minusOne: BigInt = BigInt(-1)

    val zero: Form = new Form(zeroInt, IntMap.empty)

    def constant(c: BigInt): Form = new Form(c, IntMap.empty)

    def variable(v: Int): Form = new Form(zeroInt, IntMap.singleton(v, BigInt(1)))

    /** The canonical Form for `const + sum(c * v)`, adding the coefficients
      * of repeated variables and dropping zeros.
      */
    def of(const: BigInt, coeffs: Iterable[(Int, BigInt)]): Form =
      coeffs.foldLeft(constant(const)) { case (acc, (v, c)) =>
        acc + (variable(v) * c)
      }

    // coeffs must already have no zero coefficient, as those of a Form
    private[Omega] def fromCanonical(const: BigInt, coeffs: IntMap[BigInt]): Form =
      new Form(const, coeffs)
  }

  /** `form >= 0` or `form == 0`. */
  enum Constraint derives CanEqual {
    case Geq(form: Form)
    case Eq(form: Form)

    /** Laws: `Geq(f).holds(env) == (f.eval(env) >= 0)` and
      * `Eq(f).holds(env) == (f.eval(env) == 0)`.
      */
    def holds(env: Int => BigInt): Boolean =
      this match {
        case Geq(f) => f.eval(env) >= 0
        case Eq(f)  => f.eval(env) == 0
      }

    def vars: Iterable[Int] =
      this match {
        case Geq(f) => f.vars
        case Eq(f)  => f.vars
      }
  }

  enum Result derives CanEqual {

    /** `model` gives a value to every variable of the constraints, and they
      * all hold there.
      */
    case Sat(model: IntMap[BigInt])

    /** No integer values make every constraint hold. */
    case Unsat

    /** The budget ran out. */
    case GaveUp
  }

  /** Work left. Every constraint a step looks at or builds costs one, so the
    * total work of a run is proportional to what it spends.
    *
    * It also numbers the variables that equality elimination introduces,
    * above every variable of the problem it is given.
    */
  final class Budget(limit: Long) {
    private var left: Long = limit
    private var nextVar: Int = 0

    /** Spend n; false once the budget is overspent. */
    def spend(n: Long): Boolean = {
      left -= n
      left >= 0
    }

    def exhausted: Boolean = left < 0

    private[Omega] def avoid(vars: Iterable[Int]): Unit =
      vars.foreach(v => if (v >= nextVar) nextVar = v + 1)

    private[Omega] def freshVar(): Int = {
      val v = nextVar
      nextVar += 1
      v
    }
  }

  /** `floor(a / k)` for `k > 0`.
    *
    * Laws: for `k > 0` and `q == floorDiv(a, k)`: `k * q <= a < k * (q + 1)`.
    */
  def floorDiv(a: BigInt, k: BigInt): BigInt = {
    val q = a / k
    if ((a % k != 0) && (a < 0)) q - 1 else q
  }

  /** `ceil(a / k)` for `k > 0`.
    *
    * Laws: for `k > 0` and `q == ceilDiv(a, k)`: `k * (q - 1) < a <= k * q`.
    */
  def ceilDiv(a: BigInt, k: BigInt): BigInt = -floorDiv(-a, k)

  /** Whether the constraints have a common integer solution.
    *
    * Laws, for every list of constraints `cs`:
    *   - if `solve(cs, b) == Sat(m)` then `m` has every variable of `cs` and
    *     every constraint holds at `m`
    *   - if `solve(cs, b) == Unsat` then no integer values make every
    *     constraint hold
    *   - with a budget large enough it never returns `GaveUp`
    */
  def solve(constraints: List[Constraint], budget: Budget): Result = {
    val vars = constraints.iterator.flatMap(_.vars).toSet
    budget.avoid(vars)
    val eqs = constraints.collect { case Constraint.Eq(f) => f }
    val geqs = constraints.collect { case Constraint.Geq(f) => f }
    solveProblem(eqs, geqs, budget) match {
      case Result.Sat(model) =>
        // the internal variables are no business of the caller
        Result.Sat(IntMap.from(vars.iterator.map(v => (v, model.getOrElse(v, Form.zeroInt)))))
      case other => other
    }
  }

  private def withZeros(model: IntMap[BigInt], vars: Iterable[Int]): IntMap[BigInt] =
    vars.foldLeft(model) { (acc, v) =>
      if (acc.contains(v)) acc else acc.updated(v, Form.zeroInt)
    }

  private def gcdOf(f: Form): BigInt =
    f.coeffs.valuesIterator.foldLeft(Form.zeroInt)(_.gcd(_))

  // `eqs` are `f == 0` and `geqs` are `f >= 0`.
  private def solveProblem(eqs: List[Form], geqs: List[Form], budget: Budget): Result =
    if (!budget.spend(eqs.size + geqs.size + 1)) Result.GaveUp
    else
      normalizeEqs(eqs) match {
        case None             => Result.Unsat
        case Some(e :: rest)  => eliminateEq(e, rest, geqs, budget)
        case Some(Nil)        =>
          normalizeGeqs(geqs) match {
            case Geqs.Contradiction       => Result.Unsat
            case Geqs.Found(eqs1, geqs1)  => solveProblem(eqs1, geqs1, budget)
            case Geqs.Tightened(geqs1)    => eliminateVar(geqs1, budget)
          }
      }

  // Each equality divided by the gcd of its coefficients, and those without
  // variables dropped. None if one can't hold.
  private def normalizeEqs(eqs: List[Form]): Option[List[Form]] =
    eqs.traverseFilter { e =>
      if (e.isConstant) {
        if (e.const == 0) Some(None) else None
      } else {
        val g = gcdOf(e)
        if (e.const % g != 0) None
        else Some(Some(e.divideExactly(g)))
      }
    }

  private def eliminateEq(
      e: Form,
      rest: List[Form],
      geqs: List[Form],
      budget: Budget
  ): Result =
    // the smallest variable with one, so the choice doesn't depend on the
    // map's iteration order
    e.coeffs.iterator.filter { case (_, a) => a.abs == 1 }.minByOption(_._1) match {
      case Some((x, a)) =>
        // a * x + r == 0 with a == 1 or -1, so x == -a * r
        val definition = e.without(x) * (-a)
        val sub = solveProblem(
          rest.map(_.substitute(x, definition)),
          geqs.map(_.substitute(x, definition)),
          budget
        )
        sub match {
          case Result.Sat(m) =>
            val m1 = withZeros(m, definition.vars)
            Result.Sat(m1.updated(x, definition.eval(m1)))
          case other => other
        }
      case None =>
        // Normalized, so the gcd of the coefficients is 1 and, none being 1
        // or -1, there are at least two. Let a_k be the smallest. Writing
        // a_i == q_i * a_k + r_i with 0 <= r_i < |a_k| for the others, the
        // unimodular change of variables x_k == y - sum(q_i * x_i) turns
        // a_k * x_k + sum(a_i * x_i) into a_k * y + sum(r_i * x_i). Some r_i
        // is not zero (else a_k would divide every coefficient), so the
        // smallest coefficient shrinks, and eventually one is 1 or -1.
        val (k, ak) = e.coeffs.minBy { case (v, a) => (a.abs, v) }
        val y = budget.freshVar()
        val qs = e.coeffs.iterator.collect {
          case (i, ai) if i != k => (i, (ai - ai.mod(ak.abs)) / ak)
        }
        val definition = Form.variable(y) - Form.of(Form.zeroInt, qs.toList)
        val sub = solveProblem(
          (e :: rest).map(_.substitute(k, definition)),
          geqs.map(_.substitute(k, definition)),
          budget
        )
        sub match {
          case Result.Sat(m) =>
            val m1 = withZeros(m, definition.vars)
            Result.Sat(m1.updated(k, definition.eval(m1)))
          case other => other
        }
    }

  private enum Geqs derives CanEqual {
    case Contradiction
    // some opposite pairs met: equalities, and the other inequalities
    case Found(eqs: List[Form], geqs: List[Form])
    case Tightened(geqs: List[Form])
  }

  // Inequalities divided by the gcd of their coefficients, those without
  // variables checked and dropped, and of each set with the same
  // coefficients only the tightest kept, in the order they first appear.
  // `f >= 0` and `-f >= 0` mean `f == 0`.
  private def normalizeGeqs(geqs: List[Form]): Geqs = {
    type Coeffs = IntMap[BigInt]
    def negate(k: Coeffs): Coeffs = k.transform((_, c) => -c)

    // the smallest constant for each set of coefficients
    var tightest = Map.empty[Coeffs, BigInt]
    var order = List.empty[Coeffs]
    var contradiction = false
    geqs.foreach { f =>
      if (f.isConstant) {
        if (f.const < 0) contradiction = true
      } else {
        val f1 = f.divideFloor(gcdOf(f))
        tightest.get(f1.coeffs) match {
          case Some(c) =>
            if (f1.const < c) tightest = tightest.updated(f1.coeffs, f1.const)
          case None =>
            tightest = tightest.updated(f1.coeffs, f1.const)
            order = f1.coeffs :: order
        }
      }
    }
    if (contradiction) Geqs.Contradiction
    else {
      val keys = order.reverse
      // c1 + p >= 0 and c2 - p >= 0 mean -c1 <= p <= c2. Each such pair is
      // found from both sides.
      val pairs = keys.flatMap { k =>
        tightest.get(negate(k)).map(c2 => (k, tightest(k), c2))
      }
      if (pairs.exists { case (_, c1, c2) => c1 + c2 < 0 }) Geqs.Contradiction
      else {
        val met: Set[Coeffs] =
          pairs.iterator.collect { case (k, c1, c2) if c1 + c2 == 0 => k }.toSet
        if (met.isEmpty)
          Geqs.Tightened(keys.map(k => Form.fromCanonical(tightest(k), k)))
        else {
          // one equality per pair: from the side that appears first
          val (eqs, _) = keys.foldLeft((List.empty[Form], Set.empty[Coeffs])) {
            case ((acc, seen), k) =>
              if (met(k) && !seen(negate(k)))
                (Form.fromCanonical(tightest(k), k) :: acc, seen + k)
              else (acc, seen)
          }
          val rest = keys.filterNot(met).map(k => Form.fromCanonical(tightest(k), k))
          Geqs.Found(eqs.reverse, rest)
        }
      }
    }
  }

  // Projects a variable out of inequalities, all normalized and none
  // constant.
  private def eliminateVar(geqs: List[Form], budget: Budget): Result =
    if (geqs.isEmpty) Result.Sat(IntMap.empty)
    else {
      val vars = geqs.iterator.flatMap(_.vars).toVector.distinct.sorted
      def lowers(x: Int): List[Form] = geqs.filter(_.coeff(x) > 0)
      def uppers(x: Int): List[Form] = geqs.filter(_.coeff(x) < 0)

      vars.find(x => lowers(x).isEmpty || uppers(x).isEmpty) match {
        case Some(x) =>
          // x is bounded on one side only: any value of the other variables
          // leaves room for x, so its constraints can go
          val (withX, others) = geqs.partition(_.coeff(x) != 0)
          solveProblem(Nil, others, budget) match {
            case Result.Sat(m) => Result.Sat(placeVar(x, withX, m))
            case other         => other
          }
        case None =>
          // An exact projection needs a coefficient of 1 on every lower
          // bound or on every upper bound. Prefer those, then the fewest
          // new constraints.
          def exact(x: Int): Boolean =
            lowers(x).forall(_.coeff(x) == 1) || uppers(x).forall(_.coeff(x) == -1)
          def cost(x: Int): Int = lowers(x).size * uppers(x).size
          val x = vars.minBy(x => (if (exact(x)) 0 else 1, cost(x), x))
          eliminateBounded(x, geqs, exact(x), budget)
      }
    }

  // The value of x from its bounds at m: the largest lower bound, else the
  // smallest upper bound, else 0. When there is an integer between every
  // pair of bounds at m, it is in all of them.
  private def placeVar(x: Int, bounds: List[Form], m: IntMap[BigInt]): IntMap[BigInt] = {
    val m1 = withZeros(m, bounds.iterator.flatMap(_.vars).filter(_ != x).toList)
    // a * x + rest >= 0 is x >= ceil(-rest / a) for a > 0 and
    // x <= floor(rest / -a) for a < 0
    val los = bounds.collect {
      case f if f.coeff(x) > 0 => ceilDiv(-f.without(x).eval(m1), f.coeff(x))
    }
    val his = bounds.collect {
      case f if f.coeff(x) < 0 => floorDiv(f.without(x).eval(m1), -f.coeff(x))
    }
    val value =
      if (los.nonEmpty) los.max
      else if (his.nonEmpty) his.min
      else Form.zeroInt
    m1.updated(x, value)
  }

  // x has lower and upper bounds.
  private def eliminateBounded(x: Int, geqs: List[Form], exact: Boolean, budget: Budget): Result = {
    val (withX, others) = geqs.partition(_.coeff(x) != 0)
    val lows = withX.filter(_.coeff(x) > 0)
    val highs = withX.filter(_.coeff(x) < 0)
    // For a lower bound f == a * x + r >= 0 and an upper bound
    // g == -b * x + s >= 0 (a, b > 0): b * f + a * g == b * r + a * s, which
    // must be >= 0 for a real x to fit (the real shadow), and
    // >= (a - 1) * (b - 1) for an integer x to fit whatever the other values
    // are (the dark shadow).
    def shadow(dark: Boolean): List[Form] =
      others ::: (for {
        f <- lows
        g <- highs
      } yield {
        val a = f.coeff(x)
        val b = -g.coeff(x)
        val sum = (f * b) + (g * a)
        if (dark) sum.plus(-((a - 1) * (b - 1))) else sum
      })

    def rebuild(r: Result): Result =
      r match {
        case Result.Sat(m) => Result.Sat(placeVar(x, withX, m))
        case other         => other
      }

    // A shadow has a constraint for every pair, which can be far more than
    // the problem had, so it is paid for before it is built.
    val pairs = lows.size.toLong * highs.size.toLong
    def solveShadow(dark: Boolean): Result =
      if (budget.spend(pairs)) solveProblem(Nil, shadow(dark), budget)
      else Result.GaveUp

    if (exact) rebuild(solveShadow(dark = false))
    else
      solveShadow(dark = false) match {
        case Result.Sat(_) =>
          solveShadow(dark = true) match {
            case Result.Unsat => splinters(x, geqs, lows, highs, budget)
            case other        => rebuild(other)
          }
        case other => other
      }
  }

  // A solution outside the dark shadow sits close to a lower bound: for a
  // pair f == a * x + r >= 0 and g == -b * x + s >= 0 whose dark shadow
  // fails, b * f <= b * r + a * s < (a - 1) * (b - 1), so
  // f <= (a * b - a - b) / b <= (a * m - a - m) / m, with m the largest b.
  // So every solution is in the dark shadow or has f == i for a lower bound f
  // and some 0 <= i <= floor((a * m - a - m) / m).
  private def splinters(
      x: Int,
      geqs: List[Form],
      lows: List[Form],
      highs: List[Form],
      budget: Budget
  ): Result = {
    val m = highs.iterator.map(g => -g.coeff(x)).max
    val pinned = lows.iterator.flatMap { f =>
      val a = f.coeff(x)
      val top = floorDiv(a * m - a - m, m)
      Iterator.iterate(Form.zeroInt)(_ + 1).takeWhile(_ <= top).map(i => f.plus(-i))
    }
    // The first solution found wins. There can be as many splinters as the
    // coefficients are large, so this stops as soon as the budget is gone
    // (the only reason a step gives up) instead of trying the rest.
    @annotation.tailrec
    def loop(): Result =
      if (!pinned.hasNext) Result.Unsat
      else
        solveProblem(List(pinned.next()), geqs, budget) match {
          case Result.Unsat => loop()
          case other        => other
        }
    loop()
  }
}
