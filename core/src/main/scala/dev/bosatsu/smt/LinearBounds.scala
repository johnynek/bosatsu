package dev.bosatsu.smt

import SmtExpr.*

/** A small, sound prover for the integer goals that recursion checking sends
  * to the solver, so the common cases don't need z3.
  *
  * Those goals look like "given `n > 0`, `(n - 1) div 2 >= 0`" or "given
  * `n > 0`, `n div 10 < n`". This decides them in two steps:
  *
  *   1. From the facts, find a lower and upper bound for each integer variable
  *      (an interval, possibly unbounded on either side). Facts that bound a
  *      single variable count, including `!=`, for example `n >= 0` and
  *      `n != 0` give `n >= 1`. All other facts are ignored, which only makes
  *      the facts weaker, so it is sound.
  *   2. For a goal `a < b`, find linear expressions `U >= a` and `L <= b`
  *      (over the variables), and check that `U - L <= -1` everywhere in the
  *      variables' intervals. Linear bounds keep relations between terms, so
  *      `(n - 1) div 2 < n` works: for `n - 1 >= 0`, `(n - 1) div 2 <= n - 1`.
  *
  * Terms mean what they mean in the SMT-LIB that `SmtToSExpr` renders for z3:
  * `+`, `-` and `*` with one argument render as that argument (so
  * `Sub(Vector(x))` is `x`, not `-x`), and with no arguments they render as
  * invalid SMT-LIB, so those are never used to prove anything.
  *
  * `div` and `mod` follow SMT-LIB semantics: for a constant `k > 0`,
  * `a div k` is `floor(a / k)`, and `a mod k` is in `[0, |k| - 1]` for any
  * constant `k != 0`. Division by a variable or by zero, uninterpreted
  * functions and booleans other than the connectives are never used to prove
  * anything.
  *
  * Below, an environment `env` gives every integer variable a value, and
  * "`env` in box `b`" means `b.contains(env)`. The laws of each function are
  * checked by LinearBoundsLawsTest, and `implies` against z3 by
  * LinearBoundsTest.
  *
  * `implies` returning false means "not proven here": callers fall back to z3.
  */
private[smt] object LinearBounds {

  /** The linear function `const + sum(coeffs(v) * v)` over integer variables.
    *
    * Canonical: no coefficient is zero (the constructor is private, and every
    * operation keeps this), so two Lins are equal exactly when they are equal
    * as functions.
    *
    * Laws, for all Lins `a`, `b`, `c`, integers `j`, `k` and environments
    * `env`:
    *   - `eval` is a homomorphism: `(a + b).eval(env) == a.eval(env) +
    *     b.eval(env)`, `(a * k).eval(env) == a.eval(env) * k`,
    *     `(-a).eval(env) == -a.eval(env)`, `(a - b).eval(env) == a.eval(env) -
    *     b.eval(env)` and `a.plus(k).eval(env) == a.eval(env) + k`
    *   - `+` is associative and commutative, with identity `Lin.zero` and
    *     inverse `-a`
    *   - `*` distributes over `+` on either side, `(a * j) * k == a * (j * k)`,
    *     `a * 1 == a` and `a * 0 == Lin.zero`
    *   - `a - b == a + (-b)`, `-a == a * -1` and `a.plus(k) == a +
    *     Lin.constant(k)`
    *   - `a == b` exactly when `a.eval(env) == b.eval(env)` for every `env`
    */
  final case class Lin private (const: BigInt, coeffs: Map[String, BigInt])
      derives CanEqual {

    /** The value at env; variables not in coeffs aren't read. */
    def eval(env: String => BigInt): BigInt =
      coeffs.foldLeft(const) { case (acc, (v, c)) => acc + c * env(v) }

    /** Laws: `(a + b).eval(env) == a.eval(env) + b.eval(env)`; associative,
      * commutative, identity `Lin.zero`.
      */
    def +(that: Lin): Lin =
      new Lin(
        const + that.const,
        that.coeffs.foldLeft(coeffs) { case (acc, (v, c)) =>
          val c1 = acc.getOrElse(v, BigInt(0)) + c
          if (c1 == 0) acc - v else acc.updated(v, c1)
        }
      )

    /** Laws: `(a * k).eval(env) == a.eval(env) * k`; `a * 0 == Lin.zero`;
      * `a * 1 == a`; `(a * j) * k == a * (j * k)`; distributes over `+`.
      */
    def *(k: BigInt): Lin =
      if (k == 0) Lin.zero
      else new Lin(const * k, coeffs.map { case (v, c) => (v, c * k) })

    /** Laws: `(-a).eval(env) == -a.eval(env)`; `a + (-a) == Lin.zero`;
      * `-(-a) == a`.
      */
    def unary_- : Lin = this * BigInt(-1)

    /** Laws: `(a - b).eval(env) == a.eval(env) - b.eval(env)`; `a - b == a +
      * (-b)`; `a - a == Lin.zero`.
      */
    def -(that: Lin): Lin = this + (-that)

    /** Laws: `a.plus(k).eval(env) == a.eval(env) + k`; `a.plus(k) == a +
      * Lin.constant(k)`.
      */
    def plus(k: BigInt): Lin = new Lin(const + k, coeffs)
  }

  object Lin {

    /** Laws: `zero.eval(env) == 0`; the identity of `+`. */
    val zero: Lin = new Lin(BigInt(0), Map.empty)

    /** Laws: `constant(c).eval(env) == c`; no variables. */
    def constant(c: BigInt): Lin = new Lin(c, Map.empty)

    /** Laws: `variable(v).eval(env) == env(v)`. */
    def variable(v: String): Lin = new Lin(BigInt(0), Map(v -> BigInt(1)))

    /** The canonical Lin for `const + sum(coeffs(v) * v)`, dropping zero
      * coefficients.
      *
      * Laws: `of(c, m).eval(env) == c + sum(m(v) * env(v))`; `of(a.const,
      * a.coeffs) == a`.
      */
    def of(const: BigInt, coeffs: Map[String, BigInt]): Lin =
      new Lin(const, coeffs.filter { case (_, c) => c != 0 })
  }

  /** The integers `x` with `lo <= x <= hi`. Both bounds are inclusive, and
    * None means unbounded on that side. The interval is empty when `lo > hi`.
    *
    * Every operation is exact on the sets it denotes, as stated with
    * `contains` in each method's laws, except where a law says the result is
    * the smallest interval containing a set that isn't itself an interval.
    */
  final case class Interval(lo: Option[BigInt], hi: Option[BigInt])
      derives CanEqual {

    /** Laws: `contains(x) == (lo.forall(_ <= x) && hi.forall(x <= _))`. */
    def contains(x: BigInt): Boolean =
      (lo match {
        case Some(l) => l <= x
        case None    => true
      }) && (hi match {
        case Some(h) => x <= h
        case None    => true
      })

    /** Laws: `isEmpty` exactly when no `x` has `contains(x)`. */
    def isEmpty: Boolean =
      (lo, hi) match {
        case (Some(l), Some(h)) => l > h
        case _                  => false
      }

    /** The sums `x + y` of members `x` of this and `y` of that.
      *
      * Laws: if `a.contains(x)` and `b.contains(y)` then `(a +
      * b).contains(x + y)`; if `(a + b).contains(z)` then there are `x`, `y`
      * with `a.contains(x)`, `b.contains(y)` and `x + y == z`; empty if either
      * is empty.
      */
    def +(that: Interval): Interval =
      if (isEmpty || that.isEmpty) Interval.empty
      else Interval(addOpt(lo, that.lo), addOpt(hi, that.hi))

    /** Laws: `(-a).contains(-x) == a.contains(x)`; `-(-a)` contains exactly
      * the members of `a`.
      */
    def unary_- : Interval = Interval(negOpt(hi), negOpt(lo))

    /** The smallest interval containing `x * k` for every member `x`.
      *
      * Laws: if `a.contains(x)` then `(a * k).contains(x * k)`; for `k != 0`
      * every bound of `a * k` is `x * k` for a bound `x` of `a` (so nothing
      * smaller contains the products); `a * 0` is `Interval.point(0)` unless
      * `a` is empty; empty if `a` is empty.
      */
    def *(k: BigInt): Interval =
      if (isEmpty) Interval.empty
      else if (k == 0) Interval.point(BigInt(0))
      else if (k > 0) Interval(mulOpt(lo, k), mulOpt(hi, k))
      else Interval(mulOpt(hi, k), mulOpt(lo, k))

    /** The smallest interval containing both: the convex hull of the union.
      *
      * Laws: if `a.contains(x) || b.contains(x)` then `(a join
      * b).contains(x)`; every bound of `a join b` is a bound of `a` or `b`
      * (so nothing smaller contains both); `Interval.empty` is the identity;
      * commutative, associative and idempotent.
      */
    def join(that: Interval): Interval =
      if (isEmpty) that
      else if (that.isEmpty) this
      else
        Interval(
          (lo, that.lo) match {
            case (Some(a), Some(b)) => Some(a.min(b))
            case _                  => None
          },
          (hi, that.hi) match {
            case (Some(a), Some(b)) => Some(a.max(b))
            case _                  => None
          }
        )

    /** The intersection.
      *
      * Laws: `(a meet b).contains(x) == (a.contains(x) && b.contains(x))`;
      * `Interval.all` is the identity; commutative, associative and
      * idempotent; absorption with `join`.
      */
    def meet(that: Interval): Interval =
      Interval(maxOpt(lo, that.lo), minOpt(hi, that.hi))

    /** The smallest interval containing `floorDiv(x, k)` for every member `x`,
      * for `k > 0`.
      *
      * Laws: if `a.contains(x)` then `a.divFloor(k).contains(floorDiv(x,
      * k))`; every bound of the result is `floorDiv(x, k)` for a bound `x` of
      * `a`; empty if `a` is empty.
      */
    def divFloor(k: BigInt): Interval = {
      require(k > 0, s"divFloor needs k > 0, got $k")
      if (isEmpty) Interval.empty
      else
        Interval(
          lo match {
            case Some(l) => Some(floorDiv(l, k))
            case None    => None
          },
          hi match {
            case Some(h) => Some(floorDiv(h, k))
            case None    => None
          }
        )
    }
  }

  object Interval {

    /** Laws: `all.contains(x)` for every `x`; the identity of `meet`. */
    val all: Interval = Interval(None, None)

    /** Laws: `!empty.contains(x)` for every `x`; the identity of `join`. */
    val empty: Interval = Interval(Some(BigInt(1)), Some(BigInt(0)))

    /** Laws: `point(c).contains(x) == (x == c)`. */
    def point(c: BigInt): Interval = Interval(Some(c), Some(c))
  }

  // Interval bound arithmetic. None is an infinite bound, and each helper is
  // only used on one kind of bound, as its name says.

  // Laws: the sum of two lower bounds, or of two upper bounds; infinite if
  // either is.
  private def addOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(x + y)
      case _                  => None
    }

  // Laws: negating flips a lower bound to an upper bound and back; infinite
  // stays infinite.
  private def negOpt(a: Option[BigInt]): Option[BigInt] =
    a match {
      case Some(x) => Some(-x)
      case None    => None
    }

  // Laws: scaling a bound; infinite stays infinite (callers handle k == 0 and
  // swap ends for k < 0).
  private def mulOpt(a: Option[BigInt], k: BigInt): Option[BigInt] =
    a match {
      case Some(x) => Some(x * k)
      case None    => None
    }

  // Laws: the larger of two lower bounds, None (-infinity) the smallest.
  private def maxOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(x.max(y))
      case (None, y)          => y
      case (x, None)          => x
    }

  // Laws: the smaller of two upper bounds, None (+infinity) the largest.
  private def minOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(x.min(y))
      case (None, y)          => y
      case (x, None)          => x
    }

  /** `floor(a / k)` for `k > 0`. BigInt's `/` rounds toward zero.
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

  // Keep the number of candidate bounds small; sums take all pairs.
  private val MaxCandidates = 4

  /** Variable intervals from the facts, and values each variable can't be.
    * Variables not in `intervals` are unbounded.
    */
  final case class Box(
      intervals: Map[String, Interval],
      excluded: Map[String, Set[BigInt]]
  ) {

    /** Laws: `b.interval(v)` is `Interval.all` for a variable not in
      * `intervals`.
      */
    def interval(v: String): Interval = intervals.getOrElse(v, Interval.all)

    /** Laws: `b.contains(env)` exactly when every variable is in its interval
      * and not excluded.
      */
    def contains(env: String => BigInt): Boolean =
      intervals.forall { case (v, i) => i.contains(env(v)) } &&
        excluded.forall { case (v, xs) => !xs(env(v)) }
  }

  /** The exact linear form of e, if it has one.
    *
    * Laws: if `linear(e) == Some(l)` then `l.eval(env)` is the value of `e` at
    * `env`, for every `env`.
    */
  def linear(e: IntExpr): Option[Lin] =
    e match {
      case IntConst(c) => Some(Lin.constant(c))
      case Var(name)   => Some(Lin.variable(name))
      case Add(args) if args.nonEmpty =>
        args.foldLeft(Option(Lin.zero)) { (acc, a) =>
          acc match {
            case Some(l) =>
              linear(a) match {
                case Some(r) => Some(l + r)
                case None    => None
              }
            case None => None
          }
        }
      case Sub(args) =>
        args.toList match {
          case Nil          => None
          case one :: Nil   => linear(one) // rendered as `one`, see above
          case head :: rest =>
            rest.foldLeft(linear(head)) { (acc, a) =>
              acc match {
                case Some(l) =>
                  linear(a) match {
                    case Some(r) => Some(l - r)
                    case None    => None
                  }
                case None => None
              }
            }
        }
      case Mul(args) if args.nonEmpty =>
        val lins = args.map(linear)
        if (lins.exists(_.isEmpty)) None
        else {
          val (consts, terms) = lins.flatten.partition(_.coeffs.isEmpty)
          val k = consts.foldLeft(BigInt(1))(_ * _.const)
          terms.toList match {
            case Nil         => Some(Lin.constant(k))
            case term :: Nil => Some(term * k)
            case _           => None
          }
        }
      case Ite(cond, ifTrue, ifFalse) =>
        constantBool(cond) match {
          case Some(c) => linear(if (c) ifTrue else ifFalse)
          case None    => None
        }
      case _ => None
    }

  /** The value of a boolean with no variables, if it can be computed.
    *
    * Laws: if `constantBool(p) == Some(v)` then `p` is `v` at every `env`.
    */
  def constantBool(p: BoolExpr): Option[Boolean] = {
    def constantInt(i: IntExpr): Option[BigInt] =
      linear(i) match {
        case Some(l) if l.coeffs.isEmpty => Some(l.const)
        case _                           => None
      }
    def cmp(l: IntExpr, r: IntExpr)(fn: (BigInt, BigInt) => Boolean) =
      (constantInt(l), constantInt(r)) match {
        case (Some(a), Some(b)) => Some(fn(a, b))
        case _                  => None
      }

    p match {
      case BoolConst(v)    => Some(v)
      case Not(in)         =>
        constantBool(in) match {
          case Some(v) => Some(!v)
          case None    => None
        }
      case EqInt(l, r)     => cmp(l, r)(_ == _)
      case Lt(l, r)        => cmp(l, r)(_ < _)
      case Lte(l, r)       => cmp(l, r)(_ <= _)
      case Gt(l, r)        => cmp(l, r)(_ > _)
      case Gte(l, r)       => cmp(l, r)(_ >= _)
      case And(args)       =>
        val vs = args.map(constantBool)
        if (vs.contains(Some(false))) Some(false)
        else if (vs.forall(_ == Some(true))) Some(true)
        else None
      case Or(args) =>
        val vs = args.map(constantBool)
        if (vs.contains(Some(true))) Some(true)
        else if (vs.forall(_ == Some(false))) Some(false)
        else None
      case _ => None
    }
  }

  /** The conjuncts of a fact, pushing negation through and/or.
    *
    * Laws: at every `env`, `fact` holds exactly when every atom in
    * `atoms(fact)` holds.
    */
  def atoms(fact: BoolExpr): Vector[BoolExpr] =
    fact match {
      case And(args)      => args.flatMap(atoms)
      case Not(Or(args))  => args.flatMap(a => atoms(Not(a)))
      case Not(Not(in))   => atoms(in)
      case other          => Vector(other)
    }

  /** A comparison of a linear form with 0. */
  enum Rel derives CanEqual {
    case Le, Ge, Eq, Ne

    /** Laws: `Le.holds(x) == (x <= 0)`, and so on. */
    def holds(x: BigInt): Boolean =
      this match {
        case Le => x <= 0
        case Ge => x >= 0
        case Eq => x == 0
        case Ne => x != 0
      }
  }

  /** Reads `lin rel 0` from an atom, if it compares linear integer terms.
    *
    * Laws: if `linearAtom(atom) == Some((lin, rel))` then at every `env`,
    * `atom` holds exactly when `rel.holds(lin.eval(env))`.
    */
  def linearAtom(atom: BoolExpr): Option[(Lin, Rel)] = {
    def diff(l: IntExpr, r: IntExpr, adjust: BigInt, rel: Rel): Option[(Lin, Rel)] =
      (linear(l), linear(r)) match {
        case (Some(ll), Some(rr)) => Some(((ll - rr).plus(adjust), rel))
        case _                    => None
      }
    val zero = BigInt(0)

    atom match {
      // l < r iff l - r + 1 <= 0, and l > r iff l - r - 1 >= 0 (integers)
      case Lt(l, r)       => diff(l, r, BigInt(1), Rel.Le)
      case Lte(l, r)      => diff(l, r, zero, Rel.Le)
      case Gt(l, r)       => diff(l, r, BigInt(-1), Rel.Ge)
      case Gte(l, r)      => diff(l, r, zero, Rel.Ge)
      case EqInt(l, r)    => diff(l, r, zero, Rel.Eq)
      case Not(Lt(l, r))  => linearAtom(Gte(l, r))
      case Not(Lte(l, r)) => linearAtom(Gt(l, r))
      case Not(Gt(l, r))  => linearAtom(Lte(l, r))
      case Not(Gte(l, r)) => linearAtom(Lt(l, r))
      case Not(EqInt(l, r)) => diff(l, r, zero, Rel.Ne)
      case _              => None
    }
  }

  /** Bounds from the facts; None if the facts can't all hold.
    *
    * Laws: if every fact holds at `env` then `box(facts) == Some(b)` with
    * `b.contains(env)`. So `box(facts) == None` means no `env` satisfies the
    * facts.
    */
  def box(facts: Iterable[BoolExpr]): Option[Box] = {
    val start: Option[Box] = Some(Box(Map.empty, Map.empty))
    val withAtoms = facts.iterator.flatMap(atoms).foldLeft(start) {
      case (None, _)       => None
      case (Some(b), atom) =>
        if (atom == BoolConst.False) None
        else
          linearAtom(atom) match {
            case Some(fact) => addFact(b, fact)
            case None       => Some(b)
          }
    }
    withAtoms match {
      case Some(b) => tighten(b)
      case None    => None
    }
  }

  /** Narrows b with the fact `lin rel 0` when it involves one variable.
    *
    * Laws: if `b.contains(env)` and `rel.holds(lin.eval(env))` then
    * `addFact(b, (lin, rel)) == Some(b1)` with `b1.contains(env)`.
    */
  def addFact(b: Box, fact: (Lin, Rel)): Option[Box] = {
    val (lin, rel) = fact
    lin.coeffs.toList match {
      case Nil =>
        // no variables: the fact is a constant comparison
        if (rel.holds(lin.const)) Some(b) else None
      case (v, a) :: Nil =>
        // a * v + c rel 0
        val c = lin.const
        def bound(i: Interval): Option[Box] = {
          val i1 = b.interval(v).meet(i)
          if (i1.isEmpty) None
          else Some(b.copy(intervals = b.intervals.updated(v, i1)))
        }
        // a * v <= -c, or >= -c
        def le: Interval =
          if (a > 0) Interval(None, Some(floorDiv(-c, a)))
          else Interval(Some(ceilDiv(c, -a)), None)
        def ge: Interval =
          if (a > 0) Interval(Some(ceilDiv(-c, a)), None)
          else Interval(None, Some(floorDiv(c, -a)))
        rel match {
          case Rel.Le => bound(le)
          case Rel.Ge => bound(ge)
          case Rel.Eq =>
            if ((-c) % a == 0) bound(Interval.point(-c / a)) else None
          case Rel.Ne =>
            if ((-c) % a == 0) {
              val x = -c / a
              Some(
                b.copy(excluded =
                  b.excluded.updated(v, b.excluded.getOrElse(v, Set.empty) + x)
                )
              )
            } else Some(b)
        }
      case _ =>
        // relates two or more variables: ignored
        Some(b)
    }
  }

  /** Moves each interval's bounds past excluded values: `n >= 0` and `n != 0`
    * give `n >= 1`.
    *
    * Laws: for every `env`, `b.contains(env)` exactly when `tighten(b) ==
    * Some(b1)` and `b1.contains(env)`.
    */
  def tighten(b: Box): Option[Box] =
    b.excluded.foldLeft(Option(b)) {
      case (None, _)               => None
      case (Some(acc), (v, excl)) =>
        // the excluded set is finite, so these end
        @annotation.tailrec
        def up(x: BigInt): BigInt = if (excl(x)) up(x + 1) else x
        @annotation.tailrec
        def down(x: BigInt): BigInt = if (excl(x)) down(x - 1) else x
        val i = acc.interval(v)
        val i1 = Interval(
          i.lo match {
            case Some(l) => Some(up(l))
            case None    => None
          },
          i.hi match {
            case Some(h) => Some(down(h))
            case None    => None
          }
        )
        if (i1.isEmpty) None
        else Some(acc.copy(intervals = acc.intervals.updated(v, i1)))
    }

  /** An interval containing every value e takes in the box.
    *
    * Laws: if `b.contains(env)` then `interval(e, b).contains(v)` for the value
    * `v` of `e` at `env`, under any interpretation of division by zero.
    */
  def interval(e: IntExpr, b: Box): Interval =
    e match {
      case IntConst(c) => Interval.point(c)
      case Var(name)   => b.interval(name)
      case Add(args) if args.nonEmpty =>
        args.foldLeft(Interval.point(BigInt(0)))(_ + interval(_, b))
      case Sub(args) =>
        args.toList match {
          case Nil          => Interval.all
          case one :: Nil   => interval(one, b)
          case head :: rest =>
            rest.foldLeft(interval(head, b))((acc, a) => acc + (-interval(a, b)))
        }
      case Mul(args) if args.nonEmpty =>
        linear(e) match {
          case Some(lin) => linInterval(lin, b)
          case None      =>
            // products of variables: only when every factor is bounded
            args.map(interval(_, b)).reduce(mulInterval)
        }
      case Div(num, IntConst(k)) if k > 0 =>
        interval(num, b).divFloor(k)
      case Mod(_, IntConst(k)) if k != 0 =>
        Interval(Some(BigInt(0)), Some(k.abs - 1))
      case Ite(cond, ifTrue, ifFalse) =>
        constantBool(cond) match {
          case Some(c) => interval(if (c) ifTrue else ifFalse, b)
          case None    => interval(ifTrue, b).join(interval(ifFalse, b))
        }
      case _ => Interval.all
    }

  /** An interval containing every product of members.
    *
    * Laws: if `x.contains(a)` and `y.contains(c)` then `mulInterval(x,
    * y).contains(a * c)`; empty if either is empty.
    */
  def mulInterval(x: Interval, y: Interval): Interval =
    if (x.isEmpty || y.isEmpty) Interval.empty
    else
      (x, y) match {
        case (Interval(Some(a), Some(b)), Interval(Some(c), Some(d))) =>
          val ps = List(a * c, a * d, b * c, b * d)
          Interval(Some(ps.min), Some(ps.max))
        case _ => Interval.all
      }

  /** The interval of values lin takes in the box's intervals.
    *
    * Laws: if `b.contains(env)` then `linInterval(lin, b).contains(lin.eval(env))`.
    */
  def linInterval(lin: Lin, b: Box): Interval =
    lin.coeffs.foldLeft(Interval.point(lin.const)) { case (acc, (v, c)) =>
      acc + (b.interval(v) * c)
    }

  /** Linear expressions known to be <= e (lowers) and >= e (uppers), for
    * every value of the variables within the box. Empty means none found.
    */
  final case class Bounds(lowers: List[Lin], uppers: List[Lin])

  // Laws: the constant bounds of i, which hold wherever i contains the value.
  private def constBounds(i: Interval): Bounds =
    Bounds(
      i.lo match {
        case Some(l) => Lin.constant(l) :: Nil
        case None    => Nil
      },
      i.hi match {
        case Some(h) => Lin.constant(h) :: Nil
        case None    => Nil
      }
    )

  // Laws: if x <= a for every x in xs and y <= c for every y in ys, then every
  // result is <= a + c (and likewise for >=).
  private def sums(xs: List[Lin], ys: List[Lin]): List[Lin] =
    xs.flatMap(x => ys.map(x + _)).distinct.take(MaxCandidates)

  /** Linear lower and upper bounds of e within the box.
    *
    * Laws: if `b.contains(env)` and `v` is the value of `e` at `env` (under any
    * interpretation of division by zero), then `l.eval(env) <= v` for every
    * `l` in `bounds(e, b).lowers`, and `v <= u.eval(env)` for every `u` in
    * `bounds(e, b).uppers`.
    */
  def bounds(e: IntExpr, b: Box): Bounds =
    linear(e) match {
      case Some(lin) =>
        // exact, plus constant bounds which can be tighter against constants
        val cb = constBounds(linInterval(lin, b))
        Bounds(lin :: cb.lowers, lin :: cb.uppers)
      case None =>
        e match {
          case Add(args) if args.nonEmpty =>
            args.map(bounds(_, b)).foldLeft(Bounds(Lin.zero :: Nil, Lin.zero :: Nil)) {
              (acc, bs) => Bounds(sums(acc.lowers, bs.lowers), sums(acc.uppers, bs.uppers))
            }
          case Sub(args) =>
            args.toList match {
              case Nil          => Bounds(Nil, Nil)
              case one :: Nil   => bounds(one, b)
              case head :: rest =>
                rest.map(bounds(_, b)).foldLeft(bounds(head, b)) { (acc, bs) =>
                  Bounds(
                    sums(acc.lowers, bs.uppers.map(-_)),
                    sums(acc.uppers, bs.lowers.map(-_))
                  )
                }
            }
          case Mul(args) if args.nonEmpty =>
            // a constant times one nonlinear term
            def constantOf(i: IntExpr): Option[BigInt] =
              linear(i) match {
                case Some(l) if l.coeffs.isEmpty => Some(l.const)
                case _                           => None
              }
            val (consts, terms) = args.partition(constantOf(_).nonEmpty)
            val k = consts.foldLeft(BigInt(1))((acc, c) => acc * constantOf(c).getOrElse(BigInt(1)))
            terms.toList match {
              case term :: Nil =>
                val bs = bounds(term, b)
                if (k >= 0) Bounds(bs.lowers.map(_ * k), bs.uppers.map(_ * k))
                else Bounds(bs.uppers.map(_ * k), bs.lowers.map(_ * k))
              case _ =>
                constBounds(interval(e, b))
            }
          case Div(num, IntConst(k)) if k > 0 =>
            val numLo = interval(num, b).lo
            val cb = constBounds(interval(e, b))
            val numUppers = bounds(num, b).uppers
            // For num >= 0, num div k <= num; and for num >= 1 and k >= 2,
            // num div k <= num / 2 <= num - 1. These hold pointwise, so they
            // hold for any upper bound of num too.
            val linUppers =
              if (k == 1) numUppers
              else
                numLo match {
                  case Some(l) if l >= 1 => numUppers.map(_.plus(BigInt(-1)))
                  case Some(l) if l >= 0 => numUppers
                  case _                 => Nil
                }
            val linLowers =
              if (k == 1) bounds(num, b).lowers else Nil
            Bounds(
              (linLowers ::: cb.lowers).distinct.take(MaxCandidates),
              (linUppers ::: cb.uppers).distinct.take(MaxCandidates)
            )
          case Ite(cond, ifTrue, ifFalse) =>
            constantBool(cond) match {
              case Some(c) => bounds(if (c) ifTrue else ifFalse, b)
              case None    => constBounds(interval(e, b))
            }
          case _ =>
            constBounds(interval(e, b))
        }
    }

  /** The largest value of lin within the box's intervals, if it is bounded.
    *
    * Laws: if `maxOver(lin, b) == Some(m)` then `lin.eval(env) <= m` whenever
    * `b.contains(env)`; and when every interval in `b` is nonempty, some
    * `env` with every variable in its interval has `lin.eval(env) == m`.
    */
  def maxOver(lin: Lin, b: Box): Option[BigInt] =
    lin.coeffs.foldLeft(Option(lin.const)) { case (acc, (v, c)) =>
      acc match {
        case Some(a) =>
          val i = b.interval(v)
          val end: Option[BigInt] = if (c > 0) i.hi else i.lo
          end match {
            case Some(x) => Some(a + c * x)
            case None    => None
          }
        case None => None
      }
    }

  /** Laws: if `diffAtMost(a, c, limit, b)` then `va - vc <= limit` for the
    * values `va`, `vc` of `a` and `c` at every `env` in `b`.
    */
  def diffAtMost(a: IntExpr, c: IntExpr, limit: BigInt, b: Box): Boolean = {
    val ua = bounds(a, b).uppers
    val lc = bounds(c, b).lowers
    ua.exists { u =>
      lc.exists { l =>
        maxOver(u - l, b) match {
          case Some(m) => m <= limit
          case None    => false
        }
      }
    }
  }

  /** Laws: if `proves(goal, b)` then `goal` holds at every `env` in `b`. */
  def proves(goal: BoolExpr, b: Box): Boolean =
    goal match {
      case BoolConst(v)    => v
      case Lt(l, r)        => diffAtMost(l, r, BigInt(-1), b)
      case Lte(l, r)       => diffAtMost(l, r, BigInt(0), b)
      case Gt(l, r)        => diffAtMost(r, l, BigInt(-1), b)
      case Gte(l, r)       => diffAtMost(r, l, BigInt(0), b)
      case EqInt(l, r)     => proves(Lte(l, r), b) && proves(Gte(l, r), b)
      case And(args)       => args.forall(proves(_, b))
      case Or(args)        => args.exists(proves(_, b))
      case Not(in)         =>
        in match {
          case BoolConst(v)  => !v
          case Lt(l, r)      => proves(Gte(l, r), b)
          case Lte(l, r)     => proves(Gt(l, r), b)
          case Gt(l, r)      => proves(Lte(l, r), b)
          case Gte(l, r)     => proves(Lt(l, r), b)
          case EqInt(l, r)   => proves(Lt(l, r), b) || proves(Gt(l, r), b)
          case Not(in1)      => proves(in1, b)
          case And(args)     => args.exists(a => proves(Not(a), b))
          case Or(args)      => args.forall(a => proves(Not(a), b))
          case _             => false
        }
      case _ => false
    }

  /** True if the facts imply the goal. False means not proven here.
    *
    * Laws: if `implies(goal, facts)` then `goal` holds at every `env` where
    * all the facts hold. In particular z3 finds `facts and not goal`
    * unsatisfiable.
    */
  def implies(goal: BoolExpr, facts: Iterable[BoolExpr]): Boolean =
    box(facts) match {
      case None    => true // the facts can't all hold, so anything follows
      case Some(b) => proves(goal, b)
    }
}
