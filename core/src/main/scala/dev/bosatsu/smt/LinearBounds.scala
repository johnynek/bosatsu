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
  * `implies` returning false means "not proven here": callers fall back to z3.
  */
private[smt] object LinearBounds {

  /** const + sum(coeff * variable) */
  final case class Lin(const: BigInt, coeffs: Map[String, BigInt])
      derives CanEqual {
    def +(that: Lin): Lin =
      Lin(
        const + that.const,
        that.coeffs.foldLeft(coeffs) { case (acc, (v, c)) =>
          val c1 = acc.getOrElse(v, BigInt(0)) + c
          if (c1 == 0) acc - v else acc.updated(v, c1)
        }
      )

    def *(k: BigInt): Lin =
      if (k == 0) Lin.constant(BigInt(0))
      else Lin(const * k, coeffs.map { case (v, c) => (v, c * k) })

    def unary_- : Lin = this * BigInt(-1)

    def -(that: Lin): Lin = this + (-that)

    def plus(k: BigInt): Lin = copy(const = const + k)
  }

  object Lin {
    def constant(c: BigInt): Lin = Lin(c, Map.empty)
    def variable(v: String): Lin = Lin(BigInt(0), Map(v -> BigInt(1)))
  }

  /** An integer interval; None means unbounded on that side. */
  final case class Interval(lo: Option[BigInt], hi: Option[BigInt])
      derives CanEqual {
    def +(that: Interval): Interval =
      Interval(addOpt(lo, that.lo), addOpt(hi, that.hi))

    def unary_- : Interval = Interval(hi.map(-_), lo.map(-_))

    def *(k: BigInt): Interval =
      if (k == 0) Interval.point(BigInt(0))
      else if (k > 0) Interval(lo.map(_ * k), hi.map(_ * k))
      else Interval(hi.map(_ * k), lo.map(_ * k))

    def join(that: Interval): Interval =
      Interval(
        for { a <- lo; b <- that.lo } yield a.min(b),
        for { a <- hi; b <- that.hi } yield a.max(b)
      )

    def meet(that: Interval): Interval =
      Interval(maxOpt(lo, that.lo), minOpt(hi, that.hi))

    def isEmpty: Boolean =
      (lo, hi) match {
        case (Some(l), Some(h)) => l > h
        case _                  => false
      }
  }

  object Interval {
    val all: Interval = Interval(None, None)
    def point(c: BigInt): Interval = Interval(Some(c), Some(c))
  }

  private def addOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    for { x <- a; y <- b } yield x + y

  private def maxOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(x.max(y))
      case (None, y)          => y
      case (x, None)          => x
    }

  private def minOpt(a: Option[BigInt], b: Option[BigInt]): Option[BigInt] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(x.min(y))
      case (None, y)          => y
      case (x, None)          => x
    }

  /** floor(a / k) for k > 0; BigInt's / rounds toward zero */
  private def floorDiv(a: BigInt, k: BigInt): BigInt = {
    val q = a / k
    if ((a % k != 0) && (a < 0)) q - 1 else q
  }

  /** ceil(a / k) for k > 0 */
  private def ceilDiv(a: BigInt, k: BigInt): BigInt = -floorDiv(-a, k)

  // Keep the number of candidate bounds small; sums take all pairs.
  private val MaxCandidates = 4

  /** Variable intervals from the facts, and values each variable can't be.
    * Facts that don't bound a single variable are ignored.
    */
  final case class Box(
      intervals: Map[String, Interval],
      excluded: Map[String, Set[BigInt]]
  ) {
    def interval(v: String): Interval = intervals.getOrElse(v, Interval.all)
  }

  /** The exact linear form of e, if it has one. */
  def linear(e: IntExpr): Option[Lin] =
    e match {
      case IntConst(c) => Some(Lin.constant(c))
      case Var(name)   => Some(Lin.variable(name))
      case Add(args) if args.nonEmpty =>
        args.foldLeft(Option(Lin.constant(BigInt(0)))) { (acc, a) =>
          for { l <- acc; r <- linear(a) } yield l + r
        }
      case Sub(args) =>
        args.toList match {
          case Nil          => None
          case one :: Nil   => linear(one) // rendered as `one`, see above
          case head :: rest =>
            rest.foldLeft(linear(head)) { (acc, a) =>
              for { l <- acc; r <- linear(a) } yield l - r
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
        constantBool(cond).flatMap { c =>
          linear(if (c) ifTrue else ifFalse)
        }
      case _ => None
    }

  /** The value of a boolean with no variables, if it can be computed. */
  private def constantBool(b: BoolExpr): Option[Boolean] = {
    def cmp(l: IntExpr, r: IntExpr)(fn: (BigInt, BigInt) => Boolean) =
      for {
        ll <- linear(l).filter(_.coeffs.isEmpty)
        rr <- linear(r).filter(_.coeffs.isEmpty)
      } yield fn(ll.const, rr.const)

    b match {
      case BoolConst(v)    => Some(v)
      case Not(in)         => constantBool(in).map(!_)
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

  /** The conjuncts of a fact, pushing negation through and/or. */
  private def atoms(fact: BoolExpr): Vector[BoolExpr] =
    fact match {
      case And(args)      => args.flatMap(atoms)
      case Not(Or(args))  => args.flatMap(a => atoms(Not(a)))
      case Not(Not(in))   => atoms(in)
      case other          => Vector(other)
    }

  private enum Rel derives CanEqual {
    case Le, Ge, Eq, Ne
  }

  // Reads `lin rel 0` from an atom, if it compares linear integer terms.
  private def linearAtom(atom: BoolExpr): Option[(Lin, Rel)] = {
    def diff(l: IntExpr, r: IntExpr): Option[Lin] =
      for { ll <- linear(l); rr <- linear(r) } yield ll - rr

    atom match {
      case Lt(l, r)       => diff(l, r).map(d => (d.plus(BigInt(1)), Rel.Le))
      case Lte(l, r)      => diff(l, r).map(d => (d, Rel.Le))
      case Gt(l, r)       => diff(l, r).map(d => (d.plus(BigInt(-1)), Rel.Ge))
      case Gte(l, r)      => diff(l, r).map(d => (d, Rel.Ge))
      case EqInt(l, r)    => diff(l, r).map(d => (d, Rel.Eq))
      case Not(Lt(l, r))  => linearAtom(Gte(l, r))
      case Not(Lte(l, r)) => linearAtom(Gt(l, r))
      case Not(Gt(l, r))  => linearAtom(Lte(l, r))
      case Not(Gte(l, r)) => linearAtom(Lt(l, r))
      case Not(EqInt(l, r)) => diff(l, r).map(d => (d, Rel.Ne))
      case _              => None
    }
  }

  /** Bounds from the facts; None if the facts can't all hold. */
  def box(facts: Iterable[BoolExpr]): Option[Box] = {
    val start: Option[Box] = Some(Box(Map.empty, Map.empty))
    val withAtoms = facts.iterator.flatMap(atoms).foldLeft(start) {
      case (None, _)       => None
      case (Some(b), atom) =>
        if (atom == BoolConst.False) None
        else linearAtom(atom).fold(Option(b))(addFact(b, _))
    }
    withAtoms.flatMap(tighten)
  }

  private def addFact(b: Box, fact: (Lin, Rel)): Option[Box] = {
    val (lin, rel) = fact
    lin.coeffs.toList match {
      case Nil =>
        // no variables: the fact is a constant comparison
        val c = lin.const
        val holds = rel match {
          case Rel.Le => c <= 0
          case Rel.Ge => c >= 0
          case Rel.Eq => c == 0
          case Rel.Ne => c != 0
        }
        if (holds) Some(b) else None
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

  // Move bounds past excluded values: n >= 0 and n != 0 give n >= 1.
  private def tighten(b: Box): Option[Box] =
    b.excluded.foldLeft(Option(b)) {
      case (None, _)               => None
      case (Some(acc), (v, excl)) =>
        val i = acc.interval(v)
        // the excluded set is finite, so these loops end
        var lo = i.lo
        while (lo.exists(excl)) lo = lo.map(_ + 1)
        var hi = i.hi
        while (hi.exists(excl)) hi = hi.map(_ - 1)
        val i1 = Interval(lo, hi)
        if (i1.isEmpty) None
        else Some(acc.copy(intervals = acc.intervals.updated(v, i1)))
    }

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
        val i = interval(num, b)
        Interval(i.lo.map(floorDiv(_, k)), i.hi.map(floorDiv(_, k)))
      case Mod(_, IntConst(k)) if k != 0 =>
        Interval(Some(BigInt(0)), Some(k.abs - 1))
      case Ite(cond, ifTrue, ifFalse) =>
        constantBool(cond) match {
          case Some(c) => interval(if (c) ifTrue else ifFalse, b)
          case None    => interval(ifTrue, b).join(interval(ifFalse, b))
        }
      case _ => Interval.all
    }

  private def mulInterval(x: Interval, y: Interval): Interval =
    (x, y) match {
      case (Interval(Some(a), Some(b)), Interval(Some(c), Some(d))) =>
        val ps = List(a * c, a * d, b * c, b * d)
        Interval(Some(ps.min), Some(ps.max))
      case _ => Interval.all
    }

  private def linInterval(lin: Lin, b: Box): Interval =
    lin.coeffs.foldLeft(Interval.point(lin.const)) { case (acc, (v, c)) =>
      acc + (b.interval(v) * c)
    }

  /** Linear expressions known to be <= e (lowers) and >= e (uppers), for
    * every value of the variables within the box. Empty means none found.
    */
  final case class Bounds(lowers: List[Lin], uppers: List[Lin])

  private def constBounds(i: Interval): Bounds =
    Bounds(
      i.lo.map(Lin.constant).toList,
      i.hi.map(Lin.constant).toList
    )

  private def sums(xs: List[Lin], ys: List[Lin]): List[Lin] =
    (for { x <- xs; y <- ys } yield x + y).distinct.take(MaxCandidates)

  def bounds(e: IntExpr, b: Box): Bounds =
    linear(e) match {
      case Some(lin) =>
        // exact, plus constant bounds which can be tighter against constants
        val cb = constBounds(linInterval(lin, b))
        Bounds(lin :: cb.lowers, lin :: cb.uppers)
      case None =>
        e match {
          case Add(args) if args.nonEmpty =>
            args.map(bounds(_, b)).foldLeft(Bounds(List(Lin.constant(BigInt(0))), List(Lin.constant(BigInt(0))))) {
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
            val (consts, terms) = args.partition(linear(_).exists(_.coeffs.isEmpty))
            val k = consts.foldLeft(BigInt(1))((acc, c) => acc * linear(c).fold(BigInt(1))(_.const))
            terms.toList match {
              case term :: Nil =>
                val bs = bounds(term, b)
                if (k >= 0) Bounds(bs.lowers.map(_ * k), bs.uppers.map(_ * k))
                else Bounds(bs.uppers.map(_ * k), bs.lowers.map(_ * k))
              case _ =>
                constBounds(interval(e, b))
            }
          case Div(num, IntConst(k)) if k > 0 =>
            val numI = interval(num, b)
            val cb = constBounds(interval(e, b))
            val numUppers = bounds(num, b).uppers
            // For num >= 0, num div k <= num; and for num >= 1 and k >= 2,
            // num div k <= num / 2 <= num - 1. These hold pointwise, so they
            // hold for any upper bound of num too.
            val linUppers =
              if (k == 1) numUppers
              else if (numI.lo.exists(_ >= 1)) numUppers.map(_.plus(BigInt(-1)))
              else if (numI.lo.exists(_ >= 0)) numUppers
              else Nil
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

  /** The largest value of lin within the box, if it is bounded. */
  private def maxOver(lin: Lin, b: Box): Option[BigInt] =
    lin.coeffs.foldLeft(Option(lin.const)) { case (acc, (v, c)) =>
      val i = b.interval(v)
      val end = if (c > 0) i.hi else i.lo
      for { a <- acc; x <- end } yield a + c * x
    }

  // Some U >= a and L <= c with max(U - L) <= limit, so a - c <= limit.
  private def diffAtMost(a: IntExpr, c: IntExpr, limit: BigInt, b: Box): Boolean = {
    val ua = bounds(a, b).uppers
    val lc = bounds(c, b).lowers
    ua.exists(u => lc.exists(l => maxOver(u - l, b).exists(_ <= limit)))
  }

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

  /** True if the facts imply the goal. False means not proven here. */
  def implies(goal: BoolExpr, facts: Iterable[BoolExpr]): Boolean =
    box(facts) match {
      case None    => true // the facts can't all hold, so anything follows
      case Some(b) => proves(goal, b)
    }
}
