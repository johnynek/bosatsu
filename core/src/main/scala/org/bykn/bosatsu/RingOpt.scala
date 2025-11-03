package org.bykn.bosatsu

import scala.math.Numeric

object RingOpt {

  sealed trait Expr[+A] { expr =>
    def map[B](fn: A => B): Expr[B] =
      expr match {
        case Zero       => Zero
        case One        => One
        case Integer(n) => Integer(n)
        case Symbol(a)  => Symbol(fn(a))
        case Add(x, y)  => Add(x.map(fn), y.map(fn))
        case Mult(x, y) => Mult(x.map(fn), y.map(fn))
        case Neg(x)     => Neg(x.map(fn))
      }

    def isZero: Boolean =
      expr match {
        case Zero            => true
        case One | Symbol(_) => false
        case Integer(n)      => n == 0
        case Add(x, y)       => x.isZero && y.isZero
        case Mult(x, y)      => x.isZero || y.isZero
        case Neg(n)          => n.isZero
      }

    def isOne: Boolean =
      expr match {
        case One                                   => true
        case Neg(Integer(n1)) if n1 == -1          => true
        case Zero | Symbol(_) | Add(_, _) | Neg(_) => false
        case Integer(n)                            => n == 1
        case Mult(x, y)                            => x.isOne && y.isOne
      }

    def nonNeg: Boolean =
      expr match {
        case Neg(_) => false
        case _      => true
      }

    def noTopLevelAdd: Boolean =
      expr match {
        case Add(_, _) => false
        case _         => true
      }

    def normalizeNeg: Expr[A] = expr match {
      case Zero                => Zero
      case One                 => Integer(-1)
      case Integer(n)          => canonInt[A](-n)
      case Neg(y)              => y
      case Add(Neg(x), Neg(y)) => Add(x, y)
      case Add(x, Neg(y))      => Add(x.normalizeNeg, y)
      case Add(Neg(x), y)      => Add(x, y.normalizeNeg)
      case Mult(Neg(x), y)     => Mult(x, y)
      case Mult(x, Neg(y))     => Mult(x, y)
      case other               => Neg(other)
    }

  }

  object Expr {
    // Efficiently embed a BigInt into any Numeric[A] by double-and-add of `one`
    private def fromBigInt[A](n: BigInt)(implicit num: Numeric[A]): A = {
      import num._
      if (n == 0) zero
      else {
        val abs = n.abs
        @annotation.tailrec
        def loop(k: BigInt, acc: A, addend: A): A =
          if (k == 0) acc
          else {
            val acc1 = if ((k & 1) == 1) plus(acc, addend) else acc
            loop(k >> 1, acc1, plus(addend, addend))
          }
        val base = loop(abs, zero, one)
        if (n.signum < 0) negate(base) else base
      }
    }

    def toValue[A](expr: Expr[A])(implicit num: Numeric[A]): A =
      expr match {
        case Zero       => num.zero
        case One        => num.one
        case Integer(n) => fromBigInt[A](n)
        case Symbol(a)  => a
        case Add(x, y)  => num.plus(toValue(x), toValue(y))
        case Mult(x, y) => num.times(toValue(x), toValue(y))
        case Neg(x)     => num.negate(toValue(x))
      }
  }

  case object Zero extends Expr[Nothing]
  case object One extends Expr[Nothing]
  final case class Integer(toBigInt: BigInt) extends Expr[Nothing]
  final case class Symbol[A](item: A) extends Expr[A]
  final case class Add[A](left: Expr[A], right: Expr[A]) extends Expr[A]
  final case class Mult[A](left: Expr[A], right: Expr[A]) extends Expr[A]
  final case class Neg[A](arg: Expr[A]) extends Expr[A]

  // Heuristic cost model (make Mult much costlier than Add)
  final case class Weights(mult: Int, add: Int, neg: Int) {
    require(add > 0, s"add = $add must be > 0")
    require(mult > 0, s"mult = $mult must be > 0")
    require(neg > 0, s"neg = $neg must be > 0")

    def cost[A](e: Expr[A]): Int = e match {
      case Zero | One | Integer(_) | Symbol(_) => 0
      case Neg(x)                              => neg + cost(x)
      case Add(a, b)                           => add + cost(a) + cost(b)
      case Mult(a, b)                          => mult + cost(a) + cost(b)
    }
  }

  object Weights {
    val default: Weights = Weights(mult = 5, add = 1, neg = 1)
  }

  // === Public API ===
  def normalize[A](e: Expr[A], W: Weights = Weights.default): Expr[A] = {
    val ne = norm(e, W)
    val cost = W.cost(ne)
    if (cost < W.cost(e)) {
      // maybe we can refine
      // this is obviously suboptimal... it would be better to not have to check this
      // but this requires that the value we return has gone through one cycle and gotten
      // the same cost at the end.
      normalize(ne, W)
    } else e // we couldn't improve cost, just return the input
  }

  // Small helper: canonical integer literal
  private def canonInt[A](n: BigInt): Expr[A] =
    if (n == 0) Zero
    else if (n == 1) One
    else Integer(n)

  // Combine a BigInt coeff with a product base into a product-like Expr
  private def buildScaled[A](
      coeff: BigInt,
      base: Expr[A],
      w: Weights
  ): Expr[A] =
    if (coeff == 0) Zero
    else if (base.isOne) canonInt[A](coeff)
    else
      coeff.signum match {
        case 1 =>
          if (coeff == 1) base
          else {
            val bcost = w.cost(base)
            val multCost = w.mult + bcost
            val addCost = coeff * bcost + (coeff - 1) * w.add
            if (multCost <= addCost) multAll(Integer(coeff) :: base :: Nil)
            else {
              // add instead
              addAll(List.fill(coeff.toInt)(base))
            }
          }
        case -1 =>
          if (coeff == -1) Neg(base)
          else {
            val bcost = w.cost(base)
            val negCoeff = -coeff
            val multCost = w.mult + bcost
            val addCost = negCoeff * bcost + (negCoeff - 1) * w.add + w.neg
            if (multCost <= addCost) multAll(Integer(coeff) :: base :: Nil)
            else {
              // add instead
              Neg(addAll(List.fill(negCoeff.toInt)(base)))
            }
          }
        case _ => Zero
      }

  // === Core normalization ===

  private def norm[A](e: Expr[A], W: Weights): Expr[A] = e match {
    case Zero | One | Symbol(_) => e

    case Integer(n) => canonInt[A](n)

    case Neg(x) =>
      // Normalize and push negation into integers when possible
      norm(x, W).normalizeNeg

    case Add(a, b) =>
      normAdd(flattenAdd(a :: b :: Nil), W)

    case Mult(a, b) =>
      normMult(flattenMult(a :: b :: Nil), W)
  }

  // Flatteners that also drop obvious identities but not reordering yet
  // invariant: no Zero or Add items returned in the list
  private def flattenAdd[A](e: List[Expr[A]]): List[Expr[A]] = {
    @annotation.tailrec
    def loop(in: List[Expr[A]], result: List[Expr[A]]): List[Expr[A]] =
      in match {
        case Add(x, y) :: tail => loop(x :: y :: tail, result)
        case other :: tail     =>
          val r2 = if (other.isZero) result else (other :: result)
          loop(tail, r2)
        case Nil => result
      }

    loop(e, Nil)
  }
  // invariant: no One or Mult items returned in the list
  private def flattenMult[A](e: List[Expr[A]]): List[Expr[A]] = {
    @annotation.tailrec
    def loop(in: List[Expr[A]], result: List[Expr[A]]): List[Expr[A]] =
      in match {
        case Mult(x, y) :: tail => loop(x :: y :: tail, result)
        case other :: tail      =>
          val r2 = if (other.isOne) result else (other :: result)
          loop(tail, r2)
        case Nil => result
      }

    loop(e, Nil)
  }

  // Turn a product-like term into (integer coefficient, product base-without-integers)
  // this is only used when Adds have been flattened out, so there should be no Add nodes
  private def coeffAndBase[A](t0: Expr[A]): (BigInt, Expr[A]) = {
    // Pull a single outer neg or negative integer sign
    val (negated, t) = stripNeg(t0)
    val sign = if (negated) BigInt(-1) else BigInt(1)

    t match {
      case One        => (sign, One)
      case Integer(n) => (sign * n, One)
      case Mult(a, b) =>
        val fs = flattenMult(a :: b :: Nil)
        var c = sign
        val non = scala.collection.mutable.ListBuffer.empty[Expr[A]]
        fs.foreach {
          case One        => ()
          case Integer(n) => c = c * n
          case Neg(x) => c = -c; non += x // should be rare post-norm, but safe
          case x      => non += x
        }
        if (non.isEmpty) (c, One)
        else {
          val base = multAll(non.toList)
          (c, base)
        }
      case x => (sign, x)
    }
  }

  // Normalize addition list: combine integer coefficients per base, sort, optional factoring
  private def normAdd[A](terms0: List[Expr[A]], W: Weights): Expr[A] =
    if (terms0.isEmpty) Zero
    else {
      // Fully normalize each term and drop zeros
      val terms =
        for {
          term0 <- terms0
          norm0 = norm(term0, W)
          term1 <- flattenAdd(norm0 :: Nil)
          if !term1.isZero
          (c, base) = coeffAndBase(term1)
          flatBase <- flattenAdd(base :: Nil)
          // now none of flatBase are Add(_, _)
        } yield (c, flatBase)

      // Sum integer coefficients for identical bases
      val tallied: Map[Expr[A], BigInt] =
        terms.foldLeft(Map.empty[Expr[A], BigInt]) { case (acc, (c, base)) =>
          val c2 = acc.getOrElse(base, BigInt(0)) + c
          if (c2 == 0) acc - base else acc.updated(base, c2)
        }

      // Rebuild terms from (base -> coeff)
      val collapsed: List[Expr[A]] =
        tallied.iterator.map { case (base, c) =>
          buildScaled(c, base, W)
        }.toList

      if (collapsed.isEmpty) Zero
      else {
        val canon = sortExpr(collapsed)
        val summed = addAll(canon)
        tryFactor(canon, W) match {
          case None     => summed
          case Some(fs) => chooseBest(summed, fs, W)
        }
      }
    }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A](factors0: List[Expr[A]], W: Weights): Expr[A] =
    if (factors0.isEmpty) One
    else if (factors0.exists(_ == Zero)) Zero
    else {
      val normed = factors0.flatMap(e => flattenMult(norm(e, W) :: Nil))
      // Accumulate integer coefficient and collect non-integer factors
      var coeff = BigInt(1)
      val bodies = scala.collection.mutable.ListBuffer.empty[Expr[A]]
      normed.foreach {
        case Zero       => coeff = 0
        case One        => ()
        case Integer(n) => coeff *= n
        case Neg(x)     => coeff = -coeff; bodies += x
        case x          => bodies += x
      }
      if (coeff == 0) Zero
      else {
        val base = multAll(bodies.toList)
        buildScaled(coeff, base, W)
      }
    }

  private def multAll[A](items: List[Expr[A]]): Expr[A] =
    sortExpr(flattenMult(items)).foldLeft[Expr[A]](One) { case (x, y) =>
      if (x.isOne) y else if (y.isOne) x else Mult(x, y)
    }

  private def addAll[A](items: List[Expr[A]]): Expr[A] =
    sortExpr(flattenAdd(items)).foldLeft[Expr[A]](Zero) { case (x, y) =>
      if (x.isZero) y else if (y.isZero) x else Add(x, y)
    }

  // === Local factoring across sums ===
  // If all terms are product-like (Symbol/Neg/Mult/One/Integer), factor common non-integer
  // products AND (new) the gcd of integer coefficients.
  private def tryFactor[A](terms: List[Expr[A]], W: Weights): Option[Expr[A]] =
    if ((terms.lengthCompare(2) < 0) || !terms.forall(_.noTopLevelAdd)) None
    else {
      // we have at least 2 items and none of them have top level adds
      // Represent each term as (integer coeff, multiset of non-integer factors)
      val asProducts: List[(BigInt, List[Expr[A]])] = terms.map { t =>
        val (c, base) = coeffAndBase(t)
        val fs = base match {
          case One        => Nil
          case Mult(a, b) =>
            flattenMult(a :: b :: Nil) // already sorted by coeffAndBase
          case x => x :: Nil
        }
        (c, fs)
      }

      // Common non-integer factors
      val commonNonInt: List[Expr[A]] =
        intersectMultisets(asProducts.map(_._2))

      // GCD of coefficients (positive)
      val gcdCoeff: BigInt =
        asProducts.map { case (c, _) => c.abs }.reduce(_.gcd(_))

      val hasCommonFactor =
        commonNonInt.nonEmpty || gcdCoeff > 1

      if (!hasCommonFactor) None
      else {
        // Remove common factors and divide coefficients by gcd
        val residuals: List[Expr[A]] = asProducts.map { case (c, fs) =>
          val rest = subtractMultiset(fs, commonNonInt)
          val base = multAll(rest)
          val newCoeff =
            if (gcdCoeff > 1) c / gcdCoeff else c
          buildScaled(newCoeff, base, W)
        }

        val inner = addAll(residuals)
        val outer =
          if (gcdCoeff > 1) multAll(Integer(gcdCoeff) :: inner :: commonNonInt)
          else multAll(inner :: commonNonInt)

        // Only accept factoring if it reduces cost (or ties and is canonical-smaller)
        Some(chooseBest(addAll(terms), outer, W))
      }
    }

  private def intersectMultisets[A](lists: List[List[A]]): List[A] = {
    def toM(fs: List[A]): Map[A, Int] =
      fs.groupBy(identity).view.mapValues(_.size).toMap

    lists
      .map(toM)
      .reduce { (m1, m2) =>
        (for {
          k <- m1.keySet intersect m2.keySet
        } yield k -> math.min(m1(k), m2(k))).toMap
      }
      .toList
      .flatMap { case (k, n) => List.fill(n)(k) }
  }

  private def subtractMultiset[A](
      fs: List[A],
      sub: List[A]
  ): List[A] = {
    val need = sub.groupBy(identity).view.mapValues(_.size).toMap
    val buf = scala.collection.mutable.Map.from(need)
    fs.flatMap { f =>
      buf.get(f) match {
        case Some(n) if n > 0 => buf.update(f, n - 1); Nil
        case _                => f :: Nil
      }
    }
  }

  // === Negation helpers ===

  // Pull a single outer negation sign OR a negative Integer
  private def stripNeg[A](e: Expr[A]): (Boolean, Expr[A]) = e match {
    case Neg(Neg(y))    => stripNeg(y)
    case Neg(x)         => (true, x)
    case i @ Integer(n) =>
      if (n.signum < 0) (true, Integer(-n))
      else (false, i)

    case add @ Add(x, y) =>
      // if we can remove negative from all, do it:
      val flat = flattenAdd(x :: y :: Nil).map(stripNeg)
      if (flat.forall(_._1)) {
        val term = addAll(flat.map(_._2))
        (true, term)
      } else (false, add)
    case mult @ Mult(x, y) =>
      // if we can pull a negative out, do it
      val flat = flattenMult(x :: y :: Nil).map(stripNeg)
      if (flat.forall { case (neg, _) => !neg }) {
        // all are positive
        (false, mult)
      } else {
        // mix of positive and negative, we can pull out the sign
        val parity = flat.count { case (neg, _) => neg }
        val justItems = multAll(flat.map(_._2))
        ((parity & 1) == 1, justItems)
      }
    case pos @ (Zero | One | Symbol(_)) => (false, pos)
  }

  // === Cost & tie-breaking ===

  private def chooseBest[A](
      left: Expr[A],
      right: Expr[A],
      W: Weights
  ): Expr[A] = {
    val lcost = W.cost(left)
    val rcost = W.cost(right)
    if (lcost < rcost) left
    else if (rcost < lcost) right
    else {
      // cost is the same, break tie with the key
      if (key(left) < key(right)) left
      else right
    }
  }

  // Canonical structural key to enforce determinism (commutativity handled by sorting)
  private def key[A](e: Expr[A]): String = e match {
    case Zero       => "0"
    case One        => "1"
    case Integer(n) => s"I($n)"
    case Symbol(n)  => s"S($n)"
    case Neg(x)     => s"N(${key(x)})"
    case Add(a, b)  => s"A(${key(a)},${key(b)})"
    case Mult(a, b) => s"M(${key(a)},${key(b)})"
  }

  // Sort a list of Expr[A]s by key (used for commutativity)
  private def sortExpr[A](es: List[Expr[A]]): List[Expr[A]] =
    es.sortBy(key)
}
