package org.bykn.bosatsu

import scala.math.Numeric
import cats.syntax.all._

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

    def maybeBigInt(onSym: A => Option[BigInt]): Option[BigInt] =
      expr match {
        case Symbol(s)  => onSym(s)
        case Zero       => Some(BigInt(0))
        case One        => Some(BigInt(1))
        case Integer(n) => Some(n)
        case Add(x, y)  =>
          (x.maybeBigInt(onSym), y.maybeBigInt(onSym)).mapN(_ + _)
        case Mult(x, y) =>
          (x.maybeBigInt(onSym), y.maybeBigInt(onSym)).mapN(_ * _)
        case Neg(n) =>
          n.maybeBigInt(onSym).map(n => -n)
      }

    // Return true if we are definitely (and cheaply) zero
    // doesn't fully evaluate complex expressions or know what Symbols mean
    def isZero: Boolean =
      expr match {
        case Zero            => true
        case One | Symbol(_) => false
        case Integer(n)      => n == 0
        case Add(x, y)       =>
          x.isZero && y.isZero
        case Mult(x, y) => x.isZero || y.isZero
        case Neg(n)     => n.isZero
      }

    // Return true if we are definitely (and cheaply) one
    // doesn't fully evaluate complex expressions or know what Symbols mean
    def isOne: Boolean =
      expr match {
        case One                                   => true
        case Neg(Integer(n1)) if n1 == -1          => true
        case Zero | Symbol(_) | Neg(_) | Add(_, _) => false
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

    // Do neg if it is as cheap or cheaper (only <= number of Neg nodes)
    def cheapNeg: Option[Expr[A]] = expr match {
      case Zero                => Some(Zero)
      case One                 => Some(Integer(-1))
      case Integer(n)          => Some(canonInt[A](-n))
      case Neg(y)              => Some(y)
      case Add(Neg(x), Neg(y)) => Some(Add(x, y))
      // these next two are allowed because normalizeNeg adds at most 1 node
      // so, the total Neg nodes is the same on both sides, satisfying <=
      case Add(x, Neg(y)) => Some(Add(x.normalizeNeg, y))
      case Add(Neg(x), y) => Some(Add(x, y.normalizeNeg))
      case Add(x, y)      =>
        for {
          nx <- x.cheapNeg
          ny <- y.cheapNeg
        } yield Add(nx, ny)
      case Mult(x, y) =>
        x.cheapNeg
          .map(Mult(_, y))
          .orElse {
            y.cheapNeg.map(Mult(x, _))
          }
      case _ => None
    }

    // At most add 1 Neg node
    def normalizeNeg: Expr[A] =
      cheapNeg match {
        case None        => Neg(expr)
        case Some(cheap) => cheap
      }

    // This returns all the trivially signed and scaled numbers in the front
    // and no Neg(_), Integer(_), or One items in the second item
    def unproduct: (BigInt, List[Expr[A]]) = {
      val (c, base) = coeffAndBase
      val fs = base match {
        case Mult(a, b) =>
          Expr.flattenMult(a :: b :: Nil) // already sorted by coeffAndBase
        case x =>
          if (x.isOne) Nil else (x :: Nil)
      }
      (c, fs)
    }

    // Pull a single outer negation sign OR a negative Integer
    // this never returns an outer `Neg(_)` in the expr
    def stripNeg: (Boolean, Expr[A]) = expr match {
      case Neg(Neg(y)) => y.stripNeg
      case Neg(x)      =>
        val (inner, c) = x.stripNeg
        (!inner, c)
      case i @ Integer(n) =>
        if (n.signum < 0) (true, canonInt(-n))
        else (false, i)

      case add @ Add(x, y) =>
        // if we can remove negative from all, do it:
        val flat = Expr.flattenAdd(x :: y :: Nil).map(_.stripNeg)
        if (flat.forall(_._1)) {
          val term = Expr.addAll(flat.map(_._2))
          // make sure we never return Neg
          val (s1, t1) = term.stripNeg
          (s1 ^ true, t1)
        } else (false, add)
      case mult @ Mult(x, y) =>
        // if we can pull a negative out, do it
        val flat = Expr.flattenMult(x :: y :: Nil).map(_.stripNeg)
        if (flat.forall { case (neg, _) => !neg }) {
          // all are positive
          (false, mult)
        } else {
          // mix of positive and negative, we can pull out the sign
          val parity = flat.count { case (neg, _) => neg }
          val justItems = Expr.multAll(flat.map(_._2))
          // make sure we never return Neg
          val (s1, t1) = justItems.stripNeg
          val flip = (parity & 1) == 1
          (s1 ^ flip, t1)
        }
      case pos @ (Zero | One | Symbol(_)) => (false, pos)
    }

    // Turn a product-like term into (integer coefficient, product base-without-integers)
    // this is only used when Adds have been flattened out, so there should be no Add nodes
    def coeffAndBase: (BigInt, Expr[A]) = {
      // Pull a single outer neg or negative integer sign
      val (negated, t) = expr.stripNeg
      val sign = if (negated) BigInt(-1) else BigInt(1)

      t match {
        case One        => (sign, One)
        case Integer(n) => (sign * n, One)
        case Neg(n)     =>
          sys.error(
            s"invariant violation: found Neg($n) from stripNeg on $expr"
          )
        case Mult(a, b) =>
          val fs = Expr.flattenMult(a :: b :: Nil)
          var c = sign
          val non = scala.collection.mutable.ListBuffer.empty[Expr[A]]
          fs.foreach {
            case One        => ()
            case Integer(n) => c = c * n
            case Neg(x)     =>
              c = -c; non += x // should be rare post-norm, but safe
            case x => non += x
          }
          val base = Expr.multAll(non.toList)
          (c, base)
        case x => (sign, x)
      }
    }

    def *[A1 >: A](that: Expr[A1]): Expr[A1] = Mult(expr, that)
    def +[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, that)
    def -[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, Neg(that))
    def unary_- : Expr[A] = Neg(expr)
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

    // Flatteners that also drop obvious identities but not reordering yet
    // invariant: no Zero or Add items returned in the list
    def flattenAdd[A](e: List[Expr[A]]): List[Expr[A]] = {
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

    // return all the positive and negative additive terms from this list
    // invariant: return no top level Add(_, _) or Neg(_)
    def flattenAddSub[A](e: List[Expr[A]]): (List[Expr[A]], List[Expr[A]]) = {
      @annotation.tailrec
      def loop(
          inPos: List[Expr[A]],
          inNeg: List[Expr[A]],
          resultPos: List[Expr[A]],
          resultNeg: List[Expr[A]],
          intRes: BigInt
      ): (List[Expr[A]], List[Expr[A]]) =
        inPos match {
          case Add(x, y) :: tail =>
            loop(x :: y :: tail, inNeg, resultPos, resultNeg, intRes)
          case Neg(x) :: tail =>
            loop(tail, x :: inNeg, resultPos, resultNeg, intRes)
          case Integer(n) :: tail =>
            loop(tail, inNeg, resultPos, resultNeg, intRes + n)
          case One :: tail =>
            loop(tail, inNeg, resultPos, resultNeg, intRes + 1)
          case other :: tail =>
            val r2 = if (other.isZero) resultPos else (other :: resultPos)
            loop(tail, inNeg, r2, resultNeg, intRes)
          case Nil =>
            inNeg match {
              case Nil =>
                if (intRes == 0) (resultPos, resultNeg)
                else (canonInt(intRes) :: resultPos, resultNeg)
              case Add(x, y) :: tail =>
                loop(Nil, x :: y :: tail, resultPos, resultNeg, intRes)
              case Integer(n) :: tail =>
                loop(Nil, tail, resultPos, resultNeg, intRes - n)
              case One :: tail =>
                loop(Nil, tail, resultPos, resultNeg, intRes - 1)
              case Neg(x) :: tail =>
                loop(x :: Nil, tail, resultPos, resultNeg, intRes)
              case other :: tail =>
                val r2 = if (other.isZero) resultNeg else (other :: resultNeg)
                loop(Nil, tail, resultPos, r2, intRes)
            }
        }

      loop(e, Nil, Nil, Nil, BigInt(0))
    }
    // invariant: no One or Mult items, and at most one Integer returned in the list
    def flattenMult[A](e: List[Expr[A]]): List[Expr[A]] = {
      @annotation.tailrec
      def loop(
          in: List[Expr[A]],
          result: List[Expr[A]],
          ints: BigInt,
          pos: Boolean
      ): List[Expr[A]] =
        in match {
          case Neg(x) :: tail =>
            loop(x :: tail, result, ints, !pos)
          case Integer(n) :: tail =>
            if (n == 0) (Zero :: Nil)
            else if (n == -1) loop(tail, result, ints, !pos)
            else loop(tail, result, ints * n, pos)
          case Mult(x, y) :: tail => loop(x :: y :: tail, result, ints, pos)
          case other :: tail      =>
            if (other.isZero) (Zero :: Nil)
            else {
              val r2 = if (other.isOne) result else (other :: result)
              loop(tail, r2, ints, pos)
            }
          case Nil =>
            if (ints == 1) {
              if (pos) result else (Neg(One) :: result)
            } else {
              val i = if (pos) ints else (-ints)
              Integer(i) :: result
            }
        }

      loop(e, Nil, BigInt(1), true)
    }

    def multAll[A](items: List[Expr[A]]): Expr[A] = {
      @annotation.tailrec
      def loop(stack: List[Expr[A]], ints: BigInt, acc: Expr[A]): Expr[A] =
        stack match {
          case Nil =>
            if (ints == 1) acc
            else {
              val ix = Integer(ints)
              if (acc.isOne) ix
              else Mult(ix, acc)
            }
          case Integer(n) :: tail => loop(tail, ints * n, acc)
          case Neg(n) :: tail     =>
            loop(n :: tail, -ints, acc)
          case Mult(_, _) :: _ =>
            sys.error(s"invariant violation: flattenMult returned Mult")
          case (others @ (Symbol(_) | Add(_, _) | Zero | One)) :: tail =>
            if (others.isZero) Zero
            else if (others.isOne) loop(tail, ints, acc)
            else loop(tail, ints, Mult(others, acc))
        }

      val flat = sortExpr(flattenMult(items))
      loop(flat, BigInt(1), One)
    }

    def addAll[A](items: List[Expr[A]]): Expr[A] = {
      @annotation.tailrec
      def loop(stack: List[Expr[A]], ints: BigInt, acc: Expr[A]): Expr[A] =
        stack match {
          case Nil =>
            if (ints == 0) acc
            else {
              val ix = Integer(ints)
              if (acc.isZero) ix
              else Add(acc, ix)
            }
          case Integer(n) :: tail => loop(tail, ints + n, acc)
          case (others @ (Symbol(_) | Mult(_, _) | One)) :: tail =>
            if (others.isZero) loop(tail, ints, acc)
            else loop(tail, ints, Add(others, acc))
          case (err @ (Add(_, _) | Neg(_) | Zero)) :: _ =>
            sys.error(s"invariant violation: unexpected Add/Neg/Zero: $err")
        }

      val (pos, neg) = flattenAddSub(items)
      val posPart = loop(sortExpr(pos), BigInt(0), Zero)
      val negPart = loop(sortExpr(neg), BigInt(0), Zero)
      if (negPart.isZero) posPart
      else if (posPart.isZero) Neg(negPart)
      else Add(posPart, Neg(negPart))
    }

    // Canonical structural key to enforce determinism (commutativity handled by sorting)
    def key[A](e: Expr[A]): String = e match {
      case Zero       => "0"
      case One        => "1"
      case Integer(n) => s"I($n)"
      case Symbol(n)  => s"S($n)"
      case Neg(x)     => s"N(${key(x)})"
      case Add(a, b)  => s"A(${key(a)},${key(b)})"
      case Mult(a, b) => s"M(${key(a)},${key(b)})"
    }

    // Sort a list of Expr[A]s by key (used for commutativity)
    def sortExpr[A](es: List[Expr[A]]): List[Expr[A]] =
      es.sortBy(key)
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
            if (multCost <= addCost) Expr.multAll(Integer(coeff) :: base :: Nil)
            else {
              // add instead
              Expr.addAll(List.fill(coeff.toInt)(base))
            }
          }
        case -1 =>
          if (coeff == -1) Neg(base)
          else {
            val bcost = w.cost(base)
            val negCoeff = -coeff
            val multCost = w.mult + bcost
            val addCost = negCoeff * bcost + (negCoeff - 1) * w.add + w.neg
            if (multCost <= addCost) Expr.multAll(Integer(coeff) :: base :: Nil)
            else {
              // add instead
              Neg(Expr.addAll(List.fill(negCoeff.toInt)(base)))
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

    case add @ Add(_, _) =>
      normAdd1(add, W)

    case Mult(a, b) =>
      normMult(Expr.flattenMult(a :: b :: Nil), W)
  }

  private def normAdd1[A](add: Add[A], W: Weights): Expr[A] = {
    val (pos, neg) = Expr.flattenAddSub(add :: Nil)
    def toProd(term: Expr[A]): List[Expr[A]] =
      Expr.flattenMult(term :: Nil)

    val sumProd = pos.map(toProd) ::: neg.map(t => toProd(Neg(t)))

    def optSumProd(sumProd: List[List[Expr[A]]]): Expr[A] = {
      // we are going to factor the terms into: a(a_part) + not_a
      // for all atoms in the sum. Then, choose the one with the least cost.
      // then we will recurse on the least cost.
      val atoms: List[Expr[A]] = sumProd.flatten.distinct.filterNot(_.isOne)

      def divTerm(prod: List[Expr[A]], term: Expr[A]): List[Expr[A]] = {
        val idx = prod.indexWhere(_ == term)
        // delete idx
        prod.take(idx) ::: prod.drop(idx + 1)
      }

      def sumProdOf(es: List[List[Expr[A]]]): Expr[A] =
        Expr.addAll(es.map(Expr.multAll))

      def affine(
          atom: Expr[A],
          withAtom: Expr[A],
          withoutAtom: Expr[A]
      ): Expr[A] = {
        val atom1 = if (withAtom.isOne) atom else (atom * withAtom)
        if (withoutAtom.isZero) atom1 else (atom1 + withoutAtom)
      }

      val currentCost = W.cost(sumProdOf(sumProd))
      val factored
          : List[(Expr[A], List[List[Expr[A]]], List[List[Expr[A]]], Int)] =
        atoms
          .map { atom =>
            val (hasAtom, doesNot) = sumProd.partition(l => l.exists(_ == atom))
            val divAtom = hasAtom.map(divTerm(_, atom))
            val cost =
              W.cost(affine(atom, sumProdOf(divAtom), sumProdOf(doesNot)))
            (atom, divAtom, doesNot, cost)
          }
          .filter { case (_, _, _, cost) => cost <= currentCost }

      if (factored.isEmpty) {
        // we don't seem to improve by any greedy step, recurse where we are:
        sumProdOf(sumProd.map(_.map(norm(_, W))))
      } else {
        // we have at least one factorization that is better, try that:
        val (atom, divAtom, notAtom, _) = factored.minBy {
          case (expr, _, _, cost) => (cost, Expr.key(expr))
        }
        val withAtom = optSumProd(divAtom)
        val withoutAtom = optSumProd(notAtom)
        val normAtom = norm(atom, W)
        affine(normAtom, withAtom, withoutAtom)
      }
    }

    optSumProd(sumProd)
  }
  // Normalize addition list: combine integer coefficients per base, sort, optional factoring
  private def normAdd0[A](add: Add[A], W: Weights): Expr[A] = {
    val (pos, neg) = Expr.flattenAddSub(add :: Nil)
    // Fully normalize each term and drop zeros
    val posTerms =
      for {
        p <- pos
        term = norm(p, W)
        if !term.isZero
        (c, base) = term.coeffAndBase
        flatBase <- Expr.flattenAdd(base :: Nil)
        // now none of flatBase are Add(_, _)
      } yield (c, flatBase)

    val negTerms =
      for {
        n <- neg
        term = norm(n, W)
        if !term.isZero
        (c, base) = term.coeffAndBase
        flatBase <- Expr.flattenAdd(base :: Nil)
        // now none of flatBase are Add(_, _)
      } yield (-c, flatBase)

    val terms = posTerms ::: negTerms
    // Sum integer coefficients for identical bases
    val tallied: Map[Expr[A], BigInt] =
      terms.foldLeft(Map.empty[Expr[A], BigInt]) { case (acc, (c, base)) =>
        val c2 = acc.getOrElse(base, BigInt(0)) + c
        if (c2 == 0) acc - base else acc.updated(base, c2)
      }

    factorize(tallied, W)
  }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A](factors0: List[Expr[A]], W: Weights): Expr[A] =
    if (factors0.isEmpty) One
    else if (factors0.exists(_ == Zero)) Zero
    else {
      val normed = factors0.flatMap(e => Expr.flattenMult(norm(e, W) :: Nil))
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
        val base = Expr.multAll(bodies.toList)
        buildScaled(coeff, base, W)
      }
    }

  // Given the sum of terms, return the best factorization of it we can efficiently find
  private def factorize[A](
      tallied: Map[Expr[A], BigInt],
      weights: Weights
  ): Expr[A] = {
    // Rebuild terms from (base -> coeff)
    val terms: List[Expr[A]] =
      tallied.iterator.map { case (base, c) =>
        buildScaled(c, base, weights)
      }.toList

    if (terms.isEmpty) Zero
    else {
      val canon = Expr.sortExpr(terms)
      val summed = Expr.addAll(canon)
      tryFactor(canon, weights) match {
        case None     => summed
        case Some(fs) => chooseBest(summed, fs, weights)
      }
    }
  }

  // === Local factoring across sums ===
  // If all terms are product-like (Symbol/Neg/Mult/One/Integer), factor common non-integer
  // products AND (new) the gcd of integer coefficients.
  private def tryFactor[A](terms: List[Expr[A]], W: Weights): Option[Expr[A]] =
    if ((terms.lengthCompare(2) < 0) || !terms.forall(_.noTopLevelAdd)) None
    else {
      // we have at least 2 items and none of them have top level adds
      // Represent each term as (integer coeff, multiset of non-integer factors)
      val asProducts: List[(BigInt, List[Expr[A]])] = terms.map(_.unproduct)

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
          val base = Expr.multAll(rest)
          val newCoeff =
            if (gcdCoeff > 1) c / gcdCoeff else c
          buildScaled(newCoeff, base, W)
        }

        val inner = Expr.addAll(residuals)
        val outer =
          if (gcdCoeff > 1)
            Expr.multAll(Integer(gcdCoeff) :: inner :: commonNonInt)
          else Expr.multAll(inner :: commonNonInt)

        // Only accept factoring if it reduces cost (or ties and is canonical-smaller)
        Some(chooseBest(Expr.addAll(terms), outer, W))
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
      if (Expr.key(left) < Expr.key(right)) left
      else right
    }
  }

}
