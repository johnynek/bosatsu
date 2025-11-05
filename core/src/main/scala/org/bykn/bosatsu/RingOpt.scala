package org.bykn.bosatsu

import scala.math.Numeric
import cats.Show
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
      case Zero       => Some(Zero)
      case One        => Some(Integer(-1))
      case Integer(n) => Some(canonInt[A](-n))
      case Neg(y)     => Some(y)
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
      case Symbol(_) => None
    }

    // At most add 1 Neg node
    def normalizeNeg: Expr[A] =
      cheapNeg match {
        case None        => Neg(expr)
        case Some(cheap) => cheap
      }

    def *[A1 >: A](that: Expr[A1]): Expr[A1] = Mult(expr, that)
    def +[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, that)
    def -[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, Neg(that))
    def unary_- : Expr[A] = Neg(expr)
  }

  object Expr {
    implicit def showExpr[A: Show]: Show[Expr[A]] =
      new Show[Expr[A]] {
        def show(e: Expr[A]): String =
          e match {
            case Zero       => "\u24EA" // 0 in a circle
            case One        => "\u2460" // 1 in a circle
            case Integer(n) => n.toString
            case Symbol(a)  => show"[$a]"
            case Add(x, y)  => show"($x + $y)"
            case Mult(x, y) => show"($x * $y)"
            case Neg(x)     => show"-($x)"
          }
      }
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
              // if (pos) result else (canonInt(-1) :: result)
              if (pos) result else (Neg(One) :: result)
            } else {
              val i = if (pos) ints else (-ints)
              canonInt(i) :: result
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
            else if (ints == -1) acc.normalizeNeg
            else {
              val ix = canonInt(ints)
              checkMult(ix, acc)
            }
          case Integer(n) :: tail =>
            if (n == 0) Zero
            else loop(tail, ints * n, acc)
          case Neg(n) :: tail =>
            loop(n :: tail, -ints, acc)
          case Mult(_, _) :: _ =>
            sys.error(s"invariant violation: flattenMult returned Mult")
          case (others @ (Symbol(_) | Add(_, _) | Zero | One)) :: tail =>
            if (others.isZero) Zero
            else loop(tail, ints, checkMult(others, acc))
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
              val ix = canonInt(ints)
              checkAdd(acc, ix)
            }
          case Integer(n) :: tail => loop(tail, ints + n, acc)
          case (others @ (Symbol(_) | Mult(_, _) | One)) :: tail =>
            if (others.isOne) loop(tail, ints + 1, acc)
            else loop(tail, ints, checkAdd(others, acc))
          case (err @ (Add(_, _) | Neg(_) | Zero)) :: _ =>
            sys.error(s"invariant violation: unexpected Add/Neg/Zero: $err")
        }

      val (pos, neg) = flattenAddSub(items)
      val posPart = loop(sortExpr(pos), BigInt(0), Zero)
      val negPart = loop(sortExpr(neg), BigInt(0), Zero)
      checkAdd(posPart, negPart.normalizeNeg)
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

    // check if either arg is zero or one before multiplying
    def checkMult[A](a: Expr[A], b: Expr[A]): Expr[A] =
      if (a.isZero || b.isZero) Zero
      else if (a.isOne) b
      else if (b.isOne) a
      else Mult(a, b)

    // check if either arg is zero before adding
    def checkAdd[A](a: Expr[A], b: Expr[A]): Expr[A] =
      if (a.isZero) b
      else if (b.isZero) a
      else Add(a, b)
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
    @annotation.tailrec
    def loop(e: Expr[A], cost: Int): Expr[A] = {
      val ne = norm(e, W)
      val costNE = W.cost(ne)
      if (costNE < cost) {
        loop(ne, costNE)
      } else if (costNE == cost) {
        // couldn't improve things, but maybe normalized them
        ne
      } else {
        // costNE > cost
        sys.error(
          s"normalize increased cost: e = $e, cost = $cost, ne = $ne, costNE = $costNE"
        )
      }
    }

    loop(e, W.cost(e))
  }

  // Small helper: canonical integer literal
  def canonInt[A](n: BigInt): Expr[A] =
    if (n == 0) Zero
    else if (n == 1) One
    else Integer(n)

  // === Core normalization ===

  private def norm[A](e: Expr[A], W: Weights): Expr[A] = e match {
    case Zero | One | Symbol(_) => e

    case Integer(n) => canonInt[A](n)

    case Neg(x) =>
      // Normalize and push negation into integers when possible
      norm(x, W).normalizeNeg

    case add @ Add(_, _) =>
      normAdd(add, W)

    case Mult(a, b) =>
      val factors = Expr.flattenMult(a :: b :: Nil)
      normMult(factors, W)
  }

  private def normAdd[A](add: Add[A], W: Weights): Expr[A] = {
    val (pos, neg) = Expr.flattenAddSub(add.left :: add.right :: Nil)

    def toProd(term: Expr[A]): List[Expr[A]] =
      Expr.flattenMult(term :: Nil)

    val sumProd = (pos.iterator.map(toProd) ++
      neg.iterator.map(t => toProd(t.normalizeNeg))).toList

    def optSumProd(sumProd: List[List[Expr[A]]]): Expr[A] =
      sumProd match {
        case Nil         => Zero
        case prod :: Nil => normMult(prod, W)
        case _           => {
          // we are going to factor the terms into: a(a_part) + not_a
          // for all atoms in the sum. Then, choose the one with the least cost.
          // then we will recurse on the least cost.
          val atoms: List[Expr[A]] = sumProd.flatten.distinct.filterNot(_.isOne)

          def divTerm(prod: List[Expr[A]], term: Expr[A]): List[Expr[A]] = {
            // uses Expr.equals
            val idx = prod.indexWhere(_ == term)
            // the term should be in the list
            assert(idx >= 0)
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
            val atom1 = Expr.multAll(atom :: withAtom :: Nil)
            Expr.addAll(atom1 :: withoutAtom :: Nil)
          }

          val factored
              : List[(Expr[A], List[List[Expr[A]]], List[List[Expr[A]]], Int)] =
            if (atoms.isEmpty) Nil
            else {
              val currentCost = W.cost(sumProdOf(sumProd))
              atoms
                .map { atom =>
                  val (hasAtom, doesNot) =
                    sumProd.partition(l => l.exists(_ == atom))
                  val divAtom = hasAtom.map(divTerm(_, atom))
                  // note, this is not the real cost because
                  // we have not normalized atom, divAtom or doesNot
                  // we are trying to get a cheaper estimate of the best path.
                  // this is a heuristic
                  val cost =
                    W.cost(affine(atom, sumProdOf(divAtom), sumProdOf(doesNot)))
                  (atom, divAtom, doesNot, cost)
                }
                .filter { case (_, _, _, cost) => cost <= currentCost }
            }

          if (factored.isEmpty) {
            // we don't seem to improve by any greedy step, recurse where we are:
            Expr.addAll(Expr.sortExpr(sumProd.map { prod =>
              normMult(prod, W)
            }))
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
      }

    optSumProd(sumProd)
  }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A](factors: List[Expr[A]], W: Weights): Expr[A] = {
    val nfact = factors.map(norm(_, W))
    if (nfact.isEmpty) One
    else if (nfact.exists(_.isZero)) Zero
    else {
      Expr.multAll(Expr.sortExpr(nfact))
    }
  }
}
