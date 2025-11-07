package org.bykn.bosatsu

import cats.{Hash, Show}
import cats.data.NonEmptyList
import cats.collections.HashMap
import cats.syntax.all._
import scala.math.Numeric
import scala.util.hashing.MurmurHash3

object RingOpt {

  sealed trait MultiSet[A, B] {
    def hash: Hash[A]
    def numeric: Numeric[B]

    def add(a: A, count: B): MultiSet[A, B]
    def +(a: A): MultiSet[A, B] = add(a, numeric.one)
    def count(a: A): B
    def remove(a: A, count: B): MultiSet[A, B]
    def -(a: A): MultiSet[A, B] = remove(a, numeric.one)
    def --(that: MultiSet[A, B]): MultiSet[A, B]
    def nonZeroIterator: Iterator[(A, B)]
  }

  object MultiSet {
    private case class Impl[A, B](
        hashMap: HashMap[A, B],
        val numeric: Numeric[B]
    ) extends MultiSet[A, B] {
      def hash = hashMap.hashKey

      def add(a: A, count: B): MultiSet[A, B] =
        if (numeric.equiv(count, numeric.zero)) this
        else
          hashMap.get(a) match {
            case Some(b) =>
              val b1 = numeric.plus(b, count)
              if (numeric.equiv(b1, numeric.zero))
                Impl(hashMap.removed(a), numeric)
              else Impl(hashMap.updated(a, b1), numeric)
            case None =>
              Impl(hashMap.updated(a, count), numeric)
          }

      def count(a: A): B = hashMap.getOrElse(a, numeric.zero)
      def remove(a: A, count: B): MultiSet[A, B] =
        if (numeric.equiv(count, numeric.zero)) this
        else
          hashMap.get(a) match {
            case Some(b) =>
              val b1 = numeric.minus(b, count)
              if (numeric.equiv(b1, numeric.zero))
                Impl(hashMap.removed(a), numeric)
              else Impl(hashMap.updated(a, b1), numeric)
            case None =>
              Impl(hashMap.updated(a, numeric.negate(count)), numeric)
          }
      def nonZeroIterator: Iterator[(A, B)] = hashMap.iterator
      def --(that: MultiSet[A, B]): MultiSet[A, B] =
        that.nonZeroIterator.foldLeft(this: MultiSet[A, B]) {
          case (ms, (k, v)) =>
            ms.remove(k, v)
        }
    }

    def empty[A, B](implicit h: Hash[A], n: Numeric[B]): MultiSet[A, B] =
      Impl(HashMap.empty[A, B], n)

    def fromList[A: Hash, B: Numeric](as: List[A]): MultiSet[A, B] =
      as.foldLeft(empty[A, B])((ms, a) => ms + a)
  }

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

    // If this is a single integer node
    def isInt: Option[BigInt] =
      expr match {
        case Zero       => Some(BigInt(0))
        case One        => Some(BigInt(1))
        case Integer(n) => Some(n)
        case _          => None
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

    // Do neg if it is cheaper (only < number of Neg nodes)
    def saveNeg: Option[Expr[A]] = expr match {
      case Zero | One | Integer(_) | Symbol(_) => None
      case Neg(y)                              => Some(y)
      case Add(x, y)                           =>
        // these next two are allowed because normalizeNeg adds at most 1 node
        // so, the total Neg nodes is the same on both sides, satisfying <=
        (for {
          nx <- x.saveNeg
          ny <- y.cheapNeg
        } yield Add(nx, ny)).orElse {
          for {
            ny <- y.saveNeg
            nx <- x.cheapNeg
          } yield Add(nx, ny)
        }
      case Mult(x, y) =>
        x.saveNeg
          .map(Mult(_, y))
          .orElse {
            y.saveNeg.map(Mult(x, _))
          }
    }
    // Do neg if it is as cheap or cheaper (only <= number of Neg nodes)
    def cheapNeg: Option[Expr[A]] = expr match {
      case Zero       => Some(Zero)
      case One        => Some(Integer(-1))
      case Integer(n) => Some(canonInt[A](-n))
      case Neg(y)     => Some(y)
      case Add(x, y)  =>
        // these first two are allowed because normalizeNeg adds at most 1 node and saveNeg saves one
        // so, the total Neg nodes is the same on both sides, satisfying <=
        x.saveNeg
          .map(Add(_, y.normalizeNeg))
          .orElse(
            y.saveNeg.map(Add(x.normalizeNeg, _))
          )
          .orElse(
            // both cheap
            for {
              nx <- x.cheapNeg
              ny <- y.cheapNeg
            } yield Add(nx, ny)
          )
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

    // if we can implement this by absorbing a multiplication by a constant
    // do it. The cost of the returned expression is <= original
    def absorbMultConst(c: BigInt): Option[Expr[A]] =
      if ((c == 0) || isZero) Some(Zero)
      else
        expr match {
          case One        => Some(Integer(c))
          case Zero       => Some(Zero)
          case Symbol(_)  => None
          case Neg(n)     => n.absorbMultConst(-c)
          case Integer(n) => Some(canonInt(n * c))
          case Add(x, y)  =>
            // (x + y)*c == (x*c + y*c)
            for {
              xc <- x.absorbMultConst(c)
              yc <- y.absorbMultConst(c)
            } yield Expr.checkAdd(xc, yc)
          case Mult(x, y) =>
            // x*y*c == x * (y*c) or (x*c) *y
            x.absorbMultConst(c)
              .map(Expr.checkMult(_, y))
              .orElse(y.absorbMultConst(c).map(Expr.checkMult(x, _)))
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

    implicit def hashExpr[A: Hash]: Hash[Expr[A]] =
      new Hash[Expr[A]] {
        def hash(a: Expr[A]): Int =
          a match {
            case Symbol(a)      => 1 + 31 * Hash[A].hash(a)
            case One            => 2 + 31 * MurmurHash3.caseClassHash(One)
            case Zero           => 3 + 31 * MurmurHash3.caseClassHash(Zero)
            case i @ Integer(_) => 4 + 31 * MurmurHash3.caseClassHash(i)
            case Add(x, y)      => 5 + 31 * (hash(x) + 31 * hash(y))
            case Mult(x, y)     => 6 + 31 * (hash(x) + 31 * hash(y))
            case Neg(x)         => 7 + 31 * (hash(x))
          }
        def eqv(a: Expr[A], b: Expr[A]): Boolean =
          a match {
            case Symbol(a) =>
              b match {
                case Symbol(b) => Hash[A].eqv(a, b)
                case _         => false
              }
            case (One | Zero | Integer(_)) => a == b
            case Add(x, y)                 =>
              b match {
                case Add(z, w) => eqv(x, z) && eqv(y, w)
                case _         => false
              }
            case Mult(x, y) =>
              b match {
                case Mult(z, w) => eqv(x, z) && eqv(y, w)
                case _          => false
              }
            case Neg(x) =>
              b match {
                case Neg(y) => eqv(x, y)
                case _      => false
              }
          }
      }
    // Efficiently embed a BigInt into any Numeric[A] by double-and-add of `one`
    def fromBigInt[A](n: BigInt)(implicit num: Numeric[A]): A = {
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
    def flattenAddSub[A: Hash](
        e: List[Expr[A]]
    ): (List[Expr[A]], List[Expr[A]]) = {
      @annotation.tailrec
      def loop(
          inPos: List[Expr[A]],
          inNeg: List[Expr[A]],
          res: MultiSet[Expr[A], BigInt],
          intRes: BigInt
      ): (List[Expr[A]], List[Expr[A]]) =
        inPos match {
          case Add(x, y) :: tail =>
            loop(x :: y :: tail, inNeg, res, intRes)
          case Neg(x) :: tail =>
            loop(tail, x :: inNeg, res, intRes)
          case Integer(n) :: tail =>
            loop(tail, inNeg, res, intRes + n)
          case One :: tail =>
            loop(tail, inNeg, res, intRes + 1)
          case other :: tail =>
            val r2 = if (other.isZero) res else (res + other)
            loop(tail, inNeg, r2, intRes)
          case Nil =>
            inNeg match {
              case Nil =>
                @annotation.tailrec
                def bigFill(
                    bi: BigInt,
                    expr: Expr[A],
                    acc: List[Expr[A]]
                ): List[Expr[A]] =
                  if (bi.isValidInt) {
                    val listFill = List.fill(bi.toInt)(expr)
                    if (acc.isEmpty) listFill
                    else (listFill reverse_::: acc)
                  } else {
                    // too big
                    bigFill(bi - 1, expr, expr :: acc)
                  }

                // flatten out the results
                val e = List.empty[Expr[A]]
                val resTup @ (resultPos, resultNeg) =
                  res.nonZeroIterator.foldLeft((e, e)) {
                    case ((pos, neg), (expr, cnt)) =>
                      if (cnt > 0) {
                        (bigFill(cnt, expr, pos), neg)
                      } else {
                        // cnt < 0, exactly zero we don't see
                        // try to negate
                        expr.cheapNeg match {
                          case Some(negExpr) =>
                            (bigFill(-cnt, negExpr, pos), neg)
                          case None =>
                            (pos, bigFill(-cnt, expr, neg))
                        }
                      }
                  }
                if (intRes == 0) resTup
                else (canonInt(intRes) :: resultPos, resultNeg)
              case Add(x, y) :: tail =>
                loop(Nil, x :: y :: tail, res, intRes)
              case Integer(n) :: tail =>
                loop(Nil, tail, res, intRes - n)
              case One :: tail =>
                loop(Nil, tail, res, intRes - 1)
              case Neg(x) :: tail =>
                loop(x :: Nil, tail, res, intRes)
              case other :: tail =>
                val r2 = if (other.isZero) res else (res - other)
                loop(Nil, tail, r2, intRes)
            }
        }

      loop(e, Nil, MultiSet.empty[Expr[A], BigInt], BigInt(0))
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
            else if (other.isOne) {
              loop(tail, result, ints, pos)
            } else {
              other.saveNeg match {
                case Some(n) =>
                  // we can save by negating
                  loop(tail, n :: result, ints, !pos)
                case None =>
                  // we can't negate
                  loop(tail, other :: result, ints, pos)
              }
            }
          case Nil =>
            if (ints == 1) {
              if (pos) result
              else {
                // we need to negate the result as cheaply as possible
                result match {
                  case Nil      => canonInt(-1) :: Nil
                  case h :: Nil => h.normalizeNeg :: Nil
                  case h :: t   =>
                    @annotation.tailrec
                    def loop(
                        nel: NonEmptyList[Expr[A]],
                        prepend: List[Expr[A]]
                    ): NonEmptyList[Expr[A]] =
                      nel.head.cheapNeg match {
                        case Some(h) =>
                          NonEmptyList(h, nel.tail).prependList(prepend.reverse)
                        case None =>
                          NonEmptyList.fromList(nel.tail) match {
                            case Some(tailNE) =>
                              loop(tailNE, nel.head :: prepend)
                            case None =>
                              // we can't find anything to the far right, so just negate the first item
                              val all = nel.prependList(prepend.reverse)
                              NonEmptyList(all.head.normalizeNeg, all.tail)
                          }
                      }
                    loop(NonEmptyList(h, t), Nil).toList
                }
              }
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

    def addAll[A: Hash](items: List[Expr[A]]): Expr[A] = {
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
      case Add(a, b)  => s"A(${key(a)},${key(b)})"
      case Neg(x)     => s"N(${key(x)})"
      case Mult(a, b) => s"M(${key(a)},${key(b)})"
      // put the constants last
      case Symbol(n)  => s"S($n)"
      case Integer(n) => s"Z($n)"
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

    def replicateAdd[A](cnt: BigInt, expr: Expr[A]): Expr[A] = {
      def loop(cnt: BigInt): Expr[A] =
        if (cnt <= 0) Zero
        else if (cnt == 1) expr
        else {
          // for all x > 1
          // x * expr = (2n + 1) * expr, or (2n) expr
          val half = loop(cnt >> 1)
          val twiceHalf = checkAdd(half, half)
          val lowPart = if (cnt.testBit(0)) {
            // this is odd:
            expr
          } else {
            Zero
          }
          checkAdd(twiceHalf, lowPart)
        }

      if (cnt >= 0) loop(cnt)
      else loop(-cnt).normalizeNeg
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

    def constMult[A](expr: Expr[A], const: BigInt): Expr[A] =
      expr.absorbMultConst(const).getOrElse {
        // cost of (e + e + ... + e) with bi terms
        // this is i * cost(e) + (i - 1) * add
        // cost of multiplication would be cost(e) + mult
        // so it's better to multiply when:
        // cost(e) + mult <= i * cost(e) + (i - 1) * add
        // mult <= (i - 1) * (cost(e) + add)
        // with negation, we need:
        // cost(e) + mult <= i * cost(e) + (i - 1) * add + neg
        // which is
        // mult <= (i - 1) * (cost(e) + add) + neg
        val costE = BigInt(cost(expr))
        if (const > 0) {
          if (BigInt(mult) < (const - 1) * (costE + add)) {
            // multiplication wins
            Expr.multAll(expr :: Integer(const) :: Nil)
          } else {
            Expr.replicateAdd(const, expr)
          }
        } else if (const < 0) {
          if (BigInt(mult) < ((-const - 1) * (costE + add) + neg)) {
            // multiplication wins
            Expr.multAll(expr :: Integer(const) :: Nil)
          } else {
            Expr.replicateAdd(const, expr)
          }
        } else {
          // const == 0
          canonInt(0)
        }
      }
  }

  object Weights {
    val default: Weights = Weights(mult = 5, add = 1, neg = 1)
  }

  // === Public API ===
  def normalize[A: Hash](e: Expr[A], W: Weights = Weights.default): Expr[A] = {
    val eInit = e
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
        implicit val showA: Show[A] = Show.fromToString
        val (pos, neg) = Expr.flattenAddSub(e :: Nil)
        sys.error(
          show"normalize increased cost: ${if (eInit != e) show"eInit = $eInit, "
            else ""}e = $e, cost = $cost, ne = $ne, costNE = $costNE, e_pos = $pos, e_neg = $neg"
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

  private def norm[A: Hash](e: Expr[A], W: Weights): Expr[A] = e match {
    case Zero | One | Symbol(_) => e

    case Integer(n) => canonInt[A](n)

    case Neg(x) =>
      // Normalize and push negation into integers when possible
      norm(x, W).normalizeNeg

    case Add(left, right) =>
      normAdd(left, right, W)

    case Mult(a, b) =>
      val factors = Expr.flattenMult(a :: b :: Nil)
      normMult(factors, W)
  }

  def optSumProd[A: Hash](sumProd: List[List[Expr[A]]], W: Weights): Expr[A] =
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
          val atom1 = atom.isInt match {
            case Some(aInt) =>
              W.constMult(withAtom, aInt)
            case None =>
              withAtom.isInt match {
                case Some(waInt) =>
                  W.constMult(atom, waInt)
                case None =>
                  Expr.multAll(atom :: withAtom :: Nil)
              }
          }
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
          val withAtom = optSumProd(divAtom, W)
          val withoutAtom = optSumProd(notAtom, W)
          val normAtom = norm(atom, W)
          affine(normAtom, withAtom, withoutAtom)
        }
      }
    }

  private def normAdd[A: Hash](
      left: Expr[A],
      right: Expr[A],
      W: Weights
  ): Expr[A] = {
    val (pos, neg) = Expr.flattenAddSub(left :: right :: Nil)
    // we could represent this as Add(pos, Neg(neg))
    // if there is only one neg term we can just add the negative
    // in and keep going, else we have to normalize + and - separately

    def toProd(term: Expr[A]): List[Expr[A]] =
      Expr.flattenMult(term :: Nil)

    if (neg.lengthCompare(1) <= 0) {
      // there is at most one negative
      val sumProd = (pos.iterator.map(toProd) ++
        neg.iterator.map(t => toProd(t.normalizeNeg))).toList

      optSumProd(sumProd, W)
    } else {
      // there are 2 or more negative terms
      // if there is exactly one positive term and we can negate it for free:
      val justCheap = pos match {
        case p :: Nil => p.cheapNeg
        case _        => None
      }

      justCheap
        .map { negp =>
          optSumProd(toProd(negp) :: neg.map(toProd), W).normalizeNeg
        }
        .getOrElse {
          // otherwise there are 2 or more positive and negative terms
          val optPos = optSumProd(pos.map(toProd), W)
          val optNeg = optSumProd(neg.map(toProd), W)
          Expr.addAll(optPos :: optNeg.normalizeNeg :: Nil)
        }
    }

    /*
    // if more are negative than positive, we are better off negating
    // the whole thing
    @annotation.tailrec
    def leftShorter(left: List[Any], right: List[Any]): Boolean =
      left match {
        case Nil        => right.nonEmpty
        case _ :: ltail =>
          right match {
            case Nil        => false
            case _ :: rtail => leftShorter(ltail, rtail)
          }
      }

    // This step is problematic. We can take terms like: Add(x, Neg(y))
    // and add as many Negs as there are additive terms in y. This step
    // can increase costs and break the invariant that we always improve things
    val (outerNeg, sumProd) =
      if (leftShorter(pos, neg)) {
        (
          true,
          (pos.iterator.map(t => toProd(t.normalizeNeg)) ++
            neg.iterator.map(toProd)).toList
        )
      } else {
        (
          false,
          (pos.iterator.map(toProd) ++
            neg.iterator.map(t => toProd(t.normalizeNeg))).toList
        )
      }

    val res0 = optSumProd(sumProd, W)
    if (outerNeg) res0.normalizeNeg
    else res0
     */
  }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A: Hash](factors: List[Expr[A]], W: Weights): Expr[A] = {
    val nfact = factors.map(norm(_, W))
    Expr.multAll(nfact)
  }
}
