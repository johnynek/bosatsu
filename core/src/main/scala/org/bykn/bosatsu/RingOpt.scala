package org.bykn.bosatsu

import cats.{Hash, Show}
import cats.collections.{HashMap, HashSet}
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
    def negate: MultiSet[A, B]
    def nonZeroIterator: Iterator[(A, B)]

    def hashMap: HashMap[A, B]
  }

  object MultiSet {
    implicit def showMultiSet[A: Show, B: Show]: Show[MultiSet[A, B]] =
      Show[HashMap[A, B]].contramap[MultiSet[A, B]](_.hashMap)

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

      def negate: MultiSet[A, B] = {
        import hashMap.hashKey
        Impl(
          HashMap.fromIterableOnce(
            hashMap.iterator.map { case (a, b) => (a, numeric.negate(b)) }
          ),
          numeric
        )
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
        case One                          => true
        case Zero | Symbol(_) | Add(_, _) => false
        case Neg(n)                       => n.isNegOne
        case Integer(n)                   => n == 1
        case Mult(x, y)                   => x.isOne && y.isOne
      }

    def isNegOne: Boolean =
      expr match {
        case Integer(n)                         => n == -1
        case Neg(x)                             => x.isOne
        case One | Zero | Symbol(_) | Add(_, _) => false
        case Mult(x, y)                         =>
          (x.isNegOne && y.isOne) || (x.isOne && y.isNegOne)
      }

    // TODO: how this is allowed to change cost isn't clear
    // probably unbounded change is bad
    def maybeDivInt(bi: BigInt): Option[Expr[A]] =
      if (bi == 0) None
      else if (bi == 1) Some(this)
      else if (bi == -1) Some(normalizeNeg)
      else
        expr match {
          case Zero            => Some(Zero)
          case One | Symbol(_) => None
          case Neg(n)          =>
            // (-x)/y == -(x/y)
            n.maybeDivInt(bi).map(_.normalizeNeg)
          case Integer(n) =>
            val (d, m) = n /% bi
            if (m == 0) Some(canonInt(d))
            else None
          case Add(x, y) =>
            // (x + y)/n == (x/n + y/n)
            (x.maybeDivInt(bi), y.maybeDivInt(bi)).mapN(Add(_, _))
          case Mult(x, y) =>
            x.unConstMult
              .flatMap { case (bix, x1) =>
                val (d, m) = bix /% bi
                if (m == 0)
                  Some(Expr.checkMult(Integer(d), Expr.checkMult(x1, y)))
                else {
                  // we can't divide just using the left
                  y.unConstMult.flatMap { case (biy, y1) =>
                    val biXY = bix * biy
                    val (d, m) = biXY /% bi
                    if (m == 0)
                      Some(Expr.checkMult(Integer(d), Expr.checkMult(x1, y1)))
                    else None
                  }
                }
              }
              .orElse {
                // we can't divide just using the left
                y.unConstMult.flatMap { case (biy, y1) =>
                  val (d, m) = biy /% bi
                  if (m == 0)
                    Some(Expr.checkMult(Integer(d), Expr.checkMult(x, y1)))
                  else None
                }
              }
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

    def unConstMult: Option[(BigInt, Expr[A])] =
      expr match {
        case One | Symbol(_) => None
        case Zero            => Some((BigInt(0), One))
        case Integer(n)      => Some((n, One))
        case Add(x, y)       =>
          // (n1 * x1 + n2 * y1) = (g := gcd(n1, n2))((n1/g)*x1 + (n2/g)*y1)
          x.unConstMult.flatMap { case xres @ (n1, x1) =>
            if (n1 == 0) y.unConstMult
            else {
              y.unConstMult.flatMap { case (n2, y1) =>
                if (n2 == 0) Some(xres)
                else {
                  val g = n1.gcd(n2)
                  if (g != 1) {
                    Some(
                      (
                        g,
                        Expr.checkAdd(
                          Expr.checkMult(Integer(n1 / g), x1),
                          Expr.checkMult(Integer(n2 / g), y1)
                        )
                      )
                    )
                  } else None
                }

              }
            }
          }
        case Mult(x, y) =>
          (x.unConstMult, y.unConstMult) match {
            case (None, None)           => None
            case (Some((n1, x1)), None) =>
              Some((n1, Expr.checkMult(x1, y)))
            case (None, Some((n1, y1))) =>
              Some((n1, Expr.checkMult(x, y1)))
            case (Some((n1, x1)), Some((n2, y1))) =>
              Some((n1 * n2, Expr.checkMult(x1, y1)))
          }
        case Neg(One) => Some((BigInt(-1), One))
        case Neg(x)   =>
          x.unConstMult.map { case (n, x) => (-n, x) }
      }
    // if we can implement this by absorbing a multiplication by a constant
    // do it. The cost of the returned expression is <= original
    def absorbMultConst(c: BigInt): Option[Expr[A]] =
      if ((c == 0) || isZero) Some(Zero)
      else
        expr match {
          case One        => Some(canonInt(c))
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
        @annotation.tailrec
        def simpleFlatAdd(e: List[Expr[A]], acc: List[Expr[A]]): List[Expr[A]] =
          e match {
            case Nil               => acc
            case Add(x, y) :: tail =>
              simpleFlatAdd(x :: y :: tail, acc)
            case notAdd :: tail =>
              simpleFlatAdd(tail, notAdd :: acc)
          }
        @annotation.tailrec
        def simpleFlatMult(
            e: List[Expr[A]],
            acc: List[Expr[A]]
        ): List[Expr[A]] =
          e match {
            case Nil                => acc
            case Mult(x, y) :: tail =>
              simpleFlatMult(x :: y :: tail, acc)
            case notMult :: tail =>
              simpleFlatMult(tail, notMult :: acc)
          }

        // TODO: this isn't strictly tail recursive though we deal with long chains of
        // adds or mults, but not mutually nested ones
        def hash(a: Expr[A]): Int =
          a match {
            case Symbol(a)      => 1 + 31 * Hash[A].hash(a)
            case One            => 2 + 31 * MurmurHash3.caseClassHash(One)
            case Zero           => 3 + 31 * MurmurHash3.caseClassHash(Zero)
            case i @ Integer(_) => 4 + 31 * MurmurHash3.caseClassHash(i)
            case Add(x, y)      =>
              val flatAdd = simpleFlatAdd(x :: y :: Nil, Nil)
              5 + flatAdd.foldLeft(0) { (acc, x) =>
                31 * acc + hash(x)
              }
            case Mult(x, y) =>
              val flatMult = simpleFlatMult(x :: y :: Nil, Nil)
              6 + flatMult.foldLeft(0) { (acc, x) =>
                31 * acc + hash(x)
              }
            case Neg(x) => 7 + 31 * (hash(x))
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

    def toValue[A](expr: Expr[A])(implicit num: Numeric[A]): A = {
      // very deep chains of multiplications produce astronomically
      // large numbers so we assume those aren't deep but maybe add is
      // also, large chains of Neg don't make much sense, so are very
      // unlikely to appear in natural code
      @annotation.tailrec
      def loop(es: List[Expr[A]], sum: A): A =
        es match {
          case Nil          => sum
          case expr :: tail =>
            expr match {
              case Zero       => loop(tail, sum)
              case One        => loop(tail, num.plus(sum, num.one))
              case Integer(n) => loop(tail, num.plus(sum, fromBigInt[A](n)))
              case Symbol(a)  => loop(tail, num.plus(sum, a))
              case Add(x, y)  => loop(x :: y :: tail, sum)
              // handle chains of Neg
              case Neg(Neg(x)) => loop(x :: tail, sum)
              // these last two we cheat and use toValue directly
              case Mult(x, y) =>
                loop(tail, num.plus(sum, num.times(toValue(x), toValue(y))))
              case Neg(x) =>
                loop(tail, num.plus(sum, num.negate(toValue(x))))
            }
        }

      loop(expr :: Nil, num.zero)
    }

    // return the sum such that all non-sum terms get a coefficient and we also
    // return any integer parts fully added up (as the first item in the tuple)
    // so: val (const, terms) = groupSum(es)
    // then terms.iterator.foldLeft(const) { case (acc, (e, n)) =>
    //   acc + n * e
    // }
    // is the same as the original sum
    def groupSum[A: Hash](
        e: List[Expr[A]]
    ): (BigInt, MultiSet[AddTerm[A], BigInt]) = {
      @annotation.tailrec
      def loop(
          in: List[(Boolean, Expr[A])],
          res: MultiSet[AddTerm[A], BigInt],
          intRes: BigInt
      ): (BigInt, MultiSet[AddTerm[A], BigInt]) =
        in match {
          case (c, Add(x, y)) :: tail =>
            loop((c, x) :: (c, y) :: tail, res, intRes)
          case (c, Neg(x)) :: tail =>
            loop((!c, x) :: tail, res, intRes)
          case (n1, Integer(n)) :: tail =>
            loop(tail, res, intRes + (if (n1) n else (-n)))
          case (n1, One) :: tail =>
            loop(tail, res, intRes + (if (n1) 1 else -1))
          case (_, Zero) :: tail =>
            loop(tail, res, intRes)
          case (n1, sym @ Symbol(_)) :: tail =>
            loop(tail, res.add(sym, if (n1) 1 else -1), intRes)
          case (c, m @ Mult(_, _)) :: tail =>
            /*
            We have some choices here: we could try to avoid
            negation with cheapNeg, but also we could pull
            integers out with unConstMult. But also,
            how many times do different factors show up?
            If we go too crazy now, that defeats the purpose
            of optSumProd...
             */
            val r2 =
              if (m.isZero) res
              else res.add(m, if (c) 1 else -1)
            loop(tail, r2, intRes)
          case Nil => (intRes, res)
        }

      loop(e.map((true, _)), MultiSet.empty[AddTerm[A], BigInt], BigInt(0))
    }

    // return all the positive and negative additive terms from this list
    // invariant: return no top level Add(_, _) or Neg(_)
    def flattenAddSub[A: Hash](
        e: List[Expr[A]],
        w: Weights
    ): (List[Expr[A]], List[Expr[A]]) = {

      def bigFill(
          bi: BigInt,
          expr: Expr[A],
          acc: List[Expr[A]]
      ): List[Expr[A]] =
        if (bi == -1) sys.error(s"invariant violation: bi=$bi, expr=$expr")
        else if (bi == 0) acc
        else if (bi == 1) (expr :: acc)
        else if (w.multIsBetter(expr, bi)) {
          // Expr.multAll(expr :: Integer(bi) :: Nil) :: acc
          Expr.checkMult(expr, Integer(bi)) :: acc
        } else if (bi.isValidInt) {
          val listFill = List.fill(bi.toInt)(expr)
          if (acc.isEmpty) listFill
          else (listFill reverse_::: acc)
        } else {
          // this should basically never happen, it means
          // that somehow multiplication is basically infinitely expensive
          // and even adding something together IntMax times is better than
          // multiplying once
          var lst: List[Expr[A]] = acc
          var size = bi
          while (size > 0) {
            size = size - 1
            lst = expr :: lst
          }
          lst
        }

      val (intRes, res) = groupSum(e)
      // flatten out the results
      // Put the values in a canonical order
      // we reverse so after the foldLeft will return the right order
      val nonZeros = res.nonZeroIterator
        .map { case (e, b) => (e.toExpr, b) }
        .toList
        .reverse
        .sortBy { case (e, _) =>
          Expr.key(e)
        }
      val empty = List.empty[Expr[A]]
      val resTup @ (resultPos, resultNeg) =
        nonZeros.foldLeft((empty, empty)) { case ((pos, neg), (expr, cnt)) =>
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
      else {
        if (resultPos.isEmpty && resultNeg.nonEmpty && (intRes < 0)) {
          (resultPos, canonInt(-intRes) :: resultNeg)
        } else {
          (canonInt(intRes) :: resultPos, resultNeg)
        }
      }
    }

    def negateProduct[A](head: Expr[A], tail: List[Expr[A]]): List[Expr[A]] =
      tail match {
        case Nil => head.normalizeNeg :: Nil
        case _   =>
          val all = head :: tail
          val allIdx = all.zipWithIndex
          val cheaps = allIdx.flatMap { case (e, idx) =>
            e.cheapNeg.map((_, idx))
          }
          if (cheaps.nonEmpty) {
            // we can select a cheap negation
            val (e, idx) = cheaps.minBy { case (e, _) => Expr.key(e) }
            all.updated(idx, e)
          } else {
            // just pick the minimum of none are cheap
            val (e, idx) = allIdx.minBy { case (e, _) => Expr.key(e) }
            all.updated(idx, e.normalizeNeg)
          }
      }

    // invariant: no One (or .isOne), Zero (or items that .isZero is true), Integer Mult items in list
    def flattenMult[A](e: List[Expr[A]]): (Option[BigInt], List[Expr[A]]) = {
      @annotation.tailrec
      def loop(
          in: List[Expr[A]],
          result: List[Expr[A]],
          ints: BigInt,
          pos: Boolean
      ): (Option[BigInt], List[Expr[A]]) =
        in match {
          case Zero :: _      => (Some(BigInt(0)), Nil)
          case One :: tail    => loop(tail, result, ints, pos)
          case Neg(x) :: tail =>
            loop(x :: tail, result, ints, !pos)
          case Integer(n) :: tail =>
            if (n == 0) (Some(BigInt(0)), Nil)
            else if (n == -1) loop(tail, result, ints, !pos)
            else loop(tail, result, ints * n, pos)
          case (sym @ Symbol(_)) :: tail =>
            // symbol is totally opaque
            loop(tail, sym :: result, ints, pos)
          case Mult(x, y) :: tail => loop(x :: y :: tail, result, ints, pos)
          case (add @ Add(_, _)) :: tail =>
            if (add.isZero) (Some(BigInt(0)), Nil)
            else {
              add.saveNeg match {
                case Some(n) =>
                  // we can save by negating
                  loop(tail, n :: result, ints, !pos)
                case None =>
                  // we can't negate
                  loop(tail, add :: result, ints, pos)
              }
            }
          case Nil =>
            if (ints == 1) {
              if (pos) (None, result)
              else {
                // we need to negate the result as cheaply as possible
                result match {
                  case Nil       => (Some(BigInt(-1)), Nil)
                  case h :: tail => (None, negateProduct(h, tail))
                }
              }
            } else {
              val i = if (pos) ints else (-ints)
              (Some(i), result)
            }
        }

      loop(e, Nil, BigInt(1), true)
    }

    def multAll[A](items: List[Expr[A]]): Expr[A] = {
      @annotation.tailrec
      def loop(stack: List[Expr[A]], ints: BigInt, acc: Expr[A]): Expr[A] =
        stack match {
          case Nil =>
            val ix = canonInt(ints)
            checkMult(ix, acc)
          case Integer(n) :: tail =>
            if (n == 0) Zero
            else loop(tail, ints * n, acc)
          case Neg(n) :: tail =>
            loop(n :: tail, -ints, acc)
          case (others @ (Symbol(_) | Add(_, _))) :: tail =>
            loop(tail, ints, checkMult(others, acc))
          case (unexpect @ (Mult(_, _) | One | Zero)) :: _ =>
            sys.error(s"invariant violation: flattenMult returned $unexpect")
        }

      val (optInt, flat) = flattenMult(sortExpr(items))
      loop(flat, optInt.getOrElse(BigInt(1)), One)
    }

    def addAll[A: Hash](items: List[Expr[A]], w: Weights): Expr[A] = {
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

      val (pos, neg) = flattenAddSub(items, w)
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
      // Negation is always cheaper than mult by -1
      else if (a.isNegOne) b.normalizeNeg
      else if (b.isNegOne) a.normalizeNeg
      else Mult(a, b)

    // check if either arg is zero before adding
    def checkAdd[A](a: Expr[A], b: Expr[A]): Expr[A] =
      if (a.isZero) b
      else if (b.isZero) a
      else Add(a, b)

    def replicateAdd[A](cnt: BigInt, expr: Expr[A]): Expr[A] = {
      // invariant: cnt >= 1 and expr != 0
      def loop(cnt: BigInt): Expr[A] =
        if (cnt == 1) expr
        else {
          // cnt >= 2, so (cnt >> 1) >= 1
          // for all x > 1
          // x * expr = (2n + 1) * expr, or (2n) expr
          val half = loop(cnt >> 1)
          val twiceHalf = checkAdd(half, half)
          if (cnt.testBit(0)) {
            // this is odd:
            Add(twiceHalf, expr)
          } else {
            twiceHalf
          }
        }

      if (expr.isZero || cnt == 0) Zero
      else if (cnt > 0) loop(cnt)
      else
        (expr.cheapNeg match {
          case Some(negExpr) => replicateAdd(-cnt, negExpr)
          case None          => loop(-cnt).normalizeNeg
        })
    }
  }

  // These are Symbol(_) or Mult(_, _)
  sealed trait AddTerm[+A] {
    def toExpr: Expr[A]
  }
  object AddTerm {
    implicit def hashAddTerm[A: Hash]: Hash[AddTerm[A]] =
      Hash[Expr[A]].contramap[AddTerm[A]](_.toExpr)

    implicit def showAddTerm[A: Show]: Show[AddTerm[A]] =
      Show[Expr[A]].contramap[AddTerm[A]](_.toExpr)
  }

  case object Zero extends Expr[Nothing]
  case object One extends Expr[Nothing]
  final case class Integer(toBigInt: BigInt) extends Expr[Nothing]
  final case class Symbol[A](item: A) extends Expr[A] with AddTerm[A] {
    def toExpr: Expr[A] = this
  }
  final case class Add[A](left: Expr[A], right: Expr[A]) extends Expr[A]
  final case class Mult[A](left: Expr[A], right: Expr[A])
      extends Expr[A]
      with AddTerm[A] {
    def toExpr: Expr[A] = this
  }
  final case class Neg[A](arg: Expr[A]) extends Expr[A]

  // Heuristic cost model (make Mult much costlier than Add)
  final case class Weights(mult: Int, add: Int, neg: Int) {
    require(add > 0, s"add = $add must be > 0")
    require(mult > 0, s"mult = $mult must be > 0")
    require(neg > 0, s"neg = $neg must be > 0")

    def cost[A](e: Expr[A]): Long = {
      @annotation.tailrec
      def loop(stack: List[Expr[A]], acc: Long): Long =
        stack match {
          case e :: tail =>
            e match {
              case Zero | One | Integer(_) | Symbol(_) => loop(tail, acc)
              case Neg(x)    => loop(x :: tail, neg + acc)
              case Add(a, b) =>
                loop(a :: b :: tail, acc + add)
              case Mult(a, b) =>
                loop(a :: b :: tail, acc + mult)
            }
          case Nil => acc
        }

      loop(e :: Nil, 0L)
    }

    /** is it better to do x + x + ... + x (n times) or n * x we can statically
      * compute a number such that if n >= it is always better to multiply.
      */
    val multThreshold: BigInt = {
      // cost of (e + e + ... + e) with bi terms
      // this is i * cost(e) + (i - 1) * add
      // cost of multiplication would be cost(e) + mult
      // so it's better to multiply when:
      // cost(e) + mult <= i * cost(e) + (i - 1) * add
      // mult <= (i - 1) * (cost(e) + add)
      // cost(e) >= 0, so we need the smallest i such that
      // (mult - (i - 1) * add) <= 0
      // mult/add <= (i - 1)
      // mult/add + 1 <= i

      // consider negative multiplications:

      // cost of -(+e + e + ... + e) with bi terms
      // this is i * cost(e) + (i - 1) * add + neg
      // cost of multiplication would be cost(e) + mult
      // so it's better to multiply when:
      // cost(e) + mult <= i * cost(e) + (i - 1) * add + neg
      // mult <= (i - 1) * (cost(e) + add) + neg
      // cost(e) >= 0, so we need the smalled i such that
      // (mult - (i - 1) * add - neg) <= 0
      // (mult - neg)/add - n <= (i - 1)
      // (mult - neg)/add + 1 <= i
      // but, the e could have an outer Neg(_) which
      // can easily be removed, so actually
      // we need to resort to using the above in the pessimistic case

      val (d, m) = BigInt(mult) /% BigInt(add)
      if (m == 0) (d + 1)
      else (d + 2)
    }

    // invariant: const != 0, 1, -1
    private[RingOpt] def multIsBetter[A](
        expr: Expr[A],
        const: BigInt
    ): Boolean = {
      // we know const != 0, 1, -1 because those can be absorbed
      val cAbs = const.abs
      if (multThreshold <= cAbs) {
        // it's always better to multiply
        true
      } else {
        val maybeNegated = if (const < 0) {
          expr.cheapNeg.map(multIsBetter(_, -const))
        } else {
          None
        }

        maybeNegated match {
          case Some(res) => res
          case None      =>

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
            val constMinus1 = cAbs - 1
            val lhsNum = if (const > 0) {
              // if (mult - (i - 1) * add)/(i - 1) <= cost(e)
              // we are better off with mult. But note, cost(e) >=0
              // so, if the lhs is negative, we don't need to compute the cost
              BigInt(mult) - constMinus1 * add
            } else {
              // if (mult - neg - (i - 1) * add)/(i - 1) <= cost(e)
              BigInt(mult - neg) - constMinus1 * add
            }

            lhsNum <= constMinus1 * cost(expr)
        }
      }
    }

    def constMult[A](expr: Expr[A], const: BigInt): Expr[A] =
      expr.absorbMultConst(const).getOrElse {
        if (multIsBetter(expr, const)) {
          if (const < 0) {
            expr.cheapNeg match {
              case Some(negE) =>
                Expr.multAll(negE :: Integer(-const) :: Nil)
              case None =>
                Expr.multAll(expr :: Integer(const) :: Nil)
            }
          } else {
            Expr.multAll(expr :: Integer(const) :: Nil)
          }
        } else {
          Expr.replicateAdd(const, expr)
        }
      }
  }

  object Weights {
    val default: Weights = Weights(mult = 5, add = 1, neg = 1)
  }

  // === Public API ===
  def normalize[A: Hash](e: Expr[A], W: Weights = Weights.default): Expr[A] = {
    val eInit = e
    // if we can't reduce the cost but keep producing different items stop
    // at 10000 so we don't loop forever or blow up memory
    val MaxCount = 10000
    @annotation.tailrec
    def loop(
        e: Expr[A],
        cost: Long,
        reached: HashSet[Expr[A]],
        cnt: Int
    ): Expr[A] =
      if (cnt >= MaxCount) {
        reached.iterator.minBy(Expr.key(_))
      } else {
        val ne = norm(e, W)
        val costNE = W.cost(ne)
        if (costNE < cost) {
          // we improved matters
          loop(ne, costNE, HashSet.empty[Expr[A]].add(ne), 0)
        } else if (costNE == cost) {
          // couldn't improve things, but maybe normalized them
          if (reached.contains(ne)) {
            // we have reached this, return the minimum in the set:
            reached.iterator.minBy(Expr.key(_))
          } else {
            // we haven't seen this before, add it, and normalize again
            loop(ne, costNE, reached.add(ne), cnt + 1)
          }
        } else {
          // costNE > cost
          /*
          implicit val showA: Show[A] = Show.fromToString
          val (pos, neg) = Expr.flattenAddSub(e :: Nil, W)
          sys.error(
            show"normalize increased cost: ${if (eInit != e) show"eInit = $eInit, "
              else ""}e = $e, cost = $cost, ne = $ne, costNE = $costNE, e_pos = $pos, e_neg = $neg"
          )
           */
          // TODO: we should never get here, increasing cost by normalization
          // means we probably are pretty far from optimal and we need to improve
          // our algorithm
          reached.iterator.minBy(Expr.key(_))
        }
      }

    loop(e, W.cost(e), HashSet.empty[Expr[A]].add(e), 0)
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

    case Neg(Neg(x))           => norm(x, W)
    case Neg(Add(left, right)) =>
      val (const, terms) = Expr.groupSum(left :: right :: Nil)
      val opt = normAdd(terms.negate, W)
      opt match {
        case Integer(bi) => Integer(bi - const)
        case _           => Expr.checkAdd(opt, Integer(-const))
      }
    case Neg(mult @ Mult(_, _)) => norm[A](Neg(One) * mult, W)
    case ns @ Neg(Symbol(_))    => ns
    case Neg(Zero)              => Zero
    case Neg(One)               => canonInt(-1)
    case Neg(Integer(n))        => canonInt(-n)

    case Add(left, right) =>
      val (const, terms) = Expr.groupSum(left :: right :: Nil)
      val opt = normAdd(terms, W)
      opt match {
        case Integer(bi) => Integer(bi + const)
        case _           => Expr.checkAdd(opt, Integer(const))
      }

    case Mult(a, b) =>
      val (optConst, factors) = Expr.flattenMult(a :: b :: Nil)
      normMult(optConst, factors, W)
  }

  private def optSumProd[A: Hash](
      sumProd: List[(Option[BigInt], List[Expr[A]])],
      W: Weights
  ): Expr[A] =
    sumProd match {
      case Nil                => Zero
      case (biO, prod) :: Nil =>
        normMult(biO, prod, W)
      case _ => {
        // we are going to factor the terms into: a(a_part) + not_a
        // for all atoms in the sum. Then, choose the one with the least cost.
        // then we will recurse on the least cost.
        val atoms: List[Expr[A]] = sumProd.flatMap { case (obi, l) =>
          // TODO: we need better handling of the constant products
          obi.flatMap { bi =>
            val biPos = bi.abs
            if ((biPos != 1) && (biPos != 0)) Some(Integer(biPos))
            else None
          }.toList ::: l
        }.distinct

        def divTerm(
            biProd: (Option[BigInt], List[Expr[A]]),
            term: Expr[A]
        ): (Option[BigInt], List[Expr[A]]) = {
          val (obi, prod) = biProd
          term match {
            case Integer(termBi) =>
              assert(obi.isDefined)
              implicit val showA: Show[A] = Show.fromToString
              assert(
                (termBi != 1) && (termBi != -1),
                show"biProd = $biProd, term = $term, sumProd = $sumProd"
              )
              (Some(obi.get / termBi), prod)
            case _ =>
              val idx = prod.indexWhere(Hash[Expr[A]].eqv(_, term))
              // the term should be in the list
              assert(idx >= 0)
              // delete idx
              (obi, prod.take(idx) ::: prod.drop(idx + 1))
          }
        }

        def sumProdOf(es: List[(Option[BigInt], List[Expr[A]])]): Expr[A] =
          Expr.addAll(
            es.map { case (obi, es) =>
              val ePart = Expr.multAll(es)
              obi match {
                case Some(bi) => W.constMult(ePart, bi)
                case None     => ePart
              }
            },
            W
          )

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
          Expr.addAll(atom1 :: withoutAtom :: Nil, W)
        }

        val factored: List[
          (
              Expr[A],
              List[(Option[BigInt], List[Expr[A]])],
              List[(Option[BigInt], List[Expr[A]])],
              Long
          )
        ] =
          if (atoms.isEmpty) Nil
          else {
            val currentCost = W.cost(sumProdOf(sumProd))
            atoms
              .map { atom =>
                val (hasAtom, doesNot) =
                  sumProd.partition { case (obi, l) =>
                    atom match {
                      case Integer(atomI) =>
                        obi.exists(_ % atomI == 0)
                      case _ => l.exists(Hash[Expr[A]].eqv(atom, _))
                    }
                  }

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
          Expr.addAll(
            Expr.sortExpr(sumProd.map { case (obi, prod) =>
              normMult(obi, prod, W)
            }),
            W
          )
        } else {
          // we have at least one factorization that is better, try that:
          val (atom, divAtom, notAtom, _) = factored.minBy {
            case (expr, _, _, cost) => (cost, Expr.key(expr))
          }
          val withAtom = optSumProd(divAtom, W)
          val withoutAtom = optSumProd(notAtom, W)
          val normAtom = norm(atom, W)
          val default = affine(normAtom, withAtom, withoutAtom)
          withAtom match {
            case Integer(bi) =>
              // atom * bi + woa == (atom + (woa/bi)) * bi
              withoutAtom.maybeDivInt(bi) match {
                case Some(woaDiv) =>
                  val withDiv =
                    W.constMult(Expr.addAll(atom :: woaDiv :: Nil, W), bi)
                  if (W.cost(withDiv) <= W.cost(default)) {
                    withDiv
                  } else default
                case None =>
                  default
              }
            case _ => default
          }
        }
      }
    }

  private def normAdd[A: Hash](
      sum: MultiSet[AddTerm[A], BigInt],
      W: Weights
  ): Expr[A] = {

    val negCount = sum.nonZeroIterator.map(_._2 < 0).length
    val (neg, sum1) = if (negCount > sum.hashMap.size / 2) {
      // we should negate after
      (true, sum.negate)
    } else {
      (false, sum)
    }

    val sumProd = sum1.nonZeroIterator.flatMap { case (e, c) =>
      val eExpr = e.toExpr
      val (flatC, flatMult) = Expr.flattenMult(eExpr :: Nil)
      val c1 = flatC match {
        case None     => c
        case Some(c0) => c0 * c
      }
      // TODO: here we are mixing multiplication from duplicates in Add
      // and also from inside Mults. But small multiplications are often
      // suboptimal
      if ((c1 < 0) || (W.multThreshold <= c1)) {
        // multiplication should be better
        val c2 = if (c1 == 1) None else Some(c1)

        (c2, flatMult) :: Nil
      } else {
        // repeated adds are better
        // this toInt is safe because multThresh is always <= Int.MaxValue
        List.fill(c1.toInt)((None, flatMult))
      }

    }.toList

    val res = optSumProd(sumProd, W)
    if (neg) res.normalizeNeg else res
  }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A: Hash](
      optConst: Option[BigInt],
      factors: List[Expr[A]],
      W: Weights
  ): Expr[A] = {
    val normFactors = Expr.multAll(factors.map(norm(_, W)))
    optConst match {
      case Some(const) => W.constMult(normFactors, const)
      case None        => normFactors
    }
  }
}
