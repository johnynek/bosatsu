package dev.bosatsu

import cats.{Applicative, Eq, Hash, Order, Show}
import cats.Order.catsKernelOrderingForOrder
import cats.collections.{HashMap, HashSet}
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.math.Numeric

object RingOpt {

  // This is somehow not found in cats...
  implicit private def hashNEL[A: Hash]: Hash[NonEmptyList[A]] =
    Hash[List[A]].contramap[NonEmptyList[A]](_.toList)

  sealed trait MultiSet[A, B] {
    def hash: Hash[A]
    def numeric: Numeric[B]

    def *(that: B): MultiSet[A, B]
    def add(a: A, count: B): MultiSet[A, B]
    def +(a: A): MultiSet[A, B] = add(a, numeric.one)
    def ++(that: MultiSet[A, B]): MultiSet[A, B] =
      that.nonZeroIterator.foldLeft(this: MultiSet[A, B]) { case (ms, (k, v)) =>
        ms.add(k, v)
      }

    def count(a: A): B
    def remove(a: A, count: B): MultiSet[A, B]
    def -(a: A): MultiSet[A, B] = remove(a, numeric.one)
    def --(that: MultiSet[A, B]): MultiSet[A, B] =
      that.nonZeroIterator.foldLeft(this: MultiSet[A, B]) { case (ms, (k, v)) =>
        ms.remove(k, v)
      }
    def negate: MultiSet[A, B]
    def nonZeroIterator: Iterator[(A, B)]

    def hashMap: HashMap[A, B]
    def size: Int = hashMap.size
    def isZero: Boolean = hashMap.isEmpty

    def filter(fn: ((A, B)) => Boolean): MultiSet[A, B]
  }

  object MultiSet {
    implicit def showMultiSet[A: Show, B: Show]: Show[MultiSet[A, B]] =
      Show[HashMap[A, B]].contramap[MultiSet[A, B]](_.hashMap)

    private case class Impl[A, B](
        hashMap: HashMap[A, B],
        val numeric: Numeric[B]
    ) extends MultiSet[A, B] {
      def hash = hashMap.hashKey

      def *(that: B): MultiSet[A, B] = {
        import hashMap.hashKey
        if (numeric.equiv(numeric.zero, that))
          Impl[A, B](HashMap.empty, numeric)
        else if (numeric.equiv(numeric.one, that)) this
        else
          Impl(
            HashMap.fromIterableOnce(hashMap.iterator.map { case (a, b) =>
              (a, numeric.times(b, that))
            }),
            numeric
          )
      }

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
      def filter(fn: ((A, B)) => Boolean): MultiSet[A, B] = {
        import hashMap.hashKey
        Impl(HashMap.fromIterableOnce(hashMap.iterator.filter(fn)), numeric)
      }
    }

    def empty[A, B](implicit h: Hash[A], n: Numeric[B]): MultiSet[A, B] =
      Impl(HashMap.empty[A, B], n)

    def fromList[A: Hash, B: Numeric](as: List[A]): MultiSet[A, B] =
      as.foldLeft(empty[A, B])((ms, a) => ms + a)

    def fromListCount[A: Hash, B: Numeric](as: List[(A, B)]): MultiSet[A, B] =
      as.foldLeft(empty[A, B]) { case (ms, (a, b)) => ms.add(a, b) }
  }

  sealed trait Expr[+A] derives CanEqual { expr =>
    def map[B](fn: A => B): Expr[B] =
      toStack.map(fn).toExpr match {
        case Right(exprB) => exprB
        case Left(err)    =>
          sys.error(s"invariant violation: toExpr returned Left($err)")
      }

    def traverse[F[_]: Applicative, B](fn: A => F[B]): F[Expr[B]] =
      toStack
        .traverse(fn)
        .map(_.toExpr match {
          case Right(exprB) => exprB
          case Left(err)    =>
            sys.error(s"invariant violation: toExpr returned Left($err)")
        })

    def maybeBigInt(onSym: A => Option[BigInt]): Option[BigInt] =
      // Stack makes this stack safe
      toStack.maybeBigInt(onSym)

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

    /** divide by an integer returning an expression with cost <= current cost
      * additionally, we can recombine the bi without increasing cost with:
      * res.bestEffortConstMult(div)
      */
    def maybeDivInt(bi: BigInt): Option[Expr[A]] =
      if (bi == 0) None
      else if (bi == 1) Some(this)
      else if (bi == -1) cheapNeg
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
            // (x + y)/n == (x/n +y/n)
            // NOTE: this isn't exact, Add could have common factors,
            // like Add(2, 4) / 6 == 1. This isn't full normalization, but local inspection
            // of the graph
            (x.maybeDivInt(bi), y.maybeDivInt(bi)).mapN(Expr.checkAdd(_, _))
          case mul @ Mult(a, b) =>
            a.maybeDivInt(bi)
              .map(Expr.checkMult(_, b))
              .orElse {
                b.maybeDivInt(bi).map(Expr.checkMult(a, _))
              }
              .orElse {
                // we can't take fully from either side, let's try both sides

                // Note: unConstMult can't find all constants, for instance
                // (10 * Symbol(a) + 2 * Symbol(a)) doesn't return (12, Symbol(a))
                // since that would require a normalizing equivalance between two formulas
                // which we don't currently (cheaply) have.
                mul.unConstMult match {
                  case Some((coeff, rest)) =>
                    val (d, m) = coeff /% bi
                    if (m == 0)
                      rest.absorbMultConst(d) match {
                        case s @ Some(_) => s
                        case _           => Some(rest.bestEffortConstMult(d))
                      }
                    else
                      None

                  case _ => None
                }
              }
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
            // put the cheapNeg last, which may lead to a subtraction
          } yield Add(ny, nx)
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
        // try to put the negative item second to preserve a subtraction
        x.saveNeg
          .map(Add(_, y.normalizeNeg))
          .orElse(
            y.saveNeg.map(Add(_, x.normalizeNeg))
          )
          .orElse(
            // both cheap
            for {
              nx <- x.cheapNeg
              ny <- y.cheapNeg
            } yield Add(nx, ny)
          )
      /*
      case Mult(Integer(x), Integer(y)) => Some(canonInt(-(x * y)))
      case Mult(_, Integer(_)) | Mult(Integer(_), _) =>
        // just because we can cheaply negate a multiplication
        // argument, doesn't mean that the cost is less after mult
        // consider x * (-2). That can be cheaper than -(x + x)
        // but x * 2 is very often more expensive than x + x.
        //
        None*/
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

    /** if we return Some((i, x)) the laws here are:
      *   1. Mult(Integer(i), x) == this
      *   2. cost(Mult(Integer(i), x)) <= cost(this) || x.isOne
      *   3. this.unConstMult.flatMap { case (_, e) => e.unConstMult } == None ||
      *      (this.isZero) (we pull all the ints out once)
      *   4. if we return zero, both the integer and the Expr are zero.
      *   5. we never return 1 or -1 as the multiplier
      *   6. the expression returned is never Integer(_) or Neg(_)
      */
    def unConstMult: Option[(BigInt, Expr[A])] = {
      val origCounts = Expr.opCounts(expr)

      def noOpCostIncrease(coeff: BigInt, inner: Expr[A]): Boolean =
        if (inner.isOne || inner.isZero) true
        else {
          val newCounts = Expr.opCounts(Mult(Integer(coeff), inner))
          newCounts.asCheapOrCheaperThan(origCounts)
        }

      def loop(expr: Expr[A], insideMult: Boolean): Option[(BigInt, Expr[A])] =
        expr match {
          case s @ Symbol(_) =>
            if (insideMult) Some((BigInt(1), s))
            else None
          case One =>
            // we pull out -1 insideMult so we can group all the signs if possible
            if (insideMult) Some((BigInt(1), One))
            else None
          case Integer(n) =>
            if (n == 0) {
              // Zero is caught by checkAdd and checkMult so okay to return
              Some((BigInt(0), Zero))
            } else if ((n == 1) || (n == -1)) {
              if (insideMult) Some((n, One))
              else None
            } else Some((n, One))
          case Zero =>
            // Zero is caught by checkAdd and checkMult so okay to return
            Some((BigInt(0), Zero))
          case Neg(x) =>
            loop(x, insideMult) match {
              case Some((n, x)) => Some((-n, x))
              case None         =>
                if (insideMult) Some((BigInt(-1), x))
                else None
            }
          case Add(x, y) =>
            // to save, we need to pull one or the other all the way out:
            // (n1 * x1 + n2 * y1) = n1 * (x1 + (n2/n1) * y1) or
            // (n1 * x1 + n2 * y1) = n2 * ((n1/n2) * x1 + y1)
            (loop(x, insideMult), loop(y, insideMult))
              .flatMapN { case (xres @ (n1, x1), yres @ (n2, x2)) =>
                if (n1 == 0) {
                  // x == 0
                  Some(yres)
                } else if (n2 == 0) {
                  // y == 0
                  Some(xres)
                } else if (x1.isOne && x2.isOne) {
                  // if we have (n1*x + n2*x) we can just evaluate the sum directly
                  // = (n1 + n2, x)
                  Some((n1 + n2, One))
                } else {
                  val (d21, m21) = n2 /% n1
                  if (m21 == 0) {
                    // we can pull n1 out
                    Some(
                      (
                        n1,
                        Expr.checkAdd(x1, x2.bestEffortConstMult(d21))
                      )
                    )
                  } else {
                    val (d12, m12) = n1 /% n2
                    if (m12 == 0) {
                      // we can pull n2 out
                      Some(
                        (
                          n2,
                          Expr.checkAdd(x1.bestEffortConstMult(d12), x2)
                        )
                      )
                    } else {
                      // (n1 * x1 + n2 * x2) but neither n1 | n2 nor n2 | n1
                      // if we have g = gcd(n1, n2) and g != 1, -1
                      // we could write g * ((n1/g) * x1 + (n2/g) * x2)
                      // if the cost is lower. And that's where absorbMultConst
                      // comes in. If we can absorb at least one of them, we
                      // save <= the number of multiplications
                      val g = n1.gcd(n2)
                      if ((g != 1) && (g != -1)) {
                        val n1g = n1 / g
                        val n2g = n2 / g

                        x1.absorbMultConst(n1g)
                          .map { n11 =>
                            (
                              g,
                              Expr.checkAdd(
                                n11,
                                x2.bestEffortConstMult(n2g)
                              )
                            )
                          }
                          .orElse {
                            x2.absorbMultConst(n2g).map { n22 =>
                              (
                                g,
                                Expr.checkAdd(
                                  x1.bestEffortConstMult(n1g),
                                  n22
                                )
                              )
                            }
                          }
                      } else None
                    }
                  }
                }
              }
          case Mult(x, y) =>
            // Inside Mult we are more permissive to allow integers to bubble up
            // but we need to be sure not to return 1 in the end
            @inline def lx = loop(x, insideMult = true)
            @inline def ly = loop(y, insideMult = true)

            val res =
              (lx, ly) match {
                case (None, None)           => None
                case (Some((n1, x1)), None) =>
                  val m = Expr.checkMult(x1, y)
                  val i = if (m.isZero) BigInt(0) else n1
                  Some((i, m))
                case (None, Some((n1, y1))) =>
                  val m = Expr.checkMult(x, y1)
                  val i = if (m.isZero) BigInt(0) else n1
                  Some((i, m))
                case (Some((n1, x1)), Some((n2, y1))) =>
                  Some((n1 * n2, Expr.checkMult(x1, y1)))
              }

            if (insideMult) res
            else {
              // we aren't double nested, so make sure not to return 1
              res match {
                case s @ Some((n, _)) if (n != 1) && (n != -1) => s
                case _                                         => None
              }
            }
        }

      @annotation.tailrec
      def fix(prod: BigInt, e: Expr[A]): Option[(BigInt, Expr[A])] =
        loop(e, false) match {
          case Some((bi, inner)) =>
            if (bi == 0) Some((BigInt(0), Zero))
            else {
              fix(bi * prod, inner)
            }
          case None =>
            if ((prod == 1) || (prod == -1)) None
            else Some((prod, e))
        }

      fix(BigInt(1), expr).filter { case (coeff, inner) =>
        noOpCostIncrease(coeff, inner)
      }
    }

    // try first to absorb, then to negate, only then do we use Mult
    def bestEffortConstMult(c: BigInt): Expr[A] =
      absorbMultConst(c) match {
        case Some(res) => res
        case None      =>
          // negation should always be cheaper than multiplication
          if (c == -1) normalizeNeg
          else {
            expr match {
              // Keep the sign on the literal when we can so we don't add a
              // standalone Neg node and increase cost.
              case Neg(n) => n.bestEffortConstMult(-c)
              case _      =>
                // we know c != 1, 0, -1
                // we know that this is not 1, 0, -1 because absorbMultConst handles those
                if (c < 0) {
                  // see if we can remove a Neg
                  cheapNeg match {
                    case Some(n) => Mult(n, Integer(-c))
                    case None    => Mult(this, Integer(c))
                  }
                } else {
                  Mult(this, Integer(c))
                }
            }
          }
      }
    // if we can implement this by absorbing a multiplication by a constant
    // do it. The cost of the returned expression is <= original
    def absorbMultConst(c: BigInt): Option[Expr[A]] =
      // check zeros first
      if (c == 0) Some(Zero)
      else if (isZero) Some(Zero)
      else if (c == 1) Some(this)
      else if (c == -1) {
        // we can't be sure negation is cheaper (yet)
        // maybe if cheapNeg took weights we could tell
        None
      } else if (isOne) Some(canonInt(c))
      else if (isNegOne) Some(canonInt(-c))
      else
        expr match {
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
          case _ =>
            // Zero, One, -1 are handled above
            None
        }

    def *[A1 >: A](that: Expr[A1]): Expr[A1] = Mult(expr, that)
    def +[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, that)
    def -[A1 >: A](that: Expr[A1]): Expr[A1] = Add(expr, Neg(that))
    def unary_- : Expr[A] = Neg(expr)

    def graphSize: Long = {
      @annotation.tailrec
      def loop(stack: List[Expr[A]], acc: Long): Long =
        stack match {
          case Nil       => acc
          case e :: tail =>
            val acc1 = acc + 1
            e match {
              case Zero | One | Integer(_) | Symbol(_) =>
                // leaf node, nothing more to push
                loop(tail, acc1)

              case Neg(x) =>
                loop(x :: tail, acc1)

              case Add(x, y) =>
                loop(x :: y :: tail, acc1)

              case Mult(x, y) =>
                loop(x :: y :: tail, acc1)
            }
        }
      loop(expr :: Nil, 0L)

    }

    private[RingOpt] lazy val toStack: Stack[A] =
      Stack.fromExpr(this)
  }

  object Expr {
    final case class OpCounts(mul: Int, add: Int, neg: Int) {
      def asCheapOrCheaperThan(other: OpCounts): Boolean =
        (mul <= other.mul) && (add <= other.add) && (neg <= other.neg)
    }

    def opCounts[A](e: Expr[A]): OpCounts = {
      @annotation.tailrec
      def loop(
          stack: List[Expr[A]],
          mulCnt: Int,
          addCnt: Int,
          negCnt: Int
      ): OpCounts =
        stack match {
          case Nil => OpCounts(mulCnt, addCnt, negCnt)
          case head :: tail =>
            head match {
              case Zero | One | Integer(_) | Symbol(_) =>
                loop(tail, mulCnt, addCnt, negCnt)
              case Neg(x) =>
                loop(x :: tail, mulCnt, addCnt, negCnt + 1)
              case Add(x, y) =>
                loop(x :: y :: tail, mulCnt, addCnt + 1, negCnt)
              case Mult(x, y) =>
                loop(x :: y :: tail, mulCnt + 1, addCnt, negCnt)
            }
        }

      loop(e :: Nil, 0, 0, 0)
    }

    def symbol[A](a: A): Expr[A] = Symbol(a)
    def int(i: BigInt): Expr[Nothing] = Integer(i)

    implicit def eqExpr[A](implicit eqA: Eq[A]): Eq[Expr[A]] =
      new Eq[Expr[A]] {
        import Stack._

        private given Eq[A] = eqA
        private given Eq[Leaf[A]] =
          new Eq[Leaf[A]] {
            override def eqv(left: Leaf[A], right: Leaf[A]): Boolean =
              (left, right) match {
                case (Zero, Zero)             => true
                case (One, One)               => true
                case (Integer(l), Integer(r)) => l == r
                case (Symbol(l), Symbol(r))   => (l: A) === (r: A)
                case _                        => false
              }
          }

        @annotation.tailrec
        private def loop(left: Stack[A], right: Stack[A]): Boolean =
          (left, right) match {
            case (Empty, Empty)                           => true
            case (Push(lleaf, lonto), Push(rleaf, ronto)) =>
              ((lleaf: Leaf[A]) === (rleaf: Leaf[A])) && loop(lonto, ronto)
            case (Operate(lop, lon), Operate(rop, ron)) =>
              (lop == rop) && loop(lon, ron)
            case _ => false
          }

        override def eqv(left: Expr[A], right: Expr[A]): Boolean =
          loop(left.toStack, right.toStack)
      }

    implicit class ExprOps[A](private val expr: Expr[A]) extends AnyVal {
      // This does basic normalization, like ordering Add(_, _) and Mult(_, _)
      // and collapsing constants, we do this before doing harder optimizations
      // since we have better equality at that point
      def basicNorm(implicit o: Order[A]): Expr[A] = {
        def normExpr[A1 <: A](e: Expr[A1]): Expr[A] = e.basicNorm(using o)

        def add(a: Expr[A], b: Expr[A]) =
          if (a === Zero) b
          else if (b === Zero) a
          else {
            def isAdd(e: Expr[A]): Boolean =
              e match {
                case Add(_, _) => true
                case _         => false
              }

            val aAdd = isAdd(a)
            val bAdd = isAdd(b)
            if (bAdd && !aAdd) {
              // just associate directly
              Add(a, b)
            } else if (aAdd && !bAdd) {
              Add(b, a)
            }
            // both or neither are Add
            else if (Order[Expr[A]].lteqv(a, b)) Add(a, b)
            else Add(b, a)
          }

        def mult(a: Expr[A], b: Expr[A]) =
          if (Order[Expr[A]].lteqv(a, b)) Mult(a, b)
          else Mult(b, a)

        def simpleNeg(e: Expr[A]): Expr[A] =
          // normalizeNeg isn't idempotent
          e.normalizeNeg match {
            case Neg(x) => Neg(normExpr(x))
            case notNeg =>
              // the negative was pushed down, normalize again
              normExpr(notNeg)
          }

        expr match {
          case Zero | One | Symbol(_) => expr
          case Integer(i)             => canonInt(i)
          case Neg(Neg(x))            => normExpr(x)
          case Neg(x)                 => simpleNeg(normExpr(x))
          case Add(x, y)              =>
            // TODO: if we move this to object Expr and take Hash[A]
            // we could normalize x + -(x) into 0, but that should be
            // handled in the groupSum so maybe that's not needed
            def step(nx: Expr[A], ny: Expr[A]): Expr[A] =
              nx match {
                case Integer(ix) =>
                  if (ix == 0) ny
                  else
                    ny match {
                      case Integer(iy) => canonInt(ix + iy)
                      case Zero        => nx
                      case One         => canonInt(ix + 1)
                      case _           => add(nx, ny)
                    }
                case Zero => ny
                case One  =>
                  ny match {
                    case Integer(iy) => canonInt(iy + 1)
                    case Zero        => nx
                    case One         => canonInt(2)
                    case _           => add(nx, ny)
                  }
                case _ =>
                  ny match {
                    case Zero => nx
                    case _    => add(nx, ny)
                  }
              }
            def flatAdd(
                es: List[Expr[A]],
                const: BigInt,
                acc: List[Expr[A]]
            ): Expr[A] =
              es match {
                case Add(x, y) :: tail =>
                  flatAdd(x :: y :: tail, const, acc)
                case notAdd :: tail =>
                  normExpr(notAdd) match {
                    case Add(x, y) =>
                      flatAdd(x :: y :: tail, const, acc)
                    case Zero =>
                      flatAdd(tail, const, acc)
                    case One =>
                      flatAdd(tail, const + 1, acc)
                    case Integer(i) =>
                      flatAdd(tail, const + i, acc)
                    case (other @ (Symbol(_) | Mult(_, _) | Neg(_))) =>
                      flatAdd(tail, const, other :: acc)
                  }
                case Nil =>
                  if (acc.isEmpty) canonInt(const)
                  else
                    step(
                      acc.sorted.reduceRight(step(_, _)),
                      canonInt(const)
                    )
              }

            flatAdd(x :: y :: Nil, BigInt(0), Nil)
          case Mult(x, y) =>
            def step(nx: Expr[A], ny: Expr[A]) =
              nx match {
                case Integer(ix) =>
                  if (ix == 0) canonInt(0)
                  else if (ix == -1) simpleNeg(ny)
                  else
                    ny match {
                      case Integer(iy) => canonInt(ix * iy)
                      case Zero        => canonInt(0)
                      case One         => nx
                      case Neg(negY)   =>
                        if (ix == -1) negY
                        else mult(canonInt(-ix), negY)
                      case _ => mult(nx, ny)
                    }
                case Zero      => canonInt(0)
                case One       => ny
                case Neg(negX) =>
                  ny match {
                    case Zero        => Zero
                    case One         => nx
                    case Neg(negY)   => mult(negX, negY)
                    case Integer(iy) =>
                      if (iy == -1) negX
                      else mult(negX, canonInt(-iy))
                    case _ =>
                      simpleNeg(mult(negX, ny))
                  }
                case _ =>
                  ny match {
                    case Zero                      => Zero
                    case One                       => nx
                    case Integer(iy) if (iy == -1) => simpleNeg(nx)
                    case Neg(negy)                 =>
                      simpleNeg(mult(nx, negy))
                    case _ => mult(nx, ny)
                  }
              }
            def flatMult(
                es: List[Expr[A]],
                coeff: BigInt,
                acc: List[Expr[A]]
            ): Expr[A] =
              es match {
                case Mult(x, y) :: tail =>
                  flatMult(x :: y :: tail, coeff, acc)
                case One :: tail =>
                  flatMult(tail, coeff, acc)
                case Zero :: _           => Zero
                case Integer(ix) :: tail =>
                  flatMult(tail, coeff * ix, acc)
                case Neg(nx) :: tail =>
                  flatMult(nx :: tail, coeff * -1, acc)
                case (sym @ Symbol(_)) :: tail =>
                  flatMult(tail, coeff, sym :: acc)
                case (add @ Add(_, _)) :: tail =>
                  add.saveNeg match {
                    case Some(negAdd) =>
                      // saveNeg isn't normalized
                      flatMult(normExpr(negAdd) :: tail, -coeff, acc)
                    case None =>
                      flatMult(tail, coeff, add :: acc)
                  }
                case Nil =>
                  if (acc.isEmpty) canonInt(coeff)
                  else
                    step(
                      acc.sorted.reduceRight(step(_, _)),
                      canonInt(coeff)
                    )
              }

            flatMult(normExpr(x) :: normExpr(y) :: Nil, BigInt(1), Nil)
        }
      }

    }

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

    // This is also an Ordering[Expr[A]]
    implicit def numericExpr[A: Ordering]: Numeric[Expr[A]] =
      new Numeric[Expr[A]] {
        val ordExpr = orderExpr[A](using Order.fromOrdering[A])

        override val zero: Expr[A] = Zero
        override val one: Expr[A] = One

        def fromInt(x: Int): Expr[A] =
          canonInt(BigInt(x))

        def minus(a: Expr[A], b: Expr[A]): Expr[A] =
          Add(a, Neg(b))

        def negate(a: Expr[A]): Expr[A] = Neg(a)

        def parseString(str: String): Option[Expr[A]] =
          None // we could do this if we could parse A

        def plus(a: Expr[A], b: Expr[A]): Expr[A] = Add(a, b)

        def times(a: Expr[A], b: Expr[A]): Expr[A] = Mult(a, b)

        def toDouble(e: Expr[A]): Double =
          e.maybeBigInt(_ => None) match {
            case Some(bi) => bi.toDouble
            case None     => java.lang.Double.NaN
          }

        def toFloat(e: Expr[A]): Float = toDouble(e).toFloat
        def toLong(e: Expr[A]): Long =
          e.maybeBigInt(_ => None) match {
            case Some(bi) => bi.toLong
            case None     => 0L
          }
        def toInt(e: Expr[A]): Int =
          e.maybeBigInt(_ => None) match {
            case Some(bi) => bi.toInt
            case None     => 0
          }

        def compare(a: Expr[A], b: Expr[A]): Int =
          ordExpr.compare(a, b)
      }

    implicit def hashExpr[A: Hash]: Hash[Expr[A]] =
      // we use the private lazy val to prevent converting to stack repeatedly
      Hash[Stack[A]].contramap[Expr[A]](_.toStack)

    implicit def orderExpr[A: Order]: Order[Expr[A]] =
      // we use the private lazy val to prevent converting to stack repeatedly
      Order[Stack[A]].contramap[Expr[A]](_.toStack)

    // Efficiently embed a BigInt into any Numeric[A]
    def fromBigInt[A](n: BigInt)(implicit num: Numeric[A]): A = {
      import num._
      if (n.isValidInt) fromInt(n.toInt)
      else {
        // We want a large positive int, 2^31 is negative
        val BaseBits = 30
        val BaseMask = (1 << BaseBits) - 1
        val Base = fromInt(1 << BaseBits)
        val abs = n.abs
        @annotation.tailrec
        def loop(k: BigInt, thisBase: A, acc: A): A =
          // k = Base * k1 + k0 = Base * (Base * k11 + k10) + k0
          if (k == 0) acc
          else {
            val k0 = fromInt(k.toInt & BaseMask)
            val k1 = k >> BaseBits
            val nextBase = times(thisBase, Base)
            loop(k1, nextBase, plus(times(k0, thisBase), acc))
          }
        val base = loop(abs, one, zero)
        if (n.signum < 0) negate(base) else base
      }
    }

    def toValue[A](expr: Expr[A])(implicit num: Numeric[A]): A =
      // Stack is stack safe, as the name suggests
      Stack.toValue(expr.toStack) match {
        case Right(a)  => a
        case Left(err) =>
          // This cannot happen because all stacks from Exprs
          // are valid and can be converted toValue
          sys.error(
            s"invariant violation, well formed Expr couldn't be evaluated: $err"
          )
      }

    // invariant: no One (or .isOne), Zero (or items that .isZero is true), Integer Mult items in list
    def flattenMult[A: Order](e: List[Expr[A]]): (BigInt, List[MultTerm[A]]) = {
      @annotation.tailrec
      def loop(
          in: List[Expr[A]],
          result: List[MultTerm[A]],
          ints: BigInt
      ): (BigInt, List[MultTerm[A]]) =
        in match {
          case Zero :: _      => (BigInt(0), Nil)
          case One :: tail    => loop(tail, result, ints)
          case Neg(x) :: tail =>
            loop(x :: tail, result, -ints)
          case Integer(n) :: tail =>
            if (n == 0) (BigInt(0), Nil)
            else loop(tail, result, ints * n)
          case (sym @ Symbol(_)) :: tail =>
            // symbol is totally opaque
            loop(tail, sym :: result, ints)
          case Mult(x, y) :: tail        => loop(x :: y :: tail, result, ints)
          case (add @ Add(_, _)) :: tail =>
            if (add.isZero) (BigInt(0), Nil)
            else {
              val (x, y, z) = add.saveNeg
                .map { n =>
                  // we can save by negating
                  (n :: tail, result, -ints)
                }
                .orElse {
                  // see if we can factor out a constant from the add
                  add.unConstMult.map { case (n, expr) =>
                    if (n == 0) (Nil, Nil, BigInt(0))
                    else (expr :: tail, result, n * ints)
                  }
                }
                .getOrElse {
                  (tail, add :: result, ints)
                }

              loop(x, y, z)
            }
          case Nil => (ints, result.sortBy(_.toExpr))
        }

      loop(e, Nil, BigInt(1))
    }

    def multAll[A: Order](items: List[Expr[A]]): Expr[A] = {
      val (coeff, flat) = flattenMult(items.sorted)
      // foldRight so the rhs is shallow in the Mult
      val base =
        flat.foldRight(One: Expr[A])((t, acc) => checkMult(t.toExpr, acc))
      checkMult(canonInt(coeff), base)
    }

    def addAll[A: Order](items: List[Expr[A]]): Expr[A] = {
      @annotation.tailrec
      def loop(
          stack: List[(Boolean, Expr[A])],
          ints: BigInt,
          pos: List[Expr[A]],
          neg: List[Expr[A]]
      ): Expr[A] =
        stack match {
          case Nil =>
            val ix = canonInt[A](ints)
            val p = pos.foldLeft(ix)(checkAdd(_, _))
            val n = neg.foldLeft(Zero: Expr[A])(checkAdd(_, _)).normalizeNeg
            checkAdd(p, n)
          case (isPos, Integer(n)) :: tail =>
            val n1 = if (isPos) n else -n
            loop(tail, ints + n1, pos, neg)
          case (isPos, sym @ Symbol(_)) :: tail =>
            if (isPos) loop(tail, ints, sym :: pos, neg)
            else loop(tail, ints, pos, sym :: neg)
          case (isPos, mult @ Mult(_, _)) :: tail =>
            // we can potentially save by negating a mult
            mult.saveNeg match {
              case Some(negMult) =>
                val isPos1 = !isPos
                if (isPos1) loop(tail, ints, negMult :: pos, neg)
                else loop(tail, ints, pos, negMult :: neg)
              case None =>
                if (isPos) loop(tail, ints, mult :: pos, neg)
                else loop(tail, ints, pos, mult :: neg)
            }
          case (isPos, Neg(x)) :: tail =>
            loop((!isPos, x) :: tail, ints, pos, neg)
          case (isPos, Add(a, b)) :: tail =>
            loop((isPos, a) :: (isPos, b) :: tail, ints, pos, neg)
          case (_, Zero) :: tail =>
            loop(tail, ints, pos, neg)
          case (isPos, One) :: tail =>
            val n1 = if (isPos) 1 else -1
            loop(tail, ints + n1, pos, neg)
        }
      loop(items.sorted.map(x => (true, x)), BigInt(0), Nil, Nil)
    }

    // check if either arg is zero or one before multiplying
    def checkMult[A](a: Expr[A], b: Expr[A]): Expr[A] =
      /*
      if (a.isZero || b.isZero) Zero
      else if (a.isOne) b
      else if (b.isOne) a
      // Negation is always cheaper than mult by -1
      else if (a.isNegOne) b.normalizeNeg
      else if (b.isNegOne) a.normalizeNeg
      else { */
      a.maybeBigInt(_ => None) match {
        case Some(ia) => b.bestEffortConstMult(ia)
        case None     =>
          b.maybeBigInt(_ => None) match {
            case Some(ib) => a.bestEffortConstMult(ib)
            case None     => Mult(a, b)
          }
      }
    // }

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

    def undistribute[A: Hash](e: Expr[A]): Expr[A] = {
      // helper to check equality
      def isEq(x: Expr[A], y: Expr[A]): Boolean = Hash[Expr[A]].eqv(x, y)

      e match {
        case Add(l0, r0) =>
          val l0a: Expr[A] = l0
          val r0a: Expr[A] = r0
          val l = undistribute[A](l0a)
          val r = undistribute[A](r0a)

          (l, r) match {
            case (Mult(a, b), Mult(c, d)) =>
              if (isEq(a, c)) Mult(a, undistribute(Add(b, d)))
              else if (isEq(a, d)) Mult(a, undistribute(Add(b, c)))
              else if (isEq(b, c)) Mult(b, undistribute(Add(a, d)))
              else if (isEq(b, d)) Mult(b, undistribute(Add(a, c)))
              else Add(l, r)
            case (Mult(a, b), Neg(c)) =>
              if (isEq(a, c)) Mult(a, Add(b, Neg(One)))
              else if (isEq(b, c)) Mult(b, Add(a, Neg(One)))
              else Add(l, r)
            case (Mult(a, b), c) =>
              if (isEq(a, c)) Mult(a, Add(b, One))
              else if (isEq(b, c)) Mult(b, Add(a, One))
              else Add(l, r)
            case (Neg(c), Mult(a, b)) =>
              if (isEq(a, c)) Mult(a, Add(Neg(One), b))
              else if (isEq(b, c)) Mult(b, Add(Neg(One), a))
              else Add(l, r)
            case (c, Mult(a, b)) =>
              if (isEq(a, c)) Mult(a, Add(One, b))
              else if (isEq(b, c)) Mult(b, Add(One, a))
              else Add(l, r)
            case _ => Add(l, r)
          }
        case Neg(x) =>
          val xa: Expr[A] = x
          Neg(undistribute[A](xa))
        case Mult(x, y) =>
          val xa: Expr[A] = x
          val ya: Expr[A] = y
          Mult(undistribute[A](xa), undistribute[A](ya))
        case One | Zero | Symbol(_) | Integer(_) => e
      }
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

  sealed trait Leaf[+A] extends Expr[A] derives CanEqual
  // These are Symbol(_) or Add(_, _)
  sealed trait MultTerm[+A] {
    def toExpr: Expr[A]
  }
  object MultTerm {
    implicit def hashMultTerm[A: Hash]: Hash[MultTerm[A]] =
      Hash[Expr[A]].contramap[MultTerm[A]](_.toExpr)

    implicit def orderMultTerm[A: Order]: Order[MultTerm[A]] =
      Order[Expr[A]].contramap[MultTerm[A]](_.toExpr)

    implicit def showMultTerm[A: Show]: Show[MultTerm[A]] =
      Show[Expr[A]].contramap[MultTerm[A]](_.toExpr)
  }

  case object Zero extends Expr[Nothing] with Leaf[Nothing]
  case object One extends Expr[Nothing] with Leaf[Nothing]
  final case class Integer(toBigInt: BigInt)
      extends Expr[Nothing]
      with Leaf[Nothing]
  final case class Symbol[A](item: A)
      extends Expr[A]
      with AddTerm[A]
      with MultTerm[A]
      with Leaf[A] {
    def toExpr: Expr[A] = this
  }
  final case class Add[A](left: Expr[A], right: Expr[A])
      extends Expr[A]
      with MultTerm[A] {
    def toExpr: Expr[A] = this
  }
  final case class Mult[A](left: Expr[A], right: Expr[A])
      extends Expr[A]
      with AddTerm[A] {
    def toExpr: Expr[A] = this
  }
  final case class Neg[A](arg: Expr[A]) extends Expr[A]

  sealed abstract class Op(val opId: Int) derives CanEqual
  object Op {
    case object Neg extends Op(0)
    case object Add extends Op(1)
    case object Mult extends Op(2)

    implicit val hashOp: Hash[Op] = Hash.fromUniversalHashCode
  }

  sealed trait Stack[+A] derives CanEqual {
    def maybeBigInt(onSym: A => Option[BigInt]): Option[BigInt] = {
      import Stack._

      def toI(l: Leaf[A]): Option[BigInt] =
        l match {
          case Symbol(a)  => onSym(a)
          case Integer(a) => Some(a)
          case One        => Some(BigInt(1))
          case Zero       => Some(BigInt(0))
        }

      @annotation.tailrec
      def loop(s: Stack[A], alist: List[Option[BigInt]]): Option[BigInt] =
        s match {
          case Empty =>
            alist match {
              case optA :: Nil => optA
              case _           => None
            }
          case Push(leaf, rest) =>
            loop(rest, toI(leaf) :: alist)
          case Operate(op, rest) =>
            op match {
              case Op.Add =>
                alist match {
                  case Some(a) :: Some(b) :: tail =>
                    loop(rest, Some(a + b) :: tail)
                  case _ :: _ :: tail => loop(rest, None :: tail)
                  case _              =>
                    // this is an error
                    None
                }
              case Op.Mult =>
                alist match {
                  case Some(a) :: Some(b) :: tail =>
                    loop(rest, Some(a * b) :: tail)
                  case (sz @ Some(z)) :: None :: tail if z == BigInt(0) =>
                    loop(rest, sz :: tail)
                  case None :: (sz @ Some(z)) :: tail if z == BigInt(0) =>
                    loop(rest, sz :: tail)
                  case _ :: _ :: tail =>
                    // we don't know what it is
                    loop(rest, None :: tail)
                  case _ => None
                }
              case Op.Neg =>
                alist match {
                  case Some(a) :: tail => loop(rest, Some(-a) :: tail)
                  case None :: tail    => loop(rest, None :: tail)
                  case Nil             => None
                }
            }
        }

      loop(this, Nil)
    }

    def map[B](fn: A => B): Stack[B] = {
      import Stack._
      @annotation.tailrec
      def opList(s: Stack[A], items: List[Either[Op, Leaf[B]]]): Stack[B] =
        s match {
          case Empty =>
            items.foldLeft(Empty: Stack[B]) {
              case (s, Right(l)) => Push(l, s)
              case (s, Left(o))  => Operate(o, s)
            }
          case Push(Symbol(a), onto) =>
            opList(onto, Right(Symbol(fn(a))) :: items)
          case Push(One, onto) =>
            opList(onto, Right(One) :: items)
          case Push(Zero, onto) =>
            opList(onto, Right(Zero) :: items)
          case Push(i @ Integer(_), onto) =>
            opList(onto, Right(i) :: items)
          case Operate(op, on) =>
            opList(on, Left(op) :: items)
        }

      opList(this, Nil)
    }

    def traverse[F[_], B](
        fn: A => F[B]
    )(implicit F: Applicative[F]): F[Stack[B]] = {
      import Stack._
      val r0: F[Leaf[B]] = F.pure(Zero)
      val r1: F[Leaf[B]] = F.pure(One)

      @annotation.tailrec
      def opList(s: Stack[A], items: List[Either[Op, Leaf[A]]]): F[Stack[B]] =
        s match {
          case Empty =>
            // Now we traverse the list and turn it back into Stack
            items
              .traverse { either =>
                either.traverse {
                  case Symbol(a)  => fn(a).map(b => Symbol(b): Leaf[B])
                  case Zero       => r0
                  case One        => r1
                  case Integer(i) => F.pure(Integer(i): Leaf[B])
                }
              }
              .map { items =>
                items.foldLeft(Empty: Stack[B]) {
                  case (s, Right(l)) => Push(l, s)
                  case (s, Left(o))  => Operate(o, s)
                }
              }
          case Push(leaf, onto) =>
            opList(onto, Right(leaf) :: items)
          case Operate(op, on) =>
            opList(on, Left(op) :: items)
        }

      opList(this, Nil)
    }

    /** If this stack was constructed from an Expr it will always return Right
      */
    def toExpr: Either[Stack.Error[? <: Expr[A]], Expr[A]] = {
      import Stack._
      // This is duplicative with toValue, but
      // I can't see a way to reuse and get exact equality
      @annotation.tailrec
      def loop(
          s: Stack[A],
          alist: List[Expr[A]]
      ): Either[Error[? <: Expr[A]], Expr[A]] =
        s match {
          case Empty =>
            alist match {
              case a :: Nil  => Right(a)
              case notSingle => Left(Error.ResultStackNotSingleton(notSingle))
            }
          case Push(leaf, rest) =>
            loop(rest, leaf :: alist)
          case Operate(op, rest) =>
            op match {
              case Op.Add =>
                alist match {
                  case a :: b :: tail => loop(rest, Add(b, a) :: tail)
                  case zeroOrOne @ (Nil | (_ :: Nil)) =>
                    Left(Error.ExpectedTwoOps(Op.Add, zeroOrOne.headOption))
                }
              case Op.Mult =>
                alist match {
                  case a :: b :: tail => loop(rest, Mult(b, a) :: tail)
                  case zeroOrOne @ (Nil | (_ :: Nil)) =>
                    Left(Error.ExpectedTwoOps(Op.Mult, zeroOrOne.headOption))
                }
              case Op.Neg =>
                alist match {
                  case a :: tail => loop(rest, Neg(a) :: tail)
                  case Nil       =>
                    Left(Error.ExpectedOneOp(Op.Neg))
                }
            }
        }

      loop(this, Nil)
    }
  }

  object Stack {
    case object Empty extends Stack[Nothing]
    case class Push[A](leaf: Leaf[A], onto: Stack[A]) extends Stack[A]
    case class Operate[A](op: Op, on: Stack[A]) extends Stack[A]

    implicit def hashStack[A: Hash]: Hash[Stack[A]] =
      new Hash[Stack[A]] {
        private val hashA: Hash[A] = Hash[A]
        private given Eq[A] = hashA
        private given Eq[Leaf[A]] =
          new Eq[Leaf[A]] {
            override def eqv(left: Leaf[A], right: Leaf[A]): Boolean =
              (left, right) match {
                case (Zero, Zero)             => true
                case (One, One)               => true
                case (Integer(l), Integer(r)) => l == r
                case (Symbol(l), Symbol(r))   => (l: A) === (r: A)
                case _                        => false
              }
          }

        override def hash(s: Stack[A]): Int = {
          @annotation.tailrec
          def loop(current: Stack[A], acc: Int): Int = current match {
            case Empty            => acc
            case Push(leaf, onto) =>
              val leafHash = leaf match {
                case (Zero | One) => leaf.hashCode
                case Integer(bi)  => bi.hashCode // Still relies on BigInt hash
                case Symbol(a)    => hashA.hash(a)
              }
              loop(onto, 31 * acc + leafHash)
            case Operate(op, on) =>
              loop(on, 31 * acc + op.hashCode)
          }
          loop(s, 1)
        }

        @annotation.tailrec
        final def eqv(left: Stack[A], right: Stack[A]): Boolean =
          (left, right) match {
            case (Empty, Empty)                           => true
            case (Push(lleaf, lonto), Push(rleaf, ronto)) =>
              ((lleaf: Leaf[A]) === (rleaf: Leaf[A])) && eqv(lonto, ronto)
            case (Operate(lop, lon), Operate(rop, ron)) =>
              (lop == rop) && eqv(lon, ron)
            case _ => false
          }
      }

    implicit def stackOrder[A: Order]: Order[Stack[A]] =
      new Order[Stack[A]] {
        @annotation.tailrec
        final def compare(a: Stack[A], b: Stack[A]): Int =
          a match {
            case Empty =>
              b match {
                case Empty => 0
                case _     => -1
              }
            case Push(al, arest) =>
              b match {
                case Push(bl, brest) =>
                  val c0 = al match {
                    case Zero =>
                      bl match {
                        case Zero => 0
                        case _    => -1
                      }
                    case One =>
                      bl match {
                        case Zero => 1
                        case One  => 0
                        case _    => -1
                      }
                    case Integer(ai) =>
                      bl match {
                        case Zero | One  => 1
                        case Integer(bi) => ai.compare(bi)
                        case _           => -1
                      }
                    case Symbol(aa) =>
                      bl match {
                        case Symbol(ba) => Order[A].compare(aa, ba)
                        case _          => 1
                      }
                  }

                  if (c0 == 0) compare(arest, brest)
                  else c0
                case Empty         => 1
                case Operate(_, _) => -1
              }
            case Operate(ao, arest) =>
              b match {
                case Operate(bo, brest) =>
                  val c0 = java.lang.Integer.compare(ao.opId, bo.opId)
                  if (c0 == 0) compare(arest, brest)
                  else c0
                case Empty      => 1
                case Push(_, _) => 1
              }
          }
      }

    sealed trait Error[A]
    object Error {
      case class ResultStackNotSingleton[A](rest: List[A]) extends Error[A]
      case class ExpectedTwoOps[A](op: Op, stack: Option[A]) extends Error[A]
      case class ExpectedOneOp[A](op: Op) extends Error[A]
    }

    def toValue[A](
        s: Stack[A]
    )(implicit num: Numeric[A]): Either[Error[A], A] = {
      def toA(l: Leaf[A]): A =
        l match {
          case Symbol(a)  => a
          case Integer(a) => Expr.fromBigInt[A](a)
          case One        => num.one
          case Zero       => num.zero
        }

      @annotation.tailrec
      def loop(s: Stack[A], alist: List[A]): Either[Error[A], A] =
        s match {
          case Empty =>
            alist match {
              case a :: Nil  => Right(a)
              case notSingle => Left(Error.ResultStackNotSingleton(notSingle))
            }
          case Push(leaf, rest) =>
            loop(rest, toA(leaf) :: alist)
          case Operate(op, rest) =>
            op match {
              case Op.Add =>
                alist match {
                  case a :: b :: tail => loop(rest, num.plus(a, b) :: tail)
                  case zeroOrOne @ (Nil | (_ :: Nil)) =>
                    Left(Error.ExpectedTwoOps(Op.Add, zeroOrOne.headOption))
                }
              case Op.Mult =>
                alist match {
                  case a :: b :: tail => loop(rest, num.times(a, b) :: tail)
                  case zeroOrOne @ (Nil | (_ :: Nil)) =>
                    Left(Error.ExpectedTwoOps(Op.Mult, zeroOrOne.headOption))
                }
              case Op.Neg =>
                alist match {
                  case a :: tail => loop(rest, num.negate(a) :: tail)
                  case Nil       =>
                    Left(Error.ExpectedOneOp(Op.Neg))
                }
            }
        }

      loop(s, Nil)
    }

    def fromExpr[A](e: Expr[A]): Stack[A] = {
      @annotation.tailrec
      def loop(es: List[Either[Op, Expr[A]]], init: Stack[A]): Stack[A] =
        es match {
          case Right(leaf: Leaf[A]) :: tail =>
            loop(tail, Push(leaf, init))
          case Right(Add(left, right)) :: tail =>
            loop(Left(Op.Add) :: Right(right) :: Right(left) :: tail, init)
          case Right(Mult(left, right)) :: tail =>
            loop(Left(Op.Mult) :: Right(right) :: Right(left) :: tail, init)
          case Right(Neg(n)) :: tail =>
            loop(Left(Op.Neg) :: Right(n) :: tail, init)
          case Left(op) :: tail =>
            loop(tail, Operate(op, init))
          case Nil => init
        }

      loop(Right(e) :: Nil, Empty)
    }
  }

  // Heuristic cost model (make Mult much costlier than Add)
  final case class Weights(mult: Int, add: Int, neg: Int) {
    Require(add > 0, s"add = $add must be > 0")
    Require(mult > 0, s"mult = $mult must be > 0")
    Require(neg > 0, s"neg = $neg must be > 0")

    def costOf(opCounts: Expr.OpCounts): Long =
      (opCounts.mul.toLong * mult) +
        (opCounts.add.toLong * add) +
        (opCounts.neg.toLong * neg)

    def cost[A](e: Expr[A]): Long = {
      val counts = Expr.opCounts(e)
      (counts.mul.toLong * mult) +
        (counts.add.toLong * add) +
        (counts.neg.toLong * neg)
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

    def multIsBetter[A](
        expr: Expr[A],
        const: BigInt
    ): Boolean =
      if ((const == 0) || (const == 1) || (const == -1)) false
      else {
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
              (expr.maybeBigInt(_ => None).isDefined) || {
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
      }

    def constMult[A: Order](expr: Expr[A], const: BigInt): Expr[A] =
      expr.absorbMultConst(const).getOrElse {
        if (multIsBetter(expr, const)) {
          if (const < 0) {
            if ((const == -1) && (neg <= mult)) {
              expr.normalizeNeg
            } else
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
    implicit val showWeights: Show[Weights] = Show.fromToString
  }

  // === Public API ===
  def normalize[A: Hash: Order](
      e0: Expr[A],
      W: Weights = Weights.default
  ): Expr[A] = {
    // if we can't reduce the cost but keep producing different items stop
    // at 100 so we don't loop forever or blow up memory
    val MaxCount = 100
    @annotation.tailrec
    def loop(
        e: Expr[A],
        cost: Long,
        reached: HashSet[Expr[A]],
        cnt: Int
    ): Expr[A] =
      if (cnt >= MaxCount) {
        reached.iterator.min
      } else {
        val ne = normConstMult(norm(e, W), W).basicNorm
        val costNE = W.cost(ne)
        if (costNE < cost) {
          // we improved matters
          loop(ne, costNE, HashSet.empty[Expr[A]].add(ne), 0)
        } else if (costNE == cost) {
          // couldn't improve things, but maybe normalized them
          if (reached.contains(ne)) {
            // we have reached this, return the minimum in the set:
            reached.iterator.min
          } else {
            // we haven't seen this before, add it, and normalize again
            loop(ne, costNE, reached.add(ne), cnt + 1)
          }
        } else {
          // costNE > cost
          // TODO: we should never get here, increasing cost by normalization
          // means we probably are pretty far from optimal and we need to improve
          // our algorithm
          reached.iterator.min
        }
      }

    // The point of this is to reorder terms like (x * 2) * x
    // so that we don't have constant multipliers or additions hiding deep
    // inside. It helps with the repeatedAdd normalization in corner cases
    // but doesn't seem to frustrate undistribute much for the a*b + a*c
    // cases we want to work (although, probably some nesting of those doesn't)
    // work well, I would imagine
    // strip obvious top-level additive no-ops without flattening the full tree,
    // since flattening can hide useful factorization structure.
    val e =
      e0 match {
        case Add(x, y) => Expr.checkAdd[A](x, y)
        case other     => other
      }
    loop(e, W.cost(e), HashSet.empty[Expr[A]].add(e), 0)
  }

  // Small helper: canonical integer literal
  def canonInt[A](n: BigInt): Expr[A] =
    if (n == 0) Zero
    else if (n == 1) One
    else Integer(n)

  /** Possibly rewrite terms like 2*x into x + x, if it is better This is a
    * phase after factorization, but before sorting Mult/Add nodes
    */
  def normConstMult[A: Order](e: Expr[A], w: Weights): Expr[A] =
    e match {
      case Symbol(_) | One | Zero | Integer(_) => e
      case Neg(n)                              =>
        val nA: Expr[A] = n
        Neg(normConstMult[A](nA, w))
      case Add(x, y) =>
        val xA: Expr[A] = x
        val yA: Expr[A] = y
        Add(normConstMult[A](xA, w), normConstMult[A](yA, w))
      case Mult(Integer(n), Neg(e)) =>
        val eA: Expr[A] = e
        normConstMult[A](Mult(Integer(-n), eA), w)
      case Mult(Neg(e), Integer(n)) =>
        val eA: Expr[A] = e
        normConstMult[A](Mult(Integer(-n), eA), w)
      case Mult(Integer(n), leaf @ (Symbol(_) | One | Zero | Integer(_))) =>
        val leafA: Expr[A] = leaf
        w.constMult[A](leafA, n)
      case Mult(leaf @ (Symbol(_) | One | Zero | Integer(_)), Integer(n)) =>
        val leafA: Expr[A] = leaf
        w.constMult[A](leafA, n)
      case Mult(i @ Integer(n), add @ Add(x, y)) =>
        // we could do n * x + n * y
        val xA: Expr[A] = x
        val yA: Expr[A] = y
        val addA: Expr[A] = add
        val distribute = normConstMult[A](Add(Mult(i, xA), Mult(i, yA)), w)
        val costD = w.cost(distribute)
        // or we could treat the add as an atom
        val atom = w.constMult[A](normConstMult[A](addA, w), n)
        val costA = w.cost(atom)
        if (costD < costA) distribute
        else if (costD >= costA) atom
        else {
          // they are the same... sort
          if (Order[Expr[A]].lt(distribute, atom)) distribute
          else atom
        }

      case Mult(add @ Add(_, _), i @ Integer(_)) =>
        // reverse and loop
        val addA: Expr[A] = add
        normConstMult[A](Mult(i, addA), w)
      case Mult(x, y) =>
        val xA: Expr[A] = x
        val yA: Expr[A] = y
        val (const, factors) = Expr.flattenMult[A](xA :: yA :: Nil)
        if (factors.isEmpty) Integer(const)
        else {
          // we know factors isn't empty now
          def oneAndRest[T](ls: List[T]): List[(T, List[T])] =
            ls match {
              case Nil       => Nil
              case a :: tail =>
                (a, tail) :: oneAndRest(tail).map { case (o, r) =>
                  (o, a :: r)
                }
            }
          // recurse exactly once on each factor
          val normFactors: List[(MultTerm[A], Expr[A])] =
            factors.map(e => (e, normConstMult[A](e.toExpr, w)))
          // find the best one to push into
          val all: List[(Long, Expr[A])] =
            oneAndRest(normFactors).map { case ((target, _), rest) =>
              val normed =
                normConstMult[A](
                  w.constMult[A](target.toExpr, const),
                  w
                ) :: rest.map(_._2)
              val prod = Expr.multAll(normed)
              val costProd = w.cost(prod)
              (costProd, prod)
            }
          val (minCost, _) = all.minBy(_._1)
          all
            .filter { case (c, _) => c == minCost }
            .minBy { case (_, p) => (p.graphSize, p) }
            ._2
        }

    }

  case class GroupSum[A](const: BigInt, terms: MultiSet[AddTerm[A], BigInt])

  object GroupSum {
    implicit def showGroupSum[A: Show]: Show[GroupSum[A]] =
      new Show[GroupSum[A]] {
        def show(a: GroupSum[A]): String =
          show"GroupSum(${a.const}, ${a.terms})"
      }
    // return the sum such that all non-sum terms get a coefficient and we also
    // return any integer parts fully added up (as the first item in the tuple)
    // so: val (const, terms) = groupSum(es)
    // then terms.iterator.foldLeft(const) { case (acc, (e, n)) =>
    //   acc + n * e
    // }
    // is the same as the original sum
    def apply[A: Hash: Order](
        e: List[Expr[A]]
    ): GroupSum[A] = {
      @annotation.tailrec
      def loop(
          in: List[(BigInt, Expr[A])],
          res: MultiSet[AddTerm[A], BigInt],
          intRes: BigInt
      ): GroupSum[A] =
        in match {
          case (c, Add(x, y)) :: tail =>
            loop((c, x) :: (c, y) :: tail, res, intRes)
          case (c, Neg(x)) :: tail =>
            loop((-c, x) :: tail, res, intRes)
          case (n1, Integer(n)) :: tail =>
            loop(tail, res, intRes + (n1 * n))
          case (n1, One) :: tail =>
            loop(tail, res, intRes + n1)
          case (_, Zero) :: tail =>
            loop(tail, res, intRes)
          case (n1, sym @ Symbol(_)) :: tail =>
            loop(tail, res.add(sym, n1), intRes)
          case (c, m @ Mult(l, r)) :: tail =>
            if (m.isZero) loop(tail, res, intRes)
            else {
              m.saveNeg match {
                case Some(cn) =>
                  // If we can reduce costs of the mult, do so
                  // and bubble up the signs into c
                  loop((-c, cn) :: tail, res, intRes)
                case None =>
                  m.unConstMult match {
                    case Some((bi, inner)) =>
                      loop((c * bi, inner) :: tail, res, intRes)
                    case None =>
                      val lA: Expr[A] = l
                      val rA: Expr[A] = r
                      val (c1, prodTerms) = Expr.flattenMult[A](lA :: rA :: Nil)
                      prodTerms match {
                        case (add: Add[?]) :: Nil =>
                          // make sure we never add singleton Add(_, _)
                          val c2 = c * c1
                          loop(
                            (c2, add.left) :: (c2, add.right) :: tail,
                            res,
                            intRes
                          )
                        case _ =>
                          loop(tail, res.add(m, c), intRes)
                      }
                  }
                /*
                  loop(tail, res.add(m, c), intRes)
                 */
              }
            }
          case Nil => GroupSum(intRes, res)
        }

      loop(e.map((BigInt(1), _)), MultiSet.empty[AddTerm[A], BigInt], BigInt(0))
    }
  }

  // this represents a sum of products
  // const + sum_i(bi * pi)
  // Invariant: we never have just one Add(_, _) as a key
  case class SumProd[A](
      const: BigInt,
      terms: MultiSet[NonEmptyList[MultTerm[A]], BigInt]
  )(implicit ordA: Order[A]) {

    // If the constant is zero, and there is only one item in terms, this is actually a Mult Node
    def toMult: Option[Mult[A]] =
      if ((const == BigInt(0)) && (terms.hashMap.size == 1)) {
        val (mults, coeff) = terms.nonZeroIterator.next()
        /*
        we don't check this (but we do in property checks), but we never
        have a single Add(_, _) node by construction. This is maintained in Splits
        and in construction of SumProd

        mults match {
          case NonEmptyList(Add(_, _), Nil) =>
            sys.error(s"invariant violation toMult: $mults")
          case _ => ()
        }
         */
        val m = mults.iterator.map(_.toExpr).reduce(Mult(_, _))
        Some(Mult(m, Integer(coeff)))
      } else None

    def isZero: Boolean = (const == 0) && terms.isZero

    private def signOf(x: Expr[A], b: BigInt, w: Weights): Int = {
      val costPos = w.cost(x.bestEffortConstMult(b))
      val costNeg = w.cost(x.bestEffortConstMult(-b))
      if (costPos < costNeg) 1 else if (costNeg < costPos) -1 else 0
    }

    // what are all the ways we can factor by
    // an Integer or a MultTerm such that
    // (expr * a + b) == this
    // invariant: ((a == 1) && (b == 0)) == false and a != this, b != this
    def splits(implicit
        h: Hash[A]
    ): List[(Either[BigInt, MultTerm[A]], SumProd[A], SumProd[A])] = if (
      terms.isZero
    ) Nil
    else {

      val termSrcs
          : HashMap[MultTerm[A], NonEmptyList[NonEmptyList[MultTerm[A]]]] =
        HashMap.empty

      val allMts = terms.nonZeroIterator.foldLeft(termSrcs) {
        case (acc, (mts, _)) =>
          mts.foldLeft(acc) { case (acc, mt) =>
            acc.get(mt) match {
              case Some(existing) =>
                acc.updated(mt, mts :: existing)
              case None =>
                acc.updated(mt, NonEmptyList(mts, Nil))
            }
          }
      }
      val htK = terms.hashMap.hashKey

      val mtSplits = allMts.iterator
        .filter { case (_, NonEmptyList(_, tail)) =>
          // only consider terms that appear in two or more places
          tail.nonEmpty
        }
        .map { case (mt, keys) =>
          val newTermsList =
            terms.nonZeroIterator
              .filter { case (k, _) =>
                keys.exists(htK.eqv(k, _))
              }
              .map { case (k, b) =>
                // we have to remove the term from the NonEmptyList
                val ks = k.toList
                val idx = ks.indexWhere(kMt => Hash[MultTerm[A]].eqv(mt, kMt))
                val divList = ks.take(idx) ::: ks.drop(idx + 1)
                (NonEmptyList.fromList(divList), b)
              }
              .toList
          val divConst = newTermsList.collect { case (None, b) => b }.sum
          val divTermsList0 =
            MultiSet.fromListCount(newTermsList.collect {
              case (Some(k @ NonEmptyList(Symbol(_), _)), b) => (k, b)
              case (Some(k @ NonEmptyList(Add(_, _), tail)), b)
                  if tail.nonEmpty =>
                // we don't keep singleton Add nodes
                (k, b)
            })
          // All the singleton terms should be combined into a single add
          val divTermsCombine =
            newTermsList.collect {
              case (Some(NonEmptyList(add @ Add(_, _), Nil)), b) =>
                Mult(add, Integer(b))
            }
          val div = divTermsCombine match {
            case Nil      => SumProd(divConst, divTermsList0)
            case notEmpty =>
              // We want to lift all the Add(_, _) nodes out
              // so they allow factorizations across the adds
              val SumProd(cAdd, tAdd) = SumProd(notEmpty)
              SumProd(divConst + cAdd, divTermsList0 ++ tAdd)
          }
          val mod = SumProd(
            const,
            terms.filter { case (k, _) =>
              !keys.exists(htK.eqv(k, _))
            }
          )
          (Right(mt), div, mod)
        }
        .toList

      val biSrcs: Set[BigInt] = {
        val e = Set.empty[BigInt]
        if (const != 0 && const != -1 && const != 1) (e + const)
        else e
      }

      val allBis = terms.nonZeroIterator.foldLeft(biSrcs) {
        case (acc, (_, bi)) =>
          if ((bi == 1) || (bi == -1)) acc
          else {
            // add the positive and negative because Neg is generally very cheap
            acc + bi + (-bi)
          }
      }

      val biSplits = allBis.iterator.flatMap { case bi =>
        val divTerms =
          terms.nonZeroIterator
            .flatMap { case (k, v) =>
              val (d, m) = v /% bi
              if (m == 0) ((k, d) :: Nil)
              else Nil
            }
            .foldLeft(MultiSet.empty[NonEmptyList[MultTerm[A]], BigInt]) {
              case (acc, (k, v)) =>
                acc.add(k, v)
            }

        if (divTerms.isZero) Nil
        else {
          val modTerms = terms.filter { case (_, v) => (v % bi) != 0 }
          val (cd, cm) =
            // if (modTerms.hashMap.nonEmpty) (const /% bi)
            // else (BigInt(0), const)
            (BigInt(0), const)

          val div = SumProd(cd, divTerms)
          val mod = SumProd(cm, modTerms)
          (Left(bi), div, mod) :: Nil
        }
      }.toList

      biSplits ::: mtSplits
    }

    // This is *not* normalized and just the direct conversion of the current
    // expression into an Expr
    val directToExpr: Expr[A] = {
      val e0: Expr[A] = canonInt(const)
      // We sometimes return this term directly so we need to make
      // sure it is ordered
      val orderedTerms = terms.nonZeroIterator.toList.sorted
      orderedTerms.foldLeft(e0) { case (acc, (ms, c)) =>
        val prod =
          ms.map(_.toExpr).reduceLeft(Expr.checkMult)

        val prodC = prod.bestEffortConstMult(c)
        Expr.checkAdd(acc, prodC)
      }
    }

    def normalize(w: Weights)(implicit h: Hash[A]): Expr[A] =
      toMult match {
        case Some(m) =>
          // Sometimes we can reduce SumProd into just a product,
          // so to normalize it, use the normal path
          RingOpt.norm(m, w)
        case None => {
          val withCost = splits.map {
            case (right @ Right(mt), div, mod) =>
              val e1 =
                Expr.addAll(
                  Expr.checkMult(mt.toExpr, div.directToExpr) ::
                    mod.directToExpr ::
                    Nil
                )

              (right, div, mod, e1, w.cost(e1))
            case (left @ Left(bi), div, mod) =>
              val e1 =
                Expr.addAll(
                  div.directToExpr.bestEffortConstMult(bi) ::
                    mod.directToExpr ::
                    Nil
                )

              (left, div, mod, e1, w.cost(e1))
          }

          val nextSteps =
            if (withCost.isEmpty) Nil
            else {
              val thisCost = w.cost(directToExpr)
              withCost
                .filter { case (_, _, _, _, c) => c <= thisCost }
            }

          if (nextSteps.isEmpty) {
            // None of the steps are as good as the current state
            val (neg0, unsigned, pos0) = {
              val termsList = terms.nonZeroIterator.map { case (factors, bi) =>
                val normFactors =
                  Expr.multAll(factors.toList.map(mt => norm(mt.toExpr, w)))
                (normFactors, bi)
              }.toList

              val label = termsList.map { case kb @ (k, bi) =>
                (kb, signOf(k, bi, w))
              }
              (
                label.collect { case (kb, x) if x < 0 => kb },
                label.collect { case (kb, x) if x == 0 => kb },
                label.collect { case (kb, x) if x > 0 => kb }
              )
            }
            // Helper to build a sum from a list of terms.
            // If signFlip is true, it builds Sum( k*(-b) ) for each (k,b)
            // If signFlip is false, it builds Sum( k*b ) for each (k,b)
            def buildSum(
                termList: List[(Expr[A], BigInt)],
                signFlip: Boolean
            ): Expr[A] =
              Expr.addAll(
                termList
                  .map { case (mult, bi) =>
                    val coeff = if (signFlip) -bi else bi
                    mult.bestEffortConstMult(coeff)
                  }
              )

            // --- Option 1: Positive-dominant form: (Const + Pos + Unsigned) - (Neg) ---
            // 'pos0' and 'unsigned' terms are built as (k*b)
            val posSum1 = buildSum(pos0 ::: unsigned, signFlip = false)
            // 'neg0' terms are built as (k*(-b))
            val negSum1 = buildSum(neg0, signFlip = true)

            val finalExprPos = Expr.addAll(
              canonInt(const) ::
                posSum1 ::
                negSum1.normalizeNeg :: // This creates the subtraction
                Nil
            )
            val costPos = w.cost(finalExprPos)

            // --- Option 2: Negative-dominant form: -(( -Const + Neg + Unsigned) - (Pos)) ---
            // 'neg0' and 'unsigned' terms are built as (k*(-b))
            val negSum2 = buildSum(neg0 ::: unsigned, signFlip = true)
            // 'pos0' terms are built as (k*b)
            val posSum2 = buildSum(pos0, signFlip = false)

            val finalExprNeg = Expr
              .addAll(
                canonInt(-const) ::
                  negSum2 ::
                  posSum2.normalizeNeg :: // This creates the subtraction
                  Nil
              )
              .normalizeNeg // The outer negation
            val costNeg = w.cost(finalExprNeg)

            // --- Option 3: Don't flip any signs
            val finalDefault = Expr.addAll(
              canonInt(const) ::
                buildSum(pos0 ::: unsigned ::: neg0, signFlip = false) ::
                Nil
            )
            val costDefault = w.cost(finalDefault)

            val all = (costPos, finalExprPos) :: (costNeg, finalExprNeg) :: (
              costDefault,
              finalDefault
            ) :: Nil

            val minCost = all.iterator.map(_._1).min
            val (_, best) = all.filter { case (c, _) => c == minCost }.minBy {
              case (_, e) => e
            }
            best

          } else {
            val (e, div, mod, _, _) = nextSteps.minBy {
              case (_, _, mod, e1, c) =>
                // try to choose the smallest cost, breaking ties with the smallest
                // left over set, then just the usual sorting
                (c, mod.terms.size, e1)
            }

            val normDiv = div.normalize(w)
            val normMod = mod.normalize(w)
            // e * div + mod
            // but just because e does not divide mod, doesn't mean there
            // are no terms in mod that could divide div
            e match {
              case Right(e) =>
                val left = Expr.checkMult(norm(e.toExpr, w), normDiv)
                Expr.addAll(left :: normMod :: Nil)
              case Left(bi) =>
                // num * normDiv + normMod
                // but we could do:
                // Neg((-num) * normDiv + (-normMod))
                // if it is better
                val pos =
                  Expr.addAll(normDiv.bestEffortConstMult(bi) :: normMod :: Nil)
                val neg =
                  Expr
                    .addAll(
                      normDiv.bestEffortConstMult(
                        -bi
                      ) :: normMod.normalizeNeg :: Nil
                    )
                    .normalizeNeg

                if (w.cost(pos) > w.cost(neg)) neg
                else pos
            }
          }
        }
      }
  }

  object SumProd {
    implicit def showSumProd[A: Show]: Show[SumProd[A]] =
      new Show[SumProd[A]] {
        def show(sp: SumProd[A]): String =
          show"SumProd(${sp.const}, ${sp.terms})"
      }

    def apply[A: Hash: Order](lst: List[Expr[A]]): SumProd[A] =
      fromGroupSum(GroupSum(lst))

    def fromGroupSum[A: Hash: Order](gs: GroupSum[A]): SumProd[A] = {
      val terms0 = MultiSet.empty[NonEmptyList[MultTerm[A]], BigInt]
      val (c1, t1) = gs.terms.nonZeroIterator.foldLeft((gs.const, terms0)) {
        case ((accConst, acc), (e, c)) =>
          val (c0, flatMult) = Expr.flattenMult(e.toExpr :: Nil)
          NonEmptyList.fromList(flatMult) match {
            case None =>
              // this is another constant 1
              (accConst + c0 * c, acc)
            case Some(nel) =>
              // this is a product of Symbol and Add(_, _) nodes
              (accConst, acc.add(nel, c0 * c))
          }
      }
      SumProd(c1, t1)
    }
  }

  // === Core normalization ===

  private def norm[A: Hash: Order](e: Expr[A], W: Weights): Expr[A] = {
    val u = Expr.undistribute[A](e)
    u match {
      case Zero | One | Symbol(_) => u

      case Integer(n) => canonInt[A](n)

      case Neg(Neg(x)) =>
        val xA: Expr[A] = x
        norm[A](xA, W)
      case Neg(Add(left, right)) =>
        val leftA: Expr[A] = left
        val rightA: Expr[A] = right
        val GroupSum(const, terms) = GroupSum[A](leftA :: rightA :: Nil)
        val negGs = GroupSum[A](-const, terms.negate)
        val sumProd = SumProd.fromGroupSum[A](negGs)
        sumProd.normalize(W)

      case Neg(mult @ Mult(_, _)) =>
        val multA: Expr[A] = mult
        norm[A](Neg(One) * multA, W)
      case ns @ Neg(Symbol(_)) => ns
      case Neg(Zero)           => Zero
      case Neg(One)            => canonInt(-1)
      case Neg(Integer(n))     => canonInt(-n)

      case Add(left, right) =>
        val leftA: Expr[A] = left
        val rightA: Expr[A] = right
        val sumProd = SumProd[A](leftA :: rightA :: Nil).normalize(W)

        // Preserve repeated-add structure when it is cheaper than term-wise
        // decomposition. This avoids losing opportunities like 2 * (x + y).
        if (Hash[Expr[A]].eqv(leftA, rightA)) {
          val doubled = Expr.checkMult(Integer(2), leftA)
          val costDouble = W.cost(doubled)
          val costSumProd = W.cost(sumProd)
          if (costDouble < costSumProd) doubled
          else if (
            costDouble == costSumProd && Order[Expr[A]].lt(doubled, sumProd)
          )
            doubled
          else sumProd
        } else sumProd

      case Mult(a, b) =>
        val aA: Expr[A] = a
        val bA: Expr[A] = b
        val (const, factors) = Expr.flattenMult[A](aA :: bA :: Nil)
        val normFactors =
          Expr.multAll(factors.map(e => norm[A](e.toExpr, W)))
        normFactors.bestEffortConstMult(const)
    }
  }
}
