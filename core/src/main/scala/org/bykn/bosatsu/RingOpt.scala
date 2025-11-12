package org.bykn.bosatsu

import cats.{Hash, Show}
import cats.collections.{HashMap, HashSet}
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.math.Numeric
import scala.util.hashing.MurmurHash3

object RingOpt {

  // This is somehow not found in cats...
  implicit private def hashNEL[A: Hash]: Hash[NonEmptyList[A]] =
    Hash[List[A]].contramap[NonEmptyList[A]](_.toList)

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
    // This does basic normalization, like ordering Add(_, _) and Mult(_, _)
    // and collapsing constants, we do this before doing harder optimizations
    // since we have better equality at that point
    def basicNorm: Expr[A] = {
      def add(a: Expr[A], b: Expr[A]) =
        if (a == Zero) b
        else if (b == Zero) a
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
          else if (Expr.key(a) <= Expr.key(b)) Add(a, b)
          else Add(b, a)
        }

      def mult(a: Expr[A], b: Expr[A]) =
        if (Expr.key(a) <= Expr.key(b)) Mult(a, b)
        else Mult(b, a)

      def simpleNeg(e: Expr[A]): Expr[A] =
        // normalizeNeg isn't idempotent
        e.normalizeNeg match {
          case Neg(x) => Neg(x.basicNorm)
          case notNeg =>
            // the negative was pushed down, normalize again
            notNeg.basicNorm
        }

      expr match {
        case Zero | One | Symbol(_) => expr
        case Integer(i)             => canonInt(i)
        case Neg(Neg(x))            => x.basicNorm
        case Neg(x)                 => simpleNeg(x.basicNorm)
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
                notAdd.basicNorm match {
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
                    acc.sortBy(Expr.key(_)).reduceRight(step(_, _)),
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
                    flatMult(negAdd.basicNorm :: tail, -coeff, acc)
                  case None =>
                    flatMult(tail, coeff, add :: acc)
                }
              case Nil =>
                if (acc.isEmpty) canonInt(coeff)
                else
                  step(
                    acc.sortBy(Expr.key(_)).reduceRight(step(_, _)),
                    canonInt(coeff)
                  )
            }

          flatMult(x.basicNorm :: y.basicNorm :: Nil, BigInt(1), Nil)
      }
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
          // multiplication by zero can shield unknown symbols
          x.maybeBigInt(onSym) match {
            case sx @ Some(bx) =>
              if (bx == 0) sx
              else {
                y.maybeBigInt(onSym) match {
                  case Some(by) => Some(bx * by)
                  case None     => None
                }
              }
            case None =>
              y.maybeBigInt(onSym) match {
                case s @ Some(by) if by == 0 => s
                case _                       => None
              }
          }
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

    /** the laws here are:
      *   1. Mult(Integer(i), x) == this
      *   2. cost(Mult(Integer(i), x)) <= cost(this) || x.isOne
      *   3. this.unConstMult.flatMap { case (_, e) => e.unConstMult } == None ||
      *      (this.isZero) (we pull all the ints out once)
      *   4. if we return zero, both the integer and the Expr are zero.
      *   5. we never return 1 or -1 as the multiplier
      *   6. the expression returned is never Integer(_) or Neg(_)
      */
    def unConstMult: Option[(BigInt, Expr[A])] = {
      def loop(expr: Expr[A], insideMult: Boolean): Option[(BigInt, Expr[A])] =
        expr match {
          case Symbol(_) => None
          case One       =>
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
                  val i = if (m == Zero) BigInt(0) else n1
                  Some((i, m))
                case (None, Some((n1, y1))) =>
                  val m = Expr.checkMult(x, y1)
                  val i = if (m == Zero) BigInt(0) else n1
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
          case res @ Some((bi, inner)) =>
            if (bi == 0) res
            else {
              /*
              implicit val showA: Show[A] = Show.fromToString
              println(show"e=$e, bi=$bi, inner=$inner")
              assert((bi != 1) && (bi != -1))
               */
              fix(bi * prod, inner)
            }
          case None =>
            if (prod == 1) None
            else Some((prod, e))
        }

      fix(BigInt(1), expr)
    }

    // try first to absorb, then to negate, only then do we use Mult
    def bestEffortConstMult(c: BigInt): Expr[A] =
      absorbMultConst(c) match {
        case Some(res) => res
        case None      =>
          // negation should always be cheaper than multiplication
          if (c == -1) normalizeNeg
          else {
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
    // if we can implement this by absorbing a multiplication by a constant
    // do it. The cost of the returned expression is <= original
    def absorbMultConst(c: BigInt): Option[Expr[A]] =
      // check zeros first
      if (c == 0) Some(Zero)
      else if (isZero) Some(Zero)
      else if (c == 1) Some(this)
      else if (c == -1) cheapNeg
      else if (isOne) Some(canonInt(c))
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
          expr.bestEffortConstMult(bi) :: acc
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

      val GroupSum(intRes, res) = GroupSum(e)
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
    def flattenMult[A](e: List[Expr[A]]): (BigInt, List[MultTerm[A]]) = {
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
              add.saveNeg match {
                case Some(n) =>
                  // we can save by negating
                  loop(n :: tail, result, -ints)
                case None =>
                  // we can't save but maybe still 0 cost negate
                  add.cheapNeg match {
                    case Some(n) if Expr.key(n) < Expr.key(add) =>
                      // x + (-y) and y + (-x) may have the same cost
                      // and we can flip them back and forth with zero cost
                      // if so, choose the one with the smaller key
                      loop(n :: tail, result, -ints)
                    case _ =>
                      loop(tail, add :: result, ints)
                  }
              }
            }
          case Nil => (ints, result.sortBy(e => Expr.key(e.toExpr)))
        }

      loop(e, Nil, BigInt(1))
    }

    def multAll[A](items: List[Expr[A]]): Expr[A] = {
      @annotation.tailrec
      def loop(stack: List[MultTerm[A]], ints: BigInt, acc: Expr[A]): Expr[A] =
        stack match {
          case Nil =>
            val ix = canonInt(ints)
            checkMult(ix, acc)
          case (others @ (Symbol(_) | Add(_, _))) :: tail =>
            loop(tail, ints, checkMult(others.toExpr, acc))
        }

      val (optInt, flat) = flattenMult(sortExpr(items))
      loop(flat, optInt, One)
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

  // These are Symbol(_) or Add(_, _)
  sealed trait MultTerm[+A] {
    def toExpr: Expr[A]
  }
  object MultTerm {
    implicit def hashMultTerm[A: Hash]: Hash[MultTerm[A]] =
      Hash[Expr[A]].contramap[MultTerm[A]](_.toExpr)

    implicit def showMultTerm[A: Show]: Show[MultTerm[A]] =
      Show[Expr[A]].contramap[MultTerm[A]](_.toExpr)
  }

  case object Zero extends Expr[Nothing]
  case object One extends Expr[Nothing]
  final case class Integer(toBigInt: BigInt) extends Expr[Nothing]
  final case class Symbol[A](item: A)
      extends Expr[A]
      with AddTerm[A]
      with MultTerm[A] {
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
  }

  // === Public API ===
  def normalize[A: Hash](e: Expr[A], W: Weights = Weights.default): Expr[A] = {
    // val eInit = e
    // if we can't reduce the cost but keep producing different items stop
    // at 100 so we don't loop forever or blow up memory
    val MaxCount = 100
    @annotation.tailrec
    def loop(
        e0: Expr[A],
        cost: Long,
        reached: HashSet[Expr[A]],
        cnt: Int
    ): Expr[A] =
      if (cnt >= MaxCount) {
        reached.iterator.minBy(Expr.key(_))
      } else {
        val e = e0.basicNorm
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

    loop(e, W.cost(e), HashSet.empty[Expr[A]].add(e), 0).basicNorm
  }

  // Small helper: canonical integer literal
  def canonInt[A](n: BigInt): Expr[A] =
    if (n == 0) Zero
    else if (n == 1) One
    else Integer(n)

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
    def apply[A: Hash](
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
          case (c, m @ Mult(_, _)) :: tail =>
            if (m.isZero) loop(tail, res, intRes)
            else {
              m.saveNeg match {
                case Some(cn) =>
                  // If we can reduce costs of the mult, do so
                  // and bubble up the signs into c
                  loop((-c, cn) :: tail, res, intRes)
                case None =>
                  /*
                We have some choices here: we could try to avoid
                negation with cheapNeg, but also we could pull
                integers out with unConstMult. But also,
                how many times do different factors show up?
                If we go too crazy now, that defeats the purpose
                of optSumProd...

                one challenge with using unConstMult here is that
                it frustrates the flattenAddSub + addAll not increasing
                costs law... since multiplying can be worse than adding
                for small adds. We need to have a way to unroll that correctly
                if we are going to use unConstMult
              m.unConstMult match {
                case Some((bi, inner)) =>
                  loop((c * bi, inner) :: tail, res, intRes)
                case None =>
                  loop(tail, res.add(m, c), intRes)
              }
                   */
                  loop(tail, res.add(m, c), intRes)
              }
            }
          case Nil => GroupSum(intRes, res)
        }

      loop(e.map((BigInt(1), _)), MultiSet.empty[AddTerm[A], BigInt], BigInt(0))
    }
  }

  // this represents a sum of products
  // const + sum_i(bi * pi)
  case class SumProd[A](
      const: BigInt,
      terms: MultiSet[NonEmptyList[MultTerm[A]], BigInt]
  ) {
    def morePositive: Boolean = {
      val it = terms.nonZeroIterator
      var pos = 0
      var neg = 0
      while (it.hasNext) {
        val (_, cnt) = it.next()
        if (cnt == 1) {
          pos = pos + 1
        } else if (cnt == -1) {
          neg = neg + 1
        }
      }

      return (pos >= neg)
    }
    // what are all the ways we can factor by
    // an Integer or a MultTerm such that
    // (expr * a + b) == this
    // invariant: ((a == 1) && (b == 0)) == false and a != this, b != this
    def splits(implicit
        h: Hash[A]
    ): List[(Either[BigInt, MultTerm[A]], SumProd[A], SumProd[A])] = {

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
                (
                  NonEmptyList
                    .fromList(ks.take(idx) ::: ks.drop(idx + 1)),
                  b
                )
              }
              .toList
          val divConst = newTermsList.collect { case (None, b) => b }.sum
          val divTermsList0 =
            newTermsList.collect {
              case (Some(k @ NonEmptyList(_, tail)), b) if tail.nonEmpty =>
                (k, b)
            }
          // All the singleton terms should be combined into a single add
          val divTermsCombine =
            newTermsList.collect { case (Some(NonEmptyList(k, Nil)), b) =>
              (k, b)
            }
          val arg = divTermsCombine match {
            case Nil           => divTermsList0
            case (k, b) :: Nil =>
              // just one
              (NonEmptyList(k, Nil), b) :: divTermsList0
            case twoOrMore =>
              // we can combine this into a single MultTerm
              val coeff = twoOrMore.map(_._2).reduce(_.gcd(_))
              val (k, c) = twoOrMore.iterator
                .map { case (k, b) => (k, b / coeff) }
                .reduce[(MultTerm[A], BigInt)] { case ((k1, b1), (k2, b2)) =>
                  (
                    Add(
                      k1.toExpr.bestEffortConstMult(b1),
                      k2.toExpr.bestEffortConstMult(b2)
                    ),
                    BigInt(1)
                  )
                }

              (NonEmptyList(k, Nil), c * coeff) :: divTermsList0
          }
          val divTerms = MultiSet.fromListCount(arg)
          val div = SumProd(
            divConst,
            divTerms
          )
          val mod = SumProd(
            const,
            terms.filter { case (k, _) =>
              !keys.exists(htK.eqv(k, _))
            }
          )
          (Right(mt), div, mod)
        }
        .toList

      val biSrcs: HashMap[BigInt, NonEmptyList[NonEmptyList[MultTerm[A]]]] =
        HashMap.empty

      val allBis = terms.nonZeroIterator.foldLeft(biSrcs) {
        case (acc, (_, bi)) if (bi == 1) || (bi == -1) => acc
        case (acc, (mts, bi))                          =>
          // add the positive and negative because Neg is generally very cheap
          (bi :: -bi :: Nil).foldLeft(acc) { case (acc, bi) =>

            acc.get(bi) match {
              case Some(existing) =>
                acc.updated(bi, mts :: existing)
              case None =>
                acc.updated(bi, NonEmptyList(mts, Nil))
            }
          }
      }

      val biSplits = allBis.iterator
        .filter { case (_, NonEmptyList(_, tail)) =>
          // only consider terms that appear in two or more places
          tail.nonEmpty
        }
        .map { case (bi, _) =>
          val (cd, cm) = const /% bi
          val div = SumProd(
            cd, {
              implicit val hashNEL: Hash[NonEmptyList[MultTerm[A]]] =
                Hash[List[MultTerm[A]]]
                  .contramap[NonEmptyList[MultTerm[A]]](_.toList)

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
            }
          )
          val mod = SumProd(
            cm,
            terms.filter { case (_, v) => (v % bi) != 0 }
          )
          (Left(bi), div, mod)
        }
        .toList

      biSplits ::: mtSplits
    }

    // This is *not* normalized and just the direct conversion of the current
    // expression into an Expr
    def directToExpr(w: Weights): Expr[A] = {
      val e0: Expr[A] = canonInt(const)
      terms.nonZeroIterator.foldLeft(e0) { case (acc, (ms, c)) =>
        val prod =
          ms.map(_.toExpr).reduce[Expr[A]]((acc, m) => Expr.checkMult(acc, m))

        val prodC = w.constMult(prod, c)
        Expr.checkAdd(acc, prodC)
      }
    }

    def normalize(w: Weights)(implicit h: Hash[A]): Expr[A] = {
      // if splits is empty, we never use this
      lazy val thisExpr = directToExpr(w)
      lazy val thisCost = w.cost(thisExpr)

      val withCost = splits.map {
        case (right @ Right(mt), div, mod) =>
          val e1 =
            Expr.checkAdd(
              Expr.checkMult(mt.toExpr, div.directToExpr(w)),
              mod.directToExpr(w)
            )

          (right, div, mod, e1, w.cost(e1))
        case (left @ Left(bi), div, mod) =>
          val e1 =
            Expr.checkAdd(
              w.constMult(div.directToExpr(w), bi),
              mod.directToExpr(w)
            )

          (left, div, mod, e1, w.cost(e1))
      }
      // implicit val showA: Show[A] = Show.fromToString
      // println(show"this=$this")
      // println(show"thisCost = $thisCost, withCost=$withCost")
      val nextSteps = withCost
        .filter { case (_, _, _, _, c) => c <= thisCost }

      if (nextSteps.isEmpty) {
        // None of the steps are as good as the current state
        // println(show"nextSteps is empty: $this")

        val (biFn, trans) =
          if (morePositive) {
            ({ (bi: BigInt) => bi }, { (e: Expr[A]) => e })
          } else {
            ({ (bi: BigInt) => -bi }, { (e: Expr[A]) => e.normalizeNeg })
          }

        val constE: Expr[A] = Integer(biFn(const))
        val nonNeg = terms.nonZeroIterator
          .map { case (k, bi) => (k, biFn(bi)) }
          .collect {
            case (mult, bi) if bi != -1 => normMult(bi, mult.toList, w)
          }
          .toList
          .sortBy(Expr.key(_))

        val negatives = terms.nonZeroIterator
          .map { case (k, bi) => (k, biFn(bi)) }
          .collect {
            case (mult, bi) if bi == -1 => normMult(BigInt(1), mult.toList, w)
          }
          .toList
          .sortBy(Expr.key(_))

        val pos = nonNeg.foldLeft(constE)(Expr.checkAdd(_, _))
        val neg =
          negatives.foldLeft(Zero: Expr[A])(Expr.checkAdd(_, _)).normalizeNeg

        trans(Expr.checkAdd(pos, neg))
      } else {
        val (e, div, mod, _, _) = nextSteps.minBy { case (_, _, _, e1, c) =>
          (c, Expr.key(e1))
        }
        val normDiv = div.normalize(w)
        val normMod = mod.normalize(w)
        // println(show"nextSteps not empty: $this\n\te=$e, div = $div, mod = $mod")
        // e * div + mod
        // but just because e does not divide mod, doesn't mean there
        // are no terms in mod that could divide div
        e match {
          case Right(e) =>
            Expr.checkAdd(Expr.checkMult(norm(e.toExpr, w), normDiv), normMod)
          case Left(bi) =>
            Expr.checkAdd(w.constMult(normDiv, bi), normMod)
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

    def apply[A: Hash](lst: List[Expr[A]]): SumProd[A] =
      fromGroupSum(GroupSum(lst))

    def fromGroupSum[A: Hash](gs: GroupSum[A]): SumProd[A] = {
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

  private def norm[A: Hash](e: Expr[A], W: Weights): Expr[A] = e match {
    case Zero | One | Symbol(_) => e

    case Integer(n) => canonInt[A](n)

    case Neg(Neg(x))           => norm(x, W)
    case Neg(Add(left, right)) =>
      val GroupSum(const, terms) = GroupSum(left :: right :: Nil)
      val negGs = GroupSum(-const, terms.negate)
      val sumProd = SumProd.fromGroupSum(negGs)
      sumProd.normalize(W)

    case Neg(mult @ Mult(_, _)) => norm[A](Neg(One) * mult, W)
    case ns @ Neg(Symbol(_))    => ns
    case Neg(Zero)              => Zero
    case Neg(One)               => canonInt(-1)
    case Neg(Integer(n))        => canonInt(-n)

    case Add(left, right) =>
      val gs = GroupSum(left :: right :: Nil)
      val sumProd = SumProd.fromGroupSum(gs)
      val res = sumProd.normalize(W)
      // implicit val showA: Show[A] = Show.fromToString
      // println(show"e = $e, gs = $gs, sumProd = $sumProd, res = $res")
      res

    case Mult(a, b) =>
      val (optConst, factors) = Expr.flattenMult(a :: b :: Nil)
      normMult(optConst, factors, W)
  }

  // Normalize multiplication list: fold integer factors and signs; sort other factors
  private def normMult[A: Hash](
      const: BigInt,
      factors: List[MultTerm[A]],
      W: Weights
  ): Expr[A] = {
    val normFactors = Expr.multAll(factors.map(mt => norm(mt.toExpr, W)))
    W.constMult(normFactors, const)
  }
}
