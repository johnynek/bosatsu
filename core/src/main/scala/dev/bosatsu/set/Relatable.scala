package dev.bosatsu.set

import cats.Eq
import cats.syntax.eq._

trait Relatable[A] {
  def relate(left: A, right: A): Rel
}

object Relatable {
  def apply[A](implicit r: Relatable[A]): Relatable[A] = r

  def fromSubsetIntersects[A](
      // True when all elements of left are in right
      subset: (A, A) => Boolean,
      // true when there exists 1 or more element in both
      intersects: (A, A) => Boolean
  ): Relatable[A] =
    new Relatable[A] {
      def relate(left: A, right: A): Rel = {
        val leftSub = subset(left, right)
        val rightSub = subset(right, left)
        if (leftSub) {
          if (rightSub) Rel.Same
          else Rel.Sub
        } else if (rightSub) Rel.Super
        else if (intersects(left, right)) Rel.Intersects
        else Rel.Disjoint
      }
    }
  def setRelatable[A]: Relatable[Set[A]] =
    fromSubsetIntersects(
      _.subsetOf(_),
      (s1, s2) => {
        if (s1.size <= s2.size) s1.exists(s2)
        else s2.exists(s1)
      }
    )

  def fromUniversalEquals[A]: Relatable[A] =
    new Relatable[A] {
      private given Eq[A] = Eq.fromUniversalEquals
      def relate(i: A, j: A) =
        if (i === j) Rel.Same else Rel.Disjoint
    }

  /** Make a relatable where unions are represented by Lists. we need three
    * functions:
    *   1. is a value A empty 2. compute the intersection of two values 3. given
    *      A, either split it in two, or give a function to see if a ==
    *      union(as) (given a >= union(as))
    */
  def listUnion[A: Relatable](
      isEmptyFn: A => Boolean,
      intersectFn: (A, A) => List[A],
      solveOne: A => Either[List[A] => Boolean, (A, A)]
  ): Relatable[List[A]] =
    new Relatable[List[A]] { self =>
      val unionRelMod: UnionRelModule[List[A]] =
        new UnionRelModule[List[A]] {
          def relatable: Relatable[List[A]] = self
          def isEmpty(ls: List[A]) = ls.forall(isEmptyFn)
          def deunion(ls: List[A]) =
            ls.size match {
              // $COVERAGE-OFF$
              case 0 => sys.error("invariant violation: deunion(Nil)")
              // $COVERAGE-ON$
              case 1 =>
                solveOne(ls.head) match {
                  case Left(equ) =>
                    Left { (b, c) =>
                      // we have to see if ls.head > (b | c) or == (b | c)
                      if (equ(b ::: c)) Rel.Same
                      else Rel.Super
                    }
                  case Right((a1, a2)) =>
                    Right((a1 :: Nil, a2 :: Nil))
                }
              case sz =>
                Right((ls.splitAt(sz / 2)))
            }

          def cheapUnion(head: List[A], tail: List[List[A]]) =
            (head :: tail).flatten.distinct

          def intersect(a: List[A], b: List[A]): List[A] =
            if (a.isEmpty || b.isEmpty) Nil
            else
              for {
                ai <- a
                bi <- b
                i <- intersectFn(ai, bi)
              } yield i
        }

      def relate(left: List[A], right: List[A]): Rel =
        (left, right) match {
          case (Nil, r) =>
            if (unionRelMod.isEmpty(r)) Rel.Same
            else Rel.Sub
          case (l, Nil) =>
            if (unionRelMod.isEmpty(l)) Rel.Same
            else Rel.Super
          case (a :: Nil, b :: Nil) =>
            // just one on both sides:
            Relatable[A].relate(a, b)
          case (_, _ :: Nil) =>
            val (ll, lr) = left.splitAt(left.size / 2)
            // if we have only one item on the right, move it to the left
            unionRelMod.unionRelCompare(right, ll, lr).invert
          case (l, _) =>
            // there are at least two items on the right
            val (rl, rr) = right.splitAt(right.size / 2)
            unionRelMod.unionRelCompare(l, rl, rr)
        }
    }

  /** unionCompare compares a <:> (b | c)
    *
    * It can give a Rel or a PartialRel as a result Note: we always evaluate a
    * <:> b so if you can choose, that should be the simpler value to check
    *
    * Important: b and c cannot be bottom values. They cannot be empty.
    */
  private def unionRelCompare1[A: Relatable](
      a: A,
      b: A,
      c: A
  ): Either[PartialRel, Rel] = {
    import Rel._
    import PartialRel._

    inline def ac = Relatable[A].relate(a, c)
    Relatable[A].relate(a, b) match {
      case Sub =>
        Right(Sub) // (a < b), then a < (b|c)
      case Same =>
        ac match { // a=b, so b|c = a|c, so a <= b|c
          case Sub => Right(Sub) // (a=b) then b|c = a|c, which == a, if c == a.
          case Same       => Right(Same) // a = b = c
          case Super      => Right(Same) // (a=b), a > c. So, b|c = a|c = a
          case Intersects =>
            Right(Sub) // a=b, a n c. b|c = a|c which is bigger than a
          case Disjoint => Right(Sub) // a=b, so b|c = a|c
        }
      case Super =>
        ac match {
          case Sub   => Right(Sub) // a < c, so a < (b|c)
          case Same  => Right(Same) // a = c, a > b. b|c = b|a, and a > b
          case Super =>
            Left(SuperSame) // if a > b, a > c, then a > (b|c) or a = (b|c)
          case Intersects =>
            // a > b, c has some outside a, but b could cover all not in c so
            // a < b|c or a n b|c, Sub or Intersect.
            Left(SubIntersects)
          case Disjoint =>
            Right(
              Intersects
            ) // a > b, a ! c, all of c is outside a, but all b inside
        }
      case Intersects =>
        ac match {
          case Sub   => Right(Sub) // a < c so a < (b|c)
          case Same  => Right(Sub) // a n b, a = c. b|c = b|a, so a < b|a
          case Super =>
            // a > c, b has some outside a, but c could cover all not in b so
            // a < b|c or a n b|c, Sub or Intersect.
            Left(SubIntersects)
          case Intersects =>
            Left(SubIntersects) // a n b, a n c, so a < (b|c) or a n (b|c).
          case Disjoint => Right(Intersects) // a n b, a ! c, b|c
        }
      case Disjoint =>
        ac match {
          case Sub        => Right(Sub)
          case Same       => Right(Sub)
          case Super      => Right(Intersects)
          case Intersects => Right(Intersects)
          case Disjoint   => Right(Disjoint)
        }
    }
  }

  abstract class UnionRelModule[A] {
    import Rel._
    import PartialRel._

    def relatable: Relatable[A]

    /** Either deunion *non-empty* a into two non-empty values or return a
      * function that solves the problem of a <:> (b | c) where we know for sure
      * that the answer is either Super or Same which is to say, we know that a
      * >= (b | c)
      */
    def deunion(a: A): Either[(A, A) => Rel.SuperOrSame, (A, A)]

    /** This can be a cheap union, not a totally normalizing union.
      */
    def cheapUnion(head: A, tail: List[A]): A

    def intersect(a: A, b: A): A

    // Is A the empty set
    def isEmpty(a: A): Boolean

    private def subIntersectsCase(ab: A, a1: A, a2: A): Rel =
      unionRelCompare1(ab, a1, a2)(using relatable) match {
        case Right(Sub)          => Intersects
        case Right(Same)         => Sub
        case Left(SubIntersects) =>
          Intersects // we know a <:> b is < or n, so a&b <:> a is < implies this
        case Left(SuperSame) => Sub
        // $COVERAGE-OFF$
        case Right(rel) =>
          // this should never happen because we know that ab is sub or intersect
          sys.error(s"unexpected rel: $rel, ab = $ab, a1 = $a1, a2 = $a2")
        // $COVERAGE-ON$
      }

    /** compare a to (b1|b2)
      */
    final def unionRelCompare(a: A, b1: A, b2: A): Rel =
      if (isEmpty(b1)) relatable.relate(a, b2)
      else if (isEmpty(b2)) relatable.relate(a, b1)
      else
        unionRelCompare1(a, b1, b2)(using relatable) match {
          case Right(rel) =>
            rel
          case Left(p) =>
            // Note, a is never empty here because if it is, unionRelCompare1 is Sub
            (deunion(a), p) match {
              case (Right((a1, a2)), SubIntersects) =>
                val head = intersect(b1, a1)
                val tail = intersect(b2, a1) :: intersect(b1, a2) :: intersect(
                  b2,
                  a2
                ) :: Nil
                val ab = cheapUnion(head, tail)
                subIntersectsCase(ab, a1, a2)
              case (Right((a1, a2)), SuperSame) =>
                // if we have SuperSame and invert(p1) what is the result
                inline def andInvert(p1: PartialRel): Rel =
                  p1 match {
                    case SuperSame     => Same
                    case SubIntersects => Super
                  }

                // we know that a1 and a2 are not empty because they are the result
                // of a deunion
                unionRelCompare1(cheapUnion(b1, b2 :: Nil), a1, a2)(using
                  relatable
                ) match {
                  case Left(r)  => andInvert(r)
                  case Right(r) => r.invert
                }
              case (Left(f), SubIntersects) =>
                // we know a < (b1| b2) or it intersects
                // so, if we try again with ((b1 | b2) & a)
                // if a < (b1 | b2), then a <:> ((b1 | b2) & a) == Same
                // else if a intersects (b1 | b2), then a <:> ((b1 | b2) & a) == Super
                val ab1 = intersect(a, b1)
                val ab2 = intersect(a, b2)
                f(ab1, ab2) match {
                  case Same  => Sub
                  case Super => Intersects
                }
              case (Left(f), SuperSame) =>
                f(b1, b2)
            }
        }
  }
}
