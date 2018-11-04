package org.bykn.bosatsu

import cats.{Monad, Applicative}
import cats.data.NonEmptyList
import cats.implicits._

import rankn.{Type, TypeEnv}
import Pattern._

object TotalityCheck {
  type Cons = (PackageName, ConstructorName)
  type Res[+A] = Either[NonEmptyList[Error], A]
  type Patterns = List[Pattern[Cons, Type]]

  sealed abstract class Error
  case class ArityMismatch(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv, expected: Int, found: Int) extends Error
  case class UnknownConstructor(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv) extends Error
  case class UntypedPattern(pat: Pattern[Cons, Type], env: TypeEnv) extends Error
}

case class TotalityCheck(inEnv: TypeEnv) {
  import TotalityCheck._

  /**
   * in the given type environment, return
   * a list of matches that would make the current set of matches total
   *
   * Note, a law here is that:
   * missingBranches(te, t, branches).flatMap { ms =>
   *   assert(missingBranches(te, t, branches ::: ms).isEmpty)
   * }
   */
  def missingBranches(branches: Patterns): Res[Patterns] = {
    def step(patMiss: (Patterns, Patterns)): Res[Either[(Patterns, Patterns), Patterns]] = {
      val (branches, missing0) = patMiss
      branches match {
        case Nil =>
          Right(Right(missing0))
        case h :: tail =>
          difference(missing0, h)
            .map { newMissing =>
              Left((tail, newMissing))
            }
      }
    }

    Monad[Res].tailRecM((branches, List(WildCard): Patterns))(step _)
  }

  /**
   * Return true of this set of branches represents a total match
   *
   * useful for testing, but a better error message will be obtained from using
   * missingBranches
   */
  def isTotal(branches: Patterns): Res[Boolean] =
    missingBranches(branches).map(_.isEmpty)

  /**
   * This is like a non-symmetric set difference, where we are removing the right from the left
   */
  def difference(left: Patterns, right: Pattern[Cons, Type]): Res[Patterns] =
    left.traverse(difference0(_, right)).map(_.flatten)

  @annotation.tailrec
  private def matchesEmpty(lp: ListPat[Cons, Type]): Boolean =
    lp.parts match {
      case Nil => true
      case Left(_) :: tail => matchesEmpty(ListPat(tail))
      case Right(_) :: _ => false
    }

  def difference0(left: Pattern[Cons, Type], right: Pattern[Cons, Type]): Res[Patterns] = {
    isTotal(right).flatMap {
      case true => Right(Nil): Res[Patterns]
      case false =>
        (left, right) match {
          case (WildCard | Var(_), Literal(_)) =>
            // the left is infinite, and the right is just one value
            Right(left :: Nil)
          case (WildCard | Var(_), lp@ListPat(_)) =>
            // _ is the same as [*_] for well typed expressions
            difference0(ListPat(Left(None) :: Nil), lp)
          case (ListPat(lp), rightList@ListPat(rp)) =>
            (lp, rp) match {
              case (Nil, Nil) =>
                // total overlap
                Right(Nil)
              case (Nil, Right(_) :: _) =>
                // a list of 1 or more, can't match less
                Right(left :: Nil)
              case (Nil, Left(_) :: tail) =>
                // we can have zero or more, 1 or more clearly can't match:
                // if the tail can match 0, we anhilate, otherwise not
                if (matchesEmpty(ListPat(tail))) Right(Nil)
                else Right(left :: Nil)
              case (Right(_) :: _, Nil) =>
                // left has at least one
                Right(left :: Nil)
              case (Right(lhead) :: ltail, Right(rhead) :: rtail) =>
                // we use productDifference here
                productDifference((lhead, rhead) :: (ListPat(ltail), ListPat(rtail)) :: Nil)
                  .map { listOfList =>
                    listOfList.map {
                      case h :: ListPat(tail) :: Nil =>
                        ListPat(Right(h) :: tail)
                      case other =>
                        sys.error(s"expected exactly two items: $other")
                    }
                  }
              case (Left(_) :: tail, Nil) =>
                // if tail matches empty, then we can only match 1 or more
                // else, these are disjoint
                if (matchesEmpty(ListPat(tail)))
                  Right(ListPat(Right(WildCard) :: lp) :: Nil)
                else Right(left :: Nil)
              case (Left(_) :: tail, Right(_) :: _) =>
                // The right hand side can't match a zero length list
                val zero = ListPat(tail)
                val oneOrMore = ListPat(Right(WildCard) :: lp)
                difference0(oneOrMore, right)
                  .map(zero :: _)
              case (_, Left(_) :: rtail) if matchesEmpty(ListPat(rtail)) =>
                // this is a total match
                Right(Nil)
              case (_, Left(_) :: rtail) =>
                // In this branch, the right cannot match
                // the empty list, but the left side can
                // we could in principle match a finite
                // list from either direction, so we reverse
                // and try again
                difference0(ListPat(lp.reverse), ListPat(rp.reverse))
                  .map(_.map {
                    case ListPat(diff) => ListPat(diff.reverse)
                    case other => sys.error(s"unreachable: list patterns can't difference to non-list: $other")
                  })
            }
          case (WildCard | Var(_), PositionalStruct(nm, ps)) =>
            inEnv.definedTypeFor(nm) match {
              case None => Left(NonEmptyList.of(UnknownConstructor(nm, right, inEnv)))
              case Some(dt) =>
                dt.constructors.traverse {
                  case (c, params, _) if (dt.packageName, c) == nm =>
                    /*
                     * At each position we compute the difference with _
                     * then make:
                     * Struct(d1, _, _), Struct(_, d2, _), ...
                     */
                    def poke[M[_]: Applicative, A](items: List[A])(fn: A => M[List[A]]): M[List[List[A]]] =
                      items match {
                        case Nil => Applicative[M].pure(Nil)
                        case h :: tail =>
                          val ptail = poke(tail)(fn)
                          val head = fn(h)
                          (head, ptail).mapN { (heads, tails) =>
                            val t2 = tails.map(h :: _)
                            val h1 = heads.map(_ :: tail)
                            h1 ::: t2
                          }
                      }

                    // for this one, we need to compute the difference for each:
                    poke(ps) { p => difference0(WildCard, p) }
                      .map(_.map(PositionalStruct(nm, _)))

                  case (c, params, _) =>
                    // TODO, this could be smarter
                    // we need to learn how to deal with typed generics
                    def argToPat(t: (ParamName, Type)): Pattern[Cons, Type] =
                      if (Type.hasNoVars(t._2)) Annotation(WildCard, t._2)
                      else WildCard

                    Right(List(PositionalStruct((dt.packageName, c), params.map(argToPat))))
                }
                .map(_.flatten)
            }
          case (llit@Literal(l), Literal(r)) =>
            if (l == r) Right(Nil): Res[Patterns]
            else Right(llit :: Nil): Res[Patterns]
          case (PositionalStruct(ln, lp), PositionalStruct(rn, rp)) if ln == rn =>
            // we have two matching structs
            val arityMatch =
              checkArity(ln, lp.size, left)
                .product(checkArity(rn, rp.size, right))
                .as(())
            productDifference(lp zip rp).map { pats =>
              pats.map(PositionalStruct(ln, _))
            }
          case _ =>
            // There is no overlap
            Right(left :: Nil): Res[Patterns]
        }
    }
  }

  def intersection(
    left: Pattern[Cons, Type],
    right: Pattern[Cons, Type]): Res[List[Pattern[Cons, Type]]] =
      (left, right) match {
        case (WildCard | Var(_), v) => Right(List(v))
        case (v, WildCard | Var(_)) => Right(List(v))
        case (Annotation(p, _), t) => intersection(p, t)
        case (t, Annotation(p, _)) => intersection(t, p)
        case (Literal(a), Literal(b)) =>
          if (a == b) Right(List(left))
          else Right(Nil)
        case (Literal(_), _) => Right(Nil)
        case (_, Literal(_)) => Right(Nil)
        case (ListPat(leftL), ListPat(rightL)) =>
          (leftL, rightL) match {
            case (Nil, Nil) => Right(List(left))
            case (Nil, Right(_) :: _) => Right(Nil)
            case (_, Left(_) :: tail) if matchesEmpty(ListPat(tail)) => Right(List(left))
            case (Nil, Left(_) :: _) => Right(List(left))
            case (Right(_) :: _, Nil) => Right(Nil)
            case (Right(lh) :: lt, Right(rh) :: rt) =>
              intersection(lh, rh).flatMap {
                case Nil => Right(Nil)
                case nonEmpty =>
                  intersection(ListPat(lt), ListPat(rt))
                    .map(_.flatMap {
                      case ListPat(ts) => nonEmpty.map { h => ListPat(Right(h) :: ts) }
                      case other => sys.error(s"unreachable: list patterns can't intersect to non-list: $other")
                    })
              }
            case (Right(lh) :: lt, Left(rh) :: rt) =>
              val zero = ListPat(rt)
              val oneOrMore = ListPat(Right(WildCard) :: rightL)
              // a n (b0 + b1) = (a n b0) + (a n b1)
              for {
                withZ <- intersection(left, zero)
                with0 <- intersection(left, oneOrMore)
              } yield withZ ::: with0
            case (Left(_) :: lt, Left(_) :: rt) =>
              intersection(ListPat(lt), ListPat(rt))
                .map(_.map {
                  case ListPat(tail) => ListPat(Left(None) :: tail)
                  case other => sys.error(s"unreachable: list patterns can't intersect to non-list: $other")
                })
            case (_, _) =>
              // intersection is symmetric
              intersection(right, left)
          }
        case (ListPat(_), _) => Right(Nil)
        case (_, ListPat(_)) => Right(Nil)
        case (PositionalStruct(ln, lps), PositionalStruct(rn, rps)) =>
          if (ln == rn) {
            val check = for {
              _ <- checkArity(ln, lps.size, left)
              _ <- checkArity(rn, rps.size, right)
            } yield ()

            type ResList[A] = Res[List[A]]
            implicit val app = Applicative[Res].compose(Applicative[List])
            val parts = check.flatMap { _ =>
              lps.zip(rps).traverse[ResList, Pattern[Cons, Type]] {
                case (l, r) => intersection(l, r)
              }
            }

            parts.map(_.map(PositionalStruct(ln, _)))
          }
          else Right(Nil)
      }

  /**
   * There the list is a tuple or product pattern
   * the left and right should be the same size and the result will be a list of lists
   * with the inner having the same size
   */
  def productDifference(
    zip: List[(Pattern[Cons, Type], Pattern[Cons, Type])]
  ): Res[List[List[Pattern[Cons, Type]]]] =
    /*
     * (Left(_), _) -- (Right(_), Right(_)) = (Left(_), _)
     * (Left(_), _) -- (Left(_), Right(_)) = (Left(_), Left(_))
     *
     * (Left(_), _, _) -- (Left(_), Right(_), Right(_)) = (L, L, R), (L, R, L), (L, R, R)
     *
     * (Left(_), _) -- (Left(Right(_)), Right(_)) = (L(L(_)), _), (L(R), L(_))
     *
     * This seems to be difference of a product of sets. The formula for this
     * seems to be:
     *
     * (a0 x a1) - (b0 x b1) = (a0 - b0) x a1 + (a0 n b0) x (a1 - b1)
     */
    zip match {
      case Nil => Right(Nil) // complete match
      case (lh, rh) :: tail =>
        type Result = Res[List[List[Pattern[Cons, Type]]]]
        val headDiff: Result =
          difference0(lh, rh).map(_.map(_ :: tail.map(_._1)))

        val tailDiff: Result =
          intersection(lh, rh).flatMap {
            case Nil =>
              // we don't need to recurse on the rest
              Right(Nil)
            case nonEmpty =>
              productDifference(tail).map { pats =>
                nonEmpty.flatMap { intr =>
                  pats.map(intr :: _)
                }
              }
          }

        (headDiff, tailDiff).mapN(_ ::: _)
    }

  /**
   * Constructors must match all items to be legal
   */
  private def checkArity(nm: Cons, size: Int, pat: Pattern[Cons, Type]): Res[Unit] =
    inEnv.typeConstructors.get(nm) match {
      case None => Left(NonEmptyList.of(UnknownConstructor(nm, pat, inEnv)))
      case Some((_, params, _)) =>
        val cmp = params.lengthCompare(size)
        if (cmp == 0) Right(())
        else Left(NonEmptyList.of(ArityMismatch(nm, pat, inEnv, size, params.size)))
    }

  /**
   * Can a given pattern match everything for a the current type
   */
  private def isTotal(p: Pattern[Cons, Type]): Res[Boolean] =
    p match {
      case Pattern.WildCard | Pattern.Var(_) => Right(true)
      case Pattern.Literal(_) => Right(false) // literals are not total
      case Pattern.ListPat(Left(_) :: rest) =>
        Right(matchesEmpty(ListPat(rest)))
      case Pattern.ListPat(_) =>
        // either can't match everything on the front or back
        Right(false)
      case Pattern.Annotation(p, _) => isTotal(p)
      case Pattern.PositionalStruct(name, params) =>
        // This is total if the struct has a single constructor AND each of the patterns is total
        inEnv.definedTypeFor(name) match {
          case None =>
            Left(NonEmptyList.of(UnknownConstructor(name, p, inEnv)))
          case Some(dt) =>
            if (dt.isStruct) params.forallM(isTotal)
            else Right(false)
        }
    }


}
