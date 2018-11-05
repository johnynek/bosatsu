package org.bykn.bosatsu

import cats.{Monad, Applicative, Eq}
import cats.data.NonEmptyList
import cats.implicits._

import rankn.{Type, TypeEnv}
import Pattern._

object TotalityCheck {
  type Cons = (PackageName, ConstructorName)
  type Res[+A] = Either[NonEmptyList[Error], A]
  type Patterns = List[Pattern[Cons, Type]]
  type ListPatElem = Either[Option[String], Pattern[Cons, Type]]

  sealed abstract class Error
  case class ArityMismatch(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv, expected: Int, found: Int) extends Error
  case class UnknownConstructor(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv) extends Error
  case class UntypedPattern(pat: Pattern[Cons, Type], env: TypeEnv) extends Error
  case class MultipleSplicesInPattern(pat: ListPat[Cons, Type], env: TypeEnv) extends Error
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
  private def matchesEmpty(lp: List[ListPatElem]): Boolean =
    lp match {
      case Nil => true
      case Left(_) :: tail => matchesEmpty(tail)
      case Right(_) :: _ => false
    }

  private def difference0List(
    lp: List[Either[Option[String], Pattern[Cons, Type]]],
    rp: List[Either[Option[String], Pattern[Cons, Type]]]): Res[Patterns] = {
    (lp, rp) match {
      case (Nil, Nil) =>
        // total overlap
        Right(Nil)
      case (Nil, Right(_) :: _) =>
        // a list of 1 or more, can't match less
        Right(ListPat(lp) :: Nil)
      case (Nil, Left(_) :: tail) =>
        // we can have zero or more, 1 or more clearly can't match:
        // if the tail can match 0, we anhilate, otherwise not
        if (matchesEmpty(tail)) Right(Nil)
        else Right(ListPat(lp) :: Nil)
      case (Right(_) :: _, Nil) =>
        // left has at least one
        Right(ListPat(lp) :: Nil)
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
        if (matchesEmpty(tail))
          Right(ListPat(Right(WildCard) :: lp) :: Nil)
        else Right(ListPat(lp) :: Nil)
      case (Left(_) :: tail, Right(_) :: _) =>
        val zero = tail
        val oneOrMore = Right(WildCard) :: lp
        (difference0List(zero, rp), difference0List(oneOrMore, rp)).mapN(_ ::: _)
      case (_, Left(_) :: rtail) if matchesEmpty(rtail) =>
        // this is a total match
        Right(Nil)
      case (_, Left(_) :: _) =>
        // if this pattern ends with Left(_) we have
        // a hard match problem on our hands. For now, we ban it:
        val revRight = rp.reverse
        revRight match {
          case Left(_) :: tail if !matchesEmpty(tail) =>
            Left(NonEmptyList.of(MultipleSplicesInPattern(ListPat(rp), inEnv)))
          case _ =>
            // we can make progress:

            // In this branch, the right cannot match
            // the empty list, but the left side can
            // we could in principle match a finite
            // list from either direction, so we reverse
            // and try again
            difference0List(lp.reverse, revRight)
              .map(_.map {
                case ListPat(diff) => ListPat(diff.reverse)
                case other => sys.error(s"unreachable: list patterns can't difference to non-list: $other")
              })
        }
    }
  }

  def difference0(left: Pattern[Cons, Type], right: Pattern[Cons, Type]): Res[Patterns] =
    (left, right) match {
      case (_, WildCard | Var(_)) => Right(Nil)
      case (WildCard | Var(_), Literal(_)) =>
        // the left is infinite, and the right is just one value
        Right(left :: Nil)
      case (WildCard | Var(_), _) if isTotal(right) == Right(true) =>
        Right(Nil)
      case (WildCard, ListPat(rp)) =>
        // _ is the same as [*_] for well typed expressions
        difference0List(Left(None) :: Nil, rp)
      case (Var(v), ListPat(rp)) =>
        // v is the same as [*v] for well typed expressions
        difference0List(Left(Some(v)) :: Nil, rp)
      case (ListPat(lp), ListPat(rp)) =>
        difference0List(lp, rp)
      case (Literal(_), ListPat(_) | PositionalStruct(_, _)) =>
        Right(left :: Nil)
      case (ListPat(_), Literal(_) | PositionalStruct(_, _)) =>
        Right(left :: Nil)
      case (PositionalStruct(_, _), Literal(_) | ListPat(_)) =>
        Right(left :: Nil)
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

        arityMatch >> productDifference(lp zip rp).map { pats =>
          pats.map(PositionalStruct(ln, _))
        }
      case (PositionalStruct(_, _), PositionalStruct(_, _)) =>
        Right(left :: Nil)

      // case _ =>
      //   // There is no overlap
      //   Right(left :: Nil): Res[Patterns]
    }

  def intersection(
    left: Pattern[Cons, Type],
    right: Pattern[Cons, Type]): Res[List[Pattern[Cons, Type]]] =
      (left, right) match {
        case (Var(va), Var(vb)) => Right(List(Var(Ordering[String].min(va, vb))))
        case (WildCard, v) => Right(List(v))
        case (v, WildCard) => Right(List(v))
        case (Var(_), v) => Right(List(v))
        case (v, Var(_)) => Right(List(v))
        case (Annotation(p, _), t) => intersection(p, t)
        case (t, Annotation(p, _)) => intersection(t, p)
        case (Literal(a), Literal(b)) =>
          if (a == b) Right(List(left))
          else Right(Nil)
        case (Literal(_), _) => Right(Nil)
        case (_, Literal(_)) => Right(Nil)
        case (ListPat(leftL), ListPat(rightL)) =>
          intersectionList(leftL, rightL)
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
            check >>
              lps.zip(rps).traverse[ResList, Pattern[Cons, Type]] {
                case (l, r) => intersection(l, r)
              }
              .map(_.map(PositionalStruct(ln, _)))
          }
          else Right(Nil)
      }

  private def intersectionList(leftL: List[ListPatElem], rightL: List[ListPatElem]): Res[Patterns] = {
    def left = ListPat(leftL)
    (leftL, rightL) match {
      case (_, Left(_) :: tail) if matchesEmpty(tail) =>
        // the right hand side is a top value, it can match any list, so intersection with top is
        // left
        Right(left :: Nil)
      case (Left(_) :: tail, _) if matchesEmpty(tail) =>
        // the left hand side is a top value, it can match any list, so intersection with top is
        // right
        Right(ListPat(rightL) :: Nil)
      case (Nil, Nil) => Right(List(left))
      case (Nil, Right(_) :: _) => Right(Nil)
      case (Nil, Left(_) :: _) | (Left(_) :: _, Nil) =>
        // the non Nil patterns can't match empty due to the above:
        Right(Nil)
      case (Right(_) :: _, Nil) => Right(Nil)
      case (Right(lh) :: lt, Right(rh) :: rt) =>
        intersection(lh, rh).flatMap {
          case Nil => Right(Nil)
          case nonEmpty =>
            intersectionList(lt, rt)
              .map(_.flatMap {
                case ListPat(ts) =>
                  // this could create duplicates
                  nonEmpty
                    .map { h => ListPat(Right(h) :: ts) }
                    .distinct
                case other => sys.error(s"unreachable: list patterns can't intersect to non-list: $other")
              })
        }
      case (Right(lh) :: lt, Left(rh) :: rt) =>
        // a n (b0 + b1) = (a n b0) + (a n b1)
        val zero = rt
        val oneOrMore = Right(WildCard) :: rightL
        for {
          withZ <- intersectionList(leftL, zero)
          with0 <- intersectionList(leftL, oneOrMore)
        } yield (withZ ::: with0).distinct
      case (Left(_) :: _, Right(_) :: _) =>
        // intersection is symmetric
        intersectionList(rightL, leftL)
      case (Left(a) :: lt, Left(b) :: rt) =>
        /*
         * the left and right can consume any number
         * of items before matching the rest.
         *
         * if we assume rt has no additional Lefts,
         * we can pad the left to be the same size
         * by adding wildcards, and repeat
         */
        def hasMultiple(ps: List[ListPatElem]): Boolean =
          ps.exists {
            case Left(_) => true
            case Right(_) => false
          }

        (hasMultiple(lt), hasMultiple(rt)) match {
          case (true, true) =>
            Left(NonEmptyList.of(
              MultipleSplicesInPattern(ListPat(rightL), inEnv),
              MultipleSplicesInPattern(ListPat(leftL), inEnv)))
          case (_, false) =>
            /*
             * make suffix of rt that lines up with lt
             */
            val rtSize = rt.size
            val ltSize = lt.size
            val padSize = rtSize - ltSize
            val (initLt, lastLt) =
              if (padSize > 0) {
                (List.empty[ListPatElem], List.fill(padSize)(Right(WildCard)) reverse_::: lt)
              }
              else {
                (lt.take(-padSize), lt.drop(-padSize))
              }
            intersectionList(lastLt, rt)
              .map(_.map {
                case ListPat(tail) =>
                  val m: ListPatElem = Left(if (a == b) a else None)
                  ListPat(m :: initLt ::: tail)
                case other => sys.error(s"unreachable: list patterns can't intersect to non-list: $other")
              })
          case (false, _) =>
            // intersection is symmetric
            intersectionList(rightL, leftL)
        }
    }
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
     *
     * Note, if a1 - b1 = a1, this becomes:
     * ((a0 - b0) + (a0 n b0)) x a1 = a0 x a1
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

        tailDiff match {
          case Right((intr :: td) :: Nil)
            if tail.zip(td).forall { case ((a1, _), d) => eqPat.eqv(a1, d) } =>
              // this is the rule that if the rest has no diff, the first
              // part has no diff
              // not needed for correctness, but useful for normalizing
              Right(zip.map(_._1) :: Nil)
          case _ =>
            (headDiff, tailDiff).mapN(_ ::: _)
        }
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
        Right(matchesEmpty(rest))
      case Pattern.ListPat(_) =>
        // can't match everything on the front
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

  def normalizePattern(p: Pattern[Cons, Type]): Pattern[Cons, Type] =
    isTotal(p) match {
      case Right(true) => WildCard
      case _ =>
        p match {
          case WildCard | Literal(_) => p
          case Var(_) => WildCard
          case ListPat(ls) =>
            val normLs: List[ListPatElem] =
              ls.map {
                case Left(_) => Left(None)
                case Right(p) => Right(normalizePattern(p))
              }
            normLs match {
              case Left(None) :: Nil => WildCard
              case rest => ListPat(rest)
            }
          case Annotation(p, t) => Annotation(normalizePattern(p), t)
          case PositionalStruct(n, params) =>
            PositionalStruct(n, params.map(normalizePattern))
        }
    }

  /**
   * This tells if two patterns for the same type
   * would match the same values
   */
  val eqPat: Eq[Pattern[Cons, Type]] =
    new Eq[Pattern[Cons, Type]] {
      def eqv(l: Pattern[Cons, Type], r: Pattern[Cons, Type]) =
        normalizePattern(l) == normalizePattern(r)
    }
}
