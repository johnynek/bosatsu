package org.bykn.bosatsu

import cats.{Order, Applicative, Eq}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

import rankn.{Type, TypeEnv}
import Pattern._

import Identifier.{Bindable, Constructor}

object TotalityCheck {
  type Cons = (PackageName, Constructor)
  type Res[+A] = Either[NonEmptyList[Error], A]
  type Patterns = List[Pattern[Cons, Type]]
  type ListPatElem = ListPart[Pattern[Cons, Type]]

  sealed abstract class Error
  case class ArityMismatch(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv[Any], expected: Int, found: Int) extends Error
  case class UnknownConstructor(cons: Cons, in: Pattern[Cons, Type], env: TypeEnv[Any]) extends Error
  case class MultipleSplicesInPattern(pat: ListPat[Cons, Type], env: TypeEnv[Any]) extends Error
  case class InvalidStrPat(pat: StrPat, env: TypeEnv[Any]) extends Error

  sealed abstract class ExprError[A] {
    def matchExpr: Expr.Match[A]
  }
  case class NonTotalMatch[A](matchExpr: Expr.Match[A], missing: NonEmptyList[Pattern[Cons, Type]]) extends ExprError[A]
  case class InvalidPattern[A](matchExpr: Expr.Match[A], err: Error) extends ExprError[A]
  case class UnreachableBranches[A](matchExpr: Expr.Match[A], branches: NonEmptyList[Pattern[Cons, Type]]) extends ExprError[A]
}

/**
 * Here is code for performing totality checks of matches.
 * One key thing: we can assume that any two patterns are describing the same type, or otherwise
 * typechecking cannot pass. So, this allows us to make certain inferences, e.g.
 * _ - [_] = [_, _, *_]
 * because we know the type must be a list of some kind of [_] is to be a well typed pattern.
 *
 * similarly, some things are ill-typed: `1 - 'foo'` doesn't make any sense. Those two patterns
 * don't describe the same type.
 */
case class TotalityCheck(inEnv: TypeEnv[Any]) {
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
  def missingBranches(branches: Patterns): Res[Patterns] =
    branches.foldM(List(WildCard): Patterns) {
      (missing, nextPattern) => difference(missing, nextPattern)
    }
    .map { missing =>
      // filter any unreachable, which can happen when earlier items shadow later
      // ones
      unreachableBranches(missing) match {
        case Left(_) => missing
        case Right(unreach) =>
          missing.filterNot(unreach.toSet)
      }
    }

  /**
   * if we match these branches in order, which of them
   * are completely covered by previous matches
   */
  def unreachableBranches(branches: Patterns): Res[Patterns] = {
    def withPrev(bs: Patterns, prev: Patterns): List[(Pattern[Cons, Type], Patterns)] =
      bs match {
        case Nil => Nil
        case h :: tail =>
          (h, prev.reverse) :: withPrev(tail, h :: prev)
      }

    withPrev(branches, Nil)
      .traverse { case (p, prev) =>
        differenceAll(p :: Nil, prev)
          .map { remaining =>
            // if there is nothing, this is unreachable
            if (remaining.isEmpty) Some(p)
            else None
          }
      }
      .map(_.collect { case Some(p) => p })
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
   * Constructors must match all items to be legal
   */
  private def checkArity(nm: Cons, size: Int, pat: Pattern[Cons, Type]): Res[Unit] =
    inEnv.typeConstructors.get(nm) match {
      case None => Left(NonEmptyList.of(UnknownConstructor(nm, pat, inEnv)))
      case Some((_, params, _)) =>
        val cmp = params.lengthCompare(size)
        if (cmp == 0) validUnit
        else Left(NonEmptyList.of(ArityMismatch(nm, pat, inEnv, size, params.size)))
    }

  private[this] val validUnit: Res[Unit] = Right(())
  /**
   * Check that a given pattern follows all the rules.
   *
   * The main rules are:
   * * in strings, you cannot have two adjacent variable patterns (where should one end?)
   * * in lists we cannot have more than one variable pattern (maybe relaxed later to the above)
   */
  def validatePattern(p: Pattern[Cons, Type]): Res[Unit] =
    p match {
      case lp@ListPat(parts) =>
        val globs = parts.count {
          case _: ListPart.Glob => true
          case ListPart.Item(_) => false
        }

        val outer =
          if (globs > 1) Left(NonEmptyList(MultipleSplicesInPattern(lp, inEnv), Nil))
          else validUnit

        val inners: Res[Unit] =
          parts.parTraverse_ {
            case ListPart.Item(p) => validatePattern(p)
            case _ => validUnit
          }

        (outer, inners).parMapN { (_, _) => () }

      case sp@StrPat(_) =>
        val simp = sp.toSimple
        if (simp.normalize == simp) validUnit
        else Left(NonEmptyList(InvalidStrPat(sp, inEnv), Nil))

      case PositionalStruct(name, args) =>
        // This is total if the struct has a single constructor AND each of the patterns is total
        val nameCheck = inEnv.definedTypeFor(name) match {
          case None =>
            Left(NonEmptyList.of(UnknownConstructor(name, p, inEnv)))
          case Some(_) => resBool(true)
        }
        val argCheck = args.parTraverse_(validatePattern)
        val arityCheck = checkArity(name, args.size, p)
        (nameCheck, argCheck, arityCheck).parMapN { (_, _, _) => () }

      case _ => validUnit
    }

  /**
   * Check that an expression, and all inner expressions, are total, or return
   * a NonEmptyList of matches that are not total
   */
  def checkExpr[A](expr: Expr[A]): ValidatedNel[ExprError[A], Unit] = {
    import Expr._
    expr match {
      case Annotation(e, _, _) => checkExpr(e)
      case AnnotatedLambda(_, _, e, _) => checkExpr(e)
      case Lambda(_, e, _) => checkExpr(e)
      case Var(_, _, _) | Literal(_, _) => Validated.valid(())
      case App(fn, arg, _) => checkExpr(fn) *> checkExpr(arg)
      case Let(_, e1, e2, _, _) => checkExpr(e1) *> checkExpr(e2)
      case m@Match(arg, branches, _) =>
        val patterns = branches.toList.map(_._1)
        patterns
          .parTraverse_(validatePattern)
          .leftMap { nel =>
            nel.map(InvalidPattern(m, _))
          }
          .toValidated
          .andThen { _ =>
            // if the patterns are good, then we check them for totality
            val argAndBranchExprs = arg :: branches.toList.map(_._2)
            val recursion = argAndBranchExprs.traverse_(checkExpr)

            val missing: ValidatedNel[ExprError[A], Unit] =
              missingBranches(patterns) match {
                case Left(errs) =>
                  Validated.invalid(errs.map { err => InvalidPattern(m, err) })
                case Right(mis) =>
                  NonEmptyList.fromList(mis) match {
                    case Some(nel) =>
                      Validated.invalidNel(NonTotalMatch(m, nel): ExprError[A])
                    case None => Validated.valid(())
                  }
              }

            val unreachable: ValidatedNel[ExprError[A], Unit] =
              unreachableBranches(patterns) match {
                case Left(errs) =>
                  Validated.invalid(errs.map { err => InvalidPattern(m, err) })
                case Right(unr) =>
                  NonEmptyList.fromList(unr) match {
                    case Some(nel) =>
                      Validated.invalidNel(UnreachableBranches(m, nel): ExprError[A])
                    case None => Validated.valid(())
                  }
              }

            missing *> unreachable *> recursion
          }
          .leftMap { errs =>
            val errList = errs.toList
            // distinct can't reduce to 0
            NonEmptyList.fromListUnsafe(errList.distinct)
          }
    }
  }

  /**
   * This is like a non-symmetric set difference, where we are removing the right from the left
   */
  def difference(left: Patterns, right: Pattern[Cons, Type]): Res[Patterns] =
    left.traverse(difference0(_, right)).map(_.flatten)

  @annotation.tailrec
  final def differenceAll(left: Patterns, right: Patterns): Res[Patterns] =
    right match {
      case Nil => Right(left)
      case h :: tail =>
        difference(left, h) match {
          case err@Left(_) => err
          case Right(left0) => differenceAll(left0, tail)
        }
    }

  private[this] val rightNil = Right(Nil)
  @inline private[this] def emptyUnion[A]: Res[List[A]] =
    rightNil

  @inline private[this] def singleUnion[A](a: A): Res[List[A]] =
    Right(a :: Nil)

  private[this] val rightTrue = Right(true)
  private[this] val rightFalse = Right(false)
  @inline private[this] def resBool(b: Boolean): Res[Boolean] =
    if (b) rightTrue else rightFalse

  @annotation.tailrec
  private def matchesEmpty(lp: List[ListPatElem]): Boolean =
    lp match {
      case Nil => true
      case (_: ListPart.Glob) :: tail => matchesEmpty(tail)
      case ListPart.Item(_) :: _ => false
    }

  /**
   * By invariant, we never allow invalid patterns to enter
   * this method
   */
  private def difference0List(
    lp: List[ListPart[Pattern[Cons, Type]]],
    rp: List[ListPart[Pattern[Cons, Type]]]): Res[List[ListPat[Cons, Type]]] =
    (lp, rp) match {
      case (Nil, Nil) =>
        // total overlap
        emptyUnion
      case (Nil, ListPart.Item(_) :: _) =>
        // a list of 1 or more, can't match less
        singleUnion(ListPat(lp))
      case (Nil, (_: ListPart.Glob) :: tail) =>
        // we can have zero or more, 1 or more clearly can't match:
        // if the tail can match 0, we anhilate, otherwise not
        if (matchesEmpty(tail)) emptyUnion
        else singleUnion(ListPat(lp))
      case (ListPart.Item(_) :: _, Nil) =>
        // left has at least one
        singleUnion(ListPat(lp))
      case ((_: ListPart.Glob) :: tail, Nil) =>
        // if tail matches empty, then we can only match 1 or more
        // else, these are disjoint
        if (matchesEmpty(tail))
          singleUnion(ListPat(ListPart.Item(WildCard) :: lp))
        else singleUnion(ListPat(lp))
      case (ListPart.Item(lhead) :: ltail, ListPart.Item(rhead) :: rtail) =>
        // we use productDifference here
        productDifference((lhead, rhead) :: (ListPat(ltail), ListPat(rtail)) :: Nil)
          .map { listOfList =>
            listOfList.map {
              case NonEmptyList(h, ListPat(tail) :: Nil) =>
                ListPat(ListPart.Item(h) :: tail)
              case other =>
                // $COVERAGE-OFF$this should be unreachable
                sys.error(s"expected exactly two items: $other")
                // $COVERAGE-ON$
            }
          }
      case ((_: ListPart.Glob) :: tail, ListPart.Item(_) :: _) =>
        /*
         * Note since we only allow a single splice,
         * tail has no splices, and is thus finite
         *
         * This is using the rule:
         * [*_, rest] = [rest] | [_, *_, rest]
         *
         * (x + y) - z = (x - z) + (y - z)
         */
        val zero = tail
        val oneOrMore = ListPart.Item(WildCard) :: lp
        // If the left and right are disjoint,
        // this creates a different representation
        // of the left
        (difference0List(zero, rp), difference0List(oneOrMore, rp))
          .mapN { (zz, oo) => unifyUnionList(zz ::: oo) }
      case (_, (_: ListPart.Glob) :: rtail) if matchesEmpty(rtail) =>
        // this is a total match
        emptyUnion
      case (_, (_: ListPart.Glob) :: _) =>
        // we know the right can't end in Left since
        // it starts with Left and the tail is not empty
        // we can make progress:
        difference0List(lp.reverse, rp.reverse)
          .map(_.map {
            case ListPat(diff) => ListPat(diff.reverse)
          })
    }

  def normalizeUnion(u: Pattern.Union[Cons, Type]): NonEmptyList[Pattern[Cons, Type]] = {
    implicit val ordPat: Order[Pattern[Cons, Type]] = Order.fromOrdering
    val flattened = Pattern.flatten(u)

    if (isTotal(flattened.toList) == resBool(true)) NonEmptyList(WildCard, Nil)
    else {
      flattened.distinct.sorted
    }
  }

  def difference0(left: Pattern[Cons, Type], right: Pattern[Cons, Type]): Res[Patterns] =
    (left, right) match {
      case (_, WildCard | Var(_)) => emptyUnion
      case (WildCard | Var(_), Literal(_)) =>
        // the left is infinite, and the right is just one value
        singleUnion(left)
      case (WildCard | Var(_), _) if isTotal(right) == resBool(true) =>
        emptyUnion
      case (Named(_, p), r) => difference0(p, r)
      case (l, Named(_, p)) => difference0(l, p)
      case (Annotation(p, _), r) => difference0(p, r)
      case (l, Annotation(p, _)) => difference0(l, p)
      case (WildCard, listPat@ListPat(rp)) =>
        // _ is the same as [*_] for well typed expressions
        difference0List(ListPart.WildList :: Nil, rp)
      case (Var(v), listPat@ListPat(rp)) =>
        // v is the same as [*v] for well typed expressions
        difference0List(ListPart.NamedList(v) :: Nil, rp)
      case (u@Union(_, _), right) =>
        val ulist = normalizeUnion(u).toList
        difference(ulist, right)
          .map(_.distinct.sorted)
          .map { diff =>
            // we know diff <= union, if diff >= u, then
            // just return the union since we want
            if (leftIsSuperSet(diff, u)) u :: Nil
            else diff
          }
      case (left, u@Union(_, _)) =>
        differenceAll(left :: Nil, normalizeUnion(u).toList).map(_.distinct.sorted)
      case (left@ListPat(lp), right@ListPat(rp)) =>
        difference0List(lp, rp)
      case (Literal(_), ListPat(_) | PositionalStruct(_, _)) =>
        singleUnion(left)
      case (Literal(Lit.Str(str)), p@StrPat(_)) if p.matches(str) =>
        emptyUnion
      case (sa@StrPat(_), Literal(Lit.Str(str))) =>
        Right(sa.toSimple.difference(SimpleStringPattern.Lit(str))
          .map { p => StrPat.fromSimple(p.normalize): Pattern[Cons, Type] }
          .sorted)
      case (sa@StrPat(_), sb@StrPat(_)) =>
        Right(sa.toSimple.difference(sb.toSimple)
          .map { p => StrPat.fromSimple(p.normalize): Pattern[Cons, Type] }
          .sorted)
      case (WildCard, StrPat(_)) =>
        // _ is the same as "${_}" for well typed expressions
        difference0(StrPat(NonEmptyList(StrPart.WildStr, Nil)), right)
      case (Var(v), StrPat(_)) =>
        // v is the same as "${v}" for well typed expressions
        difference0(StrPat(NonEmptyList(StrPart.NamedStr(v), Nil)), right)
      case (_, StrPat(_)) =>
        // ill-typed
        singleUnion(left)
      case (StrPat(_), _) =>
        // ill-typed
        singleUnion(left)
      case (ListPat(_), Literal(_) | PositionalStruct(_, _)) =>
        singleUnion(left)
      case (PositionalStruct(_, _), Literal(_) | ListPat(_)) =>
        singleUnion(left)
      case (WildCard | Var(_), PositionalStruct(nm, ps)) =>
        inEnv.definedTypeFor(nm) match {
          case None => Left(NonEmptyList.of(UnknownConstructor(nm, right, inEnv)))
          case Some(dt) =>
            dt.constructors.traverse {
              case cf if (dt.packageName, cf.name) == nm =>
                // we can replace _ with Struct(_, _...)
                val newWild = PositionalStruct(nm, cf.args.map(_ => WildCard))
                difference0(newWild, right)

              case cf =>
                // TODO, this could be smarter
                // we need to learn how to deal with typed generics
                def argToPat[A](t: (A, Type)): Pattern[Cons, Type] =
                  if (Type.hasNoVars(t._2)) Annotation(WildCard, t._2)
                  else WildCard

                singleUnion(PositionalStruct((dt.packageName, cf.name), cf.args.map(argToPat)))
            }
            .map(_.flatten)
        }
      case (llit@Literal(l), Literal(r)) =>
        if (l == r) emptyUnion
        else singleUnion(llit)
      case (PositionalStruct(ln, lp), PositionalStruct(rn, rp)) if ln == rn =>
        productDifference(lp zip rp).map { pats =>
          pats.map { tup => PositionalStruct(ln, tup.toList) }
        }
      case (PositionalStruct(_, _), PositionalStruct(_, _)) =>
        singleUnion(left)
    }

  def intersection(
    left: Pattern[Cons, Type],
    right: Pattern[Cons, Type]): Res[List[Pattern[Cons, Type]]] =
      (left, right) match {
        case (u@Union(_, _), p) =>
          normalizeUnion(u)
            .toList
            .traverse(intersection(_, p))
            .map(_.flatten.distinct.sorted)
        case (p, u@Union(_, _)) =>
          normalizeUnion(u)
            .toList
            .traverse(intersection(p, _))
            .map(_.flatten.distinct.sorted)
        case (Var(va), Var(vb)) => singleUnion(Var(Ordering[Bindable].min(va, vb)))
        case (Named(va, pa), Named(vb, pb)) if va == vb =>
          intersection(pa, pb).map(_.map(Named(va, _)))
        case (Named(va, pa), r) => intersection(pa, r)
        case (l, Named(vb, pb)) => intersection(l, pb)
        case (WildCard, v) => singleUnion(v)
        case (v, WildCard) => singleUnion(v)
        case (Var(_), v) => singleUnion(v)
        case (v, Var(_)) => singleUnion(v)
        case (Annotation(p, _), t) => intersection(p, t)
        case (t, Annotation(p, _)) => intersection(t, p)
        case (Literal(a), Literal(b)) =>
          if (a == b) singleUnion(left)
          else emptyUnion
        case (Literal(Lit.Str(s)), p@StrPat(_)) =>
          if (p.matches(s)) singleUnion(left)
          else emptyUnion
        case (p@StrPat(_), Literal(Lit.Str(s))) =>
          if (p.matches(s)) singleUnion(right)
          else emptyUnion
        case (p1@StrPat(_), p2@StrPat(_)) =>
          val n1 = p1.toSimple.unname
          val n2 = p2.toSimple.unname
          val intr =
            if (n1 == n2) (implicitly[Ordering[Pattern[Cons, Type]]].min(p1, p2) :: Nil)
            else n1
              .intersection(n2)
              .map(_.normalize) // intersection can make non-normal patterns
              .distinct
              .map { part =>
                StrPat.fromSimple(part)
              }

          Right(intr.sorted)
        case (p@StrPat(_), _) => emptyUnion
        case (_, p@StrPat(_)) => emptyUnion
        case (Literal(_), _) => emptyUnion
        case (_, Literal(_)) => emptyUnion
        case (lp@ListPat(leftL), rp@ListPat(rightL)) =>
          intersectionList(leftL, rightL).map { ps => (ps: Patterns).sorted }
        case (ListPat(_), _) => emptyUnion
        case (_, ListPat(_)) => emptyUnion
        case (PositionalStruct(ln, lps), PositionalStruct(rn, rps)) =>
          if (ln == rn) {
            type ResList[A] = Res[List[A]]
            implicit val app = Applicative[Res].compose(Applicative[List])
            lps.zip(rps).traverse[ResList, Pattern[Cons, Type]] {
              case (l, r) => intersection(l, r)
            }
            .map(_.map(PositionalStruct(ln, _): Pattern[Cons, Type]))
            .map(_.sorted)
          }
          else emptyUnion
      }

  // Try to unify lists according to the rules:
  // x u [_, *, x] = [*, x]
  // x u [*, _, x] = [*, x]
  // x u [x, *, _] = [x, *]
  // x u [x, _, *] = [x, *]
  //
  // this is an incomplete heuristic now, not a complete solution
  private def unifyUnionList(union: List[ListPat[Cons, Type]]): List[ListPat[Cons, Type]] = {

    def isAnyStar(list: List[ListPatElem]): Boolean =
      list match {
        case ListPart.Item(WildCard | Var(_)) :: (_: ListPart.Glob) :: Nil => true
        case (_: ListPart.Glob) :: ListPart.Item(WildCard | Var(_)) :: Nil => true
        case _ => false
      }

    def unifyPair(left: List[ListPatElem], right: List[ListPatElem]): Option[List[ListPatElem]] =
      if (left.startsWith(right)) {
        if (isAnyStar(left.drop(right.size))) Some(right :+ ListPart.WildList)
        else None
      }
      else if (right.startsWith(left)) {
        if (isAnyStar(right.drop(left.size))) Some(left :+ ListPart.WildList)
        else None
      }
      else if (left.endsWith(right)) {
        if (isAnyStar(left.dropRight(right.size))) Some(ListPart.WildList :: right)
        else None
      }
      else if (right.endsWith(left)) {
        if (isAnyStar(right.dropRight(left.size))) Some(ListPart.WildList :: left)
        else None
      }
      else None

    val items = union.toArray

    val pairs = for {
      i <- (0 until items.length).iterator
      j <- ((i + 1) until items.length).iterator
      pair <- unifyPair(items(i).parts, items(j).parts).iterator
    } yield (ListPat(pair), i, j)

    // stop after the first unification and loop
    if (pairs.hasNext) {
      val (pair, i, j) = pairs.next
      items(i) = null
      items(j) = null
      val rest = items.iterator.filterNot(_ == null).toList
      // let's look again
      unifyUnionList(pair :: rest)
    }
    else union
  }

  // invariant: each input has at most 1 splice pattern. This should be checked by callers.
  private def intersectionList(leftL: List[ListPatElem], rightL: List[ListPatElem]): Res[List[ListPat[Cons, Type]]] = {
    def left = ListPat(leftL)
    (leftL, rightL) match {
      case (_, (_: ListPart.Glob) :: tail) if matchesEmpty(tail) =>
        // the right hand side is a top value, it can match any list, so intersection with top is
        // left
        singleUnion(left)
      case ((_: ListPart.Glob) :: tail, _) if matchesEmpty(tail) =>
        // the left hand side is a top value, it can match any list, so intersection with top is
        // right
        singleUnion(ListPat(rightL))
      case (Nil, (_: ListPart.Glob) :: _) | ((_: ListPart.Glob) :: _, Nil) =>
        // the non Nil patterns can't match empty due to the above:
        emptyUnion
      case (Nil, Nil) =>
        // both only match the empty list
        singleUnion(left)
      case (Nil, ListPart.Item(_) :: _) => emptyUnion
      case (ListPart.Item(_) :: _, Nil) => emptyUnion
      case (ListPart.Item(lh) :: lt, ListPart.Item(rh) :: rt) =>
        intersection(lh, rh).flatMap {
          /*
           * If heads is empty, we don't need to recurse
           */
          case Nil => emptyUnion
          case heads =>
            // heads is not empty, now let's do intersection on the rest
            intersectionList(lt, rt)
              .map { inner =>
                heads.flatMap { h =>
                  inner.map(_.prepend(ListPart.Item(h)))
                }
            }
        }
        .map(unifyUnionList(_))
      case ((lh@ListPart.Item(_)) :: lt, (_: ListPart.Glob) :: rt) =>
        // we know the glob is not empty, because otherwise it would
        // matchEmpty above
        //
        // [lh :: lt] n [* :: rt] =
        // [lh :: lt] n (rt u [_, * :: rt]) =
        // (left n rt) u (lh :: (lt n right))
        for {
          lefts <- intersectionList(leftL, rt)
          rights <- intersectionList(lt, rightL)
          rights1 = rights.map(_.prepend(lh))
        } yield unifyUnionList(lefts ::: rights1)
      case ((_: ListPart.Glob) :: _, ListPart.Item(_) :: _) =>
        // intersection is symmetric
        intersectionList(rightL, leftL)
      case ((a: ListPart.Glob) :: lt, (b: ListPart.Glob) :: rt) =>
        /*
         * We have a trick to diagonalize an intersection like
         * this:
         * [* :: lt] n [* :: rt] =
         *   * :: ( (lt n rt) u (lt n (_ :: rightL)) u ((_ :: leftL) n rt) )
         *
         *  not each of the intersections as smaller that the input
         */
        val anyLt = ListPart.Item(WildCard) :: leftL
        val anyRt = ListPart.Item(WildCard) :: rightL
        val wildHead: ListPart[Pattern[Cons, Type]] =
          if (a == b) a else ListPart.WildList
        val int0 = intersectionList(lt, rt)
        val int1 = intersectionList(lt, anyRt)
        val int2 = intersectionList(anyLt, rt)
        (int0, int1, int2)
          .mapN { (l0, l1, l2) =>
            val union = l0 ::: l1 ::: l2
            // wild to each pattern in the list
            val withWild = union.map(_.prepend(wildHead))
            unifyUnionList(withWild)
          }
    }
  }

  /*
   * TODO, this a little weak now, would be great to make this tight and directly
   * tested. I think introducing union patterns will force our hand
   *
   * This is private because it is currently an approximation that sometimes
   * give false negatives
   */
  private def leftIsSuperSet(superSet: Patterns, subSet: Pattern[Cons, Type]): Boolean = {
    // This is true, but doesn't terminate
    // superSet match {
    //   case Nil => false
    //   case h :: tail =>
    //     difference(subSet, h) match {
    //       case Left(_) => false
    //       case Right(newSubs) =>
    //         leftIsSuperSet(tail, newSubs)
    //     }
    // }
    def loop(superSet: Patterns, subSet: Pattern[Cons, Type]): Boolean =
      (superSet, subSet) match {
        case ((WildCard | Var(_)) :: _, _) => true
        case (Named(_, p) :: t, r) => loop(p :: t, r)
        case (sup, Named(_, sub)) => loop(sup, sub)
        case (_, Annotation(p, _)) => loop(superSet, p)
        case (_, (WildCard | Var(_))) => false // we never call this on a total superset
        case (Union(h, t) :: rest, sub) =>
          loop(h :: t.toList ::: rest, sub)
        case (items, Union(h, t)) =>
          loop(items, h) && t.forall(loop(items, _))
        case ((Literal(a) :: tail), lit@Literal(b)) =>
          (a == b) || loop(tail, lit)
        case ((Literal(_) :: tail), notLit) => loop(tail, notLit)
        case (((p@StrPat(_)) :: tail), str@Literal(Lit.Str(s))) =>
          p.matches(s) || loop(tail, str)
        case (((p@StrPat(_)) :: tail), strPat@StrPat(_)) =>
          // this is true, but there are other cases of supersets
          // we have not yet computed. Not clear it is useful
          p.isTotal ||
            (normalizePattern(p) == normalizePattern(strPat)) ||
            strPat.toLiteralString.exists(p.matches(_)) ||
            loop(tail, strPat)
        case ((StrPat(_) :: tail), other) =>
          loop(tail, other)
        case (_ :: tail, strPat@StrPat(_)) =>
          loop(tail, strPat)
        case (Annotation(p, _) :: tail, sub) => loop(p :: tail, sub)
        case (_, PositionalStruct(psub, partsSub)) =>
          val partsSize = partsSub.size
          val structs = superSet.collect { case PositionalStruct(n, parts) if n == psub => parts }
          def toList(p: Patterns): ListPat[Cons, Type] =
            ListPat(p.map(ListPart.Item(_)))
          val subListPat = toList(partsSub)
          loop(structs.map(toList), subListPat)
        case (PositionalStruct(_, _) :: tail, ListPat(_) | Literal(_)) => loop(tail, subSet)
        case ((left@ListPat(_)) :: tail, right@ListPat(_)) =>
          // in case
          val nonList = tail.filter {
            case ListPat(_) => false
            case _ => true
          }
          val tailLists: List[ListPat[Cons, Type]] = tail.collect { case lp@ListPat(_) => lp }
          loop(nonList, right) || listSuper(left :: tailLists, right)
        case ((ListPat(_)) :: tail, notList) =>
          loop(tail, notList)
        case (Nil, _) => false
      }

    loop(superSet, subSet)
  }

  /**
   * This is the complex part of this problem
   * [] | [_, *_] == [*_]
   * [] | [*_, _] == [*_]
   *
   * we could also concat onto the front or back
   */
  private def listSuper(left: List[ListPat[Cons, Type]], right: ListPat[Cons, Type]): Boolean =
    left.exists(eqPat.eqv(_, right))
      // case (ListPat(Right(p) :: lrest) :: tail, ListPat(Right(subp) :: subrest)) =>
      //   (listSuper(p :: Nil, subp) && listSuper(ListPat(lrest) :: Nil, ListPat(subrest))) ||
      //     listSuper(tail, subSet)
      // case ((lp@ListPat(Left(_) :: lrest)) :: tail, ListPat(Right(_) :: subrest)) =>
      //   // the left can absorb this right
      //   (listSuper(lp :: Nil, ListPat(subrest))) ||
      //     listSuper(tail, subSet)
      // case (ListPat(Right(p) :: suprest) :: tail, ListPat(Left(_) :: subrest)) =>
      //   listSuper(superSet, ListPat(subrest))
      // case (ListPat(Nil) :: tail, ListPat(parts)) =>
      //   parts.isEmpty || listSuper(tail, subSet)

  /**
   * There the list is a tuple or product pattern
   * the left and right should be the same size and the result will be a list of lists
   * with the inner having the same size.
   *
   * Note, we return a NonEmptyList because if we have a Nil input, then, there is
   * no difference so we always return emptyUnion in that case.
   *
   * The result is a union
   */
  def productDifference(
    zip: List[(Pattern[Cons, Type], Pattern[Cons, Type])]
  ): Res[List[NonEmptyList[Pattern[Cons, Type]]]] =
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
     *
     * similarly: a0 - b0 = a0, implies a0 n b0 = 0
     * so, the difference is a0 x a1, or no difference...
     *
     * note that a0 - b0 <= a0, so if we have a0 - b0 >= a0, we know a0 - b0 = a0
     */
    zip match {
      case Nil => emptyUnion // complete match
      case (lh, rh) :: tail =>
        type Result = Res[List[NonEmptyList[Pattern[Cons, Type]]]]

        val headDiff = difference0(lh, rh)

        def noDiffResult: List[NonEmptyList[Pattern[Cons, Type]]] =
          NonEmptyList(lh, tail.map(_._1)) :: Nil

        headDiff.flatMap {
          case noDiff if leftIsSuperSet(noDiff, lh) =>
            Right(noDiffResult)
          case hd =>
            val tailDiff: Result =
              intersection(lh, rh).flatMap {
                case Nil =>
                  // There is no intersection in head
                  emptyUnion
                case intrs =>
                  // note that the each item in the inner list
                  // has the same size as tail
                  val taild = productDifference(tail)
                  intrs.traverse { intr: Pattern[Cons, Type] =>
                    // intrs is a non empty list
                    // of to union
                    taild.map { union: List[NonEmptyList[Pattern[Cons, Type]]] =>
                      // union is a list of patterns of the same size as tail
                      union.map(intr :: _)
                    }
                  }
                  .map(_.flatten)
              }

            def productAsList(prod: List[Pattern[Cons, Type]]): Pattern[Cons, Type] =
              ListPat(prod.map(ListPart.Item(_)))

            tailDiff.map { union =>
              val unionAsList = union.map { t => productAsList(t.tail) }
              val tailProd = productAsList(tail.map(_._1))

              if (leftIsSuperSet(unionAsList, tailProd)) {
                // this is the rule that if the rest has no diff, the first
                // part has no diff
                // not needed for correctness, but useful for normalizing
                noDiffResult
              }
              else {
                val headDiffWithRest = hd.map { h => NonEmptyList(h, tail.map(_._1)) }
                headDiffWithRest ::: union
              }
            }
        }
    }


  /**
   * Can a given pattern match everything for a the current type
   */
  private def isTotal(p: Pattern[Cons, Type]): Boolean =
    p match {
      case Pattern.WildCard | Pattern.Var(_) => true
      case Pattern.Named(_, p) => isTotal(p)
      case Pattern.Literal(_) => false // literals are not total
      case s@Pattern.StrPat(_) => s.isTotal
      case Pattern.ListPat((_: ListPart.Glob) :: rest) =>
        matchesEmpty(rest)
      case Pattern.ListPat(_) =>
        // can't match everything on the front
        false
      case Pattern.Annotation(p, _) => isTotal(p)
      case Pattern.PositionalStruct(name, params) =>
        inEnv.definedTypeFor(name) match {
          case None =>
            false
          case Some(dt) =>
            // we check at the beginning if the arity matches
            // so we don't need to check here
            dt.isStruct && params.forall(isTotal)
        }
      case Pattern.Union(h, t) => isTotal(h :: t.toList) == resBool(true)
    }

  /**
   * recursively replace as much as possible with Wildcard
   * This should match exactly the same set for the same type as
   * the previous pattern, without any binding names
   */
  def normalizePattern(p: Pattern[Cons, Type]): Pattern[Cons, Type] =
    p match {
      case WildCard | Literal(_) => p
      case Var(_) => WildCard
      case Named(_, p) => normalizePattern(p)
      case Annotation(p, t) => normalizePattern(p)
      case Union(h, t) =>
        val pats = normalizeUnion(Union(normalizePattern(h), t.map(normalizePattern(_))))
        Pattern.union(pats.head, pats.tail)
      case _ if isTotal(p) => WildCard
      case strPat@StrPat(_) =>
        StrPat.fromSimple(strPat.toSimple.unname)
      case ListPat(ls) =>
        val normLs: List[ListPatElem] =
          ls.map {
            case _: ListPart.Glob => ListPart.WildList
            case ListPart.Item(p) => ListPart.Item(normalizePattern(p))
          }
        // we can't be a single WildCard because this is not total
        ListPat(normLs)
      case PositionalStruct(n, params) =>
        PositionalStruct(n, params.map(normalizePattern))
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
