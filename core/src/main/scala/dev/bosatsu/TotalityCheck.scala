package dev.bosatsu

import cats.Eq
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import dev.bosatsu.pattern.{SeqPattern, SeqPart}
import dev.bosatsu.set.{Rel, Relatable, SetOps}

import dev.bosatsu.graph.Memoize.memoizeDagHashed

import rankn.{Type, TypeEnv}
import Pattern._

import Identifier.{Bindable, Constructor}
import dev.bosatsu.rankn.DefinedType

object TotalityCheck {
  type Cons = (PackageName, Constructor)
  type Res[+A] = Either[NonEmptyList[Error], A]
  type Patterns = List[Pattern[Cons, Type]]
  type ListPatCons = ListPat[Cons, Type]
  type ListPatElem = ListPart[Pattern[Cons, Type]]

  sealed abstract class Error
  case class ArityMismatch(
      cons: Cons,
      in: Pattern[Cons, Type],
      env: TypeEnv[Any],
      expected: Int,
      found: Int
  ) extends Error
  case class UnknownConstructor(
      cons: Cons,
      in: Pattern[Cons, Type],
      env: TypeEnv[Any]
  ) extends Error
  case class MultipleSplicesInPattern(
      pat: ListPat[Cons, Type],
      env: TypeEnv[Any]
  ) extends Error
  case class InvalidStrPat(pat: StrPat, env: TypeEnv[Any]) extends Error

  sealed abstract class ExprError[A] {
    def matchExpr: Expr.Match[A]
  }
  case class NonTotalMatch[A](
      matchExpr: Expr.Match[A],
      missing: NonEmptyList[Pattern[Cons, Type]]
  ) extends ExprError[A]
  case class InvalidPattern[A](matchExpr: Expr.Match[A], err: Error)
      extends ExprError[A]
  case class UnreachableBranches[A](
      matchExpr: Expr.Match[A],
      branches: NonEmptyList[Pattern[Cons, Type]]
  ) extends ExprError[A]
}

/** Here is code for performing totality checks of matches. One key thing: we
  * can assume that any two patterns are describing the same type, or otherwise
  * typechecking cannot pass. So, this allows us to make certain inferences,
  * e.g. _ - [_] = [_, _, *_] because we know the type must be a list of some
  * kind of [_] is to be a well typed pattern.
  *
  * similarly, some things are ill-typed: `1 - 'foo'` doesn't make any sense.
  * Those two patterns don't describe the same type.
  */
case class TotalityCheck(inEnv: TypeEnv[Any]) {
  import TotalityCheck._

  /** Constructors must match all items to be legal
    */
  private def checkArity(
      nm: Cons,
      size: Int,
      pat: Pattern[Cons, Type]
  ): Res[Unit] =
    inEnv.typeConstructors.get(nm) match {
      case None => Left(NonEmptyList.of(UnknownConstructor(nm, pat, inEnv)))
      case Some((_, _, params, _)) =>
        val cmp = params.lengthCompare(size)
        if (cmp == 0) validUnit
        else
          Left(
            NonEmptyList.of(ArityMismatch(nm, pat, inEnv, size, params.size))
          )
    }

  private val validUnit: Res[Unit] = Right(())

  /** Check that a given pattern follows all the rules.
    *
    * The main rules are: * in strings, you cannot have two adjacent variable
    * patterns (where should one end?) * in lists we cannot have more than one
    * variable pattern (maybe relaxed later to the above)
    */
  def validatePattern(p: Pattern[Cons, Type]): Res[Unit] =
    p match {
      case lp @ ListPat(_) =>
        val parts = lp.parts
        val twoAdj = lp.toSeqPattern.toList.sliding(2).exists {
          case Seq(SeqPart.Wildcard, SeqPart.Wildcard) => true
          case _                                       => false
        }
        val outer =
          if (!twoAdj) validUnit
          else Left(NonEmptyList(MultipleSplicesInPattern(lp, inEnv), Nil))

        val inners: Res[Unit] =
          parts.parTraverse_ {
            case ListPart.Item(p) => validatePattern(p)
            case _                => validUnit
          }

        (outer, inners).parMapN((_, _) => ())

      case sp @ StrPat(_) =>
        val simp = sp.toSeqPattern
        def hasAdjacentWild[A](seq: SeqPattern[A]): Boolean =
          seq match {
            case SeqPattern.Empty                       => false
            case SeqPattern.Cat(SeqPart.Wildcard, tail) =>
              tail match {
                case SeqPattern.Cat(SeqPart.Wildcard, _) => true
                case notStartWild => hasAdjacentWild(notStartWild)
              }
            case SeqPattern.Cat(_, tail) => hasAdjacentWild(tail)
          }
        if (!hasAdjacentWild(simp)) validUnit
        else Left(NonEmptyList(InvalidStrPat(sp, inEnv), Nil))

      case PositionalStruct(name, args) =>
        // This is total if the struct has a single constructor AND each of the patterns is total
        val argCheck = args.parTraverse_(validatePattern)
        val nameCheck = checkArity(name, args.size, p)
        (nameCheck, argCheck).parMapN((_, _) => ())

      case _ => validUnit
    }

  /** Check that an expression, and all inner expressions, are total, or return
    * a NonEmptyList of matches that are not total
    */
  def checkExpr[A](expr: Expr[A]): ValidatedNel[ExprError[A], Unit] = {
    import Expr._
    expr match {
      case Annotation(e, _, _)                           => checkExpr(e)
      case Generic(_, e)                                 => checkExpr(e)
      case Lambda(_, e, _)                               => checkExpr(e)
      case Global(_, _, _) | Local(_, _) | Literal(_, _) => Validated.valid(())
      case App(fn, args, _)     => checkExpr(fn) *> args.traverse_(checkExpr)
      case Let(_, e1, e2, _, _) => checkExpr(e1) *> checkExpr(e2)
      case m @ Match(arg, branches, _) =>
        val allPatterns = branches.toList.map(_.pattern)
        val unguardedPatterns = branches.toList.collect {
          case branch if branch.guard.isEmpty => branch.pattern
        }
        allPatterns
          .parTraverse_(validatePattern)
          .leftMap { nel =>
            nel.map(InvalidPattern(m, _))
          }
          .toValidated
          .andThen { _ =>
            // if the patterns are good, then we check them for totality
            val argAndBranchExprs = arg :: branches.toList.flatMap { branch =>
              branch.guard.toList ::: (branch.expr :: Nil)
            }
            val recursion = argAndBranchExprs.traverse_(checkExpr)

            val missing: ValidatedNel[ExprError[A], Unit] = {
              val mis = patternSetOps.missingBranches(topList, unguardedPatterns)
              NonEmptyList.fromList(mis) match {
                case Some(nel) =>
                  Validated.invalidNel(NonTotalMatch(m, nel): ExprError[A])
                case None => Validated.valid(())
              }
            }

            val unreachable: ValidatedNel[ExprError[A], Unit] = {
              val unr = {
                @annotation.tailrec
                def loop(
                    rem: List[Expr.Branch[A]],
                    covered: List[Pattern[Cons, Type]],
                    acc: List[Pattern[Cons, Type]]
                ): List[Pattern[Cons, Type]] =
                  rem match {
                    case Nil => acc.reverse
                    case branch :: tail =>
                      val isUnreachable = fromList(covered) match {
                        case None => false
                        case Some(cov) =>
                          patternSetOps
                            .difference(branch.pattern, cov)
                            .isEmpty
                      }
                      val covered1 =
                        if (branch.guard.isEmpty) branch.pattern :: covered
                        else covered
                      val acc1 =
                        if (isUnreachable) branch.pattern :: acc
                        else acc
                      loop(tail, covered1, acc1)
                  }
                loop(branches.toList, Nil, Nil)
              }
              NonEmptyList.fromList(unr) match {
                case Some(nel) =>
                  Validated.invalidNel(
                    UnreachableBranches(m, nel): ExprError[A]
                  )
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

  private val topList = WildCard :: Nil

  def missingBranches(p: Patterns): Patterns =
    patternSetOps.missingBranches(topList, p)

  def intersection(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Patterns =
    patternSetOps.intersection(a, b)

  def difference(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Patterns =
    patternSetOps.difference(a, b)

  private def structToList(
      n: Cons,
      args: List[Pattern[Cons, Type]]
  ): Option[Pattern.ListPat[Cons, Type]] =
    (n, args) match {
      case ((PackageName.PredefName, Constructor("EmptyList")), Nil) =>
        Some(Pattern.ListPat(Nil))
      case (
            (PackageName.PredefName, Constructor("NonEmptyList")),
            h :: t :: Nil
          ) =>
        val tailRes = t match {
          case Pattern.PositionalStruct(n, a) =>
            structToList(n, a).map(_.parts)
          case Pattern.ListPat(parts) => Some(parts)
          case _                      =>
            if (patternSetOps.isTop(t)) Some(Pattern.ListPart.WildList :: Nil)
            else None
        }
        tailRes.map(t => Pattern.ListPat(Pattern.ListPart.Item(h) :: t))
      case _ => None
    }

  private lazy val seqP: SetOps[SeqPattern[Pattern[Cons, Type]]] = {
    val sp = SeqPart.part1SetOps(using patternSetOps)
    SeqPattern.seqPatternSetOps(using sp, implicitly)
  }

  private lazy val listPatternSetOps: SetOps[ListPat[Cons, Type]] =
    SetOps.imap[SeqPattern[Pattern[Cons, Type]], ListPat[Cons, Type]](
      seqP,
      ListPat.fromSeqPattern(_),
      _.toSeqPattern
    )

  private val strPatternSetOps: SetOps[StrPat] =
    SetOps.imap[SeqPattern[Int], StrPat](
      SeqPattern.seqPatternSetOps(using
        SeqPart.part1SetOps(using SetOps.distinct[Int]),
        implicitly
      ),
      StrPat.fromSeqPattern(_),
      _.toSeqPattern
    )

  private val getProd: Int => SetOps[List[Pattern[Cons, Type]]] =
    memoizeDagHashed[Int, SetOps[List[Pattern[Cons, Type]]]] {
      case (arity, _) if arity <= 0 => SetOps.unit(Nil)
      case (arity, rec)             =>
        val head = patternSetOps
        val tail = rec(arity - 1)
        val prod = SetOps.product(head, tail)

        type L = List[Pattern[Cons, Type]]

        SetOps.imap[(Pattern[Cons, Type], L), L](
          prod,
          { case (h, t) => h :: t },
          {
            case h :: t => (h, t)
            // $COVERAGE-OFF$
            case _ => sys.error(s"invalid arity: $arity, found empty list")
            // $COVERAGE-ON$
          }
        )
    }

  private def fromList(
      pats: List[Pattern[Cons, Type]]
  ): Option[Pattern[Cons, Type]] =
    pats match {
      case Nil        => None
      case one :: Nil => Some(one)
      case h :: tail  => Some(Pattern.union(h, tail))
    }

  lazy val patternSetOps: SetOps[Pattern[Cons, Type]] =
    new SetOps[Pattern[Cons, Type]] { self =>
      val urm: Relatable.UnionRelModule[Option[Pattern[Cons, Type]]] =
        new Relatable.UnionRelModule[Option[Pattern[Cons, Type]]] {
          def relatable: Relatable[Option[Pattern[Cons, Type]]] =
            new Relatable[Option[Pattern[Cons, Type]]] {
              def relate(
                  left: Option[Pattern[Cons, Type]],
                  right: Option[Pattern[Cons, Type]]
              ): Rel =
                (left, right) match {
                  case (Some(l), Some(r)) => self.relate(l, r)
                  case (None, None)       => Rel.Same
                  case (None, Some(_))    => Rel.Sub
                  case (Some(_), None)    => Rel.Super
                }
            }

          // The outer list is union, the inner list is product
          val unionProduct: Relatable[List[List[Pattern[Cons, Type]]]] =
            Relatable.listUnion[List[Pattern[Cons, Type]]](
              isEmptyFn = { _ => false }, // all products match something
              intersectFn = { (left, right) =>
                val size = left.size
                if (size == right.size) {
                  getProd(size).intersection(left, right)
                } else Nil
              },
              solveOne = { prod =>
                (prod.zipWithIndex.collectFirstSome { case (param, idx) =>
                  deunion(Some(param)) match {
                    case Right((Some(p1), Some(p2))) => Some(((p1, p2), idx))
                    case _                           => None
                  }
                }) match {
                  case Some(((p1, p2), idx)) =>
                    // we can split:
                    Right((prod.updated(idx, p1), prod.updated(idx, p2)))
                  case None =>
                    val prodSetOps = getProd(prod.size)

                    Left(union =>
                      prodSetOps.missingBranches(prod :: Nil, union).isEmpty
                    )
                }
              }
            )(using
              new Relatable[List[Pattern[Cons, Type]]] {
                def relate(
                    a: List[Pattern[Cons, Type]],
                    b: List[Pattern[Cons, Type]]
                ) = {
                  val size = a.size
                  if (size == b.size) getProd(size).relate(a, b)
                  else Rel.Disjoint
                }
              }
            )

          def cheapUnion(
              head: Option[Pattern[Cons, Type]],
              tail: List[Option[Pattern[Cons, Type]]]
          ): Option[Pattern[Cons, Type]] =
            fromList((head :: tail).flatten)

          // there are no empty patterns
          def isEmpty(a: Option[Pattern[Cons, Type]]): Boolean = a.isEmpty
          def intersect(
              a: Option[Pattern[Cons, Type]],
              b: Option[Pattern[Cons, Type]]
          ): Option[Pattern[Cons, Type]] =
            (a, b) match {
              case (Some(l), Some(r)) =>
                fromList(self.intersection(l, r))
              case (None, _) => None
              case (_, None) => None
            }

          // we know that a is nonEmpty by contract
          def deunion(a: Option[Pattern[Cons, Type]]): Either[
            (
                Option[Pattern[Cons, Type]],
                Option[Pattern[Cons, Type]]
            ) => Rel.SuperOrSame,
            (Option[Pattern[Cons, Type]], Option[Pattern[Cons, Type]])
          ] =
            a.get match {
              case u @ Pattern.Union(_, _) =>
                val (left, right) = u.split
                Right((Some(left), Some(right)))
              case Pattern.Literal(_) =>
                // if a literal is >= something it is same, no partial supersets
                Left((_, _) => Rel.Same)
              case Named(_, pat)      => deunion(Some(pat))
              case Annotation(pat, _) => deunion(Some(pat))
              case WildCard | Var(_)  =>
                Left((a, b) =>
                  // unify union returns no top level unions
                  // so isTop is cheap
                  if (unifyUnion(a.toList ::: b.toList).exists(isTop)) Rel.Same
                  else Rel.Super
                )
              case pos @ PositionalStruct(name, params) =>
                structToList(name, params) match {
                  case Some(lp) => deunion(Some(lp))
                  case None     =>
                    // this isn't a list and is >= the union of two items
                    // so it has to be a struct with the same name
                    // assuming pos >= pat
                    // we know that pat is a Struct(name, _) or union of those
                    // decompose it into List representing the union, of a list
                    // of the fields

                    val paramSize = params.size

                    def unstruct(
                        pat: Option[Pattern[Cons, Type]]
                    ): List[List[Pattern[Cons, Type]]] =
                      pat match {
                        case None                          => Nil
                        case Some(PositionalStruct(n, ps)) =>
                          assert(n == name)
                          assert(ps.size == paramSize)
                          ps :: Nil
                        case Some(Pattern.Union(h, t)) =>
                          (h :: t.toList).flatMap(p => unstruct(Some(p)))
                        // $COVERAGE-OFF$
                        case Some(Annotation(p, _)) => unstruct(Some(p))
                        case Some(Named(_, p))      => unstruct(Some(p))
                        case Some(unexpected)       =>
                          sys.error(
                            s"unexpected sub pattern of ($pos) in deunion: $unexpected"
                          )
                        // $COVERAGE-ON$
                      }

                    def solve(
                        p1: Option[Pattern[Cons, Type]],
                        p2: Option[Pattern[Cons, Type]]
                    ): Rel.SuperOrSame = {
                      val unionParams = (unstruct(p1) ::: unstruct(p2)).distinct
                      // pos <:> (p1 | p2) we can just element wise un

                      unionProduct
                        .relate(params :: Nil, unionParams)
                        .asInstanceOf[Rel.SuperOrSame]
                    }

                    Left(solve(_, _))
                }
              case lp @ ListPat(_) =>
                def optPatternToList(
                    p: Option[Pattern[Cons, Type]]
                ): List[ListPatCons] =
                  p match {
                    case Some(sp: ListPatCons)         => sp :: Nil
                    case Some(PositionalStruct(n, ps)) =>
                      optPatternToList(structToList(n, ps))
                    case Some(u @ Union(_, _)) =>
                      val (l, r) = u.split
                      optPatternToList(Some(l)) :::
                        optPatternToList(Some(r))
                    case Some(p @ (Annotation(_, _) | Named(_, _))) =>
                      optPatternToList(fromList(unwrap(p).toList))
                    case _ => Nil
                  }

                Left { (b, c) =>
                  val rhs = optPatternToList(b) ::: optPatternToList(c)
                  if (listPatternSetOps.missingBranches(lp :: Nil, rhs).isEmpty)
                    Rel.Same
                  else Rel.Super
                }
              case sp @ StrPat(_) =>
                def optPatternToStr(
                    p: Option[Pattern[Cons, Type]]
                ): List[StrPat] =
                  p match {
                    case Some(sp @ StrPat(_))      => sp :: Nil
                    case Some(Literal(Lit.Str(s))) =>
                      StrPat.fromLitStr(s) :: Nil
                    case Some(Union(h, t)) =>
                      optPatternToStr(Some(h)) :::
                        optPatternToStr(Some(Pattern.union(t.head, t.tail)))
                    case Some(p @ (Annotation(_, _) | Named(_, _))) =>
                      optPatternToStr(fromList(unwrap(p).toList))
                    case _ => Nil
                  }

                Left { (b, c) =>
                  val rhs = optPatternToStr(b) ::: optPatternToStr(c)
                  if (strPatternSetOps.missingBranches(sp :: Nil, rhs).isEmpty)
                    Rel.Same
                  else Rel.Super
                }
            }

        }
      val top: Option[Pattern[Cons, Type]] = Some(WildCard)

      def topFor(dt: DefinedType[Any]): List[PositionalStruct[Cons, Type]] =
        dt.constructors.map { cf =>
          PositionalStruct(
            (dt.packageName, cf.name),
            cf.args.map(_ => WildCard)
          )
        }

      def intersection(
          left: Pattern[Cons, Type],
          right: Pattern[Cons, Type]
      ): List[Pattern[Cons, Type]] =
        (left, right) match {
          case (_, _) if left == right => left :: Nil
          case (Var(va), Var(vb)) => Var(Ordering[Bindable].min(va, vb)) :: Nil
          case (Var(_), v)        => v :: Nil
          case (v, Var(_))        => v :: Nil
          case (Named(va, pa), Named(vb, pb)) if va == vb =>
            intersection(pa, pb).map(Named(va, _))
          case (Named(_, pa), r)        => intersection(pa, r)
          case (l, Named(_, pb))        => intersection(l, pb)
          case (WildCard, v)            => v :: Nil
          case (v, WildCard)            => v :: Nil
          case (Annotation(p, _), t)    => intersection(p, t)
          case (t, Annotation(p, _))    => intersection(t, p)
          case (Literal(a), Literal(b)) =>
            if (a === b) left :: Nil
            else Nil
          case (Literal(Lit.Str(s)), p @ StrPat(_)) =>
            if (p.matches(s)) left :: Nil
            else Nil
          case (p @ StrPat(_), Literal(Lit.Str(s))) =>
            if (p.matches(s)) right :: Nil
            else Nil
          case (p1 @ StrPat(_), p2 @ StrPat(_)) =>
            strPatternSetOps.intersection(p1, p2)
          case (lp: ListPatCons, rp: ListPatCons) =>
            listPatternSetOps.intersection(lp, rp)
          case (PositionalStruct(n, as), rp: ListPatCons) =>
            structToList(n, as) match {
              case Some(lp) => intersection(lp, rp)
              case None     =>
                if (isTop(rp)) left :: Nil
                else Nil
            }
          case (lp: ListPatCons, pos @ PositionalStruct(_, _)) =>
            intersection(pos, lp)
          case (PositionalStruct(ln, lps), PositionalStruct(rn, rps)) =>
            if (ln == rn) {
              val la = lps.size
              if (rps.size == la) {
                // the arity must match or check expr fails
                // if the arity doesn't match, just consider this
                // a mismatch
                unifyUnion(
                  getProd(la)
                    .intersection(lps, rps)
                    .map(PositionalStruct(ln, _))
                )
              } else Nil
            } else Nil
          case _ =>
            relate(left, right) match {
              case Rel.Disjoint   => Nil
              case Rel.Sub        => left :: Nil
              case Rel.Same       => normalizePattern(left) :: Nil
              case Rel.Super      => right :: Nil
              case Rel.Intersects =>
                // we know that neither left nor right can be a top
                // value because top >= everything
                //
                // non-trivial intersection
                (left, right) match {
                  case (Union(a, b), p) =>
                    val u = a :: b.toList
                    unifyUnion(u.flatMap(intersection(_, p)))
                  case (p, Union(a, b)) =>
                    val u = a :: b.toList
                    unifyUnion(u.flatMap(intersection(p, _)))
                  case _ =>
                    sys.error(
                      s"can't intersect and get here: intersection($left, $right)"
                    )
                }
            }
        }

      def difference(
          left: Pattern[Cons, Type],
          right: Pattern[Cons, Type]
      ): Patterns =
        relate(left, right) match {
          case Rel.Sub | Rel.Same => Nil
          case Rel.Disjoint       => left :: Nil
          case _                  =>
            lazy val leftIsTop = isTop(left)
            // left is a superset of right or intersects
            // so we are definitely not returning left or Nil
            // if we can exactly compute the difference.
            // also, right cannot be top, because nothing
            // is a strict superset or only intersects with top
            (left, right) match {
              case (Named(_, p), r)                        => difference(p, r)
              case (l, Named(_, p))                        => difference(l, p)
              case (Annotation(p, _), r)                   => difference(p, r)
              case (l, Annotation(p, _))                   => difference(l, p)
              case (left: ListPatCons, right: ListPatCons) =>
                listPatternSetOps.difference(left, right)
              case (_, listPat: ListPatCons) if leftIsTop =>
                // _ is the same as [*_] for well typed expressions
                listPatternSetOps.difference(
                  ListPat(ListPart.WildList :: Nil),
                  listPat
                )
              case (sa @ StrPat(_), Literal(Lit.Str(str))) =>
                strPatternSetOps.difference(sa, StrPat.fromLitStr(str))
              case (sa @ StrPat(_), sb @ StrPat(_)) =>
                strPatternSetOps.difference(sa, sb)
              case (_, right @ StrPat(_)) if leftIsTop =>
                // _ is the same as "${_}" for well typed expressions
                strPatternSetOps.difference(StrPat.Wild, right)
              case (_, Literal(Lit.Str(s))) if leftIsTop =>
                if (s.isEmpty) {
                  // "${_}" - "" == "$.{_}${_}"
                  strPatternSetOps.difference(
                    StrPat.Wild,
                    StrPat.fromLitStr("")
                  )
                } else {
                  // this is not(str), but we can't represent that, :(
                  topList
                }
              // below here it is starting to get complex
              case (Union(_, _), _) if leftIsTop =>
                difference(WildCard, right)
              // recall none of the rhs are tops, so we don't need to unify
              // before difference
              case (Union(a, b), _) =>
                unifyUnion(
                  differenceAll(
                    a :: b.toList,
                    Pattern.flatten(right).toList
                  )
                )
              case (left, Union(_, _)) =>
                val u = Pattern.flatten(right).toList
                unifyUnion(differenceAll(left :: Nil, u))
              case (PositionalStruct(ln, lp), PositionalStruct(rn, rp))
                  if ln == rn =>
                val la = lp.size
                if (rp.size == la) {
                  // the arity must match or check expr fails
                  // if the arity doesn't match, just consider this
                  // a mismatch
                  unifyUnion(
                    getProd(la)
                      .difference(lp, rp)
                      .map(PositionalStruct(ln, _))
                  )
                } else (left :: Nil)
              case (PositionalStruct(n, as), rp: ListPatCons) =>
                structToList(n, as) match {
                  case Some(lp) => difference(lp, rp)
                  case None     => left :: Nil
                }
              case (lp: ListPatCons, PositionalStruct(n, as)) =>
                structToList(n, as) match {
                  case Some(rp) => difference(lp, rp)
                  case None     => left :: Nil
                }
              case (_, PositionalStruct(nm, _)) if leftIsTop =>
                inEnv.definedTypeFor(nm) match {
                  case Some(dt) =>
                    topFor(dt).flatMap(difference(_, right))
                  case None =>
                    // just assume this is infinitely bigger than unknown
                    // types
                    topList
                }
              case (_, _) =>
                if (leftIsTop) topList
                else left :: Nil
            }
        }

      def isTop(p: Pattern[Cons, Type]): Boolean =
        p match {
          case Pattern.WildCard | Pattern.Var(_) => true
          case Pattern.Named(_, p)               => isTop(p)
          case Pattern.Annotation(p, _)          => isTop(p)
          case Pattern.Literal(_)    => false // literals are not total
          case s @ Pattern.StrPat(_) => strPatternSetOps.isTop(s)
          case l: ListPatCons        => listPatternSetOps.isTop(l)
          case Pattern.PositionalStruct(name, params) =>
            inEnv.definedTypeFor(name) match {
              case Some(dt) =>
                // we check at the beginning if the arity matches
                // so we don't need to check here
                dt.isStruct && params.forall(isTop)
              case None => false
            }
          case union @ Pattern.Union(_, _) =>
            unifyUnion(union :: Nil).exists(isTop)
        }

      override def subset(
          a0: Pattern[Cons, Type],
          b0: Pattern[Cons, Type]
      ): Boolean =
        relate(a0, b0).isSubtype

      def relate(a0: Pattern[Cons, Type], b0: Pattern[Cons, Type]): Rel = {
        def loop(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Rel =
          (a, b) match {
            case _ if a == b           => Rel.Same
            case (Named(_, p), _)      => loop(p, b)
            case (_, Named(_, p))      => loop(a, p)
            case (Annotation(p, _), _) => loop(p, b)
            case (_, Annotation(p, _)) => loop(a, p)
            case (_, u @ Union(_, _))  =>
              val utop = isTop(u)
              if (isTop(a)) {
                if (utop) Rel.Same
                else Rel.Super
              } else if (utop) Rel.Sub
              else {
                val (ua, ub) = u.split
                urm.unionRelCompare(Some(a), Some(ua), Some(ub))
              }
            case (Union(_, _), _) => loop(b, a).invert
            // All unions have been handled by this point
            case (Literal(Lit.Str(s)), sp @ Pattern.StrPat(_)) =>
              sp.toLiteralString match {
                case Some(rs) =>
                  if (s == rs) Rel.Same
                  else Rel.Disjoint
                case None =>
                  if (!sp.matches(s)) Rel.Disjoint
                  else Rel.Sub
              }
            case (Literal(_), _) =>
              if (isTop(b)) Rel.Sub
              else Rel.Disjoint
            case (_, Literal(_)) => loop(b, a).invert
            case (s1 @ Pattern.StrPat(_), s2 @ Pattern.StrPat(_)) =>
              strPatternSetOps.relate(s1, s2)
            case (s1 @ Pattern.StrPat(_), _) =>
              if (isTop(b)) {
                if (s1.isTotal) Rel.Same
                else Rel.Sub
              } else if (s1.isTotal) Rel.Super
              else Rel.Disjoint
            case (_, Pattern.StrPat(_))             => loop(b, a).invert
            case (l1: ListPatCons, l2: ListPatCons) =>
              listPatternSetOps.relate(l1, l2)
            case (lp: ListPatCons, Pattern.PositionalStruct(n, p)) =>
              structToList(n, p) match {
                case Some(right) => relate(lp, right)
                case None        =>
                  if (listPatternSetOps.isTop(lp)) Rel.Super
                  else Rel.Disjoint
              }
            case (Pattern.PositionalStruct(_, _), _: ListPatCons) =>
              relate(b, a).invert
            case (s1: ListPatCons, _) =>
              if (isTop(b)) {
                if (listPatternSetOps.isTop(s1)) Rel.Same
                else Rel.Sub
              } else if (listPatternSetOps.isTop(s1)) Rel.Super
              else Rel.Disjoint
            case (_, _: ListPatCons) => loop(b, a).invert
            case (
                  Pattern.PositionalStruct(ln, lp),
                  Pattern.PositionalStruct(rn, rp)
                ) =>
              if ((ln == rn) && (lp.size == rp.size)) {
                (lp.zip(rp).foldLeft(Rel.Same: Rel) { case (acc, (l, r)) =>
                  acc.lazyCombine(loop(l, r))
                })
              } else Rel.Disjoint
            case (Pattern.PositionalStruct(_, _), _) =>
              if (isTop(b)) {
                if (isTop(a)) Rel.Same
                else Rel.Sub
              } else Rel.Disjoint
            case (_, Pattern.PositionalStruct(_, _)) =>
              loop(b, a).invert
            case (Var(_) | WildCard, Var(_) | WildCard) =>
              Rel.Same
          }
        loop(a0, b0)
      }

      private def unwrap(
          p: Pattern[Cons, Type]
      ): NonEmptyList[Pattern[Cons, Type]] =
        p match {
          case Named(_, pat)      => unwrap(pat)
          case Annotation(pat, _) => unwrap(pat)
          case u @ Union(_, _)    => Pattern.flatten(u).flatMap(unwrap)
          case _                  => NonEmptyList.one(p)
        }

      // Invariant: this returns no top level unions
      def unifyUnion(u0: List[Pattern[Cons, Type]]): List[Pattern[Cons, Type]] =
        u0.flatMap(unwrap(_).toList) match {
          case Nil                      => Nil
          case singleton @ (one :: Nil) =>
            if (isTop(one)) topList else singleton
          case u =>
            val structsDs = u
              .collect { case Pattern.PositionalStruct(n, a) => (n, a) }
              .groupByNel { case (n, a) => (n, a.size) }
              .iterator
              .flatMap { case ((n, arity), as) =>
                val optDt = inEnv.definedTypeFor(n)
                getProd(arity)
                  .unifyUnion(as.toList.map(_._2))
                  .map { params =>
                    (optDt, Pattern.PositionalStruct(n, params))
                  }
              }
              .toList

            // See if all the structs together have created all the needed
            // items for a top value
            val hasTopStruct = structsDs
              .groupBy(_._1)
              .iterator
              .exists {
                case (Some(dt), dtsPs) =>
                  val topList = topFor(dt)
                  val structSet = dtsPs.map(_._2).toSet
                  topList.forall(structSet)
                case (None, _) => false
              }

            if (hasTopStruct) topList
            else {
              val structs = structsDs.map(_._2)
              val lps = listPatternSetOps.unifyUnion(u.collect {
                case lp: ListPatCons => lp
              })
              val sps = strPatternSetOps.unifyUnion(u.collect {
                case sp @ Pattern.StrPat(_) => sp
              })

              if (lps.exists(isTop) || sps.exists(isTop)) topList
              else {

                val strs = u.collect { case Pattern.Literal(ls @ Lit.Str(_)) =>
                  ls
                }

                val distinctStrs =
                  strs.distinct
                    .filterNot { s =>
                      sps.exists(_.matches(s.toStr))
                    }
                    .sortBy(_.toStr)
                    .map(Pattern.Literal(_))

                val notListStr = u.filterNot {
                  case Pattern.ListPat(_) | Pattern.StrPat(_) |
                      Pattern.Literal(Lit.Str(_)) |
                      Pattern.PositionalStruct(_, _) =>
                    true
                  case _ => false
                }.distinct

                if (notListStr.exists(isTop)) topList
                else
                  (lps ::: sps ::: distinctStrs ::: notListStr ::: structs).sorted
              }
            }
        }
    }

  /** recursively replace as much as possible with Wildcard This should match
    * exactly the same set for the same type as the previous pattern, without
    * any binding names
    */
  def normalizePattern(p: Pattern[Cons, Type]): Pattern[Cons, Type] =
    p match {
      case WildCard | Literal(_)       => p
      case Var(_)                      => WildCard
      case Named(_, p)                 => normalizePattern(p)
      case Annotation(p, _)            => normalizePattern(p)
      case _ if patternSetOps.isTop(p) => WildCard
      case u @ Union(_, _)             =>
        val flattened = Pattern.flatten(u).map(normalizePattern(_))

        patternSetOps.unifyUnion(flattened.toList) match {
          case h :: t => Pattern.union(h, t)
          case Nil    =>
            // $COVERAGE-OFF$
            sys.error("unreachable: union can't remove items")
          // $COVERAGE-ON$
        }
      case strPat @ StrPat(_) =>
        strPat.toLiteralString match {
          case Some(str) => Literal(Lit.Str(str))
          case None      => StrPat.fromSeqPattern(strPat.toSeqPattern)
        }
      case ListPat(parts) =>
        val p1 =
          parts.map {
            case Pattern.ListPart.WildList | Pattern.ListPart.NamedList(_) =>
              Pattern.ListPart.WildList
            case Pattern.ListPart.Item(p) =>
              Pattern.ListPart.Item(normalizePattern(p))
          }

        ListPat.fromSeqPattern(ListPat(p1).toSeqPattern)
      case PositionalStruct(n, params) =>
        val normParams = params.map(normalizePattern)
        structToList(n, normParams) match {
          case None     => PositionalStruct(n, normParams)
          case Some(lp) => lp
        }
    }

  /** This tells if two patterns for the same type would match the same values
    */
  val eqPat: Eq[Pattern[Cons, Type]] =
    new Eq[Pattern[Cons, Type]] {
      def eqv(l: Pattern[Cons, Type], r: Pattern[Cons, Type]) =
        patternSetOps.equiv(l, r)
    }
}
