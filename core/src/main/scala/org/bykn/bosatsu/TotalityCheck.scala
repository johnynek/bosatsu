package org.bykn.bosatsu

import cats.Eq
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import org.bykn.bosatsu.pattern.{SeqPattern, SeqPart}
import org.bykn.bosatsu.set.{Rel, Relatable, SetOps}

import org.bykn.bosatsu.graph.Memoize.memoizeDagHashed

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
        val twoAdj = lp.toSeqPattern.toList.sliding(2).exists {
          case Seq(SeqPart.Wildcard, SeqPart.Wildcard) => true
          case _ => false
        }
        val outer =
          if (!twoAdj) validUnit
          else Left(NonEmptyList(MultipleSplicesInPattern(lp, inEnv), Nil))

        val inners: Res[Unit] =
          parts.parTraverse_ {
            case ListPart.Item(p) => validatePattern(p)
            case _ => validUnit
          }

        (outer, inners).parMapN { (_, _) => () }

      case sp@StrPat(_) =>
        val simp = sp.toSeqPattern
        def hasAdjacentWild[A](seq: SeqPattern[A]): Boolean =
          seq match {
            case SeqPattern.Empty => false
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
        (nameCheck, argCheck).parMapN { (_, _) => () }

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
      case Generic(_, e) => checkExpr(e)
      case Lambda(_, e, _) => checkExpr(e)
      case Global(_, _, _) | Local(_, _) | Literal(_, _) => Validated.valid(())
      case App(fn, args, _) => checkExpr(fn) *> args.traverse_(checkExpr)
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

            val missing: ValidatedNel[ExprError[A], Unit] = {
              val mis = patternSetOps.missingBranches(WildCard :: Nil, patterns)
              NonEmptyList.fromList(mis) match {
                case Some(nel) =>
                  Validated.invalidNel(NonTotalMatch(m, nel): ExprError[A])
                case None => Validated.valid(())
              }
            }

            val unreachable: ValidatedNel[ExprError[A], Unit] = {
              val unr = patternSetOps.unreachableBranches(patterns)
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

  def missingBranches(p: Patterns): Patterns =
    patternSetOps.missingBranches(WildCard :: Nil, p)

  def intersection(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Patterns =
    patternSetOps.intersection(a, b)

  def difference(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Patterns =
    patternSetOps.difference(a, b)

  def isTotal(p: Patterns): Boolean =
    missingBranches(p).isEmpty

  private def structToList(n: Cons, args: List[Pattern[Cons, Type]]): Option[Pattern.ListPat[Cons, Type]] =
    (n, args) match {
      case ((PackageName.PredefName, Constructor("EmptyList")), Nil) => Some(Pattern.ListPat(Nil))
      case ((PackageName.PredefName, Constructor("NonEmptyList")), h :: t :: Nil) =>
        val tailRes = t match {
          case Pattern.PositionalStruct(n, a) =>
            structToList(n, a).map(_.parts)
          case Pattern.ListPat(parts) => Some(parts)
          case _  =>
            if (isTotal(t :: Nil)) Some(Pattern.ListPart.WildList :: Nil)
            else None
        }
        tailRes.map { t => Pattern.ListPat(Pattern.ListPart.Item(h) :: t) }
      case _ => None
    }

  private lazy val seqP: SetOps[SeqPattern[Pattern[Cons, Type]]] = {
    val sp = SeqPart.part1SetOps(patternSetOps)
    SeqPattern.seqPatternSetOps(sp, implicitly)
  }

  private lazy val listPatternSetOps: SetOps[ListPat[Cons, Type]] =
    SetOps.imap[SeqPattern[Pattern[Cons, Type]], ListPat[Cons, Type]](
      seqP,
      ListPat.fromSeqPattern(_),
      _.toSeqPattern)

  private val strPatternSetOps: SetOps[StrPat] =
    SetOps.imap[SeqPattern[Char], StrPat](
      SeqPattern.seqPatternSetOps(SeqPart.part1SetOps(SetOps.distinct[Char]), implicitly),
      StrPat.fromSeqPattern(_),
      _.toSeqPattern)

  private val getProd: Int => SetOps[List[Pattern[Cons, Type]]] =
    memoizeDagHashed[Int, SetOps[List[Pattern[Cons, Type]]]] {
      case (arity, _) if arity <= 0 => SetOps.unit(Nil)
      case (arity, rec) =>
        val head = patternSetOps
        val tail = rec(arity - 1)
        val prod = SetOps.product(head, tail)

        type L = List[Pattern[Cons, Type]]

        SetOps.imap[(Pattern[Cons, Type], L), L](
          prod,
          { case (h, t) => h :: t },
          {
            case h :: t => (h, t)
            case _ => sys.error(s"invalid arity: $arity, found empty list")
          })
    }

  lazy val patternSetOps: SetOps[Pattern[Cons, Type]] =
    new SetOps[Pattern[Cons, Type]] { self =>

      val urm = new Relatable.UnionRelModule[Option[Pattern[Cons, Type]]] {
        def relatable: Relatable[Option[Pattern[Cons,Type]]] =
          new Relatable[Option[Pattern[Cons, Type]]] {
            def relate(left: Option[Pattern[Cons,Type]], right: Option[Pattern[Cons,Type]]): Rel =
              (left, right) match {
                case (Some(l), Some(r)) => self.relate(l, r)
                case (None, None) => Rel.Same
                case (None, Some(_)) => Rel.Sub
                case (Some(_), None) => Rel.Super
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
              }  
              else Nil
            },
            solveOne = { prod =>
              (prod.zipWithIndex.collectFirstSome { case (param, idx) =>
                deunion(Some(param)) match {
                  case Right((Some(p1), Some(p2))) => Some(((p1, p2), idx))
                  case _ => None
                }
              }) match {
                case Some(((p1, p2), idx)) =>
                  // we can split:
                  Right((prod.updated(idx, p1), prod.updated(idx, p2)))
                case None =>
                  val prodSetOps = getProd(prod.size)

                  Left({ union =>
                    prodSetOps.missingBranches(prod :: Nil, union).isEmpty
                  })
              }
            })(new Relatable[List[Pattern[Cons, Type]]] {
              def relate(a: List[Pattern[Cons, Type]], b: List[Pattern[Cons, Type]]) = {
                val size = a.size
                if (size == b.size) getProd(size).relate(a, b)
                else Rel.Disjoint
              }
            })

        def cheapUnion(head: Option[Pattern[Cons, Type]], tail: List[Option[Pattern[Cons,Type]]]): Option[Pattern[Cons,Type]] =
          fromList((head :: tail).flatten)

        def fromList(pats: List[Pattern[Cons, Type]]): Option[Pattern[Cons, Type]] =
          pats match {
            case Nil => None
            case one :: Nil => Some(one)
            case h :: tail => Some(Pattern.union(h, tail))
          }

        // there are no empty patterns
        def isEmpty(a: Option[Pattern[Cons,Type]]): Boolean = a.isEmpty
        def intersect(a: Option[Pattern[Cons,Type]], b: Option[Pattern[Cons,Type]]): Option[Pattern[Cons,Type]] =
          (a, b) match {
            case (Some(l), Some(r)) =>
              fromList(self.intersection(l, r))
            case (None, _) => None
            case (_, None) => None
          }

        // we know that a is nonEmpty by contract
        def deunion(a: Option[Pattern[Cons,Type]]): Either[(Option[Pattern[Cons,Type]], Option[Pattern[Cons,Type]]) => Rel.SuperOrSame,(Option[Pattern[Cons,Type]], Option[Pattern[Cons,Type]])] =
          a.get match {
            case Pattern.Union(head, rest) =>
              Right((Some(head), Some(Pattern.union(rest.head, rest.tail))))
            case Pattern.Literal(_) =>
              // if a literal is >= something it is same, no partial supersets
              Left((_, _) => Rel.Same)
            case Named(_, pat) => deunion(Some(pat))
            case Annotation(pat, _) => deunion(Some(pat))
            case WildCard | Var(_) =>
              Left({(a, b) =>
                if (unifyUnion(a.toList ::: b.toList).exists(isTop)) Rel.Same
                else Rel.Super
              })
            case pos @ PositionalStruct(name, params) =>
              structToList(name, params) match {
                case Some(lp) => deunion(Some(lp))
                case None =>
                    // this isn't a list and is >= the union of two items
                    // so it has to be a struct with the same name
                    // assuming pos >= pat
                    // we know that pat is a Struct(name, _) or union of those
                    // decompose it into List representing the union, of a list
                    // of the fields

                    val paramSize = params.size

                    def unstruct(pat: Option[Pattern[Cons, Type]]): List[List[Pattern[Cons, Type]]] =
                      pat match {
                        case None => Nil
                        case Some(PositionalStruct(n, ps)) =>
                          assert(n == name)
                          assert(ps.size == paramSize)
                          ps :: Nil
                        case Some(Pattern.Union(h, t)) =>
                          (h :: t.toList).flatMap { p => unstruct(Some(p)) }
                        case Some(unexpected) =>
                          sys.error(s"unexpected sub pattern of ($pos) in deunion: $unexpected")
                      }
                    
                    def solve(p1: Option[Pattern[Cons,Type]], p2: Option[Pattern[Cons,Type]]): Rel.SuperOrSame = {
                      val unionParams = (unstruct(p1) ::: unstruct(p2)).distinct
                      // pos <:> (p1 | p2) we can just element wise un
                      
                      unionProduct.relate(params :: Nil, unionParams)
                        .asInstanceOf[Rel.SuperOrSame]
                    }

                    Left(solve(_, _))
                }
            case lp@ListPat(_) =>
              def optPatternToList(p: Option[Pattern[Cons, Type]]): List[ListPat[Cons, Type]] =
                p match {
                  case Some(sp @ ListPat(_)) => sp :: Nil
                  case Some(PositionalStruct(n, ps)) =>
                    optPatternToList(structToList(n, ps))
                  case Some(Union(h, t)) =>
                    optPatternToList(Some(h)) :::
                      optPatternToList(Some(Pattern.union(t.head, t.tail)))
                  case _ => Nil
                }

              Left(
                {(b, c) =>
                  val rhs = optPatternToList(b) ::: optPatternToList(c) 
                  if (listPatternSetOps.missingBranches(lp :: Nil, rhs).isEmpty) Rel.Same
                  else Rel.Super
                }
              )
            case sp@StrPat(_) =>
              def optPatternToStr(p: Option[Pattern[Cons, Type]]): List[StrPat] =
                p match {
                  case Some(sp @ StrPat(_)) => sp :: Nil
                  case Some(Union(h, t)) =>
                    optPatternToStr(Some(h)) :::
                      optPatternToStr(Some(Pattern.union(t.head, t.tail)))
                  case _ => Nil
                }

              Left(
                {(b, c) =>
                  val rhs = optPatternToStr(b) ::: optPatternToStr(c) 
                  if (strPatternSetOps.missingBranches(sp :: Nil, rhs).isEmpty) Rel.Same
                  else Rel.Super
                }
              )
          }

      }
      val top: Option[Pattern[Cons, Type]] = Some(WildCard)

      def intersection(
        left: Pattern[Cons, Type],
        right: Pattern[Cons, Type]): List[Pattern[Cons, Type]] =
          (left, right) match {
            case (Var(va), Var(vb)) => Var(Ordering[Bindable].min(va, vb)) :: Nil
            case (Var(_), v) => v :: Nil
            case (v, Var(_)) => v :: Nil
            case (Named(va, pa), Named(vb, pb)) if va == vb =>
              intersection(pa, pb).map(Named(va, _))
            case (Named(_, pa), r) => intersection(pa, r)
            case (l, Named(_, pb)) => intersection(l, pb)
            case (WildCard, v) => v :: Nil
            case (v, WildCard) => v :: Nil
            case (_, _) if left == right => left :: Nil
            case (Annotation(p, _), t) => intersection(p, t)
            case (t, Annotation(p, _)) => intersection(t, p)
            case (Literal(a), Literal(b)) =>
              if (a == b) left :: Nil
              else Nil
            case (Literal(Lit.Str(s)), p@StrPat(_)) =>
              if (p.matches(s)) left :: Nil
              else Nil
            case (p@StrPat(_), Literal(Lit.Str(s))) =>
              if (p.matches(s)) right :: Nil
              else Nil
            case (p1@StrPat(_), p2@StrPat(_)) =>
              strPatternSetOps.intersection(p1, p2)
            case (lp@ListPat(_), rp@ListPat(_)) =>
              listPatternSetOps.intersection(lp, rp)
            case (PositionalStruct(n, as), rp@ListPat(_)) =>
              structToList(n, as) match {
                case Some(lp) => intersection(lp, rp)
                case None =>
                  if (isTop(rp)) left :: Nil
                  else Nil
              }
            case (lp@ListPat(_), pos@PositionalStruct(_, _)) =>
              intersection(pos, lp)
            case (PositionalStruct(ln, lps), PositionalStruct(rn, rps)) =>
              if (ln == rn) {
                val la = lps.size
                if (rps.size == la) {
                  // the arity must match or check expr fails
                  // if the arity doesn't match, just consider this
                  // a mismatch
                  unifyUnion(getProd(la).intersection(lps, rps)
                    .map(PositionalStruct(ln, _): Pattern[Cons, Type]))
                }
                else Nil
              }
              else Nil
            case _ =>
              relate(left, right) match {
                case Rel.Disjoint => Nil
                case Rel.Sub  => left :: Nil
                case Rel.Same => normalizePattern(left) :: Nil
                case Rel.Super => right :: Nil
                case Rel.Intersects =>
                  // non-trivial intersection
                  (left, right) match {
                    case (Union(a, b), p) =>
                      val u = unifyUnion(a :: b.toList)
                      unifyUnion(u.flatMap(intersection(_, p)))
                    case (p, Union(a, b)) =>
                      val u = unifyUnion(a :: b.toList)
                      unifyUnion(u.flatMap(intersection(p, _)))
                    case _ =>
                      sys.error(s"can't intersect and get here: intersection($left, $right)")
                  }
              }
          }

      def difference(left: Pattern[Cons, Type], right: Pattern[Cons, Type]): Patterns =
        relate(left, right) match {
          case Rel.Sub | Rel.Same => Nil
          case Rel.Disjoint => left :: Nil
          case _ =>
            // left is a superset of right or intersects
            // so we are definitely not returning left or Nil
            (left, right) match {
              case (Named(_, p), r) => difference(p, r)
              case (l, Named(_, p)) => difference(l, p)
              case (Annotation(p, _), r) => difference(p, r)
              case (l, Annotation(p, _)) => difference(l, p)
              case (Var(v), listPat@ListPat(_)) =>
                // v is the same as [*v] for well typed expressions
                listPatternSetOps.difference(ListPat(ListPart.NamedList(v) :: Nil), listPat)
              case (left@ListPat(_), right@ListPat(_)) =>
                listPatternSetOps.difference(left, right)
              case (_, listPat@ListPat(_)) if isTop(left) =>
                // _ is the same as [*_] for well typed expressions
                listPatternSetOps.difference(ListPat(ListPart.WildList :: Nil), listPat)
              case (sa@StrPat(_), Literal(Lit.Str(str))) =>
                strPatternSetOps.difference(sa, StrPat.fromLitStr(str))
              case (sa@StrPat(_), sb@StrPat(_)) =>
                strPatternSetOps.difference(sa, sb)
              case (WildCard, right@StrPat(_)) =>
                // _ is the same as "${_}" for well typed expressions
                strPatternSetOps.difference(StrPat(NonEmptyList(StrPart.WildStr, Nil)), right)
              case (WildCard, Literal(Lit.Str(str))) =>
                difference(WildCard, StrPat.fromLitStr(str))
              case (Var(v), right@StrPat(_)) =>
                // v is the same as "${v}" for well typed expressions
                strPatternSetOps.difference(StrPat(NonEmptyList(StrPart.NamedStr(v), Nil)), right)
              // below here it is starting to get complex
              case (Union(a, b), Union(c, d)) =>
                unifyUnion(differenceAll(a :: b.toList, c :: d.toList))
              case (Union(a, b), right) =>
                val u = unifyUnion(a :: b.toList)
                unifyUnion(differenceAll(u, right :: Nil))
              case (left, Union(a, b)) =>
                val u = unifyUnion(a :: b.toList)
                unifyUnion(differenceAll(left :: Nil, u))
              case (PositionalStruct(ln, lp), PositionalStruct(rn, rp)) if ln == rn =>
                val la = lp.size
                if (rp.size == la) {
                  // the arity must match or check expr fails
                  // if the arity doesn't match, just consider this
                  // a mismatch
                  unifyUnion(getProd(la).difference(lp, rp)
                    .map(PositionalStruct(ln, _): Pattern[Cons, Type]))
                }
                else (left :: Nil)
              case (PositionalStruct(n, as), rp@ListPat(_)) =>
                structToList(n, as) match {
                  case Some(lp) => difference(lp, rp)
                  case None => left :: Nil
                }
              case (lp@ListPat(_), PositionalStruct(n, as)) =>
                structToList(n, as) match {
                  case Some(rp) => difference(lp, rp)
                  case None => left :: Nil
                }
              case (_, PositionalStruct(nm, _)) if isTop(left) =>
                inEnv.definedTypeFor(nm) match {
                  case None =>
                    // just assume this is infinitely bigger than unknown
                    // types
                    left :: Nil
                  case Some(dt) =>
                    dt.constructors.flatMap {
                      case cf if (dt.packageName, cf.name) == nm =>
                        // we can replace _ with Struct(_, _...)
                        val newWild = PositionalStruct(nm, cf.args.map(_ => WildCard))
                        difference(newWild, right)

                      case cf =>
                        // TODO, this could be smarter
                        // we need to learn how to deal with typed generics
                        def argToPat[A](t: (A, Type)): Pattern[Cons, Type] =
                          if (Type.hasNoVars(t._2)) Annotation(WildCard, t._2)
                          else WildCard

                        PositionalStruct((dt.packageName, cf.name), cf.args.map(argToPat)) :: Nil
                    }
                }
              case (_, _) =>
                // ill-typed
                if (isTop(left)) {
                  right match {
                    case StrPat(_) => difference(StrPat.Wild, right)
                    case ListPat(_) => difference(ListPat.Wild, right)
                    // we already checked for right as PositionalStruct
                    // we can't compute top - literal
                    case _ =>
                      // we can't solve this
                      left :: Nil
                  }
                }
                else left :: Nil
            }
          }

      def isTop(p: Pattern[Cons, Type]): Boolean =
        p match {
          case Pattern.WildCard | Pattern.Var(_) => true
          case Pattern.Named(_, p) => isTop(p)
          case Pattern.Annotation(p, _) => isTop(p)
          case Pattern.Literal(_) => false // literals are not total
          case s@Pattern.StrPat(_) => strPatternSetOps.isTop(s)
          case l@Pattern.ListPat(_) => listPatternSetOps.isTop(l)
          case Pattern.PositionalStruct(name, params) =>
            inEnv.definedTypeFor(name) match {
              case None =>
                false
              case Some(dt) =>
                // we check at the beginning if the arity matches
                // so we don't need to check here
                dt.isStruct && params.forall(isTop)
            }
          case Pattern.Union(h, t) =>
            val u = h :: t.toList
            u.exists(isTop(_)) || {
              self.missingBranches(WildCard :: Nil, u).isEmpty
            }
        }

      private def isTopCheap(p: Pattern[Cons, Type]): Boolean =
        p match {
          case Pattern.WildCard | Pattern.Var(_) => true
          case Pattern.Named(_, p) => isTopCheap(p)
          case Pattern.Annotation(p, _) => isTopCheap(p)
          case Pattern.Literal(_) => false // literals are not total
          case s@Pattern.StrPat(_) => strPatternSetOps.isTop(s)
          case l@Pattern.ListPat(_) => listPatternSetOps.isTop(l)
          case Pattern.PositionalStruct(name, params) =>
            inEnv.definedTypeFor(name) match {
              case None =>
                false
              case Some(dt) =>
                // we check at the beginning if the arity matches
                // so we don't need to check here
                dt.isStruct && params.forall(isTopCheap)
            }
          case Pattern.Union(h, t) =>
            val u = h :: t.toList
            u.exists(isTopCheap(_))
        }

      def subset(a0: Pattern[Cons, Type], b0: Pattern[Cons, Type]): Boolean =
        isTopCheap(b0) || relate(a0, b0).isSubtype

      override def relate(a0: Pattern[Cons,Type], b0: Pattern[Cons,Type]): Rel = {
        def loop(a: Pattern[Cons, Type], b: Pattern[Cons, Type]): Rel =
          (a, b) match {
            case _ if a == b => Rel.Same
            case (_, Union(h, t)) =>
              urm.unionRelCompare(
                Some(a),
                Some(h),
                Some(Pattern.union(t.head, t.tail)))
            case (Union(_, _), _) => loop(b, a).invert
            case (Literal(Lit.Str(s)), sp@Pattern.StrPat(_)) =>
              // due to normalization, StrPat is not a literal
              if (!sp.matches(s)) Rel.Disjoint
              else Rel.Sub
            case (Pattern.StrPat(_), Literal(Lit.Str(_))) =>
              // due to normalization, StrPat is not a literal
              loop(b, a).invert
            case (s1@Pattern.StrPat(_), s2@Pattern.StrPat(_)) =>
              strPatternSetOps.relate(s1, s2)
            case (l1@Pattern.ListPat(_), l2@Pattern.ListPat(_)) =>
              listPatternSetOps.relate(l1, l2)
            case (Pattern.PositionalStruct(ln, lp), Pattern.PositionalStruct(rn, rp)) =>
              if ((ln == rn) && (lp.size == rp.size)) {
                (lp.zip(rp).foldLeft(Rel.Same: Rel) { case (acc, (l, r)) =>
                  acc.lazyCombine(loop(l, r))
                })
              }
              else Rel.Disjoint
            case _ =>
              val ta = isTop(a)
              val tb = isTop(b)
              if (ta) {
                if (tb) Rel.Same
                else Rel.Super
              }
              else if (tb) {
                Rel.Sub
              }
              else Rel.Disjoint
          }

          loop(normalizePattern(a0), normalizePattern(b0))
        }

      def unifyUnion(u0: List[Pattern[Cons, Type]]): List[Pattern[Cons, Type]] =
        if (u0.exists(isTopCheap(_))) WildCard :: Nil
        else {

          val u = u0.flatMap { p => Pattern.flatten(normalizePattern(p)).toList }

          val lps = listPatternSetOps.unifyUnion(u.collect { case lp@Pattern.ListPat(_) => lp })
          val sps = strPatternSetOps.unifyUnion(u.collect { case sp@Pattern.StrPat(_) => sp })
          val strs = u.collect { case Pattern.Literal(ls@Lit.Str(_)) => ls }

          val distinctStrs =
            strs
              .distinct
              .filterNot { s =>
                sps.exists(_.matches(s.toStr))
              }
              .sortBy(_.toStr)
              .map(Pattern.Literal(_))

          val notListStr = u.filterNot {
            case Pattern.ListPat(_) | Pattern.StrPat(_) | Pattern.Literal(Lit.Str(_)) | Pattern.PositionalStruct(_, _) => true
            case _ => false
          }
          .distinct

          val structs = u
            .collect { case Pattern.PositionalStruct(n, a) => (n, a) }
            .groupByNel { case (n, a) => (n, a.size) }
            .iterator
            .flatMap { case ((n, arity), as) =>
              getProd(arity)
                .unifyUnion(as.toList.map(_._2))
                .map(Pattern.PositionalStruct(n, _): Pattern[Cons, Type])
            }
            .toList

          (lps ::: sps ::: distinctStrs :::  notListStr ::: structs).sorted
        }
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
      case Annotation(p, _) => normalizePattern(p)
      case u@Union(_, _) =>
        val flattened = Pattern.flatten(u).map(normalizePattern(_))

        patternSetOps.unifyUnion(flattened.toList) match {
          case h :: t => Pattern.union(h, t)
          case Nil =>
            // $COVERAGE-OFF$
            sys.error("unreachable: union can't remove items")
            // $COVERAGE-ON$
        }

      case _ if patternSetOps.isTop(p) => WildCard
      case strPat@StrPat(_) =>
        strPat.toLiteralString match {
          case Some(str) => Literal(Lit.Str(str))
          case None => StrPat.fromSeqPattern(strPat.toSeqPattern)
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
          case None => PositionalStruct(n, normParams)
          case Some(lp) => lp
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
