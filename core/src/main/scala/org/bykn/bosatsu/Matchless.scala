package org.bykn.bosatsu

import cats.{Monad, Monoid}
import cats.data.{Chain, NonEmptyList, WriterT}
import org.bykn.bosatsu.rankn.{DataRepr, Type, RefSpace}

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr {
    def apply(expr: Expr): App =
      this match {
        case App(fn, a) => App(fn, a :+ expr)
        case notApp     => App(notApp, NonEmptyList(expr, Nil))
      }
  }
  // these hold bindings either in the code, or temporary
  // local ones, note CheapExpr never trigger a side effect
  sealed trait CheapExpr extends Expr
  sealed abstract class FnExpr extends Expr

  sealed abstract class StrPart
  object StrPart {
    sealed abstract class Glob(val capture: Boolean) extends StrPart
    case object WildStr extends Glob(false)
    case object IndexStr extends Glob(true)
    case class LitStr(asString: String) extends StrPart
  }

  // we should probably allocate static slots for each bindable,
  // and replace the local with an integer offset slot access for
  // the closure state
  case class Lambda(captures: List[Bindable], arg: Bindable, expr: Expr)
      extends FnExpr

  // this is a tail recursive function that should be compiled into a loop
  // when a call to name is done inside body, that should restart the loop
  // the type of this Expr a function with the arity of args that returns
  // the type of body
  case class LoopFn(
      captures: List[Bindable],
      name: Bindable,
      argshead: Bindable,
      argstail: List[Bindable],
      body: Expr
  ) extends FnExpr

  case class Global(pack: PackageName, name: Bindable) extends CheapExpr

  // these are immutable (but can be shadowed)
  case class Local(arg: Bindable) extends CheapExpr
  // these are is a separate namespace from Expr
  case class LocalAnon(ident: Long) extends CheapExpr
  // these are mutable variables that can be updated while evaluating an BoolExpr
  case class LocalAnonMut(ident: Long) extends CheapExpr

  // we aggregate all the applications to potentially make dispatch more efficient
  // note fn is never an App
  case class App(fn: Expr, arg: NonEmptyList[Expr]) extends Expr
  case class Let(
      arg: Either[LocalAnon, (Bindable, RecursionKind)],
      expr: Expr,
      in: Expr
  ) extends Expr
  case class LetMut(name: LocalAnonMut, span: Expr) extends Expr
  case class Literal(lit: Lit) extends CheapExpr

  // these result in Int values which are also used as booleans
  // evaluating these CAN have side effects of mutating LocalAnon
  // variables.
  sealed abstract class BoolExpr {
    final def &&(that: BoolExpr): BoolExpr =
      (this, that) match {
        case (TrueConst, r) => r
        case (l, TrueConst) => l
        case _              => And(this, that)
      }
  }
  // returns 1 if it does, else 0
  case class EqualsLit(expr: CheapExpr, lit: Lit) extends BoolExpr
  case class EqualsNat(expr: CheapExpr, nat: DataRepr.Nat) extends BoolExpr
  // 1 if both are > 0
  case class And(e1: BoolExpr, e2: BoolExpr) extends BoolExpr
  // checks if variant matches, and if so, writes to
  // a given mut
  case class CheckVariant(
      expr: CheapExpr,
      expect: Int,
      size: Int,
      famArities: List[Int]
  ) extends BoolExpr
  // handle list matching, this is a while loop, that is evaluting
  // lst is initialized to init, leftAcc is initialized to empty
  // tail until it is true while mutating lst => lst.tail
  // this has the side-effect of mutating lst and leftAcc as well as any side effects that check has
  // which could have nested searches of its own
  case class SearchList(
      lst: LocalAnonMut,
      init: CheapExpr,
      check: BoolExpr,
      leftAcc: Option[LocalAnonMut]
  ) extends BoolExpr
  // set the mutable variable to the given expr and return true
  // string matching is complex done at a lower level
  case class MatchString(
      arg: CheapExpr,
      parts: List[StrPart],
      binds: List[LocalAnonMut]
  ) extends BoolExpr
  // set the mutable variable to the given expr and return true
  case class SetMut(target: LocalAnonMut, expr: Expr) extends BoolExpr
  case object TrueConst extends BoolExpr

  def hasSideEffect(bx: BoolExpr): Boolean =
    bx match {
      case SetMut(_, _) => true
      case TrueConst | CheckVariant(_, _, _, _) | EqualsLit(_, _) |
          EqualsNat(_, _) =>
        false
      case MatchString(_, _, b) => b.nonEmpty
      case And(b1, b2)          => hasSideEffect(b1) || hasSideEffect(b2)
      case SearchList(_, _, b, l) =>
        l.nonEmpty || hasSideEffect(b)
    }

  case class If(cond: BoolExpr, thenExpr: Expr, elseExpr: Expr) extends Expr
  case class Always(cond: BoolExpr, thenExpr: Expr) extends Expr
  def always(cond: BoolExpr, thenExpr: Expr): Expr =
    if (hasSideEffect(cond)) Always(cond, thenExpr)
    else thenExpr

  /** These aren't really super cheap, but when we treat them cheap we check
    * that we will only call them one time
    */
  case class GetEnumElement(arg: CheapExpr, variant: Int, index: Int, size: Int)
      extends CheapExpr
  case class GetStructElement(arg: CheapExpr, index: Int, size: Int)
      extends CheapExpr

  sealed abstract class ConsExpr extends Expr {
    def arity: Int
  }
  // we need to compile calls to constructors into these
  case class MakeEnum(variant: Int, arity: Int, famArities: List[Int])
      extends ConsExpr
  case class MakeStruct(arity: Int) extends ConsExpr
  case object ZeroNat extends ConsExpr {
    def arity = 0
  }
  // this is the function Nat -> Nat
  case object SuccNat extends ConsExpr {
    def arity = 1
  }

  case class PrevNat(of: Expr) extends Expr

  private def asCheap(expr: Expr): Option[CheapExpr] =
    expr match {
      case c: CheapExpr => Some(c)
      case _            => None
    }

  private def maybeMemo[F[_]: Monad](
      tmp: F[Long]
  )(fn: CheapExpr => F[Expr]): Expr => F[Expr] = { (arg: Expr) =>
    asCheap(arg) match {
      case Some(c) => fn(c)
      case None =>
        for {
          nm <- tmp
          bound = LocalAnon(nm)
          res <- fn(bound)
        } yield Let(Left(bound), arg, res)
    }
  }

  private[this] val empty = (PackageName.PredefName, Constructor("EmptyList"))
  private[this] val cons = (PackageName.PredefName, Constructor("NonEmptyList"))
  private[this] val reverseFn =
    Global(PackageName.PredefName, Identifier.Name("reverse"))

  // drop all items in the tail after the first time fn returns true
  // as a result, we have 0 or 1 items where fn is true in the result
  // and it is always the last if there is 1
  def stopAt[A](nel: NonEmptyList[A])(fn: A => Boolean): NonEmptyList[A] =
    nel match {
      case NonEmptyList(h, _) if fn(h) => NonEmptyList(h, Nil)
      case s @ NonEmptyList(_, Nil)    => s
      case NonEmptyList(h0, h1 :: t)   => h0 :: stopAt(NonEmptyList(h1, t))(fn)
    }

  // same as fromLet below, but uses RefSpace
  def fromLet[A](name: Bindable, rec: RecursionKind, te: TypedExpr[A])(
      variantOf: (PackageName, Constructor) => Option[DataRepr]
  ): Expr =
    (for {
      c <- RefSpace.allocCounter
      expr <- fromLet(name, rec, te, variantOf, c)
    } yield expr).run.value

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLet[F[_]: Monad, A](
      name: Bindable,
      rec: RecursionKind,
      te: TypedExpr[A],
      variantOf: (PackageName, Constructor) => Option[DataRepr],
      makeAnon: F[Long]
  ): F[Expr] = {

    type UnionMatch =
      NonEmptyList[(List[LocalAnonMut], BoolExpr, List[(Bindable, Expr)])]
    val wildMatch: UnionMatch = NonEmptyList((Nil, TrueConst, Nil), Nil)

    val emptyExpr: Expr =
      empty match {
        case (p, c) =>
          variantOf(p, c) match {
            case Some(DataRepr.Enum(v, s, f)) => MakeEnum(v, s, f)
            case other                        =>
              /* We assume the structure of Lists to be standard linked lists
               * Empty cannot be a struct
               */

              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"empty List should be an enum, found: $other"
              )
            // $COVERAGE-ON$
          }
      }

    def loopLetVal(
        name: Bindable,
        e: TypedExpr[A],
        rec: RecursionKind
    ): F[Expr] = {
      lazy val e0 = loop(e)
      rec match {
        case RecursionKind.Recursive =>
          def letrec(e: Expr): Expr =
            Let(Right((name, RecursionKind.Recursive)), e, Local(name))

          // this could be tail recursive
          if (
            TypedExpr.selfCallKind(name, e) == TypedExpr.SelfCallKind.TailCall
          ) {
            val arity = Type.Fun.arity(e.getType)
            // we know that arity > 0 because, otherwise we can't have a total
            // self recursive loop, but property checks send in ill-typed
            // e and so we handle that by checking for arity > 0
            TypedExpr.toArgsBody(arity, e) match {
              case Some((params, body)) if arity > 0 =>
                // we know params is non-empty because arity > 0
                val args = params.map(_._1)
                val argshead = args.head
                val argstail = args.tail
                val captures =
                  TypedExpr.freeVars(body :: Nil).filterNot(args.toSet)
                loop(body).map { v =>
                  letrec(LoopFn(captures, name, argshead, argstail, v))
                }
              case _ =>
                // TODO: I don't think this case should ever happen in real code
                // but it definitely does in fuzz tests
                e0.map(letrec)
            }
          } else {
            // otherwise let rec x = fn in x
            e0.map(letrec)
          }
        case RecursionKind.NonRecursive => e0
      }
    }

    def loop(te: TypedExpr[A]): F[Expr] =
      te match {
        case TypedExpr.Generic(_, expr)    => loop(expr)
        case TypedExpr.Annotation(term, _) => loop(term)
        case TypedExpr.AnnotatedLambda(arg, _, res, _) =>
          val captures = TypedExpr.freeVars(res :: Nil).filterNot(_ === arg)
          loop(res).map(Lambda(captures, arg, _))
        case TypedExpr.Global(pack, cons @ Constructor(_), _, _) =>
          Monad[F].pure(variantOf(pack, cons) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Enum(v, a, f) => MakeEnum(v, a, f)
                case DataRepr.Struct(a)     => MakeStruct(a)
                case DataRepr.NewType       => MakeStruct(1)
                case DataRepr.ZeroNat       => ZeroNat
                case DataRepr.SuccNat       => SuccNat
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          })
        case TypedExpr.Global(pack, notCons: Bindable, _, _) =>
          Monad[F].pure(Global(pack, notCons))
        case TypedExpr.Local(bind, _, _) =>
          Monad[F].pure(Local(bind))
        case TypedExpr.App(fn, a, _, _) =>
          (loop(fn), loop(a)).mapN(_(_))
        case TypedExpr.Let(a, e, in, r, _) =>
          (loopLetVal(a, e, r), loop(in)).mapN(Let(Right((a, r)), _, _))
        case TypedExpr.Literal(lit, _, _) => Monad[F].pure(Literal(lit))
        case TypedExpr.Match(arg, branches, _) =>
          (
            loop(arg),
            branches.traverse { case (p, te) => loop(te).map((p, _)) }
          ).tupled
            .flatMap { case (a, b) => matchExpr(a, makeAnon, b) }
      }

    /*
     * A simple pattern is either:
     * 1. one that has no binding what-so-ever
     * 2. a total binding to a given name
     * 3. or we return None indicating not one of these
     */
    def maybeSimple(
        p: Pattern[(PackageName, Constructor), Type]
    ): Option[Either[Bindable, Unit]] =
      p match {
        case Pattern.WildCard   => Some(Right(()))
        case Pattern.Literal(_) =>
          // Literals are never total
          None
        case Pattern.Var(v) => Some(Left(v))
        case Pattern.Named(v, p) =>
          maybeSimple(p) match {
            case Some(Right(_)) => Some(Left(v))
            case _              => None
          }
        case Pattern.StrPat(s) =>
          s match {
            case NonEmptyList(Pattern.StrPart.WildStr, Nil) => Some(Right(()))
            case NonEmptyList(Pattern.StrPart.NamedStr(n), Nil) => Some(Left(n))
            case _                                              => None
          }
        case Pattern.ListPat(l) =>
          l match {
            case Pattern.ListPart.WildList :: Nil     => Some(Right(()))
            case Pattern.ListPart.NamedList(n) :: Nil => Some(Left(n))
            case _                                    => None
          }
        case Pattern.Annotation(p, _)                    => maybeSimple(p)
        case Pattern.PositionalStruct((pack, cname), ps) =>
          // Only branch-free structs with no inner names are simple
          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(_) | DataRepr.NewType =>
                  ps.traverse(maybeSimple).flatMap { inners =>
                    if (inners.forall(_ === Right(()))) Some(Right(()))
                    else None
                  }
                case _ => None
              }
            // $COVERAGE-OFF$
            case None =>
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, t) =>
          (h :: t.toList).traverse(maybeSimple).flatMap { inners =>
            if (inners.forall(_ === Right(()))) Some(Right(()))
            else None
          }
      }

    // return the check expression for the check we need to do, and the list of bindings
    // if must match is true, we know that the pattern must match, so we can potentially remove some checks
    def doesMatch(
        arg: CheapExpr,
        pat: Pattern[(PackageName, Constructor), Type],
        mustMatch: Boolean
    ): F[UnionMatch] = {
      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          Monad[F].pure(wildMatch)
        case Pattern.Literal(lit) =>
          Monad[F].pure(NonEmptyList((Nil, EqualsLit(arg, lit), Nil), Nil))
        case Pattern.Var(v) =>
          Monad[F].pure(NonEmptyList((Nil, TrueConst, (v, arg) :: Nil), Nil))
        case Pattern.Named(v, p) =>
          doesMatch(arg, p, mustMatch).map(_.map { case (l0, cond, bs) =>
            (l0, cond, (v, arg) :: bs)
          })
        case Pattern.StrPat(items) =>
          val sbinds: List[Bindable] =
            items.toList
              .collect {
                // that each name is distinct
                // should be checked in the SourceConverter/TotalityChecking code
                case Pattern.StrPart.NamedStr(n) => n
              }

          val muts = sbinds.traverse { b =>
            makeAnon.map(LocalAnonMut(_)).map((b, _))
          }

          val pat = items.toList.map {
            case Pattern.StrPart.NamedStr(_) => StrPart.IndexStr
            case Pattern.StrPart.WildStr     => StrPart.WildStr
            case Pattern.StrPart.LitStr(s)   => StrPart.LitStr(s)
          }

          muts.map { binds =>
            val ms = binds.map(_._2)

            NonEmptyList.of((ms, MatchString(arg, pat, ms), binds))
          }
        case lp @ Pattern.ListPat(_) =>
          lp.toPositionalStruct(empty, cons) match {
            case Right(p) => doesMatch(arg, p, mustMatch)
            case Left(
                  (glob, right @ NonEmptyList(Pattern.ListPart.Item(_), _))
                ) =>
              // we have a non-trailing list pattern
              // to match, this becomes a search problem
              // we loop over all the matches of p in the list,
              // then we put the prefix on the glob, and the suffix against
              // the tail.
              //
              // we know all the bindings we will make, allocate
              // anons for them, do the loop, and then return
              // the boolean of did we match
              val leftF: F[Option[(LocalAnonMut, Bindable)]] =
                glob match {
                  case Pattern.ListPart.WildList =>
                    Monad[F].pure(None)
                  case Pattern.ListPart.NamedList(ln) =>
                    makeAnon.map { nm => Some((LocalAnonMut(nm), ln)) }
                }

              (leftF, makeAnon).tupled
                .flatMap { case (optAnonLeft, tmpList) =>
                  val anonList = LocalAnonMut(tmpList)

                  doesMatch(anonList, Pattern.ListPat(right.toList), false)
                    .map { cases =>
                      cases.map {
                        case (_, TrueConst, _) =>
                          // $COVERAGE-OFF$

                          // this shouldn't be possible, since there are no total list matches with
                          // one item since we recurse on a ListPat with the first item being Right
                          // which as we can see above always returns Some(_)
                          throw new IllegalStateException(
                            s"$right should not be a total match"
                          )
                        // $COVERAGE-ON$
                        case (preLet, expr, binds) =>
                          val letTail = anonList :: preLet

                          val (resLet, leftOpt, resBind) =
                            optAnonLeft match {
                              case Some((anonLeft, ln)) =>
                                val revList = reverseFn(anonLeft)
                                (
                                  anonLeft :: letTail,
                                  Some(anonLeft),
                                  (ln, revList) :: binds
                                )
                              case None =>
                                (letTail, None, binds)
                            }

                          (
                            resLet,
                            SearchList(anonList, arg, expr, leftOpt),
                            resBind
                          )
                      }
                    }
                }
            case Left(
                  (glob, right @ NonEmptyList(_: Pattern.ListPart.Glob, _))
                ) =>
              // we search on the right side, so the left will match nothing
              // this should be banned by SourceConverter/TotalityChecker because
              // it is confusing, but it can be handled

              // $COVERAGE-OFF$
              glob match {
                case Pattern.ListPart.WildList =>
                  // no binding on the let
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                case Pattern.ListPart.NamedList(ln) =>
                  // bind empty to ln
                  doesMatch(arg, Pattern.ListPat(right.toList), mustMatch)
                    .map { nel =>
                      nel.map { case (preLet, expr, binds) =>
                        (preLet, expr, (ln, emptyExpr) :: binds)
                      }
                    }
              }
            // $COVERAGE-ON$
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p, mustMatch)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          type Locals = Chain[(LocalAnonMut, Expr)]
          def asStruct(
              getter: Int => CheapExpr
          ): WriterT[F, Locals, UnionMatch] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            def operate(
                pat: Pattern[(PackageName, Constructor), Type],
                idx: Int
            ): WriterT[F, Locals, UnionMatch] =
              maybeSimple(pat) match {
                case Some(Right(())) =>
                  // this is a total match
                  WriterT.value(wildMatch)
                case Some(Left(v)) =>
                  // this is just an alias
                  WriterT.value(
                    NonEmptyList((Nil, TrueConst, (v, getter(idx)) :: Nil), Nil)
                  )
                case None =>
                  // we make an anonymous variable and write to that:
                  for {
                    nm <- WriterT.valueT[F, Locals, Long](makeAnon)
                    lam = LocalAnonMut(nm)
                    um <- WriterT.valueT[F, Locals, UnionMatch](
                      doesMatch(lam, pat, mustMatch)
                    )
                    // if this is a total match, we don't need to do the getter at all
                    chain =
                      if (um == wildMatch) Chain.empty
                      else Chain.one((lam, getter(idx)))
                    _ <- WriterT.tell[F, Locals](chain)
                  } yield um
              }

            val ands: WriterT[F, Locals, List[UnionMatch]] =
              params.zipWithIndex
                .traverse { case (pati, i) => operate(pati, i) }

            ands.map(NonEmptyList.fromList(_) match {
              case None => wildMatch
              case Some(nel) =>
                product(nel) { case ((l1, o1, b1), (l2, o2, b2)) =>
                  (l1 ::: l2, o1 && o2, b1 ::: b2)
                }
            })
          }

          def forStruct(size: Int) =
            asStruct { pos => GetStructElement(arg, pos, size) }.run
              .map { case (anons, ums) =>
                ums.map { case (pre, cond, bind) =>
                  val pre1 = anons.foldLeft(pre) { case (pre, (a, _)) =>
                    a :: pre
                  }
                  // we have to set these variables before we can evaluate the condition
                  val cond1 = anons.foldLeft(cond) { case (c, (a, e)) =>
                    SetMut(a, e) && c
                  }
                  (pre1, cond1, bind)
                }
              }

          variantOf(pack, cname) match {
            case Some(dr) =>
              dr match {
                case DataRepr.Struct(size)        => forStruct(size)
                case DataRepr.NewType             => forStruct(1)
                case DataRepr.Enum(vidx, size, f) =>
                  // if we match the variant, then treat it as a struct
                  val cv: BoolExpr =
                    if (mustMatch) TrueConst
                    else CheckVariant(arg, vidx, size, f)
                  asStruct { pos => GetEnumElement(arg, vidx, pos, size) }.run
                    .map { case (anons, ums) =>
                      if (ums == wildMatch) {
                        // we just need to check the variant
                        assert(
                          anons.isEmpty,
                          "anons must by construction always be empty on wildMatch"
                        )
                        NonEmptyList((Nil, cv, Nil), Nil)
                      } else {
                        // now we need to set up the binds if the variant is right
                        val cond1 = anons.foldLeft(cv) {
                          case (c, (mut, expr)) =>
                            c && SetMut(mut, expr)
                        }

                        ums.map { case (pre, cond, b) =>
                          val pre1 = anons.foldLeft(pre) {
                            case (pre, (mut, _)) => mut :: pre
                          }
                          (pre1, cond1 && cond, b)
                        }
                      }
                    }
                case DataRepr.ZeroNat =>
                  val cv: BoolExpr =
                    if (mustMatch) TrueConst
                    else EqualsNat(arg, DataRepr.ZeroNat)
                  Monad[F].pure(NonEmptyList((Nil, cv, Nil), Nil))
                case DataRepr.SuccNat =>
                  params match {
                    case single :: Nil =>
                      // if we match, we recur on the inner pattern and prev of current
                      val check =
                        if (mustMatch) TrueConst
                        else EqualsNat(arg, DataRepr.SuccNat)
                      for {
                        nm <- makeAnon
                        loc = LocalAnonMut(nm)
                        prev = PrevNat(arg)
                        rest <- doesMatch(loc, single, mustMatch)
                      } yield rest.map { case (preLets, cond, res) =>
                        (
                          loc :: preLets,
                          check && SetMut(loc, prev) && cond,
                          res
                        )
                      }
                    case other =>
                      // $COVERAGE-OFF$
                      throw new IllegalStateException(
                        s"expected typechecked Nat to only have one param, found: $other in $pat"
                      )
                    // $COVERAGE-ON$
                  }
              }
            case None =>
              // $COVERAGE-OFF$
              throw new IllegalStateException(
                s"could not find $cons in global data types"
              )
            // $COVERAGE-ON$
          }
        case Pattern.Union(h, ts) =>
          // note this list is exactly as long as h :: ts
          val unionMustMatch = NonEmptyList.fromListUnsafe(
            List.fill(ts.size)(false) ::: mustMatch :: Nil
          )
          ((h :: ts)
            .zip(unionMustMatch))
            .traverse { case (p, mm) => doesMatch(arg, p, mm) }
            .map { nene =>
              val nel = nene.flatten
              // at the first total match, we can stop
              stopAt(nel) {
                case (_, TrueConst, _) => true
                case _                 => false
              }
            }
      }
    }

    def lets(binds: List[(Bindable, Expr)], in: Expr): Expr =
      binds.foldRight(in) { case ((b, e), r) =>
        val arg = Right((b, RecursionKind.NonRecursive))
        Let(arg, e, r)
      }

    def checkLets(binds: List[LocalAnonMut], in: Expr): Expr =
      binds.foldLeft(in) { case (rest, anon) =>
        LetMut(anon, rest)
      }

    def matchExpr(
        arg: Expr,
        tmp: F[Long],
        branches: NonEmptyList[
          (Pattern[(PackageName, Constructor), Type], Expr)
        ]
    ): F[Expr] = {

      def recur(
          arg: CheapExpr,
          branches: NonEmptyList[
            (Pattern[(PackageName, Constructor), Type], Expr)
          ]
      ): F[Expr] = {
        val (p1, r1) = branches.head

        def loop(
            cbs: NonEmptyList[
              (List[LocalAnonMut], BoolExpr, List[(Bindable, Expr)])
            ]
        ): F[Expr] =
          cbs match {
            case NonEmptyList((b0, TrueConst, binds), _) =>
              // this is a total match, no fall through
              val right = lets(binds, r1)
              Monad[F].pure(checkLets(b0, right))
            case NonEmptyList((b0, cond, binds), others) =>
              val thisBranch = lets(binds, r1)
              val res = others match {
                case oh :: ot =>
                  loop(NonEmptyList(oh, ot)).map { te =>
                    If(cond, thisBranch, te)
                  }
                case Nil =>
                  branches.tail match {
                    case Nil =>
                      // this must be total, but we still need
                      // to evaluate cond since it can have side
                      // effects
                      Monad[F].pure(always(cond, thisBranch))
                    case bh :: bt =>
                      recur(arg, NonEmptyList(bh, bt)).map { te =>
                        If(cond, thisBranch, te)
                      }
                  }
              }

              res.map(checkLets(b0, _))
          }

        doesMatch(arg, p1, branches.tail.isEmpty).flatMap(loop)
      }

      val argFn = maybeMemo(tmp)(recur(_, branches))

      argFn(arg)
    }

    loopLetVal(name, te, rec)
  }

  // toy matcher to see the structure
  // Left means match any number of items, like *_
  def matchList[A, B: Monoid](
      items: List[A],
      pattern: List[Either[List[A] => B, A => Option[B]]]
  ): Option[B] =
    pattern match {
      case Nil =>
        if (items.isEmpty) Some(Monoid[B].empty)
        else None
      case Right(fn) :: pt =>
        items match {
          case ih :: it =>
            fn(ih) match {
              case None    => None
              case Some(b) => matchList(it, pt).map(Monoid[B].combine(b, _))
            }
          case Nil => None
        }

      case Left(lstFn) :: Nil =>
        Some(lstFn(items))

      case Left(lstFn) :: (pt @ (Left(_) :: _)) =>
        // it is ambiguous how much to absorb
        // so, just assume lstFn gets nothing
        matchList(items, pt)
          .map(Monoid.combine(lstFn(Nil), _))

      case Left(lstFn) :: (pt @ (Right(_) :: _)) =>
        var revLeft: List[A] = Nil
        var it = items
        var result: Option[B] = None
        // this cannot match an empty list
        while (result.isEmpty && it.nonEmpty) {
          matchList(it, pt) match {
            case Some(rb) =>
              val leftB = lstFn(revLeft.reverse)
              result = Some(Monoid[B].combine(leftB, rb))
            case None =>
              revLeft = it.head :: revLeft
              it = it.tail
          }
        }
        result
      /*
       * The above should be an imperative version
       * of this code. The imperative code
       * is easier to translate into low level
       * instructions
        items
          .toStream
          .mapWithIndex { (a, idx) => afn(a).map((_, idx)) }
          .collect { case Some(m) => m }
          .flatMap { case (b, idx) =>
            val left = items.take(idx)
            val leftB = Monoid[B].combine(lstFn(left), b)

            val right = items.drop(idx + 1)
            matchList(right, pt)
              .map { br =>
                Monoid[B].combine(leftB, br)
              }
          }
          .headOption
       */
    }

  /** return the expanded product of sums
    */
  def product[A1](
      sum: NonEmptyList[NonEmptyList[A1]]
  )(prod: (A1, A1) => A1): NonEmptyList[A1] =
    sum match {
      case NonEmptyList(h, Nil) =>
        // this (a1 + a2 + a3) case
        h
      case NonEmptyList(h0, h1 :: tail) =>
        val rightProd = product(NonEmptyList(h1, tail))(prod)
        // (a0 + a1 + ...) * rightProd
        // = a0 * rightProd + a1 * rightProd + ...
        for {
          ai <- h0
          r <- rightProd
        } yield prod(ai, r)
    }

}
