package org.bykn.bosatsu

import cats.{Monad, Monoid}
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.{DataRepr, Type, RefSpace}

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr {
    def apply(expr: Expr): App =
      this match {
        case App(fn, a) => App(fn, a :+ expr)
        case notApp => App(notApp, NonEmptyList(expr, Nil))
      }
  }
  // these hold bindings either in the code, or temporary
  // local ones
  sealed abstract class RefExpr extends Expr
  sealed trait CheapExpr extends Expr
  sealed abstract class LocalTmp extends RefExpr {
    def ident: Long
  }

  sealed abstract class StrPart
  object StrPart {
    case object WildStr extends StrPart
    case object IndexStr extends StrPart
    case class LitStr(asString: String) extends StrPart
  }

  // we should probably allocate static slots for each bindable,
  // and replace the local with an integer offset slot access for
  // the closure state
  case class Lambda(captures: List[Bindable], arg: Bindable, expr: Expr) extends Expr

  // this is a tail recursive function that should be compiled into a loop
  // when a call to name is done inside body, that should restart the loop
  // the type of this Expr a function with the arity of args that returns
  // the type of body
  case class LoopFn(captures: List[Bindable], name: Bindable, argshead: Bindable, argstail: List[Bindable], body: Expr) extends Expr

  case class Global(pack: PackageName, name: Bindable) extends CheapExpr

  // these are immutable (but can be shadowed)
  case class Local(arg: Bindable) extends RefExpr with CheapExpr
  // these are is a separate namespace from Expr
  case class LocalAnon(ident: Long) extends LocalTmp with CheapExpr
  // these are mutable variables that can be updated while evaluating an IntExpr
  case class LocalAnonMut(ident: Long) extends LocalTmp with CheapExpr

  // we aggregate all the applications to potentially make dispatch more efficient
  // note fn is never an App
  case class App(fn: Expr, arg: NonEmptyList[Expr]) extends Expr
  case class Let(arg: Either[LocalTmp, (Bindable, RecursionKind)], expr: Expr, in: Expr) extends Expr
  case class Literal(lit: Lit) extends CheapExpr

  // these result in Int values which are also used as booleans
  // evaluating these CAN have side effects of mutating LocalAnon
  // variables.
  sealed abstract class IntExpr
  // returns 1 if it does, else 0
  case class EqualsLit(expr: Expr, lit: Lit) extends IntExpr
  case class EqualsInt(expr: IntExpr, int: Int) extends IntExpr
  case class EqualsNat(expr: Expr, nat: DataRepr.Nat) extends IntExpr
  // 1 if both are > 0
  case class AndInt(e1: IntExpr, e2: IntExpr) extends IntExpr
  // returns variant number from 0
  case class GetVariant(expr: Expr) extends IntExpr
  // handle list matching, this is a while loop, that is evaluting
  // lst is initialized to init, leftAcc is initialized to empty
  // tail until it is true while mutating lst => lst.tail
  // this has the side-effect of mutating lst and leftAcc as well as any side effects that check has
  // which could have nested searches of its own
  case class SearchList(lst: LocalAnonMut, init: CheapExpr, check: IntExpr, leftAcc: Option[LocalAnonMut]) extends IntExpr

  case class If(cond: IntExpr, thenExpr: Expr, elseExpr: Expr) extends Expr

  // string matching is complex done at a lower level
  // return a variant of either value 0, no match, or 1 with
  // todo: we should probably make this be like SearchList and be explicit
  // about the algorithm rather than hiding it and requiring an allocation
  // here.
  case class MatchString(arg: CheapExpr, parts: List[StrPart], binds: Int) extends Expr

  case class GetEnumElement(arg: Expr, variant: Int, index: Int, size: Int) extends Expr
  case class GetStructElement(arg: Expr, index: Int, size: Int) extends Expr

  sealed abstract class ConsExpr extends Expr
  // we need to compile calls to constructors into these
  case class MakeEnum(variant: Int, arity: Int) extends ConsExpr
  case class MakeStruct(arity: Int) extends ConsExpr
  case object ZeroNat extends ConsExpr
  // this is the function Nat -> Nat
  case object SuccNat extends ConsExpr

  case class PrevNat(of: Expr) extends Expr

  private def asCheap(expr: Expr): Option[CheapExpr] =
    expr match {
      case c: CheapExpr => Some(c)
      case _ => None
    }

  private def maybeMemo[F[_]: Monad](tmp: F[Long])(fn: CheapExpr => F[Expr]): Expr => F[Expr] =
    { arg: Expr =>
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
  private[this] val reverseFn = Global(PackageName.PredefName, Identifier.Name("reverse"))

  // drop all items in the tail after the first time fn returns true
  // as a result, we have 0 or 1 items where fn is true in the result
  // and it is always the last if there is 1
  def stopAt[A](nel: NonEmptyList[A])(fn: A => Boolean): NonEmptyList[A] =
    nel match {
      case NonEmptyList(h, _) if fn(h) => NonEmptyList(h, Nil)
      case s@NonEmptyList(_, Nil) => s
      case NonEmptyList(h0, h1 :: t) => h0 :: stopAt(NonEmptyList(h1, t))(fn)
    }

  def andInt(o1: Option[IntExpr], o2: Option[IntExpr]): Option[IntExpr] =
    (o1, o2) match {
      case (Some(e1), Some(e2)) => Some(AndInt(e1, e2))
      case (None, r) => r
      case (l, None) => l
    }

  // same as fromLet below, but uses RefSpace
  def fromLet[A](
    name: Bindable,
    rec: RecursionKind,
    te: TypedExpr[A])(
    variantOf: (PackageName, Constructor) => DataRepr): Expr =
      (for {
        c <- RefSpace.allocCounter
        expr <- fromLet(name, rec, te, variantOf, c)
      } yield expr).run.value

  // we need a TypeEnv to inline the creation of structs and variants
  def fromLet[F[_]: Monad, A](
    name: Bindable,
    rec: RecursionKind,
    te: TypedExpr[A],
    variantOf: (PackageName, Constructor) => DataRepr,
    makeAnon: F[Long]): F[Expr] = {

    val emptyExpr: Expr =
      empty match {
        case (p, c) =>
          variantOf(p, c) match {
            case DataRepr.Enum(v, s) => MakeEnum(v, s)
            case other =>
              /* We assume the structure of Lists to be standard linked lists
               * Empty cannot be a struct
               */

              // $COVERAGE-OFF$
              throw new IllegalStateException(s"empty List should be an enum, found: $other")
              // $COVERAGE-ON$
          }
      }

    def loopLetVal(name: Bindable, e: TypedExpr[A], rec: RecursionKind): F[Expr] = {
      lazy val e0 = loop(e)
      rec match {
        case RecursionKind.Recursive =>
          // this could be tail recursive
          if (TypedExpr.selfCallKind(name, e) == TypedExpr.SelfCallKind.TailCall) {
            val arity = Type.Fun.arity(e.getType)
            // we know that arity > 0 because, otherwise we can't have a total
            // self recursive loop
            if (arity <= 0) throw new IllegalStateException(s"expected arity > 0, found $arity in $e")
            else {
              TypedExpr.toArgsBody(arity, e) match {
                case Some((params, body)) =>
                  // we know params is non-empty because arity > 0
                  val args = params.map(_._1)
                  val argshead = args.head
                  val argstail = args.tail
                  val captures = TypedExpr.freeVars(body :: Nil).filterNot(args.toSet)
                  loop(body).map(LoopFn(captures, name, argshead, argstail, _))
                case None =>
                  // TODO: I don't think this case should ever happen
                  e0.map { value => Let(Right((name, RecursionKind.Recursive)), value, Local(name)) }
              }
            }
          }
          else {
            // otherwise let rec x = fn in x
            e0.map { value => Let(Right((name, RecursionKind.Recursive)), value, Local(name)) }
          }
        case RecursionKind.NonRecursive => e0
      }
    }

    def loop(te: TypedExpr[A]): F[Expr] =
      te match {
        case TypedExpr.Generic(_, expr, _) => loop(expr)
        case TypedExpr.Annotation(term, _, _) => loop(term)
        case TypedExpr.AnnotatedLambda(arg, _, res, _) =>
          val captures = TypedExpr.freeVars(res :: Nil).filterNot(_ === arg)
          loop(res).map(Lambda(captures, arg, _))
        case TypedExpr.Global(pack, cons@Constructor(_), _, _) =>
          Monad[F].pure(variantOf(pack, cons) match {
            case DataRepr.Enum(v, a) => MakeEnum(v, a)
            case DataRepr.Struct(a) => MakeStruct(a)
            case DataRepr.NewType => MakeStruct(1)
            case DataRepr.ZeroNat => ZeroNat
            case DataRepr.SuccNat => SuccNat
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
          (loop(arg), branches.traverse { case (p, te) => loop(te).map((p, _)) })
            .tupled
            .flatMap { case (a, b) => matchExpr(a, makeAnon, b) }
      }

    // return the check expression for the check we need to do, and the list of bindings
    def doesMatch(arg: CheapExpr, pat: Pattern[(PackageName, Constructor), Type]): F[NonEmptyList[(List[(LocalTmp, Expr)], Option[IntExpr], List[(Bindable, Expr)])]] = {
      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          Monad[F].pure(NonEmptyList((Nil, None, Nil), Nil))
        case Pattern.Literal(lit) =>
          Monad[F].pure(NonEmptyList((Nil, Some(EqualsLit(arg, lit)), Nil), Nil))
        case Pattern.Var(v) =>
          Monad[F].pure(NonEmptyList((Nil, None, (v, arg) :: Nil), Nil))
        case Pattern.Named(v, p) =>
          doesMatch(arg, p).map(_.map { case (l0, cond, bs) =>
            (l0, cond, (v, arg) :: bs)
          })
        case Pattern.StrPat(items) =>
          val sbinds: List[Bindable] =
            items
              .toList
              .collect {
                // that each name is distinct
                // should be checked in the SourceConverter/TotalityChecking code
                case Pattern.StrPart.NamedStr(n) => n
              }

          val me = MatchString(
            arg,
            items.toList.map {
              case Pattern.StrPart.NamedStr(n) => StrPart.IndexStr
              case Pattern.StrPart.WildStr => StrPart.WildStr
              case Pattern.StrPart.LitStr(s) => StrPart.LitStr(s)
            },
            sbinds.length)

          // if this matches 0 variant, we don't match, else we
          // if we match the variant, then treat it as a struct
          makeAnon.map { nm =>
            val boundMatch: LocalAnon = LocalAnon(nm)

            val variant = GetVariant(boundMatch)
            val vmatch = EqualsInt(variant, 1)

            NonEmptyList.of((
              (boundMatch, me) :: Nil,
              Some(vmatch),
              sbinds.mapWithIndex { case (b, idx) =>
                (b, GetEnumElement(boundMatch, 1, idx, sbinds.length))
              }))
          }
        case lp@Pattern.ListPat(_) =>

          lp.toPositionalStruct(empty, cons) match {
            case Right(p) => doesMatch(arg, p)
            case Left((glob, right@NonEmptyList(Pattern.ListPart.Item(_), _))) =>
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

              (leftF, makeAnon)
                .tupled
                .flatMap { case (optAnonLeft, tmpList) =>
                  val anonList = LocalAnonMut(tmpList)

                  doesMatch(anonList, Pattern.ListPat(right.toList))
                    .map { cases =>
                      cases.map {
                        case (preLet, Some(expr), binds) =>

                          val letTail = (anonList, arg) :: preLet

                          val (resLet, resBind, leftOpt) =
                            optAnonLeft match {
                              case Some((anonLeft, ln)) =>
                                val revList = reverseFn(anonLeft)
                                ((anonLeft, emptyExpr) :: letTail, (ln, revList) :: binds, Some(anonLeft))
                              case None =>
                                (letTail, binds, None)
                            }

                          (resLet, Some(SearchList(anonList, arg, expr, leftOpt)), resBind)
                        case (_, None, _) =>
                          // $COVERAGE-OFF$

                          // this shouldn't be possible, since there are no total list matches with
                          // one item since we recurse on a ListPat with the first item being Right
                          // which as we can see above always returns Some(_)
                          throw new IllegalStateException(s"$right should not be a total match")
                          // $COVERAGE-ON$
                      }
                    }
                }
            case Left((glob, right@NonEmptyList(_: Pattern.ListPart.Glob, _))) =>
              // we search on the right side, so the left will match nothing
              // this should be banned by SourceConverter/TotalityChecker because
              // it is confusing, but it can be handled

              // $COVERAGE-OFF$
              glob match {
                case Pattern.ListPart.WildList =>
                  // no binding on the let
                  doesMatch(arg, Pattern.ListPat(right.toList))
                case Pattern.ListPart.NamedList(ln) =>
                  // bind empty to ln
                  doesMatch(arg, Pattern.ListPat(right.toList))
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
          doesMatch(arg, p)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          def asStruct(getter: Int => Expr): F[NonEmptyList[(List[(LocalTmp, Expr)], Option[IntExpr], List[(Bindable, Expr)])]] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            val ands: F[List[NonEmptyList[(List[(LocalTmp, Expr)], Option[IntExpr], List[(Bindable, Expr)])]]] =
              params.zipWithIndex
                .traverse { case (pati, i) =>
                  // we don't want to repeatedly do the get on the recursion, so,
                  // set up a local to store the result of the get
                  for {
                    g <- makeAnon
                    localGet = LocalAnon(g)
                    expr = getter(i)
                    rec <- doesMatch(localGet, pati)
                  } yield rec.map { case (preLet, ex, b) => ((localGet, expr) :: preLet, ex, b) }
                }

            ands.map(NonEmptyList.fromList(_) match {
              case None => NonEmptyList((Nil, None, Nil), Nil)
              case Some(nel) => product(nel) { case ((l1, o1, b1), (l2, o2, b2)) =>
                (l1 ::: l2, andInt(o1, o2), b1 ::: b2)
              }
            })
          }

          variantOf(pack, cname) match {
            case DataRepr.Struct(size) =>
              // this is a struct, so we check each parameter
              asStruct { pos => GetStructElement(arg, pos, size) }
            case DataRepr.NewType =>
              // this is a struct, so we check each parameter
              asStruct { pos => GetStructElement(arg, pos, 1) }
            case DataRepr.Enum(vidx, size) =>
              // if we match the variant, then treat it as a struct
              val variant = GetVariant(arg)
              val vmatch = EqualsInt(variant, vidx)

              asStruct(GetEnumElement(arg, vidx, _, size))
                .map(_.map { case (l0, oi, b) => (l0, andInt(Some(vmatch), oi), b) })
            case DataRepr.ZeroNat =>
              Monad[F].pure(NonEmptyList((Nil, Some(EqualsNat(arg, DataRepr.ZeroNat)), Nil), Nil))
            case DataRepr.SuccNat =>
              params match {
                case single :: Nil =>
                  // if we match, we recur on the inner pattern and prev of current
                  val check = Some(EqualsNat(arg, DataRepr.SuccNat))
                  // TODO: we are unconditionally decrementing, even if doesMatch
                  // does not bind. We shouldn't do that.
                  val bind: F[(LocalAnon, Expr)] = makeAnon.map { nm => (LocalAnon(nm), PrevNat(arg)) }
                  for {
                    pre <- bind
                    rest <- doesMatch(pre._1, single)
                  } yield rest.map { case (preLets, cond, res) => (pre :: preLets, andInt(check, cond), res) }
                case other =>
                  // $COVERAGE-OFF$
                  throw new IllegalStateException(s"expected typechecked Nat to only have one param, found: $other in $pat")
                  // $COVERAGE-ON$
              }
          }
        case Pattern.Union(h, ts) =>
          (h :: ts).traverse(doesMatch(arg, _)).map { nene =>
            val nel = nene.flatten
            // at the first total match, we can stop
            stopAt(nel) { case (_, cond, _) => cond.isEmpty }
          }
      }
    }

    def lets(binds: List[(RefExpr, Expr)], in: Expr): Expr =
      binds.foldRight(in) { case ((refx, e), r) =>

        val arg = refx match {
          case Local(b) => Right((b, RecursionKind.NonRecursive))
          case tmp: LocalTmp => Left(tmp)
        }
        Let(arg, e, r)
      }

    def matchExpr(arg: Expr, tmp: F[Long], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {

      def recur(arg: CheapExpr, branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {
        val (p1, r1) = branches.head

        def loop(cbs: NonEmptyList[(List[(LocalTmp, Expr)], Option[IntExpr], List[(Bindable, Expr)])]): F[Expr] =
          cbs match {
            case NonEmptyList((b0, None, binds), _) =>
              // this is a total match, no fall through
              val allBinds = b0 ::: binds.map { case (b, e) => (Local(b), e) }
              Monad[F].pure(lets(allBinds, r1))
            case NonEmptyList((b0, Some(cond), binds), others) =>
              val matchBinds = binds.map { case (b, e) => (Local(b), e) }
              val thisBranch = lets(matchBinds, r1)
              val res = others match {
                case oh :: ot =>
                  loop(NonEmptyList(oh, ot)).map { te =>
                    If(cond, thisBranch, te)
                  }
                case Nil =>
                  branches.tail match {
                    case Nil =>
                      // this must be total
                      Monad[F].pure(thisBranch)
                    case bh :: bt =>
                      recur(arg, NonEmptyList(bh, bt)).map { te =>
                        If(cond, thisBranch, te)
                      }
                  }
              }

              res.map(lets(b0, _))
          }

        doesMatch(arg, p1).flatMap(loop)
      }

      val argFn = maybeMemo(tmp)(recur(_, branches))

      argFn(arg)
    }

    loopLetVal(name, te, rec)
  }

  // toy matcher to see the structure
  // Left means match any number of items, like *_
  def matchList[A, B: Monoid](items: List[A], pattern: List[Either[List[A] => B, A => Option[B]]]): Option[B] =
    pattern match {
      case Nil =>
        if (items.isEmpty) Some(Monoid[B].empty)
        else None
      case Right(fn) :: pt =>
        items match {
          case ih :: it =>
            fn(ih) match {
              case None => None
              case Some(b) => matchList(it, pt).map(Monoid[B].combine(b, _))
            }
          case Nil => None
        }

      case Left(lstFn) :: Nil =>
        Some(lstFn(items))

      case Left(lstFn) :: (pt@(Left(_) :: _)) =>
        // it is ambiguous how much to absorb
        // so, just assume lstFn gets nothing
        matchList(items, pt)
          .map(Monoid.combine(lstFn(Nil), _))

      case Left(lstFn) :: (pt@(Right(_) :: _))=>
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

  /**
   * return the expanded product of sums
   */
  def product[A1](sum: NonEmptyList[NonEmptyList[A1]])(prod: (A1, A1) => A1): NonEmptyList[A1] =
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
