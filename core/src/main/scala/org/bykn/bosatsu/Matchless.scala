package org.bykn.bosatsu

import cats.{Monad, Monoid}
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.Type

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr
  // these result in Int values which are also used as booleans
  // evaluating these CAN have side effects of mutating LocalAnon
  // variables.
  sealed abstract class IntExpr extends Expr
  // these hold bindings either in the code, or temporary
  // local ones
  sealed abstract class RefExpr extends Expr
  sealed trait CheapExpr extends Expr

  sealed abstract class StrPart
  object StrPart {
    case object WildStr extends StrPart
    case object IndexStr extends StrPart
    case class LitStr(asString: String) extends StrPart
  }

  case class Lambda(captures: List[Identifier], arg: Bindable, expr: Expr) extends Expr
  case class GlobalName(pack: PackageName, name: Bindable) extends CheapExpr

  // these are immutable (but can be shadowed)
  case class LocalName(arg: Bindable) extends RefExpr with CheapExpr
  // these are is a separate namespace from Expr
  // these are mutable variables that can be updated while evaluating an IntExpr
  case class LocalAnon(ident: Long) extends RefExpr with CheapExpr

  case class App(fn: Expr, arg: Expr) extends Expr
  case class Let(arg: RefExpr, expr: Expr, in: Expr, recursive: RecursionKind) extends Expr
  case class Literal(lit: Lit) extends CheapExpr

  // returns 1 if it does, else 0
  case class EqualsLit(expr: Expr, lit: Lit) extends IntExpr
  case class EqualsInt(expr: Expr, int: Int) extends IntExpr
  // 1 if both are > 0
  case class AndInt(e1: IntExpr, e2: IntExpr) extends IntExpr
  // returns variant number from 0
  case class GetVariant(expr: Expr) extends IntExpr
  // handle list matching, this is a while loop, that is evaluting
  // tail until it is true while mutating lst => lst.tail
  // this has the side-effect of mutating lst and leftAcc as well as any side effects that check has
  // which could have nested searches of its own
  case class SearchList(lst: LocalAnon, check: IntExpr, leftAcc: Option[LocalAnon]) extends IntExpr

  case class If(cond: IntExpr, thenExpr: Expr, elseExpr: Expr) extends Expr

  // string matching is complex done at a lower level
  // return a variant of either value 0, no match, or 1 with
  case class MatchString(arg: Expr, parts: List[StrPart], binds: Int) extends Expr

  case class GetEnumElement(arg: Expr, variant: Int, index: Int, size: Int) extends Expr
  case class GetStructElement(arg: Expr, index: Int, size: Int) extends Expr
  // we need to compile calls to constructors into these
  case class MakeEnum(variant: Int, arity: Int) extends Expr
  case class MakeStruct(arity: Int) extends Expr

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
          } yield Let(bound, arg, res, RecursionKind.NonRecursive)
      }
    }

  private[this] val empty = (PackageName.PredefName, Constructor("EmptyList"))
  private[this] val cons = (PackageName.PredefName, Constructor("NonEmptyList"))
  private[this] val reverseFn = GlobalName(PackageName.PredefName, Identifier.Name("reverse"))

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
  // we need a TypeEnv to inline the creation of structs and variants
  def fromTypedExpr[F[_]: Monad, A](te: TypedExpr[A])(
    variantOf: (Option[PackageName], Constructor) => (Option[Int], Int), makeAnon: F[Long]): F[Expr] = {

    val emptyExpr: Expr =
      empty match {
        case (p, c) =>
          variantOf(Some(p), c) match {
            case (Some(v), s) => MakeEnum(v, s)
            case (None, s) => MakeStruct(s)
          }
      }

    def loop(te: TypedExpr[A]): F[Expr] =
      te match {
        case TypedExpr.Generic(_, expr, _) => loop(expr)
        case TypedExpr.Annotation(term, _, _) => loop(term)
        case TypedExpr.AnnotatedLambda(arg, _, res, _) =>
          loop(res).map(Lambda(TypedExpr.freeVars(res :: Nil), arg, _))
        case TypedExpr.Var(optP, cons@Constructor(_), _, _) =>
          Monad[F].pure(variantOf(optP, cons) match {
            case (Some(v), a) => MakeEnum(v, a)
            case (None, a) => MakeStruct(a)
          })
        case TypedExpr.Var(optPack, bind: Bindable, _, _) =>
          Monad[F].pure(optPack match {
            case Some(pack) => GlobalName(pack, bind)
            case None => LocalName(bind)
          })
        case TypedExpr.App(fn, a, _, _) => (loop(fn), loop(a)).mapN(App(_, _))
        case TypedExpr.Let(a, e, in, r, _) => (loop(e), loop(in)).mapN(Let(LocalName(a), _, _, r))
        case TypedExpr.Literal(lit, _, _) => Monad[F].pure(Literal(lit))
        case TypedExpr.Match(arg, branches, _) =>
          (loop(arg), branches.traverse { case (p, te) => loop(te).map((p, _)) })
            .tupled
            .flatMap { case (a, b) => matchExpr(a, makeAnon, b) }
      }

    // return the check expression for the check we need to do, and the list of bindings
    def doesMatch(arg: CheapExpr, pat: Pattern[(PackageName, Constructor), Type]): F[NonEmptyList[(List[(LocalAnon, Expr)], Option[IntExpr], List[(Bindable, Expr)])]] = {
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
              val leftF: F[Option[(LocalAnon, Bindable)]] =
                glob match {
                  case Pattern.ListPart.WildList =>
                    Monad[F].pure(None)
                  case Pattern.ListPart.NamedList(ln) =>
                    makeAnon.map { nm => Some((LocalAnon(nm), ln)) }
                }

              (leftF, makeAnon)
                .tupled
                .flatMap { case (optAnonLeft, tmpList) =>
                  val anonList = LocalAnon(tmpList)

                  doesMatch(anonList, Pattern.ListPat(right.toList))
                    .map { cases =>
                      cases.map {
                        case (preLet, Some(expr), binds) =>

                          val letTail = (anonList, arg) :: preLet

                          val (resLet, resBind, leftOpt) =
                            optAnonLeft match {
                              case Some((anonLeft, ln)) =>
                                val revList = App(reverseFn, anonLeft)
                                ((anonLeft, emptyExpr) :: letTail, (ln, revList) :: binds, Some(anonLeft))
                              case None =>
                                (letTail, binds, None)
                            }

                          (resLet, Some(SearchList(anonList, expr, leftOpt)), resBind)
                        case (_, None, _) =>
                          // this shouldn't be possible, since there are no total list matches with
                          // one item since we recurse on a ListPat with the first item being Right
                          // which as we can see above always returns Some(_)
                          throw new IllegalStateException(s"$right should not be a total match")
                      }
                    }
                }
            case Left((glob, right@NonEmptyList(_: Pattern.ListPart.Glob, _))) =>
              // we search on the right side, so the left will match nothing
              // this should be banned by SourceConverter/TotalityChecker because
              // it is confusing, but it can be handled
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
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          def asStruct(getter: Int => Expr): F[NonEmptyList[(List[(LocalAnon, Expr)], Option[IntExpr], List[(Bindable, Expr)])]] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            val ands: F[List[NonEmptyList[(List[(LocalAnon, Expr)], Option[IntExpr], List[(Bindable, Expr)])]]] =
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

            def product[A1](sum: NonEmptyList[NonEmptyList[A1]])(fn: (A1, A1) => A1): NonEmptyList[A1] =
              sum match {
                case NonEmptyList(h, Nil) =>
                  // this (a1 + a2 + a3) case
                  h
                case NonEmptyList(h0, h1 :: tail) =>
                  val rightProd = product(NonEmptyList(h1, tail))(fn)
                  // (a0 + a1 + ...) * rightProd
                  // = a0 * rightProd + a1 * rightProd + ...
                  for {
                    ai <- h0
                    r <- rightProd
                  } yield fn(ai, r)
              }

            ands.map(NonEmptyList.fromList(_) match {
              case None => NonEmptyList((Nil, None, Nil), Nil)
              case Some(nel) => product(nel) { case ((l1, o1, b1), (l2, o2, b2)) =>
                (l1 ::: l2, andInt(o1, o2), b1 ::: b2)
              }
            })
          }

          variantOf(Some(pack), cname) match {
            case (None, size) =>
              // this is a struct, so we check each parameter
              asStruct { pos => GetStructElement(arg, pos, size) }
            case (Some(vidx), size) =>
              // if we match the variant, then treat it as a struct
              val variant = GetVariant(arg)
              val vmatch = EqualsInt(variant, vidx)

              asStruct(GetEnumElement(arg, vidx, _, size))
                .map(_.map { case (l0, oi, b) => (l0, andInt(Some(vmatch), oi), b) })
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
        Let(refx, e, r, RecursionKind.NonRecursive)
      }

    def matchExpr(arg: Expr, tmp: F[Long], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {

      def recur(arg: CheapExpr, branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {
        val (p1, r1) = branches.head

        def loop(cbs: NonEmptyList[(List[(LocalAnon, Expr)], Option[IntExpr], List[(Bindable, Expr)])]): F[Expr] =
          cbs match {
            case NonEmptyList((b0, None, binds), _) =>
              // this is a total match, no fall through
              val allBinds = b0 ::: binds.map { case (b, e) => (LocalName(b), e) }
              Monad[F].pure(lets(allBinds, r1))
            case NonEmptyList((b0, Some(cond), binds), others) =>
              val matchBinds = binds.map { case (b, e) => (LocalName(b), e) }
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

    loop(te)
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
}
