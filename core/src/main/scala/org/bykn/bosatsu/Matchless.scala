package org.bykn.bosatsu

import cats.Monad
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.Type

import Identifier.{Bindable, Constructor}

import cats.implicits._

object Matchless {
  sealed abstract class Expr
  sealed abstract class IntExpr extends Expr
  sealed abstract class RefExpr extends Expr

  sealed abstract class StrPart
  object StrPart {
    case object WildStr extends StrPart
    case object IndexStr extends StrPart
    case class LitStr(asString: String) extends StrPart
  }

  case class Lambda(captures: List[Identifier], arg: Bindable, expr: Expr) extends Expr
  case class GlobalName(pack: PackageName, name: Bindable) extends Expr
  case class LocalName(arg: Bindable) extends RefExpr
  // these are is a separate namespace from Expr
  case class LocalAnon(ident: Long) extends RefExpr
  case class App(fn: Expr, arg: Expr) extends Expr
  case class Let(arg: RefExpr, expr: Expr, in: Expr, recursive: RecursionKind) extends Expr
  case class Literal(lit: Lit) extends Expr

  // returns 1 if it does, else 0
  case class EqualsLit(expr: Expr, lit: Lit) extends IntExpr
  case class EqualsInt(expr: Expr, int: Int) extends IntExpr
  // 1 if both are > 0
  case class AndInt(e1: IntExpr, e2: IntExpr) extends IntExpr
  // returns variant number from 0
  case class GetVariant(expr: Expr) extends IntExpr
  case class If(cond: IntExpr, thenExpr: Expr, elseExpr: Expr) extends Expr

  // string matching is complex done at a lower level
  // return a variant of either value 0, no match, or 1 with
  case class MatchString(arg: Expr, parts: List[StrPart], binds: Int) extends Expr

  case class GetElement(arg: Expr, index: Int) extends Expr
  // we need to compile calls to constructors into these
  case class MakeVariant(variant: Int, arity: Int) extends Expr
  case class MakeStruct(arity: Int) extends Expr
  // this is an error state for code that should be unreachable due to compilation/typechecking
  case class Unreachable(message: String) extends Expr

  private def isCheap(expr: Expr): Boolean =
    expr match {
      case GlobalName(_, _) | LocalName(_) | Literal(_) | Unreachable(_) | LocalAnon(_) => true
      case _ => false
    }

  private[this] val empty = (PackageName.PredefName, Constructor("EmptyList"))
  private[this] val cons = (PackageName.PredefName, Constructor("NonEmptyList"))

  def andInt(o1: Option[IntExpr], o2: Option[IntExpr]): Option[IntExpr] =
    (o1, o2) match {
      case (Some(e1), Some(e2)) => Some(AndInt(e1, e2))
      case (None, r) => r
      case (l, None) => l
    }
  // we need a TypeEnv to inline the creation of structs and variants
  def fromTypedExpr[F[_]: Monad, A](te: TypedExpr[A])(
    variantOf: (Option[PackageName], Constructor) => (Option[Int], Int), makeAnon: F[Long]): F[Expr] = {
    def loop(te: TypedExpr[A]): F[Expr] =
      te match {
        case TypedExpr.Generic(_, expr, _) => loop(expr)
        case TypedExpr.Annotation(term, _, _) => loop(term)
        case TypedExpr.AnnotatedLambda(arg, _, res, _) =>
          loop(res).map(Lambda(TypedExpr.freeVars(res :: Nil), arg, _))
        case TypedExpr.Var(optP, cons@Constructor(_), _, _) =>
          Monad[F].pure(variantOf(optP, cons) match {
            case (Some(v), a) => MakeVariant(v, a)
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
    def doesMatch(arg: Expr, pat: Pattern[(PackageName, Constructor), Type]): NonEmptyList[(Option[IntExpr], List[(Bindable, Expr)])] = {
      pat match {
        case Pattern.WildCard =>
          // this is a total pattern
          NonEmptyList((None, Nil), Nil)
        case Pattern.Literal(lit) =>
          NonEmptyList((Some(EqualsLit(arg, lit)), Nil), Nil)
        case Pattern.Var(v) =>
          NonEmptyList((None, (v, arg) :: Nil), Nil)
        case Pattern.Named(v, p) =>
          doesMatch(arg, p).map { case (cond, bs) =>
            (cond, (v, arg) :: bs)
          }
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
          val boundMatch: LocalAnon = ???
          val variant = GetVariant(me)
          val vmatch = EqualsInt(variant, 1)
          // we need to add a Let wrapping the current branch,
          // can do that with a continuation we pass through
          val cont = { expr: Expr => Let(boundMatch, me, expr, RecursionKind.NonRecursive) }

          NonEmptyList.of((
            Some(vmatch),
            sbinds.mapWithIndex { case (b, idx) =>
              (b, GetElement(boundMatch, idx))
            }))
        case lp@Pattern.ListPat(_) =>
          lp.toPositionalStruct(empty, cons) match {
            case Some(p) => doesMatch(arg, p)
            case None => ???
          }

        case Pattern.Annotation(p, _) =>
          // we discard types at this point
          doesMatch(arg, p)
        case Pattern.PositionalStruct((pack, cname), params) =>
          // we assume the patterns have already been optimized
          // so that useless total patterns have been replaced with _
          def asStruct: NonEmptyList[(Option[IntExpr], List[(Bindable, Expr)])] = {
            // we have an and of a series of ors:
            // (m1 + m2 + m3) * (m4 + m5 + m6) ... =
            // we need to multiply them all out into a single set of ors
            val ands: List[NonEmptyList[(Option[IntExpr], List[(Bindable, Expr)])]] =
              params.zipWithIndex
                .map { case (pati, i) =>
                  val ei = GetElement(arg, i)
                  doesMatch(ei, pati)
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

            NonEmptyList.fromList(ands) match {
              case None => NonEmptyList((None, Nil), Nil)
              case Some(nel) => product(nel) { case ((o1, b1), (o2, b2)) =>
                (andInt(o1, o2), b1 ::: b2)
              }
            }
          }

          variantOf(Some(pack), cname) match {
            case (None, _) =>
              // this is a struct, so we check each parameter
              asStruct
            case (Some(vidx), _) =>
              // if we match the variant, then treat it as a struct
              val variant = GetVariant(arg)
              val vmatch = EqualsInt(variant, vidx)

              asStruct.map { case (oi, b) => (andInt(Some(vmatch), oi), b) }
          }
        case Pattern.Union(h, ts) =>
          (h :: ts).flatMap(doesMatch(arg, _))
      }
    }

    def matchExpr(arg: Expr, tmp: F[Long], branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {
      def maybeMemo: F[(Expr, Expr => Expr)] =
        if (isCheap(arg)) Monad[F].pure((arg, identity[Expr]))
        else tmp.map { nm =>
          val bound = LocalAnon(nm)

          val fn = { expr: Expr => Let(bound, arg, expr, RecursionKind.NonRecursive) }

          (bound, fn)
        }

      def recur(arg: Expr, branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], Expr)]): F[Expr] = {
        val (p1, r1) = branches.head

        def lets(binds: List[(Bindable, Expr)]): Expr =
          binds.foldRight(r1) { case ((b, e), r) =>
            Let(LocalName(b), e, r, RecursionKind.NonRecursive)
          }


        def loop(cbs: NonEmptyList[(Option[IntExpr], List[(Bindable, Expr)])]): F[Expr] =
          cbs match {
            case NonEmptyList((None, binds), _) =>
              // this is a total match, no fall through
              Monad[F].pure(lets(binds))
            case NonEmptyList((Some(cond), binds), others) =>
              val thisBranch = lets(binds)
              others match {
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
          }

        loop(doesMatch(arg, p1))
      }

      maybeMemo.flatMap { case (arg, continuation) =>
        recur(arg, branches).map(continuation)
      }
    }

    loop(te)
  }

}
