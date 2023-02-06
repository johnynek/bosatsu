package org.bykn.bosatsu

import cats.Applicative
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.Identifier.Constructor

import cats.implicits._

object TypeRefConverter {
  /**
   * given the ability to convert a name to a fully resolved
   * type constant, convert TypeRef to Type
   */
  def apply[F[_]: Applicative](t: TypeRef)(nameToType: Constructor => F[Type.Const]): F[Type] = {
    def toType(t: TypeRef): F[Type] = apply(t)(nameToType)

    import Type._
    import TypeRef._

    t match {
      case TypeVar(v) => Applicative[F].pure(TyVar(Type.Var.Bound(v)))
      case TypeName(n) => nameToType(n.ident).map(TyConst(_))
      case TypeArrow(a, b) => (toType(a), toType(b)).mapN(Fun(_, _))
      case TypeApply(a, bs) =>
        (toType(a), bs.toList.traverse(toType)).mapN(Type.applyAll(_, _))
      case TypeLambda(pars, e) =>
        toType(e).map { te =>
          // TODO Kind support
          Type.forAll(pars.map { case TypeVar(v) => (Type.Var.Bound(v), Kind.Type) }.toList, te)
        }
      case TypeTuple(ts) =>
        ts.traverse(toType).map(Type.Tuple(_))
    }
  }

  def fromTypeA[F[_]: Applicative](
    tpe: Type,
    onSkolem: Type.Var.Skolem => F[TypeRef],
    onMeta: Long => F[TypeRef],
    onConst: Type.Const.Defined => F[TypeRef]): F[TypeRef] = {
    import Type._
    import TypeRef._

    def loop(tpe: Type) = fromTypeA(tpe, onSkolem, onMeta, onConst)

    tpe match {
      case ForAll(vs, in) =>
        // TODO Kind support in TypeRef
        val args = vs.map { case (Type.Var.Bound(b), _) => TypeVar(b) }
        loop(in).map(TypeLambda(args, _))
      case Type.Tuple(ts) =>
        // this needs to be above TyConst
        ts.traverse(loop(_)).map(TypeTuple(_))
      case TyConst(defined@Type.Const.Defined(_, _)) =>
        onConst(defined)
      case Type.Fun(from, to) =>
        (loop(from), loop(to)).mapN { (ftr, ttr) =>
          TypeArrow(ftr, ttr)
        }
      case ta@TyApply(_, _) =>
        val (on, args) = unapplyAll(ta)
        (loop(on), args.traverse(loop)).mapN {
          (of, arg1) =>
            TypeApply(of, NonEmptyList.fromListUnsafe(arg1))
        }
      case TyVar(tv) =>
        tv match {
          case Type.Var.Bound(v) =>
            Applicative[F].pure(TypeVar(v))
          case sk@Type.Var.Skolem(_, _) =>
            onSkolem(sk)
        }
      case TyMeta(Type.Meta(id, _)) =>
        onMeta(id)
      // $COVERAGE-OFF$
      case other =>
        // the extractors mess this up
        sys.error(s"unreachable: $other")
        // $COVERAGE-ON$
    }
  }

}
