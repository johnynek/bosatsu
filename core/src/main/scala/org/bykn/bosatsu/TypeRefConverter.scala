package org.bykn.bosatsu

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.Identifier.Constructor

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
        @annotation.tailrec
        def toType1(fn: Type, args: NonEmptyList[Type]): Type =
          args match {
            case NonEmptyList(a0, Nil) => TyApply(fn,a0)
            case NonEmptyList(a0, a1 :: as) =>
              toType1(TyApply(fn, a0), NonEmptyList(a1, as))
          }
        (toType(a), bs.traverse(toType)).mapN(toType1(_, _))
      case TypeLambda(pars0, TypeLambda(pars1, e)) =>
        // we normalize to lifting all the foralls to the outside
        toType(TypeLambda(pars0 ::: pars1, e))
      case TypeLambda(pars, e) =>
        toType(e).map { te =>
          Type.forAll(pars.map { case TypeVar(v) => Type.Var.Bound(v) }.toList, te)
        }
      case TypeTuple(ts) =>
        ts.traverse(toType).map(Type.Tuple(_))
    }
  }
}
