package org.bykn.bosatsu

import cats.Applicative
import cats.data.NonEmptyList
import cats.implicits._
import cats.parse.{Parser => P}
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
        (toType(a), bs.toList.traverse(toType)).mapN(Type.applyAll(_, _))
      case TypeLambda(pars, e) =>
        toType(e).map { te =>
          Type.forAll(pars.map { case TypeVar(v) => Type.Var.Bound(v) }.toList, te)
        }
      case TypeTuple(ts) =>
        ts.traverse(toType).map(Type.Tuple(_))
    }
  }
}
