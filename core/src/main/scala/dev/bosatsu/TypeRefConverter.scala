package dev.bosatsu

import cats.Applicative
import cats.data.NonEmptyList
import dev.bosatsu.rankn.Type
import dev.bosatsu.Identifier.Constructor

import cats.implicits._

object TypeRefConverter {

  /** given the ability to convert a name to a fully resolved type constant,
    * convert TypeRef to Type
    */
  def apply[F[_]: Applicative](
      t: TypeRef
  )(nameToType: Constructor => F[Type.Const]): F[Type] = {
    def toType(t: TypeRef): F[Type] = apply(t)(nameToType)

    import Type._
    import TypeRef._

    t match {
      case tv @ TypeVar(_)  => Applicative[F].pure(TyVar(tv.toBoundVar))
      case TypeName(n)      => nameToType(n.ident).map(TyConst(_))
      case TypeArrow(as, b) =>
        (as.traverse(toType(_)), toType(b)).mapN(Fun(_, _))
      case TypeApply(a, bs) =>
        (toType(a), bs.toList.traverse(toType)).mapN(Type.applyAll(_, _))
      case TypeForAll(pars, e) =>
        toType(e).map { te =>
          Type.forAll(
            pars.map { case (TypeVar(v), optK) =>
              val k = optK match {
                case None    => Kind.Type
                case Some(k) => k
              }
              (Type.Var.Bound(v), k)
            },
            te
          )
        }
      case TypeExists(pars, e) =>
        toType(e).map { te =>
          Type.exists(
            pars.map { case (TypeVar(v), optK) =>
              val k = optK match {
                case None    => Kind.Type
                case Some(k) => k
              }
              (Type.Var.Bound(v), k)
            },
            te
          )
        }
      case TypeTuple(ts) =>
        ts.traverse(toType).map(Type.Tuple(_))
    }
  }

  def fromTypeA[F[_]: Applicative](
      tpe: Type,
      onSkolem: Type.Var.Skolem => F[TypeRef],
      onMeta: Long => F[TypeRef],
      onConst: Type.Const.Defined => F[TypeRef]
  ): F[TypeRef] = {
    import Type._
    import TypeRef._

    def loop(tpe: Type) = fromTypeA(tpe, onSkolem, onMeta, onConst)

    tpe match {
      case ForAll(vs, in) =>
        val args = vs.map { case (Type.Var.Bound(b), k) =>
          val k1 = if (k.isType) None else Some(k)
          (TypeVar(b), k1)
        }
        loop(in).map(TypeForAll(args, _))
      case Exists(vs, in) =>
        val args = vs.map { case (Type.Var.Bound(b), k) =>
          val k1 = if (k.isType) None else Some(k)
          (TypeVar(b), k1)
        }
        loop(in).map(TypeExists(args, _))
      case Type.Tuple(ts) =>
        // this needs to be above TyConst
        ts.traverse(loop(_)).map(TypeTuple(_))
      case TyConst(defined @ Type.Const.Defined(_, _)) =>
        onConst(defined)
      case Type.Fun(args, to) =>
        (args.traverse(loop), loop(to)).mapN { (ftr, ttr) =>
          TypeArrow(ftr, ttr)
        }
      case ta @ TyApply(_, _) =>
        val (on, args) = unapplyAll(ta)
        (loop(on), args.traverse(loop)).mapN { (of, arg1) =>
          TypeApply(of, NonEmptyList.fromListUnsafe(arg1))
        }
      case TyVar(tv) =>
        tv match {
          case Type.Var.Bound(v) =>
            Applicative[F].pure(TypeVar(v))
          case sk: Type.Var.Skolem =>
            onSkolem(sk)
        }
      case TyMeta(Type.Meta(_, id, _, _)) =>
        onMeta(id)
    }
  }

}
