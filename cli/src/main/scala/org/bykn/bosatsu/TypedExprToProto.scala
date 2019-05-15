package org.bykn.bosatsu

import _root_.bosatsu.TypedAst.{Type => ProtoType}
import org.bykn.bosatsu.rankn.Type
import scala.util.{Failure, Success, Try}

import cats.implicits._
/**
 * convert TypedExpr to and from Protobuf representation
 */
object ProtoConverter {
  def typeFromProto(p: ProtoType): Try[Type] = {
    import ProtoType.Value
    import bosatsu.TypedAst.{Type => _, _}

    def bound(n: String): Type.Var.Bound =
      Type.Var.Bound(n)

    p.value match {
      case Value.Empty =>
        Failure(new Exception("empty type found"))
      case Value.TypeConst(TypeConst(pack, t)) =>
        PackageName.parse(pack) match {
          case None =>
            Failure(new Exception(s"invalid package name: $pack, in $p"))
          case Some(pack) =>
            Try {
              val cons = Identifier.unsafeParse(Identifier.consParser, t)
              Type.TyConst(Type.Const.Defined(pack, TypeName(cons)))
            }
        }
      case Value.TypeVar(TypeVar(nm)) =>
        Success(Type.TyVar(bound(nm)))
      case Value.TypeForAll(TypeForAll(args, in)) =>
        in match {
          case None => Failure(new Exception(s"invalid: ForAll($args, $in)"))
          case Some(tpe) =>
            typeFromProto(tpe).map { in =>
              val boundArgs = args.iterator.map(bound).toList
              Type.forAll(boundArgs, in)
            }
        }
      case Value.TypeApply(TypeApply(left, right)) =>
        left.product(right) match {
          case None => Failure(new Exception(s"invalid TypeApply($left, $right)"))
          case Some((l, r)) =>
            (typeFromProto(l), typeFromProto(r)).mapN(Type.TyApply(_, _))
        }
    }
  }
  def typeToProto(p: Type): Try[ProtoType] = {
    import ProtoType.Value
    import bosatsu.TypedAst.{Type => _, _}

    p match {
      case Type.ForAll(bs, t) =>
        typeToProto(t).map { pt =>
          ProtoType(Value.TypeForAll(TypeForAll(bs.toList.map(_.name), Some(pt))))
        }
      case Type.TyApply(on, arg) =>
        (typeToProto(on), typeToProto(arg)).mapN { (o, a) =>
          ProtoType(Value.TypeApply(TypeApply(Some(o), Some(a))))
        }
      case Type.TyConst(Type.Const.Defined(p, n)) =>
        Success(ProtoType(Value.TypeConst(TypeConst(p.asString, n.ident.asString))))
      case Type.TyVar(Type.Var.Bound(n)) =>
        Success(ProtoType(Value.TypeVar(TypeVar(n))))
      case Type.TyVar(Type.Var.Skolem(_, _)) | Type.TyMeta(_) =>
        Failure(new Exception(s"invalid type to serialize: $p"))
    }
  }
}
