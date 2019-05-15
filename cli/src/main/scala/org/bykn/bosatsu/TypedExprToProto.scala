package org.bykn.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import org.bykn.bosatsu.rankn.{Type, DefinedType}
import scala.util.{Failure, Success, Try}

import cats.implicits._

/**
 * convert TypedExpr to and from Protobuf representation
 */
object ProtoConverter {
  def typeConstFromProto(p: proto.TypeConst): Try[Type.Const.Defined] = {
    val proto.TypeConst(pack, t) = p
    PackageName.parse(pack) match {
      case None =>
        Failure(new Exception(s"invalid package name: $pack, in $p"))
      case Some(pack) =>
        Try {
          val cons = Identifier.unsafeParse(Identifier.consParser, t)
          Type.Const.Defined(pack, TypeName(cons))
        }
    }
  }

  def typeConstToProto(tc: Type.Const): proto.TypeConst =
    tc match {
      case Type.Const.Defined(p, n) =>
        proto.TypeConst(p.asString, n.ident.asString)
    }

  def typeVarBoundToProto(tvb: Type.Var.Bound): proto.TypeVar =
    proto.TypeVar(tvb.name)

  def typeVarBoundFromProto(tv: proto.TypeVar): Type.Var.Bound =
    Type.Var.Bound(tv.varName)

  def typeFromProto(p: proto.Type): Try[Type] = {
    import proto.Type.Value
    import bosatsu.TypedAst.{Type => _, _}

    p.value match {
      case Value.Empty =>
        Failure(new Exception("empty type found"))
      case Value.TypeConst(tc) => typeConstFromProto(tc).map(Type.TyConst(_))
      case Value.TypeVar(tv) =>
        Success(Type.TyVar(typeVarBoundFromProto(tv)))
      case Value.TypeForAll(TypeForAll(args, in)) =>
        in match {
          case None => Failure(new Exception(s"invalid: ForAll($args, $in)"))
          case Some(tpe) =>
            typeFromProto(tpe).map { in =>
              val boundArgs = args.iterator.map(Type.Var.Bound(_)).toList
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
  def typeToProto(p: Type): Try[proto.Type] = {
    import proto.Type.Value
    import bosatsu.TypedAst.{Type => _, _}

    p match {
      case Type.ForAll(bs, t) =>
        typeToProto(t).map { pt =>
          proto.Type(Value.TypeForAll(TypeForAll(bs.toList.map(_.name), Some(pt))))
        }
      case Type.TyApply(on, arg) =>
        (typeToProto(on), typeToProto(arg)).mapN { (o, a) =>
          proto.Type(Value.TypeApply(TypeApply(Some(o), Some(a))))
        }
      case Type.TyConst(tcd) =>
        Success(proto.Type(Value.TypeConst(typeConstToProto(tcd))))
      case Type.TyVar(Type.Var.Bound(n)) =>
        Success(proto.Type(Value.TypeVar(TypeVar(n))))
      case Type.TyVar(Type.Var.Skolem(_, _)) | Type.TyMeta(_) =>
        Failure(new Exception(s"invalid type to serialize: $p"))
    }
  }

  def varianceToProto(v: Variance): proto.Variance =
    v match {
      case Variance.Phantom => proto.Variance.Phantom
      case Variance.Covariant => proto.Variance.Covariant
      case Variance.Contravariant => proto.Variance.Contravariant
      case Variance.Invariant => proto.Variance.Invariant
    }

  def varianceFromProto(p: proto.Variance): Try[Variance] =
    p match {
      case proto.Variance.Phantom => Success(Variance.Phantom)
      case proto.Variance.Covariant => Success(Variance.Covariant)
      case proto.Variance.Contravariant => Success(Variance.Contravariant)
      case proto.Variance.Invariant => Success(Variance.Invariant)
      case proto.Variance.Unrecognized(value) => Failure(new Exception(s"unrecognized value for variance: $value"))
    }

  def definedTypeToProto(d: DefinedType[Variance]): Try[proto.DefinedType] = {
    val tc = typeConstToProto(d.toTypeConst)
    def paramToProto(tv: (Type.Var.Bound, Variance)): proto.TypeParam =
      proto.TypeParam(Some(typeVarBoundToProto(tv._1)), varianceToProto(tv._2))

    val protoTypeParams: List[proto.TypeParam] = d.annotatedTypeParams.map(paramToProto)

    val constructors: Try[List[proto.ConstructorFn]] =
      d.constructors.traverse { case (c, tp, _) =>
        tp.traverse { case (b, t) =>
          typeToProto(t).map { tpe =>
            proto.FnParam(b.asString, Some(tpe))
          }
        }
        .map { params =>
          proto.ConstructorFn(c.asString, params)
        }
      }

    constructors.map(proto.DefinedType(Some(tc), protoTypeParams, _))
  }

  def definedTypeFromProto(pdt: proto.DefinedType): Try[DefinedType[Variance]] = {
    def paramFromProto(tp: proto.TypeParam): Try[(Type.Var.Bound, Variance)] =
      tp.typeVar match {
        case None => Failure(new Exception(s"expected type variable in $tp"))
        case Some(tv) =>
          varianceFromProto(tp.variance).map { v =>
            val tvb = typeVarBoundFromProto(tv)
            (tvb, v)
          }
      }

    def fnParamFromProto(p: proto.FnParam): Try[(Identifier.Bindable, Type)] =
      p.typeOf match {
        case None => Failure(new Exception(s"invalid FnParam, missing typeOf: $p"))
        case Some(tpe) =>
          Try(Identifier.unsafeParse(Identifier.bindableParser, p.name))
            .product(typeFromProto(tpe))
      }

    def consFromProto(tc: Type.Const.Defined, tp: List[Type.Var.Bound], c: proto.ConstructorFn): Try[(Identifier.Constructor, List[(Identifier.Bindable, Type)], Type)] = {
      Try(Identifier.unsafeParse(Identifier.consParser, c.name)).flatMap { cname =>
        //def
        c.params.toList.traverse(fnParamFromProto)
          .map { fnParams =>
            val fnType = DefinedType.constructorValueType(tc.packageName, tc.name, tp, fnParams.map(_._2))
            (cname, fnParams, fnType)
          }
      }
    }

    pdt.typeConst match {
      case None => Failure(new Exception(s"missing typeConst: $pdt"))
      case Some(tc) =>
        for {
          tconst <- typeConstFromProto(tc)
          tparams <- pdt.typeParams.toList.traverse(paramFromProto)
          cons <- pdt.constructors.toList.traverse(consFromProto(tconst, tparams.map(_._1), _))
        } yield DefinedType(tconst.packageName, tconst.name, tparams, cons)
    }
  }

  def interfaceToProto(iface: Package.Interface): Try[proto.Interface] = {
    val allDts = DefinedType.listToMap(
      iface.exports.flatMap { ex =>
        ex.tag match {
          case Referant.Value(_) => Nil
          case Referant.DefinedT(dt) => dt :: Nil
          case Referant.Constructor(_, dt, _, _) => dt :: Nil
        }
      }).mapWithIndex { (dt, idx) => (dt, idx) }

    val tryProtoDts = allDts
      .traverse { case (dt, _) => definedTypeToProto(dt) }
      .map(_.iterator.map(_._2).toList)

    def expNameToProto(e: ExportedName[Referant[Variance]]): Try[proto.ExportedName] = {
      val protoRef =
        e.tag match {
          case Referant.Value(t) =>
            typeToProto(t).map { pt =>
              proto.Referant(proto.Referant.Referant.Value(pt))
            }
          case Referant.DefinedT(dt) =>
            val key = (dt.packageName, dt.name)
            allDts.get(key) match {
              case Some((_, idx)) =>
                Success(
                  proto.Referant(proto.Referant.Referant.DefinedTypePtr(idx + 1))
                )
              case None => Failure(new Exception(s"missing defined type for $key in $e"))
            }
          case Referant.Constructor(nm, dt, _, _) =>
            val key = (dt.packageName, dt.name)
            allDts.get(key) match {
              case Some((dtV, dtIdx)) =>
                val cIdx = dtV.constructors.indexWhere { case (c, _, _) => c == nm }
                if (cIdx >= 0) {
                  Success(
                    proto.Referant(
                      proto.Referant.Referant.Constructor(
                        proto.ConstructorPtr(dtIdx + 1, cIdx + 1))))
                }
                else Failure(new Exception(s"missing contructor for type $key, $nm, with local: $dt"))
              case None => Failure(new Exception(s"missing defined type for $key in $e"))
            }
        }
      val exKind = e match {
        case ExportedName.Binding(b, _) => proto.ExportedName.Kind.Binding(b.asString)
        case ExportedName.TypeName(n, _) => proto.ExportedName.Kind.TypeName(n.asString)
        case ExportedName.Constructor(n, _) => proto.ExportedName.Kind.ConstructorName(n.asString)
      }

      protoRef.map { ref => proto.ExportedName(Some(ref), exKind) }
    }

    val tryExports = iface.exports.traverse(expNameToProto)

    (tryProtoDts, tryExports).mapN { (dts, exps) =>
      proto.Interface(iface.name.asString, dts, exps)
    }
  }

  private def referantFromProto[A](dts: Vector[DefinedType[A]], ref: proto.Referant): Try[Referant[A]] = {
    def getDt(idx: Int): Try[DefinedType[A]] = {
      // idx is 1 based:
      val fixedIdx = idx - 1
      if ((fixedIdx < 0) || (fixedIdx >= dts.size))
        Failure(new Exception(s"invalid index: $idx in $ref, size: ${dts.size}"))
      else Success(dts(fixedIdx))
    }

    ref.referant match {
      case proto.Referant.Referant.Value(t) =>
        typeFromProto(t).map(Referant.Value(_))
      case proto.Referant.Referant.DefinedTypePtr(idx) =>
        getDt(idx).map(Referant.DefinedT(_))
      case proto.Referant.Referant.Constructor(proto.ConstructorPtr(dtIdx, cIdx)) =>
        getDt(dtIdx).flatMap { dt =>
          // cIdx is 1 based:
          val fixedIdx = cIdx - 1
          dt.constructors.get(fixedIdx.toLong) match {
            case None =>
              Failure(new Exception(s"invalid constructor index: $cIdx in: $dt"))
            case Some((c, a, t)) =>
              Success(Referant.Constructor(c, dt, a, t))
          }
        }
      case proto.Referant.Referant.Empty => Failure(new Exception(s"empty referant found: $ref"))
    }
  }

  private def exportedNameFromProto[A](dts: Vector[DefinedType[A]], en: proto.ExportedName): Try[ExportedName[Referant[A]]] = {
    val tryRef = en.referant match {
      case Some(r) => referantFromProto(dts, r)
      case None => Failure(new Exception(s"missing referant in $en"))
    }

    tryRef.flatMap { ref =>
      en.kind match {
        case proto.ExportedName.Kind.Binding(s) =>
          Try(ExportedName.Binding(Identifier.unsafeParse(Identifier.bindableParser, s), ref))
        case proto.ExportedName.Kind.TypeName(n) =>
          Try(ExportedName.TypeName(Identifier.unsafeParse(Identifier.consParser, n), ref))
        case proto.ExportedName.Kind.ConstructorName(n) =>
          Try(ExportedName.Constructor(Identifier.unsafeParse(Identifier.consParser, n), ref))
        case proto.ExportedName.Kind.Empty => Failure(new Exception(s"empty exported name kind found in $ref"))
      }
    }
  }

  def interfaceFromProto(protoIface: proto.Interface): Try[Package.Interface] =
    PackageName.parse(protoIface.packageName) match {
      case None => Failure(new Exception(s"bad package name in: $protoIface"))
      case Some(pn) =>
        protoIface
          .definedTypes
          .toVector
          .traverse(definedTypeFromProto)
          .flatMap { dtVect =>
            val exports: Try[List[ExportedName[Referant[Variance]]]] =
              protoIface.exports.toList.traverse(exportedNameFromProto(dtVect, _))

            exports.map { exports =>
              Package(pn, Nil, exports, ())
            }
          }
    }
}
