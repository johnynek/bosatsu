package org.bykn.bosatsu

import cats.implicits._
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}

import Value._

case class ValueToJson(getDefinedType: Type.Const => Option[DefinedType[Any]]) {

  def canEncodeToNull(t: Type): Boolean =
    t match {
      case Type.UnitType => true
      case Type.OptionT(inner) =>
        // if the inside of an Option cannot be null, we can use null
        // to represent None
        !canEncodeToNull(inner)
      case Type.ForAll(_, inner) => canEncodeToNull(inner)
      case _ => false
    }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  def toJson(a: Value, tpe: Type): Option[Json] =
    tpe match {
      case Type.IntType =>
        val ExternalValue(v) = a
        Some(Json.JNumberStr(v.toString))
      case Type.StrType =>
        val ExternalValue(v) = a
        Some(Json.JString(v.toString))
      case Type.BoolType =>
        a match {
          case True => Some(Json.JBool(true))
          case False => Some(Json.JBool(false))
          case other =>
            // $COVERAGE-OFF$this should be unreachable
            sys.error(s"invalid cast to Boolean: $other")
            // $COVERAGE-ON$
        }
      case Type.UnitType =>
        // encode this as null
        Some(Json.JNull)
      case opt@Type.OptionT(tpe) =>
        if (canEncodeToNull(opt)) {
          // not a nested option
          a match {
            case VOption(None) => Some(Json.JNull)
            case VOption(Some(a)) => toJson(a, tpe)
          }
        }
        else {
          // we can't encode Option[Option[T]] as null or not, so we encode
          // as list of 0 or 1 items
          a match {
            case VOption(None) => Some(Json.JArray(Vector.empty))
            case VOption(Some(a)) =>
              toJson(a, tpe).map { j => Json.JArray(Vector(j)) }
          }
        }
      case Type.ListT(t) =>
        val VList(vs) = a
        vs.toVector
          .traverse { v => toJson(v, t) }
          .map(Json.JArray(_))
      case Type.DictT(Type.StrType, vt) =>
        val VDict(d) = a
        d.toList.traverse { case (k, v) =>
          val Str(kstr) = k
          toJson(v, vt).map((kstr, _))
        }
        .map(Json.JObject(_))
      case Type.Tuple(ts) =>
        val Tuple(as) = a
        as.zip(ts)
          .toVector
          .traverse { case (a, t) =>
            toJson(a, t)
          }
          .map(Json.JArray(_))
      case Type.ForAll(_, inner) =>
        // we assume the generic positions don't matter and to continue
        toJson(a, inner)
      case _ =>
        val vp =
          a match {
            case s: SumValue => Some((s.variant, s.value))
            case p: ProductValue => Some((0, p))
            case _ => None
          }

        vp match {
          case None =>
            a match {
              case ExternalValue(b: BigInteger) =>
                Some(Json.JNumberStr(b.toString))
              case _ => None
            }
          case Some((variant, prod)) =>
            Type.rootConst(tpe)
              .flatMap {
                case Type.TyConst(const) => getDefinedType(const)
              }
              .flatMap { dt =>
                val cons = dt.constructors
                val (_, targs) = Type.applicationArgs(tpe)
                val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]
                cons.lift(variant).flatMap { cf =>
                  prod.toList.zip(cf.args).traverse { case (a1, (pn, t)) =>
                    toJson(a1, Type.substituteVar(t, replaceMap)).map((pn.asString, _))
                  }
                }
              }
              .map { ps => Json.JObject(ps) }
        }
    }
}
