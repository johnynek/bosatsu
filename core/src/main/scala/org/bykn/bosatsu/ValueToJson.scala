package org.bykn.bosatsu

import cats.data.NonEmptyList
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
   * this code ASSUMES the type is correct. If not, we may return
   * incorrect data.
   */
  def toJson(a: Value, tpe: Type): Either[JsonEncodingError, Json] = {
    def loop(a: Value, tpe: Type, revPath: List[Type]): Either[JsonEncodingError, Json] =
      tpe match {
        case Type.IntType =>
          a match {
            case ExternalValue(v) =>
              Right(Json.JNumberStr(v.toString))
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }
        case Type.StrType =>
          a match {
            case ExternalValue(v) =>
              Right(Json.JString(v.toString))
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }
        case Type.BoolType =>
          a match {
            case True => Right(Json.JBool(true))
            case False => Right(Json.JBool(false))
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }
        case Type.UnitType =>
          // encode this as null
          Right(Json.JNull)
        case opt@Type.OptionT(t1) =>
          if (canEncodeToNull(opt)) {
            // not a nested option
            a match {
              case VOption(None) => Right(Json.JNull)
              case VOption(Some(a)) => loop(a, t1, tpe :: revPath)
            }
          }
          else {
            // we can't encode Option[Option[T]] as null or not, so we encode
            // as list of 0 or 1 items
            a match {
              case VOption(None) => Right(Json.JArray(Vector.empty))
              case VOption(Some(a)) =>
                loop(a, t1, tpe :: revPath).map { j => Json.JArray(Vector(j)) }
            }
          }
        case Type.ListT(t) =>
          a match {
            case VList(vs) =>
              vs.toVector
                .traverse { v => loop(v, t, tpe :: revPath) }
                .map(Json.JArray(_))
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }
        case Type.DictT(Type.StrType, vt) =>
          a match {
            case VDict(d) =>
              d.toList.traverse { case (k, v) =>
                k match {
                  case Str(kstr) =>
                    loop(v, vt, tpe :: revPath).map((kstr, _))
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                    // $COVERAGE-ON$
                }
              }
              .map(Json.JObject(_))
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }
        case Type.Tuple(ts) =>
          val ras = a match {
            case Tuple(as) => Right(as)
            case other =>
              // $COVERAGE-OFF$this should be unreachable
              Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
          }

          ras.flatMap { as =>
            as.zip(ts)
              .toVector
              .traverse { case (a, t) =>
                loop(a, t, tpe :: revPath)
              }
          }
          .map(Json.JArray(_))
        case Type.ForAll(_, inner) =>
          // we assume the generic positions don't matter and to continue
          loop(a, inner, tpe :: revPath)
        case _ =>
          val vp =
            a match {
              case s: SumValue => Some((s.variant, s.value))
              case p: ProductValue => Some((0, p))
              case _ => None
            }

          def err[A]: Either[JsonEncodingError, A] =
            Left(JsonEncodingError.UnsupportedType(NonEmptyList.fromListUnsafe(tpe :: revPath).reverse))

          def toRes[A](opt: Option[A]) =
            opt match {
              case Some(a) => Right(a)
              case None => err
            }

          vp match {
            case None =>
              a match {
                case ExternalValue(b: BigInteger) =>
                  Right(Json.JNumberStr(b.toString))
                case _ => err
              }
            case Some((variant, prod)) =>
              toRes(Type.rootConst(tpe))
                .flatMap {
                  case Type.TyConst(const) =>
                    getDefinedType(const) match {
                      case Some(dt) => Right(dt)
                      case None => err
                    }
                }
                .flatMap { dt =>
                  val cons = dt.constructors
                  val (_, targs) = Type.applicationArgs(tpe)
                  val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]
                  toRes(cons.lift(variant))
                    .flatMap { cf =>
                      prod.toList.zip(cf.args).traverse { case (a1, (pn, t)) =>
                        loop(a1, Type.substituteVar(t, replaceMap), tpe :: revPath).map((pn.asString, _))
                      }
                    }
                }
                .map { ps => Json.JObject(ps) }
          }
      }

      loop(a, tpe, Nil)
    }
}

sealed abstract class JsonEncodingError
object JsonEncodingError {
  final case class UnsupportedType(path: NonEmptyList[Type]) extends JsonEncodingError
  final case class IllTyped(path: List[Type], tpe: Type, value: Value) extends JsonEncodingError
}
