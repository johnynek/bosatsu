package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.implicits._
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}
import scala.collection.mutable.{Map => MMap}

import Value._

import JsonEncodingError._

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
   *
   * Note, we statically build the conversion function if it is possible at
   * all, after that, only value errors can occur
   *
   * this code ASSUMES the type is correct. If not, we may return
   * incorrect data.
   */
  def toJson(tpe: Type): Either[UnsupportedType, Value => Either[ValueError, Json]] = {

    // Here we only cache successful runs so our path information doesn't interfere with caching
    val successCache: MMap[Type, Value => Either[ValueError, Json]] = MMap()

    def loop(tpe: Type, revPath: List[Type]): Either[UnsupportedType, Value => Either[ValueError, Json]] =
      successCache.get(tpe) match {
        case Some(res) => Right(res)
        case None =>
          val either: Either[UnsupportedType, Value => Either[ValueError, Json]] = tpe match {
            case Type.IntType =>
              Right({
                case ExternalValue(v) =>
                  Right(Json.JNumberStr(v.toString))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              })
            case Type.StrType =>
              Right({
                case ExternalValue(v) =>
                  Right(Json.JString(v.toString))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              })
            case Type.BoolType =>
              Right({
                case True => Right(Json.JBool(true))
                case False => Right(Json.JBool(false))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              })
            case Type.UnitType =>
              // encode this as null
              Right({
                case UnitValue => Right(Json.JNull)
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
                })
            case opt@Type.OptionT(t1) =>
              if (canEncodeToNull(opt)) {
                // not a nested option
                loop(t1, tpe :: revPath).map { inner =>

                  {
                    case VOption(None) => Right(Json.JNull)
                    case VOption(Some(a)) => inner(a)
                  }
                }
              }
              else {
                // we can't encode Option[Option[T]] as null or not, so we encode
                // as list of 0 or 1 items

                loop(t1, tpe :: revPath).map { inner =>

                  {
                    case VOption(None) => Right(Json.JArray(Vector.empty))
                    case VOption(Some(a)) => inner(a).map { j => Json.JArray(Vector(j)) }
                  }
                }
              }
            case Type.ListT(t) =>
              loop(t, tpe :: revPath).map { inner =>

                {
                  case VList(vs) =>
                    vs.toVector
                      .traverse(inner)
                      .map(Json.JArray(_))
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                    // $COVERAGE-ON$
                }
              }
            case Type.DictT(Type.StrType, vt) =>
              loop(vt, tpe :: revPath).map { inner =>

                {
                  case VDict(d) =>
                    d.toList.traverse { case (k, v) =>
                      k match {
                        case Str(kstr) => inner(v).map((kstr, _))
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
              }
            case Type.Tuple(ts) =>
              val p1 = tpe :: revPath
              val innersErr = ts.traverse(loop(_, p1))

              innersErr.map { inners =>

                {
                  case Tuple(as) =>
                    as.zip(inners)
                      .toVector
                      .traverse { case (a, fn) => fn(a) }
                      .map(Json.JArray(_))
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                    // $COVERAGE-ON$
                }
              }

            case Type.ForAll(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath)
            case otherType =>
              val fullPath = tpe :: revPath

              val const =
                Type.rootConst(tpe) match {
                  case Some(a) => Right(a)
                  case None =>
                    Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                }

            val resInner: Either[
              UnsupportedType,
              List[(Int, List[(String, Option[Value => Either[ValueError, Json]])])]
            ] =
                const
                  .flatMap {
                    case Type.TyConst(const) =>
                      getDefinedType(const) match {
                        case Some(dt) => Right(dt)
                        case None =>
                          Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                      }
                  }
                  .flatMap { dt =>
                    val cons = dt.constructors
                    val (_, targs) = Type.applicationArgs(tpe)
                    val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]

                    cons.zipWithIndex
                      .traverse { case (cf, idx) =>
                        cf.args.traverse { case (pn, t) =>
                          val subsT = Type.substituteVar(t, replaceMap)
                          // we have to handle recursion in types
                          val fnErr =
                            if ((t == otherType) || (subsT == otherType)) {
                              // we don't want to trigger building resFn
                              // we use None here to represent
                              // plugging in the result at the end
                              Right(None)
                            }
                            else loop(subsT, fullPath).map(Some(_))

                          fnErr.map((pn.asString, _))
                        }
                        .map { pair => (idx, pair) }
                      }
                  }

              val resFn: Either[UnsupportedType, Value => Either[ValueError, Json]] =
                resInner.map { idxRes =>

                  // This is lazy because it references fn below
                  lazy val mapping: Map[Int, List[(String, Value => Either[ValueError, Json])]] =
                    // if we are in here, all constituent parts can be solved
                      idxRes.iterator.map { case (idx, lst) =>
                        idx -> lst.map {
                          case (nm, None) => (nm, fn)
                          case (nm, Some(fn)) => (nm, fn)
                        }
                    }
                    .toMap

                  lazy val fn = { a: Value =>
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
                            // This is a hack to deal with NatLike types
                            Right(Json.JNumberStr(b.toString))
                          case other =>
                            Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                        }
                      case Some((variant, prod)) =>
                        mapping.get(variant) match {
                          case Some(fn) =>
                            prod.toList.zip(fn)
                              .traverse { case (p, (key, f)) =>
                                f(p).map((key, _))
                              }
                              .map { ps => Json.JObject(ps) }
                          case None =>
                            Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, a))
                        }
                    }
                 }

                 fn
              }

            resFn
          }

          either.foreach { fn => successCache.put(tpe, fn); () }
          either
      }

      loop(tpe, Nil)
    }
}

sealed abstract class JsonEncodingError
object JsonEncodingError {
  sealed abstract class ValueError extends JsonEncodingError

  final case class UnsupportedType(path: NonEmptyList[Type]) extends JsonEncodingError
  final case class IllTyped(path: List[Type], tpe: Type, value: Value) extends ValueError
}
