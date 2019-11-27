package org.bykn.bosatsu

import cats.Eval
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
   * Is a given type supported for Json conversion
   */
  def supported(t: Type): Either[UnsupportedType, Unit] = {
    // if we are currently working on a Type
    // we assume it is supported, and it isn't
    // if some other constituent type isn't
    val good = Right(())
    def loop(t: Type, working: List[Type]): Either[UnsupportedType, Unit] = {

      def bad: Either[UnsupportedType, Unit] =
        Left(UnsupportedType(NonEmptyList(t, working).reverse))

      t match {
        case _ if working.contains(t) => good
        case Type.IntType | Type.StrType | Type.BoolType | Type.UnitType => good
        case Type.OptionT(inner) => loop(inner, t :: working)
        case Type.ListT(inner) => loop(inner, t :: working )
        case Type.DictT(Type.StrType, inner) => loop(inner, t :: working)
        case Type.ForAll(_, inner) => loop(t, t :: working)
        case Type.TyVar(_) | Type.TyMeta(_) => bad
        case Type.Tuple(ts) =>
          val w1 = t :: working
          ts.traverse_(loop(_, w1))
        case consOrApply =>
          val w1 = consOrApply :: working
          Type.rootConst(consOrApply) match {
            case None => bad
            case Some(Type.TyConst(const)) =>
              getDefinedType(const) match {
                case None => bad
                case Some(dt) =>
                  val cons = dt.constructors
                  val (_, targs) = Type.applicationArgs(consOrApply)
                  val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  cons.traverse_ { cf =>
                    cf.args.traverse_ { case (_, t) =>
                      loop(Type.substituteVar(t, replaceMap), w1)
                    }
                  }
              }
          }
      }
    }
    loop(t, Nil)
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

    type Fn = Value => Either[ValueError, Json]
    // when we complete a custom type, we put it in here
    val successCache: MMap[Type, Eval[Fn]] = MMap()

    def get[A](e: Either[UnsupportedType, A]): A =
      e match {
        case Right(a) => a
        case Left(u) => sys.error(s"should have only called on a supported type: $u")
      }

    def loop(tpe: Type, revPath: List[Type]): Eval[Fn] =
      // we know we can support this, so when we recurse it
      // is safe to call .right.get
      successCache.get(tpe) match {
        case Some(fn) => fn
        case None =>
          val res: Eval[Fn] = Eval.later(tpe match {
            case Type.IntType =>
              {
                case ExternalValue(v: BigInteger) =>
                  Right(Json.JNumberStr(v.toString))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.StrType =>
              {
                case ExternalValue(v) =>
                  Right(Json.JString(v.toString))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.BoolType =>
              {
                case True => Right(Json.JBool(true))
                case False => Right(Json.JBool(false))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.UnitType =>
              // encode this as null
              {
                case UnitValue => Right(Json.JNull)
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
                }
            case opt@Type.OptionT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

              if (canEncodeToNull(opt)) {
              // not a nested option

                {
                  case VOption(None) => Right(Json.JNull)
                  case VOption(Some(a)) => inner(a)
                  case other =>
                    Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                }
              }
              else {

                {
                  case VOption(None) => Right(Json.JArray(Vector.empty))
                  case VOption(Some(a)) => inner(a).map { j => Json.JArray(Vector(j)) }
                  case other =>
                    Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                }
              }
            case Type.ListT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

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
            case Type.DictT(Type.StrType, vt) =>
              lazy val inner = loop(vt, tpe :: revPath).value

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
            case Type.Tuple(ts) =>
              val p1 = tpe :: revPath
              lazy val inners = ts.traverse { t => loop(t, p1) }.value

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

            case Type.ForAll(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath).value
            case otherType =>
              // We can have complicated recursion here, we
              // need to be careful with Eval.later/lazy to tie the knot
              val fullPath = tpe :: revPath

              val dt =
                get(Type.rootConst(tpe) match {
                  case Some(Type.TyConst(const)) =>
                    getDefinedType(const) match {
                      case Some(dt) => Right(dt)
                      case None =>
                        Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                    }
                  case None =>
                    Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                })

              dt.natLike match {
                case DefinedType.NatLike.Not =>
                  val resInner: Eval[Map[Int, List[(String, Fn)]]] = {
                    val cons = dt.constructors
                    val (_, targs) = Type.applicationArgs(tpe)
                    val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]

                    cons.zipWithIndex
                      .traverse { case (cf, idx) =>
                        val rec = cf.args.traverse { case (field, t) =>
                          val subsT = Type.substituteVar(t, replaceMap)
                          val next = loop(subsT, fullPath)
                          next.map { fn => (field.asString, fn) }
                        }
                        rec.map { fields => (idx, fields) }
                      }
                      .map(_.toMap)
                  }

                  if (dt.isNewType) {
                    lazy val inner = resInner.value.head._2.head._2

                    { v => inner(v) }
                  }
                  else if (dt.isStruct) {
                    lazy val productsInner = resInner.value.head._2

                    { case prod: ProductValue =>
                        prod.toList.zip(productsInner)
                          .traverse { case (p, (key, f)) =>
                            f(p).map((key, _))
                          }
                          .map { ps => Json.JObject(ps) }

                      case other =>
                        Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                    }
                  }
                  else {
                    lazy val mapping: Map[Int, List[(String, Fn)]] =
                      // if we are in here, all constituent parts can be solved
                      resInner.value

                    {
                      case s: SumValue =>
                        mapping.get(s.variant) match {
                          case Some(fn) =>
                            s.value.toList.zip(fn)
                              .traverse { case (p, (key, f)) =>
                                f(p).map((key, _))
                              }
                              .map { ps => Json.JObject(ps) }
                          case None =>
                            Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, s))
                        }
                      case a =>
                        Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, a))
                     }
                   }
                case _ =>
                  // this is nat-like
                  {
                    case ExternalValue(b: BigInteger) =>
                      Right(Json.JNumberStr(b.toString))
                    case other =>
                      Left(JsonEncodingError.IllTyped(revPath.reverse, tpe, other))
                  }
              }
            })
          // put the result in the cache before we compute it
          // so we can recurse
          successCache.put(tpe, res)
          res
        }

      supported(tpe).map(_ => loop(tpe, Nil).value)
    }

  /**
   * Convert a Json to a Value
   *
   * Note, we statically build the conversion function if it is possible at
   * all, after that, only value errors can occur
   *
   * this code ASSUMES the type is correct. If not, we may return
   * incorrect data.
   */
  def toValue(tpe: Type): Either[UnsupportedType, Json => Either[IllTypedJson, Value]] = {

    type Fn = Json => Either[IllTypedJson, Value]
    // when we complete a custom type, we put it in here
    val successCache: MMap[Type, Eval[Fn]] = MMap()

    def get[A](e: Either[UnsupportedType, A]): A =
      e match {
        case Right(a) => a
        case Left(u) => sys.error(s"should have only called on a supported type: $u")
      }

    def loop(tpe: Type, revPath: List[Type]): Eval[Fn] =
      successCache.get(tpe) match {
        case Some(res) => res
        case None =>
          val res: Eval[Json => Either[IllTypedJson, Value]] = Eval.later(tpe match {
            case Type.IntType =>
              {
                case j@Json.JNumberStr(_) =>
                  j.numberKind match {
                    case int: Json.NumberKind.IntegerKind =>
                      Right(ExternalValue(int.toBigInteger))
                    case _ =>
                      Left(IllTypedJson(revPath.reverse, tpe, j))
                  }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.StrType =>
              {
                case Json.JString(v) =>
                  Right(ExternalValue(v))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.BoolType =>
              {
                case Json.JBool(value) =>
                  Right(if (value) True else False)
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.UnitType =>
              // encode this as null
              {
                case Json.JNull => Right(UnitValue)
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case opt@Type.OptionT(t1) =>
              if (canEncodeToNull(opt)) {
                // not a nested option
                lazy val inner = loop(t1, tpe :: revPath).value

                {
                  case Json.JNull => Right(VOption.none)
                  case notNull => inner(notNull).map(VOption.some(_))
                }
              }
              else {
                // we can't encode Option[Option[T]] as null or not, so we encode
                // as list of 0 or 1 items

                lazy val inner = loop(t1, tpe :: revPath).value

                {
                  case Json.JArray(items) if items.lengthCompare(1) <= 0 =>
                    items.headOption match {
                      case None => Right(VOption.none)
                      case Some(a) => inner(a).map(VOption.some(_))
                    }
                  case other =>
                      Left(IllTypedJson(revPath.reverse, tpe, other))
                }
              }
            case Type.ListT(t) =>
              lazy val inner = loop(t, tpe :: revPath).value

              {
                case Json.JArray(vs) =>
                  vs.toVector
                    .traverse(inner)
                    .map { vs => VList(vs.toList) }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.DictT(Type.StrType, vt) =>
              lazy val inner = loop(vt, tpe :: revPath).value

              {
                case Json.JObject(items) =>
                  items.traverse { case (k, v) =>
                    inner(v).map((k, _))
                  }
                  .map { kvs =>
                    VDict.fromStringKeys(kvs)
                  }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }
            case Type.Tuple(ts) =>
              val p1 = tpe :: revPath
              lazy val inners = ts.traverse(loop(_, p1)).value

              {
                case Json.JArray(as) =>
                  as.zip(inners)
                    .toVector
                    .traverse { case (a, fn) => fn(a) }
                    .map { vs => Tuple.fromList(vs.toList) }
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
              }

            case Type.ForAll(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath).value
            case otherType =>
              val fullPath = tpe :: revPath

              val dt =
                get(Type.rootConst(tpe) match {
                  case Some(Type.TyConst(const)) =>
                    getDefinedType(const) match {
                      case Some(dt) => Right(dt)
                      case None =>
                        Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                    }
                  case None =>
                    Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                })

              val resInner: Eval[
                List[(Int, List[(String, Json => Either[IllTypedJson, Value])])]
              ] = {
                  val cons = dt.constructors
                  val (_, targs) = Type.applicationArgs(tpe)
                  val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  cons.zipWithIndex
                    .traverse { case (cf, idx) =>
                      cf.args.traverse { case (pn, t) =>
                        val subsT = Type.substituteVar(t, replaceMap)
                        loop(subsT, fullPath)
                          .map((pn.asString, _))
                      }
                      .map { pair => (idx, pair) }
                    }
                  }

              dt.natLike match {
                case DefinedType.NatLike.Not =>
                  if (dt.isNewType) {
                    // there is one single arg constructor
                    lazy val inner = resInner.value.head._2.head._2

                    { j => inner(j) }
                  }
                  else {
                    // This is lazy because we don't want to run
                    // the Evals until we have the first value
                    lazy val mapping: List[(Int, Map[String, (Int, Fn)])] =
                      // if we are in here, all constituent parts can be solved
                        resInner.value.map { case (idx, lst) =>
                          (idx,
                            lst
                              .iterator
                              .zipWithIndex
                              .map { case ((nm, fn), idx) => (nm, (idx, fn)) }
                              .toMap)
                        }

                    {
                      case obj@Json.JObject(_) =>
                        val keySet = obj.toMap.keySet
                        def run(cand: List[(Int, Map[String, (Int, Fn)])]): Either[IllTypedJson, Value] =
                          cand match {
                            case Nil =>
                              Left(IllTypedJson(revPath.reverse, tpe, obj))
                            case (variant, decode) :: _ if keySet == decode.keySet =>
                              val itemArray = new Array[Value](keySet.size)
                              obj.items.foldM(itemArray) { case (ary, (k, v)) =>
                                val (idx, fn) = decode(k)
                                fn(v).map { value =>
                                  ary(idx) = value
                                  ary
                                }
                              }
                              .map { ary =>
                                val prod = ProductValue.fromList(ary.toList)
                                if (dt.isStruct) prod
                                else SumValue(variant, prod)
                              }
                            case _ :: tail => run(tail)
                          }

                        run(mapping)
                      case other =>
                        Left(IllTypedJson(revPath.reverse, tpe, other))
                    }
                  }
                case _ =>
                  // this is a nat like type which we encode into integers
                  {
                    case j@Json.JNumberStr(_) =>
                      // handle Nat types
                      j.numberKind match {
                        case int: Json.NumberKind.IntegerKind =>
                          // todo
                          Right(ExternalValue(int.toBigInteger))
                        case _ =>
                          Left(IllTypedJson(revPath.reverse, tpe, j))
                      }
                    case other =>
                      Left(IllTypedJson(revPath.reverse, tpe, other))
                  }
              }
            })

          successCache.put(tpe, res)
          res
      }

      supported(tpe).map(_ => loop(tpe, Nil).value)
    }

}

sealed abstract class JsonEncodingError
object JsonEncodingError {
  sealed abstract class ValueError extends JsonEncodingError

  final case class UnsupportedType(path: NonEmptyList[Type]) extends JsonEncodingError
  final case class IllTyped(path: List[Type], tpe: Type, value: Value) extends ValueError

  final case class IllTypedJson(path: List[Type], tpe: Type, value: Json) extends JsonEncodingError
}
