package dev.bosatsu

import cats.Eval
import cats.Show
import cats.data.NonEmptyList
import cats.implicits._
import java.math.BigInteger
import dev.bosatsu.rankn.{DefinedType, Type, DataFamily}
import scala.collection.mutable.{Map => MMap, LinkedHashSet}

import Value._

import JsonEncodingError._

case class ValueToJson(getDefinedType: Type.Const => Option[DefinedType[Any]]) {
  private val arrayTypeConst: Type.Const.Defined =
    Type.Const.Defined(
      PackageName.parts("Bosatsu", "Collection", "Array"),
      TypeName("Array")
    )
  private val bytesTypeConst: Type.Const.Defined =
    Type.Const.Defined(
      PackageName.parts("Bosatsu", "IO", "Bytes"),
      TypeName("Bytes")
    )
  private val bosatsuJsonTypeConst: Type.Const.Defined =
    Type.Const.Defined(PackageName.parts("Bosatsu", "Json"), TypeName("Json"))
  private val optionalTypeConst: Type.Const.Defined =
    Type.Const.Defined(
      PackageName.parts("Bosatsu", "Json"),
      TypeName("Optional")
    )
  private val nullableTypeConst: Type.Const.Defined =
    Type.Const.Defined(
      PackageName.parts("Bosatsu", "Json"),
      TypeName("Nullable")
    )

  private object OptionalT {
    def unapply(t: Type): Option[Type] =
      t match {
        case Type.TyApply(Type.TyConst(`optionalTypeConst`), inner) =>
          Some(inner)
        case _ => None
      }
  }

  private object NullableT {
    def unapply(t: Type): Option[Type] =
      t match {
        case Type.TyApply(Type.TyConst(`nullableTypeConst`), inner) =>
          Some(inner)
        case _ => None
      }
  }

  private def parseFloat64(str: String): Option[Double] =
    try Some(java.lang.Double.parseDouble(str))
    catch {
      case _: NumberFormatException => None
    }

  private def isIntegerNumberToken(str: String): Boolean = {
    val len = str.length
    if (len == 0) false
    else {
      val start =
        if (str.charAt(0) == '-') {
          if (len == 1) return false
          1
        } else 0
      var idx = start
      while (idx < len) {
        val c = str.charAt(idx)
        if ((c < '0') || ('9' < c)) return false
        idx = idx + 1
      }
      true
    }
  }

  private def decodeOptionalLike(value: Value): Option[Option[Value]] =
    value match {
      case s: SumValue
          if (s.variant == 0) && (s.value == UnitValue) =>
        Some(None)
      case s: SumValue if s.variant == 1 =>
        s.value.values match {
          case Array(v) => Some(Some(v))
          case _        => None
        }
      case _ => None
    }

  private def encodeOptionalLike(value: Option[Value]): Value =
    value match {
      case None        => SumValue(0, UnitValue)
      case Some(inner) => SumValue(1, ProductValue.single(inner))
    }

  private def isSingleUnicodeScalar(v: String): Boolean =
    if (v.isEmpty) false
    else {
      val cp = v.codePointAt(0)
      val isSurrogateCodePoint = cp >= 0xd800 && cp <= 0xdfff
      !isSurrogateCodePoint &&
      v.length == Character.charCount(cp) &&
      v.codePointCount(0, v.length) == 1
    }

  def canEncodeToNull(t: Type): Boolean =
    t match {
      case Type.UnitType       => true
      case Type.OptionT(inner) =>
        // if the inside of an Option cannot be null, we can use null
        // to represent None
        !canEncodeToNull(inner)
      case OptionalT(inner) =>
        // Optional follows Option semantics outside of struct/enum field positions.
        !canEncodeToNull(inner)
      case Type.ForAll(_, inner) => canEncodeToNull(inner)
      case Type.Exists(_, inner) => canEncodeToNull(inner)
      case _                     => false
    }

  /** Return all unsupported json conversion issues for this type.
    *
    * The list is deduplicated by unsupported leaf type and reason.
    */
  def unsupportedIssues(
      tpe: Type
  ): List[(NonEmptyList[Type], String)] = {
    val seen = LinkedHashSet.empty[(Type, String)]
    val issues = List.newBuilder[(NonEmptyList[Type], String)]

    def addIssue(
        t: Type,
        revPath: List[Type],
        reason: String
    ): Unit =
      if (seen.add((t, reason))) {
        val path = NonEmptyList(t, revPath).reverse
        issues += ((path, reason))
      }

    def loop(t: Type, revPath: List[Type], working: List[Type]): Unit =
      t match {
        case _ if working.contains(t)                                    => ()
        case Type.IntType | Type.Float64Type | Type.StrType | Type.CharType |
            Type.BoolType | Type.UnitType =>
          ()
        case Type.TyConst(`bytesTypeConst`) =>
          ()
        case Type.TyApply(Type.TyConst(`arrayTypeConst`), inner) =>
          loop(inner, t :: revPath, t :: working)
        case Type.OptionT(inner) =>
          loop(inner, t :: revPath, t :: working)
        case OptionalT(inner) =>
          loop(inner, t :: revPath, t :: working)
        case NullableT(inner) =>
          loop(inner, t :: revPath, t :: working)
        case Type.TyConst(`bosatsuJsonTypeConst`) =>
          ()
        case Type.ListT(inner) =>
          loop(inner, t :: revPath, t :: working)
        case Type.DictT(Type.StrType, inner) =>
          loop(inner, t :: revPath, t :: working)
        case Type.Fun(_)                     =>
          addIssue(t, revPath, "function types are not serializable to Json")
        case Type.DictT(_, _)                =>
          addIssue(
            t,
            revPath,
            "only Dict[String, a] can be encoded as a Json object"
          )
        case Type.ForAll(_, _)               =>
          addIssue(
            t,
            revPath,
            "forall (polymorphic) types are not serializable to Json"
          )
        case Type.Exists(_, _)               =>
          addIssue(
            t,
            revPath,
            "existential types are not serializable to Json"
          )
        case Type.TyVar(_) =>
          addIssue(t, revPath, "unresolved type variables are not serializable")
        case Type.TyMeta(_) =>
          addIssue(t, revPath, "unsolved type metavariables are not serializable")
        case Type.Tuple(ts) =>
          val w1 = t :: working
          val p1 = t :: revPath
          ts.foreach(loop(_, p1, w1))
        case consOrApply =>
          val w1 = consOrApply :: working
          val p1 = consOrApply :: revPath
          Type.rootConst(consOrApply) match {
            case None =>
              addIssue(
                consOrApply,
                revPath,
                "type root constructor could not be resolved"
              )
            case Some(Type.TyConst(const)) =>
              getDefinedType(const) match {
                case None =>
                  addIssue(
                    consOrApply,
                    revPath,
                    "defined type metadata could not be resolved"
                  )
                case Some(dt) =>
                  val (_, targs) = Type.unapplyAll(consOrApply)
                  val replaceMap =
                    dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  dt.constructors.foreach { cf =>
                    cf.args.foreach { param =>
                      val substituted =
                        Type.substituteVar(param.tpe, replaceMap)
                      loop(substituted, p1, w1)
                    }
                  }
              }
          }
      }

    loop(tpe, Nil, Nil)
    issues.result()
  }

  /** Is a given type supported for Json conversion
    */
  def supported(t: Type): Either[UnsupportedType, Unit] =
    unsupportedIssues(t).headOption match {
      case None            => Right(())
      case Some((path, _)) => Left(UnsupportedType(path))
    }

  // used only after we have checked that types are supported
  private def get[A](e: Either[UnsupportedType, A]): A =
    e match {
      case Right(a) => a
      // $COVERAGE-OFF$
      case Left(u) =>
        sys.error(s"should have only called on a supported type: $u")
      // $COVERAGE-ON$
    }

  def fieldName(n: Identifier.Bindable): String = {
    import Identifier.{Name, Backticked, Operator, Synthetic}
    n match {
      case Name(n) => n
      case Backticked(n) => n
      // these are kind of weird, but I guess allowed
      case Operator(n) => n
      case Synthetic(n) => n
    }
  }

  /** Convert a typechecked value to Json
    *
    * Note, we statically build the conversion function if it is possible at
    * all, after that, only value errors can occur
    *
    * this code ASSUMES the type is correct. If not, we may return incorrect
    * data.
    */
  def toJson(
      tpe: Type
  ): Either[UnsupportedType, Value => Either[IllTyped, Json]] = {

    type Fn = Value => Either[IllTyped, Json]
    // when we complete a custom type, we put it in here
    val successCache: MMap[Type, Eval[Fn]] = MMap()


    def loop(tpe: Type, revPath: List[Type]): Eval[Fn] =
      // we know we can support this, so when we recurse it
      // is safe to call .right.get
      successCache.get(tpe) match {
        case Some(fn) => fn
        case None     =>
          val res: Eval[Fn] = Eval.later(tpe match {
            case Type.IntType => {
              case ExternalValue(v: java.lang.Integer) =>
                Right(Json.JNumberStr(v.toString))
              case ExternalValue(v: BigInteger) =>
                Right(Json.JNumberStr(v.toString))
              // $COVERAGE-OFF$this should be unreachable
              case other =>
                Left(IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
            }
            case Type.Float64Type => {
              case VFloat(v) =>
                if (java.lang.Double.isFinite(v))
                  Right(Json.JNumberStr(java.lang.Double.toString(v)))
                else Right(Json.JString(java.lang.Double.toString(v)))
              case other =>
                Left(IllTyped(revPath.reverse, tpe, other))
            }
            case Type.StrType => {
              case ExternalValue(v: String) =>
                Right(Json.JString(v))
              // $COVERAGE-OFF$this should be unreachable
              case other =>
                Left(IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
            }
            case Type.CharType => {
              case ExternalValue(v: String) if isSingleUnicodeScalar(v) =>
                Right(Json.JString(v))
              case other =>
                Left(IllTyped(revPath.reverse, tpe, other))
            }
            case Type.BoolType => {
              case True  => Right(Json.JBool(true))
              case False => Right(Json.JBool(false))
              case other =>
                // $COVERAGE-OFF$this should be unreachable
                Left(IllTyped(revPath.reverse, tpe, other))
              // $COVERAGE-ON$
            }
            case Type.UnitType =>
              // encode this as null
              {
                case UnitValue => Right(Json.JNull)
                case other     =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case opt @ Type.OptionT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

              if (canEncodeToNull(opt)) {
                // not a nested option

                case VOption(None)    => Right(Json.JNull)
                case VOption(Some(a)) => inner(a)
                case other            =>
                  Left(IllTyped(revPath.reverse, tpe, other))
              }
              else
                {

                  case VOption(None)    => Right(Json.JArray(Vector.empty))
                  case VOption(Some(a)) =>
                    inner(a).map(j => Json.JArray(Vector(j)))
                  case other =>
                    Left(IllTyped(revPath.reverse, tpe, other))
                }
            case opt @ OptionalT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

              if (canEncodeToNull(opt)) {
                // not a nested option
                {
                  case value =>
                    decodeOptionalLike(value) match {
                      case Some(None)    => Right(Json.JNull)
                      case Some(Some(a)) => inner(a)
                      case None          =>
                        Left(IllTyped(revPath.reverse, tpe, value))
                    }
                }
              } else {
                {
                  case value =>
                    decodeOptionalLike(value) match {
                      case Some(None)    => Right(Json.JArray(Vector.empty))
                      case Some(Some(a)) =>
                        inner(a).map(j => Json.JArray(Vector(j)))
                      case None =>
                        Left(IllTyped(revPath.reverse, tpe, value))
                    }
                }
              }
            case NullableT(t1) =>
              lazy val inner = loop(t1, tpe :: revPath).value

              {
                case s: SumValue
                    if (s.variant == 0) && (s.value == UnitValue) =>
                  Right(Json.JNull)
                case s: SumValue if s.variant == 1 =>
                  s.value.values match {
                    case Array(a) => inner(a)
                    case _        => Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case other =>
                  Left(IllTyped(revPath.reverse, tpe, other))
              }
            case Type.TyConst(`bosatsuJsonTypeConst`) =>
              lazy val recur = loop(tpe, tpe :: revPath).value

              {
                case s: SumValue
                    if (s.variant == 0) && (s.value == UnitValue) =>
                  Right(Json.JNull)
                case s: SumValue if s.variant == 1 =>
                  s.value.values match {
                    case Array(True)  => Right(Json.JBool(true))
                    case Array(False) => Right(Json.JBool(false))
                    case _            => Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case s: SumValue if s.variant == 2 =>
                  s.value.values match {
                    case Array(ExternalValue(v: String)) =>
                      Right(Json.JString(v))
                    case _ =>
                      Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case s: SumValue if s.variant == 3 =>
                  s.value.values match {
                    case Array(ExternalValue(v: java.lang.Integer)) =>
                      Right(Json.JNumberStr(v.toString))
                    case Array(ExternalValue(v: BigInteger)) =>
                      Right(Json.JNumberStr(v.toString))
                    case _ =>
                      Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case s: SumValue if s.variant == 4 =>
                  s.value.values match {
                    case Array(VFloat(v)) =>
                      if (java.lang.Double.isFinite(v))
                        Right(Json.JNumberStr(java.lang.Double.toString(v)))
                      else Right(Json.JString(java.lang.Double.toString(v)))
                    case _ =>
                      Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case s: SumValue if s.variant == 5 =>
                  s.value.values match {
                    case Array(VList(items)) =>
                      items.toVector.traverse(recur).map(Json.JArray(_))
                    case _ =>
                      Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case s: SumValue if s.variant == 6 =>
                  s.value.values match {
                    case Array(VList(items)) =>
                      items
                        .traverse {
                          case Tuple(Str(k) :: v :: Nil) =>
                            recur(v).map((k, _))
                          case bad =>
                            Left(IllTyped(revPath.reverse, tpe, bad))
                        }
                        .map(Json.JObject(_))
                    case _ =>
                      Left(IllTyped(revPath.reverse, tpe, s))
                  }
                case other =>
                  Left(IllTyped(revPath.reverse, tpe, other))
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
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.DictT(Type.StrType, vt) =>
              lazy val inner = loop(vt, tpe :: revPath).value

              {
                case VDict(d) =>
                  d.toList
                    .traverse { case (k, v) =>
                      k match {
                        case Str(kstr) => inner(v).map((kstr, _))
                        case other     =>
                          // $COVERAGE-OFF$this should be unreachable
                          Left(IllTyped(revPath.reverse, tpe, other))
                        // $COVERAGE-ON$
                      }
                    }
                    .map(Json.JObject(_))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.Tuple(ts) =>
              val p1 = tpe :: revPath
              lazy val inners = ts.traverse(t => loop(t, p1)).value
              val tsize = ts.size

              {
                case Tuple(as) if as.size == tsize =>
                  as.zip(inners)
                    .toVector
                    .traverse { case (a, fn) => fn(a) }
                    .map(Json.JArray(_))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTyped(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
            case Type.TyApply(Type.TyConst(`arrayTypeConst`), itemType) =>
              lazy val inner = loop(itemType, tpe :: revPath).value

              {
                case ExternalValue(arr: PredefImpl.ArrayValue) =>
                  val values =
                    arr.data.iterator
                      .slice(arr.offset, arr.offset + arr.len)
                      .toList
                  values.traverse(inner).map(items => Json.JArray(items.toVector))
                case other =>
                  Left(IllTyped(revPath.reverse, tpe, other))
              }
            case Type.TyConst(`bytesTypeConst`) =>
              {
                case ExternalValue(bytes: PredefImpl.BytesValue) =>
                  val items = Vector.newBuilder[Json]
                  var idx = 0
                  while (idx < bytes.len) {
                    val intValue = bytes.data(bytes.offset + idx).toInt & 0xff
                    items += Json.JNumberStr(intValue.toString)
                    idx = idx + 1
                  }
                  Right(Json.JArray(items.result()))
                case other =>
                  Left(IllTyped(revPath.reverse, tpe, other))
              }

            case Type.ForAll(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath).value
            case Type.Exists(_, inner) =>
              // we assume the generic positions don't matter and to continue
              loop(inner, tpe :: revPath).value
            case _ =>
              // We can have complicated recursion here, we
              // need to be careful with Eval.later/lazy to tie the knot
              val fullPath = tpe :: revPath

              val dt =
                get(Type.rootConst(tpe) match {
                  case Some(Type.TyConst(const)) =>
                    getDefinedType(const) match {
                      case Some(dt) => Right(dt)
                      case None     =>
                        Left(
                          UnsupportedType(NonEmptyList(tpe, revPath).reverse)
                        )
                    }
                  case None =>
                    Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                })

              dt.dataFamily match {
                case DataFamily.Nat => {
                  case ExternalValue(b: (BigInteger | java.lang.Integer)) =>
                    Right(Json.JNumberStr(b.toString))
                  case other =>
                    Left(IllTyped(revPath.reverse, tpe, other))
                }
                case notNat =>
                  val cons = dt.constructors
                  val (_, targs) = Type.unapplyAll(tpe)
                  val replaceMap =
                    dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  sealed trait ObjFieldEncoder {
                    def key: String
                  }
                  final case class RequiredField(key: String, fn: Fn)
                      extends ObjFieldEncoder
                  final case class OptionalField(key: String, fn: Fn)
                      extends ObjFieldEncoder

                  def encodeField(
                      value: Value,
                      field: ObjFieldEncoder
                  ): Either[IllTyped, List[(String, Json)]] =
                    field match {
                      case RequiredField(key, fn) =>
                        fn(value).map(json => (key, json) :: Nil)
                      case OptionalField(key, fn) =>
                        decodeOptionalLike(value) match {
                          case Some(None) => Right(Nil)
                          case Some(Some(v)) =>
                            fn(v).map(json => (key, json) :: Nil)
                          case None =>
                            Left(IllTyped(revPath.reverse, tpe, value))
                        }
                    }

                  val resInner: Eval[Map[Int, List[ObjFieldEncoder]]] =
                    cons.zipWithIndex
                      .traverse { case (cf, idx) =>
                        val rec = cf.args.traverse { param =>
                          val subsT = Type.substituteVar(param.tpe, replaceMap)
                          subsT match {
                            case OptionalT(inner) =>
                              loop(inner, fullPath)
                                .map(
                                  fn => OptionalField(fieldName(param.name), fn)
                                )
                            case _ =>
                              loop(subsT, fullPath)
                                .map(
                                  fn => RequiredField(fieldName(param.name), fn)
                                )
                          }
                        }
                        rec.map(fields => (idx, fields))
                      }
                      .map(_.toMap)

                  notNat match {
                    case DataFamily.NewType =>
                      lazy val fieldAndInner = resInner.value.head._2.head

                      { v =>
                        encodeField(v, fieldAndInner).map { kvs =>
                          Json.JObject(kvs)
                        }
                      }
                    case DataFamily.Struct =>
                      lazy val productsInner = resInner.value.head._2
                      lazy val size = productsInner.size

                      {
                        case prod: ProductValue =>
                          val plist = prod.values.toList

                          if (plist.size == size) {
                            plist
                              .zip(productsInner)
                              .traverse { case (p, field) =>
                                encodeField(p, field)
                              }
                              .map(ps => Json.JObject(ps.flatten))
                          } else {
                            Left(IllTyped(revPath.reverse, tpe, prod))
                          }

                        case other =>
                          Left(IllTyped(revPath.reverse, tpe, other))
                      }
                    case _ => // this is Enum
                      lazy val mapping: Map[Int, List[ObjFieldEncoder]] =
                        // if we are in here, all constituent parts can be solved
                        resInner.value

                      {
                        case s: SumValue =>
                          mapping.get(s.variant) match {
                            case Some(fn) =>
                              val vlist = s.value.values.toList
                              if (vlist.size == fn.size) {
                                vlist
                                  .zip(fn)
                                  .traverse { case (p, field) =>
                                    encodeField(p, field)
                                  }
                                  .map(ps => Json.JObject(ps.flatten))
                              } else Left(IllTyped(revPath.reverse, tpe, s))
                            case None =>
                              Left(IllTyped(revPath.reverse, tpe, s))
                          }
                        case a =>
                          Left(IllTyped(revPath.reverse, tpe, a))
                      }
                  }
              }
          })
          // put the result in the cache before we compute it
          // so we can recurse
          successCache.put(tpe, res): Unit
          res
      }

    supported(tpe).map(_ => loop(tpe, Nil).value)
  }

  /** Convert a Json to a Value
    *
    * Note, we statically build the conversion function if it is possible at
    * all, after that, only value errors can occur
    *
    * this code ASSUMES the type is correct. If not, we may return incorrect
    * data.
    */
  def toValue(
      tpe: Type
  ): Either[UnsupportedType, Json => Either[IllTypedJson, Value]] = {

    type Fn = Json => Either[IllTypedJson, Value]
    // when we complete a custom type, we put it in here
    val successCache: MMap[Type, Eval[Fn]] = MMap()

    def loop(tpe: Type, revPath: List[Type]): Eval[Fn] =
      successCache.get(tpe) match {
        case Some(res) => res
        case None      =>
          val res: Eval[Json => Either[IllTypedJson, Value]] =
            Eval.later(tpe match {
              case Type.IntType => {
                case Json.JBigInteger(b) =>
                  Right(VInt(b))
                case other =>
                  Left(IllTypedJson(revPath.reverse, tpe, other))
              }
              case Type.Float64Type => {
                case num @ Json.JNumberStr(str) =>
                  parseFloat64(str)
                    .map(VFloat(_))
                    .toRight(IllTypedJson(revPath.reverse, tpe, num))
                case str @ Json.JString(value) =>
                  parseFloat64(value)
                    .map(VFloat(_))
                    .toRight(IllTypedJson(revPath.reverse, tpe, str))
                case other =>
                  Left(IllTypedJson(revPath.reverse, tpe, other))
              }
              case Type.StrType => {
                case Json.JString(v) =>
                  Right(ExternalValue(v))
                case other =>
                  // $COVERAGE-OFF$this should be unreachable
                  Left(IllTypedJson(revPath.reverse, tpe, other))
                // $COVERAGE-ON$
              }
              case Type.CharType => {
                case Json.JString(v) if isSingleUnicodeScalar(v) =>
                  Right(ExternalValue(v))
                case other =>
                  Left(IllTypedJson(revPath.reverse, tpe, other))
              }
              case Type.BoolType => {
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
                  case other      =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
                }
              case opt @ Type.OptionT(t1) =>
                if (canEncodeToNull(opt)) {
                  // not a nested option
                  lazy val inner = loop(t1, tpe :: revPath).value

                  {
                    case Json.JNull => Right(VOption.none)
                    case notNull    => inner(notNull).map(VOption.some(_))
                  }
                } else {
                  // we can't encode Option[Option[T]] as null or not, so we encode
                  // as list of 0 or 1 items

                  lazy val inner = loop(t1, tpe :: revPath).value

                  {
                    case Json.JArray(items) if items.lengthCompare(1) <= 0 =>
                      items.headOption match {
                        case None    => Right(VOption.none)
                        case Some(a) => inner(a).map(VOption.some(_))
                      }
                    case other =>
                      Left(IllTypedJson(revPath.reverse, tpe, other))
                  }
                }
              case opt @ OptionalT(t1) =>
                if (canEncodeToNull(opt)) {
                  // not a nested option
                  lazy val inner = loop(t1, tpe :: revPath).value

                  {
                    case Json.JNull =>
                      Right(encodeOptionalLike(None))
                    case notNull =>
                      inner(notNull).map(v => encodeOptionalLike(Some(v)))
                  }
                } else {
                  // we can't encode Option[Option[T]] as null or not, so we encode
                  // as list of 0 or 1 items
                  lazy val inner = loop(t1, tpe :: revPath).value

                  {
                    case Json.JArray(items) if items.lengthCompare(1) <= 0 =>
                      items.headOption match {
                        case None =>
                          Right(encodeOptionalLike(None))
                        case Some(a) =>
                          inner(a).map(v => encodeOptionalLike(Some(v)))
                      }
                    case other =>
                      Left(IllTypedJson(revPath.reverse, tpe, other))
                  }
                }
              case NullableT(t1) =>
                lazy val inner = loop(t1, tpe :: revPath).value

                {
                  case Json.JNull =>
                    Right(SumValue(0, UnitValue))
                  case notNull =>
                    inner(notNull)
                      .map(v => SumValue(1, ProductValue.single(v)))
                }
              case Type.TyConst(`bosatsuJsonTypeConst`) =>
                lazy val recur = loop(tpe, tpe :: revPath).value

                {
                  case Json.JNull =>
                    Right(SumValue(0, UnitValue))
                  case Json.JBool.True =>
                    Right(
                      SumValue(
                        1,
                        ProductValue.single(True)
                      )
                    )
                  case Json.JBool.False =>
                    Right(
                      SumValue(
                        1,
                        ProductValue.single(False)
                      )
                    )
                  case Json.JString(value) =>
                    Right(
                      SumValue(
                        2,
                        ProductValue.single(ExternalValue(value))
                      )
                    )
                  case num @ Json.JNumberStr(str) =>
                    if (isIntegerNumberToken(str))
                      Right(
                        SumValue(
                          3,
                          ProductValue.single(VInt(new BigInteger(str)))
                        )
                      )
                    else
                      parseFloat64(str) match {
                        case Some(v) =>
                          Right(
                            SumValue(4, ProductValue.single(VFloat(v)))
                          )
                        case None =>
                          Left(IllTypedJson(revPath.reverse, tpe, num))
                      }
                  case Json.JArray(items) =>
                    items.toVector
                      .traverse(recur)
                      .map(vs =>
                        SumValue(5, ProductValue.single(VList(vs.toList)))
                      )
                  case Json.JObject(items) =>
                    items
                      .traverse { case (k, v) =>
                        recur(v).map(decoded => Tuple(Str(k), decoded))
                      }
                      .map(vs =>
                        SumValue(6, ProductValue.single(VList(vs)))
                      )
                }
              case Type.ListT(t) =>
                lazy val inner = loop(t, tpe :: revPath).value

                {
                  case Json.JArray(vs) =>
                    vs.toVector
                      .traverse(inner)
                      .map(vs => VList(vs.toList))
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
                }
              case Type.DictT(Type.StrType, vt) =>
                lazy val inner = loop(vt, tpe :: revPath).value

                {
                  case Json.JObject(items) =>
                    items
                      .traverse { case (k, v) =>
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
                  case ary @ Json.JArray(as) =>
                    if (as.size == inners.size) {
                      as.zip(inners)
                        .toVector
                        .traverse { case (a, fn) => fn(a) }
                        .map(vs => Tuple.fromList(vs.toList))
                    } else Left(IllTypedJson(revPath.reverse, tpe, ary))
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    Left(IllTypedJson(revPath.reverse, tpe, other))
                  // $COVERAGE-ON$
                }
              case Type.TyApply(Type.TyConst(`arrayTypeConst`), itemType) =>
                lazy val inner = loop(itemType, tpe :: revPath).value

                {
                  case Json.JArray(items) =>
                    items.toVector.traverse(inner).map { values =>
                      val data = values.toArray
                      ExternalValue(PredefImpl.ArrayValue(data, 0, data.length))
                    }
                  case other =>
                    Left(IllTypedJson(revPath.reverse, tpe, other))
                }
              case Type.TyConst(`bytesTypeConst`) =>
                {
                  case Json.JArray(items) =>
                    val data = new Array[Byte](items.length)
                    var idx = 0
                    var invalid: Option[Json] = None
                    while (idx < items.length && invalid.isEmpty) {
                      items(idx) match {
                        case Json.JBigInteger(bi)
                            if bi.signum >= 0 &&
                              bi.compareTo(BigInteger.valueOf(255L)) <= 0 =>
                          data(idx) = (bi.intValue & 0xff).toByte
                          idx = idx + 1
                        case other =>
                          invalid = Some(other)
                      }
                    }

                    invalid match {
                      case Some(bad) =>
                        Left(IllTypedJson(revPath.reverse, tpe, bad))
                      case None      =>
                        if (data.isEmpty) Right(PredefImpl.emptyBytes)
                        else
                          Right(
                            ExternalValue(
                              PredefImpl.BytesValue(data, 0, data.length)
                            )
                          )
                    }
                  case other =>
                    Left(IllTypedJson(revPath.reverse, tpe, other))
                }

              case Type.ForAll(_, inner) =>
                // we assume the generic positions don't matter and to continue
                loop(inner, tpe :: revPath).value
              case Type.Exists(_, inner) =>
                // we assume the generic positions don't matter and to continue
                loop(inner, tpe :: revPath).value
              case _ =>
                val fullPath = tpe :: revPath

                val dt =
                  get(Type.rootConst(tpe) match {
                    case Some(Type.TyConst(const)) =>
                      getDefinedType(const) match {
                        case Some(dt) => Right(dt)
                        case None     =>
                          Left(
                            UnsupportedType(NonEmptyList(tpe, revPath).reverse)
                          )
                      }
                    case None =>
                      Left(UnsupportedType(NonEmptyList(tpe, revPath).reverse))
                  })

                sealed trait ObjFieldDecoder {
                  def idx: Int
                  def key: String
                }
                final case class RequiredField(idx: Int, key: String, fn: Fn)
                    extends ObjFieldDecoder
                final case class OptionalField(idx: Int, key: String, fn: Fn)
                    extends ObjFieldDecoder

                val resInner: Eval[List[(Int, List[ObjFieldDecoder])]] = {
                  val cons = dt.constructors
                  val (_, targs) = Type.unapplyAll(tpe)
                  val replaceMap =
                    dt.typeParams.zip(targs).toMap[Type.Var, Type]

                  cons.zipWithIndex
                    .traverse { case (cf, idx) =>
                      cf.args.zipWithIndex
                        .traverse { case (param, fieldIdx) =>
                          val subsT = Type.substituteVar(param.tpe, replaceMap)
                          subsT match {
                            case OptionalT(inner) =>
                              loop(inner, fullPath).map(
                                fn =>
                                  OptionalField(
                                    fieldIdx,
                                    fieldName(param.name),
                                    fn
                                  )
                              )
                            case _ =>
                              loop(subsT, fullPath).map(
                                fn =>
                                  RequiredField(
                                    fieldIdx,
                                    fieldName(param.name),
                                    fn
                                  )
                              )
                          }
                        }
                        .map(pair => (idx, pair))
                    }
                }

                dt.dataFamily match {
                  case DataFamily.NewType =>
                    // there is one single arg constructor.
                    // The runtime value is still unboxed, but for JSON we keep
                    // the source-level field/object shape.
                    lazy val fieldAndInner = resInner.value.head._2.head

                    {
                      case obj @ Json.JObject(_) =>
                        val itemMap = obj.toMap
                        fieldAndInner match {
                          case RequiredField(_, fieldName, inner) =>
                            if (itemMap.keySet == Set(fieldName))
                              inner(itemMap(fieldName))
                            else Left(IllTypedJson(revPath.reverse, tpe, obj))
                          case OptionalField(_, fieldName, inner) =>
                            if (itemMap.isEmpty) Right(encodeOptionalLike(None))
                            else if (itemMap.keySet == Set(fieldName))
                              inner(itemMap(fieldName))
                                .map(v => encodeOptionalLike(Some(v)))
                            else Left(IllTypedJson(revPath.reverse, tpe, obj))
                        }
                      case other =>
                        Left(IllTypedJson(revPath.reverse, tpe, other))
                    }
                  case DataFamily.Struct | DataFamily.Enum =>
                    // This is lazy because we don't want to run
                    // the Evals until we have the first value
                    lazy val mapping: List[(Int, List[ObjFieldDecoder])] =
                      // if we are in here, all constituent parts can be solved
                      resInner.value

                    {
                      case obj @ Json.JObject(_) =>
                        val itemMap = obj.toMap
                        val keySet = itemMap.keySet
                        def run(
                            cand: List[(Int, List[ObjFieldDecoder])]
                        ): Either[IllTypedJson, Value] =
                          cand match {
                            case Nil =>
                              Left(IllTypedJson(revPath.reverse, tpe, obj))
                            case (variant, decode) :: tail =>
                              val allKeys = decode.iterator.map(_.key).toSet
                              val requiredKeys = decode.collect {
                                case RequiredField(_, k, _) => k
                              }.toSet

                              if (requiredKeys.subsetOf(keySet) && keySet
                                  .subsetOf(allKeys)) {
                                val itemArray = new Array[Value](decode.size)
                                decode
                                  .foldM(itemArray) { (ary, field) =>
                                    field match {
                                      case RequiredField(idx, key, fn) =>
                                        itemMap.get(key) match {
                                          case Some(value) =>
                                            fn(value).map { parsed =>
                                              ary(idx) = parsed
                                              ary
                                            }
                                          case None =>
                                            Left(
                                              IllTypedJson(
                                                revPath.reverse,
                                                tpe,
                                                obj
                                              )
                                            )
                                        }
                                      case OptionalField(idx, key, fn) =>
                                        itemMap.get(key) match {
                                          case Some(value) =>
                                            fn(value).map { parsed =>
                                              ary(idx) =
                                                encodeOptionalLike(Some(parsed))
                                              ary
                                            }
                                          case None =>
                                            ary(idx) = encodeOptionalLike(None)
                                            Right(ary)
                                        }
                                    }
                                  }
                                  .map { ary =>
                                    val prod = ProductValue.fromList(ary.toList)
                                    if (dt.isStruct) prod
                                    else SumValue(variant, prod)
                                  }
                              } else run(tail)
                          }

                        run(mapping)
                      case other =>
                        Left(IllTypedJson(revPath.reverse, tpe, other))
                    }
                  case DataFamily.Nat =>
                    // this is a nat like type which we encode into integers
                    {
                      case Json.JBigInteger(bi) =>
                        Right(ExternalValue(bi))
                      case other =>
                        Left(IllTypedJson(revPath.reverse, tpe, other))
                    }
                }
            })

          successCache.put(tpe, res): Unit
          res
      }

    supported(tpe).map(_ => loop(tpe, Nil).value)
  }

  /** Given a type return the function to convert it a function if it is not a
    * function, we consider it a function of 0-arity
    */
  def valueFnToJsonFn(t: Type): Either[
    UnsupportedType,
    (Int, Value => Either[DataError, Json.JArray => Either[DataError, Json]])
  ] =
    t match {
      case Type.Fun((args, res)) =>
        (args.traverse(toValue(_)), toJson(res)).mapN { (argsFn, resFn) =>
          // if we get in here, we can convert all the args and results

          val arity = argsFn.size
          val argsFnVector = argsFn.toList.toVector

          (
            arity,
            {
              case Value.FnValue(fn) =>
                val jsonFn = { (inputs: Json.JArray) =>
                  if (inputs.toVector.size != arity)
                    Left(IllTypedJson(Nil, t, inputs))
                  else {
                    // we know arity >= 1 because it is a function, so the fromListUnsafe will succeed
                    inputs.toVector
                      .zip(argsFnVector)
                      .traverse { case (a, fn) => fn(a) }
                      .map { vect =>
                        fn(NonEmptyList.fromListUnsafe(vect.toList))
                      }
                      .flatMap(resFn)
                  }
                }

                Right(jsonFn)
              case notFn => Left(IllTyped(Nil, t, notFn))
            }
          )
        }
      case _ =>
        // this isn't a function at all
        toJson(t).map { (fn: (Value) => Either[DataError, Json]) =>
          (
            0,
            fn.andThen { either =>
              either.map { result => (args: Json.JArray) =>
                if (args.toVector.isEmpty) Right(result)
                else Left(IllTypedJson(Nil, t, args))
              }
            }
          )

        }
    }
}

sealed abstract class JsonEncodingError
object JsonEncodingError {
  sealed abstract class DataError extends JsonEncodingError
  object DataError {
    private def pathToString(path: List[Type]): String =
      if (path.isEmpty) "root"
      else
        path
          .map(Type.fullyResolvedDocument.document(_).render(80))
          .mkString(" -> ")

    private def typeToString(tpe: Type): String =
      Type.fullyResolvedDocument.document(tpe).render(80)

    given showDataError: Show[DataError] =
      Show.show {
        case IllTyped(path, tpe, value) =>
          show"ill-typed value at ${pathToString(path)} for expected type ${typeToString(tpe)}: ${value.getClass.getName}"
        case IllTypedJson(path, tpe, value) =>
          show"ill-typed json at ${pathToString(path)} for expected type ${typeToString(tpe)}: ${value.render}"
      }
  }

  final case class UnsupportedType(path: NonEmptyList[Type])
      extends JsonEncodingError

  final case class IllTyped(path: List[Type], tpe: Type, value: Value)
      extends DataError
  object IllTyped {
    given showIllTyped: Show[IllTyped] =
      DataError.showDataError.contramap(identity)
  }
  final case class IllTypedJson(path: List[Type], tpe: Type, value: Json)
      extends DataError
  object IllTypedJson {
    given showIllTypedJson: Show[IllTypedJson] =
      DataError.showDataError.contramap(identity)
  }
}
