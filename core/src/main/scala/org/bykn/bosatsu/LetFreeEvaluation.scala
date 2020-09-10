package org.bykn.bosatsu

import cats.Eval
import LetFreeConversion.LetFreeExpressionTag
import scala.annotation.tailrec
import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}
import java.math.BigInteger

object LetFreeEvaluation {
  import LetFreeConversion.LitValue

  implicit val valueToLitValue: Value => Option[LitValue] = { v =>
    Some(LitValue(v.asExternal.toAny))
  }
  implicit val valueToStruct
      : (Value, rankn.DataFamily) => Option[(Int, List[Value])] = {
    case (v, df) =>
      df match {
        case rankn.DataFamily.Enum => {
          val vSum = v.asSum
          Some((vSum.variant, vSum.value.toList))
        }
        case rankn.DataFamily.Nat => {
          val vExt = v.asExternal
          val length: BigInteger = vExt.toAny.asInstanceOf[BigInteger]
          def loop(n: BigInteger, res: (Int, List[Value])): (Int, List[Value]) =
            if (n.compareTo(BigInteger.valueOf(0)) < 1) {
              res
            } else {
              loop(
                n.add(BigInteger.valueOf(-1)),
                (
                  1,
                  List(
                    Value.SumValue(res._1, Value.ProductValue.fromList(res._2))
                  )
                )
              )
            }
          Some(loop(length, (0, Nil)))
        }
        case rankn.DataFamily.Struct => {
          val vProd = v.asProduct
          Some((0, vProd.toList))
        }
        case rankn.DataFamily.NewType => {
          Some((0, List(v)))
        }
      }
  }
  implicit val valueToList: Value => Option[List[Value]] = { value =>
    @tailrec
    def loop(v: Value, acc: List[Value]): List[Value] = {
      val vSum = v.asSum
      vSum.variant match {
        case 0 => acc
        case 1 =>
          vSum.value.toList match {
            case List(h, t) => loop(t, h :: acc)
            case _ =>
              sys.error(
                "typechecking should make sure we have exactly two args here"
              )
          }
        case n =>
          sys.error(
            s"typechecking should make sure this is only 0 or 1 and not $n"
          )
      }
    }
    Some(loop(value, Nil).reverse)
  }

  implicit val valueFromList: List[Value] => Value = { fullList =>
    @tailrec
    def loop(lst: List[Value], acc: Value): Value = lst match {
      case Nil => acc
      case head :: tail =>
        loop(
          tail,
          Value.SumValue(1, Value.ProductValue.fromList(List(head, acc)))
        )
    }
    loop(fullList.reverse, Value.SumValue(0, Value.UnitValue))
  }

  sealed trait LetFreeValue {
    def toJson: Json = this match {
      case ilv @ LazyValue(expression, scope, _) =>
        Json.JObject(
          List(
            "state" -> Json.JString("expression"),
            "expression" -> Json.JString(expression.serialize),
            "scope" -> Json.JArray(ilv.cleanedScope.map {
              case (n, nv) =>
                Json.JArray(Vector(Json.JNumberStr(n.toString), nv.toJson))
            }.toVector)
          )
        )
      case ComputedValue(value) =>
        Json.JObject(
          List(
            "state" -> Json.JString("computed"),
            "data" -> Json.JString(value.toString)
          )
        )
    }

    def toKey: String = toJson.render.hashCode.toString

    lazy val toStructNat = nvToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum = nvToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct = nvToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType = nvToStructImpl(this, rankn.DataFamily.NewType)

    def toStruct(df: rankn.DataFamily): Option[(Int, List[LetFreeValue])] =
      df match {
        case rankn.DataFamily.Nat     => toStructNat
        case rankn.DataFamily.Enum    => toStructEnum
        case rankn.DataFamily.Struct  => toStructStruct
        case rankn.DataFamily.NewType => toStructNewType
      }

    implicit val extEnv: ExtEnv
    implicit val cache: Cache
  }

  case class LazyValue(
      expression: LetFreeExpression,
      scope: List[LetFreeValue],
      value: Eval[Value]
  )(implicit extEnvArg: ExtEnv, cacheArg: Cache)
      extends LetFreeValue {
    def cleanedScope: List[(Int, LetFreeValue)] =
      expression.varSet.toList.sorted.map { n => (n, scope(n)) }

    implicit val extEnv = extEnvArg
    implicit val cache = cacheArg
  }

  case class ComputedValue(value: Value)(
      implicit extEnvArg: ExtEnv,
      cacheArg: Cache
  ) extends LetFreeValue {
    implicit val extEnv = extEnvArg
    implicit val cache = cacheArg
  }

  implicit def nvToLitValue(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[LetFreeConversion.LitValue] = { nv =>
    valueToLitValue(nvToV(nv))
  }
  implicit def nvToStruct(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): (LetFreeValue, rankn.DataFamily) => Option[(Int, List[LetFreeValue])] = {
    case (nv, df) => nv.toStruct(df)
  }

  def nvToStructImpl(
      nv: LetFreeValue,
      df: rankn.DataFamily
  )(implicit extEnv: ExtEnv, cache: Cache) = {
    nv match {
      case ComputedValue(v) =>
        valueToStruct(v, df).map {
          case (n, lst) => (n, lst.map(vv => ComputedValue(vv)))
        }
      case LazyValue(expr, scope, value) =>
        expr match {
          case LetFreeExpression.Struct(n, lst, df) =>
            Some((n, lst.zipWithIndex.map {
              case (ne, i) =>
                // There is a possible optimization here where we don't need to evaluate the whole struct just for an arg
                LazyValue(ne, scope, value.map { v => v.structArgs(df)(i) })
            }))
          case LetFreeExpression.App(fn, arg) =>
            applyApplyable(
              evalToApplyable(LazyValue(fn, scope, Eval.later {
                evalToValue(fn, scope)
              })),
              LazyValue(arg, scope, Eval.later { evalToValue(arg, scope) }),
              Some(value)
            ).toStruct(df)
          case LetFreeExpression.ExternalVar(p, n, tpe) =>
            ComputedValue(extEnv(n).value).toStruct(df)
          case LetFreeExpression.Recursion(LetFreeExpression.Lambda(expr)) =>
            LazyValue(expr, nv :: scope, value).toStruct(df)
          case LetFreeExpression.LambdaVar(index) =>
            scope(index).toStruct(df)
          case mtch @ LetFreeExpression.Match(_, _) =>
            simplifyMatch(mtch, scope).toStruct(df)
          case other =>
            sys.error(
              s"Type checking should mean this isn't a lambda or a literal: $other"
            )
        }
    }
  }
  implicit def nvToList(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[List[LetFreeValue]] = { normalValue =>
    @tailrec
    def loop(nv: LetFreeValue, acc: List[LetFreeValue]): List[LetFreeValue] = {
      nv.toStruct(rankn.DataFamily.Enum).get match {
        case (0, _)          => acc
        case (1, List(h, t)) => loop(t, h :: acc)
        case _               => sys.error("boom")
      }
    }
    Some(loop(normalValue, Nil).reverse)
  }
  implicit def nvFromList(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): List[LetFreeValue] => LetFreeValue = { scope =>
    @tailrec
    def loop(n: Int, acc: LetFreeExpression): LetFreeExpression = n match {
      case 0 => acc
      case _ =>
        loop(
          n - 1,
          LetFreeExpression
            .Struct(
              1,
              List(LetFreeExpression.LambdaVar(n - 1), acc),
              rankn.DataFamily.Enum
            )
        )
    }
    val expr = loop(
      scope.length,
      LetFreeExpression.Struct(0, Nil, rankn.DataFamily.Enum)
    )
    LazyValue(
      expr,
      scope,
      Eval.later { evalToValue(expr, scope) }
    )
  }

  type Applyable = Either[Value, (LetFreeExpression.Lambda, List[LetFreeValue])]
  type ExtEnv = Map[Identifier, Eval[Value]]
  type Cache = Option[CMap[String, (Future[Value], rankn.Type)]]
  type ToLFV = Option[LetFreeValue => Future[Value]]

  def nvToV(lfv: LetFreeValue)(implicit extEnv: ExtEnv, cache: Cache): Value = {
    lfv match {
      case LazyValue(_, _, eval) => eval.value
      case ComputedValue(value)  => value
    }
  }

  case class ExprFnValue(toExprFn: (LetFreeValue, Cache, ToLFV) => Value)(
      implicit extEnv: ExtEnv,
      cache: Cache
  ) extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v), None, None)
    }
  }

  case class LetFreeFnValue(
      lambda: LetFreeExpression.Lambda,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      evalToValue(lambda.expr, ComputedValue(v) :: scope)(extEnv, cache)
    }
  }

  def attemptExprFn(
      v: Value
  ): Either[(LetFreeValue, Cache, ToLFV) => Value, Value => Value] = v match {
    case fv @ Value.FnValue(f) =>
      fv.arg match {
        case ExprFnValue(ef) => Left(ef)
        case _               => Right(f)
      }
    case other =>
      // $COVERAGE-OFF$this should be unreachable
      sys.error(s"invalid cast to Fn: $other")
    // $COVERAGE-ON$

  }

  import scala.concurrent.ExecutionContext.Implicits.global
  def applyApplyable(
      applyable: Applyable,
      arg: LetFreeValue,
      value: Option[Eval[Value]] = None
  )(implicit extEnv: ExtEnv, cache: Cache): LetFreeValue = applyable match {
    case Left(v) =>
      attemptExprFn(v) match {
        case Left(eFn) =>
          ComputedValue(eFn(arg, cache, Some(nv => Future(nvToV(nv)))))
        case Right(fn) => {
          val v = nvToV(arg)
          ComputedValue(fn(v))
        }
      }
    case Right((LetFreeExpression.Lambda(expr), scope)) =>
      LazyValue(expr, arg :: scope, value.getOrElse(Eval.later {
        evalToValue(expr, arg :: scope)
      }))
  }

  def fnValueToLetFree(value: Value) = value match {
    case fnValue: Value.FnValue =>
      fnValue.arg match {
        case LetFreeFnValue(lambda, scope) => Right((lambda, scope))
        case _                             => Left(fnValue)
      }
    case value => Left(value)
  }

  def evalToApplyable(
      nv: LetFreeValue
  )(implicit extEnv: ExtEnv, cache: Cache): Applyable = {

    val value = nv match {
      case ComputedValue(value)       => value
      case LazyValue(ne, scope, eval) => eval.value
    }
    fnValueToLetFree(value)
  }

  def simplifyMatch(
      mtch: LetFreeExpression.Match,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): LetFreeValue = {
    val (_, patEnv, result) = mtch.branches.toList
      .collectFirst(Function.unlift({
        case (pat, result) =>
          LetFreeConversion
            .maybeBind[LetFreeValue](pat)(
              nvToLitValue(extEnv, cache),
              nvToStruct(extEnv, cache),
              nvToList(extEnv, cache),
              nvFromList(extEnv, cache)
            )
            .apply(LazyValue(mtch.arg, scope, Eval.later {
              evalToValue(mtch.arg, scope)
            }), Map()) match {
            case LetFreeConversion.Matches(env) => Some((pat, env, result))
            case LetFreeConversion.NoMatch      => None
            case LetFreeConversion.NotProvable =>
              sys.error("For value we should never be NotProvable")
          }
      }))
      .get

    ((patEnv.size - 1) to 0 by -1)
      .map(patEnv.get(_).get)
      .foldLeft[LetFreeValue](LazyValue(result, scope, Eval.later {
        evalToValue(result, scope)
      })) { (fn, arg) => applyApplyable(evalToApplyable(fn), arg) }
  }

  def evalToValue(
      ne: LetFreeExpression,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Value = ne match {
    case LetFreeExpression.App(fn, arg) => {
      val applyable = evalToApplyable(ComputedValue(evalToValue(fn, scope)))
      val argV = LazyValue(arg, scope, Eval.later {
        evalToValue(arg, scope)
      })
      nvToV(
        applyApplyable(
          applyable,
          argV
        )
      )
    }
    case LetFreeExpression.ExternalVar(p, n, tpe) => extEnv(n).value
    case mtch @ LetFreeExpression.Match(_, _) =>
      nvToV(simplifyMatch(mtch, scope))
    case LetFreeExpression.LambdaVar(index) => nvToV(scope(index))
    case lambda @ LetFreeExpression.Lambda(_) =>
      new Value.FnValue(
        LetFreeFnValue(lambda, scope)
      )
    case LetFreeExpression.Struct(enum, args, df) =>
      df match {
        case rankn.DataFamily.Enum =>
          Value.SumValue(
            enum,
            Value.ProductValue.fromList(
              args.map(arg => evalToValue(arg, scope))
            )
          )
        case rankn.DataFamily.Nat =>
          if (enum == 0) {
            Value.ExternalValue(BigInteger.valueOf(0))
          } else {
            Value.ExternalValue(
              evalToValue(args.head, scope).asExternal.toAny
                .asInstanceOf[BigInteger]
                .add(BigInteger.ONE)
            )
          }
        case rankn.DataFamily.Struct =>
          Value.ProductValue.fromList(args.map(arg => evalToValue(arg, scope)))
        case rankn.DataFamily.NewType => evalToValue(args.head, scope)
      }
    case LetFreeExpression.Literal(lit) => Value.fromLit(lit)
    case LetFreeExpression.Recursion(lambda) => {
      lambda match {
        case LetFreeExpression.Lambda(expr) => {
          val nextScope = LazyValue(ne, scope, Eval.later {
            evalToValue(ne, scope)
          }) :: scope
          evalToValue(expr, nextScope)
        }
        case _ => sys.error("A Recursion should always contain a Lambda")
      }
    }
  }

  def evaluate(
      ne: LetFreeExpression,
      extEnv: Map[Identifier, Eval[Value]],
      cache: LetFreeEvaluation.Cache
  ): Value = {
    LetFreeEvaluation.evalToValue(ne, Nil)(extEnv, cache)
  }
}

case class LetFreeEvaluation(
    packs: PackageMap.Typed[(Declaration, LetFreeExpressionTag)],
    externals: Externals
) {

  def evaluateLast(
      p: PackageName
  ): Option[(LetFreeExpression, Type, Map[Identifier, Eval[Value]])] = {

    for {
      pack <- packs.toMap.get(p)
      (name, _, tpe) <- pack.program.lets.lastOption
      lfe = tpe.tag._2.lfe
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      lfe,
      tpe.getType,
      extEnv
    )
  }

  def evaluateName(
      p: PackageName,
      name: Identifier
  ): Option[(LetFreeExpression, Type, Map[Identifier, Eval[Value]])] =
    for {
      pack <- packs.toMap.get(p)
      (_, _, tpe) <- pack.program.lets.reverse.collectFirst(Function.unlift {
        tup => if (tup._1 == name) Some(tup) else None
      })
      lfe = tpe.tag._2.lfe
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      lfe,
      tpe.getType,
      extEnv
    )

  private def externalEnv(
      p: Package.Typed[(Declaration, LetFreeExpressionTag)]
  ): LetFreeEvaluation.ExtEnv = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None    =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
        // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None      =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
        // $COVERAGE-ON$
      }
    }.toMap
  }

  private def importedEnv(
      p: Package.Typed[(Declaration, LetFreeExpressionTag)]
  ): LetFreeEvaluation.ExtEnv =
    p.imports.iterator.flatMap { imp =>
      val pack = packs.toMap.get(imp.pack.name) match {
        case Some(p) => p
        case None    =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown package: ${imp.pack.name}")
        // $COVERAGE-ON$
      }
      val exts = externalEnv(pack)
      imp.items.toList.iterator
        .flatMap { in =>
          exts.get(in.originalName).map { value => (in.localName, value) }
        }
    }.toMap

  val valueToJson: ValueToJson = ValueToJson({
    case Type.Const.Defined(pn, t) =>
      for {
        pack <- packs.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
      } yield dt
  })
  type Cache = Option[CMap[String, (Future[Value], Type)]]
  type ToLFV = Option[LetFreeEvaluation.LetFreeValue => Future[Value]]

  def exprFn(
      wrapper: (
          LetFreeEvaluation.LetFreeValue,
          rankn.Type,
          LetFreeEvaluation.Cache,
          LetFreeEvaluation.ToLFV
      ) => Any
  )(implicit extEnv: LetFreeEvaluation.ExtEnv, cache: Cache): FfiCall = {

    def evalExprFn(t: rankn.Type): LetFreeEvaluation.ExprFnValue =
      LetFreeEvaluation.ExprFnValue({ (e1, cache, eval) =>
        Value.ExternalValue(wrapper(e1, t, cache, eval))
      })

    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t)) }
  }
}
