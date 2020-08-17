package org.bykn.bosatsu

import cats.Eval
import LetFreeConversion.LetFreeExpressionTag
import scala.annotation.tailrec
import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}

object LetFreeEvaluation {
  import LetFreeConversion.LitValue

  implicit val valueToLitValue: Value => Option[LitValue] = { v =>
    Some(LitValue(v.asExternal.toAny))
  }
  implicit val valueToStruct: Value => Option[(Int, List[Value])] = { v =>
    val vSum = v.asSum
    Some((vSum.variant, vSum.value.toList))
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
      case ilv @ LazyValue(expression, scope) =>
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
  }

  case class LazyValue(expression: LetFreeExpression, scope: List[LetFreeValue]) extends LetFreeValue {
    def cleanedScope: List[(Int, LetFreeValue)] =
      expression.varSet.toList.sorted.map { n => (n, scope(n)) }
  }

  case class ComputedValue(value: Value) extends LetFreeValue

  implicit def nvToLitValue(
    implicit extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[LetFreeConversion.LitValue] = { nv =>
    valueToLitValue(nvToV(nv))
  }
  implicit def nvToStruct(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[(Int, List[LetFreeValue])] =
    nv =>
      nv match {
        case ComputedValue(v) =>
          valueToStruct(v).map {
            case (n, lst) => (n, lst.map(vv => ComputedValue(vv)))
          }
        case LazyValue(expr, scope) =>
          expr match {
            case LetFreeExpression.Struct(n, lst) =>
              Some((n, lst.map(ne => LazyValue(ne, scope))))
            case LetFreeExpression.App(fn, arg) =>
              nvToStruct.apply(
                applyApplyable(
                  evalToApplyable(LazyValue(fn, scope)),
                  LazyValue(arg, scope)
                )
              )
            case LetFreeExpression.ExternalVar(p, n, tpe) =>
              nvToStruct.apply(ComputedValue(extEnv(n).value))
            case LetFreeExpression.Recursion(LetFreeExpression.Lambda(expr)) =>
              nvToStruct.apply(LazyValue(expr, nv :: scope))
            case LetFreeExpression.LambdaVar(index) =>
              nvToStruct.apply(scope(index))
            case mtch @ LetFreeExpression.Match(_, _) =>
              nvToStruct.apply(simplifyMatch(mtch, scope))
            case other =>
              sys.error(
                s"Type checking should mean this isn't a lambda or a literal: $other"
              )
          }
      }
  implicit def nvToList(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[List[LetFreeValue]] = { normalValue =>
    @tailrec
    def loop(nv: LetFreeValue, acc: List[LetFreeValue]): List[LetFreeValue] = {
      nvToStruct.apply(nv).get match {
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
            .Struct(1, List(LetFreeExpression.LambdaVar(n - 1), acc))
        )
    }
    LazyValue(loop(scope.length, LetFreeExpression.Struct(0, Nil)), scope)
  }

  type Applyable = Either[Value, (LetFreeExpression.Lambda, List[LetFreeValue])]
  type ExtEnv = Map[Identifier, Eval[Value]]
  type Cache = Option[CMap[String, (Future[Value], rankn.Type)]]
  type ToLFV = Option[LetFreeValue => Future[Value]]

  def nvToV(lfv: LetFreeValue)(implicit extEnv: ExtEnv, cache: Cache): Value =
    lfv match {
      case LazyValue(ne, scope) => evalToValue(ne, scope)
      case ComputedValue(value) => value
    }

  case class ExprFnValue(toExprFn: (LetFreeValue, Cache, ToLFV) => Value) extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v), None, None)
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
      arg: LetFreeValue
  )(implicit extEnv: ExtEnv, cache: Cache): LetFreeValue = applyable match {
    case Left(v) =>
      attemptExprFn(v) match {
        case Left(eFn) =>
          ComputedValue(eFn(arg, cache, Some(nv => Future(nvToV(nv)))))
        case Right(fn) => ComputedValue(fn(nvToV(arg)))
      }
    case Right((LetFreeExpression.Lambda(expr), scope)) =>
      LazyValue(expr, arg :: scope)
  }

  def evalToApplyable(
      nv: LetFreeValue
  )(implicit extEnv: ExtEnv, cache: Cache): Applyable = nv match {
    case ComputedValue(value) => Left(value)
    case LazyValue(ne, scope) =>
      ne match {
        case lambda @ LetFreeExpression.Lambda(_) => Right((lambda, scope))
        case LetFreeExpression.App(fn, arg) =>
          evalToApplyable(
            applyApplyable(
              evalToApplyable(LazyValue(fn, scope)),
              LazyValue(arg, scope)
            )
          )
        case LetFreeExpression.ExternalVar(p, n, tpe) => Left(extEnv(n).value)
        case LetFreeExpression.Recursion(LetFreeExpression.Lambda(expr)) =>
          evalToApplyable(LazyValue(expr, nv :: scope))
        case LetFreeExpression.LambdaVar(index) => evalToApplyable(scope(index))
        case mtch @ LetFreeExpression.Match(_, _) =>
          evalToApplyable(simplifyMatch(mtch, scope))
        case other =>
          sys.error(
            s"Type checking should mean this isn't a struct or a literal: $other"
          )
      }
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
            .apply(LazyValue(mtch.arg, scope), Map()) match {
            case LetFreeConversion.Matches(env) => Some((pat, env, result))
            case LetFreeConversion.NoMatch      => None
            case LetFreeConversion.NotProvable =>
              sys.error("For value we should never be NotProvable")
          }
      }))
      .get

    ((patEnv.size - 1) to 0 by -1)
      .map(patEnv.get(_).get)
      .foldLeft[LetFreeValue](LazyValue(result, scope)) { (fn, arg) =>
        applyApplyable(evalToApplyable(fn), arg)
      }
  }

  def evalToValue(
      ne: LetFreeExpression,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Value = ne match {
    case LetFreeExpression.App(fn, arg) => {
      val applyable = evalToApplyable(LazyValue(fn, scope))
      nvToV(applyApplyable(applyable, LazyValue(arg, scope)))
    }
    case LetFreeExpression.ExternalVar(p, n, tpe) => extEnv(n).value
    case mtch @ LetFreeExpression.Match(_, _) =>
      nvToV(simplifyMatch(mtch, scope))
    case LetFreeExpression.LambdaVar(index) => nvToV(scope(index))
    case LetFreeExpression.Lambda(expr) =>
      Value.FnValue(v => evalToValue(expr, ComputedValue(v) :: scope))
    case LetFreeExpression.Struct(enum, args) =>
      Value.SumValue(
        enum,
        Value.ProductValue.fromList(args.map(arg => evalToValue(arg, scope)))
      )
    case LetFreeExpression.Literal(lit) => Value.fromLit(lit)
    case LetFreeExpression.Recursion(lambda) => {
      lambda match {
        case LetFreeExpression.Lambda(expr) => {
          val nextScope = LazyValue(ne, scope) :: scope
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
  ): Value = LetFreeEvaluation.evalToValue(ne, Nil)(extEnv, cache)
}

case class LetFreeEvaluation(
    packs: PackageMap.Typed[(Declaration, LetFreeExpressionTag)],
    externals: Externals
) {

  def evaluateLast(
      p: PackageName
  ): Option[(LetFreeExpression, Type, Map[Identifier, Eval[Value]])] =
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


  def exprFn(wrapper: (LetFreeEvaluation.LetFreeValue, rankn.Type, LetFreeEvaluation.Cache, LetFreeEvaluation.ToLFV) => Any): FfiCall = {

    def evalExprFn(t: rankn.Type): LetFreeEvaluation.ExprFnValue = LetFreeEvaluation.ExprFnValue({ (e1, cache, eval) => Value.ExternalValue(wrapper(e1, t, cache, eval)) })

    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t)) }
  }
}
