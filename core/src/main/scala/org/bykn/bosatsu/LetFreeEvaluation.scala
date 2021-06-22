package org.bykn.bosatsu

import cats.Eval
// import LetFreeConversion.LetFreeExpressionTag
import scala.annotation.tailrec
import rankn.Type
import scala.concurrent.Future
import scala.collection.concurrent.{Map => CMap}
import scala.collection.immutable.IntMap
import java.math.BigInteger
import org.bykn.bosatsu.rankn.DataFamily
import cats.data.NonEmptyList
import org.bykn.bosatsu.rankn.TypeEnv

import Identifier.{Bindable, Constructor}

/*
 * LetFreeEvaluation exists so that we can verify that LetFreeExpressions do describe the same
 * output that the TypedExpressions that they are converted from describe.
 *
 * It is also useful for prototyping applications that heavily use LetFreeExpressions.
 */
object LetFreeEvaluation {
  import LetFreeConversion.LitValue

  val valueToLitValue: Value => Option[LitValue] = { v =>
    Some(LitValue(v.asExternal.toAny))
  }
  val valueToStruct: (Value, rankn.DataFamily) => Option[(Int, List[Value])] = {
    case (v, df) =>
      df match {
        case rankn.DataFamily.Enum => {
          val vSum = v.asSum
          Some((vSum.variant, vSum.value.toList))
        }
        case rankn.DataFamily.Nat => {
          val vExt = v.asExternal
          val length: BigInteger = vExt.toAny.asInstanceOf[BigInteger]
          if (length.compareTo(BigInteger.ZERO) <= 0) {
            Some((0, Nil))
          } else {
            Some(
              (1, List(Value.ExternalValue(length.add(BigInteger.valueOf(-1)))))
            )
          }
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

  sealed trait LetFreeValue {
    def toJson: Json = this match {
      case ilv @ LazyValue(expression, scope) =>
        Json.JObject(
          List(
            "state" -> Json.JString("expression"),
            "expression" -> Json.JString(expressionToString(expression)),
            "scope" -> Json.JArray(ilv.cleanedScope.map { case (n, nv) =>
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

    def toLeaf: Leaf

    def toValue: Value

    def toStructNat: Option[(Int, List[LetFreeValue])]
    def toStructEnum: Option[(Int, List[LetFreeValue])]
    def toStructStruct: Option[(Int, List[LetFreeValue])]
    def toStructNewType: Option[(Int, List[LetFreeValue])]

    def toStruct(df: rankn.DataFamily): Option[(Int, List[LetFreeValue])] =
      df match {
        case rankn.DataFamily.Nat     => toStructNat
        case rankn.DataFamily.Enum    => toStructEnum
        case rankn.DataFamily.Struct  => toStructStruct
        case rankn.DataFamily.NewType => toStructNewType
      }
  }

  case class VarsTag(exprThunk: () => TypedExpr[VarsTag]) {
    lazy val expr = exprThunk()
    lazy val varSet: Set[String] = ???
  }

  def varTagTypedExpr[X](expr: TypedExpr[X]): TypedExpr[VarsTag] = ???

  def expressionToString(expr: TypedExpr[VarsTag]): String = ???

  case class LazyValue(
      expression: TypedExpr[VarsTag],
      scope: Map[String, LetFreeValue]
  )(implicit extEnvArg: ExtEnv, cacheArg: Cache)
      extends LetFreeValue {
    def cleanedScope: List[(String, LetFreeValue)] =
      expression.tag.varSet.toList.sorted.map { n => (n, scope(n)) }

    lazy val toStructNat: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: Option[(Int, List[LetFreeValue])] =
      lazyToStructImpl(this, rankn.DataFamily.NewType)

    val extEnv = extEnvArg
    val cache = cacheArg

    lazy val toLeaf = evalToLeaf(expression, scope)
    lazy val toValue = toLeaf.toValue
  }

  case class ComputedValue(value: Value) extends LetFreeValue {
    lazy val toStructNat: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Nat)
    lazy val toStructEnum: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Enum)
    lazy val toStructStruct: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.Struct)
    lazy val toStructNewType: Option[(Int, List[LetFreeValue])] =
      computedToStructImpl(this, rankn.DataFamily.NewType)

    lazy val toLeaf = Leaf.Value(this)
    val toValue = value
  }

  def nvToLitValue(implicit
      extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[LetFreeConversion.LitValue] = { lfv =>
    valueToLitValue(lfv.toValue)
  }

  def nvToStruct(
      extEnv: ExtEnv,
      cache: Cache
  ): (LetFreeValue, rankn.DataFamily) => Option[(Int, List[LetFreeValue])] = {
    case (nv, df) => nv.toStruct(df)
  }

  def computedToStructImpl(cv: ComputedValue, df: DataFamily) = cv match {
    case ComputedValue(v) =>
      valueToStruct(v, df).map { case (n, lst) =>
        (n, lst.map(vv => ComputedValue(vv)))
      }
  }

  def lazyToStructImpl(
      lv: LazyValue,
      df: rankn.DataFamily
  )(implicit extEnv: ExtEnv, cache: Cache) =
    lv.toLeaf match {
      case Leaf.Struct(n, values, _)         => Some((n, values))
      case Leaf.Value(cv @ ComputedValue(_)) => cv.toStruct(df)
      // $COVERAGE-OFF$
      case other => sys.error(s"we should not get $other")
      // $COVERAGE-ON$
    }

  sealed abstract class Leaf {
    lazy val toValue = this match {
      case Leaf.Struct(enum, args, df)  => evaluateStruct(enum, args, df)
      case Leaf.Value(ComputedValue(v)) => v
      case Leaf.Lambda(expr, scope, extEnv, cache) =>
        new Value.FnValue(
          LetFreeFnValue(expr, scope)(extEnv, cache)
        )
      case Leaf.Literal(LetFreeExpression.Literal(lit)) => Value.fromLit(lit)
    }
  }
  object Leaf {
    case class Struct(
        n: Int,
        values: List[LetFreeEvaluation.LazyValue],
        df: DataFamily
    ) extends Leaf
    case class Lambda(
        expr: LetFreeExpression.Lambda,
        scope: List[LetFreeValue],
        extEnv: ExtEnv,
        cache: Cache
    ) extends Leaf
    case class Literal(expr: LetFreeExpression.Literal) extends Leaf
    case class Value(value: ComputedValue) extends Leaf
  }

  def evalToLeaf(
      expr: TypedExpr[VarsTag],
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Leaf = expr match {
    case a @ TypedExpr.Annotation(_, _, _) =>
      ??? //letFreeConvertAnnotation(a, env, p)
    case g @ TypedExpr.Generic(_, _, _) =>
      ??? //letFreeConvertGeneric(g, env, p)
    case v @ TypedExpr.Local(_, _, _) => ??? //letFreeConvertLocal(v, env, p)
    case v @ TypedExpr.Global(_, _, _, _) =>
      ??? //letFreeConvertGlobal(v, env, p)
    case al @ TypedExpr.AnnotatedLambda(_, _, _, _) =>
      ??? //letFreeConvertAnnotatedLambda(al, env, p)
    case a @ TypedExpr.App(_, _, _, _)    => ??? //letFreeConvertApp(a, env, p)
    case l @ TypedExpr.Let(_, _, _, _, _) => ??? //letFreeConvertLet(l, env, p)
    case l @ TypedExpr.Literal(_, _, _) =>
      ??? //letFreeConvertLiteral(l, env, p)
    case m @ TypedExpr.Match(_, _, _) => ??? //letFreeConvertMatch(m, env, p)
    case LetFreeExpression.Struct(n, lst, df) =>
      Leaf.Struct(
        n,
        lst.zipWithIndex.map { case (argExpr, i) =>
          LazyValue(argExpr, scope)
        },
        df
      )
    case LetFreeExpression.App(fn, arg) => {
      applyLeaf(
        evalToLeaf(fn, scope),
        LazyValue(arg, scope)
      ).toLeaf
    }
    case LetFreeExpression.ExternalVar(p, n, tpe) =>
      Leaf.Value(ComputedValue(extEnv(n).value))
    case LetFreeExpression.Recursion(LetFreeExpression.Lambda(lambdaExpr)) => {
      lazy val leaf: Leaf =
        evalToLeaf(lambdaExpr, LazyValue(expr, scope) :: scope)
      leaf
    }
    // $COVERAGE-OFF$ we don't have Recursions without lambdas
    case LetFreeExpression.Recursion(notLambda) =>
      sys.error(s"Recursion should always contain a Lambda")
    // $COVERAGE-ON$
    case LetFreeExpression.LambdaVar(index) =>
      scope(index).toLeaf
    case mtch @ LetFreeExpression.Match(_, _) =>
      simplifyMatch(mtch, scope).toLeaf
    case lambda @ LetFreeExpression.Lambda(_) =>
      Leaf.Lambda(lambda, scope, extEnv, cache)
    case lit @ LetFreeExpression.Literal(_) => Leaf.Literal(lit)
  }

  def nvToList(implicit
      extEnv: ExtEnv,
      cache: Cache
  ): LetFreeValue => Option[List[LetFreeValue]] = { normalValue =>
    @tailrec
    def loop(nv: LetFreeValue, acc: List[LetFreeValue]): List[LetFreeValue] = {
      nv.toStruct(rankn.DataFamily.Enum).get match {
        case (0, _)          => acc
        case (1, List(h, t)) => loop(t, h :: acc)
        case _               =>
          // $COVERAGE-OFF$ this should be unreachable
          sys.error(
            "Type checking should only allow this to be applied to a list struct"
          )
        // $COVERAGE-ON$
      }
    }
    Some(loop(normalValue, Nil).reverse)
  }

  def nvFromList(implicit
      extEnv: ExtEnv,
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
      scope
    )
  }

  type Applyable = Either[Value, (LetFreeExpression.Lambda, List[LetFreeValue])]
  type ExtEnv = Map[Identifier, Eval[Value]]
  type Cache = Option[CMap[String, (Future[Value], rankn.Type)]]
  type ToLFV = Option[LetFreeValue => Future[Value]]

  case class ExprFnValue(toExprFn: LetFreeValue => Value)
      extends Value.FnValue.Arg {
    val toFn: Value => Value = { v: Value =>
      toExprFn(ComputedValue(v))
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
  ): Either[LetFreeValue => Value, Value => Value] = v match {
    case fv @ Value.FnValue(f) =>
      fv.arg match {
        case ExprFnValue(ef) => Left(ef)
        case _               => Right(f)
      }
    case other =>
      // $COVERAGE-OFF$ this should be unreachable
      sys.error(s"invalid cast to Fn: $other")
    // $COVERAGE-ON$

  }

  def applyLeaf(
      applyable: Leaf,
      arg: LetFreeValue,
      value: Option[Eval[Value]] = None
  )(implicit extEnv: ExtEnv, cache: Cache): LetFreeValue = applyable match {
    case Leaf.Lambda(LetFreeExpression.Lambda(expr), scope, _, _) => {
      // By evaluating when we add to the scope we don't have to overflow the stack later when we
      // need to use the value
      val _ = arg match {
        case lv @ LazyValue(_, _) => {
          lv.toValue
          ()
        }
        case _ => ()
      }
      LazyValue(expr, arg :: scope)
    }
    case Leaf.Value(lfv) =>
      attemptExprFn(lfv.toValue) match {
        case Left(eFn) =>
          ComputedValue(eFn(arg))
        case Right(fn) => {
          val v = arg.toValue
          ComputedValue(fn(v))
        }
      }
    // $COVERAGE-OFF$ structs aren't applyable
    case _ => sys.error("structs aren't applyable")
    // $COVERAGE-ON$
  }

  case class LetFreeValueMaybeBind(pat: LetFreePattern)(implicit
      extEnv: ExtEnv,
      cache: Cache
  ) extends LetFreeConversion.MaybeBind[LetFreeValue](pat) {
    def toLitValue(t: LetFreeValue): Option[LitValue] =
      nvToLitValue(extEnv, cache)(t)
    def toStruct(t: LetFreeValue, df: DataFamily) =
      nvToStruct(extEnv, cache)(t, df)
    def toList(t: LetFreeValue): Option[List[LetFreeValue]] =
      nvToList(extEnv, cache)(t)
    def fromList(lst: List[LetFreeValue]): LetFreeValue =
      nvFromList(extEnv, cache)(lst)
    def fromString(str: String): LetFreeValue =
      LazyValue(LetFreeExpression.Literal(Lit.Str(str)), Nil)
    def maybeBind(pat: LetFreePattern) = LetFreeValueMaybeBind(pat).apply(_, _)
  }

  def simplifyMatch(
      mtch: LetFreeExpression.Match,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): LetFreeValue = {
    val (_, patEnv, result) = mtch.branches.toList
      .collectFirst(Function.unlift({ case (pat, result) =>
        LetFreeValueMaybeBind(pat)
          .apply(LazyValue(mtch.arg, scope), IntMap.empty) match {
          case LetFreeConversion.Matches(env) => Some((pat, env, result))
          case LetFreeConversion.NoMatch      => None
          // $COVERAGE-OFF$
          case LetFreeConversion.NotProvable =>
            sys.error("For value we should never be NotProvable")
          // $COVERAGE-ON$
        }
      }))
      .get

    ((patEnv.size - 1) to 0 by -1)
      .map(patEnv.get(_).get)
      .foldLeft[LetFreeValue](LazyValue(result, scope)) { (fn, arg) =>
        applyLeaf(fn.toLeaf, arg)
      }
  }

  def evaluateStruct(
      enum: Int,
      args: List[LetFreeValue],
      df: DataFamily
  ): Value = df match {
    case rankn.DataFamily.Enum =>
      Value.SumValue(
        enum,
        Value.ProductValue.fromList(
          args.map(_.toValue)
        )
      )
    case rankn.DataFamily.Nat =>
      if (args.isEmpty) {
        Value.ExternalValue(BigInteger.valueOf(0))
      } else {
        Value.ExternalValue(
          args.head.toValue.asExternal.toAny
            .asInstanceOf[BigInteger]
            .add(BigInteger.ONE)
        )
      }
    case rankn.DataFamily.Struct =>
      Value.ProductValue.fromList(args.map(_.toValue))
    case rankn.DataFamily.NewType => args.head.toValue
  }

  def evalToValue(
      ne: LetFreeExpression,
      scope: List[LetFreeValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Value = LazyValue(ne, scope).toValue

  def evaluate(
      ne: LetFreeExpression,
      extEnv: Map[Identifier, Eval[Value]],
      cache: LetFreeEvaluation.Cache
  ): Value = evalToValue(ne, Nil)(extEnv, cache)

  def exprFn(
      arity: Int,
      wrapper: (
          rankn.Type,
          List[LetFreeValue]
      ) => Value
  ): FfiCall = {
    def evalExprFn(t: rankn.Type, revArgs: List[LetFreeValue]): ExprFnValue =
      if (revArgs.length + 1 < arity) {
        ExprFnValue { arg =>
          new Value.FnValue(evalExprFn(t, arg :: revArgs))
        }
      } else {
        ExprFnValue { case arg =>
          wrapper(t, (arg :: revArgs).reverse)
        }
      }

    FfiCall.FromFn { t => new Value.FnValue(evalExprFn(t, Nil)) }
  }
}

case class LetFreeEvaluation(
    packs: PackageMap.Typed[(Declaration, LetFreeExpressionTag)],
    externals: Externals
) {

  type Pack = Package[Package.Interface, NonEmptyList[
    Referant[Variance]
  ], Referant[Variance], Program[TypeEnv[Variance], TypedExpr[
    (Declaration, LetFreeExpressionTag)
  ], Any]]

  def evaluateCollect(
      p: PackageName,
      fn: Pack => Option[
        (
            Identifier.Bindable,
            RecursionKind,
            TypedExpr[(Declaration, LetFreeExpressionTag)]
        )
      ]
  ) = {
    for {
      pack <- packs.toMap.get(p)
      (name, _, tpe) <- fn(pack)
      lfe = tpe.tag._2
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      lfe,
      tpe.getType,
      extEnv
    )
  }

  def evaluateLast(
      p: PackageName
  ): Option[(LetFreeExpression, Type, Map[Identifier, Eval[Value]])] =
    evaluateCollect(p, { pack => pack.program.lets.lastOption })

  def evalLastTest(p: PackageName) =
    evaluateCollect(p, { pack => Package.testValue(pack) })
      .map { case (lfe, tpe, extEnv) =>
        LetFreeEvaluation.evaluate(lfe, extEnv, None)
      }
      .map(v => Eval.later(Test.fromValue(v)))

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

  val valueToJson: ValueToJson = ValueToJson({ case Type.Const.Defined(pn, t) =>
    for {
      pack <- packs.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
  })
}
