package org.bykn.bosatsu

import cats.Eval
import Normalization.NormalExpressionTag
import scala.annotation.tailrec
import rankn.Type
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

object NormalEvaluation {
  implicit val valueToLitValue: Value => Option[Normalization.LitValue] = {v => Some(Normalization.LitValue(v.asExternal.toAny))}
  implicit val valueToStruct: Value => Option[(Int, List[Value])] = {v =>
    val vSum = v.asSum
    Some((vSum.variant, vSum.value.toList))
  }
  implicit val valueToList: Value => Option[List[Value]] = { value =>
    @tailrec
    def loop(v: Value, acc: List[Value]): List[Value] = {
      val vSum = v.asSum
      vSum.variant match {
        case 0 => acc
        case 1 => vSum.value.toList match {
          case List(h, t) => loop(t, h::acc)
          case _ => sys.error("typechecking should make sure we have exactly two args here")
        }
          case n => sys.error(s"typechecking should make sure this is only 0 or 1 and not $n")
      }
    }
    Some(loop(value, Nil).reverse)
  }

  implicit val valueFromList: List[Value] => Value = { fullList =>
    @tailrec
    def loop(lst: List[Value], acc: Value): Value = lst match {
      case Nil => acc
      case head::tail => loop(tail, Value.SumValue(1, Value.ProductValue.fromList(List(head, acc))))
    }
    loop(fullList.reverse, Value.SumValue(0, Value.UnitValue))
  }

  sealed trait NormalValue {}
  case class LazyValue(expression: NormalExpression, scope: List[NormalValue]) extends NormalValue {
    def cleanedScope: List[(Int, NormalValue)] = expression.varSet.toList.sorted.map { n => (n, scope(n)) }
  }
  case class ComputedValue(value: Value) extends NormalValue

  implicit def nvToLitValue(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): NormalValue => Option[Normalization.LitValue] = { nv =>
    valueToLitValue(nvToV(nv))
  }
  implicit def nvToStruct(
      implicit extEnv: ExtEnv,
      cache: Cache
  ): NormalValue => Option[(Int, List[NormalValue])] =
    nv =>
      nv match {
        case ComputedValue(v) => valueToStruct(v).map {case (n, lst) => (n, lst.map(vv => ComputedValue(vv)))}
        case LazyValue(expr, scope) => expr match {
            case NormalExpression.Struct(n, lst) => Some((n, lst.map(ne => LazyValue(ne, scope))))
            case NormalExpression.App(fn, arg) => nvToStruct.apply(applyApplyable(evalToApplyable(LazyValue(fn, scope)), LazyValue(arg, scope)))
            case NormalExpression.ExternalVar(p, n, tpe) =>
              nvToStruct.apply(ComputedValue(extEnv(n).value))
            case NormalExpression.Recursion(NormalExpression.Lambda(expr)) =>
              nvToStruct.apply(LazyValue(expr, nv :: scope))
            case NormalExpression.LambdaVar(index) =>
              nvToStruct.apply(scope(index))
            case mtch @ NormalExpression.Match(_, _) =>
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
  ): NormalValue => Option[List[NormalValue]] = { normalValue =>
    @tailrec
    def loop(nv: NormalValue, acc: List[NormalValue]): List[NormalValue] = {
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
  ): List[NormalValue] => NormalValue = { scope =>
    @tailrec
    def loop(n: Int, acc: NormalExpression): NormalExpression = n match {
      case 0 => acc
      case _ =>
        loop(
          n - 1,
          NormalExpression
            .Struct(1, List(NormalExpression.LambdaVar(n - 1), acc))
        )
    }
    LazyValue(loop(scope.length, NormalExpression.Struct(0, Nil)), scope)
  }

  type Applyable = Either[Value, (NormalExpression.Lambda, List[NormalValue])]
  type ExtEnv = Map[Identifier, Eval[Value]]
  type Cache = Option[TrieMap[String, (Future[Value], rankn.Type)]]
  type ToLFV = Option[NormalValue => Future[Value]]

  def nvToV(nv: NormalValue)(implicit extEnv: ExtEnv, cache: Cache): Value =
    nv match {
      case LazyValue(ne, scope) => evalToValue(ne, scope)
      case ComputedValue(value) => value
    }

  import scala.concurrent.ExecutionContext.Implicits.global
  def applyApplyable(
      applyable: Applyable,
      arg: NormalValue
  )(implicit extEnv: ExtEnv, cache: Cache): NormalValue = applyable match {
    case Left(v) =>
      v.attemptExprFn match {
        case Left(eFn) =>
          ComputedValue(eFn(arg, cache, Some(nv => Future(nvToV(nv)))))
        case Right(fn) => ComputedValue(fn(nvToV(arg)))
      }
    case Right((NormalExpression.Lambda(expr), scope)) =>
      LazyValue(expr, arg :: scope)
  }

  def evalToApplyable(
      nv: NormalValue
  )(implicit extEnv: ExtEnv, cache: Cache): Applyable = nv match {
    case ComputedValue(value) => Left(value)
    case LazyValue(ne, scope) =>
      ne match {
        case lambda @ NormalExpression.Lambda(_) => Right((lambda, scope))
        case NormalExpression.App(fn, arg) =>
          evalToApplyable(
            applyApplyable(
              evalToApplyable(LazyValue(fn, scope)),
              LazyValue(arg, scope)
            )
          )
        case NormalExpression.ExternalVar(p, n, tpe) => Left(extEnv(n).value)
        case NormalExpression.Recursion(NormalExpression.Lambda(expr)) =>
          evalToApplyable(LazyValue(expr, nv :: scope))
        case NormalExpression.LambdaVar(index) => evalToApplyable(scope(index))
        case mtch @ NormalExpression.Match(_, _) =>
          evalToApplyable(simplifyMatch(mtch, scope))
        case other =>
          sys.error(
            s"Type checking should mean this isn't a struct or a literal: $other"
          )
      }
  }

  def simplifyMatch(
      mtch: NormalExpression.Match,
      scope: List[NormalValue]
  )(implicit extEnv: ExtEnv, cache: Cache): NormalValue = {
    val (_, patEnv, result) = mtch.branches.toList
      .collectFirst(Function.unlift({
        case (pat, result) =>
          Normalization
            .maybeBind[NormalValue](pat)(
              nvToLitValue(extEnv, cache),
              nvToStruct(extEnv, cache),
              nvToList(extEnv, cache),
              nvFromList(extEnv, cache)
            )
            .apply(LazyValue(mtch.arg, scope), Map()) match {
            case Normalization.Matches(env) => Some((pat, env, result))
            case Normalization.NoMatch      => None
            case Normalization.NotProvable =>
              sys.error("For value we should never be NotProvable")
          }
      }))
      .get

    ((patEnv.size - 1) to 0 by -1)
      .map(patEnv.get(_).get)
      .foldLeft[NormalValue](LazyValue(result, scope)) { (fn, arg) =>
        applyApplyable(evalToApplyable(fn), arg)
      }
  }

  def evalToValue(
      ne: NormalExpression,
      scope: List[NormalValue]
  )(implicit extEnv: ExtEnv, cache: Cache): Value = ne match {
    case NormalExpression.App(fn, arg) => {
      val applyable = evalToApplyable(LazyValue(fn, scope))
      nvToV(applyApplyable(applyable, LazyValue(arg, scope)))
    }
    case NormalExpression.ExternalVar(p, n, tpe) => extEnv(n).value
    case mtch @ NormalExpression.Match(_, _) =>
      nvToV(simplifyMatch(mtch, scope))
    case NormalExpression.LambdaVar(index) => nvToV(scope(index))
    case NormalExpression.Lambda(expr) =>
      Value.FnValue(v => evalToValue(expr, ComputedValue(v) :: scope))
    case NormalExpression.Struct(enum, args) =>
      Value.SumValue(
        enum,
        Value.ProductValue.fromList(args.map(arg => evalToValue(arg, scope)))
      )
    case NormalExpression.Literal(lit) => Value.fromLit(lit)
    case NormalExpression.Recursion(lambda) => {
      lambda match {
        case NormalExpression.Lambda(expr) => {
          val nextScope = LazyValue(ne, scope) :: scope
          evalToValue(expr, nextScope)
        }
        case _ => sys.error("A Recursion should always contain a Lambda")
      }
    }
  }
}

case class NormalEvaluation(
    packs: PackageMap.Typed[(Declaration, NormalExpressionTag)],
    externals: Externals,
    cache: NormalEvaluation.Cache
) {

  def evaluateLast(p: PackageName): Option[(Value, NormalExpression, Type)] =
    for {
      pack <- packs.toMap.get(p)
      (name, _, tpe) <- pack.program.lets.lastOption
      ne = tpe.tag._2.ne
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      NormalEvaluation.evalToValue(ne, Nil)(extEnv, cache),
      ne,
      tpe.getType
    )

  def evaluateName(
      p: PackageName,
      name: Identifier
  ): Option[(Value, NormalExpression, Type)] =
    for {
      pack <- packs.toMap.get(p)
      (_, _, tpe) <- pack.program.lets.reverse.collectFirst(Function.unlift {
        tup => if (tup._1 == name) Some(tup) else None
      })
      ne = tpe.tag._2.ne
      extEnv = externalEnv(pack) ++ importedEnv(pack)
    } yield (
      NormalEvaluation.evalToValue(ne, Nil)(extEnv, cache),
      ne,
      tpe.getType
    )

  private def externalEnv(
      p: Package.Typed[(Declaration, NormalExpressionTag)]
  ): NormalEvaluation.ExtEnv = {
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
      p: Package.Typed[(Declaration, NormalExpressionTag)]
  ): NormalEvaluation.ExtEnv =
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
}
