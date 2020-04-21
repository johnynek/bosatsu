package org.bykn.bosatsu

import cats.Eval
import Normalization.NormalExpressionTag
import scala.annotation.tailrec
import rankn.Type

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
  case class LazyValue(expression: NormalExpression, scope: List[NormalValue]) extends NormalValue
  case class ComputedValue(value: Value) extends NormalValue

  type Applyable = Either[Value, (NormalExpression.Lambda, List[NormalValue])]

  def nvToV(nv: NormalValue)(implicit extEnv: Map[Identifier, Eval[Value]]): Value = nv match {
    case LazyValue(ne, scope) => evalToValue(ne, scope)
    case ComputedValue(value) => value
  }

  def applyApplyable(applyable: Applyable, arg: NormalValue)(implicit extEnv: Map[Identifier, Eval[Value]]): NormalValue = applyable match {
    case Left(v) => ComputedValue(v.asFn(nvToV(arg)))
    case Right((NormalExpression.Lambda(expr), scope)) => LazyValue(expr, arg :: scope)
  }

  def evalToApplyable(nv: NormalValue)(implicit extEnv: Map[Identifier, Eval[Value]]): Applyable = nv match {
    case ComputedValue(value) => Left(value)
    case LazyValue(ne, scope) => ne match {
      case lambda@NormalExpression.Lambda(_) => Right((lambda, scope))
      case NormalExpression.App(fn, arg) => evalToApplyable(applyApplyable(evalToApplyable(LazyValue(fn, scope)), LazyValue(arg, scope)))
      case NormalExpression.ExternalVar(p, n) => Left(extEnv(n).value)
      case NormalExpression.Recursion(NormalExpression.Lambda(expr)) => evalToApplyable(LazyValue(expr, nv :: scope))
      case NormalExpression.LambdaVar(index) => evalToApplyable(scope(index))
      case NormalExpression.Match(_, _) => evalToApplyable(ComputedValue(nvToV(nv)))
      case other => sys.error(s"Type checking should mean this isn't a struct or a literal: $other")
    }
  }

  def evalToValue(ne: NormalExpression, scope: List[NormalValue])(implicit extEnv: Map[Identifier, Eval[Value]]): Value = ne match {
    case NormalExpression.App(fn, arg) => {
      val applyable = evalToApplyable(LazyValue(fn, scope))
      nvToV(applyApplyable(applyable, LazyValue(arg, scope)))
    }
    case NormalExpression.ExternalVar(p, n) => extEnv(n).value
    case NormalExpression.Match(arg, branches) => {
      val argVal = evalToValue(arg, scope)
      val (_, patEnv, result) = branches.toList.collectFirst(Function.unlift( { case (pat, result) =>
        Normalization.maybeBind[Value](pat).apply(argVal, Map()) match {
          case Normalization.Matches(env) => Some((pat, env, result))
          case Normalization.NoMatch => None
          case Normalization.NotProvable => sys.error("For value we should never be NotProvable")
        }
      })).get
      ((patEnv.size - 1) to 0 by -1).map(patEnv.get(_).get)
        .foldLeft(evalToValue(result, scope)) {case (fn, arg) => fn.asFn(arg) }
    }
    case NormalExpression.LambdaVar(index) => nvToV(scope(index))
    case NormalExpression.Lambda(expr) => Value.FnValue(v => evalToValue(expr, ComputedValue(v) :: scope))
    case NormalExpression.Struct(enum, args) => Value.SumValue(enum, Value.ProductValue.fromList(args.map(arg => evalToValue(arg, scope))))
    case NormalExpression.Literal(lit) => Value.fromLit(lit)
    case NormalExpression.Recursion(lambda) => {
      lambda match {
        case NormalExpression.Lambda(expr) => {
          lazy val recFn: Value = Value.FnValue { v =>
            val nextScope = ComputedValue(recFn) :: scope
            val fn = evalToValue(expr, nextScope)
            fn.asFn(v)
          }
          recFn
        }
        case _ => sys.error("A Recursion should always contain a Lambda")
      }
    }
  }
}

case class NormalEvaluation(packs: PackageMap.Typed[(Declaration, NormalExpressionTag)], externals: Externals) {

  
  def evaluateLast(p: PackageName): Option[(Value, NormalExpression)] = for {
    pack <- packs.toMap.get(p)
    (name, _, tpe) <- pack.program.lets.lastOption
    ne = tpe.tag._2.ne
    _ = println(s"ne $ne")
    _ = println(s"type ${tpe.getType}")
    _ = tpe.getType match {
      case Type.TyConst(Type.Const.Defined(p, t)) => println(s"type def ${pack.program.types.getType(p, t)}")
      case other => println(s"other $other")
    }
    extEnv = externalEnv(pack) ++ importedEnv(pack)
  } yield (NormalEvaluation.evalToValue(ne, Nil)(extEnv), ne)

  def evaluateName(p: PackageName, name: Identifier): Option[(Value, NormalExpression)] = for {
    pack <- packs.toMap.get(p)
    (_, _, tpe) <- pack.program.lets.reverse.collectFirst(Function.unlift { tup => if(tup._1 == name) Some(tup) else None })
    ne = tpe.tag._2.ne
    _ = println(s"ne $ne")
    extEnv = externalEnv(pack) ++ importedEnv(pack)
  } yield (NormalEvaluation.evalToValue(ne, Nil)(extEnv), ne)

  private def externalEnv(p: Package.Typed[(Declaration, NormalExpressionTag)]): Map[Identifier, Eval[Value]] = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
          // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, Eval.later(ext.call(tpe)))
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
          // $COVERAGE-ON$
      }
    }
      .toMap
  }

  private def importedEnv(p: Package.Typed[(Declaration, NormalExpressionTag)]): Map[Identifier, Eval[Value]] =
    p.imports.iterator.flatMap { imp =>
      val pack = packs.toMap.get(imp.pack.name) match {
        case Some(p) => p
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown package: ${imp.pack.name}")
          // $COVERAGE-ON$
      }
      val exts = externalEnv(pack)
      imp.items
        .toList
        .iterator
        .flatMap { in =>
          exts.get(in.originalName).map { value =>
            (in.localName, value)
          }
        }
    }
      .toMap
}
