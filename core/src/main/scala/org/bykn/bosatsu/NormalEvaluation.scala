package org.bykn.bosatsu

import cats.Eval
import Normalization.NormalExpressionTag
import scala.annotation.tailrec

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
}

case class NormalEvaluation(packs: PackageMap.Typed[(Declaration, NormalExpressionTag)], externals: Externals) {
  import NormalEvaluation._

  def evaluateLast(p: PackageName): Option[Value] = for
  {
    pack <- packs.toMap.get(p)
    (name, _, tpe) <- pack.program.lets.lastOption
    ne = tpe.tag._2.ne
    extEnv = externalEnv(pack) ++ importedEnv(pack)
  } yield eval(ne, Nil, extEnv)

  def eval(ne: NormalExpression, scope: List[Value], extEnv: Map[Identifier, Eval[Value]]): Value = ne match {
    case NormalExpression.App(fn, arg) => {
      val fnVal = eval(fn, scope, extEnv)
      val argVal = eval(arg, scope, extEnv)
      fnVal.asFn.apply(argVal)
    }
    case NormalExpression.ExternalVar(p, n) => extEnv(n).value
    case NormalExpression.Match(arg, branches) => {
      val argVal = eval(arg, scope, extEnv)
      val (_, patEnv, result) = branches.toList.collectFirst(Function.unlift( { case (pat, result) =>
        Normalization.maybeBind[Value](pat).apply(argVal, Map()) match {
          case Normalization.Matches(env) => Some((pat, env, result))
          case Normalization.NoMatch => None
          case Normalization.NotProvable => sys.error("For value we should never be NotProvable")
        }
      })).get
      ((patEnv.size - 1) to 0 by -1).map(patEnv.get(_).get)
        .foldLeft(eval(result, scope, extEnv)) {case (fn, arg) => fn.asFn(arg) }
    }
    case NormalExpression.LambdaVar(index) => scope(index)
    case NormalExpression.Lambda(expr) => Value.FnValue(v => eval(expr, v :: scope, extEnv))
    case NormalExpression.Struct(enum, args) => Value.SumValue(enum, Value.ProductValue.fromList(args.map(arg => eval(arg, scope, extEnv))))
    case NormalExpression.Literal(lit) => Value.fromLit(lit)
    case NormalExpression.Recursion(lambda) => {
      val outerFn = eval(lambda, scope, extEnv)
      lazy val recFn: Value = outerFn.asFn.apply(recFn)
      recFn
    }
  }

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
