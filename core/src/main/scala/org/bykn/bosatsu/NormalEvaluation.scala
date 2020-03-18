package org.bykn.bosatsu

import cats.Eval
import Normalization.NormalExpressionTag

object NormalEvaluation {

}

case class NormalEvaluation(packs: PackageMap.Typed[(Declaration, NormalExpressionTag)], externals: Externals) {
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
    case NormalExpression.Match(arg, branches) => ???
    case NormalExpression.LambdaVar(index) => scope(index)
    case NormalExpression.Lambda(expr) => ???
    case NormalExpression.Struct(enum, args) => ???
    case NormalExpression.Literal(lit) => Value.fromLit(lit)
    case NormalExpression.Recursion(lambda) => ???
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
