package org.bykn.bosatsu

object Normalization {
  import TypedExpr._

  sealed abstract class NormalExpression {
    val maxLambdaVar: Option[Int]
  }

  case class NormalNothing() extends NormalExpression {
    val maxLambdaVar = None
  }

  case class NormalExpressionTag(ne: NormalExpression, children: List[NormalExpression])

  def normalizeTag(tag: Declaration) = (tag, NormalExpressionTag(NormalNothing(), Nil))

  def normalizeExpr(expr: TypedExpr[Declaration]): TypedExpr[(Declaration, Normalization.NormalExpressionTag)] = {
      expr match {
        case a@Annotation(term, _, tag) => a.copy(term=normalizeExpr(term), tag=normalizeTag(tag))
        case g@Generic(_, in, tag) =>
          g.copy(in=normalizeExpr(in), tag=normalizeTag(tag))
        case v@Var(_, _, _, tag) => v.copy(tag=normalizeTag(tag))
        case al@AnnotatedLambda(_, _, expr, tag) =>
          al.copy(expr=normalizeExpr(expr), tag=normalizeTag(tag))
        case a@App(fn, arg, _, tag) =>
          a.copy(fn=normalizeExpr(fn), arg=normalizeExpr(arg), tag=normalizeTag(tag))
        case l@Let(_, expr, in, _, tag) =>
          l.copy(expr=normalizeExpr(expr), in=normalizeExpr(in), tag=normalizeTag(tag))
        case l@Literal(_, _, tag) => l.copy(tag=normalizeTag(tag))
        case m@Match(arg, branches, tag) => ???
      }
  }

  def normalizeLet(inferredExpr: (String, RecursionKind, TypedExpr[Declaration])): (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)]) = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    (inferredExpr._1, inferredExpr._2, normalizeExpr(inferredExpr._3))
  }

  def normalizeProgram[T, S](inferredProgram: Program[T, TypedExpr[Declaration], S]): Program[T, TypedExpr[(Declaration, Normalization.NormalExpressionTag)], S] = {
    inferredProgram.copy(
      lets  = inferredProgram.lets.map(normalizeLet),
    )
  }

  def normalizePackage(pkg: Package.Inferred): Package.Normalized = {
    pkg.copy(
      program = normalizeProgram(pkg.program)
    )
  }

  def normalizePackageMap(pkgMap: PackageMap.Inferred): PackageMap.Normalized = {
    PackageMap(pkgMap.toMap.map { case (name, pkg) => {
      (name, normalizePackage(pkg))
    }})
  }
}

