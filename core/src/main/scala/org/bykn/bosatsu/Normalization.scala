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
        case a@Annotation(_, _, _) => normalizeAnotation(a)
        case g@Generic(_, _, _) => normalizeGeneric(g)
        case v@Var(_, _, _, _) => normalizeVar(v)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al)
        case a@App(_, _, _, _) => normalizeApp(a)
        case l@Let(_, _, _, _, _) => normalizeLet(l)
        case l@Literal(_, _, _) => normalizeLiteral(l)
        case m@Match(_, _, _) => normalizeMatch(m)
      }
  }

  def normalizeAnotation(a: Annotation[Declaration]) =
    a.copy(term=normalizeExpr(a.term), tag=normalizeTag(a.tag))

  def normalizeGeneric(g: Generic[Declaration]) =
    g.copy(in=normalizeExpr(g.in), tag=normalizeTag(g.tag))

  def normalizeVar(v: Var[Declaration]) =
    v.copy(tag=normalizeTag(v.tag))

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration]) =
    al.copy(expr=normalizeExpr(al.expr), tag=normalizeTag(al.tag))

  def normalizeApp(a: App[Declaration]) =
    a.copy(fn=normalizeExpr(a.fn), arg=normalizeExpr(a.arg), tag=normalizeTag(a.tag))

  def normalizeLet(l: Let[Declaration]) =
    l.copy(expr=normalizeExpr(l.expr), in=normalizeExpr(l.in), tag=normalizeTag(l.tag))

  def normalizeLiteral(l: Literal[Declaration]) =
    l.copy(tag=normalizeTag(l.tag))

  def normalizeMatch(m: Match[Declaration]) =
    Match(arg=normalizeExpr(m.arg),
      branches=m.branches.map { case (p, expr) => (p, normalizeExpr(expr))},
      tag=normalizeTag(m.tag)
      )

  def normalizePackageLet(inferredExpr: (String, RecursionKind, TypedExpr[Declaration])): (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)]) = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    (inferredExpr._1, inferredExpr._2, normalizeExpr(inferredExpr._3))
  }

  def normalizeProgram[T, S](inferredProgram: Program[T, TypedExpr[Declaration], S]): Program[T, TypedExpr[(Declaration, Normalization.NormalExpressionTag)], S] = {
    inferredProgram.copy(
      lets  = inferredProgram.lets.map(normalizePackageLet),
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

