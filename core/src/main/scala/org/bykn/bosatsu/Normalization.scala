package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import com.stripe.dagon.Memoize

sealed abstract class NormalExpression {
  val maxLambdaVar: Option[Int]
}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression)
      extends NormalExpression {
    val maxLambdaVar = (fn.maxLambdaVar.toList ++ arg.maxLambdaVar.toList)
      .reduceLeftOption(Math.max)
  }
  case class ExternalVar(pack: PackageName, defName: String)
      extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Match(arg: NormalExpression,
                   branches: NonEmptyList[(Int, NormalExpression)])
      extends NormalExpression {
    val maxLambdaVar =
      (arg.maxLambdaVar.toList ++ branches.toList.flatMap(_._2.maxLambdaVar))
        .reduceLeftOption(Math.max)
  }
  case class LambdaVar(index: Int) extends NormalExpression {
    val maxLambdaVar = Some(index)
  }
  case class Lambda(expr: NormalExpression) extends NormalExpression {
    val maxLambdaVar = expr.maxLambdaVar.map(_ - 1).filter(_ >= 0)
  }
  case class Struct(enum: Int, args: List[NormalExpression])
      extends NormalExpression {
    val maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends NormalExpression {
    val maxLambdaVar = None
  }
  case class NormalNothing() extends NormalExpression {
    val maxLambdaVar = None
  }
}

object Normalization {
  case class NormalExpressionTag(ne: NormalExpression, children: List[NormalExpression])

  def normalizePackageMap(pkgMap: PackageMap.Inferred): PackageMap.Normalized = {
    PackageMap(pkgMap.toMap.map { case (name, pkg) => {
      (name, NormalizePackage(pkg).normalizePackage())
    }})
  }
}

case class NormalizePackage(pack: Package.Inferred) {
  import Normalization._
  import TypedExpr._
  def normalizeTag(tag: Declaration) = (tag, NormalExpressionTag(NormalExpression.NormalNothing(), Nil))

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env): TypedExpr[(Declaration, Normalization.NormalExpressionTag)] = {
      expr match {
        case a@Annotation(_, _, _) => normalizeAnotation(a, env)
        case g@Generic(_, _, _) => normalizeGeneric(g, env)
        case v@Var(_, _, _, _) => normalizeVar(v, env)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al, env)
        case a@App(_, _, _, _) => normalizeApp(a, env)
        case l@Let(_, _, _, _, _) => normalizeLet(l, env)
        case l@Literal(_, _, _) => normalizeLiteral(l, env)
        case m@Match(_, _, _) => normalizeMatch(m, env)
      }
  }

  def normalizeAnotation(a: Annotation[Declaration], env: Env) =
    a.copy(term=normalizeExpr(a.term, env), tag=normalizeTag(a.tag))

  def normalizeGeneric(g: Generic[Declaration], env: Env) =
    g.copy(in=normalizeExpr(g.in, env), tag=normalizeTag(g.tag))

  def normalizeVar(v: Var[Declaration], env: Env) =
    v.copy(tag=normalizeTag(v.tag))

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env) =
    al.copy(expr=normalizeExpr(al.expr, env), tag=normalizeTag(al.tag))

  def normalizeApp(a: App[Declaration], env: Env) =
    a.copy(fn=normalizeExpr(a.fn, env), arg=normalizeExpr(a.arg, env), tag=normalizeTag(a.tag))

  def normalizeLet(l: Let[Declaration], env: Env) =
    l.copy(expr=normalizeExpr(l.expr, env), in=normalizeExpr(l.in, env), tag=normalizeTag(l.tag))

  def normalizeLiteral(l: Literal[Declaration], env: Env) =
    l.copy(tag=normalizeTag(l.tag))

  def normalizeMatch(m: Match[Declaration], env: Env) =
    Match(arg=normalizeExpr(m.arg, env),
      branches=m.branches.map { case (p, expr) => (p, normalizeExpr(expr, env))},
      tag=normalizeTag(m.tag)
      )

  def normalizePackageLet(inferredExpr: (String, RecursionKind, TypedExpr[Declaration])): (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)]) = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    (inferredExpr._1, inferredExpr._2, normalizeExpr(inferredExpr._3, (Map(),Nil)))
  }

  def normalizeProgram[T, S](inferredProgram: Program[T, TypedExpr[Declaration], S]): Program[T, TypedExpr[(Declaration, Normalization.NormalExpressionTag)], S] = {
    inferredProgram.copy(
      lets  = inferredProgram.lets.map(normalizePackageLet),
    )
  }

  def normalizePackage(): Package.Normalized = {
    pack.copy(
      program = normalizeProgram(pack.program)
    )
  }

  private type Ref =
    Either[(String, Declaration), TypedExpr[Declaration]]
  private type ResultingRef = TypedExpr[(Declaration,  NormalExpressionTag)]
  private type Env = (Map[String, NormalExpressionTag], List[Option[String]])
  
  private[this] val norm: ((Package.Inferred, Ref, Env)) => State[
    Map[(PackageName, String), ResultingRef],
    ResultingRef] =
    Memoize
      .function[(Package.Inferred, Ref, Env),
                State[Map[(PackageName, String), ResultingRef], ResultingRef]] {
        case ((pack, Right(expr), env), recurse) =>
          State.pure(normalizeExpr(expr, env))
        case ((pack, Left((item, t)), env), recurse) =>
          NameKind(pack, item).get match { // this get should never fail due to type checking
            case NameKind.Let(expr) =>
              for {
                lookup <- State.inspect {
                  lets: Map[(PackageName, String), ResultingRef] =>
                    lets.get((pack.unfix.name, item))
                }
                res <- lookup match {
                  case Some(res) =>
                    State.pure(res): State[
                      Map[(PackageName, String), ResultingRef],
                      ResultingRef]
                  case None =>
                    for {
                      res <- recurse((pack, Right(expr), env))
                      _ <- State.modify {
                        lets: Map[(PackageName, String), ResultingRef] =>
                          lets + ((pack.unfix.name, item) -> res)
                      }
                    } yield res

                }
              } yield Expr.Var(item, addNEToTag(t, res.tag._3))
            case NameKind.Constructor(cn, dt, schm) => {
              val ne = constructor(cn, dt)
              State.pure(Expr.Var(item, addNEToTag(t, NETag(ne, Set()))))
            }
            case NameKind.Import(from, orig) => {
              // we reset the environment in the other package
              recurse((from, Left((orig, t)), (Map.empty, Nil)))
            }
            case NameKind.ExternalDef(pn, n, scheme) => {
              val ne = NormalExpression.ExternalVar(pn, n)
              State.pure(Expr.Var(item, addNEToTag(t, NETag(ne, Set()))))
            }
          }
      }
}

