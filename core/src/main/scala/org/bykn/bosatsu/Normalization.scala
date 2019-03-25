package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._

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
}

case class NormalizePackageMap(pm: PackageMap.Inferred) {
  import Normalization._
  import TypedExpr._
  def normalizeTag(tag: Declaration) = (tag, NormalExpressionTag(NormalExpression.NormalNothing(), Nil))

  val normalizePackageMap: PackageMap.Normalized = 
    PackageMap(
      pm.toMap.toList.map { case (name, pkg) => {
        normalizePackage(name, pkg).map((name, _))
    }}.sequence.run(Map()).value._2.toMap)

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
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

  def normalizeAnotation(a: Annotation[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      term <- normalizeExpr(a.term, env)
    }  yield a.copy(term=term, tag=normalizeTag(a.tag))

  def normalizeGeneric(g: Generic[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      in <- normalizeExpr(g.in, env)
    } yield g.copy(in=in, tag=normalizeTag(g.tag))

  def normalizeVar(v: Var[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    State.pure(v.copy(tag=normalizeTag(v.tag)))

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        expr <- normalizeExpr(al.expr, env)
      }
        yield al.copy(expr=expr, tag=normalizeTag(al.tag))

  def normalizeApp(a: App[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        fn <- normalizeExpr(a.fn, env)
        arg <- normalizeExpr(a.arg, env)
      } yield a.copy(fn=fn, arg=arg, tag=normalizeTag(a.tag))

  def normalizeLet(l: Let[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        expr <- normalizeExpr(l.expr, env)
        in <- normalizeExpr(l.in, env)
      } yield l.copy(expr=expr, in=in, tag=normalizeTag(l.tag))

  def normalizeLiteral(l: Literal[Declaration], env: Env): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    State.pure(l.copy(tag=normalizeTag(l.tag)))

  def normalizeMatch(m: Match[Declaration], env: Env): State[
  Map[(PackageName, String), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      arg <- normalizeExpr(m.arg, env)
      branches <- (m.branches.map { case (p, expr) => normalizeExpr(expr, env).map((p, _))}).sequence
    }
    yield Match(arg=arg,
      branches=branches,
      tag=normalizeTag(m.tag)
      )

  def normalizePackageLet(pkgName: PackageName, inferredExpr: (String, RecursionKind, TypedExpr[Declaration])): State[
  Map[(PackageName, String), ResultingRef], (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)])] = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    for {
      expr <- normalizeExpr(inferredExpr._3, (Map(), Nil))
      _ <- State.modify { cache: Map[(PackageName, String), ResultingRef] => cache + ((pkgName, inferredExpr._1) -> Right(expr))}
    }
      yield (inferredExpr._1, inferredExpr._2, expr)
  }

  def normalizeProgram[T, S](pkgName: PackageName, inferredProgram: Program[T, TypedExpr[Declaration], S]): State[
  Map[(PackageName, String), ResultingRef],
  Program[T, TypedExpr[(Declaration, Normalization.NormalExpressionTag)], S]] = {
    for { 
      lets <- inferredProgram.lets.map(normalizePackageLet(pkgName, _)).sequence
    } yield inferredProgram.copy(
      lets  = lets
    )
  }

  def normalizePackage(pkgName: PackageName, pack: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  Package.Normalized] = for {
    program <- normalizeProgram(pkgName, pack.program)
  } yield pack.copy(program = program)

  def getTag(ref: ResultingRef) = ref match {
    case Right(te) => te.tag
    case Left((_, t)) => t
  }

  private type Ref[T] =
    Either[(String, T), TypedExpr[T]]

  private type SourceRef = Ref[Declaration]
  private type ResultingRef = Ref[(Declaration,  NormalExpressionTag)]
  private type Env = (Map[String, NormalExpressionTag], List[Option[String]])
  private def norm(input: (Package.Inferred, SourceRef, Env)): State[
    Map[(PackageName, String), ResultingRef],
    ResultingRef] = input match {
        case ((pack, Right(expr), env)) =>
          for {
            expr <- normalizeExpr(expr, env)
          } yield Right(expr)
        case (pack, Left((item, t)), env) =>
          NameKind(pack, item).get match { // this get should never fail due to type checking
            case NameKind.Let(_, _, expr) =>
              for {
                lookup <- State.inspect {
                  lets: Map[(PackageName, String), ResultingRef] =>
                    lets.get((pack.name, item))
                }
                res <- lookup match {
                  case Some(res) =>
                    State.pure(res): State[
                      Map[(PackageName, String), ResultingRef],
                      ResultingRef]
                  case None =>
                    for {
                      res <- norm((pack, Right(expr), env))
                      _ <- State.modify {
                        lets: Map[(PackageName, String), ResultingRef] =>
                          lets + ((pack.name, item) -> res)
                      }
                    } yield res

                }
              } yield Left((item, getTag(res)))
            case NameKind.Constructor(cn, params, schm, vt) => {
              val neTag = NormalExpressionTag(NormalExpression.NormalNothing(), Nil)
              // constructor(cn, dt)
              State.pure(Left((item, (t, neTag))))
            }
            case NameKind.Import(from, orig) => {
              // we reset the environment in the other package
              for {
                imported <- norm((pm.toMap(from.unfix.name), Left((orig, t)), (Map.empty, Nil)))
                neTag = getTag(imported)._2
              } yield Left((item, (t, neTag)))
            }
            case NameKind.ExternalDef(pn, n, scheme) => {
              val neTag = NormalExpressionTag(NormalExpression.ExternalVar(pn, n), Nil)
              State.pure(Left((item, (t, neTag))))
            }
          }
      }
}

