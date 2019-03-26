package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._
import rankn._

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
  case class NormalExpressionTag(ne: NormalExpression, children: Set[NormalExpression])
}

case class NormalizePackageMap(pm: PackageMap.Inferred) {
  import Normalization._
  import TypedExpr._
  def normalizeTag(tag: Declaration) = (tag, NormalExpressionTag(NormalExpression.NormalNothing(), Set()))

  val normalizePackageMap: PackageMap.Normalized = 
    PackageMap(
      pm.toMap.toList.map { case (name, pkg) => {
        normalizePackage(name, pkg).map((name, _))
    }}.sequence.run(Map()).value._2.toMap)

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      expr match {
        case a@Annotation(_, _, _) => normalizeAnotation(a, env, p)
        case g@Generic(_, _, _) => normalizeGeneric(g, env, p)
        case v@Var(_, _, _, _) => normalizeVar(v, env, p)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al, env, p)
        case a@App(_, _, _, _) => normalizeApp(a, env, p)
        case l@Let(_, _, _, _, _) => normalizeLet(l, env, p)
        case l@Literal(_, _, _) => normalizeLiteral(l, env, p)
        case m@Match(_, _, _) => normalizeMatch(m, env, p)
      }
  def normalizeAnotation(a: Annotation[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      term <- normalizeExpr(a.term, env, p)
      neTag = term.tag._2
      newNeTag = neTag.copy(children = (neTag.children + neTag.ne))
      tag = (a.tag, newNeTag)
    }  yield a.copy(term=term, tag=tag)

  def normalizeGeneric(g: Generic[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      in <- normalizeExpr(g.in, env, p)
      neTag = in.tag._2
      newNeTag = neTag.copy(children = neTag.children + neTag.ne)
      tag = (g.tag, newNeTag)
    } yield g.copy(in=in, tag=tag)

  def normalizeVar(v: Var[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] = env._1.get(v.name) match {
    case None => for {
      ne <- norm((p, Left((v.name, v.tag)), env))
      neTag = getTag(ne)._2
    } yield v.copy(tag=(v.tag, neTag))
    case Some(neTag) =>
      State.pure(v.copy(tag=(v.tag, neTag)))
  }

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        expr <- normalizeExpr(al.expr, env, p)
      }
        yield al.copy(expr=expr, tag=normalizeTag(al.tag))

  def normalizeApp(a: App[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        fn <- normalizeExpr(a.fn, env, p)
        arg <- normalizeExpr(a.arg, env, p)
      } yield a.copy(fn=fn, arg=arg, tag=normalizeTag(a.tag))

  def normalizeLet(l: Let[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        expr <- normalizeExpr(l.expr, env, p)
        in <- normalizeExpr(l.in, env, p)
      } yield l.copy(expr=expr, in=in, tag=normalizeTag(l.tag))

  def normalizeLiteral(l: Literal[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, String), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    State.pure(l.copy(tag=normalizeTag(l.tag)))

  def normalizeMatch(m: Match[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      arg <- normalizeExpr(m.arg, env, p)
      branches <- (m.branches.map { case (pat, expr) => normalizeExpr(expr, env, p).map((pat, _))}).sequence
    }
    yield Match(arg=arg,
      branches=branches,
      tag=normalizeTag(m.tag)
      )

  def normalizePackageLet(pkgName: PackageName, inferredExpr: (String, RecursionKind, TypedExpr[Declaration]), pack: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef], (String, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)])] = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    for {
      expr <- normalizeExpr(inferredExpr._3, (Map(), Nil), pack)
      _ <- State.modify { cache: Map[(PackageName, String), ResultingRef] => cache + ((pkgName, inferredExpr._1) -> Right(expr))}
    }
      yield (inferredExpr._1, inferredExpr._2, expr)
  }

  def normalizeProgram(pkgName: PackageName, pack: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  Program[TypeEnv[Variance], TypedExpr[(Declaration, Normalization.NormalExpressionTag)], Statement]] = {
    for { 
      lets <- pack.program.lets.map(normalizePackageLet(pkgName, _, pack)).sequence
    } yield pack.program.copy(
      lets  = lets
    )
  }

  def normalizePackage(pkgName: PackageName, pack: Package.Inferred): State[
  Map[(PackageName, String), ResultingRef],
  Package.Normalized] = for {
    program <- normalizeProgram(pkgName, pack)
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
            expr <- normalizeExpr(expr, env, pack)
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
              val neTag = NormalExpressionTag(NormalExpression.NormalNothing(), Set())
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
              val neTag = NormalExpressionTag(NormalExpression.ExternalVar(pn, n), Set())
              State.pure(Left((item, (t, neTag))))
            }
          }
      }
}

