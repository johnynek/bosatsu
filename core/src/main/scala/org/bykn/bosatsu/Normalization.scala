package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import cats.implicits._
import rankn._

import Identifier.Constructor

sealed abstract class NormalExpression {
  /*
   * maxLambdaVar is to keep track of what is the largest de bruijn index
   * of lambda variables in the expression. This is useful because if this number
   * is positive then there are unbound variables and it should not be cached (unless
   * you want to be clever about cacheing values for when they are bound in an outer
   * scope). And when they are negative it implies there are eta reduction opportunities.
   * None essentially means -Infinity as either there are no linked expressions
   * or there are no lambda variables used in the linked expressions
   */
  def maxLambdaVar: Option[Int]
}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression)
  extends NormalExpression {
    val maxLambdaVar = (fn.maxLambdaVar.toList ++ arg.maxLambdaVar.toList)
      .reduceLeftOption(Math.max)
  }
  case class ExternalVar(pack: PackageName, defName: Identifier)
  extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Match(arg: NormalExpression,
    branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], NormalExpression)])
  extends NormalExpression {
    val maxLambdaVar =
      (arg.maxLambdaVar.toList ++ branches.toList.flatMap(_._2.maxLambdaVar))
        .reduceLeftOption(Math.max)
  }
  case class LambdaVar(index: Int) extends NormalExpression {
    val maxLambdaVar = Some(index)
  }
  /*
   * It is reasonable to ask how you can define a lambda without an identifier
   * for its argument in its expression. This is a benefit of de bruijn indexing.
   * When all lambdas have exactly one argument you identify the var by how many
   * lambdas out you have to travel.
   *
   * eg \x -> \y -> [y,x] would have normalization Lambda(Lambda(Apply(Apply(List, LambdaVar(1)), LambdaVar(0))))
   *
   * ref: https://en.wikipedia.org/wiki/De_Bruijn_index
   */
  case class Lambda(expr: NormalExpression) extends NormalExpression {
    val maxLambdaVar = expr.maxLambdaVar.map(_ - 1)
  }
  case class Struct(enum: Int, args: List[NormalExpression])
  extends NormalExpression {
    val maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Recursion(lambda: NormalExpression) {
    val maxLambdaVar = lambda.maxLambdaVar
  }
}

object Normalization {
  case class NormalExpressionTag(ne: NormalExpression, children: Set[NormalExpression])
}

case class NormalizePackageMap(pm: PackageMap.Inferred) {
  import Normalization._
  import TypedExpr._

  val normalizePackageMap: PackageMap.Normalized = 
    PackageMap(
      pm.toMap.toList.map { case (name, pkg) => {
        normalizePackage(name, pkg).map((name, _))
    }}.sequence.run(Map()).value._2.toMap)

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, Identifier), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      expr match {
        case a@Annotation(_, _, _) => normalizeAnotation(a, env, p)
        case g@Generic(_, _, _) => normalizeGeneric(g, env, p)
        case v@Var(_, _, _, _) => normalizeVar(v, env, p)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al, env, p).map(_.asInstanceOf[TypedExpr[(Declaration, NormalExpressionTag)]])
        case a@App(_, _, _, _) => normalizeApp(a, env, p).map(_.asInstanceOf[TypedExpr[(Declaration, NormalExpressionTag)]])
        case l@Let(_, _, _, _, _) => normalizeLet(l, env, p)
        case l@Literal(_, _, _) => normalizeLiteral(l, env, p)
        case m@Match(_, _, _) => normalizeMatch(m, env, p)
      }

  private def combineWithChildren(nt: NormalExpressionTag) = nt.children + nt.ne

  def normalizeAnotation(a: Annotation[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      term <- normalizeExpr(a.term, env, p)
      neTag = term.tag._2
      newNeTag = neTag.copy(children = (neTag.children + neTag.ne))
      tag = (a.tag, newNeTag)
    }  yield a.copy(term=term, tag=tag)

  def normalizeGeneric(g: Generic[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] =
    for {
      in <- normalizeExpr(g.in, env, p)
      neTag = in.tag._2
      newNeTag = neTag.copy(children = neTag.children + neTag.ne)
      tag = (g.tag, newNeTag)
    } yield g.copy(in=in, tag=tag)

  def normalizeVar(v: Var[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] = env._1.get(v.name) match {
    case None => for {
      ne <- norm((p, Left((v.name, v.tag)), env))
      neTag = getTag(ne)._2
    } yield v.copy(tag=(v.tag, neTag))
    case Some(neTag) =>
      State.pure(v.copy(tag=(v.tag, neTag)))
  }

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, Identifier), ResultingRef],
    AnnotatedLambda[(Declaration, NormalExpressionTag)]] = {
      val lambdaVars = Some(al.arg) :: env._2
      val nextEnv: Env = (env._1 ++ lambdaVars.zipWithIndex
        .collect { case (Some(n), i) => (n, i) }
        .toMap
        .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
        lambdaVars)
      for {
        eExpr <- normalizeExpr(al.expr, nextEnv, p)
        ne = NormalExpression.Lambda(eExpr.tag._2.ne)
        children = combineWithChildren(eExpr.tag._2)
        neTag = NormalExpressionTag(ne, children)
      } yield al.copy(expr=eExpr, tag=(al.tag, neTag))
    }

  def normalizeApp(a: App[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  App[(Declaration, NormalExpressionTag)]] =
    for {
      efn <- normalizeExpr(a.fn, env, p)
      earg <- normalizeExpr(a.arg, env, p)
      ne = NormalExpression.App(efn.tag._2.ne, earg.tag._2.ne)
      children = combineWithChildren(efn.tag._2) ++ combineWithChildren(earg.tag._2)
      neTag = NormalExpressionTag(ne, children)
    } yield a.copy(fn=efn, arg=earg, tag=(a.tag, neTag))

  def normalizeLet(l: Let[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] = {
    val (nextEnv, neWrapper) = l.recursive match {
    case RecursionKind.Recursive => {
      val lambdaVars = Some(l.arg) :: env._2
      val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
        .collect { case (Some(n), i) => (n, i) }
        .toMap
        .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
        lambdaVars)
      val neWrapper = {ne: NormalExpression => NormalExpression.Recursion(NormalExpression.Lambda(ne)).asInstanceOf[NormalExpression]}
      (nextEnv, neWrapper)
    }
    case _ => (env, {ne: NormalExpression => ne})
    }
      val originalLambda = AnnotatedLambda(arg=l.arg, tpe=l.expr.getType, expr=l.in, tag=l.tag)
      for {
        lambda <- normalizeAnnotatedLambda(originalLambda, env, p)
        app <- normalizeApp(App(fn=originalLambda, arg=l.expr, result=l.in.getType, tag=l.tag), env, p)
        neTag = app.tag._2.copy(ne= neWrapper(app.tag._2.ne))
      } yield l.copy(expr=app.arg, in=lambda.expr, tag=(l.tag, neTag))
    }

  def normalizeLiteral(l: Literal[Declaration], env: Env, p: Package.Inferred): State[
    Map[(PackageName, Identifier), ResultingRef],
    TypedExpr[(Declaration, NormalExpressionTag)]] =
    State.pure(l.copy(tag=(l.tag, NormalExpressionTag(NormalExpression.Literal(l.lit), Set()))))

  def normalizeMatch(m: Match[Declaration], env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  TypedExpr[(Declaration, NormalExpressionTag)]] = {
    for {
      arg <- normalizeExpr(m.arg, env, p)
      branches <- (m.branches.map { case branch => normalizeBranch(branch, env, p)}).sequence
      normalBranches = branches.map { case (p, e) => (p, e.tag._2.ne)}
      neTag = NormalExpressionTag(ne=NormalExpression.Match(arg.tag._2.ne, normalBranches), children=Set())
    }
    yield Match(arg=arg,
      branches=branches,
      tag=(m.tag, neTag)
      )
  }

  def normalizeBranch(b: (Pattern[(PackageName, Constructor), Type], TypedExpr[Declaration]), env: Env, p: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  (Pattern[(PackageName, Constructor), Type], TypedExpr[(Declaration, NormalExpressionTag)])] = {
    val (pattern, expr) = b
    val names = pattern.names.collect { case b: Identifier.Bindable => b}.map(Some(_))
    val lambdaVars = names ++ env._2
    val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
      .collect { case (Some(n), i) => (n, i) }
      .toMap
      .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
      lambdaVars)
    for {
      innerExpr <- normalizeExpr(expr, nextEnv, p)
      normalExpr = names.foldLeft(innerExpr.tag._2.ne) { case (expr, _) => NormalExpression.Lambda(expr) }
      finalExpression = innerExpr.updatedTag((innerExpr.tag._1, innerExpr.tag._2.copy(ne=normalExpr)))
    } yield (pattern, finalExpression)

  }

  def normalizePackageLet(pkgName: PackageName, inferredExpr: (Identifier.Bindable, RecursionKind, TypedExpr[Declaration]), pack: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef], (Identifier.Bindable, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)])] = {
    println(inferredExpr)

    println(s"let ${inferredExpr._1} = ${inferredExpr._3}")
    for {
      expr <- normalizeExpr(inferredExpr._3, (Map(), Nil), pack)
      _ <- State.modify { cache: Map[(PackageName, Identifier), ResultingRef] => cache + ((pkgName, inferredExpr._1) -> Right(expr))}
    }
      yield (inferredExpr._1, inferredExpr._2, expr)
  }

  def normalizeProgram(pkgName: PackageName, pack: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  Program[TypeEnv[Variance], TypedExpr[(Declaration, Normalization.NormalExpressionTag)], Statement]] = {
    for { 
      lets <- pack.program.lets.map(normalizePackageLet(pkgName, _, pack)).sequence
    } yield pack.program.copy(
      lets  = lets
    )
  }

  def normalizePackage(pkgName: PackageName, pack: Package.Inferred): State[
  Map[(PackageName, Identifier), ResultingRef],
  Package.Normalized] = for {
    program <- normalizeProgram(pkgName, pack)
  } yield pack.copy(program = program)

  def getTag(ref: ResultingRef) = ref match {
    case Right(te) => te.tag
    case Left((_, t)) => t
  }

  private type Ref[T] =
    Either[(Identifier, T), TypedExpr[T]]

  private type SourceRef = Ref[Declaration]
  private type ResultingRef = Ref[(Declaration,  NormalExpressionTag)]
  private type Env = (Map[Identifier, NormalExpressionTag], List[Option[Identifier]])
  private def norm(input: (Package.Inferred, SourceRef, Env)): State[
  Map[(PackageName, Identifier), ResultingRef],
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
              lets: Map[(PackageName, Identifier), ResultingRef] =>
                lets.get((pack.name, item))
            }
            res <- lookup match {
              case Some(res) =>
                State.pure(res): State[
                Map[(PackageName, Identifier), ResultingRef],
                ResultingRef]
              case None =>
                for {
                  res <- norm((pack, Right(expr), env))
                  _ <- State.modify {
                    lets: Map[(PackageName, Identifier), ResultingRef] =>
                      lets + ((pack.name, item) -> res)
                  }
                } yield res

            }
          } yield Left((item, getTag(res)))
              case NameKind.Constructor(cn, _, dt, _) => {
                val neTag: NormalExpressionTag = NormalExpressionTag(constructor(cn, dt), Set())
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

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): NormalExpression = {
      val (enum, arity) = dt.constructors
        .toList
        .iterator
        .zipWithIndex
        .collectFirst { case ((ctor, params, resType), idx) if ctor == c => (idx, params.size) }
        .get

      def loop(params: Int, expr: NormalExpression): NormalExpression =
        if (params == 0) expr
        else loop(params - 1, NormalExpression.Lambda(expr))

        loop(arity, NormalExpression.Struct(enum, ((arity - 1) to 0 by -1).map(NormalExpression.LambdaVar(_)).toList))
  }
}

