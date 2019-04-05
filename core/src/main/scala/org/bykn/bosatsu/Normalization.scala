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
  case class Struct(enum: Int, args: List[NormalExpression]) extends NormalExpression {
    val maxLambdaVar = args.flatMap(_.maxLambdaVar).reduceLeftOption(Math.max)
  }
  case class Literal(lit: Lit) extends NormalExpression {
    val maxLambdaVar = None
  }
  case class Recursion(lambda: NormalExpression) extends NormalExpression {
    val maxLambdaVar = lambda.maxLambdaVar
  }
}

object Normalization {
  case class NormalExpressionTag(ne: NormalExpression, children: Set[NormalExpression])
  type NormalizedPM = PackageMap.Typed[(Declaration, Normalization.NormalExpressionTag)]
  type NormalizedPac = Package.Typed[(Declaration, Normalization.NormalExpressionTag)]

  private def normalOrderReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
    val nextExpr = expr match {
      case App(Lambda(nextExpr), arg) => {
        applyLambdaSubstituion(nextExpr, Some(arg), 0)
      }
      case App(fn, arg) => {
        val nextFn = normalOrderReduction(fn)
        App(nextFn, arg)
      }
      /*case Match(struct@Struct(enum, args), branches) => { TODO: If s matches a pattern we should perform lambdaSubstitution
        Match(struct, branches)
      }*/
      case Recursion(Lambda(innerExpr)) if(innerExpr.maxLambdaVar.map(_ < 0).getOrElse(false)) => {
        applyLambdaSubstituion(innerExpr, None, 0)
      }
      case Lambda(App(innerExpr, LambdaVar(0))) => {
        innerExpr
      }
      case _ => expr
    }
    nextExpr match {
      case al @ App(Lambda(_), _) => normalOrderReduction(al)
      case App(fn, arg) =>
        normalOrderReduction(fn) match {
          case l @ Lambda(_) => normalOrderReduction(App(l, arg))
          case nfn @ _       => App(nfn, normalOrderReduction(arg))
        }
      case extVar @ ExternalVar(_, _) => extVar
      case Match(arg, branches) =>
        normalOrderReduction(arg) match {
          // case s @ Struct(_, _) => normalOrderReduction(Match(s, branches)) TODO: Check if we know enough about s to validate it matches a pattern. If so, recurse.
          case ns @ _ =>
            Match(ns, branches.map {
              case (enum, expr) => (enum, normalOrderReduction(expr))
            })
        }
      case lv @ LambdaVar(_)  => lv
      case Lambda(expr)       => Lambda(normalOrderReduction(expr))
      case Struct(enum, args) => Struct(enum, args.map(normalOrderReduction(_)))
      case l @ Literal(_)     => l
      case r@Recursion(Lambda(innerExpr)) if(innerExpr.maxLambdaVar.map(_ < 0).getOrElse(false))
                              => normalOrderReduction(r)
      case r@Recursion(_)     => r
    }
  }
  private def applyLambdaSubstituion(expr: NormalExpression,
    subst: Option[NormalExpression],
    idx: Int): NormalExpression = {
      import NormalExpression._
      expr match {
        case App(fn, arg) =>
          App(applyLambdaSubstituion(fn, subst, idx),
            applyLambdaSubstituion(arg, subst, idx))
        case ext @ ExternalVar(_, _) => ext
        case Match(arg, branches) =>
          Match(applyLambdaSubstituion(arg, subst, idx), branches.map {
            case (enum, expr) => (enum, applyLambdaSubstituion(expr, subst, idx))
          })
        case LambdaVar(varIndex) if varIndex == idx => subst.get
        case LambdaVar(varIndex) if varIndex > idx  => LambdaVar(varIndex - 1)
        case lv @ LambdaVar(_)                      => lv
        case Lambda(fn)                             => Lambda(applyLambdaSubstituion(fn, subst.map(incrementLambdaVars(_, 0)), idx + 1))
        case Struct(enum, args) =>
          Struct(enum, args.map(applyLambdaSubstituion(_, subst, idx)))
        case l @ Literal(_) => l
      }
  }

  private def incrementLambdaVars(expr: NormalExpression, lambdaDepth: Int): NormalExpression = {
    import NormalExpression._
    expr match {
      case App(fn, arg) =>
        App(incrementLambdaVars(fn, lambdaDepth),
          incrementLambdaVars(arg, lambdaDepth))
      case ext @ ExternalVar(_, _) => ext
      case Match(arg, branches) =>
        Match(incrementLambdaVars(arg, lambdaDepth), branches.map {
          case (enum, expr) => (enum, incrementLambdaVars(expr, lambdaDepth))
        })
      case LambdaVar(varIndex) if varIndex >= lambdaDepth => LambdaVar(varIndex + 1)
      case lv @ LambdaVar(_)                      => lv
      case Lambda(fn)                             => incrementLambdaVars(fn, lambdaDepth + 1)
      case Struct(enum, args) =>
        Struct(enum, args.map(incrementLambdaVars(_, lambdaDepth)))
      case l @ Literal(_) => l
      case Recursion(fn) => Recursion(incrementLambdaVars(fn, lambdaDepth))
    }
  }
}

case class NormalizePackageMap(pm: PackageMap.Inferred) {
  import Normalization._
  import TypedExpr._

  val normalizePackageMap: NormalizedPM = {
    val packs = pm.toMap.toList
    val normAll = packs.traverse { case (name, pack) =>
      normalizePackage(name, pack)
        .map((name, _))
    }
    PackageMap(normAll.run(Map()).value._2.toMap)
  }

  def normalizeExpr(expr: TypedExpr[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] = {
      expr match {
        case a@Annotation(_, _, _) => normalizeAnnotation(a, env, p)
        case g@Generic(_, _, _) => normalizeGeneric(g, env, p)
        case v@Var(_, _, _, _) => normalizeVar(v, env, p)
        case al@AnnotatedLambda(_, _, _, _) => normalizeAnnotatedLambda(al, env, p)
        case a@App(_, _, _, _) => normalizeApp(a, env, p)
        case l@Let(_, _, _, _, _) => normalizeLet(l, env, p)
        case l@Literal(_, _, _) => normalizeLiteral(l, env, p)
        case m@Match(_, _, _) => normalizeMatch(m, env, p)
      }
    }

  private def combineWithChildren(nt: NormalExpressionTag) = nt.children + nt.ne

  def normalizeAnnotation(a: Annotation[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      normalizeExpr(a.term, env, p).map { term =>
        val neTag = term.tag._2
        val tag = (a.tag, neTag)
        a.copy(term=term, tag=tag)
      }

  def normalizeGeneric(g: Generic[Declaration], env: Env, p: Package.Inferred): 
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      normalizeExpr(g.in, env, p).map { in =>
        val neTag = in.tag._2
        val tag = (g.tag, neTag)
        g.copy(in=in, tag=tag)
      }

  def normalizeVar(v: Var[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      env._1.get(v.name) match {
        case None => norm((p, Left((v.name, v.tag)), env)).map {ne =>
          val neTag = getTag(ne)._2
          v.copy(tag=(v.tag, neTag))
        }
        case Some(neTag) =>
          State.pure(v.copy(tag=(v.tag, neTag)))
      }

  def normalizeAnnotatedLambda(al: AnnotatedLambda[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] = {
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

  def normalizeApp(a: App[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      for {
        efn <- normalizeExpr(a.fn, env, p)
        earg <- normalizeExpr(a.arg, env, p)
        ne = NormalExpression.App(efn.tag._2.ne, earg.tag._2.ne)
        children = combineWithChildren(efn.tag._2) ++ combineWithChildren(earg.tag._2)
        neTag = NormalExpressionTag(ne, children)
      } yield a.copy(fn=efn, arg=earg, tag=(a.tag, neTag))

  def normalizeLet(l: Let[Declaration], env: Env, p: Package.Inferred):
    NormState[TypedExpr[(Declaration, NormalExpressionTag)]] =
      l.recursive match {
        case RecursionKind.Recursive => {
          val lambdaVars = Some(l.arg) :: env._2
          val nextEnv = (env._1 ++ lambdaVars.zipWithIndex
            .collect { case (Some(n), i) => (n, i) }
            .toMap
            .mapValues(idx => NormalExpressionTag(NormalExpression.LambdaVar(idx), Set[NormalExpression]())),
            lambdaVars)
          val neWrapper = {ne: NormalExpression => NormalExpression.Recursion(NormalExpression.Lambda(ne))}
          (nextEnv, neWrapper)
          val originalLambda = AnnotatedLambda(arg=l.arg, tpe=l.expr.getType, expr=l.in, tag=l.tag)
          for {
            ee <- normalizeExpr(l.expr, nextEnv, p)
            nextNextEnv: Env = (nextEnv._1 + (l.arg -> ee.tag._2), nextEnv._2)
            eIn <- normalizeExpr(l.in, nextEnv, p)
            ne = neWrapper(eIn.tag._2.ne)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, NormalExpressionTag(ne, eIn.tag._2.children)))
        }
        case _ =>
          for {
            ee <- normalizeExpr(l.expr, env, p)
            nextEnv: Env = (env._1 + (l.arg -> ee.tag._2), env._2)
            eIn <- normalizeExpr(l.in, nextEnv, p)
          } yield Let(l.arg, ee, eIn, l.recursive, (l.tag, eIn.tag._2))
      }

  def normalizeLiteral(l: Literal[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, NormalExpressionTag)]] =
      State.pure(l.copy(tag=(l.tag, NormalExpressionTag(NormalExpression.Literal(l.lit), Set()))))

  def normalizeMatch(m: Match[Declaration], env: Env, p: Package.Inferred): NormState[
    TypedExpr[(Declaration, NormalExpressionTag)]] = for {
      arg <- normalizeExpr(m.arg, env, p)
      branches <- (m.branches.map { case branch => normalizeBranch(branch, env, p)}).sequence
      normalBranches = branches.map { case (p, e) => (p, e.tag._2.ne)}
      neTag = NormalExpressionTag(ne=NormalExpression.Match(arg.tag._2.ne, normalBranches), children=Set())
    } yield Match(arg=arg,
      branches=branches,
      tag=(m.tag, neTag))

  def normalizeBranch(b: (Pattern[(PackageName, Constructor), Type], TypedExpr[Declaration]), env: Env, p: Package.Inferred): NormState[
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

  def normalizePackageLet(pkgName: PackageName, inferredExpr: (Identifier.Bindable, RecursionKind, TypedExpr[Declaration]), pack: Package.Inferred): 
    NormState[(Identifier.Bindable, RecursionKind, TypedExpr[(Declaration, Normalization.NormalExpressionTag)])] =
    for {
      expr <- normalizeExpr(inferredExpr._3, (Map(), Nil), pack)
      _ <- State.modify { cache: Map[(PackageName, Identifier), ResultingRef] => cache + ((pkgName, inferredExpr._1) -> Right(expr))}
    }
    yield (inferredExpr._1, inferredExpr._2, expr)

  def normalizeProgram(pkgName: PackageName, pack: Package.Inferred): NormState[
    Program[TypeEnv[Variance], TypedExpr[(Declaration, Normalization.NormalExpressionTag)], Statement]] = {
    for { 
      lets <- pack.program.lets.map(normalizePackageLet(pkgName, _, pack)).sequence
    } yield pack.program.copy(
      lets  = lets
    )
  }

  def normalizePackage(pkgName: PackageName, pack: Package.Inferred):
    NormState[NormalizedPac] = for {
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
  private type NormState[A] = State[Map[(PackageName, Identifier), ResultingRef], A]

  private def norm(input: (Package.Inferred, SourceRef, Env)): NormState[ResultingRef] =
    input match {
      case ((pack, Right(expr), env)) =>
        for {
          expr <- normalizeExpr(expr, env, pack)
        } yield Right(expr)
      case (pack, Left((item, t)), env) => {
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

