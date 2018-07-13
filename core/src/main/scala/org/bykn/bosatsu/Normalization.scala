package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Id
import cats.implicits._

sealed abstract class NormalExpression {

}

object NormalExpression {
  case class App(fn: NormalExpression, arg: NormalExpression) extends NormalExpression
  case class ExternalVar(pack: PackageName, defName: String) extends NormalExpression
  case class Match(arg: NormalExpression, branches: NonEmptyList[(Pattern[Int], NormalExpression)]) extends NormalExpression
  case class LambdaVar(index: Int) extends NormalExpression
  case class Lambda(expr: NormalExpression) extends NormalExpression
  case class Struct(enum: Int, args: List[NormalExpression]) extends NormalExpression
  case class Literal(lit: Lit) extends NormalExpression
}

case class Normalization(pm: PackageMap.Inferred) {

  def normalizeLast(p: PackageName): Option[NormalExpression] =
    (for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield {
      norm((Package.asInferred(pack), Right(expr.traverse[Id, Scheme](_._2)), (Map.empty, Nil)))
    }).map(normalOrderReduction(_))
  
  private type Ref = Either[String, Expr[Scheme]]
  private type Env = (Map[String, NormalExpression], List[String])

  private def normExpr(p: Package.Inferred,
    expr: Expr[Scheme],
    env: Env,
    recurse: ((Package.Inferred, Ref, Env)) => NormalExpression): NormalExpression = {

    import NormalExpression._

    expr match {
      case Expr.Var(v, scheme) =>
        env._1.get(v) match {
          case Some(a) => a
          case None => recurse((p, Left(v), env))
        }
      case Expr.App(fn, arg, scheme) => {
        val efn = recurse((p, Right(fn), env))
        val earg = recurse((p, Right(arg), env))
        App(efn, earg)
      }
      case Expr.Lambda(name, expr, scheme) => {
        val lambdaVars = name :: env._2
        val nextEnv = (env._1 ++ lambdaVars.zipWithIndex.toMap.mapValues(LambdaVar(_)), lambdaVars)
        Lambda(recurse((p, Right(expr), nextEnv)))
      }
      case Expr.Let(arg, e, in, scheme) => {
        val ee = recurse((p, Right(e), env))
        val nextEnv = (env._1 ++ Map(arg -> ee), env._2)
        recurse((p, Right(in), nextEnv))
      }
      case Expr.Literal(lit, scheme) => Literal(lit)
      case Expr.Match(arg, branches, scheme) => {
        val nArg = recurse((p, Right(arg), env))
        val scheme = expr.tag
        val dtName = Type.rootDeclared(scheme.result).get
        val dt = p.unfix.program.types.definedTypes
          .collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get
        val enumLookup = dt.constructors.map(_._1.asString).zipWithIndex.toMap

        val nBranches = branches.map { case(pattern, e) => 
          (
            pattern.map { case(_, ConstructorName(cname)) => enumLookup(cname)},
            recurse((p, Right(e), env))
          )
        }
        Match(nArg, nBranches)
      }
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val norm: ((Package.Inferred, Ref, Env)) => NormalExpression =
    Memoize.function[(Package.Inferred, Ref, Env), NormalExpression] {
      case ((pack, Right(expr), env), recurse) =>
        normExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr.traverse[Id, Scheme](_._2)), env))
          case NameKind.Constructor(cn, dt, schm) => constructor(cn, dt)
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), (Map.empty, Nil)))
          case NameKind.ExternalDef(pn, n, scheme) => NormalExpression.ExternalVar(pn, n)
        }
    }

  private def constructor(c: ConstructorName, dt: DefinedType): NormalExpression = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params), idx) if ctor == c => (idx, params.size) }
      .get // the ctor must be in the list or we wouldn't typecheck


    import NormalExpression._

    def loop(params: Int, expr: NormalExpression): NormalExpression =
      if (params == 0) expr
      else loop(params - 1, Lambda(expr))

    loop(arity, Struct(enum, (arity to 0 by -1).map(LambdaVar(_)).toList))
  }

  private def normalOrderReduction(expr: NormalExpression): NormalExpression = {
    import NormalExpression._
    val nextExpr = expr match {
      case App(Lambda(nextExpr), arg) => {
        applyLambdaSubstituion(nextExpr, arg, 0)
      }
      case App(fn, arg) => {
        val nextFn = normalOrderReduction(fn)
        App(nextFn, arg)
      }
      case _ => expr
    }
    nextExpr match {
      case al @ App(Lambda(_), _) => normalOrderReduction(al)
      case App(fn, arg) => App(fn, normalOrderReduction(arg))
      case extVar @ ExternalVar(_, _) => extVar
      case Match(_, _) => ???
      case lv @ LambdaVar(_) => lv
      case Lambda(expr) => Lambda(normalOrderReduction(expr))
      case Struct(enum, args) => Struct(enum, args.map(normalOrderReduction(_)))
      case l @ Literal(_) => l
    }
  }

  private def applyLambdaSubstituion(expr: NormalExpression, subst: NormalExpression, idx: Int): NormalExpression = {
    import NormalExpression._
    expr match {
      case App(fn, arg) => App(applyLambdaSubstituion(fn, subst, idx), applyLambdaSubstituion(arg, subst, idx))
      case ext @ ExternalVar(_, _) => ext
      case Match(arg, branches) => ???
      case LambdaVar(varIndex) if varIndex == idx => subst
      case lv @ LambdaVar(_) => lv
      case Lambda(fn) => Lambda(applyLambdaSubstituion(fn, subst, idx + 1))
      case Struct(enum, args) => Struct(enum, args.map(applyLambdaSubstituion(_, subst, idx)))
      case l @ Literal(_) => l
    }
  }
}
