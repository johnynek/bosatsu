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

  def normalizeLast(p: PackageName): Option[(Expr[Scheme], Scheme)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield {
      norm((Package.asInferred(pack), Right(expr.traverse[Id, Scheme](_._2)), Map.empty))
    }
  
  private type Ref = Either[String, Expr[Scheme]]

  private def normExpr(p: Package.Inferred,
    expr: Expr[Scheme],
    env: Map[String, Expr[Scheme]],
    recurse: ((Package.Inferred, Ref, Map[String, Expr[Scheme]])) => (Expr[Scheme], Scheme)): (Expr[Scheme], Scheme) = {

    import Expr._

    expr match {
      case Var(v, scheme) =>
        env.get(v) match {
          case Some(a) => (a, scheme)
          case None => recurse((p, Left(v), env))
        }
      case App(Lambda(name, fn, _), arg, scheme) => {
        val earg = recurse((p, Right(arg), env))._1
        recurse((p, Right(fn), env ++ Map(name -> earg)))
      }
      case App(fn, arg, scheme) => {
        val efn = recurse((p, Right(fn), env))._1
        val earg = recurse((p, Right(arg), env))._1
        efn match {
          case lam @ Lambda(_, _, _) => recurse((p, Right(App(lam, earg, scheme)), env))
          case _ => (App(efn, earg, scheme), scheme)
        }
      }
      case lam @ Lambda(name, expr, scheme) => (lam, scheme)
      case Let(arg, e, in, scheme) => {
        val ee = recurse((p, Right(e), env))._1
        recurse((p, Right(in), env ++ Map(arg -> ee)))
      }
      case lit @ Literal(_, scheme) => (lit, scheme)
      case Match(arg, branches, scheme) => ???
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val norm: ((Package.Inferred, Ref, Map[String, Expr[Scheme]])) => (Expr[Scheme], Scheme) =
    Memoize.function[(Package.Inferred, Ref, Map[String, Expr[Scheme]]), (Expr[Scheme], Scheme)] {
      case ((pack, Right(expr), env), recurse) =>
        normExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr.traverse[Id, Scheme](_._2)), env))
          case NameKind.Constructor(cn, dt, schm) => ???
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case NameKind.ExternalDef(pn, n, scheme) => (Expr.Var(item, scheme), scheme)
        }
    }
}
