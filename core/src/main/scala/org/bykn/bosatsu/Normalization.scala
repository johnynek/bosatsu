package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

case class Normalization(pm: PackageMap.Inferred) {

  def normalizeLast(p: PackageName): Option[(NormalizedResult, Scheme)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield norm((Package.asInferred(pack), Right(expr), Map.empty))
  
  private type Ref = Either[String, Expr[(Declaration, Scheme)]]
  private type NormalizedResult = Either[NameKind, Expr[(Declaration, Scheme)]]

  private def normExpr(p: Package.Inferred,
    expr: Expr[(Declaration, Scheme)],
    env: Map[String, Expr[(Declaration, Scheme)]],
    recurse: ((Package.Inferred, Ref, Map[String, Expr[(Declaration, Scheme)]])) => (NormalizedResult, Scheme)): (NormalizedResult, Scheme) = {

    import Expr._

    expr match {
      case Var(v, (_, scheme)) =>
        env.get(v) match {
          case Some(a) => (Right(a), scheme)
          case None => ???
        }
      case App(Lambda(name, fn, _), arg, (_, scheme)) => ???
      case App(fn, arg, (_, scheme)) => ???
      case Lambda(name, expr, (_, scheme)) => ???
      case Let(arg, e, in, (_, scheme)) => ???
      case lit @ Literal(_, (_, scheme)) => (Right(lit), scheme)
      case Match(arg, branches, (_, scheme)) => ???
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val norm: ((Package.Inferred, Ref, Map[String, Expr[(Declaration, Scheme)]])) => (NormalizedResult, Scheme) =
    Memoize.function[(Package.Inferred, Ref, Map[String, Expr[(Declaration, Scheme)]]), (NormalizedResult, Scheme)] {
      case ((pack, Right(expr), env), recurse) =>
        normExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(cn, dt, schm) => ???
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case ext @ NameKind.ExternalDef(pn, n, scheme) => (Left(ext), scheme)
        }
    }
}
