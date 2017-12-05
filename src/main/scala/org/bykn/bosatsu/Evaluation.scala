package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  def evaluate(p: PackageName, varName: String): Option[(Eval[Any], Scheme)] =
    pm.toMap.get(p).map { pack =>
      eval((Package.asInferred(pack), Left(varName), Map.empty))
    }

  def evaluateLast(p: PackageName): Option[(Eval[Any], Scheme)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield eval((Package.asInferred(pack), Right(expr), Map.empty))

  private type Ref = Either[String, Expr[(Declaration, Scheme)]]

  private def evalBranch(arg: Any,
    scheme: Scheme,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[(Declaration, Scheme)])],
    p: Package.Inferred,
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], Scheme)): Eval[Any] =

    arg match {
      case (enumId: Int, params: List[Any]) =>
        Eval.later {
          val dtName = Type.rootDeclared(scheme.result).get // this is safe because it has type checked
          // TODO this can be memoized once per package
          val dt = p.unfix.program.types.definedTypes
             .collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get // one must match
          val cname = dt.constructors(enumId)._1
          val (Pattern(_, paramVars), next) = branches.find { case (Pattern((_, ctor), _), _) => ctor === cname }.get
          val localEnv = paramVars.zip(params).collect { case (Some(p1), p2) => (p1, p2) }.toMap

          recurse((p, Right(next), env ++ localEnv))._1
        }.flatMap { e => e }

      case other => sys.error(s"logic error, in match arg evaluated to $other")
    }

  private def evalExpr(p: Package.Inferred,
    expr: Expr[(Declaration, Scheme)],
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], Scheme)): (Eval[Any], Scheme) = {

    import Expr._

    expr match {
      case Var(v, (_, scheme)) =>
        env.get(v) match {
          case Some(a) => (Eval.now(a), scheme)
          case None => recurse((p, Left(v), env))
        }
      case App(Lambda(name, fn, _), arg, (_, scheme)) =>
        (recurse((p, Right(arg), env))._1.flatMap { a =>
          val env1 = env + (name -> a)
          recurse((p, Right(fn), env1))._1
        }, scheme)
      case App(fn, arg, (_, scheme)) =>
        val efn = recurse((p, Right(fn), env))._1
        val earg = recurse((p, Right(arg), env))._1
        (for {
          fn <- efn
          afn = fn.asInstanceOf[Fn[Any, Any]] // safe because we typecheck
          a <- earg
        } yield afn(a), scheme)
      case Lambda(name, expr, (_, scheme)) =>
        val fn = new Fn[Any, Any] {
          def apply(x: Any) =
            recurse((p, Right(expr), env + (name -> x)))._1.value
        }
        (Eval.now(fn), scheme)
      case Let(arg, e, in, (_, scheme)) =>
        (recurse((p, Right(e), env))._1.flatMap { ae =>
          recurse((p, Right(in), env + (arg -> ae)))._1
        }, scheme)
      case Literal(Lit.Integer(i), (_, scheme)) => (Eval.now(i), scheme)
      case Literal(Lit.Str(str), (_, scheme)) => (Eval.now(str), scheme)
      case Match(arg, branches, (_, scheme)) =>
        val (earg, sarg) = recurse((p, Right(arg), env))
        (earg.flatMap { a =>
          evalBranch(a, sarg, branches, p, env, recurse)
        }, scheme)
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], Scheme) =
    Memoize.function[(Package.Inferred, Ref, Map[String, Any]), (Eval[Any], Scheme)] {
      case ((pack, Right(expr), env), recurse) =>
        evalExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(cn, dt, schm) =>
            (Eval.later(constructor(cn, dt)), schm)
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case NameKind.ExternalDef(pn, n, scheme) =>
            (externals.toMap((pn, n)).call(scheme.result), scheme)
        }
    }

  private def constructor(c: ConstructorName, dt: DefinedType): Any = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params), idx) if ctor == c => (idx, params.size) }
      .get // the ctor must be in the list or we wouldn't typecheck

    // TODO: this is a obviously terrible
    // the encoding is inefficient, the implementation is inefficient
    def loop(param: Int, args: List[Any]): Any =
      if (param == 0) (enum, args.reverse)
      else new Fn[Any, Any] {
        def apply(a: Any) = loop(param - 1, a :: args)
      }

    loop(arity, Nil)
  }
}
