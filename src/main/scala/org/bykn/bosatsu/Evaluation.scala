package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  def evaluate(p: PackageName, varName: String): Option[Eval[(Any, Scheme)]] =
    pm.toMap.get(p).map { pack =>
      eval((Package.asInferred(pack), Left(varName), Map.empty))
    }

  def evaluateLast(p: PackageName): Option[Eval[(Any, Scheme)]] =
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
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => Eval[(Any, Scheme)]): Eval[Any] =

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

          recurse((p, Right(next), env ++ localEnv)).map(_._1)
        }.flatMap { e => e }

      case other => sys.error(s"logic error, in match arg evaluated to $other")
    }

  private def evalExpr(p: Package.Inferred,
    expr: Expr[(Declaration, Scheme)],
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => Eval[(Any, Scheme)]): Eval[(Any, Scheme)] = {

    import Expr._

    expr match {
      case Var(v, (_, scheme)) =>
        env.get(v) match {
          case Some(a) => Eval.now((a, scheme))
          case None => recurse((p, Left(v), env))
        }
      case App(Lambda(name, fn, _), arg, (_, scheme)) =>
        recurse((p, Right(arg), env)).flatMap { case (a, _) =>
          val env1 = env + (name -> a)
          recurse((p, Right(fn), env1))
        }
        .map((_, scheme))
      case App(fn, arg, (_, scheme)) =>
        val efn = recurse((p, Right(fn), env))
        val earg = recurse((p, Right(arg), env))
        efn.flatMap { case (fn, _) =>
          val afn = fn.asInstanceOf[Fn[Any, Any]] // safe because we typecheck
          earg.map { case (aarg, _) => (afn(aarg), scheme) }
        }
      case Lambda(name, expr, (_, scheme)) =>
        val fn = new Fn[Any, Any] {
          def apply(x: Any) =
            recurse((p, Right(expr), env + (name -> x))).value._1
        }
        Eval.now((fn, scheme))
      case Let(arg, e, in, (_, scheme)) =>
        recurse((p, Right(e), env)).flatMap { case (ae, _) =>
          recurse((p, Right(in), env + (arg -> ae)))
        }
        .map { case (r, _) => (r, scheme) }
      case Literal(Lit.Integer(i), (_, scheme)) => Eval.now((i, scheme))
      case Literal(Lit.Bool(b), (_, scheme)) => Eval.now((b, scheme))
      case Literal(Lit.Str(str), (_, scheme)) => Eval.now((str, scheme))
      case Match(arg, branches, (_, scheme)) =>
        recurse((p, Right(arg), env)).flatMap { case (a, s) =>
          evalBranch(a, s, branches, p, env, recurse)
        }.map((_, scheme))

      case Op(a, op, b, (_, scheme)) =>
        val ea = recurse((p, Right(a), env))
        val eb = recurse((p, Right(b), env))

        ea.flatMap { case (a, _) =>
          eb.map { case (b, _) =>
            import Operator._
            val ai = a.asInstanceOf[Int]
            val bi = b.asInstanceOf[Int]
            op match {
              case Plus => ai + bi
              case Mul => ai * bi
              case Sub => ai - bi
              case Eql =>
                //
                if (ai == bi) True else False
            }
          }
        }
        .map((_, scheme))
    }
  }

  private[this] val True = (1, Nil)
  private[this] val False = (0, Nil)

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Inferred, Ref, Map[String, Any])) => Eval[(Any, Scheme)] =
    Memoize.function[(Package.Inferred, Ref, Map[String, Any]), Eval[(Any, Scheme)]] {
      case ((pack, Right(expr), env), recurse) =>
        evalExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(fnEval) =>
            fnEval
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case NameKind.ExternalDef(pn, n, scheme) =>
            externals.toMap((pn, n))
              .call(scheme.result)
              .map((_, scheme))
        }
    }

  private sealed abstract class NameKind
  private object NameKind {
    case class Let(value: Expr[(Declaration, Scheme)]) extends NameKind
    case class Constructor(fn: Eval[(Any, Scheme)]) extends NameKind
    case class Import(fromPack: Package.Inferred, originalName: String) extends NameKind
    case class ExternalDef(pack: PackageName, defName: String, defType: Scheme) extends NameKind

    def apply(from: Package.Inferred, item: String): Option[NameKind] = {
      val prog = from.unfix.program

      def getLet: Option[NameKind] =
        prog.getLet(item).map(Let(_))

      def getConstructor: Option[NameKind] = {
        val cn = ConstructorName(item)
        prog.types.constructors.get((from.unfix.name, cn)).map { dtype =>
          Constructor(Eval.later {
            val scheme = dtype.toScheme(cn).get // this should never throw
            val fn = constructor(cn, dtype)
            (fn, scheme)
          })
        }
      }

      def getImport: Option[NameKind] =
        from.unfix.localImport(item).map { case (originalPackage, i) =>
          Import(originalPackage, i.originalName)
        }

      def getExternal: Option[NameKind] =
        // there should not be duplicate top level names TODO lint for this
        prog.from.toStream.collectFirst {
          case Statement.ExternalDef(n, _, _, _) if n == item =>
            // The type could be an import, so we need to check for the type
            // in the TypeEnv
            val scheme = prog.types.values(n)
            val pn = from.unfix.name
            ExternalDef(pn, item, scheme)
        }

      getLet.orElse(
        getConstructor.orElse(getImport.orElse(getExternal))
      )
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
