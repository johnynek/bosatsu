package org.bykn.edgemar

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval

case class Evaluation(pm: PackageMap.Inferred) {
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
    branches: NonEmptyList[(ConstructorName, List[String], Expr[(Declaration, Scheme)])],
    p: Package.Inferred,
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => Eval[(Any, Scheme)]): Eval[Any] =

    arg match {
      case (enumId: Int, params: List[Any]) =>
        Eval.later {
          val dtName = Type.rootDeclared(scheme.result).get // this is safe because it has type checked
          // TODO this can be memoized once per package
          // TODO does this include imports?
          val dt = p.unfix.program.types.definedTypes
             .collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get // one must match
          val cname = dt.constructors.toList(enumId)._1
          val (_, paramVars, next) = branches.find { case (ctor, _, _) => ctor == cname }.get
          val localEnv = paramVars.zip(params).toMap

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
      case Ffi(lang, callsite, s@Scheme(_, t), _) =>
        Eval.later {
          val parts = callsite.split("\\.", -1).toList
          val clsName0 = parts.init.mkString(".")
          val clsName = lang match {
            case "java" => clsName0
            case "scala" => clsName0 + "$"
            case _ => sys.error(s"unknown lang: $lang")
          }
          val cls = Class.forName(clsName)
          val args = Evaluation.getJavaType(t).toArray
          val m = cls.getMethod(parts.last, args.init :_*)
          val inst = lang match {
            case "java" =>
              null
            case "scala" =>
              cls.getDeclaredField("MODULE$").get(null)
            case other => sys.error(s"unknown ffi language: $other") // TODO don't throw here
          }

          def invoke(tpe: Type, args: List[AnyRef]): Any =
            tpe match {
              case Type.Arrow(_, tail) =>
                new Fn[Any, Any] {
                  def apply(x: Any) = invoke(tail, (x.asInstanceOf[AnyRef]) :: args)
                }
              case _ =>
                m.invoke(inst, args.reverse.toArray: _*)
            }
          (invoke(t, Nil), s)
        }
      case Let(arg, e, in, (_, scheme)) =>
        recurse((p, Right(e), env)).flatMap { case (ae, _) =>
          recurse((p, Right(in), env + (arg -> ae)))
        }
        .map { case (r, _) => (r, scheme) }
      case Literal(Lit.Integer(i), (_, scheme)) => Eval.now((i, scheme))
      case Literal(Lit.Bool(b), (_, scheme)) => Eval.now((b, scheme))
      case If(arg, t, f, (_, scheme)) =>
        recurse((p, Right(arg), env)).flatMap { case (a, _) =>
          if (a.asInstanceOf[Boolean]) recurse((p, Right(t), env))
          else recurse((p, Right(f), env))
        }
        .map { case (a, _) => (a, scheme) }
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
              case Eql => ai == bi
            }
          }
        }
        .map((_, scheme))
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Inferred, Ref, Map[String, Any])) => Eval[(Any, Scheme)] =
    Memoize.function[(Package.Inferred, Ref, Map[String, Any]), Eval[(Any, Scheme)]] {
      case ((pack, Right(expr), env), recurse) =>
        evalExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        // This name is either a let, a constructor or it is an import
        val prog = pack.unfix.program
        prog.getLet(item) match {
          case Some(expr) => recurse((pack, Right(expr), env))
          case None =>
            // This could be an import, or constructor
            pack.unfix.localImport(item) match {
              case Some((originalPackage, i)) =>
                // import case, the original can't see the scope
                recurse((originalPackage, Left(i.originalName), Map.empty))
              case None =>
                // constructor case
                Eval.later {
                  val cn = ConstructorName(item)
                  val dtype = prog.types.constructors(cn)
                  val scheme = dtype.toScheme(cn).get // this should never throw
                  val fn = constructor(cn, dtype)
                  (fn, scheme)
                }
            }
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

  private def evalBranch[T](arg: Any,
    scheme: Scheme,
    branches: NonEmptyList[(ConstructorName, List[String], Expr[(T, Scheme)])],
    env: Map[String, Any],
    tpes: Map[ConstructorName, DefinedType]): Any =

    arg match {
      case (enumId: Int, params: List[Any]) =>
        val dtName = Type.rootDeclared(scheme.result).get // this is safe because it has type checked
        // TODO this can be memoized once per program
        val dt = tpes.collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get // one must match
        val cname = dt.constructors.toList(enumId)._1
        val (_, paramVars, next) = branches.find { case (ctor, _, _) => ctor == cname }.get
        val localEnv = paramVars.zip(params).toMap
        evaluateUnsafe(next, env ++ localEnv, tpes)

      case other => sys.error(s"logic error, in match arg evaluated to $other")
    }

  private def evaluateUnsafe[T](e: Expr[(T, Scheme)], env: Map[String, Any], tpes: Map[ConstructorName, DefinedType]): Any = {
    import Expr._

    e match {
      case Var(v, _) => env.getOrElse(v, constructor(ConstructorName(v), tpes(ConstructorName(v))))
      case App(Lambda(name, fn, _), arg, _) =>
        val env1 = env + (name -> evaluateUnsafe(arg, env, tpes))
        evaluateUnsafe(fn, env1, tpes)
      case App(fn, arg, _) =>
        evaluateUnsafe(fn, env, tpes).asInstanceOf[Fn[Any, Any]](evaluateUnsafe(arg, env, tpes))
      case Lambda(name, expr, _) =>
        new Fn[Any, Any] {
          def apply(x: Any) =
            evaluateUnsafe(expr, env + (name -> x), tpes)
        }
      case Ffi(lang, callsite, Scheme(_, t), _) =>
        val parts = callsite.split("\\.", -1).toList
        val clsName0 = parts.init.mkString(".")
        val clsName = lang match {
          case "java" => clsName0
          case "scala" => clsName0 + "$"
          case _ => sys.error(s"unknown lang: $lang")
        }
        val cls = Class.forName(clsName)
        val args = Evaluation.getJavaType(t).toArray
        val m = cls.getMethod(parts.last, args.init :_*)
        val inst = lang match {
          case "java" =>
            null
          case "scala" =>
            cls.getDeclaredField("MODULE$").get(null)
          case other => sys.error(s"unknown ffi language: $other") // TODO don't throw here
        }

        def invoke(tpe: Type, args: List[AnyRef]): Any =
          tpe match {
            case Type.Arrow(_, tail) =>
              new Fn[Any, Any] {
                def apply(x: Any) = invoke(tail, (x.asInstanceOf[AnyRef]) :: args)
              }
            case _ =>
              m.invoke(inst, args.reverse.toArray: _*)
          }
        invoke(t, Nil)
      case Let(arg, e, in, _) =>
        evaluateUnsafe(in, env + (arg -> evaluateUnsafe(e, env, tpes)), tpes)
      case Literal(Lit.Integer(i), _) => i
      case Literal(Lit.Bool(b), _) => b
      case If(arg, t, f, _) =>
        if (evaluateUnsafe(arg, env, tpes).asInstanceOf[Boolean]) evaluateUnsafe(t, env, tpes)
        else evaluateUnsafe(f, env, tpes)
      case Match(arg, branches, _) =>
        val earg = evaluateUnsafe(arg, env, tpes)
        evalBranch(earg, arg.tag._2, branches, env, tpes)
      case Op(a, op, b, _) =>
        val ai = evaluateUnsafe(a, env, tpes).asInstanceOf[Int]
        val bi = evaluateUnsafe(b, env, tpes).asInstanceOf[Int]
        import Operator._
        op match {
          case Plus => ai + bi
          case Mul => ai * bi
          case Sub => ai - bi
          case Eql => ai == bi
        }
    }
  }
}

object Evaluation {

  def getJavaType(t: Type): List[Class[_]] = {
    def loop(t: Type, top: Boolean): List[Class[_]] = {
      t match {
        case Type.Primitive("Int") => classOf[java.lang.Integer] :: Nil
        case Type.Primitive("Bool") => classOf[java.lang.Boolean] :: Nil
        case Type.Arrow(a, b) if top =>
          loop(a, false) match {
            case at :: Nil => at :: loop(b, top)
            case function => sys.error(s"unsupported function type $function in $t")
          }
        case _ => classOf[AnyRef] :: Nil
      }
    }
    loop(t, true)
  }

}
