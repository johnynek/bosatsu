package org.bykn.edgemar

import cats.data.NonEmptyList

object Evaluation {
  import Expr._

  def evaluate[T](e: Expr[T]): Either[TypeError, (Any, Scheme)] =
    Inference.inferExpr(e).map { scheme =>
      // if we type check, we can always evaluate
      (evaluateUnsafe(scheme, Map.empty, Map.empty), scheme.tag._2)
    }

  def evaluateProgram[S](p: Program[Declaration, S]): Option[Either[TypeError, (Any, Scheme)]] =
    p.getMainDecl.map { case expr =>
      Inference.inferExpr(p.types, expr).map { escheme =>
        // if we type check, we can always evaluate
        (evaluateUnsafe(escheme, Map.empty, p.types.constructors), escheme.tag._2)
      }
    }

  private def getJavaType(t: Type): List[Class[_]] = {
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

  private def evaluateUnsafe[T](e: Expr[(T, Scheme)], env: Map[String, Any], tpes: Map[ConstructorName, DefinedType]): Any =
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
        val args = getJavaType(t).toArray
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
