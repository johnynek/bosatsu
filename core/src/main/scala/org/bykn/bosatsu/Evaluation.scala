package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  def evaluate(p: PackageName, varName: String): Option[(Value, Scheme)] =
    pm.toMap.get(p).map { pack =>
      eval((Package.asInferred(pack), Left(varName), Map.empty))
    }

  def evaluateLast(p: PackageName): Option[(Value, Scheme)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield eval((Package.asInferred(pack), Right(expr), Map.empty))

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, scheme) =>

// enum Test:
//   TestAssert(value: Bool)
//   TestLabel(label: String, test: Test)
//   TestList(tests: List[Test])

      def toTest(a: Any): Test =
        a match {
          case (0, assertValue :: Nil) => Test.Assert(toBool(assertValue))
          case (1, (label: String) :: test :: Nil) => Test.Label(label, toTest(test))
          case (2, listOfTests :: Nil) => Test.TestList(toList(listOfTests))
          case other => sys.error(s"expected test value: $other")
        }

      def toBool(a: Any): Boolean =
        a match {
          case (0, Nil) => false
          case (1, Nil) => true
          case other => sys.error(s"expected Boolean: $other")
        }

      def toList(a: Any): List[Test] =
        a match {
          case (0, Nil) => Nil
          case (1, t :: rest :: Nil) => toTest(t) :: toList(rest)
          case other => sys.error(s"expected List: $other")
        }

      toType[Test](ea.value, scheme.result) { (any, dt, rec) =>
        if (dt.packageName == Predef.packageName) {
          dt.name.asString match {
            case "Test" =>
              // due to type checking, none of the above errors should hit
              Some(toTest(any))
            case _ =>
              None
          }
        }
        else None
      }
    }

  private type Ref = Either[String, Expr[(Declaration, Scheme)]]
  private type Value = Eval[Any]
  private type Env = Map[String, Any]

  private def evalBranch(arg: Any,
    scheme: Scheme,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[(Declaration, Scheme)])],
    p: Package.Inferred,
    env: Env,
    recurse: ((Package.Inferred, Ref, Env)) => (Value, Scheme)): Value =

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
    env: Env,
    recurse: ((Package.Inferred, Ref, Env)) => (Value, Scheme)): (Value, Scheme) = {

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
  private[this] val eval: ((Package.Inferred, Ref, Env)) => (Value, Scheme) =
    Memoize.function[(Package.Inferred, Ref, Env), (Value, Scheme)] {
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
            externals.toMap.get((pn, n)) match {
              case None =>
                throw EvaluationException(s"Missing External defintion of '${pn.parts.toList.mkString("/")} $n'. Check that your 'external' parameter is correct.")
              case Some(ext) => (ext.call(scheme.result), scheme)
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

  private def definedToJson(a: Any, dt: DefinedType, rec: (Any, Type) => Option[Json]): Option[Json] =
    if (dt.packageName == Predef.packageName) {
      dt.name.asString match {
        case "Option" =>
          a match {
            case (0, Nil) =>
              Some(Json.JNull)
            case (1, v :: Nil) =>
              dt.constructors match {
                case _ :: ((ConstructorName("Some"), (_, t) :: Nil)) :: Nil =>
                  rec(v, t)
                case other =>
                  sys.error(s"expect to find Some constructor for $v: $other")
              }
            case other => sys.error(s"some kind of type-error: $other for $dt")
          }
        case "String" =>
          Some(Json.JString(a.asInstanceOf[String]))
        case "Bool" =>
          a match {
            case (0, Nil) =>
              Some(Json.JBool(false))
            case (1, Nil) =>
              Some(Json.JBool(true))
            case other => sys.error(s"type error, expected boolean: $other")
          }
        case "Int" =>
          Some(Json.JNumberStr(a.asInstanceOf[java.lang.Integer].toString))
        case "List" =>
          // convert the list into a JArray
          val tpe = dt.constructors match {
            case _ :: ((ConstructorName("NonEmptyList"), (_, t) :: (_, _) :: Nil)) :: Nil => t
            case other => sys.error(s"unexpected constructors for list: $other")
          }

          @annotation.tailrec
          def toVec(a: Any, acc: List[Json]): Option[Vector[Json]] =
            a match {
              case (0, Nil) => Some(acc.reverse.toVector)
              case (1, head :: tail :: Nil) =>
                rec(head, tpe) match {
                  case None => None
                  case Some(h) =>
                    toVec(tail, h :: acc)
                }
              case other => sys.error(s"ill-typed list: $other, List[$tpe]")
            }

          toVec(a, Nil).map(Json.JArray(_))
        case other =>
          sys.error(s"unknown predef type: $other")
      }
    }
    else  {
      a match {
        case (variant: Int, parts: List[Any]) =>
          val cons = dt.constructors
          cons.lift(variant).flatMap { case (_, params) =>
            parts.zip(params).traverse { case (a1, (ParamName(pn), t)) =>
              rec(a1, t).map((pn, _))
            }
            .map { ps => Json.JObject(ps.toMap) }
          }
        case _ =>
          // Should never happen
          None
      }
    }

  def toJson(a: Any, schm: Scheme): Option[Json] =
    toType[Json](a, schm.result)(definedToJson(_, _, _))

  def toType[T](a: Any, t: Type)(fn: (Any, DefinedType, (Any, Type) => Option[T]) => Option[T]): Option[T] = {
    def defined(pn: PackageName, t: TypeName): Option[DefinedType] =
      for {
        pack <- pm.toMap.get(pn)
        dts = pack.program.types.definedTypes
        dt <- dts.get((pn, t))
      } yield dt

    def applyDT(dt: DefinedType, arg: Type): DefinedType =
      dt.typeParams match {
        case Type.Var(h) :: rest =>
          val subst = Subst(Map(h -> arg))
          val dt0 = dt.copy(typeParams = rest)
          Substitutable[DefinedType].apply(subst, dt0)
        case _ => sys.error(s"ill-typed no typeparams: $dt, $arg")
      }

    def applyT(t: Type, arg: Type): Either[Type, DefinedType] =
      t match {
        case Type.Arrow(_, _) => sys.error(s"ill-typed: $t[$arg]")
        case Type.TypeApply(t0, a0) =>
          applyT(t0, a0) match {
            case Right(dt) =>
              Right(applyDT(dt, arg))
            case Left(t) =>
              Left(Type.TypeApply(t, arg))
          }
        case Type.Declared(pn, typeName) =>
          val dt = defined(pn, TypeName(typeName)).getOrElse(sys.error(s"ill-typed: unknown $t"))
          Right(applyDT(dt, arg))
        case v@Type.Var(_) =>
          Left(Type.TypeApply(v, arg))
      }

    def loop(a: Any, t: Type): Option[T] = {
      t match {
        case Type.Arrow(_, _) =>
          // We can't convert a function to Json
          None
        case Type.Declared(pn, typeName) =>
          defined(pn, TypeName(typeName))
            .flatMap(fn(a, _, toType[T](_, _)(fn)))
        case Type.TypeApply(tpe, arg) =>
          applyT(tpe, arg) match {
            case Right(dt) =>
              fn(a, dt, toType[T](_, _)(fn))
            case Left(t) =>
              sys.error(s"expected a defined type. Found: $t")
          }
        case Type.Var(_) =>
          // we should have fully resolved the type
          sys.error(s"should have fully resolved the type of: $a: $t")
      }
    }
    loop(a, t)
  }
}

case class EvaluationException(message: String) extends Exception(message)
