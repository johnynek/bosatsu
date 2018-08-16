package org.bykn.bosatsu

import cats.data.{NonEmptyList, State}
import com.stripe.dagon.Memoize
import cats.{Eval, Id}
import cats.implicits._

object EvalCache {
  def apply(
    expr: Expr[(Declaration, Scheme, NormalExpression)],
    eval: State[Map[NormalExpression, (Eval[Any], Scheme)], (Eval[Any], Scheme)]
  ): State[Map[NormalExpression, (Eval[Any], Scheme)], (Eval[Any], Scheme)] = {
    val key = expr.tag._3
    for{
      cacheHit <- State.inspect[Map[NormalExpression, (Eval[Any], Scheme)], Option[(Eval[Any], Scheme)]](_.get(key))
      result <- cacheHit match {
        case Some((v,s)) => State.pure[Map[NormalExpression, (Eval[Any], Scheme)], (Eval[Any], Scheme)]((v, s))
        case None => {
          for {
            es <- eval
            ee = es._1.memoize
            s = es._2
            _ <- State.modify[Map[NormalExpression, (Eval[Any], Scheme)]](_ + (key -> ((ee, s))))
          } yield (ee,s)
        }
      }
    } yield result
  }
}

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {

  private lazy val npm: PackageMap.Normalized = Normalization(pm).normalizePackageMap

  def evaluate(p: PackageName, varName: String): Option[(Value, Scheme)] =
    npm.toMap.get(p).map { pack =>
      eval((Package.asFixed(pack), Left(varName), Map.empty)).run(Map()).value._2
    }

  def evaluateLast(p: PackageName, evalCache: Map[NormalExpression, (Value, Scheme)]): Option[(Value, Scheme, NormalExpression, Map[NormalExpression, (Value, Scheme)])] =
    for {
      pack <- npm.toMap.get(p)
      (_, exprWithNe) <- pack.program.lets.lastOption
    } yield {
      val a = eval
      val (cache, (value, scheme)) = eval((Package.asFixed(pack), Right(exprWithNe), Map.empty)).run(evalCache).value
      (value, scheme, exprWithNe.tag._3, cache)
    }

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps, Map()).flatMap { case (ea, scheme, _, _) =>

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

  private type Ref = Either[String, Expr[(Declaration, Scheme, NormalExpression)]]
  private type Value = Eval[Any]
  private type Env = Map[String, Any]
  private type StateEvalCache[T] = State[Map[NormalExpression, (Value, Scheme)], T]

  private def evalBranch(arg: Any,
    scheme: Scheme,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName)], Expr[(Declaration, Scheme, NormalExpression)])],
    p: Package.Normalized,
    env: Env,
    recurse: ((Package.Normalized, Ref, Env)) => StateEvalCache[(Value, Scheme)]): Value =
      arg match {
        case (enumId: Int, params: List[Any]) =>
          val dtName =Type.rootDeclared(scheme.result).get // this is safe because it has type checked
          // TODO this can be memoized once per package
          val dt = p.unfix.program.types.definedTypes
            .collectFirst { case (_, dtValue) if dtValue.name.asString == dtName.name => dtValue }.get // one must match
          val cname = dt.constructors(enumId)._1
          val (Pattern(_, paramVars), next) = branches.find { case (Pattern((_, ctor), _), _) => ctor === cname }.get
          val localEnv = paramVars.zip(params).collect { case (Some(p1), p2) => (p1, p2) }.toMap
          recurse((p, Right(next), env ++ localEnv)).run(Map()).value._2._1
        case other => sys.error(s"logic error, in match arg evaluated to $other")
      }

  private def evalExpr(p: Package.Normalized,
    expr: Expr[(Declaration, Scheme, NormalExpression)],
    env: Env,
    recurse: ((Package.Normalized, Ref, Env)) => StateEvalCache[(Value, Scheme)]): StateEvalCache[(Value, Scheme)] = {

    import Expr._

    expr match {
      case Var(v, (_, scheme, ne)) =>
        env.get(v) match {
          case Some(a) => State.pure((Eval.now(a), scheme))
          case None => EvalCache(expr, recurse((p, Left(v), env)))
        }
      case App(Lambda(name, fn, _), arg, (_, scheme, ne)) =>
        EvalCache(expr,
          recurse((p, Right(arg), env)).flatMap { case (arg,_) =>
            val env1 = env + (name -> arg.value)
            recurse((p, Right(fn), env1)).map { case(v,_) => (v, scheme) }
          })
      case App(fn, arg, (_, scheme, ne)) =>
        EvalCache(expr, for {
            efn <- recurse((p, Right(fn), env)).map(_._1)
            earg <- recurse((p, Right(arg), env)).map(_._1)
          } yield (for {
            fn <- efn
            afn = fn.asInstanceOf[Fn[Any, Any]] // safe because we typecheck
            a <- earg
          } yield afn(a), scheme)
        )
      case Lambda(name, expr, (_, scheme, ne)) =>
        State.inspect { c =>
          val fn = new Fn[Any, Any] {
            def apply(x: Any) =
              // We think it is safe to use the cache here but not update it
              // because we don't have a normal expression for the argument
              recurse((p, Right(expr), env + (name -> x))).run(c).value._2._1.value
          }
          (Eval.now(fn), scheme)
        }
      case Let(arg, e, in, (_, scheme, ne)) =>
        recurse((p, Right(e), env)).flatMap { case (ae, _) =>
          recurse((p, Right(in), env + (arg -> ae.value)))
            .map(vs => (vs._1, scheme))
        }
      case Literal(Lit.Integer(i), (_, scheme, ne)) => State.pure((Eval.now(i), scheme))
      case Literal(Lit.Str(str), (_, scheme, ne)) => State.pure((Eval.now(str), scheme))
      case Match(arg, branches, (_, scheme, ne)) =>
        EvalCache(expr, for {
          es <- recurse((p, Right(arg), env))
          } yield (es._1.flatMap { a =>
              evalBranch(a, es._2, branches, p, env, recurse)
          }, scheme)
        )
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Normalized, Ref, Env)) => StateEvalCache[(Value, Scheme)] =
    Memoize.function[(Package.Normalized, Ref, Env), StateEvalCache[(Value, Scheme)]] {
      case ((pack, Right(expr), env), recurse) =>
        evalExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(cn, dt, schm) =>
            State.pure((Eval.later(constructor(cn, dt)), schm))
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case NameKind.ExternalDef(pn, n, scheme) =>
            externals.toMap.get((pn, n)) match {
              case None =>
                throw EvaluationException(s"Missing External defintion of '${pn.parts.toList.mkString("/")} $n'. Check that your 'external' parameter is correct.")
              case Some(ext) => State.pure((ext.call(scheme.result), scheme))
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
