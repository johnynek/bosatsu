package org.bykn.bosatsu

import cats.Eq
import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

import org.bykn.bosatsu.rankn.{Type => NType}

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  def evaluate(p: PackageName, varName: String): Option[(Eval[Any], NType)] =
    pm.toMap.get(p).map { pack =>
      eval((Package.asInferred(pack), Left(varName), Map.empty))
    }

  def evaluateLast(p: PackageName): Option[(Eval[Any], NType)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
    } yield eval((Package.asInferred(pack), Right(expr), Map.empty))

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, tpe) =>

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

      toType[Test](ea.value, tpe) { (any, dt, rec) =>
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

  private type Ref = Either[String, TypedExpr[Declaration]]

  private def evalBranch(arg: Any,
    tpe: NType,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), rankn.Type], TypedExpr[Declaration])],
    p: Package.Inferred,
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], NType)): Eval[Any] =

    Eval.defer {
      val dtConst@rankn.Type.TyConst(rankn.Type.Const.Defined(pn0, tn)) =
        rankn.Type.rootConst(tpe).get // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)
      // this is calling apply on a map, but is safe because of type-checking
      val dt = packageForType.program.types.definedTypes((pn0, TypeName(tn)))

      def bindEnv(arg: Any,
        branches: List[(Pattern[(PackageName, ConstructorName), rankn.Type], TypedExpr[Declaration])],
        acc: Map[String, Any]): Option[(Map[String, Any], TypedExpr[Declaration])] =
        branches match {
          case Nil => None
          case (Pattern.WildCard, next):: tail => Some((acc, next))
          case (Pattern.Var(n), next) :: tail => Some((acc + (n -> arg), next))
          case (Pattern.Annotation(p, _), next) :: tail =>
            // TODO we may need to use the type here
            bindEnv(arg, (p, next) :: tail, acc)
          case (Pattern.PositionalStruct((pack, ctor), items), next) :: tail =>
            // let's see if this matches
            arg match {
              case (enumId: Int, params: List[Any]) =>
                val cname = dt.constructors(enumId)._1
                if (cname == ctor) {
                  // this is the pattern
                  // note passing in a List here we could have union patterns
                  // if all the pattern bindings were known to be of the same type
                  params.zip(items).foldM(acc) { case (e, (arg, pat)) =>
                    bindEnv(arg, List((pat, next)), e).map(_._1)
                  }.map((_, next))
                }
                else {
                  bindEnv(arg, tail, acc)
                }
              case other => sys.error(s"logic error, in match arg evaluated to $other")
            }
        }



      val (localEnv, next) = bindEnv(arg, branches.toList, env)
        .getOrElse(
            // TODO make sure we rule this out statically
            sys.error(s"non-total match: arg: $arg, branches: ${branches.map(_._1)}")
          )
      recurse((p, Right(next), localEnv))._1
    }
    .memoize

  /**
   * TODO, expr is a TypedExpr so we already know the type. returning it does not do any good that I
   * can see.
   */
  private def evalTypedExpr(p: Package.Inferred,
    expr: TypedExpr[Declaration],
    env: Map[String, Any],
    recurse: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], NType)): (Eval[Any], NType) = {

    import TypedExpr._

     expr match {
       case Generic(_, _, _) => ???
       case Annotation(e, _, _) => evalTypedExpr(p, e, env, recurse)
       case Var(v, tpe, _) =>
         env.get(v) match {
           case Some(a) => (Eval.now(a), tpe)
           case None => recurse((p, Left(v), env))
         }
       case App(AnnotatedLambda(name, argt, fn, _), arg, resT, _) =>
         (recurse((p, Right(arg), env))._1.flatMap { a =>
           val env1 = env + (name -> a)
           recurse((p, Right(fn), env1))._1
         }, resT)
       case App(fn, arg, resT, _) =>
         val efn = recurse((p, Right(fn), env))._1
         val earg = recurse((p, Right(arg), env))._1
         (for {
           fn <- efn
           afn = fn.asInstanceOf[Fn[Any, Any]] // safe because we typecheck
           a <- earg
         } yield afn(a), resT)
       case AnnotatedLambda(name, argt, expr, _) =>
         val fn = new Fn[Any, Any] {
           def apply(x: Any) =
             recurse((p, Right(expr), env + (name -> x)))._1.value
         }
         (Eval.now(fn), rankn.Type.Fun(argt, expr.getType))
       case Let(arg, e, in, _) =>
         (recurse((p, Right(e), env))._1.flatMap { ae =>
           recurse((p, Right(in), env + (arg -> ae)))._1
         }, in.getType)
       case Literal(Lit.Integer(i), tpe, _) => (Eval.now(i), tpe)
       case Literal(Lit.Str(str), tpe, _) => (Eval.now(str), tpe)
       case If(cond, ifT, ifF, _) =>
         // TODO
         // evaluate the condition the either the left or right
         ???
       case Match(arg, branches, _) =>
         val (earg, sarg) = recurse((p, Right(arg), env))
         (earg.flatMap { a =>
           evalBranch(a, sarg, branches, p, env, recurse)
        }, expr.getType)
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Inferred, Ref, Map[String, Any])) => (Eval[Any], NType) =
    Memoize.function[(Package.Inferred, Ref, Map[String, Any]), (Eval[Any], NType)] {
      case ((pack, Right(expr), env), recurse) =>
        evalTypedExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(cn, _, dt, tpe) =>
            (Eval.later(constructor(cn, dt)), tpe)
          case NameKind.Import(from, orig) =>
            // we reset the environment in the other package
            recurse((from, Left(orig), Map.empty))
          case NameKind.ExternalDef(pn, n, tpe) =>
            externals.toMap.get((pn, n)) match {
              case None =>
                throw EvaluationException(s"Missing External defintion of '${pn.parts.toList.mkString("/")} $n'. Check that your 'external' parameter is correct.")
              case Some(ext) => (ext.call(tpe), tpe)
            }
        }
    }

  private def constructor(c: ConstructorName, dt: rankn.DefinedType): Any = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params, resType), idx) if ctor == c => (idx, params.size) }
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

  private def definedToJson(a: Any, dt: rankn.DefinedType, rec: (Any, NType) => Option[Json]): Option[Json] =
    if (dt.packageName == Predef.packageName) {
      dt.name.asString match {
        case "Option" =>
          a match {
            case (0, Nil) =>
              Some(Json.JNull)
            case (1, v :: Nil) =>
              dt.constructors match {
                case _ :: ((ConstructorName("Some"), (_, t) :: Nil, _)) :: Nil =>
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
            case _ :: ((ConstructorName("NonEmptyList"), (_, t) :: (_, _) :: Nil, _)) :: Nil => t
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
          cons.lift(variant).flatMap { case (_, params, _) =>
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

  def toJson(a: Any, tpe: NType): Option[Json] =
    toType[Json](a, tpe)(definedToJson(_, _, _))

  def toType[T](a: Any, t: NType)(fn: (Any, rankn.DefinedType, (Any, NType) => Option[T]) => Option[T]): Option[T] = {
    def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType] =
      for {
        pack <- pm.toMap.get(pn)
        dts = pack.program.types.definedTypes
        dt <- dts.get((pn, t))
      } yield dt

    def applyDT(dt: rankn.DefinedType, arg: Type): rankn.DefinedType =
      dt.typeParams match {
        case NType.Var.Bound(h) :: rest =>
          // val subst = Subst(Map(h -> arg))
          // val dt0 = dt.copy(typeParams = rest)
          // Substitutable[rankn.DefinedType].apply(subst, dt0)
          ???
        case _ => sys.error(s"ill-typed no typeparams: $dt, $arg")
      }

    def applyT(t: rankn.Type, arg: rankn.Type): Either[rankn.Type, rankn.DefinedType] =
      //t match {
        // case Type.Arrow(_, _) => sys.error(s"ill-typed: $t[$arg]")
        // case Type.TypeApply(t0, a0) =>
        //   applyT(t0, a0) match {
        //     case Right(dt) =>
        //       Right(applyDT(dt, arg))
        //     case Left(t) =>
        //       Left(Type.TypeApply(t, arg))
        //   }
        // case Type.Declared(pn, typeName) =>
        //   val dt = defined(pn, TypeName(typeName)).getOrElse(sys.error(s"ill-typed: unknown $t"))
        //   Right(applyDT(dt, arg))
        // case v@Type.Var(_) =>
        //   Left(Type.TypeApply(v, arg))
        // case Type.TypeLambda(param, expr) =>
        //   // the param == arg in the expr
        //   sys.error(s"TODO: let $param = $arg in $expr")
      //}
      ???

    def loop(a: Any, t: NType): Option[T] = {
      t match {
        case rankn.Type.Fun(_, _) =>
          // We can't convert a function to Json
          None
        case rankn.Type.TyConst(rankn.Type.Const.Defined(pn, typeName)) =>
          defined(pn, TypeName(typeName))
            .flatMap(fn(a, _, toType[T](_, _)(fn)))
        case rankn.Type.TyApply(tpe, arg) =>
          applyT(tpe, arg) match {
            case Right(dt) =>
              fn(a, dt, toType[T](_, _)(fn))
            case Left(t) =>
              sys.error(s"expected a defined type. Found: $t")
          }
        case rankn.Type.TyVar(_) | rankn.Type.TyMeta(_) =>
          // we should have fully resolved the type
          sys.error(s"should have fully resolved the type of: $a: $t")
        case rankn.Type.ForAll(_, _) =>
          sys.error(s"unexpected type universally quantified: $a has type $t")
      }
    }
    loop(a, t)
  }
}

case class EvaluationException(message: String) extends Exception(message)
