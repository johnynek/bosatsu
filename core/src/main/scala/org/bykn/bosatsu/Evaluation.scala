package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._

import org.bykn.bosatsu.rankn.Type

object Evaluation {
  import Value._


  /**
   * If we later determine that this performance matters
   * and this wrapping is hurting, we could replace
   * Value with a less structured type and put
   * all the reflection into unapply calls but keep
   * most of the API
   */
  sealed abstract class Value {
    def asFn: FnValue =
      this match {
        case f@FnValue(_) => f
        case other => sys.error(s"invalid cast to Fn: $other")
      }
  }

  object Value {
    sealed abstract class ProductValue extends Value {
      def toList: List[Value] =
        this match {
          case UnitValue => Nil
          case ConsValue(head, tail) => head :: tail.toList
        }
    }

    object ProductValue {
      def fromList(ps: List[Value]): ProductValue =
        ps match {
          case Nil => UnitValue
          case h :: tail => ConsValue(h, fromList(tail))
        }
    }

    case object UnitValue extends ProductValue
    case class ConsValue(head: Value, tail: ProductValue) extends ProductValue
    case class SumValue(variant: Int, value: ProductValue) extends Value
    case class FnValue(toFn: Value => Eval[Value]) extends Value {
      def apply(a: Value): Eval[Value] = toFn(a)
    }
    case class ExternalValue(toAny: Any) extends Value

    val False: Value = SumValue(0, UnitValue)
    val True: Value = SumValue(1, UnitValue)

    object VInt {
      def apply(v: Int): Value = ExternalValue(java.lang.Integer.valueOf(v))
      def unapply(v: Value): Option[Int] =
        v match {
          case ExternalValue(v: java.lang.Integer) => Some(v.intValue)
          case _ => None
        }
    }

    object Str {
      def apply(str: String): Value = ExternalValue(str)
      def unapply(v: Value): Option[String] =
        v match {
          case ExternalValue(str: String) => Some(str)
          case _ => None
        }
    }

    object VOption {
      def unapply(v: Value): Option[Option[Value]] =
        v match {
          case UnitValue =>
            Some(None)
          case ConsValue(head, UnitValue) =>
            Some(Some(head))
          case _ => None
        }
    }

    object VList {
      def unapply(v: Value): Option[List[Value]] =
        v match {
          case SumValue(0, UnitValue) => Some(Nil)
          case SumValue(1, ConsValue(head, ConsValue(rest, UnitValue))) =>
            unapply(rest).map(head :: _)
          case _ => None
        }
    }
  }
}

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  import Evaluation.Value
  import Value._

  def evaluate(p: PackageName, varName: String): Option[(Eval[Value], Type)] =
    pm.toMap.get(p).map { pack =>
      eval((Package.asInferred(pack), Left(varName), Map.empty))
    }

  def evaluateLast(p: PackageName): Option[(Eval[Value], Type)] =
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
//
      def toTest(a: Value): Test =
        a match {
          case SumValue(0, ConsValue(assertValue, _)) =>
            Test.Assert(toBool(assertValue))
          case SumValue(1, ConsValue(Str(label), ConsValue(test, _))) =>
            Test.Label(label, toTest(test))
          case SumValue(2, ConsValue(listOfTests, _)) =>
            Test.TestList(toList(listOfTests))
          case other => sys.error(s"expected test value: $other")
        }

      def toBool(a: Value): Boolean =
        a match {
          case False => false
          case True => true
          case other => sys.error(s"expected Boolean: $other")
        }

      def toList(a: Value): List[Test] =
        a match {
          case VList(ls) => ls.map(toTest)
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

  private def evalBranch(arg: Value,
    tpe: Type,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), Type], TypedExpr[Declaration])],
    p: Package.Inferred,
    env: Map[String, Value],
    recurse: ((Package.Inferred, Ref, Map[String, Value])) => (Eval[Value], Type)): Eval[Value] =

    Eval.defer {
      val dtConst@Type.TyConst(Type.Const.Defined(pn0, tn)) =
        Type.rootConst(tpe).get // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)
      // this is calling apply on a map, but is safe because of type-checking
      val dt = packageForType.program.types.definedTypes((pn0, TypeName(tn)))

      def bindEnv(arg: Value,
        branches: List[(Pattern[(PackageName, ConstructorName), Type], TypedExpr[Declaration])],
        acc: Map[String, Value]): Option[(Map[String, Value], TypedExpr[Declaration])] =
        branches match {
          case Nil => None
          case (Pattern.WildCard, next):: tail => Some((acc, next))
          case (Pattern.Var(n), next) :: tail => Some((acc + (n -> arg), next))
          case (Pattern.Annotation(p, _), next) :: tail =>
            // TODO we may need to use the type here
            bindEnv(arg, (p, next) :: tail, acc)
          case (Pattern.PositionalStruct((pack, ctor), items), next) :: tail =>
            // let's see if this matches
            val optParams =
              if (dt.constructors.lengthCompare(1) == 0) {
                // this is a struct, which means we expect it
                arg match {
                  case p: ProductValue =>
                    Some(p.toList)
                  case other => sys.error(s"ill typed in match: $other")
                }
              }
              else {
                arg match {
                  case SumValue(enumId, v) =>
                    val cname = dt.constructors(enumId)._1
                    if (cname == ctor) {
                      Some(v.toList)
                    }
                    else None
                  case other => sys.error(s"ill typed in match: $other")
                }
              }

            optParams match {
              case Some(params) =>
                  // this is the pattern
                  // note passing in a List here we could have union patterns
                  // if all the pattern bindings were known to be of the same type
                  params.zip(items).foldM(acc) { case (e, (arg, pat)) =>
                    bindEnv(arg, List((pat, next)), e).map(_._1)
                  }.map((_, next))
              case None =>
                  // we didn't match, go to the next branch
                  bindEnv(arg, tail, acc)
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
    env: Map[String, Value],
    recurse: ((Package.Inferred, Ref, Map[String, Value])) => (Eval[Value], Type)): (Eval[Value], Type) = {

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
           afn = fn.asInstanceOf[FnValue] // safe because we typecheck
           a <- earg
           res <- afn.toFn(a)
         } yield res, resT)
       case a@AnnotatedLambda(name, argt, expr, _) =>
         val fn =
           FnValue { x =>
             recurse((p, Right(expr), env + (name -> x)))._1
           }
         (Eval.now(fn), Type.Fun(argt, expr.getType))
       case Let(arg, e, in, _) =>
         (recurse((p, Right(e), env))._1.flatMap { ae =>
           recurse((p, Right(in), env + (arg -> ae)))._1
         }, in.getType)
       case Literal(Lit.Integer(i), tpe, _) => (Eval.now(VInt(i)), tpe)
       case Literal(Lit.Str(str), tpe, _) => (Eval.now(Str(str)), tpe)
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
  private[this] val eval: ((Package.Inferred, Ref, Map[String, Value])) => (Eval[Value], Type) =
    Memoize.function[(Package.Inferred, Ref, Map[String, Value]), (Eval[Value], Type)] {
      case ((pack, Right(expr), env), recurse) =>
        evalTypedExpr(pack, expr, env, recurse)
      case ((pack, Left(item), env), recurse) =>
        NameKind(pack, item).get match { // this get should never fail due to type checking
          case NameKind.Let(expr) =>
            recurse((pack, Right(expr), env))
          case NameKind.Constructor(cn, _, dt, tpe) =>
            (constructor(cn, dt), tpe)
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

  private def constructor(c: ConstructorName, dt: rankn.DefinedType): Eval[Value] = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params, resType), idx) if ctor == c => (idx, params.size) }
      .get // the ctor must be in the list or we wouldn't typecheck

    val singleItemStruct = dt.constructors.lengthCompare(1) == 0

    // TODO: this is a obviously terrible
    // the encoding is inefficient, the implementation is inefficient
    def loop(param: Int, args: List[Value]): Value =
      if (param == 0) {
        val prod = ProductValue.fromList(args.reverse)
        if (singleItemStruct) prod
        else SumValue(enum, prod)
      }
      else FnValue { a =>
        Eval.always(loop(param - 1, a :: args))
      }

    Eval.later(loop(arity, Nil))
  }

  private def definedToJson(a: Value, dt: rankn.DefinedType, rec: (Value, Type) => Option[Json]): Option[Json] = if (dt.packageName == Predef.packageName) {
      (dt.name.asString, a) match {
        case ("Option", VOption(None)) => Some(Json.JNull)
        case ("Option", VOption(Some(v))) =>
          dt.constructors match {
            case _ :: ((ConstructorName("Some"), (_, t) :: Nil, _)) :: Nil =>
              rec(v, t)
            case other =>
              sys.error(s"expect to find Some constructor for $v: $other")
          }
        case ("String", Str(s)) =>
          Some(Json.JString(s))
        case ("Bool", True) =>
          Some(Json.JBool(true))
        case ("Bool", False) =>
          Some(Json.JBool(false))
        case ("Int", VInt(v)) =>
          Some(Json.JNumberStr(v.toString))
        case ("List", VList(vs)) =>
          // convert the list into a JArray
          val tpe = dt.constructors match {
            case _ :: ((ConstructorName("NonEmptyList"), (_, t) :: (_, _) :: Nil, _)) :: Nil => t
            case other => sys.error(s"unexpected constructors for list: $other")
          }

          vs.toVector
            .traverse { v => rec(v, tpe) }
            .map(Json.JArray(_))
        case (tname, other) =>
          sys.error(s"unknown predef type: $tname: $other")
      }
    }
    else  {
      val vp =
        a match {
          case SumValue(variant, p) => Some((variant, p))
          case p: ProductValue => Some((0, p))
          case _ => None
        }

      vp.flatMap { case (variant, prod) =>
        val cons = dt.constructors
        cons.lift(variant).flatMap { case (_, params, _) =>
          prod.toList.zip(params).traverse { case (a1, (ParamName(pn), t)) =>
            rec(a1, t).map((pn, _))
          }
          .map { ps => Json.JObject(ps.toMap) }
        }
      }
    }

  def toJson(a: Value, tpe: Type): Option[Json] =
    toType[Json](a, tpe)(definedToJson(_, _, _))

  def toType[T](a: Value, t: Type)(fn: (Value, rankn.DefinedType, (Value, Type) => Option[T]) => Option[T]): Option[T] = {
    def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType] =
      for {
        pack <- pm.toMap.get(pn)
        dts = pack.program.types.definedTypes
        dt <- dts.get((pn, t))
      } yield dt

    /**
     * TODO we are ignoring any applied types here which we will need to set.
     * the correct way is to accumulate a type environment of Map[Var.Bound, Type]
     * and pass that down, but currently no tests expose this issue so letting
     * it lurk for now
     */
    Type.rootConst(t).flatMap {
      case Type.TyConst(Type.Const.Defined(pn, n)) =>
        defined(pn, TypeName(n))
    }
    .flatMap(fn(a, _, toType[T](_, _)(fn)))
  }
}

case class EvaluationException(message: String) extends Exception(message)
