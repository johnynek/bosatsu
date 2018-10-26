package org.bykn.bosatsu

import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import cats.Eval
import cats.implicits._
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}

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
    def asFn: Value => Eval[Value] =
      this match {
        case FnValue(f) => f
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
    case class ConsValue(head: Value, tail: ProductValue) extends ProductValue {
      override val hashCode = (head, tail).hashCode
    }
    case class SumValue(variant: Int, value: ProductValue) extends Value
    case class FnValue(toFn: Value => Eval[Value]) extends Value
    case class ExternalValue(toAny: Any) extends Value

    val False: Value = SumValue(0, UnitValue)
    val True: Value = SumValue(1, UnitValue)

    object Comparison {
      def fromInt(i: Int): Value =
        if (i < 0) LT else if (i > 0) GT else EQ

      val LT: Value = SumValue(0, UnitValue)
      val EQ: Value = SumValue(1, UnitValue)
      val GT: Value = SumValue(2, UnitValue)
    }

    def fromLit(l: Lit): Value =
      l match {
        case Lit.Str(s) => ExternalValue(s)
        case Lit.Integer(i) => ExternalValue(i)
      }

    object VInt {
      def apply(v: Int): Value = apply(BigInt(v))
      def apply(v: BigInt): Value = ExternalValue(v.bigInteger)
      def unapply(v: Value): Option[BigInteger] =
        v match {
          case ExternalValue(v: BigInteger) => Some(v)
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
      val VNil: Value = SumValue(0, UnitValue)
      object Cons {
        def apply(head: Value, tail: Value): Value =
          SumValue(1, ConsValue(head, ConsValue(tail, UnitValue)))

        def unapply(v: Value): Option[(Value, Value)] =
          v match {
            case SumValue(1, ConsValue(head, ConsValue(rest, UnitValue))) =>
              Some((head, rest))
            case _ => None
          }
      }

      def unapply(v: Value): Option[List[Value]] =
        v match {
          case VNil => Some(Nil)
          case Cons(head, rest) =>
            unapply(rest).map(head :: _)
          case _ => None
        }
    }
  }

  sealed abstract class Scoped {
    import Scoped.fromFn

    def inEnv(env: Map[String, Value]): Eval[Value]

    def letNameIn(name: String, in: Scoped): Scoped =
      Scoped.Let(name, this, in)

    def flatMap(fn: (Map[String, Value], Value) => Eval[Value]): Scoped =
      fromFn { env =>
        inEnv(env).flatMap { v =>
          fn(env, v)
        }
      }

    def asLambda(name: String): Scoped =
      fromFn { env =>

        val fn =
          FnValue { x =>
            inEnv(env.updated(name, x))
          }

        Eval.now(fn)
      }

    def emptyScope: Scoped =
      fromFn(_ => inEnv(Map.empty))

    def applyArg(arg: Scoped): Scoped =
      fromFn { env =>
        for {
          fn <- inEnv(env)
          a <- arg.inEnv(env)
          res <- fn.asFn(a) // safe because we typecheck
        } yield res
      }
  }

  object Scoped {
    def const(e: Eval[Value]): Scoped =
      fromFn(_ => e)

    def unreachable: Scoped =
      const(Eval.later(sys.error("unreachable reached")))

    def orElse(name: String, next: Scoped): Scoped =
      fromFn { env =>
        env.get(name) match {
          case None => next.inEnv(env)
          case Some(v) => Eval.now(v)
        }
      }

    private case class Let(name: String, arg: Scoped, in: Scoped) extends Scoped {
      def inEnv(env: Map[String, Value]) =
        arg.inEnv(env).flatMap { v =>
          in.inEnv(env.updated(name, v))
        }.memoize
    }

    private def fromFn(fn: Map[String, Value] => Eval[Value]): Scoped =
      FromFn(fn)

    private case class FromFn(fn: Map[String, Value] => Eval[Value]) extends Scoped {
      def inEnv(env: Map[String, Value]) = fn(env)
    }
  }

}

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  import Evaluation.{Value, Scoped}
  import Value._

  def evaluate(p: PackageName, varName: String): Option[(Eval[Value], Type)] =
    pm.toMap.get(p).map { pack =>
      val (s, t) = eval((Package.asInferred(pack), Left(varName)))
      (s.inEnv(Map.empty), t)
    }

  def evaluateLast(p: PackageName): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (_, expr) <- pack.program.lets.lastOption
      tup = eval((Package.asInferred(pack), Right(expr)))
    } yield (tup._1.inEnv(Map.empty), tup._2)

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

  private def evalBranch(
    tpe: Type,
    branches: NonEmptyList[(Pattern[(PackageName, ConstructorName), Type], TypedExpr[Declaration])],
    p: Package.Inferred,
    recurse: ((Package.Inferred, Ref)) => (Scoped, Type)): (Value, Map[String, Value]) => Eval[Value] = {
      val dtConst@Type.TyConst(Type.Const.Defined(pn0, tn)) =
        Type.rootConst(tpe).get // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)
      // this is calling apply on a map, but is safe because of type-checking
      val dt = packageForType.program.types.definedTypes((pn0, TypeName(tn)))

      def definedForCons(pc: (PackageName, ConstructorName)): DefinedType =
        pm.toMap(pc._1).program.types.constructors(pc)._2

      def bindEnv[E](arg: Value,
        branches: List[(Pattern[(PackageName, ConstructorName), Type], E)],
        acc: Map[String, Value]): Option[(Map[String, Value], E)] =
        branches match {
          case Nil => None
          case (Pattern.WildCard, next):: tail => Some((acc, next))
          case (Pattern.Var(n), next) :: tail => Some((acc + (n -> arg), next))
          case (Pattern.Annotation(p, _), next) :: tail =>
            // TODO we may need to use the type here
            bindEnv(arg, (p, next) :: tail, acc)
          case (Pattern.PositionalStruct(pc@(pack, ctor), items), next) :: tail =>
            /*
             * The type in question is not the outer dt, but the type associated
             * with this current constructor
             */
            val dt = definedForCons(pc)
            val optParams =
              if (dt.isStruct) {
                // this is a struct, which means we expect it
                arg match {
                  case p: ProductValue =>
                    Some(p.toList)

                  case other =>
                    val ts = TypeRef.fromType(tpe).fold(tpe.toString)(_.toDoc.render(80))
                    sys.error(s"ill typed in match (${ctor.asString}${items.mkString}): $ts\n\n$other")
                }
              }
              else {
                arg match {
                  case SumValue(enumId, v) =>
                    val cname = dt.constructors(enumId)._1
                    if (cname == ctor) {
                      Some(v.toList)
                    }
                    else {
                      None
                    }
                  case other => sys.error(s"ill typed in match: $other")
                }
              }

            optParams.flatMap { params =>
              // this is the pattern
              // note passing in a List here we could have union patterns
              // if all the pattern bindings were known to be of the same type
              params.zip(items)
                .foldM(acc) { case (e, (arg, pat)) =>
                  bindEnv(arg, List((pat, next)), e).map(_._1)
                }
                .map((_, next))
            }
            .orElse {
                  // we didn't match, go to the next branch
                  bindEnv(arg, tail, acc)
            }
        }

      // recurse on all the branches:
      val compiledBranches = branches.map { case (pattern, branch) =>
        (pattern, recurse((p, Right(branch)))._1)
      }

      { (arg, env) =>
        val (localEnv, next) = bindEnv(arg, compiledBranches.toList, env)
          .getOrElse(
              // TODO make sure we rule this out statically
              sys.error(s"non-total match: arg: $arg, branches: ${branches.head._2.tag.toDoc.render(80)}")
            )
        next.inEnv(localEnv)
      }
    }

  /**
   * TODO, expr is a TypedExpr so we already know the type. returning it does not do any good that I
   * can see.
   */
  private def evalTypedExpr(p: Package.Inferred,
    expr: TypedExpr[Declaration],
    recurse: ((Package.Inferred, Ref)) => (Scoped, Type)): Scoped = {

    import TypedExpr._

     expr match {
       case Generic(_, e, _) =>
         // TODO, we need to probably do something with this
         evalTypedExpr(p, e, recurse)
       case Annotation(e, _, _) => evalTypedExpr(p, e, recurse)
       case Var(v, _, _) =>
         val onMissing = recurse((p, Left(v)))._1

         Scoped.orElse(v, onMissing)
       case App(AnnotatedLambda(name, _, fn, _), arg, _, _) =>
         val argE = recurse((p, Right(arg)))._1
         val fnE = recurse((p, Right(fn)))._1

         argE.letNameIn(name, fnE)
       case App(fn, arg, _, _) =>
         val efn = recurse((p, Right(fn)))._1
         val earg = recurse((p, Right(arg)))._1

         efn.applyArg(earg)
       case a@AnnotatedLambda(name, _, expr, _) =>
         val inner = recurse((p, Right(expr)))._1

         inner.asLambda(name)
       case Let(arg, e, in, _) =>
         val eres = recurse((p, Right(e)))._1
         val inres = recurse((p, Right(in)))._1

         eres.letNameIn(arg, inres)
       case Literal(lit, _, _) =>
         val res = Eval.now(Value.fromLit(lit))

         Scoped.const(res)
       case If(cond, ifT, ifF, _) =>
         val condR = recurse((p, Right(cond)))._1
         val ifR = recurse((p, Right(ifT)))._1
         val elseR = recurse((p, Right(ifF)))._1

         condR.flatMap {
           case (env, True) => ifR.inEnv(env)
           case (env, False) => elseR.inEnv(env)
           case other => sys.error(s"ill-typed, expected boolean: $other")
         }

       case Match(arg, branches, _) =>
         val argR = recurse((p, Right(arg)))._1
         val branchR = evalBranch(arg.getType, branches, p, recurse)

         argR.flatMap { (env, a) =>
           branchR(a, env)
         }
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: ((Package.Inferred, Ref)) => (Scoped, Type) =
    Memoize.function[(Package.Inferred, Ref), (Scoped, Type)] {
      case ((pack, Right(expr)), recurse) =>
        (evalTypedExpr(pack, expr, recurse), expr.getType)
      case ((pack, Left(item)), recurse) =>
        NameKind(pack, item) match {
          case None =>
            // this isn't great, but since we fully compile, even when
            // we don't use a branch, we hit this now
            (Scoped.unreachable, Type.IntType)
          case Some(NameKind.Let(expr)) =>
            recurse((pack, Right(expr)))
          case Some(NameKind.Constructor(cn, _, dt, tpe)) =>
            (Scoped.const(constructor(cn, dt)), tpe)
          case Some(NameKind.Import(from, orig)) =>
            val other = recurse((from, Left(orig)))

            // we reset the environment in the other package
            (other._1.emptyScope, other._2)
          case Some(NameKind.ExternalDef(pn, n, tpe)) =>
            externals.toMap.get((pn, n)) match {
              case None =>
                throw EvaluationException(s"Missing External defintion of '${pn.parts.toList.mkString("/")} $n'. Check that your 'external' parameter is correct.")
              case Some(ext) =>
                (Scoped.const(ext.call(tpe)), tpe)
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

    val singleItemStruct = dt.isStruct

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

    /*
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
