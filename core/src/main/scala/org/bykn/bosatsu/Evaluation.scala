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
    def asLazyFn: Eval[Value] => Eval[Value] =
      this match {
        case FnValue(f) => f
        case other =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
          // $COVERAGE-ON$
      }

    def asFn: Value => Eval[Value] =
      this match {
        case FnValue(f) => { v => f(Eval.now(v)) }
        case other =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
          // $COVERAGE-ON$
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
    case class FnValue(toFn: Eval[Value] => Eval[Value]) extends Value
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
      val none: Value = SumValue(0, UnitValue)
      def some(v: Value): Value = SumValue(1, ConsValue(v, UnitValue))

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

      def apply(items: List[Value]): Value = {
        @annotation.tailrec
        def go(vs: List[Value], acc: Value): Value =
          vs match {
            case Nil => acc
            case h :: tail => go(tail, Cons(h, acc))
          }
        go(items.reverse, VNil)
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

  type Env = Map[String, Eval[Value]]

  sealed abstract class Scoped {
    import Scoped.fromFn

    def inEnv(env: Env): Eval[Value]

    def letNameIn(name: String, in: Scoped): Scoped =
      Scoped.Let(name, this, in)

    def flatMap(fn: (Env, Value) => Eval[Value]): Scoped =
      fromFn { env =>
        inEnv(env).flatMap { v =>
          fn(env, v)
        }
      }

    def asLambda(name: String, set: Set[String]): Scoped =
      fromFn { env0 =>
        import cats.Now
        // we can remove anything not captured by the closure
        val env = env0.filter { case (k, _) => set(k) }
        val fn =
          FnValue {
            case n@Now(v) =>
              inEnv(env.updated(name, n))
            case v => v.flatMap { v0 =>
              inEnv(env.updated(name, Eval.now(v0)))
            }
          }

        Eval.now(fn)
      }

    def emptyScope: Scoped =
      fromFn(_ => inEnv(Map.empty))

    def applyArg(arg: Scoped): Scoped =
      fromFn { env =>
        val fnE = inEnv(env)
        val argE = arg.inEnv(env)
        fnE.flatMap { fn =>
          // safe because we typecheck
          fn.asLazyFn(argE)
        }
      }
  }

  object Scoped {
    def const(e: Eval[Value]): Scoped =
      fromFn(_ => e)

    val unreachable: Scoped =
      const(Eval.always(sys.error("unreachable reached")))

    def recursive(name: String, item: Scoped): Scoped = {
      fromFn { env =>
        lazy val env1: Map[String, Eval[Value]] =
          env + (name -> Eval.defer(item.inEnv(env1)).memoize)
        item.inEnv(env1)
      }
    }

    def orElse(name: String)(next: => Scoped): Scoped = {
      lazy val nextComputed = next
      fromFn { env =>
        env.get(name) match {
          case None => nextComputed.inEnv(env)
          case Some(v) => v
        }
      }
    }

    private case class Let(name: String, arg: Scoped, in: Scoped) extends Scoped {
      def inEnv(env: Env) = {
        val let = arg.inEnv(env)
        in.inEnv(env.updated(name, let))
      }
    }

    private def fromFn(fn: Env => Eval[Value]): Scoped =
      FromFn(fn)

    private case class FromFn(fn: Env => Eval[Value]) extends Scoped {
      def inEnv(env: Env) = fn(env)
    }
  }

}

case class Evaluation(pm: PackageMap.Inferred, externals: Externals) {
  import Evaluation.{Value, Scoped, Env}
  import Value._

  def evaluate(p: PackageName, varName: String): Option[(Eval[Value], Type)] =
    pm.toMap.get(p).map { pack =>
      val (s, t) = eval((pack, Left(varName)))
      (s.inEnv(Map.empty), t)
    }

  def evaluateLast(p: PackageName): Option[(Eval[Value], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, rec, expr) <- pack.program.lets.lastOption
      (scope0, tpe) = eval((pack, Right(expr)))
      scope = if (rec.isRecursive) Scoped.recursive(name, scope0) else scope0
    } yield (scope.inEnv(Map.empty), tpe)

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, tpe) =>

      // struct Assertion(value: Bool, message: String)
      // struct Test(name: String, assertions: List[Assertion])
      // struct TestSuite(name: String, tests: List[Test])
      def toAssert(a: Value): Test =
        a match {
          case ConsValue(True, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(true, message)
          case ConsValue(False, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(false, message)
          case other => sys.error(s"expected test value: $other")
        }
      def toTest(a: Value): Test =
        a match {
          case ConsValue(Str(name), ConsValue(VList(asserts), UnitValue)) =>
            Test.Suite(name, asserts.map(toAssert(_)))
          case other => sys.error(s"expected test value: $other")
        }
      def toSuite(a: Value): Test =
        a match {
          case ConsValue(Str(name), ConsValue(VList(tests), UnitValue)) =>
            Test.Suite(name, tests.map(toTest(_)))
          case other => sys.error(s"expected test value: $other")
        }

      toType[Test](ea.value, tpe) { (any, dt, rec) =>
        if (dt.packageName == Predef.packageName) {
          dt.name.asString match {
            case "Assertion" =>
              Some(toAssert(any))
            case "Test" =>
              // due to type checking, none of the above errors should hit
              Some(toTest(any))
            case "TestSuite" =>
              Some(toSuite(any))
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
    recurse: ((Package.Inferred, Ref)) => (Scoped, Type)): (Value, Env) => Eval[Value] = {
      val dtConst@Type.TyConst(Type.Const.Defined(pn0, tn)) =
        Type.rootConst(tpe).getOrElse(sys.error(s"failure to get type: $tpe")) // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)

      def definedForCons(pc: (PackageName, ConstructorName)): DefinedType[Any] =
        pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2

      val noop: (Value, Env) => Option[Env] = { (_, env) => Some(env) }
      val neverMatch: (Value, Env) => Option[Nothing] = { (_, _) => None }
      /*
       * This is used in a loop internally, so I am avoiding map and flatMap
       * in favor of pattern matching for performance
       */
      def maybeBind[E](pat: Pattern[(PackageName, ConstructorName), Type]): (Value, Env) => Option[Env] =
        pat match {
          case Pattern.WildCard => noop
          case Pattern.Literal(lit) =>
            val vlit = Value.fromLit(lit)

            { (v, env) => if (v == vlit) Some(env) else None }
          case Pattern.Var(n) =>
            { (v, env) => Some(env.updated(n, Eval.now(v))) }
          case Pattern.Named(n, p) =>
            val inner = maybeBind(p)
            // now transform this function:

            { (v, env) =>
              inner(v, env) match {
                case None => None
                case Some(env1) => Some(env1.updated(n, Eval.now(v)))
              }
            }
          case Pattern.ListPat(items) =>
            items match {
              case Nil =>
                { (arg, acc) =>
                  arg match {
                    case VList.VNil => Some(acc)
                    case _ => None
                  }
                }
              case Right(ph) :: ptail =>
                // a right hand side pattern never matches the empty list
                val fnh = maybeBind(ph)
                val fnt = maybeBind(Pattern.ListPat(ptail))

                { (arg, acc) =>
                  arg match {
                    case VList.Cons(argHead, argTail) =>
                      fnh(argHead, acc) match {
                        case None => None
                        case Some(acc1) => fnt(argTail, acc1)
                      }
                    case _ => None
                  }
                }
              case Left(splice) :: Nil =>
                // this is the common and easy case: a total match of the tail
                // we don't need to match on it being a list, because we have
                // already type checked
                splice match {
                  case Some(nm) =>
                    { (v, env) => Some(env.updated(nm, Eval.now(v))) }
                  case None =>
                    noop
                }
              case Left(splice) :: ptail =>
                // this is more costly, since we have to match a non infinite tail.
                // we reverse the tails, do the match, and take the rest into
                // the splice
                val revPat = Pattern.ListPat(ptail.reverse)
                val fnMatchTail = maybeBind(revPat)
                val ptailSize = ptail.size

                splice match {
                  case Some(nm) =>
                    { (arg, acc) =>
                      arg match {
                        case VList(asList) =>
                          // we only allow one splice, so we assume the rest of the patterns
                          val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                          fnMatchTail(VList(revArgTail), acc) match {
                            case None => None
                            case Some(acc1) => Some {
                              // now bind the rest into splice:
                              val rest = Eval.now(VList(spliceVals.reverse))
                              acc1.updated(nm, rest)
                            }
                          }
                        case notlist =>
                          // it has to be a list due to type checking
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match, expected list found: $notlist")
                          // $COVERAGE-ON$
                      }
                    }
                  case None =>
                    { (arg, acc) =>
                      arg match {
                        case VList(asList) =>
                          // we only allow one splice, so we assume the rest of the patterns
                          val (revArgTail, spliceVals) = asList.reverse.splitAt(ptailSize)
                          fnMatchTail(VList(revArgTail), acc)
                        case notlist =>
                          // it has to be a list due to type checking
                          // $COVERAGE-OFF$this should be unreachable
                          sys.error(s"ill typed in match, expected list found: $notlist")
                          // $COVERAGE-ON$
                      }
                    }
                }
            }
          case Pattern.Annotation(p, _) =>
            // TODO we may need to use the type here
            maybeBind(p)
          case Pattern.Union(h, t) =>
            // we can just loop expanding these out:
            def loop(ps: List[Pattern[(PackageName, ConstructorName), Type]]): (Value, Env) => Option[Env] =
              ps match {
                case Nil => neverMatch
                case head :: tail =>
                  val fnh = maybeBind(head)
                  val fnt = loop(tail)

                  { (arg, acc) =>
                    fnh(arg, acc) match {
                      case None => fnt(arg, acc)
                      case some => some
                    }
                  }
              }
            loop(h :: t.toList)
          case Pattern.PositionalStruct(pc@(_, ctor), items) =>
            /*
             * The type in question is not the outer dt, but the type associated
             * with this current constructor
             */
            val dt = definedForCons(pc)
            val itemFns = items.map(maybeBind(_))

            def processArgs(as: List[Value], acc: Env): Option[Env] = {
              // manually write out foldM hoping for performance improvements
              @annotation.tailrec
              def loop(vs: List[Value], fns: List[(Value, Env) => Option[Env]], env: Env): Option[Env] =
                vs match {
                  case Nil => Some(env)
                  case vh :: vt =>
                    fns match {
                      case fh :: ft =>
                        fh(vh, env) match {
                          case None => None
                          case Some(env1) => loop(vt, ft, env1)
                        }
                      case Nil => Some(env) // mismatch in size, shouldn't happen statically
                    }
                }
              loop(as, itemFns, acc)
            }

            if (dt.isStruct) {
              // this is a struct, which means we expect it
              { (arg: Value, acc: Env) =>
                arg match {
                  case p: ProductValue =>
                    // this is the pattern
                    // note passing in a List here we could have union patterns
                    // if all the pattern bindings were known to be of the same type
                    processArgs(p.toList, acc)

                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    val ts = TypeRef.fromTypes(Some(p.name), tpe :: Nil)(tpe).toDoc.render(80)
                    sys.error(s"ill typed in match (${ctor.asString}${items.mkString}): $ts\n\n$other")
                    // $COVERAGE-ON$
                }
              }
            }
            else {
              // compute the index of ctor, so we can compare integers later
              val idx = dt.constructors.map(_._1).indexOf(ctor)

              // we don't check if idx < 0, because if we compiled, it can't be
              { (arg: Value, acc: Env) =>
                arg match {
                  case SumValue(enumId, v) =>
                    if (enumId == idx) processArgs(v.toList, acc)
                    else None
                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    sys.error(s"ill typed in match: $other")
                    // $COVERAGE-ON$
                }
              }
            }
        }
      def bindEnv[E](branches: List[(Pattern[(PackageName, ConstructorName), Type], E)]): (Value, Env) => (Env, E) =
        branches match {
          case Nil =>
            // This is ruled out by typechecking, we know all our matches are total
            // $COVERAGE-OFF$this should be unreachable
            { (arg, env) =>
              sys.error(s"non-total match: arg: $arg, branches: $branches")
            }
            // $COVERAGE-ON$
          case (p, e) :: tail =>
            val pfn = maybeBind(p)

            val fnh = { (arg: Value, env: Env) =>
              pfn(arg, env) match {
                case None => None
                case Some(env) => Some((env, e))
              }
            }
            val fntail = bindEnv(tail)

            { (arg, acc) =>
              fnh(arg, acc) match {
                case None => fntail(arg, acc)
                case Some(r) => r
              }
            }
        }

      // recurse on all the branches:
      val compiledBranches = branches.map { case (pattern, branch) =>
        (pattern, recurse((p, Right(branch)))._1)
      }
      val bindFn = bindEnv(compiledBranches.toList)

      /*
       * Now we have fully compiled all the branches and eliminated a certain amount of runtime
       * cost of handling pattern matching
       */
      { (arg, env) =>
        val (localEnv, next) = bindFn(arg, env)
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
       case Var(None, ident, _, _) =>
         val v = ident.asString
         Scoped.orElse(v) {
         // this needs to be lazy
           recurse((p, Left(v)))._1
         }
       case Var(Some(p), ident, _, _) =>
         val v = ident.asString
         val pack = pm.toMap.get(p).getOrElse(sys.error(s"cannot find $p, shouldn't happen due to typechecking"))
         val (scoped, _) = eval((pack, Left(v)))
         scoped
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

         val freeVars = TypedExpr.freeVars(a :: Nil).toSet
         inner.asLambda(name, freeVars)
       case Let(arg, e, in, rec, _) =>
         val e0 = recurse((p, Right(e)))._1
         val eres =
           if (rec.isRecursive) Scoped.recursive(arg, e0)
           else e0
         val inres = recurse((p, Right(in)))._1

         eres.letNameIn(arg, inres)
       case Literal(lit, _, _) =>
         val res = Eval.now(Value.fromLit(lit))
         Scoped.const(res)
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
          case Some(NameKind.Let(name, recursive, expr)) =>
            val res0 = recurse((pack, Right(expr)))
            val s1 =
              if (recursive.isRecursive) Scoped.recursive(name, res0._1)
              else res0._1

            (s1, res0._2)
          case Some(NameKind.Constructor(cn, _, dt, tpe)) =>
            (Scoped.const(constructor(cn, dt)), tpe)
          case Some(NameKind.Import(from, orig)) =>
            val infFrom = pm.toMap(from.unfix.name)
            val other = recurse((infFrom, Left(orig)))

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

  private def constructor(c: ConstructorName, dt: rankn.DefinedType[Any]): Eval[Value] = {
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
      else FnValue { ea =>
        ea.map { a => loop(param - 1, a :: args) }.memoize
      }

    Eval.now(loop(arity, Nil))
  }

  private def definedToJson(a: Value, dt: rankn.DefinedType[Any], rec: (Value, Type) => Option[Json]): Option[Json] = if (dt.packageName == Predef.packageName) {
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
          .map { ps => Json.JObject(ps) }
        }
      }
    }

  def toJson(a: Value, tpe: Type): Option[Json] =
    toType[Json](a, tpe)(definedToJson(_, _, _))

  def toType[T](a: Value, t: Type)(fn: (Value, rankn.DefinedType[Any], (Value, Type) => Option[T]) => Option[T]): Option[T] = {
    def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType[Any]] =
      for {
        pack <- pm.toMap.get(pn)
        dt <- pack.program.types.getType(pn, t)
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
