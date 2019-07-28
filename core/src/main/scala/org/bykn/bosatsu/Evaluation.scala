package org.bykn.bosatsu

import cats.Eval
import cats.data.NonEmptyList
import com.stripe.dagon.Memoize
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}
import scala.collection.immutable.SortedMap

import cats.implicits._

import Identifier.{Bindable, Constructor}

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

    object TupleCons {
      def unapply(v: Value): Option[(Value, Value)] =
        v match {
          case ConsValue(a, ConsValue(b, UnitValue)) => Some((a, b))
          case _ => None
        }
    }

    object Tuple {
      /**
       * Tuples are encoded as:
       * (1, 2, 3) => TupleCons(1, TupleCons(2, TupleCons(3, ())))
       * since a Tuple(a, b) is encoded as
       * ConsValue(a, ConsValue(b, UnitValue))
       * this gives double wrapping
       */
      def unapply(v: Value): Option[List[Value]] =
        v match {
          case TupleCons(a, b) =>
            unapply(b).map(a :: _)
          case UnitValue => Some(Nil)
          case _ => None
        }
    }

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
          case SumValue(0, UnitValue) =>
            Some(None)
          case SumValue(1, ConsValue(head, UnitValue)) =>
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

    object VDict {
      def unapply(v: Value): Option[SortedMap[Value, Value]] =
        v match {
          case ExternalValue(v: SortedMap[_, _]) => Some(v.asInstanceOf[SortedMap[Value, Value]])
          case _ => None
        }
    }
  }

  type Env = Map[Identifier, Eval[Value]]

  sealed abstract class Scoped {
    import Scoped.fromFn

    def inEnv(env: Env): Eval[Value]

    def letNameIn(name: Bindable, in: Scoped): Scoped =
      Scoped.Let(name, this, in)

    def flatMap(fn: (Env, Value) => Eval[Value]): Scoped =
      fromFn { env =>
        inEnv(env).flatMap { v =>
          fn(env, v)
        }
      }

    def asLambda(name: Bindable): Scoped =
      fromFn { env =>
        import cats.Now
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
        val fnE = inEnv(env).memoize
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

    def recursive(name: Bindable, item: Scoped): Scoped = {
      fromFn { env =>
        lazy val env1: Map[Identifier, Eval[Value]] =
          env.updated(name, Eval.defer(item.inEnv(env1)).memoize)
        item.inEnv(env1)
      }
    }

    def orElse(name: Identifier)(next: => Scoped): Scoped = {
      lazy val nextComputed = next
      fromFn { env =>
        env.get(name) match {
          case None => nextComputed.inEnv(env)
          case Some(v) => v
        }
      }
    }

    private case class Let(name: Bindable, arg: Scoped, in: Scoped) extends Scoped {
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

case class Evaluation[T](pm: PackageMap.Typed[T], externals: Externals) {
  import Evaluation.{Value, Scoped, Env}
  import Value._

  def evaluate(p: PackageName, varName: Identifier): Option[(Eval[Value], Type)] =
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

      tpe match {
        case Type.TyConst(Type.Const.Defined(Predef.Name, tn)) =>
          tn.ident.asString match {
            case "Assertion" =>
              Some(toAssert(ea.value))
            case "Test" =>
              Some(toTest(ea.value))
            case "TestSuite" =>
              Some(toSuite(ea.value))
            case _ =>
              None
          }
        case _ => None
      }
    }

  private type Ref = Either[Identifier, TypedExpr[T]]

  private def evalBranch(
    tpe: Type,
    branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])],
    p: Package.Typed[T],
    recurse: ((Package.Typed[T], Ref)) => (Scoped, Type)): (Value, Env) => Eval[Value] = {
      val dtConst@Type.TyConst(Type.Const.Defined(pn0, tn)) =
        Type.rootConst(tpe).getOrElse(sys.error(s"failure to get type: $tpe")) // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)

      def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
        pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2

      val noop: (Value, Env) => Option[Env] = { (_, env) => Some(env) }
      val neverMatch: (Value, Env) => Option[Nothing] = { (_, _) => None }
      /*
       * This is used in a loop internally, so I am avoiding map and flatMap
       * in favor of pattern matching for performance
       */
      def maybeBind[E](pat: Pattern[(PackageName, Constructor), Type]): (Value, Env) => Option[Env] =
        pat match {
          case Pattern.WildCard => noop
          case Pattern.Literal(lit) =>
            val vlit = Value.fromLit(lit)

            { (v, env) => if (v == vlit) Some(env) else None }
          case Pattern.Var(n) =>

            { (v, env) => Some(env.updated(n, Eval.now(v))) }
          case Pattern.Named(n, p) =>
            val inner = maybeBind(p)

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
              case Pattern.ListPart.Item(ph) :: ptail =>
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
              case Pattern.ListPart.NamedList(ident) :: Nil =>
                { (v, env) => Some(env.updated(ident, Eval.now(v))) }
              case Pattern.ListPart.WildList :: Nil =>
                noop
                // this is the common and easy case: a total match of the tail
                // we don't need to match on it being a list, because we have
                // already type checked
              case (splice: Pattern.ListPart.Glob) :: ptail =>
                // this is more costly, since we have to match a non infinite tail.
                // we reverse the tails, do the match, and take the rest into
                // the splice
                val revPat = Pattern.ListPat(ptail.reverse)
                val fnMatchTail = maybeBind(revPat)
                val ptailSize = ptail.size

                splice match {
                  case Pattern.ListPart.NamedList(nm) =>
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
                  case Pattern.ListPart.WildList =>
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
            def loop(ps: List[Pattern[(PackageName, Constructor), Type]]): (Value, Env) => Option[Env] =
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
      def bindEnv[E](branches: List[(Pattern[(PackageName, Constructor), Type], E)]): (Value, Env) => (Env, E) =
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
  private def evalTypedExpr(p: Package.Typed[T],
    expr: TypedExpr[T],
    recurse: ((Package.Typed[T], Ref)) => (Scoped, Type)): Scoped = {

    import TypedExpr._

     expr match {
       case Generic(_, e, _) =>
         // TODO, we need to probably do something with this
         evalTypedExpr(p, e, recurse)
       case Annotation(e, _, _) => evalTypedExpr(p, e, recurse)
       case Var(None, ident, _, _) =>
         Scoped.orElse(ident) {
         // this needs to be lazy
           recurse((p, Left(ident)))._1
         }
       case Var(Some(p), ident, _, _) =>
         val pack = pm.toMap.get(p).getOrElse(sys.error(s"cannot find $p, shouldn't happen due to typechecking"))
         val (scoped, _) = eval((pack, Left(ident)))
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
         inner.asLambda(name)
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
  private[this] val eval: ((Package.Typed[T], Ref)) => (Scoped, Type) =
    Memoize.function[(Package.Typed[T], Ref), (Scoped, Type)] {
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
            val infFrom = pm.toMap(from.name)
            val other = recurse((infFrom, Left(orig)))

            // we reset the environment in the other package
            (other._1.emptyScope, other._2)
          case Some(NameKind.ExternalDef(pn, n, tpe)) =>
            externals.toMap.get((pn, n.asString)) match {
              case None =>
                throw EvaluationException(s"Missing External defintion of '${pn.parts.toList.mkString("/")} $n'. Check that your 'external' parameter is correct.")
              case Some(ext) =>
                (Scoped.const(ext.call(tpe)), tpe)
            }
        }
    }

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): Eval[Value] = {
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
      else FnValue {
        case cats.Now(a) => cats.Now(loop(param - 1, a :: args))
        case ea => ea.map { a => loop(param - 1, a :: args) }.memoize
      }

    Eval.now(loop(arity, Nil))
  }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  def toJson(a: Value, tpe: Type): Option[Json] = {
    def canEncodeToNull(t: Type): Boolean =
      t match {
        case Type.UnitType | Type.OptionT(_) => true
        case Type.ForAll(_, inner) => canEncodeToNull(inner)
        case _ => false
      }
    tpe match {
      case Type.IntType =>
        val ExternalValue(v) = a
        Some(Json.JNumberStr(v.toString))
      case Type.StrType =>
        val ExternalValue(v) = a
        Some(Json.JString(v.toString))
      case Type.BoolType =>
        a match {
          case True => Some(Json.JBool(true))
          case False => Some(Json.JBool(false))
          case other =>
            // $COVERAGE-OFF$this should be unreachable
            sys.error(s"invalid cast to Boolean: $other")
            // $COVERAGE-ON$
        }
      case Type.UnitType =>
        // encode this as null
        Some(Json.JNull)
      case Type.OptionT(tpe) =>
        if (canEncodeToNull(tpe)) {
          // we can't encode Option[Option[T]] as null or not, so we encode
          // as list of 0 or 1 items
          a match {
            case VOption(None) => Some(Json.JArray(Vector.empty))
            case VOption(Some(a)) =>
              toJson(a, tpe).map { j => Json.JArray(Vector(j)) }
          }
        }
        else {
          // not a nested option
          a match {
            case VOption(None) => Some(Json.JNull)
            case VOption(Some(a)) => toJson(a, tpe)
          }
        }
      case Type.ListT(t) =>
        val VList(vs) = a
        vs.toVector
          .traverse { v => toJson(v, t) }
          .map(Json.JArray(_))
      case Type.DictT(Type.StrType, vt) =>
        val VDict(d) = a
        d.toList.traverse { case (k, v) =>
          val Str(kstr) = k
          toJson(v, vt).map((kstr, _))
        }
        .map(Json.JObject(_))
      case Type.Tuple(ts) =>
        val Tuple(as) = a
        as.zip(ts)
          .toVector
          .traverse { case (a, t) =>
            toJson(a, t)
          }
          .map(Json.JArray(_))
      case Type.ForAll(_, inner) =>
        // we assume the generic positions don't matter and to continue
        toJson(a, inner)
      case _ =>
        val vp =
          a match {
            case SumValue(variant, p) => Some((variant, p))
            case p: ProductValue => Some((0, p))
            case _ => None
          }
        val optDt = Type.rootConst(tpe)
          .flatMap {
            case Type.TyConst(Type.Const.Defined(pn, n)) =>
              defined(pn, n)
          }

        (vp, optDt).mapN { case ((variant, prod), dt) =>
          val cons = dt.constructors
          val (_, targs) = Type.applicationArgs(tpe)
          val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]
          cons.lift(variant).flatMap { case (_, params, _) =>
            prod.toList.zip(params).traverse { case (a1, (pn, t)) =>
              toJson(a1, Type.substituteVar(t, replaceMap)).map((pn.asString, _))
            }
          }
        }
        .flatten
        .map { ps => Json.JObject(ps) }
    }
  }

  def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType[Any]] =
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
}

case class EvaluationException(message: String) extends Exception(message)
