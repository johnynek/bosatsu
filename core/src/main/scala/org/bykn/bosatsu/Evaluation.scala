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
  sealed abstract class Value[+T] {
    def asLazyFn[S]: Eval[Value[S]] => Eval[Value[S]] =
      this match {
        case FnValue(f, _) => f.asInstanceOf[Eval[Value[S]] => Eval[Value[S]]]
        case other =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
          // $COVERAGE-ON$
      }

    def asFn[S]: Value[S] => Eval[Value[S]] =
      this match {
        case FnValue(f, _) => { v => f(Eval.now(v.asInstanceOf[Value[T]])).asInstanceOf[Eval[Value[S]]] }
        case other =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
          // $COVERAGE-ON$
      }
  }

  object Value {
    sealed abstract class ProductValue[+T] extends Value[T] {
      def toList: List[Value[T]] =
        this match {
          case UnitValue => Nil
          case ConsValue(head, tail) => head :: tail.toList
        }
    }

    object ProductValue {
      def fromList[T](ps: List[Value[T]]): ProductValue[T] =
        ps match {
          case Nil => UnitValue
          case h :: tail => ConsValue(h, fromList(tail))
        }
    }

    case object UnitValue extends ProductValue
    case class ConsValue[+T](head: Value[T], tail: ProductValue[T]) extends ProductValue[T] {
      override val hashCode = (head, tail).hashCode
    }
    case class SumValue[T](variant: Int, value: ProductValue[T]) extends Value[T]
    case class FnValue[T](toFn: Eval[Value[T]] => Eval[Value[T]], tag: T) extends Value[T]
    object FnValue {
      def apply[T](tag: T)(toFn: Eval[Value[T]] => Eval[Value[T]]): FnValue[T] = FnValue(toFn, tag)
    }
    case class ExternalValue(toAny: Any, tokenizeFn: Any => String) extends Value {
      lazy val tokenize = Some(tokenizeFn(toAny))
    }

    val False: Value[Nothing] = SumValue(0, UnitValue)
    val True: Value[Nothing] = SumValue(1, UnitValue)

    object TupleCons {
      def unapply[T](v: Value[T]): Option[(Value[T], Value[T])] =
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
      def unapply[T](v: Value[T]): Option[List[Value[T]]] =
        v match {
          case TupleCons(a, b) =>
            unapply(b).map(a :: _)
          case UnitValue => Some(Nil)
          case _ => None
        }
    }

    object Comparison {
      def fromInt(i: Int): Value[Nothing] =
        if (i < 0) LT else if (i > 0) GT else EQ

      val LT: Value[Nothing] = SumValue(0, UnitValue)
      val EQ: Value[Nothing] = SumValue(1, UnitValue)
      val GT: Value[Nothing] = SumValue(2, UnitValue)
    }

    val tokenizeString: Any => String = _.asInstanceOf[String]
    val tokenizeInt: Any => String = _.asInstanceOf[BigInteger].toString
    def fromLit(l: Lit): Value[Nothing] =
      l match {
        case Lit.Str(s) => ExternalValue(s, tokenizeString)
        case Lit.Integer(i) => ExternalValue(i, tokenizeInt)
      }

    object VInt {
      def apply(v: Int): Value[Nothing] = apply(BigInt(v))
      def apply(v: BigInt): Value[Nothing] = ExternalValue(v.bigInteger, tokenizeInt)
      def unapply[T](v: Value[T]): Option[BigInteger] =
        v match {
          case ExternalValue(v: BigInteger, _) => Some(v)
          case _ => None
        }
    }

    object Str {
      def apply(str: String) = ExternalValue(str, tokenizeString)
      def unapply[T](v: Value[T]): Option[String] =
        v match {
          case ExternalValue(str: String, _) => Some(str)
          case _ => None
        }
    }

    object VOption {
      val none: Value[Nothing] = SumValue(0, UnitValue)
      def some[T](v: Value[T]): Value[T] = SumValue(1, ConsValue(v, UnitValue))

      def unapply[T](v: Value[T]): Option[Option[Value[T]]] =
        v match {
          case SumValue(0, UnitValue) =>
            Some(None)
          case SumValue(1, ConsValue(head, UnitValue)) =>
            Some(Some(head))
          case _ => None
        }
    }

    object VList {
      val VNil: Value[Nothing] = SumValue(0, UnitValue)
      object Cons {
        def apply[T](head: Value[T], tail: Value[T]): Value[T] =
          SumValue(1, ConsValue(head, ConsValue(tail, UnitValue)))

        def unapply[T](v: Value[T]): Option[(Value[T], Value[T])] =
          v match {
            case SumValue(1, ConsValue(head, ConsValue(rest, UnitValue))) =>
              Some((head, rest))
            case _ => None
          }
      }

      def apply[T](items: List[Value[T]]): Value[T] = {
        @annotation.tailrec
        def go(vs: List[Value[T]], acc: Value[T]): Value[T] =
          vs match {
            case Nil => acc
            case h :: tail => go(tail, Cons(h, acc))
          }
        go(items.reverse, VNil)
      }

      def unapply[T](v: Value[T]): Option[List[Value[T]]] =
        v match {
          case VNil => Some(Nil)
          case Cons(head, rest) =>
            unapply(rest).map(head :: _)
          case _ => None
        }
    }

    object VDict {
      def unapply[T](v: Value[T]): Option[SortedMap[Value[T], Value[T]]] =
        v match {
          case ExternalValue(v: SortedMap[_, _], _) => Some(v.asInstanceOf[SortedMap[Value[T], Value[T]]])
          case _ => None
        }
    }
  }

  case class Env[T,V](map: Map[Identifier, Eval[Value[V]]], tag: T) {
    def updated(name: Identifier, n: Eval[Value[V]]) = copy(map = map.updated(name, n))
    // def addLambdaVar(name: Identifier, n: Eval[Value[T]]) = copy(map = map.updated(name, n), lambdas = n :: lambdas)
    def get(name: Identifier) = map.get(name)
  }

  object Env {
    val empty = Env(Map.empty, Nil)
  }

  sealed abstract class Scoped[T,E,V] {
    import Scoped.fromFn

    def inEnv(env: Env[E,V]): Eval[Value[V]]

    def letNameIn(name: Bindable, in: Scoped[T,E,V]): Scoped[T,E,V] =
      Scoped.Let(name, this, in)

    def flatMap(fn: (Env[E,V], Value[V]) => Eval[Value[V]]): Scoped[T,E,V] =
      fromFn[T,E,V] { env =>
        inEnv(env).flatMap { v =>
          fn(env, v)
        }
      }

    def asLambda(name: Bindable, tag: T)(implicit
      updateEnv: (Env[E,V], Bindable, Eval[Value[V]]) => Env[E,V],
      valueTag: (T, Env[E,V]) => V 
    ): Scoped[T,E,V] =
      fromFn[T,E,V] { env =>
        import cats.Now
        val fn =
          FnValue[V](valueTag(tag, env)) {
            case n@Now(v) =>
              inEnv(updateEnv(env, name, n)) // inEnv(env.addLambdaVar(name, n))
            case v => v.flatMap { v0 =>
              inEnv(updateEnv(env, name, Eval.now(v0)))
              // inEnv(env.addLambdaVar(name, Eval.now(v0)))
            }
          }
          //ne, ne.map (ne => env.lambdas.take(ne.maxLambdaVar.getOrElse(0))))

        Eval.now(fn)
      }

    def emptyScope(implicit emptyEnv: Env[E,V]): Scoped[T,E,V] =
      fromFn[T,E,V](_ => inEnv(emptyEnv))

    def applyArg(arg: Scoped[T,E,V]): Scoped[T,E,V] =
      fromFn[T,E,V] { env =>
        val fnE = inEnv(env).memoize
        val argE = arg.inEnv(env)
        fnE.flatMap { fn =>
          // safe because we typecheck
          fn.asLazyFn(argE)
        }
      }


  }

  object Scoped {
    def const[T,E,V](e: Eval[Value[V]]): Scoped[T,E,V] =
      fromFn[T,E,V](_ => e)

    def unreachable[T,E,V]: Scoped[T,E,V] =
      const(Eval.always(sys.error("unreachable reached")))

    def recursive[T,E,V](name: Bindable, item: Scoped[T,E,V]): Scoped[T,E,V] = {
      fromFn[T,E,V] { env =>
        lazy val env1: Env[E,V] =
          env.updated(name, Eval.defer(item.inEnv(env1)).memoize)
        item.inEnv(env1)
      }
    }

    def orElse[T,E,V](name: Identifier)(next: => Scoped[T,E,V]): Scoped[T,E,V] = {
      lazy val nextComputed = next
      fromFn[T,E,V] { env =>
        env.get(name) match {
          case None => nextComputed.inEnv(env)
          case Some(v) => v
        }
      }
    }

    private case class Let[T,E,V](name: Bindable, arg: Scoped[T,E,V], in: Scoped[T,E,V]) extends Scoped[T,E,V] {
      def inEnv(env: Env[E,V]) = {
        val let = arg.inEnv(env)
        in.inEnv(env.updated(name, let))
      }
    }

    private def fromFn[T,E,V](fn: Env[E,V] => Eval[Value[V]]): Scoped[T,E,V] =
      FromFn(fn)

    private case class FromFn[T,E,V](fn: Env[E,V] => Eval[Value[V]]) extends Scoped[T,E,V] {
      def inEnv(env: Env[E,V]) = fn(env)
    }
  }

  case class UnitImplicits[T]() {
    implicit val tokenize: Value[Unit] => String = _ => ""
    implicit val emptyEnv: Env[Unit, Unit] = Env(Map(), ())
    implicit val scopeTagFromTag: T => Unit = _ => ()
    implicit val updateEnv: (Env[Unit, Unit], Bindable, Eval[Value[Unit]]) => Env[Unit, Unit] = (env, name, ev) =>
      Env(env.map.updated(name, ev), env.tag)
    implicit val valueTag: (Unit, Evaluation.Env[Unit, Unit]) => Unit = (_, _) => ()
    implicit val externalFnTag: (PackageName, Identifier) => (Int, List[Eval[Value[Unit]]]) => Unit =
      (_, _) => (_, _) => ()
    implicit val tagForConstructor: Unit = ()
  }

}

case class Evaluation[T, S, E, V](pm: PackageMap.Typed[T], externals: Externals[V])(implicit
  scopeTagFromTag: T => S,
  emptyEnv: Evaluation.Env[E,V],
  updateEnv: (Evaluation.Env[E,V], Bindable, Eval[Evaluation.Value[V]]) => Evaluation.Env[E,V],
  valueTag: (S, Evaluation.Env[E,V]) => V,
  externalFnTag: (PackageName, Identifier) => (Int, List[Eval[Evaluation.Value[V]]]) => V,
  tagForConstructor: V
) {
  import Evaluation.{Value, Scoped, Env}
  import Value._

  def evaluate(p: PackageName, varName: Identifier): Option[(Eval[Value[V]], Type)] =
    pm.toMap.get(p).map { pack =>
      val (s, t) = eval((pack, Left(varName)))
      (s.inEnv(emptyEnv), t)
    }

  def evaluateLast(p: PackageName): Option[(Eval[Value[V]], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, rec, expr) <- pack.program.lets.lastOption
      (scope0, tpe) = eval((pack, Right(expr)))
      scope = if (rec.isRecursive) Scoped.recursive(name, scope0) else scope0
    } yield (scope.inEnv(emptyEnv), tpe)

  def evaluateLets(p: PackageName): List[(Bindable, (Eval[Value[V]], Type, S))] =
    for {
      pack <- pm.toMap.get(p).toList
      (name, rec, expr) <- pack.program.lets
      (scope0, tpe) = eval((pack, Right(expr)))
      scope = if (rec.isRecursive) Scoped.recursive(name, scope0) else scope0
    } yield (name, (scope.inEnv(emptyEnv), tpe, scopeTagFromTag(expr.tag)))

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, tpe) =>

      // struct Assertion(value: Bool, message: String)
      // struct Test(name: String, assertions: List[Assertion])
      // struct TestSuite(name: String, tests: List[Test])
      def toAssert(a: Value[V]): Test =
        a match {
          case ConsValue(True, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(true, message)
          case ConsValue(False, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(false, message)
          case other => sys.error(s"expected test value: $other")
        }
      def toTest(a: Value[V]): Test =
        a match {
          case ConsValue(Str(name), ConsValue(VList(asserts), UnitValue)) =>
            Test.Suite(name, asserts.map(toAssert(_)))
          case other => sys.error(s"expected test value: $other")
        }
      def toSuite(a: Value[V]): Test =
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
    recurse: ((Package.Typed[T], Ref)) => (Scoped[S,E,V], Type)): (Value[V], Env[E,V]) => Eval[Value[V]] = {
      val dtConst@Type.TyConst(Type.Const.Defined(pn0, tn)) =
        Type.rootConst(tpe).getOrElse(sys.error(s"failure to get type: $tpe")) // this is safe because it has type checked

      val packageForType = pm.toMap(pn0)

      def definedForCons(pc: (PackageName, Constructor)): DefinedType[Any] =
        pm.toMap(pc._1).program.types.getConstructor(pc._1, pc._2).get._2

      val noop: (Value[V], Env[E,V]) => Option[Env[E,V]] = { (_, env) => Some(env) }
      val neverMatch: (Value[V], Env[E,V]) => Option[Nothing] = { (_, _) => None }
      /*
       * This is used in a loop internally, so I am avoiding map and flatMap
       * in favor of pattern matching for performance
       */
      def maybeBind[B](pat: Pattern[(PackageName, Constructor), Type]): (Value[V], Env[E,V]) => Option[Env[E,V]] =
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
                  case Some(ident) =>

                    { (v, env) => Some(env.updated(ident, Eval.now(v))) }
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
            def loop(ps: List[Pattern[(PackageName, Constructor), Type]]): (Value[V], Env[E,V]) => Option[Env[E,V]] =
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

            def processArgs(as: List[Value[V]], acc: Env[E,V]): Option[Env[E,V]] = {
              // manually write out foldM hoping for performance improvements
              @annotation.tailrec
              def loop(vs: List[Value[V]], fns: List[(Value[V], Env[E,V]) => Option[Env[E,V]]], env: Env[E,V]): Option[Env[E,V]] =
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
              { (arg: Value[V], acc: Env[E,V]) =>
                arg match {
                  case p: ProductValue[V] =>
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
              { (arg: Value[V], acc: Env[E,V]) =>
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
      def bindEnv[B](branches: List[(Pattern[(PackageName, Constructor), Type], B)]): (Value[V], Env[E,V]) => (Env[E,V], B) =
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

            val fnh = { (arg: Value[V], env: Env[E,V]) =>
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
    recurse: ((Package.Typed[T], Ref)) => (Scoped[S,E,V], Type)): Scoped[S,E,V] = {

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
         inner.asLambda(name, scopeTagFromTag(a.tag))
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
  private[this] val eval: ((Package.Typed[T], Ref)) => (Scoped[S,E,V], Type) =
    Memoize.function[(Package.Typed[T], Ref), (Scoped[S,E,V], Type)] {
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
                (Scoped.const(ext.call(tpe, pn, n)), tpe)
            }
        }
    }

  private def constructor(c: Constructor, dt: rankn.DefinedType[Any]): Eval[Value[V]] = {
    val (enum, arity) = dt.constructors
      .toList
      .iterator
      .zipWithIndex
      .collectFirst { case ((ctor, params, resType), idx) if ctor == c => (idx, params.size) }
      .get // the ctor must be in the list or we wouldn't typecheck

    val singleItemStruct = dt.isStruct

    // TODO: this is a obviously terrible
    // the encoding is inefficient, the implementation is inefficient
    val tag: V = tagForConstructor
    def loop(param: Int, args: List[Value[V]]): Value[V] =
      if (param == 0) {
        val prod = ProductValue.fromList(args.reverse)
        if (singleItemStruct) prod
        else SumValue(enum, prod)
      }
      else FnValue(tag) {
        case cats.Now(a) => cats.Now(loop(param - 1, a :: args))
        case ea => ea.map { a => loop(param - 1, a :: args) }.memoize
      }
      //, Some(Lambda(Struct(enum, List(LambdaVar(0))))), Some(args.map(Eval.now)))

    Eval.now(loop(arity, Nil))
  }

  /**
   * Convert a typechecked value to Json
   * this code ASSUMES the type is correct. If not, we may throw or return
   * incorrect data.
   */
  def toJson(a: Value[V], tpe: Type): Option[Json] = {
    def canEncodeToNull(t: Type): Boolean =
      t match {
        case Type.UnitType | Type.OptionT(_) => true
        case Type.ForAll(_, inner) => canEncodeToNull(inner)
        case _ => false
      }
    tpe match {
      case Type.IntType =>
        val ExternalValue(v, _) = a
        Some(Json.JNumberStr(v.toString))
      case Type.StrType =>
        val ExternalValue(v, _) = a
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
            case p: ProductValue[V] => Some((0, p))
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
