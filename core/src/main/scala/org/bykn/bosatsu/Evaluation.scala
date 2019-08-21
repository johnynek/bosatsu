package org.bykn.bosatsu

import cats.Eval
import cats.data.{Const, NonEmptyList}
import com.stripe.dagon.Memoize
import java.math.BigInteger
import org.bykn.bosatsu.rankn.{DefinedType, Type}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}

import cats.implicits._

import Identifier.{Bindable, Constructor}

object Evaluation {

  /**
    * If we later determine that this performance matters
    * and this wrapping is hurting, we could replace
    * Value with a less structured type and put
    * all the reflection into unapply calls but keep
    * most of the API
    */
  abstract class Value[T[_]](implicit valueT: ValueT[T]) {
    def asLazyFn: Eval[Value[T]] => Eval[Value[T]] =
      this match {
        case valueT.FnValue(f, _) => f
        case other                =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
        // $COVERAGE-ON$
      }

    def asFn: Value[T] => Eval[Value[T]] =
      this match {
        case valueT.FnValue(f, _) => { v =>
          f(Eval.now(v))
        }
        case other =>
          // $COVERAGE-OFF$this should be unreachable
          sys.error(s"invalid cast to Fn: $other")
        // $COVERAGE-ON$
      }
  }

  case class ValueT[T[_]]() {
    implicit val thisImpl = this
    sealed abstract class ProductValue extends Value[T] {
      def toList: List[Value[T]] =
        this match {
          case UnitValue             => Nil
          case ConsValue(head, tail) => head :: tail.toList
        }
    }

    object ProductValue {
      def fromList(ps: List[Value[T]]): ProductValue = ps match {
        case Nil       => UnitValue
        case h :: tail => ConsValue(h, fromList(tail))
      }
      def unapply(v: ProductValue): Option[List[Value[T]]] = Some(v.toList)
    }

    case object UnitValue extends ProductValue
    case class ConsValue(head: Value[T], tail: ProductValue)
        extends ProductValue {
      override val hashCode = (head, tail).hashCode
    }

    case class SumValue(variant: Int, value: ProductValue) extends Value[T]

    case class FnValue(toFn: Eval[Value[T]] => Eval[Value[T]], tag: T[Value[T]])
        extends Value[T]
    object FnValue {
      def apply(tag: T[Value[T]])(
          toFn: Eval[Value[T]] => Eval[Value[T]]
      ): FnValue = FnValue(toFn, tag)
    }
    case class ExternalValue(toAny: Any, tokenizeFn: Any => Json)
        extends Value[T] {
      lazy val tokenize = Some(tokenizeFn(toAny))
    }

    val False = SumValue(0, UnitValue)
    val True = SumValue(1, UnitValue)

    object TupleCons {
      def unapply(v: Value[T]): Option[(Value[T], Value[T])] =
        v match {
          case ConsValue(a, ConsValue(b, UnitValue)) => Some((a, b))
          case _                                     => None
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
      def unapply(v: Value[T]): Option[List[Value[T]]] =
        v match {
          case TupleCons(a, b) =>
            unapply(b).map(a :: _)
          case UnitValue => Some(Nil)
          case _         => None
        }
    }

    object Comparison {
      def fromInt(i: Int): Value[T] =
        if (i < 0) LT else if (i > 0) GT else EQ

      val LT = SumValue(0, UnitValue)
      val EQ = SumValue(1, UnitValue)
      val GT = SumValue(2, UnitValue)
    }

    val tokenizeString: Any => Json = s => Json.JString(s.toString)
    val tokenizeInt: Any => Json = i => Json.JNumberStr(i.toString)
    def fromLit(l: Lit): Value[T] =
      l match {
        case Lit.Str(s)     => ExternalValue(s, tokenizeString)
        case Lit.Integer(i) => ExternalValue(i, tokenizeInt)
      }

    object VInt {
      def apply(v: Int): Value[T] = apply(BigInt(v))
      def apply(v: BigInt): Value[T] = ExternalValue(v.bigInteger, tokenizeInt)
      def unapply(v: Value[T]): Option[BigInteger] =
        v match {
          case ExternalValue(v: BigInteger, _) => Some(v)
          case _                               => None
        }
    }

    object Str {
      def apply(str: String) = ExternalValue(str, tokenizeString)
      def unapply(v: Value[T]): Option[String] =
        v match {
          case ExternalValue(str: String, _) => Some(str)
          case _                             => None
        }
    }

    object VOption {
      val none = SumValue(0, UnitValue)
      def some(v: Value[T]): Value[T] = SumValue(1, ConsValue(v, UnitValue))

      def unapply(v: Value[T]): Option[Option[Value[T]]] =
        v match {
          case SumValue(0, UnitValue) =>
            Some(None)
          case SumValue(1, ConsValue(head, UnitValue)) =>
            Some(Some(head))
          case _ => None
        }
    }

    object VList {
      val VNil = SumValue(0, UnitValue)
      object Cons {
        def apply(head: Value[T], tail: Value[T]): Value[T] =
          SumValue(1, ConsValue(head, ConsValue(tail, UnitValue)))

        def unapply(v: Value[T]): Option[(Value[T], Value[T])] =
          v match {
            case SumValue(1, ConsValue(head, ConsValue(rest, UnitValue))) =>
              Some((head, rest))
            case _ => None
          }
      }

      def apply(items: List[Value[T]]): Value[T] = {
        @annotation.tailrec
        def go(vs: List[Value[T]], acc: Value[T]): Value[T] =
          vs match {
            case Nil       => acc
            case h :: tail => go(tail, Cons(h, acc))
          }
        go(items.reverse, VNil)
      }

      def unapply(v: Value[T]): Option[List[Value[T]]] =
        v match {
          case VNil => Some(Nil)
          case Cons(head, rest) =>
            unapply(rest).map(head :: _)
          case _ => None
        }
    }

    object VDict {
      def unapply(v: Value[T]): Option[SortedMap[Value[T], Value[T]]] =
        v match {
          case ExternalValue(v: SortedMap[_, _], _) =>
            Some(v.asInstanceOf[SortedMap[Value[T], Value[T]]])
          case _ => None
        }
    }
  }

  case class Env[T, V[_]](map: Map[Identifier, Eval[Value[V]]], tag: T) {
    def updated(name: Identifier, n: Eval[Value[V]]) =
      copy(map = map.updated(name, n))
    // def addLambdaVar(name: Identifier, n: Eval[Value[T]]) = copy(map = map.updated(name, n), lambdas = n :: lambdas)
    def get(name: Identifier) = map.get(name)
  }

  sealed abstract class Scoped[E, V[_]](implicit valueT: ValueT[V]) {
    import Scoped.fromFn

    def inEnv(env: Env[E, V]): Eval[Value[V]]

    def letNameIn(name: Bindable, in: Scoped[E, V]): Scoped[E, V] =
      Scoped.Let(name, this, in)

    def branch(fn: (Value[V], Env[E, V]) => Eval[Value[V]]): Scoped[E, V] =
      fromFn { env =>
        inEnv(env).flatMap { v =>
          fn(v, env)
        }
      }

    def asLambda[T](name: Bindable, tag: T)(
        implicit
        updateEnv: (Env[E, V], Bindable, Eval[Value[V]]) => Env[E, V],
        valueTag: (T, Env[E, V]) => V[Value[V]]
    ): Scoped[E, V] =
      fromFn[E, V] { env =>
        import cats.Now
        val fn =
          valueT.FnValue(valueTag(tag, env)) {
            case n @ Now(v) =>
              inEnv(updateEnv(env, name, n)) // inEnv(env.addLambdaVar(name, n))
            case v =>
              v.flatMap { v0 =>
                inEnv(updateEnv(env, name, Eval.now(v0)))
              // inEnv(env.addLambdaVar(name, Eval.now(v0)))
              }
          }
        //ne, ne.map (ne => env.lambdas.take(ne.maxLambdaVar.getOrElse(0))))

        Eval.now(fn)
      }

    def emptyScope(implicit emptyEnv: Env[E, V]): Scoped[E, V] =
      fromFn[E, V](_ => inEnv(emptyEnv))

    def applyArg(arg: Scoped[E,V]): Scoped[E,V] =
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
    def const[E, V[_]](
        e: Eval[Value[V]]
    )(implicit valueT: ValueT[V]): Scoped[E, V] =
      fromFn[E, V](_ => e)

    def unreachable[E, V[_]](implicit valueT: ValueT[V]): Scoped[E, V] =
      const(Eval.always(sys.error("unreachable reached")))

    def fromEnv[E, V[_]](identifier: Identifier)(implicit valueT: ValueT[V]): Scoped[E, V] =
      fromFn { env =>
        env.get(identifier) match {
          case Some(e) => e
          case None => sys.error(s"could not find $identifier in the environment with keys: ${env.map.keys.toList.sorted}")
        }
      }

    def recursive[E, V[_]](name: Bindable, item: Scoped[E,V])(implicit valueT: ValueT[V]): Scoped[E,V] =
      fromFn { env =>
        lazy val env1: Env[E, V] =
          env.updated(name, Eval.defer(item.inEnv(env1)).memoize)
        item.inEnv(env1)
      }

    def orElse[E, V[_]](
      name: Identifier
    )(next: => Scoped[E, V])(implicit valueT: ValueT[V]): Scoped[E, V] = {
      lazy val nextComputed = next
      fromFn[E, V] { env =>
        env.get(name) match {
          case None    => nextComputed.inEnv(env)
          case Some(v) => v
        }
      }
    }

    private case class Let[E, V[_]](
        name: Bindable,
        arg: Scoped[E, V],
        in: Scoped[E, V]
    )(implicit valueT: ValueT[V])
        extends Scoped[E, V] {
      def inEnv(env: Env[E, V]) = {
        val let = arg.inEnv(env)
        in.inEnv(env.updated(name, let))
      }
    }

    private def fromFn[E, V[_]](
        fn: Env[E, V] => Eval[Value[V]]
    )(implicit valueT: ValueT[V]): Scoped[E, V] =
      FromFn(fn)

    private case class FromFn[E, V[_]](fn: Env[E, V] => Eval[Value[V]])(
        implicit valueT: ValueT[V]
    ) extends Scoped[E, V] {
      def inEnv(env: Env[E, V]) = fn(env)
    }
  }

  case class UnitImplicits[T]() {
    implicit val tokenize: Value[Const[Unit, ?]] => Json = _ => Json.JNull
    implicit val emptyEnv: Env[Unit, Const[Unit, ?]] = Env(Map(), ())
    implicit val updateEnv: (
        Env[Unit, Const[Unit, ?]],
        Bindable,
        Eval[Value[Const[Unit, ?]]]
    ) => Env[Unit, Const[Unit, ?]] = (env, name, ev) =>
      Env(env.map.updated(name, ev), env.tag)
    implicit val valueTag: (
        T,
        Evaluation.Env[Unit, Const[Unit, ?]]
    ) => Const[Unit, Value[Const[Unit, ?]]] = (_, _) => Const(())
    implicit val externalFnTag: (PackageName, Identifier) => (
        Int,
        List[Eval[Value[Const[Unit, ?]]]]
    ) => Const[Unit, Value[Const[Unit, ?]]] =
      (_, _) => (_, _) => Const(())
    implicit val tagForConstructor: (
        Int,
        List[Value[Const[Unit, ?]]],
        Int
    ) => Const[Unit, Value[Const[Unit, ?]]] = (_, _, _) => Const(())
    implicit val valueT: ValueT[Const[Unit, ?]] = ValueT()
    implicit val predefImpl: PredefImpl[Const[Unit, ?]] = PredefImpl()
  }

  type NEValueTag[X] = (NormalExpression, List[Eval[X]])
  type NEEnv = Env[List[Eval[Value[NEValueTag]]], NEValueTag]
  case class NETokenImplicits[T]()(
      implicit extractNormalExpression: T => NormalExpression
  ) {
    implicit val valueT: ValueT[NEValueTag] = ValueT()
    implicit val predefImpl: PredefImpl[NEValueTag] = PredefImpl()
    implicit val emptyEnv: NEEnv = Env(Map(), List())
    implicit val updateEnv
        : (NEEnv, Bindable, Eval[Value[NEValueTag]]) => NEEnv =
      (env, name, n) =>
        env.copy(map = env.map.updated(name, n), tag = n :: env.tag)
    implicit val valueTag: (T, NEEnv) => NEValueTag[Value[NEValueTag]] =
      (t, env) => {
        val ne = extractNormalExpression(t)
        (ne, env.tag.take(ne.maxLambdaVar.map(_ + 1).getOrElse(0)))
      }
    import NormalExpression.{App, LambdaVar, ExternalVar, Struct, Lambda}
    def applyNTimes(n: Int, ne0: NormalExpression): NormalExpression =
      (0 until n).foldLeft(ne0) { (ne, k) =>
        App(ne, LambdaVar(n - k))
      }
    implicit val externalFnTag: (PackageName, Identifier) => (
        Int,
        List[Eval[Value[NEValueTag]]]
    ) => NEValueTag[Value[NEValueTag]] =
      (pn, dn) => {
        val ev = ExternalVar(pn, dn)
        (k, vars) => (applyNTimes(k, ev), vars)
      }
    def lambdaNTimes(n: Int, ne0: NormalExpression): NormalExpression =
      (0 until n).foldLeft(ne0) { (ne, _) =>
        Lambda(ne)
      }
    implicit val tagForConstructor
        : (Int, List[Value[NEValueTag]], Int) => NEValueTag[Value[NEValueTag]] =
      (param, args, enum) => {
        val arity = param + args.length
        val struct = Struct(enum, (0 until arity).map { k =>
          LambdaVar(arity - k)
        }.toList)
        (lambdaNTimes(param, struct), args.map(Eval.now))
      }
    implicit val tokenize
        : Value[NEValueTag] => Json = {
          case valueT.ExternalValue(any, tf) => tf(any)
          case valueT.FnValue(_, tag) => {
            val lst = List(
              "ne" -> Json.JString(tag._1.toString),
              "scope" -> Json.JArray(tag._2.map(ev => tokenize(ev.value)).toVector)
            )
            Json.JObject(lst)
          }
          case valueT.ProductValue(lst) => Json.JArray(lst.map(tokenize).toVector)
          case valueT.SumValue(k, p)  => Json.JArray((Json.JNumberStr(k.toString) +: p.toList.map(tokenize)).toVector)

    }
  }
}

case class Evaluation[T, E, V[_]](
    pm: PackageMap.Typed[T],
    externals: Externals[V]
)(
    implicit
    emptyEnv: Evaluation.Env[E, V],
    updateEnv: (
        Evaluation.Env[E, V],
        Bindable,
        Eval[Evaluation.Value[V]]
    ) => Evaluation.Env[E, V],
    valueTag: (T, Evaluation.Env[E, V]) => V[Evaluation.Value[V]],
    externalFnTag: (PackageName, Identifier) => (
        Int,
        List[Eval[Evaluation.Value[V]]]
    ) => V[Evaluation.Value[V]],
    tagForConstructor: (
        Int,
        List[Evaluation.Value[V]],
        Int
    ) => V[Evaluation.Value[V]],
    valueT: Evaluation.ValueT[V]
) {
  import Evaluation.{Value, Scoped, Env}
  import valueT.{
    ConsValue,
    True,
    UnitValue,
    Str,
    VList,
    False,
    fromLit,
    ProductValue,
    SumValue,
    FnValue,
    ExternalValue,
    VOption,
    VDict,
    Tuple
  }

  /**
   * Holds the final value of the environment for each Package
   */
  private[this] val envCache: MMap[PackageName, Env[E, V]] =
    MMap.empty

  private def importedEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value[V]]] =
    p.imports.iterator.flatMap { imp =>
      val pack = pm.toMap.get(imp.pack.name) match {
        case Some(p) => p
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown package: ${imp.pack.name}")
          // $COVERAGE-ON$
      }
      imp.items
        .toList
        .iterator
        .filter { in =>
          // We can ignore type imports, they aren't values
          in.tag.exists {
            case Referant.Value(_) | Referant.Constructor(_, _, _, _) => true
            case Referant.DefinedT(_) => false
          }
        }
        .map { in =>
          val value = getValue(pack, in.originalName)
          (in.localName, value)
        }
    }
    .toMap

  private def externalEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value[V]]] = {
    val externalNames = p.program.externalDefs
    externalNames.iterator.map { n =>
      val tpe = p.program.types.getValue(p.name, n) match {
        case Some(t) => t
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} import unknown external def: $n")
          // $COVERAGE-ON$
      }
      externals.toMap.get((p.name, n.asString)) match {
        case Some(ext) => (n, ext.call(tpe, p.name, n))
        case None =>
          // $COVERAGE-OFF$
          // should never happen due to typechecking
          sys.error(s"from ${p.name} no External for external def: $n")
          // $COVERAGE-ON$
      }
    }
    .toMap
  }

  private def constructorEnv(p: Package.Typed[T]): Map[Identifier, Eval[Value[V]]] =
    p.program
      .types
      .allDefinedTypes
      .iterator
      .filter(_.packageName == p.name)
      .flatMap { dt =>
        dt.constructors.iterator.map { case (cn, _, _) =>
          (cn, constructor(cn, dt))
        }
      }
      .toMap

  private def addLet(env: Env[E, V], let: (Bindable, RecursionKind, TypedExpr[T])): Env[E, V] = {
    val (name, rec, e) = let
    val e0 = eval(e)
    val eres =
      if (rec.isRecursive) Scoped.recursive(name, e0)
      else e0

    // These are added lazily
    env.updated(name, Eval.defer(eres.inEnv(env)).memoize)
  }

  private def evaluate(pack: Package.Typed[T]): Env[E, V] =
    envCache.getOrElseUpdate(pack.name, {
      val initEnv = emptyEnv.copy(map = importedEnv(pack) ++ constructorEnv(pack) ++ externalEnv(pack))
      // add all the external definitions
      pack.program.lets.foldLeft(initEnv)(addLet(_, _))
    })

  private def getValue(pack: Package.Typed[T], name: Identifier): Eval[Value[V]] =
    evaluate(pack)
      .get(name)
      // error shouldn't happen due to typechecking
      .getOrElse(sys.error(s"unknown value: $name in ${pack.name}"))

  def evaluateLast(p: PackageName): Option[(Eval[Value[V]], Type)] =
    for {
      pack <- pm.toMap.get(p)
      (name, _, tpe) <- pack.program.lets.lastOption
      value <- evaluate(pack).get(name)
    } yield (value, tpe.getType)

  def evaluateLets(p: PackageName): List[(Bindable, (Eval[Value[V]], Type, T))] = {
    for {
      pack <- pm.toMap.get(p).toList
      env = evaluate(pack)
      (name, _, expr) <- pack.program.lets
      value <- evaluate(pack).get(name).toList
    } yield (name, (value, expr.getType, expr.tag))
  }

  /* TODO: this is useful for debugging, but we should probably test it and write a parser for the
   * list syntax

  def repr: String = {
    val packs = pm.toMap.map { case (_, pack) =>
      Doc.text(s"(package ${pack.name.asString}") +
        (Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace,
          pack.program.lets.map { case (b, _, te) =>
            Doc.text(s"(let ${b.asString}") +
              (Doc.lineOrSpace + Doc.text(te.repr) + Doc.text(")")).nested(2)
          })).nested(2) + Doc.char(')')
    }

    Doc.intercalate(Doc.lineOrSpace, packs).render(80)
  }

  */

  def evalTest(ps: PackageName): Option[Test] =
    evaluateLast(ps).flatMap { case (ea, tpe) =>
      def toAssert(a: Value[V]): Test =
        a match {
          case ConsValue(True, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(true, message)
          case ConsValue(False, ConsValue(Str(message), UnitValue)) =>
            Test.Assertion(false, message)
          case other =>
            // $COVERAGE-OFF$
            sys.error(s"expected test value: $other")
            // $COVERAGE-ON$
        }
      def toSuite(a: Value[V]): Test =
        a match {
          case ConsValue(Str(name), ConsValue(VList(tests), UnitValue)) =>
            Test.Suite(name, tests.map(toTest(_)))
          case other =>
            // $COVERAGE-OFF$
            sys.error(s"expected test value: $other")
            // $COVERAGE-ON$
        }
      def toTest(a: Value[V]): Test =
        a match {
          case SumValue(0, assertion) => toAssert(assertion)
          case SumValue(1, suite) => toSuite(suite)
          case unexpected =>
            // $COVERAGE-OFF$
            sys.error(s"unreachable if compilation has worked: $unexpected")
            // $COVERAGE-ON$
        }

      tpe match {
        case Type.TyConst(Type.Const.Defined(Predef.Name, tn)) =>
          tn.ident.asString match {
            case "Test" =>
              Some(toTest(ea.value))
            case _ =>
              None
          }
        case _ => None
      }
    }

  private type Ref = TypedExpr[T]

  private def evalBranch(
    tpe: Type,
    branches: NonEmptyList[(Pattern[(PackageName, Constructor), Type], TypedExpr[T])],
    recurse: Ref => Scoped[E,V]): (Value[V], Env[E, V]) => Eval[Value[V]] = {
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
            val vlit = fromLit(lit)

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
                        case None       => None
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
          case Pattern.PositionalStruct(pc @ (_, ctor), items) =>
            /*
            * The type in question is not the outer dt, but the type associated
            * with this current constructor
            */
            val dt = definedForCons(pc)
            val itemFns = items.map(maybeBind(_))

            def processArgs(
                as: List[Value[V]],
                acc: Env[E, V]
            ): Option[Env[E, V]] = {
              // manually write out foldM hoping for performance improvements
              @annotation.tailrec
              def loop(
                  vs: List[Value[V]],
                  fns: List[(Value[V], Env[E, V]) => Option[Env[E, V]]],
                  env: Env[E, V]
              ): Option[Env[E, V]] =
                vs match {
                  case Nil => Some(env)
                  case vh :: vt =>
                    fns match {
                      case fh :: ft =>
                        fh(vh, env) match {
                          case None       => None
                          case Some(env1) => loop(vt, ft, env1)
                        }
                      case Nil =>
                        Some(env) // mismatch in size, shouldn't happen statically
                    }
                }
              loop(as, itemFns, acc)
            }

            if (dt.isStruct) {
              // this is a struct, which means we expect it
              { (arg: Value[V], acc: Env[E,V]) =>
                arg match {
                  case p: ProductValue =>
                    // this is the pattern
                    // note passing in a List here we could have union patterns
                    // if all the pattern bindings were known to be of the same type
                    processArgs(p.toList, acc)

                  case other =>
                    // $COVERAGE-OFF$this should be unreachable
                    val ts = TypeRef.fromTypes(None, tpe :: Nil)(tpe).toDoc.render(80)
                    val itemStr = items.mkString("(", ", ", ")")
                    sys.error(s"ill typed in match (${ctor.asString}$itemStr: $ts\n\n$other\n\nenv: $acc")
                    // $COVERAGE-ON$
                }
              }
            }
          else {
            // compute the index of ctor, so we can compare integers later
            val idx = dt.constructors.map(_._1).indexOf(ctor)

            // we don't check if idx < 0, because if we compiled, it can't be
            { (arg: Value[V], acc: Env[E, V]) =>
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
            val fntail = bindEnv(tail)

            { (arg, env) =>
              pfn(arg, env) match {
                case None => fntail(arg, env)
                case Some(env) => (env, e)
              }
            }
        }

      // recurse on all the branches:
      val compiledBranches = branches.map { case (pattern, branch) =>
        (pattern, recurse(branch))
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
  @annotation.tailrec
  private def evalTypedExpr(expr: TypedExpr[T], recurse: Ref => Scoped[E, V]): Scoped[E, V] = {

    import TypedExpr._

     expr match {
       case Generic(_, e, _) =>
         // TODO, we need to probably do something with this
         evalTypedExpr(e, recurse)
       case Annotation(e, _, _) => evalTypedExpr(e, recurse)
       case Var(None, ident, _, _) =>
         Scoped.fromEnv(ident)
       case Var(Some(p), ident, _, _) =>
         val pack = pm.toMap.get(p).getOrElse(sys.error(s"cannot find $p, shouldn't happen due to typechecking"))
         Scoped.const(getValue(pack, ident))
       case App(fn, arg, _, _) =>
         fn match {
           case AnnotatedLambda(name, _, fn, _) =>
             // f(\x -> res)(y) = let x = y in res
             val argE = recurse(arg)
             val fnE = recurse(fn)

             argE.letNameIn(name, fnE)
           case fn =>
             val efn = recurse(fn)
             val earg = recurse(arg)

             efn.applyArg(earg)
         }
       case a@AnnotatedLambda(name, _, expr, _) =>
         expr match {
           case App(f, Var(None, name2, _, _), _, _) if name2 == name =>
             // here is eta-conversion: \x -> f(x) == f
             recurse(f)
           case expr =>
             val inner = recurse(expr)
             inner.asLambda(name, a.tag)
         }
       case Let(arg, e, in, rec, _) =>
         val e0 = recurse(e)
         val eres =
           if (rec.isRecursive) Scoped.recursive(arg, e0)
           else e0
         val inres = recurse(in)

         eres.letNameIn(arg, inres)
       case Literal(lit, _, _) =>
         val res = Eval.now(fromLit(lit))
         Scoped.const(res)
       case Match(arg, branches, _) =>
         val argR = recurse(arg)
         val branchR = evalBranch(arg.getType, branches, recurse)

         argR.branch(branchR(_, _))
    }
  }

  /**
   * We only call this on typechecked names, which means we know
   * that names resolve
   */
  private[this] val eval: Ref => Scoped[E, V] =
    Memoize.function[Ref, Scoped[E, V]] {
      (expr, recurse) => evalTypedExpr(expr, recurse)
    }

  private def constructor(
      c: Constructor,
      dt: rankn.DefinedType[Any]
  ): Eval[Value[V]] = {
    val (enum, arity) =
      dt.constructors.toList.iterator.zipWithIndex.collectFirst {
        case ((ctor, params, resType), idx) if ctor == c => (idx, params.size)
      }.get // the ctor must be in the list or we wouldn't typecheck

    val singleItemStruct = dt.isStruct

    // TODO: this is a obviously terrible
    // the encoding is inefficient, the implementation is inefficient
    def loop(param: Int, args: List[Value[V]]): Value[V] =
      if (param == 0) {
        val prod = ProductValue.fromList(args.reverse)
        if (singleItemStruct) prod
        else SumValue(enum, prod)
      } else
        FnValue(tagForConstructor(param, args, enum)) {
          case cats.Now(a) => cats.Now(loop(param - 1, a :: args))
          case ea =>
            ea.map { a =>
              loop(param - 1, a :: args)
            }.memoize
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
        case Type.ForAll(_, inner)           => canEncodeToNull(inner)
        case _                               => false
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
          case True  => Some(Json.JBool(true))
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
              toJson(a, tpe).map { j =>
                Json.JArray(Vector(j))
              }
          }
        } else {
          // not a nested option
          a match {
            case VOption(None)    => Some(Json.JNull)
            case VOption(Some(a)) => toJson(a, tpe)
          }
        }
      case Type.ListT(t) =>
        val VList(vs) = a
        vs.toVector
          .traverse { v =>
            toJson(v, t)
          }
          .map(Json.JArray(_))
      case Type.DictT(Type.StrType, vt) =>
        val VDict(d) = a
        d.toList
          .traverse {
            case (k, v) =>
              val Str(kstr) = k
              toJson(v, vt).map((kstr, _))
          }
          .map(Json.JObject(_))
      case Type.Tuple(ts) =>
        val Tuple(as) = a
        as.zip(ts)
          .toVector
          .traverse {
            case (a, t) =>
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
            case p: ProductValue      => Some((0, p))
            case _                    => None
          }
        val optDt = Type
          .rootConst(tpe)
          .flatMap {
            case Type.TyConst(Type.Const.Defined(pn, n)) =>
              defined(pn, n)
          }

        (vp, optDt)
          .mapN {
            case ((variant, prod), dt) =>
              val cons = dt.constructors
              val (_, targs) = Type.applicationArgs(tpe)
              val replaceMap = dt.typeParams.zip(targs).toMap[Type.Var, Type]
              cons.lift(variant).flatMap {
                case (_, params, _) =>
                  prod.toList.zip(params).traverse {
                    case (a1, (pn, t)) =>
                      toJson(a1, Type.substituteVar(t, replaceMap))
                        .map((pn.asString, _))
                  }
              }
          }
          .flatten
          .map { ps =>
            Json.JObject(ps)
          }
    }
  }

  def defined(pn: PackageName, t: TypeName): Option[rankn.DefinedType[Any]] =
    for {
      pack <- pm.toMap.get(pn)
      dt <- pack.program.types.getType(pn, t)
    } yield dt
}

case class EvaluationException(message: String) extends Exception(message)
