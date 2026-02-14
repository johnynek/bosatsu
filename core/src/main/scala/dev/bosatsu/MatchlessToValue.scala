package dev.bosatsu

import cats.{Eval, Functor, Applicative, Eq}
import cats.data.NonEmptyList
import cats.evidence.Is
import cats.syntax.eq._
import java.math.BigInteger
import scala.collection.immutable.LongMap

import Identifier.Bindable
import Value._

object MatchlessToValue {
  import Matchless._

  // reuse some cache structures across a number of calls
  def traverse[F[_]: Functor, A](
      me: F[Expr[A]]
  )(resolve: (A, PackageName, Identifier) => Eval[Value]): F[Eval[Value]] = {
    val env = new Impl.Env(resolve)
    val fns = Functor[F].map(me) { expr =>
      env.loop(expr)
    }
    // now that we computed all the functions, compute the values

    Functor[F].map(fns) { fn =>
      Eval.later(fn(Impl.Scope.empty()))
    }
  }

  private object Impl {

    private val zeroNat: Value = ExternalValue(BigInteger.ZERO)
    private val succNat: Value = {
      def inc(v: Value): Value = {
        val bi = v.asExternal.toAny.asInstanceOf[BigInteger]
        ExternalValue(bi.add(BigInteger.ONE))
      }
      FnValue { case NonEmptyList(a, _) => inc(a) }
    }

    def makeCons(c: ConsExpr): Value =
      c match {
        case MakeEnum(variant, arity, _) =>
          if (arity == 0) SumValue(variant, UnitValue)
          else if (arity == 1) {
            FnValue { case NonEmptyList(v, _) =>
              SumValue(variant, ProductValue.single(v))
            }
          } else
            // arity > 1
            FnValue { args =>
              val prod = ProductValue.fromList(args.toList)
              SumValue(variant, prod)
            }
        case MakeStruct(arity) =>
          if (arity == 0) UnitValue
          else if (arity == 1) FnValue.identity
          else
            FnValue { args =>
              ProductValue.fromList(args.toList)
            }
        case ZeroNat => zeroNat
        case SuccNat => succNat
      }

    case object Uninitialized
    val uninit: Value = ExternalValue(Uninitialized)

    class DebugStr(prefix: String = "") {
      private var message: String = ""
      def set(msg: String): Unit =
        message = msg;

      def append(msg: String): Unit =
        message = message + " :: " + msg

      override def toString = prefix + message

      def scope(outer: String): DebugStr =
        new DebugStr(prefix + "/" + outer)
    }

    class Cell {
      private var value = uninit
      def set(v: Value): Unit =
        value = v

      def get(): Value = value
    }

    final case class Scope(
        locals: Map[Bindable, Eval[Value]],
        anon: LongMap[Value],
        muts: LongMap[Cell],
        slots: Vector[Value],
        extra: DebugStr
    ) {

      def let(b: Bindable, v: Eval[Value]): Scope =
        copy(locals = locals.updated(b, v))

      def letMuts(idxs: Iterator[Long]): Scope = {
        val mut1 = muts ++ idxs.map(l => (l, new Cell))
        copy(muts = mut1)
      }
      def letMut(idx: Long): Scope =
        copy(muts = muts.updated(idx, new Cell))

      def letAll(bs: NonEmptyList[Bindable], vs: NonEmptyList[Value]): Scope = {
        val b = bs.iterator
        val v = vs.iterator
        var local1 = locals
        while (b.hasNext) {
          local1 = local1.updated(b.next(), Eval.now(v.next()))
        }
        copy(locals = local1)
      }

      def debugString: String =
        s"local keys: ${locals.keySet}, anon keys: ${anon.keySet}, anonMut keys: ${muts.keySet}\nextra=$extra"

      def updateMut(mutIdx: Long, v: Value): Unit = {
        if (!muts.contains(mutIdx)) {
          sys.error(s"updateMut($mutIdx, _) but $mutIdx is empty: $debugString")
        }
        muts(mutIdx).set(v)
        ()
      }

    }

    object Scope {
      def empty(): Scope =
        Scope(
          Map.empty,
          LongMap.empty,
          LongMap.empty,
          Vector.empty,
          new DebugStr
        )

      def capture(it: Vector[Value], dbg: DebugStr = new DebugStr): Scope =
        Scope(
          Map.empty,
          LongMap.empty,
          LongMap.empty,
          it,
          dbg.scope("capture")
        )
    }

    sealed abstract class Scoped[A] {
      def apply(s: Scope): A
      def map[B](fn: A => B): Scoped[B]
      def and(
          that: Scoped[Boolean]
      )(implicit ev: Is[A, Boolean]): Scoped[Boolean] = {
        // boolean conditions are generally never static, so we can't easily exercise
        // this code if we specialize it. So, we assume it is dynamic here
        val thisBool = ev.substitute[Scoped](this)
        Dynamic { scope =>
          // this must be lazy in that it only
          // calls that if thisBool is true
          thisBool(scope) && that(scope)
        }
      }
      def toFn: Scope => A

      def withScope(ws: Scope => Scope): Scoped[A]
    }
    case class Dynamic[A](toFn: Scope => A) extends Scoped[A] {
      def apply(s: Scope) = toFn(s)
      def map[B](fn: A => B): Scoped[B] = Dynamic(toFn.andThen(fn))
      def withScope(ws: Scope => Scope): Scoped[A] =
        Dynamic(ws.andThen(toFn))
    }
    case class Static[A](value: A) extends Scoped[A] {
      def apply(s: Scope) = value
      def map[B](fn: A => B): Scoped[B] = Static(fn(value))
      def withScope(ws: Scope => Scope): Scoped[A] = this
      def toFn = Function.const(value)
    }

    object Scoped {
      implicit val matchlessScopedApplicative: Applicative[Scoped] =
        new Applicative[Scoped] {
          def pure[A](a: A): Scoped[A] = Static(a)
          override def map[A, B](aa: Scoped[A])(fn: A => B): Scoped[B] =
            aa.map(fn)
          override def map2[A, B, C](aa: Scoped[A], ab: Scoped[B])(
              fn: (A, B) => C
          ): Scoped[C] =
            (aa, ab) match {
              case (Static(a), Static(b)) => Static(fn(a, b))
              case (Static(a), db)        =>
                db.map(fn(a, _))
              case (da, Static(b)) =>
                da.map(fn(_, b))
              case (da, db) =>
                val fa = da.toFn
                val fb = db.toFn
                Dynamic(s => fn(fa(s), fb(s)))
            }
          override def ap[A, B](sf: Scoped[A => B])(sa: Scoped[A]): Scoped[B] =
            map2(sf, sa)(_(_))
        }
    }

    class Env[F](resolve: (F, PackageName, Identifier) => Eval[Value]) {
      // evaluating boolExpr can mutate an existing value in muts
      private def boolExpr(ix: BoolExpr[F]): Scoped[Boolean] =
        given Eq[Any] = Eq.fromUniversalEquals

        ix match {
          case EqualsLit(expr, lit) =>
            loop(expr).map { e =>
              val external = e.asExternal.toAny
              lit match {
                case lf: Lit.Float64 =>
                  external match {
                    case d: java.lang.Double =>
                      val left = lf.toDouble
                      val right = d.doubleValue
                      // Float literal matching follows numeric equality:
                      // -0.0 == 0.0 and NaN matches NaN.
                      (left == right) || (java.lang.Double.isNaN(
                        left
                      ) && java.lang.Double.isNaN(right))
                    case _ =>
                      // $COVERAGE-OFF$
                      false
                    // $COVERAGE-ON$
                  }
                case _ =>
                  val litAny = lit.unboxToAny
                  // Safe: Matchless values come from typechecked code, so equals only compares compatible values.
                  external === litAny
              }
            }

          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.map { v =>
                val external = v.asExternal.toAny
                // Safe: Matchless values come from typechecked code, so equals only compares compatible values.
                external === BigInteger.ZERO
              }
            else
              natF.map { v =>
                val external = v.asExternal.toAny
                // Safe: Matchless values come from typechecked code, so equals only compares compatible values.
                external =!= BigInteger.ZERO
              }

          case TrueConst     => Static(true)
          case And(ix1, ix2) =>
            boolExpr(ix1).and(boolExpr(ix2))

          case CheckVariant(enumV, idx, _, _) =>
            loop(enumV).map(_.asSum.variant == idx)

          case SetMut(LocalAnonMut(mut), expr) =>
            val exprF = loop(expr)
            // this is always dynamic
            Dynamic { (scope: Scope) =>
              scope.updateMut(mut, exprF(scope))
              true
            }
          case LetBool(localOrBind, value, in) =>
            val valueF = loop(value)
            val inF = boolExpr(in)

            localOrBind match {
              case Right(b) =>
                inF.withScope { (scope: Scope) =>
                  val vv = Eval.now(valueF(scope))
                  scope.let(b, vv)
                }
              case Left(LocalAnon(l)) =>
                inF.withScope { (scope: Scope) =>
                  val vv = valueF(scope)
                  scope.copy(anon = scope.anon.updated(l, vv))
                }
            }
          case LetMutBool(LocalAnonMut(ident), in) =>
            val inF = boolExpr(in)
            Dynamic { (scope: Scope) =>
              val scope1 = scope.letMut(ident)
              inF(scope1)
            }
        }

      // the locals can be recusive, so we box into Eval for laziness
      def loop(me: Expr[F]): Scoped[Value] =
        me match {
          case Lambda(Nil, None, args, res) =>
            val resFn = loop(res)
            // we can allocate once if there is no closure
            val scope1 = Scope.empty()
            val fn = FnValue { argV =>
              val scope2 = scope1.letAll(args, argV)
              resFn(scope2)
            }
            Static(fn)
          case Lambda(caps, None, args, res) =>
            val resFn = loop(res)
            val capScoped = caps.map(loop).toVector
            Dynamic { scope =>
              val valuesInScope = capScoped.map(scoped => scoped(scope))
              // now we ignore the scope after reading from it
              val scope1 = Scope.capture(valuesInScope, scope.extra)

              // hopefully optimization/normalization has lifted anything
              // that doesn't depend on argV above this lambda
              FnValue { argV =>
                val scope2 = scope1.letAll(args, argV)
                resFn(scope2)
              }
            }
          case Lambda(caps, Some(name), args, res) =>
            val resFn = loop(res)
            val capScoped = caps.map(loop).toVector
            Dynamic { scope =>
              val valuesInScope = capScoped.map(scoped => scoped(scope))

              lazy val scope1: Scope = Scope
                .capture(valuesInScope)
                .let(name, Eval.later(fn))

              // hopefully optimization/normalization has lifted anything
              // that doesn't depend on argV above this lambda
              lazy val fn = FnValue { argV =>
                val scope2 = scope1.letAll(args, argV)
                resFn(scope2)
              }

              fn
            }
          case WhileExpr(cond, effect, result) =>
            val condF = boolExpr(cond)
            val effectF = loop(effect)

            // conditions are never static
            // or a previous optimization/normalization
            // has failed
            Dynamic { (scope: Scope) =>
              var c = condF(scope)
              while (c) {
                effectF(scope)
                c = condF(scope)
              }
              scope.muts(result.ident).get()
            }
          case Global(f, p, n) =>
            val res = resolve(f, p, n)

            // this has to be lazy because it could be
            // in this package, which isn't complete yet
            Dynamic((_: Scope) => res.value)
          case Local(b)        => Dynamic(_.locals(b).value)
          case LocalAnon(a)    => Dynamic(_.anon(a))
          case LocalAnonMut(m) =>
            Dynamic { s =>
              s.muts.get(m) match {
                case Some(v) => v.get()
                case None => sys.error(s"could not get: $m. ${s.debugString}")
              }
            }
          case ClosureSlot(idx) => Dynamic(_.slots(idx))
          case App(expr, args)  =>
            // TODO: App(lambda(while
            // can be optimized into a while
            // loop, but there isn't any prior optimization
            // that would do this.... maybe it should
            // be in Matchless
            val exprFn = loop(expr)
            val argsFn = args.traverse(loop(_))

            Applicative[Scoped].map2(exprFn, argsFn) { (fn, args) =>
              fn.applyAll(args)
            }
          case Let(localOrBind, value, in) =>
            val valueF = loop(value)
            val inF = loop(in)

            localOrBind match {
              case Right(b) =>
                inF.withScope { (scope: Scope) =>
                  val vv = Eval.now(valueF(scope))
                  scope.let(b, vv)
                }
              case Left(LocalAnon(l)) =>
                inF.withScope { (scope: Scope) =>
                  val vv = valueF(scope)
                  scope.copy(anon = scope.anon.updated(l, vv))
                }
            }
          case lm @ LetMut(_, _) =>
            val (anonMuts, in) = lm.flatten
            val inF = loop(in)
            Dynamic { (scope: Scope) =>
              // we make sure there is
              // a value that will show up
              // strange in tests,
              // for an optimization we could
              // avoid this
              val scope1 = scope.letMuts(anonMuts.iterator.map(_.ident))
              inF(scope1)
            }
          case Literal(lit) =>
            Static(Value.fromLit(lit))
          case If(cond, thenExpr, elseExpr) =>
            val condF = boolExpr(cond)
            val thenF = loop(thenExpr)
            val elseF = loop(elseExpr)

            // conditions are (basically) never static
            // or a previous optimization/normalization
            // has failed
            Dynamic { (scope: Scope) =>
              if (condF(scope)) thenF(scope)
              else elseF(scope)
            }
          case Always.SetChain(muts, expr) =>
            val values = muts.map { case (m, e) => (m, loop(e)) }
            val exprF = loop(expr)

            Dynamic { scope =>
              values.iterator.foreach { case (m, e) =>
                val ev = e(scope)
                scope.updateMut(m.ident, ev)
              }

              exprF(scope)
            }
          case Always(cond, expr) =>
            val condF = boolExpr(cond)
            val exprF = loop(expr)

            Applicative[Scoped].map2(condF, exprF) { (cond, res) =>
              assert(cond)
              res
            }
          case GetEnumElement(expr, v, idx, _) =>
            loop(expr).map { e =>
              val sum = e.asSum
              // we could assert e.asSum.variant == v
              // we can comment this out when bugs
              // are fixed
              assert(sum.variant == v)
              sum.value.get(idx)
            }

          case GetStructElement(expr, idx, sz) =>
            val loopFn = loop(expr)
            if (sz == 1) {
              // this is a newtype
              loopFn
            } else {
              loop(expr).map { p =>
                p.asProduct.get(idx)
              }
            }
          case PrevNat(expr) =>
            loop(expr).map { bv =>
              // TODO we could cache
              // small numbers to make this
              // faster
              val anyBI = bv.asExternal.toAny
              val bi = anyBI.asInstanceOf[BigInteger]
              ExternalValue(bi.subtract(BigInteger.ONE))
            }
          case cons: ConsExpr =>
            val c = makeCons(cons)
            Static(c)
        }

    }
  }
}
