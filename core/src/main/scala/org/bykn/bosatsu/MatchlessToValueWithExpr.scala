package org.bykn.bosatsu

import cats.{Eval, Functor, Applicative}
import cats.evidence.Is
import java.math.BigInteger
import scala.collection.immutable.LongMap
import scala.collection.mutable.{LongMap => MLongMap}

import Identifier.Bindable
import Value._

import cats.implicits._

object MatchlessToValueWithExpr {
  import Matchless._

  sealed abstract class Leaf {
    def asValue: Value
  }

  object Leaf {
    case class Cons(
        head: ValueWithExpr,
        tail: ValueWithExpr
    ) extends Leaf {
      lazy val asValue = VList.Cons(head.eval, tail.eval)
    }

    case object LNil extends Leaf {
      lazy val asValue = VList.VNil
    }
  }

  sealed abstract class ValueWithExpr {
    def evalAndThen[A](fn: Value => A): A = this match {
      case ComputedValue(value)         => fn(value)
      case LazyValue(expression, scope) => ???
      case LeafValue(leaf)              => fn(leaf.asValue)
    }

    def eval: Value = evalAndThen[Value](identity(_))

    lazy val asLeaf: Leaf = this match {
      case cv @ ComputedValue(VList.Cons(head, tail)) =>
        Leaf.Cons(ComputedValue(head), ComputedValue(tail))
      case ComputedValue(_)             => Leaf.LNil
      case LazyValue(expression, scope) => ???
      case LeafValue(leaf)              => leaf
    }
  }

  final case class Scope(
      locals: Map[Bindable, Eval[ValueWithExpr]],
      anon: LongMap[ValueWithExpr],
      muts: MLongMap[ValueWithExpr]
  )
  case class LazyValue(
      expression: Expr,
      scope: Scope
  ) extends ValueWithExpr
  case class ComputedValue(value: Value) extends ValueWithExpr
  case class LeafValue(leaf: Leaf) extends ValueWithExpr

  def evalToValue(
      me: Expr,
      scope: Scope
  ): Value = LazyValue(me, scope).eval

  // reuse some cache structures across a number of calls
  def traverse[F[_]: Functor](
      me: F[Expr]
  )(
      resolve: (PackageName, Identifier) => Eval[ValueWithExpr]
  ): F[Eval[ValueWithExpr]] = {
    val env = new Impl.Env(resolve)
    val fns = Functor[F].map(me) { expr =>
      env.loop(expr)
    }
    // now that we computed all the functions, compute the values

    Functor[F].map(fns) { fn =>
      Eval.later(fn(Impl.Scope.empty))
    }
  }

  private[this] val zeroNat: ValueWithExpr = ComputedValue(
    ExternalValue(BigInteger.ZERO)
  )
  private[this] val succNat: ValueWithExpr = {
    def inc(v: Value): Value = {
      val bi = v.asExternal.toAny.asInstanceOf[BigInteger]
      ExternalValue(bi.add(BigInteger.ONE))
    }
    ComputedValue(FnValue(inc(_)))
  }

  def makeCons(c: ConsExpr): ValueWithExpr =
    c match {
      case MakeEnum(variant, arity, _) =>
        if (arity == 0) ComputedValue(SumValue(variant, UnitValue))
        else if (arity == 1) {
          ComputedValue(FnValue { v =>
            SumValue(variant, ConsValue(v, UnitValue))
          })
        } else
          ComputedValue(FnValue.curry(arity) { args =>
            val prod = ProductValue.fromList(args)
            SumValue(variant, prod)
          })
      case MakeStruct(arity) =>
        if (arity == 0) ComputedValue(UnitValue)
        else if (arity == 1) ComputedValue(FnValue.identity)
        else ComputedValue(FnValue.curry(arity)(ProductValue.fromList(_)))
      case ZeroNat => zeroNat
      case SuccNat => succNat
    }

  private object Impl {
    case object Uninitialized
    val uninit: Value = ExternalValue(Uninitialized)

    final case class Scope(
        locals: Map[Bindable, Eval[ValueWithExpr]],
        anon: LongMap[ValueWithExpr],
        muts: MLongMap[ValueWithExpr]
    ) {

      def let(b: Bindable, v: Eval[ValueWithExpr]): Scope =
        copy(locals = locals.updated(b, v))

      def updateMut(mutIdx: Long, v: ValueWithExpr): Unit = {
        assert(muts.contains(mutIdx))
        muts.put(mutIdx, v)
        ()
      }

      def capture(it: Iterable[Bindable]): Scope = {
        def loc(b: Bindable): Eval[ValueWithExpr] =
          locals.get(b) match {
            case Some(v) => v
            case None =>
              sys.error(
                s"couldn't find: $b in ${locals.keys.map(_.asString).toList}"
              )
          }

        Scope(
          it.iterator.map { b => (b, loc(b)) }.toMap,
          LongMap.empty,
          MLongMap()
        )
      }
    }

    object Scope {
      def empty(): Scope = Scope(Map.empty, LongMap.empty, MLongMap())
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
              case (Static(a), db) =>
                db.map(fn(a, _))
              case (da, Static(b)) =>
                da.map(fn(_, b))
              case (da, db) =>
                val fa = da.toFn
                val fb = db.toFn
                Dynamic { s => fn(fa(s), fb(s)) }
            }
          override def ap[A, B](sf: Scoped[A => B])(sa: Scoped[A]): Scoped[B] =
            map2(sf, sa)(_(_))
        }
    }

    class Env(resolve: (PackageName, Identifier) => Eval[ValueWithExpr]) {
      // evaluating boolExpr can mutate an existing value in muts
      private def boolExpr(ix: BoolExpr): Scoped[Boolean] =
        ix match {
          case EqualsLit(expr, lit) =>
            val litAny = lit.unboxToAny

            loop(expr).map { e =>
              e.evalAndThen(v => v.asExternal.toAny == litAny)
            }

          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.map { ve =>
                ve.evalAndThen(v => v.asExternal.toAny == BigInteger.ZERO)
              }
            else
              natF.map { ve =>
                ve.evalAndThen(v => v.asExternal.toAny != BigInteger.ZERO)
              }

          case TrueConst => Static(true)
          case And(ix1, ix2) =>
            boolExpr(ix1).and(boolExpr(ix2))

          case CheckVariant(enumV, idx, _, _) =>
            loop(enumV).map(_.evalAndThen(_.asSum.variant == idx))

          case SetMut(LocalAnonMut(mut), expr) =>
            val exprF = loop(expr)
            // this is always dynamic
            Dynamic { scope: Scope =>
              scope.updateMut(mut, exprF(scope))
              true
            }
          case MatchString(str, pat, binds) =>
            // do this before we evaluate the string
            binds match {
              case Nil =>
                // we have nothing to bind
                loop(str).map(v =>
                  v.evalAndThen { strV =>
                    val arg = strV.asExternal.toAny.asInstanceOf[String]
                    matchString(arg, pat, 0) != null
                  }
                )
              case _ =>
                val bary = binds.iterator.collect { case LocalAnonMut(id) =>
                  id
                }.toArray

                // this may be static
                val matchScope = loop(str).map(v =>
                  v.evalAndThen { str =>
                    val arg = str.asExternal.toAny.asInstanceOf[String]
                    matchString(arg, pat, bary.length)
                  }
                )
                // if we mutate scope, it has to be dynamic
                Dynamic { scope =>
                  val res = matchScope(scope)
                  if (res != null) {
                    var idx = 0
                    while (idx < bary.length) {
                      scope.updateMut(
                        bary(idx),
                        ComputedValue(ExternalValue(res(idx)))
                      )
                      idx = idx + 1
                    }
                    true
                  } else false
                }
            }

          case SearchList(LocalAnonMut(mutV), init, check, None) =>
            val initF = loop(init)
            val checkF = boolExpr(check)

            // TODO we could optimize
            // cases where checkF is Static(false) or Static(true)
            // but that is probably so rare I don't know if it will
            // help
            // e.g. [*_, _] should have been normalized
            // into [_, *_] which wouldn't trigger
            // this branch
            Dynamic { scope: Scope =>
              var currentList = initF(scope)
              var res = false
              while (currentList ne null) {
                currentList.asLeaf match {
                  case Leaf.Cons(_, tail) =>
                    scope.updateMut(mutV, currentList)
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else { currentList = tail }
                  case _ =>
                    currentList = null
                  // we don't match empty lists
                }
              }
              res
            }
          case SearchList(
                LocalAnonMut(mutV),
                init,
                check,
                Some(LocalAnonMut(left))
              ) =>
            val initF = loop(init)
            val checkF = boolExpr(check)

            // this is always dynamic
            Dynamic { scope: Scope =>
              var res = false
              var currentList = initF(scope)
              var leftList: Leaf = Leaf.LNil
              while (currentList ne null) {
                currentList.asLeaf match {
                  case Leaf.Cons(head, tail) =>
                    scope.updateMut(mutV, currentList)
                    scope.updateMut(left, LeafValue(leftList))
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else {
                      currentList = tail
                      leftList = Leaf.Cons(
                        head,
                        LeafValue(leftList)
                      )
                    }
                  case _ =>
                    currentList = null
                  // we don't match empty lists
                }
              }
              res
            }
        }

      def buildLoop(
          caps: List[Bindable],
          fnName: Bindable,
          arg0: Bindable,
          rest: List[Bindable],
          body: Scoped[ValueWithExpr]
      ): Scoped[ValueWithExpr] = {
        val argCount = rest.length + 1
        val argNames: Array[Bindable] = (arg0 :: rest).toArray
        if ((caps.lengthCompare(1) == 0) && (caps.head == fnName)) {
          // We only capture ourself and we put that in below
          val scope1 = Scope.empty()
          val fn = FnValue.curry(argCount) { allArgs =>
            var registers: List[Value] = allArgs

            // the registers are set up
            // when we recur, that is a continue on the loop,
            // we just update the registers and return null
            val continueFn = FnValue.curry(argCount) { continueArgs =>
              registers = continueArgs
              null
            }

            val scope2 = scope1.let(fnName, Eval.now(ComputedValue(continueFn)))

            var res: Value = null

            while (res eq null) {
              // read the registers into the environment
              var idx = 0
              var reg: List[Value] = registers
              var s: Scope = scope2
              while (idx < argCount) {
                val b = argNames(idx)
                val v = ComputedValue(reg.head)
                reg = reg.tail
                s = s.let(b, Eval.now(v))
                idx = idx + 1
              }
              res = body(s).eval
            }

            res
          }

          Static(ComputedValue(fn))
        } else {
          Dynamic { scope =>
            // TODO this maybe isn't helpful
            // it doesn't matter if the scope
            // is too broad for correctness.
            // It may make things go faster
            // if the caps are really small
            // or if we can GC things sooner.
            val scope1 = scope.capture(caps)

            ComputedValue(FnValue.curry(argCount) { allArgs =>
              var registers: List[Value] = allArgs

              // the registers are set up
              // when we recur, that is a continue on the loop,
              // we just update the registers and return null
              val continueFn = FnValue.curry(argCount) { continueArgs =>
                registers = continueArgs
                null
              }

              val scope2 =
                scope1.let(fnName, Eval.now(ComputedValue(continueFn)))

              var res: Value = null

              while (res eq null) {
                // read the registers into the environment
                var idx = 0
                var reg: List[Value] = registers
                var s: Scope = scope2
                while (idx < argCount) {
                  val b = argNames(idx)
                  val v = reg.head
                  reg = reg.tail
                  s = s.let(b, Eval.now(ComputedValue(v)))
                  idx = idx + 1
                }
                res = body(s).eval
              }

              res
            })
          }
        }
      }
      // the locals can be recusive, so we box into Eval for laziness
      def loop(me: Expr): Scoped[ValueWithExpr] =
        me match {
          case Lambda(caps, arg, res) =>
            val resFn = loop(res)

            if (caps.isEmpty) {
              // we can allocate once if there is no closure
              val scope1 = Scope.empty()
              val fn = FnValue { argV =>
                val scope2 = scope1.let(arg, Eval.now(ComputedValue(argV)))
                resFn(scope2).eval
              }
              Static(ComputedValue(fn))
            } else {
              Dynamic { scope =>
                val scope1 = scope.capture(caps)
                // hopefully optimization/normalization has lifted anything
                // that doesn't depend on argV above this lambda
                ComputedValue(FnValue { argV =>
                  val scope2 = scope1.let(arg, Eval.now(ComputedValue(argV)))
                  resFn(scope2).eval
                })
              }
            }
          case LoopFn(caps, thisName, argshead, argstail, body) =>
            val bodyFn = loop(body)

            buildLoop(caps, thisName, argshead, argstail, bodyFn)
          case Global(p, n) =>
            val res = resolve(p, n)

            // this has to be lazy because it could be
            // in this package, which isn't complete yet
            Dynamic { _: Scope => res.value }
          case Local(b)        => Dynamic(_.locals(b).value)
          case LocalAnon(a)    => Dynamic(_.anon(a))
          case LocalAnonMut(m) => Dynamic(_.muts(m))
          case App(expr, args) =>
            // TODO: App(LoopFn(..
            // can be optimized into a while
            // loop, but there isn't any prior optimization
            // that would do this.... maybe it should
            // be in Matchless
            val exprFn = loop(expr)
            val argsFn = args.traverse(loop(_))

            Applicative[Scoped].map2(exprFn, argsFn) { (fn, args) =>
              ComputedValue(fn.eval.applyAll(args.map(_.eval)))
            }
          case Let(Right((n1, r)), loopFn @ LoopFn(_, n2, _, _, _), Local(n3))
              if (n1 === n3) && (n1 === n2) && r.isRecursive =>
            // LoopFn already correctly handles recursion
            loop(loopFn)
          case Let(localOrBind, value, in) =>
            val valueF = loop(value)
            val inF = loop(in)

            localOrBind match {
              case Right((b, rec)) =>
                if (rec.isRecursive) {

                  inF.withScope { scope =>
                    // this is the only one that should
                    // use lazy/Eval.later
                    // we use it to tie the recursive knot
                    lazy val scope1: Scope =
                      scope.let(b, vv)

                    lazy val vv = Eval.later(valueF(scope1))

                    scope1
                  }
                } else {
                  inF.withScope { scope: Scope =>
                    val vv = Eval.now(valueF(scope))
                    scope.let(b, vv)
                  }
                }
              case Left(LocalAnon(l)) =>
                inF.withScope { scope: Scope =>
                  val vv = valueF(scope)
                  scope.copy(anon = scope.anon.updated(l, vv))
                }
            }
          case LetMut(LocalAnonMut(l), in) =>
            loop(in) match {
              case s @ Static(_) => s
              case Dynamic(inF) =>
                Dynamic { scope: Scope =>
                  // we make sure there is
                  // a value that will show up
                  // strange in tests,
                  // for an optimization we could
                  // avoid this
                  scope.muts.put(l, ComputedValue(uninit))
                  val res = inF(scope)
                  // now we can remove this from mutable scope
                  // we should be able to remove this
                  scope.muts.remove(l)
                  res
                }
            }
          case Literal(lit) =>
            Static(ComputedValue(Value.fromLit(lit)))
          case If(cond, thenExpr, elseExpr) =>
            val condF = boolExpr(cond)
            val thenF = loop(thenExpr)
            val elseF = loop(elseExpr)

            // conditions are (basically) never static
            // or a previous optimization/normalization
            // has failed
            Dynamic { scope: Scope =>
              if (condF(scope)) thenF(scope)
              else elseF(scope)
            }
          case Always(cond, expr) =>
            val condF = boolExpr(cond)
            val exprF = loop(expr)

            Applicative[Scoped].map2(condF, exprF) { (cond, res) =>
              assert(cond)
              res
            }
          case GetEnumElement(expr, v, idx, sz) =>
            loop(expr).map { e =>
              val sum = e.eval.asSum
              // we could assert e.asSum.variant == v
              // we can comment this out when bugs
              // are fixed
              assert(sum.variant == v)
              ComputedValue(sum.value.get(idx))
            }

          case GetStructElement(expr, idx, sz) =>
            val loopFn = loop(expr)
            if (sz == 1) {
              // this is a newtype
              loopFn
            } else {
              loop(expr).map { p =>
                ComputedValue(p.eval.asProduct.get(idx))
              }
            }
          case PrevNat(expr) =>
            loop(expr).map { bv =>
              // TODO we could cache
              // small numbers to make this
              // faster
              val anyBI = bv.eval.asExternal.toAny
              val bi = anyBI.asInstanceOf[BigInteger]
              ComputedValue(ExternalValue(bi.subtract(BigInteger.ONE)))
            }
          case cons: ConsExpr =>
            val c = makeCons(cons)
            Static(c)
        }

      private[this] val emptyStringArray: Array[String] = new Array[String](0)

      def matchString(
          str: String,
          pat: List[Matchless.StrPart],
          binds: Int
      ): Array[String] = {
        import Matchless.StrPart._

        val results =
          if (binds > 0) new Array[String](binds) else emptyStringArray

        def loop(
            offset: Int,
            pat: List[Matchless.StrPart],
            next: Int
        ): Boolean =
          pat match {
            case Nil => offset == str.length
            case LitStr(expect) :: tail =>
              val len = expect.length
              str.regionMatches(offset, expect, 0, len) && loop(
                offset + len,
                tail,
                next
              )
            case (h: Glob) :: tail =>
              tail match {
                case Nil =>
                  // we capture all the rest
                  if (h.capture) {
                    results(next) = str.substring(offset)
                  }
                  true
                case LitStr(expect) :: tail2 =>
                  val next1 = if (h.capture) next + 1 else next

                  var start = offset
                  var result = false
                  while (start >= 0) {
                    val candidate = str.indexOf(expect, start)
                    if (candidate >= 0) {
                      // we have to skip the current expect string
                      val check1 = loop(candidate + expect.length, tail2, next1)
                      if (check1) {
                        // this was a match, write into next if needed
                        if (h.capture) {
                          results(next) = str.substring(offset, candidate)
                        }
                        result = true
                        start = -1
                      } else {
                        // we couldn't match here, try just after candidate
                        start = candidate + 1
                      }
                    } else {
                      // no more candidates
                      start = -1
                    }
                  }
                  result
                case (_: Glob) :: _ =>
                  val n1 = if (h.capture) {
                    // this index gets the empty string
                    results(next) = ""
                    // we match the right side first, so this wild gets nothing
                    next + 1
                  } else next

                  loop(offset, tail, n1)
              }
          }

        if (loop(0, pat, 0)) results else null
      }
    }
  }
}
