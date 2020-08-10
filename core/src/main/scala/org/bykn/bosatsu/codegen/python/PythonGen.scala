package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.{PackageName, Identifier, Matchless}
import cats.Monad
import cats.data.NonEmptyList
import org.typelevel.paiges.Doc

//import Identifier.Bindable
import Matchless._

object PythonGen {

  sealed abstract class PythonExpr {
    def toDoc: Doc
  }

  sealed abstract class PythonBoolExpr {
    def toDoc: Doc
  }

  object PythonExpr {
    def ifExpr(conds: NonEmptyList[(PythonExpr, PythonExpr)], elseCond: PythonExpr): PythonExpr = ???
  }

  sealed abstract class Env[+A]
  object Env {
    implicit def envMonad: Monad[Env] = ???

    def render(env: Env[PythonExpr]): String = ???
  }

  def apply(me: Expr)(resolve: (PackageName, Identifier) => Env[PythonExpr]): Env[PythonExpr] = {
    val ops = new Impl.Ops(resolve)
    ops.loop(me)
  }

  private object Impl {
    class Ops(resolve: (PackageName, Identifier) => Env[PythonExpr]) {
      def makeCons(ce: ConsExpr): Env[PythonExpr] = ???
      def boolExpr(ix: BoolExpr): Env[PythonBoolExpr] =
        ix match {
          case EqualsLit(expr, lit) => ???
          case EqualsNat(nat, zeroOrSucc) => ???
          case TrueConst => ???
          case And(ix1, ix2) => ???
          case CheckVariant(enumV, idx) => ???
          case SetMut(LocalAnonMut(mut), expr) => ???
          case SearchList(LocalAnonMut(mutV), init, check, optLeft) => ???
        }

      def loop(expr: Expr): Env[PythonExpr] =
        expr match {
          case Lambda(caps, arg, res) => ???
          case LoopFn(caps, thisName, argshead, argstail, body) => ???
          case Global(p, n) => ???
          case Local(b) => ???
          case LocalAnon(a) => ???
          case LocalAnonMut(m) => ???
          case App(expr, args) => ???
          case Let(localOrBind, value, in) => ???
          case LetMut(LocalAnonMut(l), in) => ???
          case Literal(lit) => ???
          case If(cond, thenExpr, elseExpr) => ???
          case Always(cond, expr) => ???
          case MatchString(str, pat, binds) => ???
          case GetEnumElement(expr, v, idx, sz) => ???
          case GetStructElement(expr, idx, sz) => ???
          case PrevNat(expr) => ???
          case cons: ConsExpr => ???
        }
    }
  }

/*
  private[this] val zeroNat: Value = ExternalValue(BigInteger.ZERO)
  private[this] val succNat: Value = {
    def inc(v: Value): Value = {
      val bi = v.asExternal.toAny.asInstanceOf[BigInteger]
      ExternalValue(bi.add(BigInteger.ONE))
    }
    FnValue(inc(_))
  }

  def makeCons(c: ConsExpr): Value =
    c match {
      case MakeEnum(variant, arity) =>
        if (arity == 0) SumValue(variant, UnitValue)
        else if (arity == 1) {
          FnValue { v => SumValue(variant, ConsValue(v, UnitValue)) }
        }
        else
          FnValue.curry(arity) { args =>
            val prod = ProductValue.fromList(args)
            SumValue(variant, prod)
          }
      case MakeStruct(arity) =>
        if (arity == 0) UnitValue
        else if (arity == 1) FnValue.identity
        else FnValue.curry(arity)(ProductValue.fromList(_))
      case ZeroNat => zeroNat
      case SuccNat => succNat
    }

  private object Impl {
    case object Uninitialized
    val uninit: Value = ExternalValue(Uninitialized)

    final case class Scope(
      locals: Map[Bindable, Eval[Value]],
      anon: LongMap[Value],
      muts: MLongMap[Value]) {

      def let(b: Bindable, v: Eval[Value]): Scope =
        copy(locals = locals.updated(b, v))

      def updateMut(mutIdx: Long, v: Value): Unit = {
        assert(muts.contains(mutIdx))
        muts.put(mutIdx, v)
        ()
      }

      def capture(it: Iterable[Bindable]): Scope = {
        def loc(b: Bindable): Eval[Value] =
          locals.get(b) match {
            case Some(v) => v
            case None => sys.error(s"couldn't find: $b in ${locals.keys.map(_.asString).toList}")
          }

        Scope(
          it.iterator.map { b => (b, loc(b)) }.toMap,
            LongMap.empty,
            MLongMap())
      }
    }

    object Scope {
      def empty(): Scope = Scope(Map.empty, LongMap.empty, MLongMap())
    }

    sealed abstract class Scoped[A] {
      def apply(s: Scope): A
      def map[B](fn: A => B): Scoped[B]
      // lazily evaluate that (only if it is static or this is false)
      def and(that: Scoped[Boolean])(implicit ev: Is[A, Boolean]): Scoped[Boolean]
      def toFn: Scope => A

      def withScope(ws: Scope => Scope): Scoped[A]
    }
    case class Dynamic[A](toFn: Scope => A) extends Scoped[A] {
      def apply(s: Scope) = toFn(s)
      def map[B](fn: A => B): Scoped[B] = Dynamic(toFn.andThen(fn))
      def and(that: Scoped[Boolean])(implicit ev: Is[A, Boolean]): Scoped[Boolean] =
        that match {
          case Static(b) =>
            if (b) ev.substitute[Dynamic](this)
            else {
              // we don't need to evaluate side-effects in
              // and chains that end in static false, so we can bail out sooner
              that
            }
          case Dynamic(thatFn) =>
            val fn = ev.substitute[Scope => ?](toFn)
            Dynamic { scope =>
              if (fn(scope)) thatFn(scope)
              else false
            }
        }
      def withScope(ws: Scope => Scope): Scoped[A] =
        Dynamic(ws.andThen(toFn))
    }
    case class Static[A](value: A) extends Scoped[A] {
      def apply(s: Scope) = value
      def map[B](fn: A => B): Scoped[B] = Static(fn(value))
      def and(that: Scoped[Boolean])(implicit ev: Is[A, Boolean]): Scoped[Boolean] = {
        val thisB = ev.substitute[Static](this)
        if (thisB.value) that
        else thisB
      }

      def withScope(ws: Scope => Scope): Scoped[A] = this
      def toFn = Function.const(value)
    }

    object Scoped {
      implicit val matchlessScopedApplicative: Applicative[Scoped] =
        new Applicative[Scoped] {
          def pure[A](a: A): Scoped[A] = Static(a)
          override def map[A, B](aa: Scoped[A])(fn: A => B): Scoped[B] =
            aa.map(fn)
          override def map2[A, B, C](aa: Scoped[A], ab: Scoped[B])(fn: (A, B) => C): Scoped[C] =
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
            (sf, sa) match {
              case (Static(fn), Static(a)) => Static(fn(a))
              case (Static(fn), db) =>
                db.map(fn)
              case (da, Static(a)) =>
                da.map(_(a))
              case (df, da) =>
                val fn = df.toFn
                val a = da.toFn
                Dynamic { s => fn(s).apply(a(s)) }
            }
        }
    }

    class Env(resolve: (PackageName, Identifier) => Eval[Value]) {
      // evaluating boolExpr can mutate an existing value in muts
      private def boolExpr(ix: BoolExpr): Scoped[Boolean] =
        ix match {
          case EqualsLit(expr, lit) =>

            val litAny = lit.unboxToAny

            loop(expr).map { e =>
              e.asExternal.toAny == litAny
            }

          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.map { v =>
                v.asExternal.toAny == BigInteger.ZERO
              }
            else
              natF.map { v =>
                v.asExternal.toAny != BigInteger.ZERO
              }

          case TrueConst => Static(true)
          case And(TrueConst, ix2) => boolExpr(ix2)
          case And(ix1, TrueConst) => boolExpr(ix1)
          case And(ix1, ix2) =>
            boolExpr(ix1).and(boolExpr(ix2))

          case CheckVariant(enumV, idx) =>
            loop(enumV).map(_.asSum.variant == idx)

          case SetMut(LocalAnonMut(mut), expr) =>
            val exprF = loop(expr)
            // this is always dynamic
            Dynamic { scope: Scope =>
              scope.updateMut(mut, exprF(scope))
              true
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
                currentList match {
                  case nonempty@VList.Cons(_, tail) =>
                    scope.updateMut(mutV, nonempty)
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
          case SearchList(LocalAnonMut(mutV), init, check, Some(LocalAnonMut(left))) =>
            val initF = loop(init)
            val checkF = boolExpr(check)

            // this is always dynamic
            Dynamic { scope: Scope =>
              var res = false
              var currentList = initF(scope)
              var leftList = VList.VNil
              while (currentList ne null) {
                currentList match {
                  case nonempty@VList.Cons(head, tail) =>
                    scope.updateMut(mutV, nonempty)
                    scope.updateMut(left, leftList)
                    res = checkF(scope)
                    if (res) { currentList = null }
                    else {
                      currentList = tail
                      leftList = VList.Cons(head, leftList)
                    }
                  case _ =>
                    currentList = null
                    // we don't match empty lists
                }
              }
              res
            }
        }

      def buildLoop(caps: List[Bindable], fnName: Bindable, arg0: Bindable, rest: List[Bindable], body: Scoped[Value]): Scoped[Value] = {
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

            val scope2 = scope1.let(fnName, Eval.now(continueFn))

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
                s = s.let(b, Eval.now(v))
                idx = idx + 1
              }
              res = body(s)
            }

            res
          }

          Static(fn)
        }
        else {
          Dynamic { scope =>
            // TODO this maybe isn't helpful
            // it doesn't matter if the scope
            // is too broad for correctness.
            // It may make things go faster
            // if the caps are really small
            // or if we can GC things sooner.
            val scope1 = scope.capture(caps)

            FnValue.curry(argCount) { allArgs =>
              var registers: List[Value] = allArgs

              // the registers are set up
              // when we recur, that is a continue on the loop,
              // we just update the registers and return null
              val continueFn = FnValue.curry(argCount) { continueArgs =>
                registers = continueArgs
                null
              }

              val scope2 = scope1.let(fnName, Eval.now(continueFn))

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
                  s = s.let(b, Eval.now(v))
                  idx = idx + 1
                }
                res = body(s)
              }

              res
            }
          }
        }
      }
      // the locals can be recusive, so we box into Eval for laziness
      def loop(me: Expr): Scoped[Value] =
        me match {
          case Lambda(caps, arg, res) =>
            val resFn = loop(res)

            if (caps.isEmpty) {
              // we can allocate once if there is no closure
              val scope1 = Scope.empty()
              val fn = FnValue { argV =>
                val scope2 = scope1.let(arg, Eval.now(argV))
                resFn(scope2)
              }
              Static(fn)
            }
            else {
              Dynamic { scope =>
                val scope1 = scope.capture(caps)
                // hopefully optimization/normalization has lifted anything
                // that doesn't depend on argV above this lambda
                FnValue { argV =>
                  val scope2 = scope1.let(arg, Eval.now(argV))
                  resFn(scope2)
                }
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
          case Local(b) => Dynamic(_.locals(b).value)
          case LocalAnon(a) => Dynamic(_.anon(a))
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
              fn.applyAll(args)
            }
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
                }
                else {
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
              case s@Static(_) => s
              case Dynamic(inF) =>
                Dynamic { scope: Scope =>
                  // we make sure there is
                  // a value that will show up
                  // strange in tests,
                  // for an optimization we could
                  // avoid this
                  scope.muts.put(l, uninit)
                  val res = inF(scope)
                  // now we can remove this from mutable scope
                  // we should be able to remove this
                  scope.muts.remove(l)
                  res
                }
            }
          case Literal(lit) =>
            Static(Value.fromLit(lit))
          case If(cond, thenExpr, elseExpr) =>
            val condF = boolExpr(cond)
            val thenF = loop(thenExpr)
            val elseF = loop(elseExpr)

            condF match {
              case Static(b) =>
                if (b) thenF else elseF
              case Dynamic(cfn) =>
                Dynamic { scope: Scope =>
                  val cond = cfn(scope)
                  if (cond) thenF(scope)
                  else elseF(scope)
                }
            }
          case Always(cond, expr) =>
            val condF = boolExpr(cond)
            val exprF = loop(expr)

            condF match {
              case Static(b) =>
                assert(b)
                exprF
              case dynC =>
                Applicative[Scoped].map2(dynC, exprF) { (cond, res) =>
                  assert(cond)
                  res
                }
            }
          case MatchString(str, pat, binds) =>
            // do this before we evaluate the string
            loop(str).map { strV =>
              val arg = strV.asExternal.toAny.asInstanceOf[String]
              matchString(arg, pat, binds)
            }
          case GetEnumElement(expr, v, idx, sz) =>
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
            }
            else {
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

      def matchString(str: String, pat: List[Matchless.StrPart], binds: Int): SumValue = {
        import Matchless.StrPart._

        val results = new Array[String](binds)

        def loop(offset: Int, pat: List[Matchless.StrPart], next: Int): Boolean =
          pat match {
            case Nil => offset == str.length
            case LitStr(expect) :: tail =>
              val len = expect.length
              str.regionMatches(offset, expect, 0, len) && loop(offset + len, tail, next)
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
                      }
                      else {
                        // we couldn't match here, try just after candidate
                        start = candidate + 1
                      }
                    }
                    else {
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

        if (loop(0, pat, 0)) {
          var idx = binds - 1
          var prod: ProductValue = UnitValue
          while (0 <= idx) {
            val item = results(idx)
            idx = idx - 1
            prod = ConsValue(ExternalValue(item), prod)
          }
          SumValue(1, prod)
        }
        else {
          // this is (0, UnitValue)
          Value.False
        }
      }
    }
  }
  */
}
