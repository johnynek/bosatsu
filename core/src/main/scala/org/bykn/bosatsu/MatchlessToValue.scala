package org.bykn.bosatsu

import cats.{Eval, Foldable, Functor, Applicative}
import cats.data.NonEmptyList
import cats.evidence.Is
import java.math.BigInteger
import scala.collection.immutable.LongMap
import scala.collection.mutable.{LongMap => MLongMap}

import Identifier.Bindable
import Value._

import cats.implicits._

object MatchlessToValue {
  import Matchless._

  // reuse some cache structures across a number of calls
  def traverse[F[_]: Functor](me: F[Expr])(resolve: (PackageName, Identifier) => Eval[Value]): F[Eval[Value]] = {
    val env = new Impl.Env(resolve)
    val fns = Functor[F].map(me) { expr =>
      env.loop(expr)
    }
    // now that we computed all the functions, compute the values

    Functor[F].map(fns) { fn =>
      Eval.later(fn(Impl.Scope.empty()))
    }
  }

  private[this] val zeroNat: Value = ExternalValue(BigInteger.ZERO)
  private[this] val succNat: Value = {
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
          FnValue { case NonEmptyList(v, _) => SumValue(variant, ConsValue(v, UnitValue)) }
        }
        else
          // arity > 1
          FnValue { args =>
            val prod = ProductValue.fromList(args.toList)
            SumValue(variant, prod)
          }
      case MakeStruct(arity) =>
        if (arity == 0) UnitValue
        else if (arity == 1) FnValue.identity
        else FnValue { args =>
          ProductValue.fromList(args.toList)
        }
      case ZeroNat => zeroNat
      case SuccNat => succNat
    }

  private object Impl {
    case object Uninitialized
    val uninit: Value = ExternalValue(Uninitialized)

    final case class Scope(
      locals: Map[Bindable, Eval[Value]],
      anon: LongMap[Value],
      muts: MLongMap[Value],
      slots: Vector[Eval[Value]]) {

      def let(b: Bindable, v: Eval[Value]): Scope =
        copy(locals = locals.updated(b, v))

      def letAll[F[_]: Foldable](bs: F[(Bindable, Value)]): Scope =
        copy(locals = bs.foldLeft(locals) { case (locals, (b, v)) => locals.updated(b, Eval.now(v)) })

      def updateMut(mutIdx: Long, v: Value): Unit = {
        assert(muts.contains(mutIdx))
        muts.put(mutIdx, v)
        ()
      }

      def capture(it: Vector[Eval[Value]]): Scope =
        Scope(
          locals,
          LongMap.empty,
          MLongMap(),
          it)
    }

    object Scope {
      def empty(): Scope = Scope(Map.empty, LongMap.empty, MLongMap(), Vector.empty)
    }

    sealed abstract class Scoped[A] {
      def apply(s: Scope): A
      def map[B](fn: A => B): Scoped[B]
      def and(that: Scoped[Boolean])(implicit ev: Is[A, Boolean]): Scoped[Boolean] = {
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
            map2(sf, sa)(_(_))
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
          case MatchString(str, pat, binds) =>
            // do this before we evaluate the string
            binds match {
              case Nil =>
                // we have nothing to bind
                loop(str).map { strV =>
                  val arg = strV.asExternal.toAny.asInstanceOf[String]
                  matchString(arg, pat, 0) != null
                }
              case _ =>
                val bary = binds.iterator.collect { case LocalAnonMut(id) => id }.toArray

                // this may be static
                val matchScope = loop(str).map { str =>
                  val arg = str.asExternal.toAny.asInstanceOf[String]
                  matchString(arg, pat, bary.length)
                }
                // if we mutate scope, it has to be dynamic
                Dynamic { scope =>
                  val res = matchScope(scope)
                  if (res != null) {
                    var idx = 0
                    while (idx < bary.length) {
                      scope.updateMut(bary(idx), ExternalValue(res(idx)))
                      idx = idx + 1
                    }
                    true
                  }
                  else false
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
            Dynamic { (scope: Scope) =>
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
            Dynamic { (scope: Scope) =>
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

      def buildLoop(caps: Vector[Scoped[Value]], fnName: Bindable, args: NonEmptyList[Bindable], body: Scoped[Value]): Scoped[Value] = {
        val argCount = args.length
        val argNames: Array[Bindable] = args.toList.toArray
        if (caps.isEmpty) {
          // We only capture ourself and we put that in below
          val scope1 = Scope.empty()
          val fn = FnValue { allArgs =>
            var registers: NonEmptyList[Value] = allArgs

            // the registers are set up
            // when we recur, that is a continue on the loop,
            // we just update the registers and return null
            val continueFn = FnValue { continueArgs =>
              registers = continueArgs
              null
            }

            val scope2 = scope1.let(fnName, Eval.now(continueFn))

            var res: Value = null

            while (res eq null) {
              // read the registers into the environment
              var idx = 0
              var reg: List[Value] = registers.toList
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
            val scope1 = scope.capture(caps.map { s => Eval.later(s(scope)) })

            FnValue { allArgs =>
              var registers: NonEmptyList[Value] = allArgs

              // the registers are set up
              // when we recur, that is a continue on the loop,
              // we just update the registers and return null
              val continueFn = FnValue { continueArgs =>
                registers = continueArgs
                null
              }

              val scope2 = scope1.let(fnName, Eval.now(continueFn))

              var res: Value = null

              while (res eq null) {
                // read the registers into the environment
                var idx = 0
                var reg: List[Value] = registers.toList
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
          case Lambda(caps, args, res) =>
            val resFn = loop(res)

            if (caps.isEmpty) {
              // we can allocate once if there is no closure
              val scope1 = Scope.empty()
              val fn = FnValue { argV =>
                val scope2 = scope1.letAll(args.zip(argV))
                resFn(scope2)
              }
              Static(fn)
            }
            else {
              val capScoped = caps.map(loop).toVector
              Dynamic { scope =>
                val scope1 = scope.capture(capScoped.map { scoped => Eval.later(scoped(scope)) })
                // hopefully optimization/normalization has lifted anything
                // that doesn't depend on argV above this lambda
                FnValue { argV =>
                  val scope2 = scope1.letAll(args.zip(argV))
                  resFn(scope2)
                }
              }
            }
          case LoopFn(caps, thisName, args, body) =>
            val bodyFn = loop(body)

            buildLoop(caps.map(loop).toVector, thisName, args, bodyFn)
          case Global(p, n) =>
            val res = resolve(p, n)

            // this has to be lazy because it could be
            // in this package, which isn't complete yet
            Dynamic { (_: Scope) => res.value }
          case Local(b) => Dynamic(_.locals(b).value)
          case LocalAnon(a) => Dynamic(_.anon(a))
          case LocalAnonMut(m) => Dynamic(_.muts(m))
          case ClosureSlot(idx) => Dynamic(_.slots(idx).value)
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
          case Let(Right((n1, r)), loopFn@LoopFn(_, n2, _, _), Local(n3)) if (n1 === n3) && (n1 === n2) && r.isRecursive =>
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
                }
                else {
                  inF.withScope { (scope: Scope) =>
                    val vv = Eval.now(valueF(scope))
                    scope.let(b, vv)
                  }
                }
              case Left(LocalAnon(l)) =>
                inF.withScope { (scope: Scope) =>
                  val vv = valueF(scope)
                  scope.copy(anon = scope.anon.updated(l, vv))
                }
            }
          case LetMut(LocalAnonMut(l), in) =>
            loop(in) match {
              case s@Static(_) => s
              case Dynamic(inF) =>
                Dynamic { (scope: Scope) =>
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

            // conditions are (basically) never static
            // or a previous optimization/normalization
            // has failed
            Dynamic { (scope: Scope) =>
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

    }
    
    private[this] val emptyStringArray: Array[String] = new Array[String](0)
    def matchString(
      str: String,
      pat: List[StrPart],
      binds: Int): Array[String] = {
      import Matchless.StrPart._

      val strLen = str.length()
      val results = if (binds > 0) new Array[String](binds) else emptyStringArray

      def loop(offset: Int, pat: List[StrPart], next: Int): Boolean =
        pat match {
          case Nil => offset == strLen
          case LitStr(expect) :: tail =>
            val len = expect.length
            str.regionMatches(offset, expect, 0, len) && loop(offset + len, tail, next)
          case (c: CharPart) :: tail =>
            try {
              val nextOffset = str.offsetByCodePoints(offset, 1)
              val n =
                if (c.capture) {
                  results(next) = str.substring(offset, nextOffset)
                  next + 1
                }
                else next

              loop(nextOffset, tail, n)
            }
            catch {
              case _: IndexOutOfBoundsException => false
            }
          case (h: Glob) :: tail =>
            tail match {
              case Nil =>
                // we capture all the rest
                if (h.capture) {
                  results(next) = str.substring(offset)
                }
                true
              case rest @ ((_: CharPart) :: _) =>
                val matchableSizes = MatchSize[List](rest)

                def canMatch(off: Int): Boolean =
                  matchableSizes.canMatch(str.codePointCount(off, strLen))

                // (.*)(.)tail2
                // this is a naive algorithm that just
                // checks at all possible later offsets
                // a smarter algorithm could see if there
                // are Lit parts that can match or not
                var matched = false
                var off1 = offset
                val n1 = if (h.capture) (next + 1) else next
                while (!matched && (off1 < strLen)) {
                  matched = canMatch(off1) && loop(off1, rest, n1)
                  if (!matched) {
                    off1 = off1 + Character.charCount(str.codePointAt(off1))
                  }
                }

                matched && {
                  if (h.capture) {
                    results(next) = str.substring(offset, off1)
                  }
                  true 
                }
              case LitStr(expect) :: tail2 =>
                val next1 = if (h.capture) next + 1 else next

                val matchableSizes = MatchSize(tail2)

                def canMatch(off: Int): Boolean =
                  matchableSizes.canMatchUtf16Count(strLen - off)

                var start = offset
                var result = false
                while (start >= 0) {
                  val candidate = str.indexOf(expect, start)
                  if (candidate >= 0) {
                    // we have to skip the current expect string
                    val nextOff = candidate + expect.length
                    val check1 = canMatch(nextOff) && loop(nextOff, tail2, next1)
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
                      start = candidate + Character.charCount(str.codePointAt(candidate))
                    }
                  }
                  else {
                    // no more candidates
                    start = -1
                  }
                }
                result
              // $COVERAGE-OFF$
              case (_: Glob) :: _ =>
                // this should be an error at compile time since it
                // is never meaningful to have two adjacent globs
                sys.error(s"invariant violation, adjacent globs: $pat")
              // $COVERAGE-ON$
            }
        }

      if (loop(0, pat, 0)) results else null
    }
  }
}
