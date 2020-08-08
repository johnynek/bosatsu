package org.bykn.bosatsu

import cats.{Eval, Functor}
import java.math.BigInteger
import scala.collection.immutable.LongMap
import scala.collection.mutable.{LongMap => MLongMap}

import Identifier.Bindable
import Value._

object MatchlessToValue {
  import Matchless._

  // reuse some cache structures across a number of calls
  def traverse[F[_]: Functor](me: F[Expr])(resolve: (PackageName, Identifier) => Eval[Value]): F[Eval[Value]] = {
    val env = new Impl.Env(resolve)
    val fns = Functor[F].map(me) { expr =>
      /*
      if (expr.isInstanceOf[Let]) {
        println("=" * 800)
        println(expr)
        println("=" * 800)
      }
      */
      env.loop(expr)
    }
    // now that we computed all the functions, compute the values

    Functor[F].map(fns) { fn =>
      Eval.later(fn(Impl.Scope.empty))
    }
  }

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

    class Env(resolve: (PackageName, Identifier) => Eval[Value]) {
      // evaluating boolExpr can mutate an existing value in muts
      private def boolExpr(ix: BoolExpr): Scope => Boolean =
        ix match {
          case EqualsLit(expr, lit) =>

            val litAny = lit.unboxToAny

            loop(expr).andThen { e =>
              val left = e
                .asExternal
                .toAny

              left == litAny
            }

          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.andThen { v =>
                v.asExternal.toAny == BigInteger.ZERO
              }
            else
              natF.andThen { v =>
                v.asExternal.toAny != BigInteger.ZERO
              }

          case TrueConst => Function.const(true)
          case And(TrueConst, ix2) => boolExpr(ix2)
          case And(ix1, TrueConst) => boolExpr(ix1)
          case And(ix1, ix2) =>
            val i1F = boolExpr(ix1)
            val i2F = boolExpr(ix2)

            { scope: Scope =>
              // we should be lazy
              if (i1F(scope)) i2F(scope)
              else false
            }

          case CheckVariant(enumV, idx) =>
            val argF = loop(enumV)

            { scope: Scope =>
              val input = argF(scope)
              val sum = input.asSum
              (sum.variant == idx)
            }

          case SetMut(LocalAnonMut(mut), expr) =>
            val exprF = loop(expr)

            { scope: Scope =>
              scope.updateMut(mut, exprF(scope))
              true
            }
          case SearchList(LocalAnonMut(mutV), init, check, None) =>
            val initF = loop(init)
            val checkF = boolExpr(check)

            { scope: Scope =>
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

            { scope: Scope =>
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

      def buildLoop(caps: List[Bindable], fnName: Bindable, arg0: Bindable, rest: List[Bindable], body: Scope => Value): Scope => Value = {
        val argCount = rest.length + 1
        val argNames: Array[Bindable] = (arg0 :: rest).toArray
        // we manually put this in below
        val capNoName = caps.filterNot(_ == fnName)

        { scope =>
          val scope1 = scope.capture(capNoName)

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
      // the locals can be recusive, so we box into Eval for laziness
      def loop(me: Expr): Scope => Value =
        me match {
          case Lambda(caps, arg, res) =>
            val resFn = loop(res)

            { scope =>
              val scope1 = scope.capture(caps)
              // hopefully optimization/normalization has lifted anything
              // that doesn't depend on argV above this lambda
              FnValue { argV =>
                val scope2 = scope1.let(arg, Eval.now(argV))
                resFn(scope2)
              }
            }
          case LoopFn(caps, thisName, argshead, argstail, body) =>
            val bodyFn = loop(body)

            buildLoop(caps, thisName, argshead, argstail, bodyFn)
          case Global(p, n) =>
            val res = resolve(p, n)

            // this has to be lazy, not Function.const which is eager
            { _: Scope => res.value }
          case Local(b) => { scope => scope.locals(b).value }
          case LocalAnon(a) => { scope => scope.anon(a) }
          case LocalAnonMut(m) => { scope => scope.muts(m) }
          case App(expr, args) =>
            // TODO: App(LoopFn(..
            // can be optimized into a while
            // loop, but there isn't any prior optimization
            // that would do this.... maybe it should
            // be in Matchless
            val exprFn = loop(expr)
            val argsFn = args.map(loop(_))

            { scope =>

              @annotation.tailrec
              def app(fn: Value, tail: List[Scope => Value]): Value =
                tail match {
                  case Nil => fn
                  case h :: tail =>
                    val next = fn.asFn(h(scope))
                    app(next, tail)
                }

              val h1 = exprFn(scope).asFn(argsFn.head(scope))
              app(h1, argsFn.tail)
            }
          case Let(localOrBind, value, in) =>
            val valueF = loop(value)
            val inF = loop(in)

            localOrBind match {
              case Right((b, rec)) =>
                if (rec.isRecursive) {

                  { scope =>
                    lazy val scope1: Scope =
                      scope.let(b, vv)

                    lazy val vv = Eval.later(valueF(scope1))

                    inF(scope1)
                  }
                }
                else {
                  { scope: Scope =>
                    val vv = Eval.now(valueF(scope))
                    inF(scope.let(b, vv))
                  }
                }
              case Left(LocalAnon(l)) =>
                { scope: Scope =>
                  val vv = valueF(scope)
                  val scope1 = scope.copy(anon = scope.anon.updated(l, vv))
                  inF(scope1)
                }
            }
          case LetMut(LocalAnonMut(l), in) =>
            val inF = loop(in)

            { scope: Scope =>
              scope.muts.put(l, uninit)
              val res = inF(scope)
              // now we can remove this from mutable scope
              // we should be able to remove this
              scope.muts.remove(l)
              res
            }
          case Literal(lit) =>
            val v = Value.fromLit(lit)
            Function.const(v)
          case If(cond, thenExpr, elseExpr) =>
            val condF = boolExpr(cond)
            val thenF = loop(thenExpr)
            val elseF = loop(elseExpr)

            { scope: Scope =>
              val cond = condF(scope)
              if (cond) thenF(scope)
              else elseF(scope)
            }
          case Always(cond, expr) =>
            val condF = boolExpr(cond)
            val exprF = loop(expr)

            { scope: Scope =>
              val cond = condF(scope)
              //assert(cond != 1)
              exprF(scope)
            }
          case MatchString(str, pat, binds) =>
            // do this before we evaluate the string
            loop(str).andThen { strV =>
              val arg = strV.asExternal.toAny.asInstanceOf[String]
              matchString(arg, pat, binds)
            }
          case GetEnumElement(expr, v, idx, sz) =>
            loop(expr).andThen { e =>
              val sum = e.asSum
              // we could assert e.asSum.variant == v
              // we can comment this out when bugs
              // are fixed
              //assert(sum.variant == v)
              sum.value.get(idx)
            }

          case GetStructElement(expr, idx, sz) =>
            val loopFn = loop(expr)
            if (sz == 1) {
              // this is a newtype
              loopFn
            }
            else {
              loop(expr).andThen { p =>
                p.asProduct.get(idx)
              }
            }
          case PrevNat(expr) =>
            loop(expr).andThen { bv =>
              val anyBI = bv.asExternal.toAny
              val bi = anyBI.asInstanceOf[BigInteger]
              ExternalValue(bi.subtract(BigInteger.ONE))
            }
          case cons: ConsExpr =>
            val c = makeCons(cons)
            Function.const(c)
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
}
