package org.bykn.bosatsu

import cats.{Eval, Functor}
import java.math.BigInteger
import java.util.regex
import org.bykn.bosatsu.graph.Memoize.memoizeHashedConcurrent
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
      val toRegex: List[StrPart] => regex.Pattern =
        memoizeHashedConcurrent { parts: List[StrPart] =>
          val strPattern = parts.map {
            case StrPart.WildStr => "(?:.*)"
            case StrPart.IndexStr => "(.*)"
            case StrPart.LitStr(s) =>

              def wrap(s: String): String =
                if (s.isEmpty) ""
                else "\\Q" + s + "\\E"

              def escape(s: String): String = {
                val slashE = s.indexOf("\\E")
                if (slashE < 0) wrap(s)
                else {
                  val head = wrap(s.substring(0, slashE))
                  val quoteSlash = "\\\\E"
                  val tail = wrap(s.substring(slashE + 2))
                  head + quoteSlash + tail
                }
              }

              escape(s)
          }
          .mkString

          regex.Pattern.compile(strPattern)
        }

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

          case AndInt(ix1, ix2) =>
            val i1F = boolExpr(ix1)
            val i2F = boolExpr(ix2)

            { scope: Scope =>
              // we should be lazy
              i1F(scope) && i2F(scope)
            }

          case CheckVariant(enumV, idx, LocalAnonMut(res)) =>
            val argF = loop(enumV)

            { scope: Scope =>
              // TODO we should only be calling this on sums
              val input = argF(scope)

              val sum = input.asSum
              if (sum.variant == idx) {
                scope.updateMut(res, sum)
                true
              }
              else false
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
                    val vv = Eval.later(valueF(scope))
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
              // we should be able to remove this TODO
              //scope.muts.remove(l)
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
            val regexPat = toRegex(pat)
            loop(str).andThen { strV =>
              val arg = strV.asExternal.toAny.asInstanceOf[String]
              matchString(arg, regexPat, binds)
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

      def matchString(str: String, pat: regex.Pattern, binds: Int): SumValue = {
        val m = pat.matcher(str)
        if (m.matches()) {
          if (binds == 0) Value.True
          else {
            var idx = binds
            var prod: ProductValue = UnitValue
            while (0 < idx) {
              val item = m.group(idx)
              idx = idx - 1
              prod = ConsValue(ExternalValue(item), prod)
            }
            SumValue(1, prod)
          }
        }
        else {
          // this is (0, UnitValue)
          Value.False
        }
      }
    }
  }
}
