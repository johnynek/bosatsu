package org.bykn.bosatsu

import cats.{Eval, Functor, Id}
import java.math.BigInteger
import java.util.regex
import org.bykn.bosatsu.graph.Memoize.memoizeHashedConcurrent
import scala.collection.immutable.LongMap
import scala.collection.mutable.{LongMap => MLongMap}

import Identifier.Bindable
import Value._

object MatchlessToValue {
  import Matchless._

  def apply(me: Expr)(resolve: (PackageName, Identifier) => Eval[Value]): Eval[Value] =
    traverse[Id](me)(resolve)

  // reuse some cache structures across a number of calls
  def traverse[F[_]: Functor](me: F[Expr])(resolve: (PackageName, Identifier) => Eval[Value]): F[Eval[Value]] = {
    val env = new Impl.Env(resolve)
    val fns = Functor[F].map(me)(env.loop(_))
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
    final case class Scope(
      locals: Map[Bindable, Eval[Value]],
      anon: LongMap[Value],
      muts: MLongMap[Value]) {

      def let(b: Bindable, v: Eval[Value]): Scope =
        copy(locals = locals.updated(b, v))

      def capture(it: Iterable[Bindable]): Scope =
        Scope(
          it.iterator.map { b => (b, locals(b)) }.toMap,
            LongMap.empty,
            MLongMap())
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

      // evaluating intExpr can mutate an existing value in muts
      private def intExpr(ix: IntExpr): Scope => Int =
        ix match {
          case EqualsLit(expr, lit) =>

            val litAny = lit.unboxToAny

            loop(expr).andThen { e =>
              val left = e
                .asExternal
                .toAny

              if (left == litAny) 1
              else 0
            }
          case EqualsInt(iexpr, v) =>
            intExpr(iexpr).andThen { i =>
              if (i == v) 1
              else 0
            }
          case EqualsNat(nat, zeroOrSucc) =>
            val natF = loop(nat)

            if (zeroOrSucc.isZero)
              natF.andThen { v =>
                if (v.asExternal.toAny == BigInteger.ZERO) 1
                else 0
              }
            else
              natF.andThen { v =>
                if (v.asExternal.toAny != BigInteger.ZERO) 1
                else 0
              }

          case AndInt(ix1, ix2) =>
            val i1F = intExpr(ix1)
            val i2F = intExpr(ix2)

            { scope: Scope =>
              // we should be lazy
              if (i1F(scope) == 0) 0
              else i2F(scope)
            }

          case GetVariant(enumV) =>
            loop(enumV).andThen(_.asSum.variant)

          case SearchList(LocalAnonMut(mutV), init, check, None) =>
            val initF = loop(init)
            val checkF = intExpr(check)

            { scope: Scope =>
              var res = -1
              var currentList = initF(scope)
              while (res < 0) {
                scope.muts.put(mutV, currentList)
                // this can have a side effect, so it has to be in the loop
                res = checkF(scope)
                if (res == 0) {
                  currentList match {
                    case VList.Cons(_, tail) =>
                      currentList = tail
                      // continue on the tail
                      res = -1
                    case _ =>
                      // we are done, we never matched
                      // and res has been set to 0
                  }
                }
              }
              res
            }
          case SearchList(LocalAnonMut(mutV), init, check, Some(LocalAnonMut(left))) =>
            val initF = loop(init)
            val checkF = intExpr(check)

            { scope: Scope =>
              var res = -1
              var currentList = initF(scope)
              var leftList = VList.VNil
              while (res < 0) {
                scope.muts.put(mutV, currentList)
                scope.muts.put(left, leftList)
                // todo we have to redo this check
                // each time, we should have intExpr return Registers => Int
                // so we can just re-apply it on new registers
                res = checkF(scope)
                if (res == 0) {
                  currentList match {
                    case VList.Cons(head, tail) =>
                      currentList = tail
                      leftList = VList.Cons(head, leftList)
                      // continue on the tail
                      res = -1
                    case _ =>
                      // we are done, we never matched
                  }
                }
              }
              res
            }
        }

      def buildLoop(caps: List[Bindable], fnName: Bindable, arg0: Bindable, rest: List[Bindable], body: Scope => Value): Scope => Value = {
        val argCount = rest.length + 1
        val argNames: Array[Bindable] = (arg0 :: rest).toArray

        { scope =>
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

            def readRegisters(): Scope = {
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
              s
            }

            var res: Value = null
            while (res eq null) {
              // read the registers into the environment
              val scope = readRegisters()
              res = body(scope)
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
                val scope2 = scope.let(arg, Eval.now(argV))
                resFn(scope2)
              }
            }
          case LoopFn(caps, thisName, argshead, argstail, body) =>
            val bodyFn = loop(body)

            buildLoop(caps, thisName, argshead, argstail, bodyFn)
          case Global(p, n) =>
            val res = resolve(p, n)
            Function.const(res.value)
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
              def app(fn: Value => Value, head: Value, tail: List[Scope => Value]): Value = {
                val res = fn(head)
                tail match {
                  case Nil => res
                  case h :: tail =>
                    app(res.asFn, h(scope), tail)
                }
              }

              app(exprFn(scope).asFn, argsFn.head(scope), argsFn.tail)
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
              case Left(LocalAnonMut(l)) =>
                { scope: Scope =>
                  val vv = valueF(scope)
                  scope.muts.put(l, vv)
                  val res = inF(scope)
                  // now we can remove this from mutable scope
                  scope.muts.remove(l)
                  res
                }
            }
          case Literal(lit) =>
            val v = Value.fromLit(lit)
            Function.const(v)
          case If(cond, thenExpr, elseExpr) =>
            val condF = intExpr(cond)
            val thenF = loop(thenExpr)
            val elseF = loop(elseExpr)

            { scope: Scope =>
              val cond = condF(scope)
              if (cond == 0) elseF(scope)
              else thenF(scope)
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
              e.asSum.value.get(idx)
            }

          case GetStructElement(expr, idx, sz) =>
            loop(expr).andThen { p =>
              p.asProduct.get(idx)
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
