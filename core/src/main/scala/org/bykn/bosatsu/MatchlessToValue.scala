package org.bykn.bosatsu

import cats.Eval
import java.math.BigInteger
import java.util.regex
import org.bykn.bosatsu.graph.Memoize.memoizeHashedConcurrent
import scala.collection.immutable.LongMap
import scala.collection.mutable.{LongMap => MLongMap, Map => MMap}

import Identifier.Bindable
import Value._

object MatchlessToValue {
  import Matchless._

  def apply(me: Expr)(resolve: (PackageName, Identifier) => Value): Value = {
    val scope = new Scope(resolve)
    scope.loop(me, Map.empty, LongMap.empty)
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

  // this should be scoped to allow GC
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

  private class Scope(resolve: (PackageName, Identifier) => Value) {
    val muts: MLongMap[Value] = MLongMap()

    def newScope(): Scope = new Scope(resolve)

    // evaluating intExpr can mutate an existing value in muts
    private def intExpr(ix: IntExpr, locals: Map[Bindable, Eval[Value]], anon: LongMap[Value]): Int =
      ix match {
        case EqualsLit(expr, lit) =>
          val left = loop(expr, locals, anon)
            .asExternal
            .toAny

          if (left == lit.unboxToAny) 1
          else 0
        case EqualsInt(iexpr, v) =>
          if (intExpr(iexpr, locals, anon) == v) 1
          else 0
        case EqualsNat(nat, zeroOrSucc) =>
          val natBI = loop(nat, locals, anon).asExternal.toAny
          val eqZero = natBI == BigInteger.ZERO
          val isZero = zeroOrSucc.isZero
          val zeroTrue = isZero && eqZero
          val oneTrue = !(isZero || eqZero)
          if (zeroTrue || oneTrue) 1
          else 0

        case AndInt(ix1, ix2) =>
          val i1 = intExpr(ix1, locals, anon)
          // we should be lazy
          if (i1 == 0) 0
          else intExpr(ix2, locals, anon)
        case GetVariant(enumV) =>
          loop(enumV, locals, anon).asSum.variant

        case SearchList(LocalAnonMut(mutV), init, check, None) =>
          var res = -1
          var currentList = loop(init, locals, anon)
          while (res < 0) {
            muts.put(mutV, currentList)
            // todo we have to redo this check
            // each time, we should have intExpr return Registers => Int
            // so we can just re-apply it on new registers
            res = intExpr(check, locals, anon)
            if (res == 0) {
              currentList match {
                case VList.Cons(_, tail) =>
                  currentList = tail
                case _ =>
                  // we are done, we never matched
              }
            }
          }
          res
        case SearchList(LocalAnonMut(mutV), init, check, Some(LocalAnonMut(left))) =>
          var res = -1
          var currentList = loop(init, locals, anon)
          var leftList = VList.VNil
          while (res < 0) {
            muts.put(mutV, currentList)
            muts.put(left, leftList)
            // todo we have to redo this check
            // each time, we should have intExpr return Registers => Int
            // so we can just re-apply it on new registers
            res = intExpr(check, locals, anon)
            if (res == 0) {
              currentList match {
                case VList.Cons(head, tail) =>
                  currentList = tail
                  leftList = VList.Cons(head, leftList)
                case _ =>
                  // we are done, we never matched
              }
            }
          }
          res
      }

    // the locals can be recusive, so we box into Eval for laziness
    def loop(me: Expr, locals: Map[Bindable, Eval[Value]], anon: LongMap[Value]): Value =
      me match {
        case Lambda(caps, arg, res) =>
          val capLocal = caps.iterator.map { b => (b, locals(b)) }.toMap
          FnValue { argV =>
            // hopefully optimization/normalization has lifted anything
            // that doesn't depend on argV above this lambda
            newScope().loop(res, capLocal, LongMap.empty)
          }
        case LoopFn(caps, thisName, argshead, argstail, body) =>
          val capLocal = caps.iterator.map { b => (b, locals(b)) }.toMap
          FnValue.curry(argstail.length + 1) { allArgs =>
            val registers = MMap[Bindable, Value]()

            def setRegisters(values: List[Value]): Unit = {
              registers.put(argshead, values.head)
              var mutArgsTail = argstail
              var mutArgs = values.tail
              while (mutArgsTail.nonEmpty) {
                registers.put(mutArgsTail.head, mutArgs.head)
                mutArgsTail = mutArgsTail.tail
                mutArgs = mutArgs.tail
              }
            }

            // the registers are set up
            // when we recur, that is a continue on the loop,
            // we just update the registers and return null
            val continueFn = FnValue.curry(argstail.length + 1) { continueArgs =>
              setRegisters(continueArgs)
              null
            }

            val locals1 = locals.updated(thisName, Eval.now(continueFn))

            def readRegisters(): Map[Bindable, Eval[Value]] = {
              val it = registers.iterator
              var loc = locals1
              while (it.hasNext) {
                val (b, v) = it.next()
                loc = loc.updated(b, Eval.now(v))
              }
              loc
            }

            var res: Value = null
            val scope = newScope()
            while (res eq null) {
              // read the registers into the environment
              val loc = readRegisters()
              res = scope.loop(body, loc, LongMap.empty)
            }

            res
          }

        case Global(p, n) => resolve(p, n)
        case Local(b) => locals(b).value
        case LocalAnon(a) => anon(a)
        case LocalAnonMut(m) => muts(m)
        case App(expr, args) =>
          @annotation.tailrec
          def app(fn: Value => Value, head: Expr, tail: List[Expr]): Value = {
            val ha = loop(head, locals, anon)
            val res = fn(ha)
            tail match {
              case Nil => res
              case h :: tail =>
                app(res.asFn, h, tail)
            }
          }

          val fn = loop(expr, locals, anon)
          app(fn.asFn, args.head, args.tail)
        case Let(localOrBind, value, in) =>
          localOrBind match {
            case Right((b, rec)) =>
              if (rec.isRecursive) {

                lazy val locals1: Map[Bindable, Eval[Value]] =
                  locals.updated(b, vv)

                lazy val vv = Eval.later(loop(value, locals1, anon))

                loop(in, locals1, anon)
              }
              else {
                val vv = Eval.now(loop(value, locals, anon))
                loop(in, locals.updated(b, vv), anon)
              }
            case Left(LocalAnon(l)) =>
              val vv = loop(value, locals, anon)
              loop(in, locals, anon.updated(l, vv))
            case Left(LocalAnonMut(l)) =>
              val vv = loop(value, locals, anon)
              muts.put(l, vv)
              val res = loop(in, locals, anon)
              // now we can remove this from mutable scope
              muts.remove(l)
              res
          }
        case Literal(lit) => Value.fromLit(lit)
        case If(cond, thenExpr, elseExpr) =>
          if (intExpr(cond, locals, anon) == 0) loop(elseExpr, locals, anon)
          else loop(thenExpr, locals, anon)

        case MatchString(str, pat, binds) =>
          val arg = loop(str, locals, anon).asExternal.toAny.asInstanceOf[String]
          matchString(arg, toRegex(pat), binds)
        case GetEnumElement(expr, v, idx, sz) =>
          val sum = loop(expr, locals, anon).asSum
          sum.value.get(idx)

        case GetStructElement(expr, idx, sz) =>
          val prod = loop(expr, locals, anon).asProduct
          prod.get(idx)
        case PrevNat(expr) =>
          val anyBI = loop(expr, locals, anon).asExternal.toAny
          val bi = anyBI.asInstanceOf[BigInteger]
          ExternalValue(bi.subtract(BigInteger.ONE))
        case cons: ConsExpr => makeCons(cons)
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
