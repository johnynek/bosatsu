package org.bykn.bosatsu

import java.math.BigInteger
import scala.collection.immutable.SortedMap

/**
 * If we later determine that this performance matters
 * and this wrapping is hurting, we could replace
 * Value with a less structured type and put
 * all the reflection into unapply calls but keep
 * most of the API
 */
sealed abstract class Value {
  import Value._

  def asFn: Value => Value =
    this match {
      case FnValue(f) => f
      case other =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to Fn: $other")
        // $COVERAGE-ON$
    }

  def asSum: SumValue =
    this match {
      case s: SumValue => s
      case _ =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to SumValue: $this")
        // $COVERAGE-ON$
    }

  def asProduct:ProductValue =
    this match {
      case p: ProductValue => p
      case _ =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to ProductValue: $this")
        // $COVERAGE-ON$
    }

  def asExternal: ExternalValue =
    this match {
      case ex: ExternalValue => ex
      case _ =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to ExternalValue: $this")
        // $COVERAGE-ON$
    }
}

object Value {
  sealed abstract class ProductValue extends Value {
    def toList: List[Value] =
      this match {
        case UnitValue => Nil
        case ConsValue(head, tail) => head :: tail.toList
      }

    final def get(idx: Int): Value = {
      @annotation.tailrec
      def loop(self: ProductValue, ix: Int): Value =
        self match {
          case ConsValue(head, tail) =>
            if (ix <= 0) head
            else loop(tail, ix - 1)
          case UnitValue =>
            throw new IllegalArgumentException(s"exhausted index at $ix on ${this}.get($idx)")
        }

      loop(this, idx)
    }
  }

  object ProductValue {
    def fromList(ps: List[Value]): ProductValue =
      ps match {
        case Nil => UnitValue
        case h :: tail => ConsValue(h, fromList(tail))
      }
  }

  case object UnitValue extends ProductValue
  case class ConsValue(head: Value, tail: ProductValue) extends ProductValue {
    override val hashCode = (head, tail).hashCode
  }
  final class SumValue(val variant: Int, val value: ProductValue) extends Value {
    override def equals(that: Any) =
      that match {
        case s: SumValue => (s eq this) || ((variant == s.variant) && (value == s.value))
        case _ => false
      }
    override def hashCode: Int =
      // 65521 is the largest prime that fits in 16 (half of 32) bits
      variant * 65521 + value.hashCode

    override def toString = s"SumValue($variant, $value)"
  }
  object SumValue {
    private[this] val sizeMask = 0xffffff00
    private[this] val constCount = 256
    private[this] val constants: Array[SumValue] =
      (0 until constCount).map(new SumValue(_, UnitValue)).toArray

    def apply(variant: Int, value: ProductValue): SumValue =
      if ((value == UnitValue) && ((variant & sizeMask) == 0)) constants(variant)
      else new SumValue(variant, value)
  }
  case class FnValue(toFn: Value => Value) extends Value
  object FnValue {
    val identity: FnValue = FnValue(v => v)

    def curry(arity: Int)(vs: List[Value] => Value): Value = {
      // TODO: this is a obviously terrible
      // the encoding is inefficient, the implementation is inefficient
      def loop(param: Int, args: List[Value]): Value =
        if (param == 0) vs(args.reverse)
        else FnValue { ea =>
          loop(param - 1, ea :: args)
        }

      loop(arity, Nil)
    }

  }
  case class ExternalValue(toAny: Any) extends Value

  val False: SumValue = SumValue(0, UnitValue)
  val True: SumValue = SumValue(1, UnitValue)

  object TupleCons {
    def unapply(v: Value): Option[(Value, Value)] =
      v match {
        case ConsValue(a, ConsValue(b, UnitValue)) => Some((a, b))
        case _ => None
      }

    def apply(a: Value, b: Value): ProductValue =
      ConsValue(a, ConsValue(b, UnitValue))
  }

  object Tuple {
    /**
     * Tuples are encoded as:
     * (1, 2, 3) => TupleCons(1, TupleCons(2, TupleCons(3, ())))
     * since a Tuple(a, b) is encoded as
     * ConsValue(a, ConsValue(b, UnitValue))
     * this gives double wrapping
     */
    def unapply(v: Value): Option[List[Value]] =
      v match {
        case TupleCons(a, b) =>
          unapply(b).map(a :: _)
        case UnitValue => Some(Nil)
        case _ => None
      }

    def fromList(vs: List[Value]): ProductValue =
      vs match {
        case Nil => UnitValue
        case h :: tail => TupleCons(h, fromList(tail))
      }
  }

  object Comparison {
    def fromInt(i: Int): Value =
      if (i < 0) LT else if (i > 0) GT else EQ

    val LT: Value = SumValue(0, UnitValue)
    val EQ: Value = SumValue(1, UnitValue)
    val GT: Value = SumValue(2, UnitValue)
  }

  def fromLit(l: Lit): Value =
    l match {
      case Lit.Str(s) => ExternalValue(s)
      case Lit.Integer(i) => ExternalValue(i)
    }

  object VInt {
    def apply(v: Int): Value = apply(BigInt(v))
    def apply(v: BigInt): Value = ExternalValue(v.bigInteger)
    def unapply(v: Value): Option[BigInteger] =
      v match {
        case ExternalValue(v: BigInteger) => Some(v)
        case _ => None
      }
  }

  object Str {
    def apply(str: String): Value = ExternalValue(str)
    def unapply(v: Value): Option[String] =
      v match {
        case ExternalValue(str: String) => Some(str)
        case _ => None
      }
  }

  object VOption {
    val none: Value = SumValue(0, UnitValue)
    def some(v: Value): Value = SumValue(1, ConsValue(v, UnitValue))

    private[this] val someNone = Some(None)

    def unapply(v: Value): Option[Option[Value]] =
      v match {
        case s: SumValue =>
          if ((s.variant == 0) && (s.value == UnitValue)) someNone
          else if ((s.variant == 1)) {
            s.value match {
              case ConsValue(head, UnitValue) => Some(Some(head))
              case _ => None
            }
          }
          else None
        case _ => None
      }
  }

  object VList {
    val VNil: Value = SumValue(0, UnitValue)
    object Cons {
      def apply(head: Value, tail: Value): Value =
        SumValue(1, ConsValue(head, ConsValue(tail, UnitValue)))

      def unapply(v: Value): Option[(Value, Value)] =
        v match {
          case s: SumValue =>
            if (s.variant == 1) {
              s.value match {
                case ConsValue(head, ConsValue(rest, UnitValue)) => Some((head, rest))
                case _ => None
              }
            }
            else None
          case _ => None
        }
    }

    def apply(items: List[Value]): Value = {
      @annotation.tailrec
      def go(vs: List[Value], acc: Value): Value =
        vs match {
          case Nil => acc
          case h :: tail => go(tail, Cons(h, acc))
        }
      go(items.reverse, VNil)
    }

    def unapply(v: Value): Option[List[Value]] =
      v match {
        case VNil => Some(Nil)
        case Cons(head, rest) =>
          unapply(rest).map(head :: _)
        case _ => None
      }
  }

  object VDict {
    def unapply(v: Value): Option[SortedMap[Value, Value]] =
      v match {
        case ExternalValue(v: SortedMap[_, _]) => Some(v.asInstanceOf[SortedMap[Value, Value]])
        case _ => None
      }

    def fromStringKeys(kvs: List[(String, Value)]): Value = {
      implicit val ord: Ordering[Value] =
        implicitly[Ordering[String]].on { case Str(str) => str }

      val bldr = SortedMap.newBuilder[Value, Value]
      bldr ++= kvs.iterator.map { case (k, v) => (Str(k), v) }
      ExternalValue(bldr.result)
    }
  }
}
