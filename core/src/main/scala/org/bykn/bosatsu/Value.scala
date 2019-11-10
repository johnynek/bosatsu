package org.bykn.bosatsu

import cats.Eval
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

  def asLazyFn: Eval[Value] => Eval[Value] =
    this match {
      case FnValue(f) => f
      case other =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to Fn: $other")
        // $COVERAGE-ON$
    }

  def asFn: Value => Eval[Value] =
    this match {
      case FnValue(f) => { v => f(Eval.now(v)) }
      case other =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to Fn: $other")
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
  case class SumValue(variant: Int, value: ProductValue) extends Value
  object SumValue {
    private[this] val sizeMask = 0xffffff00
    private[this] val constCount = 256
    private[this] val constants: Array[SumValue] =
      (0 until constCount).map(new SumValue(_, UnitValue)).toArray

    def apply(variant: Int, value: ProductValue): SumValue =
      if ((value == UnitValue) && ((variant & sizeMask) == 0)) constants(variant)
      else new SumValue(variant, value)
  }
  case class FnValue(toFn: Eval[Value] => Eval[Value]) extends Value
  object FnValue {
    val identity: FnValue = FnValue(v => v)
  }
  case class ExternalValue(toAny: Any) extends Value

  val False: Value = SumValue(0, UnitValue)
  val True: Value = SumValue(1, UnitValue)

  object TupleCons {
    def unapply(v: Value): Option[(Value, Value)] =
      v match {
        case ConsValue(a, ConsValue(b, UnitValue)) => Some((a, b))
        case _ => None
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
    def unapply(v: Value): Option[List[Value]] =
      v match {
        case TupleCons(a, b) =>
          unapply(b).map(a :: _)
        case UnitValue => Some(Nil)
        case _ => None
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

    def unapply(v: Value): Option[Option[Value]] =
      v match {
        case SumValue(0, UnitValue) =>
          Some(None)
        case SumValue(1, ConsValue(head, UnitValue)) =>
          Some(Some(head))
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
          case SumValue(1, ConsValue(head, ConsValue(rest, UnitValue))) =>
            Some((head, rest))
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
  }
}
