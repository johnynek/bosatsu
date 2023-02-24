package org.bykn.bosatsu

import cats.data.NonEmptyList
import java.math.BigInteger
import scala.collection.immutable.SortedMap

/** If we later determine that this performance matters and this wrapping is
  * hurting, we could replace Value with a less structured type and put all the
  * reflection into unapply calls but keep most of the API
  */
sealed abstract class Value {
  import Value._

  def asFn: Value => Value =
    this match {
      case FnValue(f) => f
      case other      =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to Fn: $other")
      // $COVERAGE-ON$
    }

  def asSum: SumValue =
    this match {
      case s: SumValue => s
      case _           =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to SumValue: $this")
      // $COVERAGE-ON$
    }

  def asProduct: ProductValue =
    this match {
      case p: ProductValue => p
      case _               =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to ProductValue: $this")
      // $COVERAGE-ON$
    }

  def asExternal: ExternalValue =
    this match {
      case ex: ExternalValue => ex
      case _                 =>
        // $COVERAGE-OFF$this should be unreachable
        sys.error(s"invalid cast to ExternalValue: $this")
      // $COVERAGE-ON$
    }

  final def applyAll(args: NonEmptyList[Value]): Value = {
    @annotation.tailrec
    def loop(toFn: Value => Value, args: NonEmptyList[Value]): Value =
      args.tail match {
        case h :: tail => loop(toFn(args.head).asFn, NonEmptyList(h, tail))
        case Nil       => toFn(args.head)
      }

    loop(this.asFn, args)
  }
}

object Value {
  sealed abstract class ProductValue extends Value {
    def toList: List[Value] =
      this match {
        case UnitValue             => Nil
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
            throw new IllegalArgumentException(
              s"exhausted index at $ix on ${this}.get($idx)"
            )
        }

      loop(this, idx)
    }
  }

  object ProductValue {
    def fromList(ps: List[Value]): ProductValue =
      ps match {
        case Nil       => UnitValue
        case h :: tail => ConsValue(h, fromList(tail))
      }
  }

  case object UnitValue extends ProductValue
  case class ConsValue(head: Value, tail: ProductValue) extends ProductValue {
    override val hashCode = (head, tail).hashCode
  }
  final class SumValue(val variant: Int, val value: ProductValue)
      extends Value {
    override def equals(that: Any) =
      that match {
        case s: SumValue =>
          (s eq this) || ((variant == s.variant) && (value == s.value))
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
      if ((value == UnitValue) && ((variant & sizeMask) == 0))
        constants(variant)
      else new SumValue(variant, value)
  }

  class FnValue(fnValueArg: FnValue.Arg) extends Value {
    val arg: FnValue.Arg = fnValueArg
  }

  object FnValue {
    trait Arg {
      def toFn: Value => Value
    }

    case class SimpleFnValue(toFn: Value => Value) extends Arg

    def apply(toFn: Value => Value): FnValue =
      new FnValue(SimpleFnValue(toFn))

    def unapply(fnValue: FnValue): Some[Value => Value] = Some(fnValue.arg.toFn)

    val identity: FnValue = FnValue(v => v)

    def curry(arity: Int)(vs: List[Value] => Value): Value = {
      // TODO: this is a obviously terrible
      // the encoding is inefficient, the implementation is inefficient
      def loop(param: Int, args: List[Value]): Value =
        if (param == 0) vs(args.reverse)
        else
          FnValue { ea =>
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
        case _                                     => None
      }

    def apply(a: Value, b: Value): ProductValue =
      ConsValue(a, ConsValue(b, UnitValue))
  }

  object Tuple {

    /** Tuples are encoded as: (1, 2, 3) => TupleCons(1, TupleCons(2,
      * TupleCons(3, ()))) since a Tuple(a, b) is encoded as ConsValue(a,
      * ConsValue(b, UnitValue)) this gives double wrapping
      */
    def unapply(v: Value): Option[List[Value]] =
      v match {
        case TupleCons(a, b) =>
          unapply(b).map(a :: _)
        case UnitValue => Some(Nil)
        case _         => None
      }

    def fromList(vs: List[Value]): ProductValue =
      vs match {
        case Nil       => UnitValue
        case h :: tail => TupleCons(h, fromList(tail))
      }

    def apply(vs: Value*): ProductValue =
      fromList(vs.toList)
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
      case Lit.Str(s)     => ExternalValue(s)
      case Lit.Integer(i) => ExternalValue(i)
    }

  object VInt {
    def apply(v: Int): Value = apply(BigInt(v))
    def apply(v: BigInt): Value = ExternalValue(v.bigInteger)
    def unapply(v: Value): Option[BigInteger] =
      v match {
        case ExternalValue(v: BigInteger) => Some(v)
        case _                            => None
      }
  }

  object Str {
    def apply(str: String): Value = ExternalValue(str)
    def unapply(v: Value): Option[String] =
      v match {
        case ExternalValue(str: String) => Some(str)
        case _                          => None
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
              case _                          => None
            }
          } else None
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
                case ConsValue(head, ConsValue(rest, UnitValue)) =>
                  Some((head, rest))
                case _ => None
              }
            } else None
          case _ => None
        }
    }

    def apply(items: List[Value]): Value = {
      @annotation.tailrec
      def go(vs: List[Value], acc: Value): Value =
        vs match {
          case Nil       => acc
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
    def keyOrderingFromOrdFn(fn: FnValue): Ordering[Value] =
      new Ordering[Value] {
        def compare(v1: Value, v2: Value): Int = {
          // these Values are keys, but we need to convert them
          // back to tuples where the values are ignored for scala
          val v = fn
            .asFn(Tuple.fromList(v1 :: null :: Nil))
            .asFn(Tuple.fromList(v2 :: null :: Nil))
            .asSum
            .variant
          if (v == 0) -1
          else if (v == 1) 0
          else if (v == 2) 1
          else {
            // $COVERAGE-OFF$
            sys.error(s"expected variant to be 0, 1, 2: found: $v")
            // $COVERAGE-ON$
          }
        }
      }

    // enum Tree: Empty, Branch(size: Int, height: Int, key: a, left: Tree[a], right: Tree[a])
    // struct Dict[k, v](ord: Order[(k, v)], tree: Tree[(k, v)])
    def unapply(v: Value): Option[SortedMap[Value, Value]] =
      v match {
        case ConsValue(ordFn: FnValue, ConsValue(tree, UnitValue)) =>
          implicit val ord: Ordering[Value] =
            keyOrderingFromOrdFn(ordFn)

          def treeToList(
              t: Value,
              acc: SortedMap[Value, Value]
          ): SortedMap[Value, Value] = {
            val v = t.asSum
            if (v.variant == 0) acc // empty
            else {
              v.value.toList match {
                case _ :: _ :: Tuple(k :: v :: Nil) :: left :: right :: Nil =>
                  val acc1 = acc.updated(k, v)
                  val acc2 = treeToList(left, acc1)
                  treeToList(right, acc2)
                case other =>
                  // $COVERAGE-OFF$
                  sys.error(s"ill-shaped: $other")
                // $COVERAGE-ON$
              }
            }
          }

          Some(treeToList(tree, SortedMap.empty[Value, Value]))
        // $COVERAGE-OFF$
        // we generally don't test ill-typed conversion
        case _ => None
        // $COVERAGE-ON$
      }

    val strOrdFn: FnValue =
      FnValue { tup1 =>
        FnValue { tup2 =>
          (tup1, tup2) match {
            case (
                  Tuple(ExternalValue(k1: String) :: _),
                  Tuple(ExternalValue(k2: String) :: _)
                ) =>
              Comparison.fromInt(k1.compareTo(k2))
            case _ =>
              // $COVERAGE-OFF$
              sys.error(s"ill-typed in String Dict order: $tup1, $tup2")
            // $COVERAGE-ON$
          }
        }
      }

    def fromStringKeys(kvs: List[(String, Value)]): Value = {
      val allItems: Array[(String, Value)] = kvs.toMap.toArray
      java.util.Arrays
        .sort(allItems, Ordering[String].on { (kv: (String, Value)) => kv._1 })

      val empty = (BigInteger.ZERO, BigInteger.ZERO, SumValue(0, UnitValue))

      def makeTree(start: Int, end: Int): (BigInteger, BigInteger, SumValue) =
        if (start >= end) empty
        else {
          val mid = start + ((end - start) / 2)
          val (k, v) = allItems(mid)
          val (lh, lz, left) = makeTree(start, mid)
          val (rh, rz, right) = makeTree(mid + 1, end)
          val h = lh.max(rh).add(BigInteger.ONE)
          val z = lz.add(rz).add(BigInteger.ONE)
          (
            h,
            z,
            SumValue(
              1,
              ProductValue.fromList(
                ExternalValue(z) ::
                  ExternalValue(h) ::
                  Tuple.fromList(ExternalValue(k) :: v :: Nil) ::
                  left ::
                  right ::
                  Nil
              )
            )
          )
        }

      val (_, _, tree) = makeTree(0, allItems.length)
      ProductValue.fromList(strOrdFn :: tree :: Nil)
    }
  }
}
