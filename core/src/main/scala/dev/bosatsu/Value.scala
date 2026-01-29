package dev.bosatsu

import cats.data.NonEmptyList
import java.math.BigInteger
import scala.collection.immutable.SortedMap

/** If we later determine that this performance matters and this wrapping is
  * hurting, we could replace Value with a less structured type and put all the
  * reflection into unapply calls but keep most of the API
  */
sealed abstract class Value derives CanEqual {
  import Value._

  def asFn: NonEmptyList[Value] => Value =
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

  final def applyAll(args: NonEmptyList[Value]): Value =
    asFn(args)
}

object Value {
  final class ProductValue(val values: Array[Value]) extends Value {
    override lazy val hashCode =
      scala.util.hashing.MurmurHash3.arrayHash(values)
    final def get(idx: Int): Value = values(idx)

    override def equals(obj: Any): Boolean =
      obj match {
        case thatP: ProductValue =>
          (this eq thatP) ||
          java.util.Arrays.equals(
            values.asInstanceOf[Array[AnyRef]],
            thatP.values.asInstanceOf[Array[AnyRef]]
          )
        case _ => false
      }
    override def toString: String = values.mkString("ProductValue(", ",", ")")
  }

  val UnitValue: ProductValue = new ProductValue(new Array(0))

  object ProductValue {
    def single(v: Value): ProductValue =
      new ProductValue(Array(v))

    def fromList(vs: List[Value]): ProductValue =
      if (vs.isEmpty) UnitValue
      else new ProductValue(vs.toArray)

    def unapplySeq(v: Value): Option[Seq[Value]] =
      v match {
        case p: ProductValue => Some(p.values.toSeq)
        case _               => None
      }
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
    private val sizeMask = 0xffffff00
    private val constCount = 256
    private lazy val constants: Array[SumValue] =
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
      def toFn: NonEmptyList[Value] => Value
    }

    case class SimpleFnValue(toFn: NonEmptyList[Value] => Value) extends Arg

    def apply(toFn: NonEmptyList[Value] => Value): FnValue =
      new FnValue(SimpleFnValue(toFn))

    def unapply(fnValue: FnValue): Some[NonEmptyList[Value] => Value] = Some(
      fnValue.arg.toFn
    )

    val identity: FnValue = FnValue(vs => vs.head)
  }

  case class ExternalValue(toAny: Any) extends Value

  val False: SumValue = SumValue(0, UnitValue)
  val True: SumValue = SumValue(1, UnitValue)

  object Tuple {
    def unapply(v: Value): Option[List[Value]] =
      v match {
        case p: ProductValue => Some(p.values.toList)
        case _               => None
      }

    def fromList(vs: List[Value]): ProductValue =
      ProductValue.fromList(vs)

    def apply(vs: Value*): ProductValue =
      if (vs.isEmpty) UnitValue
      else new ProductValue(vs.toArray)
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
      case c @ Lit.Chr(_) => ExternalValue(c.asStr)
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
    def some(v: Value): Value = SumValue(1, ProductValue.single(v))

    private val someNone = Some(None)

    def unapply(v: Value): Option[Option[Value]] =
      v match {
        case s: SumValue =>
          if ((s.variant == 0) && (s.value == UnitValue)) someNone
          else if ((s.variant == 1)) {
            s.value.values match {
              case Array(head) => Some(Some(head))
              case _           => None
            }
          } else None
        case _ => None
      }
  }

  object VList {
    val VNil: Value = SumValue(0, UnitValue)
    object Cons {
      def apply(head: Value, tail: Value): Value =
        SumValue(1, new ProductValue(Array(head, tail)))

      def unapply(v: Value): Option[(Value, Value)] =
        v match {
          case s: SumValue =>
            if (s.variant == 1) {
              s.value.values match {
                case Array(head, rest) => Some((head, rest))
                case _                 => None
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
        case VNil             => Some(Nil)
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
          val v =
            fn.applyAll(
              NonEmptyList(
                new ProductValue(Array(v1, null)),
                new ProductValue(Array(v2, null)) :: Nil
              )
            ).asSum
              .variant

          // v = 0, 1, 2 for LT, EQ, GT
          v - 1
        }
      }

    // enum Tree: Empty, Branch(size: Int, height: Int, key: a, left: Tree[a], right: Tree[a])
    // struct Dict[k, v](ord: Order[(k, v)], tree: Tree[(k, v)])
    def unapply(v: Value): Option[SortedMap[Value, Value]] =
      v match {
        case ProductValue(ordFn: FnValue, tree) =>
          implicit val ord: Ordering[Value] =
            keyOrderingFromOrdFn(ordFn)

          def treeToList(
              t: Value,
              acc: SortedMap[Value, Value]
          ): SortedMap[Value, Value] = {
            val v = t.asSum
            if (v.variant == 0) acc // empty
            else {
              v.value match {
                case ProductValue(_, _, ProductValue(k, v), left, right) =>
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
      FnValue {
        case NonEmptyList(
              Tuple(ExternalValue(k1: String) :: _),
              Tuple(ExternalValue(k2: String) :: _) :: _
            ) =>
          Comparison.fromInt(k1.compareTo(k2))
        case badShape =>
          // $COVERAGE-OFF$
          sys.error(s"ill-typed in String Dict order: $badShape")
        // $COVERAGE-ON$
      }

    def fromStringKeys(kvs: List[(String, Value)]): Value = {
      val allItems: Array[(String, Value)] = kvs.toMap.toArray
      java.util.Arrays
        .sort(allItems, Ordering[String].on((kv: (String, Value)) => kv._1))

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
              new ProductValue(
                Array(
                  ExternalValue(z),
                  ExternalValue(h),
                  new ProductValue(Array(ExternalValue(k), v)),
                  left,
                  right
                )
              )
            )
          )
        }

      val (_, _, tree) = makeTree(0, allItems.length)
      new ProductValue(Array(strOrdFn, tree))
    }
  }
}
