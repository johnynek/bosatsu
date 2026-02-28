package dev.bosatsu

import java.math.BigInteger

object BosatsuInt {
  opaque type Tpe = java.lang.Long | BigInteger

  private val MaxLongBI = BigInteger.valueOf(Long.MaxValue)
  private val MinLongBI = BigInteger.valueOf(Long.MinValue)

  private inline def longToBigInteger(l: java.lang.Long): BigInteger =
    BigInteger.valueOf(l.longValue())

  inline def fromInt(i: Int): Tpe =
    java.lang.Long.valueOf(i.toLong)

  inline def fromLong(l: Long): Tpe =
    java.lang.Long.valueOf(l)

  inline def fromBigInteger(bi: BigInteger): Tpe =
    if (bi.compareTo(MinLongBI) >= 0 && bi.compareTo(MaxLongBI) <= 0)
      java.lang.Long.valueOf(bi.longValue())
    else bi

  def isInstance(a: Any): Boolean =
    a match {
      case _: java.lang.Long => true
      case _: BigInteger     => true
      case _                 => false
    }

  def cast(a: Any): Tpe =
    a match {
      case l: java.lang.Long => l
      case bi: BigInteger    => bi
      case other             =>
        throw new ClassCastException(s"not a BosatsuInt: $other")
    }

  final class MatchResult(private val arg: Any) extends AnyVal {
    def isEmpty: Boolean = !isInstance(arg)
    def get: Tpe = cast(arg)
  }

  def unapply(a: Any): MatchResult =
    new MatchResult(a)

  extension (t: Tpe) {
    inline def show: String =
      t.toString

    inline def toBigInteger: BigInteger =
      t match {
        case l: java.lang.Long => longToBigInteger(l)
        case bi: BigInteger    => bi
      }

    inline def compare(that: Tpe): Int =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          java.lang.Long.compare(a.longValue(), b.longValue())
        case (a: java.lang.Long, b: BigInteger) =>
          longToBigInteger(a).compareTo(b)
        case (a: BigInteger, b: java.lang.Long) =>
          a.compareTo(longToBigInteger(b))
        case (a: BigInteger, b: BigInteger) =>
          a.compareTo(b)
      }

    inline def isZero: Boolean =
      t match {
        case l: java.lang.Long => l.longValue() == 0L
        case bi: BigInteger    => bi.signum == 0
      }

    inline def toDouble: Double =
      t match {
        case l: java.lang.Long => l.doubleValue()
        case bi: BigInteger    => bi.doubleValue()
      }

    inline def +(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          val av = a.longValue()
          val bv = b.longValue()
          val sum = av + bv
          if (((av ^ sum) & (bv ^ sum)) < 0L)
            longToBigInteger(a).add(longToBigInteger(b))
          else java.lang.Long.valueOf(sum)
        case _ =>
          fromBigInteger(t.toBigInteger.add(that.toBigInteger))
      }

    inline def -(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          val av = a.longValue()
          val bv = b.longValue()
          val diff = av - bv
          if (((av ^ bv) & (av ^ diff)) < 0L)
            longToBigInteger(a).subtract(longToBigInteger(b))
          else java.lang.Long.valueOf(diff)
        case _ =>
          fromBigInteger(t.toBigInteger.subtract(that.toBigInteger))
      }

    inline def *(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          val av = a.longValue()
          val bv = b.longValue()
          try java.lang.Long.valueOf(java.lang.Math.multiplyExact(av, bv))
          catch {
            case _: ArithmeticException =>
              longToBigInteger(a).multiply(longToBigInteger(b))
          }
        case _ =>
          fromBigInteger(t.toBigInteger.multiply(that.toBigInteger))
      }

    inline def &(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          java.lang.Long.valueOf(a.longValue() & b.longValue())
        case _ =>
          fromBigInteger(t.toBigInteger.and(that.toBigInteger))
      }

    inline def |(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          java.lang.Long.valueOf(a.longValue() | b.longValue())
        case _ =>
          fromBigInteger(t.toBigInteger.or(that.toBigInteger))
      }

    inline def ^(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Long, b: java.lang.Long) =>
          java.lang.Long.valueOf(a.longValue() ^ b.longValue())
        case _ =>
          fromBigInteger(t.toBigInteger.xor(that.toBigInteger))
      }

    inline def unary_~ : Tpe =
      t match {
        case l: java.lang.Long => java.lang.Long.valueOf(~l.longValue())
        case bi: BigInteger    => fromBigInteger(bi.not())
      }

    inline def increment: Tpe =
      t match {
        case l: java.lang.Long =>
          val lv = l.longValue()
          if (lv < Long.MaxValue) java.lang.Long.valueOf(lv + 1L)
          else MaxLongBI.add(BigInteger.ONE)
        case bi: BigInteger =>
          fromBigInteger(bi.add(BigInteger.ONE))
      }

    inline def decrement: Tpe =
      t match {
        case l: java.lang.Long =>
          val lv = l.longValue()
          if (lv > Long.MinValue) java.lang.Long.valueOf(lv - 1L)
          else MinLongBI.subtract(BigInteger.ONE)
        case bi: BigInteger =>
          fromBigInteger(bi.subtract(BigInteger.ONE))
      }
  }
}
