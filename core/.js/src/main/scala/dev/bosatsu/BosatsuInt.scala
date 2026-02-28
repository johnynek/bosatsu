package dev.bosatsu

import java.math.BigInteger

object BosatsuInt {
  opaque type Tpe = java.lang.Integer | BigInteger

  private val MaxIntLong = Int.MaxValue.toLong
  private val MinIntLong = Int.MinValue.toLong
  private val MaxIntBI = BigInteger.valueOf(MaxIntLong)
  private val MinIntBI = BigInteger.valueOf(MinIntLong)

  private inline def intToBigInteger(i: java.lang.Integer): BigInteger =
    BigInteger.valueOf(i.longValue())

  inline def fromInt(i: Int): Tpe =
    java.lang.Integer.valueOf(i)

  inline def fromLong(l: Long): Tpe =
    if (l >= MinIntLong && l <= MaxIntLong)
      java.lang.Integer.valueOf(l.toInt)
    else BigInteger.valueOf(l)

  inline def fromBigInteger(bi: BigInteger): Tpe =
    if (bi.compareTo(MinIntBI) >= 0 && bi.compareTo(MaxIntBI) <= 0)
      java.lang.Integer.valueOf(bi.intValue())
    else bi

  def isInstance(a: Any): Boolean =
    a match {
      case _: java.lang.Integer => true
      case _: BigInteger        => true
      case _                    => false
    }

  def cast(a: Any): Tpe =
    a match {
      case i: java.lang.Integer => i
      case bi: BigInteger       => bi
      case other                =>
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
        case i: java.lang.Integer => intToBigInteger(i)
        case bi: BigInteger       => bi
      }

    inline def compare(that: Tpe): Int =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          java.lang.Integer.compare(a.intValue(), b.intValue())
        case (a: java.lang.Integer, b: BigInteger) =>
          intToBigInteger(a).compareTo(b)
        case (a: BigInteger, b: java.lang.Integer) =>
          a.compareTo(intToBigInteger(b))
        case (a: BigInteger, b: BigInteger) =>
          a.compareTo(b)
      }

    inline def isZero: Boolean =
      t match {
        case i: java.lang.Integer => i.intValue() == 0
        case bi: BigInteger       => bi.signum == 0
      }

    inline def toDouble: Double =
      t match {
        case i: java.lang.Integer => i.doubleValue()
        case bi: BigInteger       => bi.doubleValue()
      }

    inline def +(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          val sum = a.longValue() + b.longValue()
          if (sum >= MinIntLong && sum <= MaxIntLong)
            java.lang.Integer.valueOf(sum.toInt)
          else intToBigInteger(a).add(intToBigInteger(b))
        case _ =>
          fromBigInteger(t.toBigInteger.add(that.toBigInteger))
      }

    inline def -(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          val diff = a.longValue() - b.longValue()
          if (diff >= MinIntLong && diff <= MaxIntLong)
            java.lang.Integer.valueOf(diff.toInt)
          else intToBigInteger(a).subtract(intToBigInteger(b))
        case _ =>
          fromBigInteger(t.toBigInteger.subtract(that.toBigInteger))
      }

    inline def *(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          val prod = a.longValue() * b.longValue()
          if (prod >= MinIntLong && prod <= MaxIntLong)
            java.lang.Integer.valueOf(prod.toInt)
          else intToBigInteger(a).multiply(intToBigInteger(b))
        case _ =>
          fromBigInteger(t.toBigInteger.multiply(that.toBigInteger))
      }

    inline def &(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          java.lang.Integer.valueOf(a.intValue() & b.intValue())
        case _ =>
          fromBigInteger(t.toBigInteger.and(that.toBigInteger))
      }

    inline def |(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          java.lang.Integer.valueOf(a.intValue() | b.intValue())
        case _ =>
          fromBigInteger(t.toBigInteger.or(that.toBigInteger))
      }

    inline def ^(that: Tpe): Tpe =
      (t, that) match {
        case (a: java.lang.Integer, b: java.lang.Integer) =>
          java.lang.Integer.valueOf(a.intValue() ^ b.intValue())
        case _ =>
          fromBigInteger(t.toBigInteger.xor(that.toBigInteger))
      }

    inline def unary_~ : Tpe =
      t match {
        case i: java.lang.Integer => java.lang.Integer.valueOf(~i.intValue())
        case bi: BigInteger       => fromBigInteger(bi.not())
      }

    inline def increment: Tpe =
      t match {
        case i: java.lang.Integer =>
          val iv = i.intValue()
          if (iv < Int.MaxValue) java.lang.Integer.valueOf(iv + 1)
          else MaxIntBI.add(BigInteger.ONE)
        case bi: BigInteger =>
          fromBigInteger(bi.add(BigInteger.ONE))
      }

    inline def decrement: Tpe =
      t match {
        case i: java.lang.Integer =>
          val iv = i.intValue()
          if (iv > Int.MinValue) java.lang.Integer.valueOf(iv - 1)
          else MinIntBI.subtract(BigInteger.ONE)
        case bi: BigInteger =>
          fromBigInteger(bi.subtract(BigInteger.ONE))
      }
  }
}
