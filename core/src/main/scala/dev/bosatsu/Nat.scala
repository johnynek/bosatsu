package dev.bosatsu
import dev.bosatsu.Nat.Shift
import dev.bosatsu.Nat.Small

import Nat._

sealed abstract class Nat { lhs =>
  def toBigInt: BigInt =
    lhs match {
      case Small(asInt) =>
        BigInt(toLong(asInt))
      case Shift(x, b) =>
        // m(x + 1) + b
        two_32_BigInt * (x.inc.toBigInt) + toLong(b)
    }

  override def toString = toBigInt.toString

  def maybeLong: Option[Long] =
    lhs match {
      case Small(asInt) => Some(toLong(asInt))
      case Shift(x, b)  =>
        // 2^32 * (x + 1) + b
        x.maybeLong match {
          case Some(v0) =>
            if (v0 < Int.MaxValue) {
              val value = v0 + 1
              Some((value << 32) + toLong(b))
            } else None
          case None => None
        }
    }

  def inc: Nat =
    lhs match {
      case Small(asInt) =>
        val next = asInt + 1
        if (next != 0) wrapInt(next)
        else two_32
      case Shift(x, b) =>
        // m(x + 1) + b
        val b1 = b + 1
        if (b1 != 0) Shift(x, b1)
        else {
          // m(x + 1) + m
          // m(x + 1 + 1)
          Shift(x.inc, 0)
        }
    }

  def dec: Nat =
    lhs match {
      case Small(asInt) =>
        if (asInt != 0) wrapInt(asInt - 1)
        else zero
      case Shift(x, b) =>
        if (b != 0) Shift(x, b - 1)
        else {
          if (x.isZero) wrapInt(-1)
          else Shift(x.dec, -1)
        }
    }

  def isZero: Boolean =
    lhs match {
      case Small(0) => true
      case _        => false
    }

  // this * 2^32
  def shift_32: Nat =
    lhs match {
      case Small(asInt) =>
        if (asInt == 0) zero
        else {
          // m*(b - 1 + 1)
          Shift(wrapInt(asInt - 1), 0)
        }
      case Shift(x, b) =>
        // m*(m*(x + 1) + b)
        // m*((m(x + 1) + (b - 1)) + 1) + 0
        if (b != 0) {
          val b1 = b - 1
          Shift(Shift(x, b1), 0)
        } else if (x.isZero) {
          // m * (m(0 + 1) + 0)
          // m * ((m - 1) + 1) + 0
          val inner = wrapLong(0xffffffffL)
          Shift(inner, 0)
        } else {
          // x > 0
          // m(m(x + 1)) =
          // m(m(x + 1) - 1 + 1) + 0
          // m((m(x + 1) - 1) + 1) + 0
          val inner = x.inc.shift_32.dec
          Shift(inner, 0)
        }
    }

  def +(rhs: Nat): Nat =
    lhs match {
      case Small(l) =>
        rhs match {
          case Small(r) =>
            wrapLong(toLong(l) + toLong(r))
          case Shift(x, b) =>
            val res = toLong(l) + toLong(b)
            val low = lowBits(res)
            val high = highBits(res)
            if (high == 0) Shift(x, low)
            else {
              Shift(x + wrapInt(high), low)
            }
        }

      case Shift(x, b) =>
        rhs match {
          case Small(l) =>
            val res = toLong(l) + toLong(b)
            val low = lowBits(res)
            val high = highBits(res)
            if (high == 0) Shift(x, low)
            else {
              Shift(x + wrapInt(high), low)
            }
          case Shift(x1, b1) =>
            // (m(x + 1) + b) + (m (x1 + 1) + b1) = m(x + x1 + 1 + 1) + (b + b1)
            val res = toLong(b) + toLong(b1)
            val low = lowBits(res)
            val high = highBits(res)
            val xs = (x + x1).inc
            if (high == 0) Shift(xs, low)
            else {
              Shift(xs + wrapInt(high), low)
            }
        }
    }
  def *(rhs: Nat): Nat =
    lhs match {
      case Small(l) =>
        if (l == 0) zero
        else
          (rhs match {
            case Small(r) =>
              // can't overflow Long
              wrapLong(toLong(l) * toLong(r))
            case Shift(x, b) =>
              // (m(x + 1) + b) * l
              (x.inc * lhs).shift_32 + wrapInt(b) * lhs
          })

      case Shift(x, b) =>
        // (m(x + 1) + b) * rhs
        // (x + 1)*rhs*m + b * rhs
        (rhs * x.inc).shift_32 + rhs * wrapInt(b)
    }
}

object Nat {
  private val two_32_BigInt = BigInt(1L << 32)

  private val intMaskLow: Long = 0xffffffffL
  def toLong(i: Int): Long = i.toLong & intMaskLow
  def lowBits(l: Long): Int = l.toInt
  private def highBits(l: Long): Int = lowBits(l >> 32)

  // all numbers from [0, 2^{32} - 1]
  private case class Small(asInt: Int) extends Nat
  // base * (x + 1) + b
  private case class Shift(x: Nat, b: Int) extends Nat

  private val cache: Array[Small] =
    Array.tabulate(1024)(Small(_))

  val zero: Nat = cache(0)
  val one: Nat = cache(1)
  val two_32: Nat = Shift(zero, 0)

  // if the number is <= 0, return 0
  def fromInt(i: Int): Nat =
    if (i < 0) zero
    else if (i < cache.length) cache(i)
    else Small(i)

  // if the number is <= 0, return 0
  def fromLong(l: Long): Nat =
    if (l < 0) zero
    else wrapLong(l)

  def wrapInt(i: Int): Nat =
    if (0 <= i && i < cache.length) cache(i)
    else Small(i)

  def wrapLong(l: Long): Nat = {
    val low = lowBits(l)
    val high = highBits(l)
    if (high == 0) wrapInt(low)
    else {
      Shift(wrapInt(high - 1), low)
    }
  }

  // if b < 0 return 0
  def fromBigInt(b: BigInt): Nat =
    if (b <= 0) zero
    else if (b < two_32_BigInt) {
      fromLong(b.toLong)
    } else {
      val low = b % two_32_BigInt
      val high = b >> 32
      Shift(fromBigInt(high - 1), lowBits(low.toLong))
    }
}
