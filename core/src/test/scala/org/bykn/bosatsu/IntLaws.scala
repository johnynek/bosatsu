package org.bykn.bosatsu

import java.math.BigInteger
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite

object IntLaws {
  implicit class BIMethods(val self: BigInteger) extends AnyVal {
    def *(that: BigInteger) = self.multiply(that)
    def /(that: BigInteger) = PredefImpl.divBigInteger(self, that)
    def %(that: BigInteger) = PredefImpl.modBigInteger(self, that)
    def +(that: BigInteger) = self.add(that)
    def -(that: BigInteger) = self.subtract(that)
  }
}

class IntLaws extends AnyFunSuite {
  import IntLaws.BIMethods

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 50000)
  // PropertyCheckConfiguration(minSuccessful = 5000)
  // PropertyCheckConfiguration(minSuccessful = 500)

  val genBI: Gen[BigInteger] =
    Gen
      .choose(-128L, 128L)
      .map(BigInteger.valueOf(_))

  test("match python on some examples") {
    assert(
      BigInteger.valueOf(4L) % BigInteger.valueOf(-3L) == BigInteger.valueOf(
        -2L
      )
    )

    assert(
      BigInteger.valueOf(-8L) % BigInteger.valueOf(-2L) == BigInteger.valueOf(
        0L
      )
    )
    assert(
      BigInteger.valueOf(-8L) / BigInteger.valueOf(-2L) == BigInteger.valueOf(
        4L
      )
    )

    assert(
      BigInteger.valueOf(-4L) % BigInteger.valueOf(-3L) == BigInteger.valueOf(
        -1L
      )
    )
    assert(
      BigInteger.valueOf(13L) % BigInteger.valueOf(3L) == BigInteger.valueOf(1L)
    )
    assert(
      BigInteger.valueOf(-113L) / BigInteger.valueOf(16L) == BigInteger.valueOf(
        -8L
      )
    )

    assert(
      BigInteger.valueOf(54L) % BigInteger.valueOf(-3L) == BigInteger.valueOf(
        0L
      )
    )
    assert(
      BigInteger.valueOf(54L) / BigInteger.valueOf(-3L) == BigInteger.valueOf(
        -18L
      )
    )
  }

  test("a = (a / b) * b + (a % b)") {
    forAll(genBI, genBI) { (a, b) =>
      val div = a / b
      val mod = a % b
      val a1 = (div * b) + mod
      assert(a1 == a, s"div = $div, mod = $mod")
    }
  }

  test("a = (a / b) * b <= a if (b > 0)") {
    forAll(genBI, genBI) { (a, b) =>
      if (b.compareTo(BigInteger.ZERO) > 0) {
        val div = a / b
        val a1 = div * b
        assert(a1.compareTo(a) <= 0, s"div = $div")
      }
    }
  }

  test("a = (a / b) * b >= a if (b < 0)") {
    forAll(genBI, genBI) { (a, b) =>
      if (b.compareTo(BigInteger.ZERO) < 0) {
        val div = a / b
        val a1 = div * b
        assert(a1.compareTo(a) >= 0, s"div = $div")
      }
    }
  }

  test("(a - (a % b)) % b == 0") {
    forAll(genBI, genBI) { (a, b) =>
      assert((a - (a % b)) % b == BigInteger.ZERO)
    }
  }

  test("if (a * k) % b ==  (c * k) % b if a ~= c") {
    forAll(genBI, genBI, genBI, genBI) { (a, b, c0, k) =>
      // make something with the same mod as a
      val c = a + (c0 * b)

      assert((a * k) % b == (c * k) % b)
    }
  }

  test("if (a + k) % b ==  (c + k) % b if a ~= c") {
    forAll(genBI, genBI, genBI, genBI) { (a, b, c0, k) =>
      // make something with the same mod as a
      val c = a + (c0 * b)

      assert((a + k) % b == (c + k) % b)
    }
  }

  test("0 <= a % b <= b if b > 0") {
    forAll(genBI, genBI) { (a, b) =>
      if (b.compareTo(BigInteger.ZERO) > 0) {
        val mod = a % b
        assert(BigInteger.ZERO.compareTo(mod) <= 0)
        assert(mod.compareTo(b) < 0)
      }
    }
  }

  test("a / b <= a if b >= 0 and a >= 0") {
    forAll(genBI, genBI) { (a, b) =>
      if (
        b.compareTo(BigInteger.ZERO) >= 0 && a.compareTo(BigInteger.ZERO) >= 0
      ) {
        val div = a / b
        assert(div.compareTo(a) <= 0, div)
      }
    }
  }

  test("(-a) / (-b) == a / b") {
    forAll(genBI, genBI) { (a, b) =>
      assert((a.negate) / (b.negate) == a / b)
    }
  }

  test("a.mod(b) != 0 then has the same sign as b for b != 0") {
    forAll(genBI, genBI) { (a, b) =>
      val mod = a % b
      if ((b != BigInteger.ZERO) && (mod != BigInteger.ZERO)) {
        assert(mod.signum == b.signum)
      }
    }
  }

  test("a.mod(b) == 0 then (a/b)*b == a") {
    forAll(genBI, genBI) { (a, b) =>
      val mod = a % b
      if (mod == BigInteger.ZERO) {
        assert((a / b) * b == a)
      }
    }
  }

  test("a.mod(0) == a") {
    forAll(genBI) { a =>
      assert(a % BigInteger.ZERO == a)
    }
  }
  test("a / 0 == 0") {
    forAll(genBI) { a =>
      assert(a / BigInteger.ZERO == BigInteger.ZERO)
    }
  }

  test("gcd matches a simple implementation") {
    forAll(genBI, genBI) { (a, b) =>
      @annotation.tailrec
      def gcd(a: BigInteger, b: BigInteger): BigInteger =
        if (b == BigInteger.ZERO) a
        else {
          gcd(b, a % b)
        }

      assert(gcd(a, b) == PredefImpl.gcdBigInteger(a, b))
    }
  }
}
