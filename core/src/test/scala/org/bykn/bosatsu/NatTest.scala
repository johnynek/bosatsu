package org.bykn.bosatsu

import org.scalacheck.{Gen, Prop}

import Prop.forAll

class NatTest extends munit.ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(if (Platform.isScalaJvm) 100000 else 100)
      .withMaxDiscardRatio(10)

  //override def scalaCheckInitialSeed = "YOFqcGzXOFtFVgRFxOmODi5100tVovDS3EPOv0Ihk4C="

  lazy val genNat: Gen[Nat] = {
    val recur = Gen.lzy(genNat)
    Gen.frequency(
      (5, Gen.chooseNum(0, Long.MaxValue).map(Nat.fromLong(_))),
      (1, Gen.zip(recur, recur).map { case (a, b) => a + b }),
      (1, Gen.zip(recur, recur).map { case (a, b) => a * b })
    )
  }

  test("constants are right") {
    assertEquals(Nat.zero.maybeLong, Some(0L))
    assertEquals(Nat.one.maybeLong, Some(1L))
    assertEquals(Nat.two_32.maybeLong, Some(1L << 32))
  }
  property("fromInt/maybeLong identity") {
    forAll { (i: Int) =>
      val n = Nat.fromInt(i)  
      n.maybeLong match {
        case None => fail(s"couldn't maybeLong $i")
        case Some(l) => assert(i < 0 || (l.toInt == i))
      }
    }
  }
  property("fromLong/maybeLong identity") {
    forAll { (i: Long) =>
      val n = Nat.fromLong(i)  
      n.maybeLong match {
        case Some(l) => assert(i < 0 || (l == i))
        case None => fail(s"couldn't maybeLong $i")
      }
    }
  }

  def trunc(i: Int): Long = if (i < 0) 0L else i.toLong

  property("toLong => lowBits is identity") {
    val big = 0x80000000L

    assertEquals(Nat.lowBits(big), Int.MinValue, s"${Nat.lowBits(big)}")  
    forAll(Gen.chooseNum(Int.MinValue, Int.MaxValue)) { (i: Int) =>
      val l = Nat.toLong(i)
      assertEquals(Nat.lowBits(Nat.toLong(i)), i, s"long = $l")  
    }
  }

  property("x + y homomorphism") {
    forAll(genNat, genNat) { (ni, nj) =>
      val nk = ni + nj
      assertEquals(nk.toBigInt, ni.toBigInt + nj.toBigInt)
    }
  }

  property("x * y = y * x") {
    forAll(genNat, genNat) { (n1, n2) =>
      assertEquals(n1 * n2, n2 * n1)
    }
  }

  property("x + y = y + x") {
    forAll(genNat, genNat) { (n1, n2) =>
      assertEquals(n1 + n2, n2 + n1)
    }
  }

  property("x * y homomorphism") {
    forAll { (i0: Long, j0: Long) =>
      val i = i0 & Long.MaxValue
      val j = j0 & Long.MaxValue
      val ni = Nat.fromLong(i) 
      val nj = Nat.fromLong(j)
      val nk = ni * nj
      assertEquals(nk.toBigInt, BigInt(i) * BigInt(j))
    }
  }

  property("x.inc == x + 1") {
    forAll(genNat) { n =>
      val i = n.inc  
      val a = n + Nat.one
      assertEquals(i.toBigInt, a.toBigInt)
    }
  }

  property("x.shift_32 == x.toBigInt * 2^32") {
    val n1 = BigInt("8588888260151524380556863712485265508")
    val shift = n1 << 32
    assertEquals(Nat.fromBigInt(n1).shift_32.toBigInt, shift)

    forAll(genNat) { n =>
      val s = n.shift_32
      val viaBigInt = n.toBigInt << 32
      val viaTimes = n * Nat.two_32
      assertEquals(s.toBigInt, viaBigInt, s"viaTimes = $viaTimes")
      assertEquals(s, viaTimes)
    }
  }

  property("Nat.fromBigInt/toBigInt") {
    assertEquals(Nat.fromLong(Long.MaxValue).toBigInt, BigInt(Long.MaxValue))

    forAll { (bi0: BigInt) =>
      val bi = bi0.abs
      val n = Nat.fromBigInt(bi)  
      val b2 = n.toBigInt
      assertEquals(b2, bi)
    }
  }

  property("x.inc.dec == x") {
    forAll(genNat) { n =>
      assertEquals(n.inc.dec, n)  
    }
  }

  property("x.dec.inc == x || x.isZero") {
    forAll(genNat) { n =>
      assert((n.dec.inc == n) || n.isZero)
    }
  }
}