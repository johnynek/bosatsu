package dev.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

object VarianceGen {
  val gen: Gen[Variance] = Gen.oneOf(
    Variance.Phantom,
    Variance.Contravariant,
    Variance.Covariant,
    Variance.Invariant
  )

  implicit val arbVar: Arbitrary[Variance] = Arbitrary(gen)
}

class VarianceTest extends munit.ScalaCheckSuite {
  import VarianceGen.arbVar

  val V = Variance.varianceBoundedSemilattice

  test("variance is commutative") {
    forAll { (v1: Variance, v2: Variance) =>
      assertEquals(V.combine(v1, v2), V.combine(v2, v1))
    }
  }

  test("variance combine is associative") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      assertEquals(
        V.combine(v1, V.combine(v2, v3)),
        V.combine(V.combine(v1, v2), v3)
      )
    }
  }

  test("variance * is associative") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      assertEquals(((v1 * v2) * v3), (v1 * (v2 * v3)))
    }
  }

  test("combine matches combineAll") {
    forAll { (vs: List[Variance]) =>
      assertEquals(V.combineAllOption(vs), vs.reduceOption(_ + _))
    }
  }

  test("variance is distributive") {
    val prop = forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      val left = v1 * (v2 + v3)
      val right = (v1 * v2) + (v1 * v3)
      assertEquals(left, right, s"$left != $right")
    }

    // previous failures:
    {
      val v1 = Variance.in
      val v2 = Variance.phantom
      val v3 = Variance.co

      val left = v1 * (v2 + v3)
      val right = (v1 * v2) + (v1 * v3)
      assertEquals(left, right, s"$left != $right")
    }
    prop
  }

  test("negate is the same as muliplying by contra") {
    forAll { (v1: Variance) =>
      assertEquals(-v1, (Variance.contra * v1))
    }
  }

  test("double negation is identity") {
    forAll { (v1: Variance) =>
      assertEquals(-(-v1), v1)
    }
  }

  test("times is commutative") {
    forAll { (v1: Variance, v2: Variance) =>
      assertEquals((v1 * v2), (v2 * v1))
    }
  }

  test("variance is idempotent") {
    forAll { (v1: Variance) =>
      assertEquals(V.combine(v1, v1), v1)
    }
  }

  test("phantom is bottom") {
    assertEquals(V.empty, Variance.Phantom)
    forAll { (v1: Variance) =>
      assertEquals(V.combine(V.empty, v1), v1)
      assertEquals(V.combine(v1, V.empty), v1)
    }
  }

  test("invariant is top") {
    forAll { (v1: Variance) =>
      assertEquals(V.combine(Variance.Invariant, v1), Variance.Invariant)
      assertEquals(V.combine(v1, Variance.Invariant), Variance.Invariant)
    }
  }

  test("negate combine gives either Phantom or Invariant") {
    forAll { (v1: Variance) =>
      if (v1 == Variance.Phantom) assertEquals(-v1, v1)
      else {
        assertEquals(V.combine(-v1, v1), Variance.Invariant)
        assertEquals(V.combine(v1, -v1), Variance.Invariant)
      }
    }
  }

  test("covariant combines to get either covariant or invariant") {
    assertEquals(
      V.combine(
        Variance.Covariant,
        Variance.Contravariant
      ),
      Variance.Invariant
    )
    val results = Set(Variance.co, Variance.in)
    forAll { (v1: Variance) =>
      assert(results(V.combine(v1, Variance.Covariant)))
    }
  }

  test("contravariant combines to get either contravariant or invariant") {
    assertEquals(
      V.combine(
        Variance.Covariant,
        Variance.Contravariant
      ),
      Variance.Invariant
    )
    val results = Set(Variance.contra, Variance.in)
    forAll { (v1: Variance) =>
      assert(results(V.combine(v1, Variance.Contravariant)))
    }
  }

  test("ordering is lawful") {
    forAll { (v1: Variance, v2: Variance, v3: Variance) =>
      OrderingLaws.forOrder(v1, v2, v3)
    }
  }
}
