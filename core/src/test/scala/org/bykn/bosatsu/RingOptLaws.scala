package org.bykn.bosatsu

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

class RingOptLaws extends munit.ScalaCheckSuite {

  //override def scalaCheckInitialSeed = "HPupFd7KvUISBG8NqojEImPM5Rw7rhyP0Mknf9iv0-P="
  //override def scalaCheckInitialSeed = "QNSEpXo3Wd33vrtjCPm_X8ZvcxNm2oLGeMEBC0m9DcF="
    override def scalaCheckInitialSeed = "7njzS7m8JI3YbQGsE4WAosfS03suEYbMZdipEOhNISA="
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(500000)
      .withMaxDiscardRatio(10)

  import RingOpt._

  def genExpr[A](genA: Gen[A], depth: Int = 6): Gen[Expr[A]] =
    if (depth < 0) {
      Gen.oneOf(Gen.const(Zero), Gen.const(One), genA.map(Symbol(_)), Arbitrary.arbitrary[BigInt].map(Integer(_)))
    }
    else {
      val inner = Gen.lzy(genExpr(genA, depth - 1))
      val genFn2: Gen[(Expr[A], Expr[A]) => Expr[A]] =
        Gen.oneOf(
          { (x: Expr[A], y: Expr[A]) => Add(x, y) },
          { (x: Expr[A], y: Expr[A]) => Mult(x, y) }
        )

      Gen.oneOf(inner, inner.map(Neg(_)), Gen.zip(inner, inner, genFn2).map { case (a, b, fn) => fn(a, b) })
    }
    

  implicit def arbExpr[A: Arbitrary]: Arbitrary[Expr[A]] =
    Arbitrary(genExpr(Arbitrary.arbitrary[A]))

  implicit val arbCost: Arbitrary[Weights] =
    Arbitrary(for {
      a <- Gen.choose(1, 10)
      m <- Gen.choose(a + 1, 2 * a + 1)
      n <- Gen.choose(1, a)
    } yield Weights(mult = m, add = a, neg = n))

  property("normalization doesn't change values") {
    forAll { (expr: Expr[BigInt], w: Weights) =>
      val normE = normalize(expr, w)
      assertEquals(Expr.toValue(expr), Expr.toValue(normE))  

      // least cost
      val c0 = w.cost(expr)
      val c1 = w.cost(normE)
      assert(c0 >= c1, s"c0 = $c0, c1 = $c1, normE = $normE")

      // Idempotency
      val norm2 = normalize(normE, w)
      val cost2 = w.cost(norm2)
      assertEquals(norm2, normE, s"normE = $normE (c1 = $c1), norm2 = $norm2 (cost2 = $cost2)")

    }
  }
}