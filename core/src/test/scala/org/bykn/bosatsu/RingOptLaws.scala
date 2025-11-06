package org.bykn.bosatsu

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Shrink}
import cats.syntax.all._

class RingOptLaws extends munit.ScalaCheckSuite {

  // override def scalaCheckInitialSeed = "HPupFd7KvUISBG8NqojEImPM5Rw7rhyP0Mknf9iv0-P="
  // override def scalaCheckInitialSeed = "QNSEpXo3Wd33vrtjCPm_X8ZvcxNm2oLGeMEBC0m9DcF="
  // override def scalaCheckInitialSeed = "7njzS7m8JI3YbQGsE4WAosfS03suEYbMZdipEOhNISA="
  // override def scalaCheckInitialSeed = "Na8mB0VjIRkZ-7lAodvvlGXd1XJ77mZ8dij8x-QGpiM="
  // override def scalaCheckInitialSeed = "z8KHZZ6g7h-Qobfz9Qnc-x7IKmc5ZVzUzw4FGys_1oJ="
  // override def scalaCheckInitialSeed = "hz4zFHijK-UOXwC2oH5-dAdSGJHyT7Z58PjaJv7E2EB="
  override def scalaCheckInitialSeed =
    "GEQ98HharP10F4WeQcSp8uWetJ7sxik0ZLCJVaOeUmK="
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(5000)
      .withMaxDiscardRatio(10)

  import RingOpt._

  def genExpr[A](genA: Gen[A], depth: Int = 6): Gen[Expr[A]] =
    if (depth < 0) {
      Gen.oneOf(
        Gen.const(Zero),
        Gen.const(One),
        genA.map(Symbol(_)),
        Arbitrary.arbitrary[BigInt].map(Integer(_))
      )
    } else {
      val inner = Gen.lzy(genExpr(genA, depth - 1))
      val genFn2: Gen[(Expr[A], Expr[A]) => Expr[A]] =
        Gen.oneOf(
          (x: Expr[A], y: Expr[A]) => Add(x, y),
          (x: Expr[A], y: Expr[A]) => Mult(x, y)
        )

      Gen.oneOf(
        inner,
        inner.map(Neg(_)),
        Gen.zip(inner, inner, genFn2).map { case (a, b, fn) => fn(a, b) }
      )
    }

  implicit def arbExpr[A: Arbitrary]: Arbitrary[Expr[A]] =
    Arbitrary(genExpr(Arbitrary.arbitrary[A]))

  implicit def shrinkExpr[A: Shrink]: Shrink[Expr[A]] =
    Shrink[Expr[A]] {
      case Neg(x)     => x #:: Shrink.shrink(x).map(Neg(_))
      case Symbol(a)  => Shrink.shrink(a).map(Symbol(_))
      case Integer(n) => Shrink.shrink(n).map(Integer(_))
      case Add(a, b)  =>
        a #:: b #:: (Shrink.shrink(a).zip(Shrink.shrink(b)).map {
          case (sa, sb) => Add(sa, sb)
        })
      case Mult(a, b) =>
        a #:: b #:: (Shrink.shrink(a).zip(Shrink.shrink(b)).map {
          case (sa, sb) => Mult(sa, sb)
        })
      case Zero | One => Stream.empty
    }

  implicit val arbCost: Arbitrary[Weights] =
    Arbitrary(for {
      a <- Gen.choose(1, 10)
      m <- Gen.choose(a + 1, 2 * a + 1)
      n <- Gen.choose(1, a)
    } yield Weights(mult = m, add = a, neg = n))

  property("isOne works") {
    forAll { (e: Expr[Int]) =>
      if (e.isOne) {
        assertEquals(Expr.toValue(e), 1)
      }
    }
  }

  property("isZero works") {
    forAll { (e: Expr[Int]) =>
      if (e.isZero) {
        assertEquals(Expr.toValue(e), 0)
      }
    }
  }

  property("maybeBigInt works") {
    forAll { (e: Expr[BigInt]) =>
      e.maybeBigInt(bi => Some(bi)) match {
        case None     => fail("should always work")
        case Some(bi) =>
          assertEquals(bi, Expr.toValue(e))
      }
    }
  }

  property("flattenMult => multAll identity") {
    forAll { (expr: Expr[BigInt]) =>
      val terms = Expr.flattenMult(expr :: Nil)
      val prod = Expr.multAll(terms)
      assertEquals(Expr.toValue(prod), Expr.toValue(expr))
    }
  }

  property("flattenAddSub => addAll identity") {
    forAll { (expr: Expr[BigInt]) =>
      val (pos, neg) = Expr.flattenAddSub(expr :: Nil)
      val sum = Expr.addAll(pos) - Expr.addAll(neg)
      assertEquals(Expr.toValue(sum), Expr.toValue(expr))
    }
  }

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
      assertEquals(
        norm2,
        normE,
        s"normE = $normE (c1 = $c1), norm2 = $norm2 (cost2 = $cost2)"
      )

    }
  }

  property("flattenAddSub obeys invariant") {
    forAll { (e: Expr[Int]) =>
      val (pos, neg) = Expr.flattenAddSub(e :: Nil)

      pos.foreach {
        case bad @ (Add(_, _) | Neg(_) | Zero) =>
          fail(s"unexpected bad: $bad in pos = $pos")
        case _ => ()
      }
      neg.foreach {
        case bad @ (Add(_, _) | Neg(_) | Integer(_) | Zero) =>
          fail(s"unexpected bad: $bad in neg = $neg")
        case _ => ()
      }

      val unflattened = Add(Expr.addAll(pos), Neg(Expr.addAll(neg)))
      assertEquals(Expr.toValue(unflattened), Expr.toValue(e))
    }
  }

  property("normalize always computes pure constants") {
    forAll { (a: Expr[BigInt], w: Weights) =>
      a.maybeBigInt(_ => None).foreach { bi =>
        assertEquals(normalize(a, w), canonInt(bi))
      }
    }
  }

  property("addition by 0 is always simplified") {
    def law(a: Expr[BigInt], w: Weights) = {
      val na = normalize(a + Zero, w)
      assert(w.cost(na) <= w.cost(a), s"na = $na")
      val na1 = normalize(a + Integer(0), w)
      assert(w.cost(na1) <= w.cost(a))
    }

    law(Symbol(BigInt(0)), Weights(4, 2, 1))
    forAll((a: Expr[BigInt], w: Weights) => law(a, w))
  }
  property("multiplication by 1 is always simplified") {
    forAll { (a: Expr[BigInt], w: Weights) =>
      val na = normalize(a * One, w)
      assert(w.cost(na) <= w.cost(a), s"na = $na")
      val na1 = normalize(a * Integer(1), w)
      assert(w.cost(na1) <= w.cost(a))
    }
  }

  property("multiplication by 0 is always simplified") {
    forAll { (a: Expr[BigInt], w: Weights) =>
      val na = normalize(a * Integer(0), w)
      assert(w.cost(na) <= w.cost(Zero))
      val na1 = normalize(a * Zero, w)
      assert(w.cost(na1) <= w.cost(Zero))
    }
  }

  property("left factorization") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      val expr = a * b + a * c
      val better = a * (b + c)
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 <= c2, s"c0 = $c0, c1 = $c1, c2 = $c2, norm=$norm")
    }
  }

  property("right factorization") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      val expr = b * a + c * a
      val better = (b + c) * a
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 <= c2, s"c0 = $c0, c1 = $c1, c2 = $c2, norm=$norm")
    }
  }

  property("left/right factorization") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      val expr = a * b + c * a
      val better = a * (b + c)
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 <= c2, s"c0 = $c0, c1 = $c1, c2 = $c2, norm=$norm")
    }
  }

  // this one is too hard now
  property("a - a is normalized to zero") {
    forAll { (a: Expr[BigInt], w: Weights) =>
      val expr = a - a
      val better = Zero
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 <= c2, show"c0 = $c0, c1 = $c1, c2 = $c2, a=$a, norm=$norm")
    }
  }

  test("regression test cases") {
    val reg1 = Add(One, Neg(Add(Symbol(10), Add(Symbol(-1), Symbol(-1)))))
    val flat = Expr.flattenAddSub(reg1 :: Nil)
    println(s"flat=$flat")
    val w1 = Weights(2, 1, 1)
    val c0 = w1.cost(reg1)
    val norm1 = normalize(reg1, w1)
    val c1 = w1.cost(norm1)

    assert(c1 <= c0, show"c1=$c1, norm1=$norm1, c0=$c0, reg1=$reg1")

  }
}
