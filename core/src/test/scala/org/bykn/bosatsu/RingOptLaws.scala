package org.bykn.bosatsu

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Shrink}
import cats.{Hash, Show}
import cats.syntax.all._

class RingOptLaws extends munit.ScalaCheckSuite {

  // override def scalaCheckInitialSeed = "HPupFd7KvUISBG8NqojEImPM5Rw7rhyP0Mknf9iv0-P="
  // override def scalaCheckInitialSeed = "QNSEpXo3Wd33vrtjCPm_X8ZvcxNm2oLGeMEBC0m9DcF="
  // override def scalaCheckInitialSeed = "7njzS7m8JI3YbQGsE4WAosfS03suEYbMZdipEOhNISA="
  // override def scalaCheckInitialSeed = "Na8mB0VjIRkZ-7lAodvvlGXd1XJ77mZ8dij8x-QGpiM="
  // override def scalaCheckInitialSeed = "z8KHZZ6g7h-Qobfz9Qnc-x7IKmc5ZVzUzw4FGys_1oJ="
  // override def scalaCheckInitialSeed = "hz4zFHijK-UOXwC2oH5-dAdSGJHyT7Z58PjaJv7E2EB="
  // override def scalaCheckInitialSeed = "GEQ98HharP10F4WeQcSp8uWetJ7sxik0ZLCJVaOeUmK="
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

  property("normalizeNeg works") {
    forAll { (e: Expr[Int], w: Weights) =>
      val normNeg = e.normalizeNeg
      assertEquals(-Expr.toValue(e), Expr.toValue(normNeg))
      // we add at most 1 neg node
      assert(w.cost(normNeg) <= (w.cost(e) + w.neg))
    }
  }

  property("cheapNeg works") {
    forAll { (e: Expr[Int], w: Weights) =>
      e.cheapNeg.foreach { neg =>
        assertEquals(-Expr.toValue(e), Expr.toValue(neg))
        // we add no cost
        assert(w.cost(neg) <= w.cost(e))
      }
    }
  }

  property("saveNeg works") {
    forAll { (e: Expr[Int], w: Weights) =>
      e.saveNeg.foreach { neg =>
        assertEquals(-Expr.toValue(e), Expr.toValue(neg))
        // we decrease cost
        assert(w.cost(neg) < w.cost(e))
      }
    }
  }

  test("cheapNeg logic is correct") {
    val a = Symbol("a")
    val b = Symbol("b")

    assertEquals(Neg(a).cheapNeg, Some(a))
    assertEquals(Integer(5).cheapNeg, Some(Integer(-5)))
    assertEquals(One.cheapNeg, Some(Integer(-1)))

    // These are the "at most 1 node" rules
    val addNeg = Add(a, Neg(b))
    assertEquals(addNeg.cheapNeg, Some(Add(a.normalizeNeg, b)))

    // Symbol has no cheap neg
    assertEquals(a.cheapNeg, None)
    // normalizeNeg wraps it
    assertEquals(a.normalizeNeg, Neg(a))
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

  property("flattenAddSub => addAll doesn't increase cost") {
    forAll { (expr: Expr[BigInt], w: Weights) =>
      val (pos, neg) = Expr.flattenAddSub(expr :: Nil)
      val negSum = Expr.addAll(neg).normalizeNeg
      val sum = Expr.addAll(negSum :: pos)
      assert(w.cost(sum) <= w.cost(expr))
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
    def law[A: Hash: Show](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
      val expr = a * b + a * c
      val better = a * (b + c)
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(
        c1 <= c2,
        show"cExpr = $c0, cNorm = $c1, cBetter = $c2, expr=$expr, norm=$norm, better=$better"
      )
    }

    val regressions: List[(Expr[Int], Expr[Int], Expr[Int], Weights)] =
      (
        Integer(-3),
        Neg(Neg(Neg(Neg(Neg(Symbol(0)))))),
        Neg(Neg(Neg(Neg(Symbol(1))))),
        Weights(18, 10, 1)
      ) ::
        Nil

    // TODO: this suffers the same issue as repeatedAdds below
    // regressions.foreach { case (a, b, c, w) => law(a, b, c, w) }

    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      law(a, b, c, w)
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
  property("right/left factorization") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      val expr = b * a + a * c
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

  property("repeated adds are optimized if better".ignore) {
    // This fails because of cases like: a + -(b).
    // we wind up returning 4*a + (-4)*b, but that is two mult and one add
    // but 4*(a + -(b)) is one mult, one add, and one neg, and as long as neg < mult (common), this is better
    forAll(
      Arbitrary.arbitrary[Expr[BigInt]],
      Gen.choose(2, 20),
      Arbitrary.arbitrary[Weights]
    ) { (a: Expr[BigInt], cnt0: Int, w: Weights) =>
      val cnt = if (cnt0 <= 1) 1 else cnt0
      val normA = normalize(a, w)

      val manyAdd = normalize(List.fill(cnt)(a).reduceLeft(Add(_, _)), w)
      val costAdd = w.cost(manyAdd)

      val mult = Expr.checkMult(Integer(cnt), a)
      val costMult = w.cost(mult)
      // we could have always normalized it first by multiplication
      assert(
        costAdd <= costMult,
        show"costAdd = $costAdd, manyAdd = $manyAdd, normA = $normA, costMult = $costMult, mult = $mult"
      )
    }
  }

  test("regression test cases") {
    val cases: List[(Expr[Int], Weights)] =
      (
        Add(One, Neg(Add(Symbol(10), Add(Symbol(-1), Symbol(-1))))),
        Weights(2, 1, 1)
      ) ::
        (
          Neg(Add(Symbol(74), Add(Symbol(74), Symbol(1)))),
          Weights(15, 10, 9)
        ) ::
        (
          Add(
            Add(Add(Symbol(0), Symbol(-13)), One),
            Neg(Add(Symbol(74), Add(Symbol(74), Symbol(1))))
          ),
          Weights(15, 10, 9)
        ) ::
        Nil

    cases.foreach { case (reg, w) =>
      val flat = Expr.flattenAddSub(reg :: Nil)
      val c0 = w.cost(reg)
      // println(show"c0=$c0, reg=$reg, flat=$flat")
      val norm = normalize(reg, w)
      val c1 = w.cost(norm)
      // println(show"c1=$c1, norm=$norm")
      assert(c1 <= c0, show"c1=$c1, norm=$norm, c0=$c0, reg=$reg, flat=$flat")
    }
  }

  property("Expr.hashExpr eqv implementation is structural") {
    forAll { (a: Expr[Int], b: Expr[Int]) =>
      val eqAB = Hash[Expr[Int]].eqv(a, b)
      assertEquals(eqAB, a == b)
      assert(Hash[Expr[Int]].eqv(a, a))

      val hashA = Hash[Expr[Int]].hash(a)
      val hashB = Hash[Expr[Int]].hash(b)

      if (eqAB) {
        assertEquals(hashA, hashB)
      }

      if (hashA != hashB) {
        assert(!eqAB)
      }
    }
  }

  test("Basic MultiSet operations") {
    val ms0 = MultiSet.empty[Int, Int]

    val ms1 = ms0 + 1 + 1 + 2 // {1 -> 2, 2 -> 1}
    assertEquals(ms1.count(1), 2)
    assertEquals(ms1.count(2), 1)
    assertEquals(ms1.count(3), 0)

    val ms2 = ms1 - 1 - 2 - 2 // {1 -> 1, 2 -> -1}
    assertEquals(ms2.count(1), 1)
    assertEquals(ms2.count(2), -1)

    val ms3 = ms2.add(2, 1) // {1 -> 1, 2 -> 0}
    assertEquals(ms3.count(2), 0)
    // Check that zero-count keys are removed from iterator
    assertEquals(ms3.nonZeroIterator.map(_._1).toSet, Set(1))
  }
}
