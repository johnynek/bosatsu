package org.bykn.bosatsu

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Shrink}
import cats.{Hash, Show}
import cats.syntax.all._

class RingOptLaws extends munit.ScalaCheckSuite {

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

  implicit def arbMultiSet[A: Arbitrary: Hash, B: Arbitrary: Numeric]
      : Arbitrary[MultiSet[A, B]] =
    Arbitrary(
      Gen
        .listOf(Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[B]))
        .map { kvs =>
          kvs.foldLeft(MultiSet.empty[A, B]) { case (ms, (k, v)) =>
            ms.add(k, v)
          }
        }
    )

  property("isOne works") {
    forAll { (e: Expr[Int]) =>
      if (e.isOne) {
        assertEquals(Expr.toValue(e), 1)
      }
    }
  }

  property("isNegOne works") {
    forAll { (e: Expr[BigInt]) =>
      if (e.isNegOne) {
        assertEquals(Expr.toValue(e), BigInt(-1))
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
      val costE = w.cost(e)
      assert(w.cost(normNeg) <= (costE + w.neg))
      // if we normalizeNeg twice, cost is the same
      assert(w.cost(normNeg.normalizeNeg) <= costE)
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

  property("maybeDivInt works") {
    forAll { (e: Expr[BigInt], div: BigInt) =>
      e.maybeDivInt(div) match {
        case None => ()
        /*
        We wish something like the following were true,
        but it is not because in Add(x, y) we require dividing
        both sides, but we don't check if we only divide the combination
        we could check if both sides are integers, and do the addition
        but that's a bit expensive
          e.maybeBigInt(_ => None).foreach { i =>
            // i should not be able to be divided by div
            assert((i % div) != BigInt(0), show"i = $i")
          }
         */
        case Some(res) =>
          assertEquals(Expr.toValue(e) /% div, (Expr.toValue(res), BigInt(0)))
      }
    }
  }
  property("maybeDivInt handles ±1 and 0 correctly") {
    forAll { (e: Expr[BigInt]) =>
      assertEquals(e.maybeDivInt(0), None)
      assertEquals(e.maybeDivInt(1), Some(e))
      assertEquals(e.maybeDivInt(-1), Some(e.normalizeNeg))
    }
  }

  property("flattenMult => multAll identity") {
    forAll { (expr: Expr[BigInt]) =>
      val (obi, terms) = Expr.flattenMult(expr :: Nil)
      val prod = Expr.multAll(terms)
      assertEquals(
        Expr.toValue(prod) * obi.getOrElse(BigInt(1)),
        Expr.toValue(expr)
      )
    }
  }

  property("flattenMult doesn't increase cost") {
    forAll { (expr: Expr[BigInt], w: Weights) =>
      val (obi, terms) = Expr.flattenMult(expr :: Nil)
      val flatCost = (obi.map(Integer(_)).toList ::: terms)
        .map(w.cost(_))
        .intercalate(w.mult.toLong)
      val orig = w.cost(expr)
      obi.foreach(bi => assert(bi != 1))
      assert(
        flatCost <= orig,
        show"orig = $orig, flatCost = $flatCost, obi=$obi, flat=$terms, expr=$expr"
      )
    }
  }

  property(
    "flattenMult invariant: no One, Zero, Integer or Mult items returned in the list"
  ) {
    forAll { (expr: Expr[BigInt]) =>
      val (_, terms) = Expr.flattenMult(expr :: Nil)

      terms.foreach {
        case bad @ (One | Mult(_, _) | Zero | Integer(_)) =>
          fail(show"found bad node: $bad in terms=$terms, expr=$expr")
        case bad if bad.isZero || bad.isOne =>
          fail(
            show"found bad node (isZero/isOne): $bad in terms=$terms, expr=$expr"
          )
        case _ => ()
      }

      val ints = terms.collect { case Integer(n) => n }

      assertEquals(ints, Nil, show"ints: $ints, terms=$terms")
    }
  }

  property("constMult(One,c) returns canonInt(c)") {
    forAll { (bi: BigInt, w: Weights) =>
      val res = w.constMult(One, bi)
      assertEquals(res, canonInt(bi))
    }
  }
  property("constMult <= add or mult") {
    forAll { (expr: Expr[BigInt], const0: Int, w: Weights) =>
      // make sure the const isn't too big
      val const = BigInt(const0 & 0xfff) * BigInt(const0).signum
      val cm = w.constMult(expr, const)
      val cmCost = w.cost(cm)
      val mult = expr * Integer(const)
      val add = Expr.replicateAdd(const, expr)
      assert(
        cmCost <= w.cost(mult),
        show"cmCost = $cmCost, cm = $cm, mult=$mult, expr=$expr"
      )
      assert(
        cmCost <= w.cost(add),
        show"cmCost = $cmCost, cm = $cm, add=$add, expr=$expr"
      )
    }
  }

  property("absorbMultConst is lawful") {
    forAll { (expr: Expr[BigInt], const: BigInt) =>
      expr.absorbMultConst(const).foreach { xc =>
        assertEquals(Expr.toValue(xc), Expr.toValue(expr * Integer(const)))
      }
    }
  }

  property("multAll is order independent") {
    forAll { (exprs: List[Expr[BigInt]], seed: Long) =>
      val rand = new scala.util.Random(seed)
      val exprs2 = rand.shuffle(exprs)
      val m = Expr.multAll(exprs)
      val m2 = Expr.multAll(exprs2)
      assertEquals(m2, m)
    }
  }

  property("multAll never increases cost") {
    forAll { (expr: Expr[BigInt], w: Weights) =>
      val m = Expr.multAll(expr :: Nil)
      assert(w.cost(m) <= w.cost(expr))
    }
  }

  property("addAll never increases cost") {
    forAll { (expr: Expr[BigInt], w: Weights) =>
      val a = Expr.addAll(expr :: Nil)
      assert(w.cost(a) <= w.cost(expr))
    }
  }

  property("addAll is order independent") {
    forAll { (exprs: List[Expr[BigInt]], seed: Long) =>
      val rand = new scala.util.Random(seed)
      val exprs2 = rand.shuffle(exprs)
      val m = Expr.addAll(exprs)
      val m2 = Expr.addAll(exprs2)
      assertEquals(m2, m)
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
    def law[A: Hash: Show: Numeric](expr: Expr[A], w: Weights) = {
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
        show"normE = $normE (c1 = $c1), norm2 = $norm2 (cost2 = $cost2)"
      )
    }

    val regressions: List[(Expr[Int], Weights)] =
      (
        Neg(Add(Neg(Symbol(7)), Symbol(1))),
        Weights(13, 8, 5)
      ) ::
        (
          Add(
            Neg(Mult(Add(One, One), Symbol(-1))),
            Neg(Mult(Integer(-4000), Add(Zero, Integer(-60))))
          ),
          Weights(4, 2, 2)
        ) :: (
          Mult(Neg(Symbol(4)), Symbol(-1)),
          Weights(8, 4, 1)
        ) :: (
          Mult(Neg(Symbol(0)), Symbol(10)),
          Weights(14, 9, 8)
        ) ::
        (
          Mult(
            Mult(Add(One, One), Mult(One, One)),
            Mult(Neg(Add(Integer(2147483648L), Symbol(-2742))), Integer(-1))
          ),
          Weights(4, 2, 2)
        ) ::
        Nil
    regressions.foreach { case (e, w) => law(e, w) }

    forAll((expr: Expr[BigInt], w: Weights) => law(expr, w))
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
      // we have to able to do at least as well as better
      val c2 = w.cost(better)
      assert(c2 < c0)
      assert(
        c1 <= c0,
        show"cExpr = $c0, cNorm = $c1, cBetter = $c2, expr=$expr, norm=$norm, better=$better"
      )
      /*
      // TODO
      // we can't always reach the better construction yet
      assert(
        c1 <= c2,
        show"cExpr = $c0, cNorm = $c1, cBetter = $c2, expr=$expr, norm=$norm, better=$better"
      )
       */
    }

    val regressions: List[(Expr[Int], Expr[Int], Expr[Int], Weights)] =
      (
        Integer(-3),
        Neg(Neg(Neg(Neg(Neg(Symbol(0)))))),
        Neg(Neg(Neg(Neg(Symbol(1))))),
        Weights(18, 10, 1)
      ) ::
        (
          Add(Mult(Symbol(-9), Symbol(8)), Neg(One)),
          Symbol(0),
          Neg(Neg(Neg(One))),
          Weights(11, 9, 1)
        ) ::
        Nil

    regressions.foreach { case (a, b, c, w) => law(a, b, c, w) }

    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      law(a, b, c, w)
    }
  }

  property("right factorization") {
    def law[A: Hash](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
      val expr = b * a + c * a
      val better = (b + c) * a
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 < c0)
      // TODO: we can't always do this
      // assert(c1 <= c2, s"c0 = $c0, c1 = $c1, c2 = $c2, norm=$norm")
    }

    val regressions: List[(Expr[Int], Expr[Int], Expr[Int], Weights)] =
      (
        Add(Neg(One), Mult(Symbol(-9), Integer(-386941742))),
        Neg(Neg(Neg(Symbol(0)))),
        Integer(1),
        Weights(9, 7, 2)
      ) ::
        Nil

    regressions.foreach { case (a, b, c, w) => law(a, b, c, w) }
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      law(a, b, c, w)
    }
  }

  property("left/right factorization") {
    def law[A: Hash](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
      val expr = a * b + c * a
      val better = a * (b + c)
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(c1 < c0)
      // TODO: we can't quite always get here yet
      // assert(c1 <= c2, s"c0 = $c0, c1 = $c1, c2 = $c2, norm=$norm")
    }
    val regressions: List[(Expr[Int], Expr[Int], Expr[Int], Weights)] =
      (
        Add(
          Mult(Symbol(1), Mult(Symbol(-503), Symbol(519))),
          Mult(Symbol(107), Symbol(922))
        ),
        Neg(Neg(One)),
        Neg(Neg(Neg(Symbol(0)))),
        Weights(9, 7, 1)
      ) ::
        Nil
    regressions.foreach { case (a, b, c, w) => law(a, b, c, w) }
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      law(a, b, c, w)
    }
  }
  property("right/left factorization") {
    def law[A: Hash: Show](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
      val expr = b * a + a * c
      val better = a * (b + c)
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      assert(c2 < c0)
      assert(
        c1 <= c0,
        show"c0 = $c0, c1 = $c1, c2 = $c2, expr=$expr norm=$norm, better=$better"
      )
      /*
      TODO: we can't quite solve this yet
      assert(
        c1 <= c2,
        show"c0 = $c0, c1 = $c1, c2 = $c2, expr=$expr norm=$norm"
      )
       */
    }
    val regressions =
      (
        Integer(3),
        Neg(Neg(Neg(Neg(Symbol(0))))),
        Neg(Symbol(-1)),
        Weights(21, 10, 1)
      ) ::
        Nil

    regressions.foreach { case (a, b, c, w) => law(a, b, c, w) }

    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt], w: Weights) =>
      law(a, b, c, w)
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

  property("normalize is commutative for Add") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], w: Weights) =>
      assertEquals(normalize(a + b, w), normalize(b + a, w))
    }
  }

  property("normalize is commutative for Mult") {
    def law[A: Hash: Show: Numeric](a: Expr[A], b: Expr[A], w: Weights) = {
      val direct = normalize(a * b, w)
      val reverse = normalize(b * a, w)
      assertEquals(Expr.toValue(direct), Expr.toValue(reverse))
      assertEquals(
        w.cost(direct),
        w.cost(reverse),
        show"direct = $direct, reverse = $reverse"
      )
    }
    val regressions: List[(Expr[Int], Expr[Int], Weights)] =
      (
        Neg(Neg(Add(Integer(1030), One))),
        Neg(Neg(Neg(Add(Mult(One, Symbol(-4)), Mult(One, Integer(104)))))),
        Weights(20, 10, 9)
      ) ::
        Nil

    regressions.foreach { case (a, b, w) => law(a, b, w) }
    forAll((a: Expr[BigInt], b: Expr[BigInt], w: Weights) => law(a, b, w))
  }

  property("repeated adds are optimized if better".ignore) {
    // this is hard because (x + y + z + w) added just twice
    // will result in (x + x) + (y + y) + (z + z) + (w + w)
    // but that can't be simplified term by term to 2x + 2y ...
    // because 2x costs more than x + x. But if we could do it, then we could
    // factor the 2s out, and we do save. So, this is an example of a local
    // greedy optimization.
    //
    // also, in the past
    // This fails because of cases like: a + -(b).
    // we wind up returning 4*a + (-4)*b, but that is two mult and one add
    // but 4*(a + -(b)) is one mult, one add, and one neg, and as long as neg < mult (common), this is better
    forAll(
      Arbitrary.arbitrary[Expr[BigInt]],
      Gen.choose(2, 20),
      Arbitrary.arbitrary[Weights]
    ) { (a: Expr[BigInt], cnt0: Int, w: Weights) =>
      val cnt = if (cnt0 <= 1) 1 else cnt0

      val manyAdd = normalize(List.fill(cnt)(a).reduceLeft(Add(_, _)), w)
      val costAdd = w.cost(manyAdd)

      val mult = Expr.checkMult(Integer(cnt), a)
      val costMult = w.cost(mult)
      // we could have always normalized it first by multiplication
      assert(
        costAdd <= costMult,
        show"cnt = $cnt, costAdd = $costAdd, manyAdd = $manyAdd,costMult = $costMult, mult = $mult"
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

  property("MultiSet add/remove identity") {
    forAll { (ms: MultiSet[Int, BigInt], k: Int, v: BigInt) =>
      assertEquals(ms.add(k, v).remove(k, v).count(k), ms.count(k))
    }
  }

  property("MultiSet subtraction") {
    forAll { (ms1: MultiSet[Int, BigInt], ms2: MultiSet[Int, BigInt], k: Int) =>
      assertEquals((ms1 -- ms2).count(k), ms1.count(k) - ms2.count(k))
    }
  }

  property("Expr.fromBigInt is correct for BigInt") {
    forAll { (n: BigInt) =>
      assertEquals(Expr.fromBigInt[BigInt](n), n)
    }
  }

// In RingOptLaws
  property("replicateAdd computes correct value") {
    forAll(genExpr(Gen.choose(0, 10), 2), Gen.choose(0, 100)) {
      (e: Expr[Int], n: Int) =>
        val cnt = BigInt(n)
        val rep = Expr.replicateAdd(cnt, e)
        assertEquals(Expr.toValue(rep), Expr.toValue(e) * n)
    }
  }

  property("multThreshold works") {
    forAll(
      genExpr(Gen.choose(-10, 10), 5),
      Gen.choose(-100, 100),
      Arbitrary.arbitrary[Weights]
    ) { (e: Expr[Int], n: Int, w: Weights) =>
      val cnt = BigInt(n)
      if (w.multThreshold <= cnt.abs) {
        // multiplication is always lower cost
        val rep = Expr.replicateAdd(cnt, e)
        val mult = Expr.multAll(e :: Integer(cnt) :: Nil)
        assert(w.cost(mult) <= w.cost(rep))
      }
    }
  }
  test("checkMult(-1, -1) == One") {
    val e = Expr.checkMult(Integer(-1), Integer(-1))
    assertEquals(e, One)
  }
  test("constMult(One,c) returns canonInt(c)") {
    forAll { (c: Int) =>
      val w = Weights.default
      val bi = BigInt(c)
      val res = w.constMult(One, bi)
      assertEquals(res, canonInt(bi))
    }
  }
  test("normalize does not throw on very deep trees") {
    val depth = 50000
    val leaf = Symbol(BigInt(1))
    val deep =
      (1 to depth).foldLeft(leaf: Expr[BigInt])((acc, _) => Add(acc, leaf))
    val w = Weights(mult = 5, add = 1, neg = 1)
    // Should terminate; before fix may overflow cost
    val n = normalize(deep, w)
    assertEquals(Expr.toValue(n), Expr.toValue(deep))
  }

  property("unConstMult is the opposite of constMult") {
    forAll { (e: Expr[BigInt], w: Weights) =>
      e.unConstMult.foreach { case (bi, x) =>
        val bix = Expr.toValue(x)
        assertEquals(
          Expr.toValue(w.constMult(x, bi)),
          bix * bi,
          show"e=$e, bi=$bi, x=$x, bix=$bix"
        )
      }
    }
  }

  property("groupSum law") {
    forAll { (es: List[Expr[BigInt]]) =>
      val (const, terms) = Expr.groupSum(es)
      val combined =
        terms.nonZeroIterator.foldLeft(Integer(const): Expr[BigInt]) {
          case (acc, (e, n)) =>
            acc + (e * Integer(n))
        }

      assertEquals(
        Expr.toValue(combined),
        Expr.toValue(es.foldLeft(Zero: Expr[BigInt])(_ + _))
      )
    }
  }
}
