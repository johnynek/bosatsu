package dev.bosatsu

import cats.{Hash, Order, Show}
import cats.data.NonEmptyList
import cats.syntax.all._
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Shrink}

class RingOptLaws extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1000)
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

  implicit def shrinkExpr[A](implicit
      shrinkA: Shrink[A],
      ordA: Order[A]
  ): Shrink[Expr[A]] =
    Shrink.withLazyList[Expr[A]] {
      case Zero       => LazyList.empty
      case One        => Zero #:: LazyList.empty
      case Symbol(a)  => LazyList.from(shrinkA.shrink(a)).map(Symbol(_))
      case Integer(n) => LazyList.from(Shrink.shrink(n)).map(Integer(_))
      case Neg(x)     =>
        val shrinkX = LazyList.from(Shrink.shrink(x))
        x #:: Generators.interleave(
          shrinkX,
          shrinkX.map(Neg(_))
        )
      case add @ Add(a, b) =>
        val shrinkAdd = LazyList
          .from(Shrink.shrink((a, b)))
          .map { case (sa, sb) => Add(sa, sb) }

        val tail = a #:: b #:: Generators.interleaveAll(
          LazyList.from(Shrink.shrink(a)) ::
            LazyList.from(Shrink.shrink(b)) ::
            shrinkAdd ::
            Nil
        )

        val normAdd = (add: Expr[A]).basicNorm(using ordA)
        if (normAdd =!= add) (normAdd #:: tail)
        else tail

      case mult @ Mult(a, b) =>
        val shrinkMult = LazyList
          .from(Shrink.shrink((a, b)))
          .map { case (sa, sb) => Mult(sa, sb) }
        val tail = a #:: b #:: Generators.interleaveAll(
          LazyList.from(Shrink.shrink(a)) ::
            LazyList.from(Shrink.shrink(b)) ::
            shrinkMult ::
            Nil
        )
        val normMult = (mult: Expr[A]).basicNorm(using ordA)
        if (normMult =!= mult) (normMult #:: tail)
        else tail
    }

  implicit val arbCost: Arbitrary[Weights] =
    Arbitrary(for {
      a <- Gen.choose(1, 10)
      m <- Gen.choose(a + 1, 2 * a + 1)
      n <- Gen.choose(1, a)
    } yield Weights(mult = m, add = a, neg = n))

  implicit val shrinkWeights: Shrink[Weights] =
    Shrink.withLazyList[Weights] { case Weights(m, a, n) =>
      Shrink
        .shrink((m, a, n))
        .map { case (m0, a0, n0) =>
          val n = if (n0 < 1) 1 else n0
          val a = if (a0 < n) n else a0
          val m = if (m0 < a) a else m0
          (m, a, n)
        }
        .sliding(2)
        .takeWhile {
          case Seq((m0, a0, n0), (m1, a1, n1)) =>
            (m1 < m0) || (a1 < a0) || (n1 < n0)
          case _ => false
        }
        .map(_(0))
        .map { case (m, a, n) => Weights(m, a, n) }
        .to(LazyList)
    }

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

  property("add homomorphism") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt]) =>
      assertEquals(Expr.toValue(a + b), Expr.toValue(a) + Expr.toValue(b))
    }
  }
  property("mult homomorphism") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt]) =>
      assertEquals(Expr.toValue(a * b), Expr.toValue(a) * Expr.toValue(b))
    }
  }

  property("sub homomorphism") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt]) =>
      assertEquals(Expr.toValue(a - b), Expr.toValue(a) - Expr.toValue(b))
    }
  }

  property("neg homomorphism") {
    forAll { (a: Expr[BigInt]) =>
      assertEquals(Expr.toValue(-a), -Expr.toValue(a))
    }
  }

  property("Integer homomorphism") {
    forAll { (i: BigInt) =>
      assertEquals(Expr.toValue[BigInt](Integer(i)), i)
    }
  }

  property("Symbol homomorphism") {
    forAll { (i: BigInt) =>
      assertEquals(Expr.toValue(Symbol(i)), i)
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

  property("cheapNeg applys negation and doesn't increase cost") {
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
    assertEquals(addNeg.cheapNeg, Some(Add(b, a.normalizeNeg)))

    // Symbol has no cheap neg
    assertEquals(a.cheapNeg, None)
    // normalizeNeg wraps it
    assertEquals(a.normalizeNeg, Neg(a))
  }

  property("basicNorm normalizes to integers if possible") {
    forAll { (e: Expr[BigInt]) =>
      val normE = e.basicNorm
      e.maybeBigInt(_ => None).foreach { bi =>
        assertEquals(normE, canonInt(bi))
      }
    }
  }

  property("basicNorm doesn't change value") {
    forAll { (e: Expr[BigInt]) =>
      val normE = e.basicNorm
      assertEquals(Expr.toValue(normE), Expr.toValue(e))
    }
  }

  property("basicNorm is idempotent") {
    forAll { (e: Expr[BigInt]) =>
      val normE = e.basicNorm
      val norm2 = normE.basicNorm
      if (normE != norm2) sys.error(show"normE=$normE, norm2=$norm2")
      assertEquals(norm2, normE, show"normE=$normE, norm2=$norm2")
    }
  }

  property("basicNorm doesn't increase cost") {
    forAll { (e: Expr[BigInt], w: Weights) =>
      val normE = e.basicNorm
      val cost0 = w.cost(e)
      val cost1 = w.cost(normE)
      assert(cost1 <= cost0, show"normE($cost1) = $normE, e($cost0) = $e")

      val size0 = e.graphSize
      val size1 = normE.graphSize
      assert(size1 <= size0)
    }
  }

  property("costOf(opCounts(e)) == cost(e)") {
    forAll { (e: Expr[BigInt], w: Weights) =>
      val counts = Expr.opCounts(e)
      assertEquals(w.costOf(counts), w.cost(e), s"e=$e, counts=$counts")
    }
  }

  property(
    "undistribute doesn't increase graph size, or change the value"
  ) {
    forAll { (e: Expr[BigInt]) =>
      val unE = Expr.undistribute(e)
      /*
      val costE = w.cost(e)
      val costUn = w.cost(unE)
      assert(costUn <= costE)
       */
      val gsE = e.graphSize
      val gsUn = unE.graphSize
      assert(gsUn <= gsE)
      assertEquals(Expr.toValue(unE), Expr.toValue(e))
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

  property("maybeDivInt works and never increases cost") {
    forAll { (e: Expr[BigInt], div: BigInt, w: Weights) =>
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
          assert(w.cost(res) <= w.cost(e), show"res=$res")
          val remult = res.bestEffortConstMult(div)
          assertEquals(Expr.toValue(remult), Expr.toValue(e))
          assert(w.cost(remult) <= w.cost(e), show"remult=$remult")
          assertEquals(Expr.toValue(e) /% div, (Expr.toValue(res), BigInt(0)))
      }
    }
  }

  test("maybeDivInt remult regression #1742") {
    val div = BigInt(4)
    val e: Expr[BigInt] =
      Mult(Integer(-div), Mult(Symbol(BigInt(1)), Symbol(BigInt(2))))
    val w = Weights(mult = 3, add = 2, neg = 2)

    val res = e.maybeDivInt(div).getOrElse(fail("expected a quotient"))
    val remult = res.bestEffortConstMult(div)

    assertEquals(Expr.toValue(remult), Expr.toValue(e))
    assert(w.cost(remult) <= w.cost(e), show"e=$e, res=$res, remult=$remult")
  }

  property("unConstMult <=> maybeDivInt relationship") {
    // if unConstMult works, we could divide by the same const using maybeDivInt
    val law1Prop = forAll { (e: Expr[BigInt]) =>
      e.unConstMult.foreach { case (coeff, e1) =>
        if (coeff != 0)
          e.maybeDivInt(coeff).foreach { eDiv =>
            assertEquals(Expr.toValue(eDiv), Expr.toValue(e1))
          }
      }
    }

    // if maybeDivInt works then unConstMult works and div divides the constant
    def law2[A: Numeric](e: Expr[A], div: BigInt, w: Weights) =
      e.maybeDivInt(div).foreach { _ => // eDiv =>
        e.unConstMult match {
          case None =>
            // use w to hide a warning
            assert(w != null)
          /*
            We wish that if (e/div) == e1, then e.unConstMult should be defined and the big int should be divisible by div.
            Then, we could have some laws maybe like the below.

            The problem is, (x*y + x*z) for some integer x, is easy to run maybeDivInt(x) on. But
            unConstMult requires seeing that y is equivalent to z, which we can't (easily yet) see.
            If we had some normalizing equivalence, then we could possibly tighten this law down. But
            in the mean time, it means unConstMult is generally weaker than maybeDivInt. It's not so surprising
            since maybeDivInt has more information: it has the divisor we are trying to remove, not trying to compute
            the largest divisor we could pull out.

            val isInt = e.maybeBigInt(_ => None).isDefined

            val prod = e1 * Integer(div)
            val prodCost = w.cost(prod)
            val eCost = w.cost(e)
            val checkMultIsExpensive = prodCost > eCost
            assert(
              (div == 1) || (div == -1) || isInt || checkMultIsExpensive,
              show"expected unConstMult to work:\n\te($eCost) = $e\n\te1 = $e1\n\tprod($prodCost) = $prod"
            )
           */
          case Some((bi, rest)) =>
            // bi mod (div) == 0
            assertEquals(
              Expr.toValue(rest.bestEffortConstMult(bi)),
              Expr.toValue(e)
            )
          /* we would like something like this law to be true, but it's not
            consider a * b. maybeDivInt might pull out of a, and unConstMult may
            only pull out of b, and they have no relationship to each other because
            we can't guarantee we can unConstMult and totally normalize
            assert(
              (bi == 0) || ((div % bi) == 0) || ((bi % div) == 0),
              show"e=$e, div=$div, eDiv=$eDiv, bi=$bi, rest=$rest"
            )
           */
        }
      }

    val regressions: List[(Expr[Int], BigInt, Weights)] =
      (
        Mult(
          Add(
            Mult(Integer(-4294967296L), Symbol(0)),
            Mult(Integer(-2147483649L), Mult(Symbol(0), Integer(2147483648L)))
          ),
          Integer(-2)
        ),
        BigInt(4),
        Weights(9, 5, 5)
      ) ::
        (
          Add(
            Mult(
              Mult(Symbol(-343), Integer(BigInt("214"))),
              Integer(BigInt("214"))
            ),
            Integer(BigInt("214748364700"))
          ),
          BigInt(2),
          Weights(9, 4, 3)
        ) :: (
          Add(
            Mult(Symbol(0), Add(Integer(4294967295L), One)),
            Mult(Integer(3), Mult(Symbol(0), Integer(2147483648L)))
          ),
          BigInt(2),
          Weights(6, 3, 2)
        ) ::
        /*
      (
        Add(Neg(Mult(Mult(Neg(Add(Integer(BigInt("-4385718503068389691520991546287788452979461469253396545456509071335915520")),One)),Neg(Neg(Neg(Symbol(603))))),
                     Neg(Neg(Mult(Symbol(-343),Integer(BigInt("2147483648"))))))),
          Mult(Mult(Add(Integer(BigInt("7862135616769890340")),Integer(BigInt("-2259103778483541779407378567421224468"))),Neg(One)),Add(Neg(Mult(One,One)),Neg(Integer(BigInt("-2147483649")))))),
        BigInt(2),
        Weights(9,4,3)
      ) ::
         */
        Nil

    regressions.foreach { case (e, div, w) => law2(e, div, w) }
    val law2Prop = forAll((e: Expr[BigInt], div: BigInt, w: Weights) => law2(e, div, w))
    Prop.all(law1Prop, law2Prop)
  }

  property("maybeDivInt handles ±1 and 0 correctly") {
    forAll { (e: Expr[BigInt]) =>
      assertEquals(e.maybeDivInt(0), None)
      assertEquals(e.maybeDivInt(1), Some(e))
      assertEquals(e.maybeDivInt(-1), e.cheapNeg)
    }
  }

  property("maybeDivInt can always remove * div") {
    forAll { (e: Expr[BigInt], div: BigInt) =>
      if (div != 0) {
        assert((e * Integer(div)).maybeDivInt(div).isDefined)
        assert((Integer(div) * e).maybeDivInt(div).isDefined)
      }
    }
  }

  property("flattenMult => multAll identity") {
    forAll { (expr: Expr[BigInt]) =>
      val (bi, terms) = Expr.flattenMult(expr :: Nil)
      val prod = Expr.multAll(terms.map(_.toExpr))
      assertEquals(
        Expr.toValue(prod) * bi,
        Expr.toValue(expr),
        show"bi=$bi, terms=$terms"
      )
    }
  }

  property("flattenMult doesn't increase cost") {
    def law[A: Show: Order](expr: Expr[A], w: Weights) = {
      val (bi, terms) = Expr.flattenMult(expr :: Nil)
      val flatExpr = normConstMult(
        (Integer(bi) :: terms.map(_.toExpr))
          .foldLeft(One: Expr[A])(Expr.checkMult(_, _)),
        w
      )

      val flatCost = w.cost(flatExpr)
      val orig = w.cost(expr)
      assert(
        flatCost <= orig,
        show"orig = $orig, flatCost = $flatCost, bi=$bi, flat=$terms, flatExpr=$flatExpr, expr=$expr"
      )
    }
    val regressions: List[(Expr[Int], Weights)] =
      (
        Mult(
          Add(Zero, Symbol(0)),
          Add(Mult(Symbol(0), Integer(-1)), Integer(0))
        ),
        Weights(3, 2, 1)
      ) ::
        Nil

    regressions.foreach { case (e, w) => law(e, w) }
    forAll((e: Expr[BigInt], w: Weights) => law(e, w))
  }

  property("constMult(One,c) returns canonInt(c)") {
    forAll { (bi: BigInt, w: Weights) =>
      val res = w.constMult[BigInt](One, bi)
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
      val cMult = w.cost(mult)
      assert(
        cmCost <= cMult,
        show"const = $const, cm($cmCost) = $cm, mult($cMult)=$mult, expr=$expr"
      )
      val cAdd = w.cost(add)
      assert(
        cmCost <= cAdd,
        show"const = $const, cm($cmCost) = $cm, add($cAdd)=$add, expr=$expr"
      )
    }
  }

  property("norm(w.constMult(e, c)) == w.constMult(norm(e), c)".ignore) {
    // This isn't true now. There are cases where there are many different
    // expressions, which have the same value and cost. But multiplying some
    // by constants
    // e.g.
    // normE=(-(([0] * [0])) + [0])
    // rhs(18)=(-3 * (([0] * [0]) + -([0])))
    // lhs(15)=((([0] + -1) * [0]) * -3)
    forAll { (e: Expr[Int], c: BigInt, w: Weights) =>
      val normE = normalize(e, w)
      val rhs = w.constMult(normE, c)
      val costRhs = w.cost(rhs)

      val lhs = normalize(w.constMult(normE, c), w)
      val costLhs = w.cost(lhs)

      assert(
        costRhs <= costLhs,
        show"normE=$normE, rhs($costRhs)=$rhs, lhs($costLhs)=$lhs"
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
    forAll { (exprs0: List[Expr[BigInt]], seed: Long) =>
      val exprs = exprs0.take(20) // don't go too nuts on long lists
      val rand = new scala.util.Random(seed)
      val exprs2 = rand.shuffle(exprs)
      val m = Expr.addAll(exprs)
      val m2 = Expr.addAll(exprs2)
      assertEquals(m2, m)
    }
  }

  property("normalization doesn't change values") {
    def law[A: Hash: Show: Numeric: Order](expr: Expr[A], w: Weights) = {
      val normE = normalize(expr, w)
      assertEquals(Expr.toValue(normE), Expr.toValue(expr))

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
        Mult(
          Add(Mult(Integer(-4), Symbol(0)), Mult(Symbol(1), Add(One, One))),
          Neg(Symbol(0))
        ),
        Weights(18, 10, 9)
      ) ::
        (
          // -[0] + -([-1] * [1] * 2)
          Add(
            Neg(Symbol(0)),
            Neg(Mult(Add(Symbol(-1), Symbol(1)), Integer(2)))
          ),
          Weights(21, 10, 9)
        ) ::
        (
          // the challenge with this one is that we get
          // (([0] * -3) + -(([1] + [1]))) but if we lift the negate
          // all the way out, we could see that -([0] + [0] + [0] + [1] + [1])
          // is better.
          Add(Neg(Symbol(3)), Neg(Mult(Integer(2), Add(Symbol(3), Symbol(1))))),
          Weights(21, 10, 6)
        ) ::
        (
          Add(
            Symbol(0),
            Add(
              Add(
                Mult(Symbol(0), Mult(Symbol(-1), Integer(3))),
                Add(Neg(Symbol(1)), Neg(Symbol(-1)))
              ),
              Add(Integer(0), Neg(Neg(Neg(One))))
            )
          ),
          Weights(7, 4, 2)
        ) ::
        (
          Neg(Neg(Neg(Add(Mult(Integer(1), Integer(1)), Symbol(0))))),
          Weights(18, 10, 8)
        ) ::
        (
          Add(Neg(Add(Mult(Symbol(0), Symbol(0)), Symbol(1))), Symbol(-1)),
          Weights(6, 4, 1)
        ) ::
        (
          Neg(Add(Symbol(0), Add(Mult(Symbol(9), Symbol(9)), Zero))),
          Weights(7, 3, 1)
        ) ::
        (
          Add(Integer(-1), Neg(Mult(Symbol(0), Add(One, One)))),
          Weights(11, 10, 8)
        ) ::
        (
          Neg(Add(Add(Symbol(-1), Symbol(0)), Symbol(-1))),
          Weights(3, 2, 2)
        ) ::
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

  property("x - x is normalized to zero for symbols") {
    forAll { (bi: BigInt, w: Weights) =>
      val n = normalize(Symbol(bi) + (-Symbol(bi)), w)
      assertEquals(n, Zero)
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
      val naBase = normalize(a, w)
      val na = normalize(a + Zero, w)
      assert(w.cost(na) <= w.cost(naBase), s"na = $na, base = $naBase")
      val na1 = normalize(a + Integer(0), w)
      assert(w.cost(na1) <= w.cost(naBase))
    }

    law(Neg(Add(Symbol(0), Symbol(1))), Weights(2, 1, 1))
    law(Symbol(BigInt(0)), Weights(4, 2, 1))
    // issue #1589
    law(
      Neg(
        Add(
          Symbol(BigInt("2147483648")),
          Add(
            Mult(
              Symbol(
                BigInt(
                  "-123593876035527231221720838753456864561865872211442256816"
                )
              ),
              Symbol(BigInt("1615848243864774095"))
            ),
            Add(Symbol(BigInt("2147483648")), One)
          )
        )
      ),
      Weights(8, 4, 3)
    )
    // issue #1589
    law(
      Neg(
        Mult(
          Neg(Neg(One)),
          Neg(
            Add(
              Neg(
                Add(
                  Symbol(BigInt("9223372036854775808")),
                  Symbol(BigInt("-9223372036854775809"))
                )
              ),
              Neg(
                Mult(
                  Add(One, One),
                  Add(
                    Zero,
                    Symbol(
                      BigInt(
                        "24614105630533305221543030638953511488693039740333095040"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      Weights(9, 4, 1)
    )
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
    def law[A: Hash: Show: Order](
        a: Expr[A],
        b: Expr[A],
        c: Expr[A],
        w: Weights
    ) = {
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
      // TODO
      // we can't always reach the beter construction yet
      assert(
        c1 <= c2,
        show"cExpr = $c0, exp.basicNorm = ${expr.basicNorm} cNorm = $c1, cBetter = $c2, expr=$expr, norm=$norm, better=$better, sumProd=${SumProd(expr :: Nil)}" +
          show"\n\tsplits = ${SumProd(expr :: Nil).splits}"
      )
    }

    val regressions: List[(Expr[Int], Expr[Int], Expr[Int], Weights)] =
      (
        Add(One, Add(Mult(Symbol(-1), Symbol(1)), Mult(Symbol(0), Integer(5)))),
        Integer(-1),
        Neg(Symbol(0)),
        Weights(18, 10, 4)
      ) ::
        (
          Add(Mult(Symbol(0), Symbol(0)), Add(Symbol(0), Symbol(1))),
          Symbol(1),
          One,
          Weights(11, 5, 1)
        ) ::
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
    def law[A: Hash: Order](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
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
    def law[A: Hash: Order](a: Expr[A], b: Expr[A], c: Expr[A], w: Weights) = {
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
    def law[A: Hash: Show: Order](
        a: Expr[A],
        b: Expr[A],
        c: Expr[A],
        w: Weights
    ) = {
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
    def law[A: Hash: Show: Order](a: Expr[A], w: Weights) = {
      val expr = a - a
      val better = Zero
      val norm = normalize(expr, w)
      val c0 = w.cost(expr)
      val c1 = w.cost(norm)
      val c2 = w.cost(better)
      // we have to able to do at least as well as better
      val sumProd = SumProd(expr.basicNorm :: Nil)
      // println(show"splits = ${sumProd.splits}")
      assert(c2 < c0)
      assert(
        c1 <= c2,
        show"c1=$c1, c2=$c2, a=$a\n\texpr($c0)=$expr\n\tnorm($c1)=$norm\n\tbasicNorm(a) = ${a.basicNorm}, basicNorm(expr)=${expr.basicNorm}\n\tsumProd(expr) = ${sumProd}"
      )

    }

    val regressions: List[(Expr[Int], Weights)] =
      (
        Mult(
          Add(
            Integer(1),
            Mult(
              Add(Neg(Symbol(0)), Add(Symbol(1), Integer(-2))),
              Add(Symbol(0), Add(Neg(Symbol(-1)), Integer(-3)))
            )
          ),
          Symbol(0)
        ),
        Weights(8, 6, 4)
      ) ::
        (
          Mult(Symbol(0), Add(Add(Symbol(0), Neg(Symbol(-1))), Integer(1))),
          Weights(3, 1, 1)
        ) ::
        Nil

    regressions.foreach { case (a, w) => law(a, w) }
    forAll((a: Expr[BigInt], w: Weights) => law(a, w))
  }

  property("normalize is commutative for Add") {
    def law[A: Hash: Order: Show: Numeric](a: Expr[A], b: Expr[A], w: Weights) = {
      val left = normalize(a + b, w)
      val costLeft = w.cost(left)
      val right = normalize(b + a, w)
      val costRight = w.cost(right)
      assertEquals(
        Expr.toValue(left),
        Expr.toValue(right),
        show"left($costLeft)=$left right($costRight)=$right"
      )
      assertEquals(
        costLeft,
        costRight,
        show"left($costLeft)=$left right($costRight)=$right"
      )
    }

    val regressions: List[(Expr[BigInt], Expr[BigInt], Weights)] =
      (
        Neg(
          Add(
            Mult(Integer(-4), Symbol(BigInt(9223372036854775807L))),
            Mult(Integer(5), Symbol(BigInt(0)))
          )
        ),
        Add(Symbol(BigInt(-2147483648)), Neg(Symbol(BigInt(-1)))),
        Weights(15, 7, 4)
      ) ::
        (
          Integer(-1),
          Add(
            Mult(
              Integer(BigInt("-13671113205015572068287150262499573790")),
              Symbol(BigInt("44016100005173543111648107876578522562"))
            ),
            Neg(
              Symbol(
                BigInt("-155782816531683741903004619612818253648560646266504575984")
              )
            )
          ),
          Weights(2, 1, 1)
        ) ::
        Nil

    regressions.foreach { case (a, b, w) => law(a, b, w) }
    forAll((a: Expr[BigInt], b: Expr[BigInt], w: Weights) => law(a, b, w))
  }

  property("normalize is commutative for Mult") {
    def law[A: Hash: Show: Numeric: Order](
        a: Expr[A],
        b: Expr[A],
        w: Weights
    ) = {
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

  property("repeated adds are optimized if better") {
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
    def law[A: Show: Hash: Order](a: Expr[A], cnt0: Int, w: Weights) = {
      val cnt = if (cnt0 <= 1) 1 else cnt0

      val adds = List.fill(cnt)(a).reduceLeft(Add(_, _))
      val manyAdd = normalize(adds, w)
      val costAdd = w.cost(manyAdd)

      val mult = Expr.checkMult(Integer(cnt), a)
      val costMult = w.cost(mult)
      // we could have always normalized it first by multiplication
      val sumProd = SumProd(adds :: Nil)
      assert(
        costAdd <= costMult,
        show"a = $a, cnt = $cnt, costAdd = $costAdd, manyAdd = $manyAdd,costMult = $costMult, mult = $mult" +
          show"\nadds.basicNorm = ${adds.basicNorm} sumProd=$sumProd splits=${sumProd.splits}" +
          show"\nsumProd(undist)=${SumProd(Expr.undistribute(adds) :: Nil)}"
      )
    }

    val regressions: List[(Expr[BigInt], Int, Weights)] =
      (
        Add(
          Mult(Symbol(BigInt(0)), Integer(-14)),
          Add(
            Symbol(BigInt(-1)),
            Add(Symbol(BigInt(-1)), Symbol(BigInt(1)))
          )
        ),
        2,
        Weights(16, 8, 1)
      ) ::
        (
          Add(
            Symbol(BigInt(-1)),
            Add(Symbol(BigInt(0)), Symbol(BigInt(0)))
          ),
          2,
          Weights(12, 9, 5)
        ) ::
        (
          Mult(
            Add(One, Mult(Symbol(BigInt(0)), Integer(-2))),
            Add(Symbol(BigInt(0)), Integer(-1))
          ),
          3,
          Weights(11, 7, 2)
        ) ::
        (
          Mult(
            Add(Mult(Symbol(BigInt(0)), Integer(-2)), Integer(-1)),
            Symbol(BigInt(0))
          ),
          2,
          Weights(8, 7, 5)
        ) ::
        (
          Mult(
            Add(Mult(Symbol(BigInt(0)), Integer(-2)), Integer(-1)),
            Integer(3)
          ),
          3,
          Weights(3, 1, 1)
        ) ::
        (
          Add(
            Mult(Add(Symbol(BigInt(-1)), Symbol(BigInt(0))), Integer(2)),
            Integer(-3)
          ),
          3,
          Weights(2, 1, 1)
        ) ::
        (
          Mult(Integer(2), Symbol(BigInt(0))),
          2,
          Weights(8, 4, 1)
        ) :: (
          Neg(Add(One, Symbol(BigInt(0)))),
          8,
          Weights(4, 2, 1)
        ) ::
        (
          // issue #1589
          Add(
            Mult(
              Integer(
                BigInt(
                  "-119088922527214455169877378003275124204831957359945318400"
                )
              ),
              Add(
                Symbol(BigInt("-1")),
                Symbol(BigInt("8589497787817019633"))
              )
            ),
            Mult(
              Integer(BigInt("-58135180282744295314869849927820822272")),
              Add(
                Symbol(BigInt("-2872798615067075361")),
                Symbol(BigInt("-1"))
              )
            )
          ),
          2,
          Weights(3, 1, 1)
        ) ::
        Nil

    regressions.foreach { case (e, c, w) => law(e, c, w) }

    forAll(
      Arbitrary.arbitrary[Expr[BigInt]],
      Gen.choose(2, 20),
      Arbitrary.arbitrary[Weights]
    )((a: Expr[BigInt], cnt0: Int, w: Weights) => law(a, cnt0, w))
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
      val sumProd = SumProd(reg :: Nil)
      val c0 = w.cost(reg)
      // println(show"c0=$c0, reg=$reg, flat=$flat")
      val norm = normalize(reg, w)
      val c1 = w.cost(norm)
      // println(show"c1=$c1, norm=$norm")
      assert(
        c1 <= c0,
        show"c1=$c1, norm=$norm, c0=$c0, reg=$reg, sumProd=$sumProd"
      )
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
  property("MultiSet scalar multiplication distributes over ++") {
    forAll {
      (ms1: MultiSet[Int, BigInt], ms2: MultiSet[Int, BigInt], k: BigInt) =>
        val lhs = (ms1 ++ ms2) * k
        val rhs = (ms1 * k) ++ (ms2 * k)
        // and check counts
        assertEquals(
          lhs.nonZeroIterator.toList.sorted,
          rhs.nonZeroIterator.toList.sorted
        )
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
        val costRep = w.cost(rep)
        val mult0 = Expr.multAll(e :: Integer(cnt) :: Nil)
        val mult1 = w.constMult(e, cnt)
        val mult2 = e.bestEffortConstMult(cnt)
        assert(w.cost(mult0) <= costRep)
        assert(w.cost(mult1) <= costRep)
        assert(w.cost(mult2) <= costRep)
      }
    }
  }
  test("checkMult(-1, -1) == One") {
    val e = Expr.checkMult(Integer(-1), Integer(-1))
    assertEquals(e, One)
  }

  // TODO: this test doesn't work well with basicNorm which is still unsafe
  // for deep stacks. undistribute fails here
  test("normalize does not throw on very deep trees".ignore) {
    val depth = 50000
    val leaf = Symbol(BigInt(1))
    val deep =
      (1 to depth).foldLeft(leaf: Expr[BigInt])((acc, _) => Add(acc, leaf))
    val w = Weights(mult = 5, add = 1, neg = 1)
    val n = normalize(deep, w)
    assertEquals(Expr.toValue(n), Expr.toValue(deep))
  }

  property(
    "check the unConstMult laws"
  ) {
    def law[A: Numeric: Show: Order](e: Expr[A], w: Weights) =
      e.unConstMult.foreach { case (bi, x) =>
        val bix = Expr.toValue(x)
        val biE = Expr.toValue(e)
        assertEquals(
          implicitly[Numeric[A]].times(bix, Expr.fromBigInt(bi)),
          biE
        )

        // law1:  Mult(Integer(i), x) == this
        val biInt = Integer(bi)
        assertEquals(Expr.toValue(biInt * x), biE)
        val checkMult = Expr.checkMult(biInt, x)
        assertEquals(Expr.toValue(checkMult), biE)

        // law2: cost(Expr.checkMult(Integer(i), x)) <= cost(this)
        val costE = w.cost(e)
        val checkCost = w.cost(Mult(biInt, x))
        assert(
          checkCost <= costE || x.isOne || x.isZero,
          show"bi=$bi, x=$x, checkCost=$checkCost, costE=$costE"
        )
        // law3: e.unConstMult.flatMap { case (_, e) => e.unConstMult } == None || e.isZero (we pull all the ints out once)
        if (!x.isZero) {
          val uc = x.unConstMult
          assertEquals(uc, None, show"e=$e, bi=$bi, x=$x, uc=$uc")
        } else {
          assertEquals(x, Zero)
          assertEquals(bi, BigInt(0))
        }
        // law4: if we return zero, both are zero
        if (bi == 0) {
          // note the other side is tested above in the else branch of if (!x.isZero)
          assertEquals(x, Zero)
        }

        // law5: we never return 1, -1
        assert((bi != 1) && (bi != -1))

        // law6: the expression returned is never Integer(_) or Neg(_)
        x match {
          case bad @ (Integer(_) | Neg(_)) =>
            fail(s"bad returned expression: $bad, e=$e, bi=$bi")
          case _ => ()
        }

        // this is really just exercising constMult, but why not
        assertEquals(
          Expr.toValue(w.constMult(x, bi)),
          biE,
          show"e=$e, bi=$bi, x=$x, bix=$bix"
        )
      }

    val regressions: List[(Expr[Int], Weights)] =
      (
        Mult(Neg(Symbol(3)), Add(Integer(2), Integer(2))),
        Weights(11, 5, 2)
      ) ::
        Nil

    regressions.foreach { case (e, w) => law(e, w) }
    val lawProp = forAll((e: Expr[BigInt], w: Weights) => law(e, w))

    // some examples we can handle
    val examples: List[(Expr[Int], BigInt)] =
      (
        // 32 + (((-32 * 9) * One) * -(-(32))
        Add(
          Integer(32),
          Mult(Mult(Mult(Integer(-32), Symbol(-9)), One), Neg(Neg(Integer(32))))
        ),
        BigInt(32)
      ) :: (
        // ((-32 * 9) * One) * -(-(32))
        Mult(Mult(Mult(Integer(-32), Symbol(-9)), One), Neg(Neg(Integer(32)))),
        BigInt(-1024)
      ) ::
        Nil

    examples.foreach { case (e, bi) =>
      assertEquals(e.unConstMult.map(_._1), Some(bi), show"e=$e")
    }
    lawProp
  }

  property("unConstMult can always remove * const") {
    val regressions: List[(Expr[BigInt], BigInt)] =
      (
        Add(
          Add(
            Neg(One),
            Symbol(BigInt("20195331171140066237232561801535085096"))
          ),
          Add(
            Integer(BigInt("5770508449411511621")),
            Symbol(BigInt("9223372036854775807"))
          )
        ),
        BigInt(-2)
      ) :: (
        Add(
          Add(
            Neg(One),
            Add(
              Zero,
              Symbol(
                BigInt("-110669654550331471115483389665223738867131442269105218925")
              )
            )
          ),
          Add(
            Symbol(BigInt("19032331167890620900154367247757747878169438074714195968")),
            Integer(BigInt("6406209105737102121"))
          )
        ),
        BigInt(-3)
      ) :: Nil

    regressions.foreach { case (e, const) =>
      assert((e * Integer(const)).unConstMult.isDefined)
      assert((Integer(const) * e).unConstMult.isDefined)
    }

    forAll { (e: Expr[BigInt], const: BigInt) =>
      if ((const != 0) && (const != 1) && (const != -1)) {
        assert((e * Integer(const)).unConstMult.isDefined)
        assert((Integer(const) * e).unConstMult.isDefined)
      }
    }
  }

  property("groupSum law") {
    forAll { (es: List[Expr[BigInt]]) =>
      val GroupSum(const, terms) = GroupSum(es)
      val combined =
        terms.nonZeroIterator.foldLeft(Integer(const): Expr[BigInt]) {
          case (acc, (e, n)) =>
            // we never have zeros here
            assert(n != 0)
            acc + (e.toExpr * Integer(n))
        }

      assertEquals(
        Expr.toValue(combined),
        Expr.toValue(es.foldLeft(Zero: Expr[BigInt])(_ + _))
      )
    }
  }

  property("SumProd keys are never single Add(_, _) nodes") {
    forAll { (e: Expr[BigInt]) =>
      def allGoodKeys(e: SumProd[BigInt], msg: String) =
        e.terms.nonZeroIterator.foreach {
          case (NonEmptyList(oneAdd @ Add(_, _), Nil), _) =>
            fail(
              show"found a single add: ${(oneAdd: Expr[BigInt])} in $e. msg: $msg"
            )
          case _ => ()
        }

      val sumProd = SumProd(e :: Nil)
      allGoodKeys(sumProd, show"sumProd($e)")
      sumProd.splits.foreach { case (_, div, mod) =>
        allGoodKeys(div, show"div=$div in sumProd=$sumProd of $e")
        allGoodKeys(mod, show"mod=$mod in sumProd=$sumProd of $e")
      }
    }
  }

  property("SumProd splits produces equivalent Expr") {
    forAll { (es: Expr[BigInt]) =>
      val sumProd = SumProd(es :: Nil)
      val expect = Expr.toValue(es)
      sumProd.splits.foreach {
        case (Right(term), div, mod) =>
          val e1 = term.toExpr * div.directToExpr + mod.directToExpr
          assertEquals(Expr.toValue(e1), expect)
        case (Left(bi), div, mod) =>
          val e1 = Integer(bi) * div.directToExpr + mod.directToExpr
          assertEquals(Expr.toValue(e1), expect)
      }
    }
  }

  property("multIsBetter law") {
    def law(e: Expr[Int], c0: Int, w: Weights) = {
      // don't let replicateAdd get giant
      val c = BigInt(c0 & 0xff) * (if (c0 < 0) -1 else 1)
      if (w.multIsBetter(e, c)) {
        assert(
          w.cost(e.bestEffortConstMult(c)) <= w.cost(Expr.replicateAdd(c, e))
        )
        assert(
          w.cost(Expr.checkMult(e, Integer(c))) <= w.cost(
            Expr.replicateAdd(c, e)
          ),
          show"c = $c, e.bestEffortConstMult(c)=${e.bestEffortConstMult(c)}, replicateAdd=${Expr.replicateAdd(c, e)}, multThres=${w.multThreshold}"
        )
      } else {
        assert(
          w.cost(e.bestEffortConstMult(c)) >= w.cost(Expr.replicateAdd(c, e)),
          show"c = $c, e.bestEffortConstMult(c)=${e.bestEffortConstMult(c)}, replicateAdd=${Expr.replicateAdd(c, e)}"
        )
      }
    }

    val regressions: List[(Expr[Int], Int, Weights)] =
      (
        Integer(-1),
        3,
        Weights(19, 9, 1)
      ) :: (
        Add(Zero, Neg(Symbol(0))),
        -2032106750,
        Weights(16, 8, 8)
      ) ::
        Nil

    regressions.foreach { case (e, c0, w) => law(e, c0, w) }
    forAll((e: Expr[Int], c0: Int, w: Weights) => law(e, c0, w))
  }

  property("Stack.toValue is the same as Expr.toValue and maybeBigInt") {
    forAll { (e: Expr[BigInt]) =>
      val a = Expr.toValue(e)
      val stack = Stack.fromExpr(e)
      val stackA = Stack.toValue(stack)
      assertEquals(stackA, Right(a), s"stack=$stack")
      assertEquals(stackA.toOption, e.maybeBigInt(bi => Some(bi)))
      assertEquals(
        stack.maybeBigInt(bi => Some(bi)),
        e.maybeBigInt(bi => Some(bi))
      )
    }
  }

  property("Stack.map(identity)") {
    forAll { (e: Expr[BigInt]) =>
      val stack = Stack.fromExpr(e)
      val stack1 = stack.map(x => x)
      assertEquals(stack1, stack)
    }
  }
  property("Stack.fromExpr / toExpr round trip") {
    forAll { (e: Expr[BigInt]) =>
      val stack = Stack.fromExpr(e)
      stack.toExpr match {
        case Right(e1) =>
          assert(Hash[Expr[BigInt]].eqv(e, e1), show"e1=$e1")
          assertEquals(e1, e)
        case Left(err) =>
          fail(err.toString)
      }
    }
  }

  property("Expr Order is lawful") {
    forAll { (a: Expr[BigInt], b: Expr[BigInt], c: Expr[BigInt]) =>
      OrderingLaws.forOrder(a, b, c)
    }
  }

  test("use traverse on Expr to evaluate") {
    val x = Expr.symbol("x")
    val y = Expr.symbol("y")

    val formula = (x + Expr.int(1)) * (x + Expr.int(2)) + y
    val subs = Map("x" -> 1, "y" -> 10)
    val toInt = formula.traverse(subs.get(_)).flatMap { exp =>
      exp.maybeBigInt(i => Some(BigInt(i)))
    }
    assertEquals(toInt, Some((BigInt(1) + 1) * (BigInt(1) + 2) + BigInt(10)))
  }
}
