package dev.bosatsu.set

import cats.Eq
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.Prop.forAll

abstract class SetOpsLaws[A] extends munit.ScalaCheckSuite {
  val setOps: SetOps[A]

  def genItem: Gen[A]
  implicit def shrinkItem: Shrink[A] =
    Shrink.withLazyList(_ => LazyList.empty)

  def genUnion: Gen[List[A]] = Gen.listOf(genItem)

  def eqUnion: Gen[Eq[List[A]]]

  def eqA: Gen[Eq[A]] =
    eqUnion.map { eqList =>
      Eq.by((a: A) => a :: Nil)(using eqList)
    }

  import setOps._

  def intersectionIsCommutative(a1: A, a2: A, eqA: Eq[List[A]]) = {
    val a12 = intersection(a1, a2)
    val a21 = intersection(a2, a1)

    assert(eqA.eqv(a12, a21), s"$a12 != $a21")
  }

  def differenceIsIdempotent(a: A, b: A, eqAs: Eq[List[A]])(implicit
      loc: munit.Location
  ) = {
    val c = unifyUnion(difference(a, b))
    val c1 = unifyUnion(differenceAll(c, b :: Nil))
    assert(eqAs.eqv(c, c1), s"c = $c\n\nc1 = $c1")
  }

  def emptyIntersectionMeansDiffIdent(p1: A, p2: A, eqU: Eq[List[A]]) = {
    val inter = intersection(p1, p2)
    val diff = difference(p1, p2)

    if (inter.isEmpty) {
      assert(eqU.eqv(diff, p1 :: Nil), s"diff = $diff")
    }
  }

  test("intersection is commutative") {
    forAll(genItem, genItem, eqUnion)(intersectionIsCommutative(_, _, _))
  }

  def intersectionIsAssociative(p1: A, p2: A, p3: A, eqA: Eq[List[A]]) = {

    val leftI = for {
      i1 <- intersection(p1, p2)
      i2 <- intersection(i1, p3)
    } yield i2

    val rightI = for {
      i1 <- intersection(p2, p3)
      i2 <- intersection(p1, i1)
    } yield i2

    assert(eqA.eqv(leftI, rightI), s"\n\n$leftI\n\n$rightI")
  }

  test("intersection is associative") {
    forAll(genItem, genItem, genItem, eqUnion)(
      intersectionIsAssociative(_, _, _, _)
    )
  }

  test("unify union makes size <= input") {
    forAll(genUnion) { (ps: List[A]) =>
      val unified = unifyUnion(ps)

      assert(
        ps.size >= unified.size,
        s"input(${ps.size}): $ps\n\nunified(${unified.size}) = $unified\n\n"
      )
    }
  }

  test("difference is idempotent: (a - b) = c, c - b == c") {
    forAll(genItem, genItem, eqUnion)(differenceIsIdempotent(_, _, _))
  }

  def selfDifferenceLaw(p1: A, p2: A) = {
    if (Eq.fromUniversalEquals[A].eqv(p1, p2)) {
      assertEquals(difference(p1, p2), Nil)
    }
    assertEquals(difference(p1, p1), Nil)
    assertEquals(difference(p2, p2), Nil)
  }

  test("a - a == 0") {
    forAll(genItem, genItem)(selfDifferenceLaw(_, _))
  }

  test("a n a == a") {
    forAll(genItem, eqA) { (a, eqv) =>
      val intr = intersection(a, a)
      assert(intr.forall(eqv.eqv(_, a)))
      assert(intr.nonEmpty)
    }
  }

  test("x - top = 0") {
    top.foreach { t =>
      forAll(genItem) { (x) =>
        assertEquals(difference(x, t), Nil)
      }
    }

    forAll(genItem, genItem) { (x: A, y: A) =>
      if (isTop(y)) assert(difference(x, y).isEmpty)
    }
  }

  test("if a n b = 0 then a - b = a") {
    // difference is an upper bound, so this is not true
    // although we wish it were
    /*
      if (diff.map(_.normalize).distinct == (p1.normalize :: Nil)) {
        // intersection is 0
        assert(inter.isEmpty)
      }
     */

    forAll(genItem, genItem, eqUnion)(emptyIntersectionMeansDiffIdent(_, _, _))
  }

  test("x - y = z, then x - y - z = 0") {
    forAll(genItem, genItem) { (x: A, y: A) =>
      val z = difference(x, y)
      val z1 = unifyUnion(differenceAll(z, z))
      assertEquals(z1, Nil, s"z = $z")
    }
  }

  def isSubsetIntr(a: A, b: A, eqAs: Eq[List[A]]) =
    eqAs.eqv(intersection(a, b), (a :: Nil))

  def isSubsetDiff(a: A, b: A) =
    difference(a, b).isEmpty

  def subsetConsistencyLaw(a: A, b: A, eqAs: Eq[List[A]]) = {
    val intSub = isSubsetIntr(a, b, eqAs)
    val diffSub = isSubsetDiff(a, b)

    if (subset(a, b)) {
      assert(intSub)
      assert(diffSub)
      assertEquals(intSub, diffSub)
    } else {
      // we can have false positives of intSub
      // when we have a sampling equality
      assertEquals(diffSub, false)
    }
  }

  test("subset consistency: a n b == a <=> a - b = 0") {
    forAll(genItem, genItem, eqUnion)(subsetConsistencyLaw(_, _, _))
  }

  test("difference returns distinct values") {
    forAll(genItem, genItem) { (a, b) =>
      val c = difference(a, b)
      assertEquals(c, c.distinct)
    }
  }

  test("intersection returns distinct values") {
    forAll(genItem, genItem) { (a, b) =>
      val c = intersection(a, b)
      assertEquals(c, c.distinct)
    }
  }

  test("no missing/unused paradox") {
    top.foreach { wild =>
      /*
       * We don't want to produce a list of missing branches, but then add them
       * and find we have unused branches
       */
      val smallList: Gen[List[A]] =
        for {
          cnt <- Gen.choose(1, 2)
          list <- Gen.listOfN(cnt, genItem)
        } yield list

      forAll(genItem, smallList) { (h, t) =>
        val pats = h :: t
        val initUnreach = unreachableBranches(pats).toSet
        val patsGood = pats.filterNot(initUnreach)
        val missing = missingBranches(wild :: Nil, patsGood)
        if (missing.nonEmpty) {
          unreachableBranches(patsGood ::: missing).isEmpty
        } else true
      }
    }
  }

  // (a - b) n c == (a n c) - (b n c)
  // This law is pretty questionable since difference is an
  // upper bound for us (currently) and we have differences
  // on both sides. There are a ton of work arounds below
  // which maybe indicates we should just disable the test
  def diffIntersectionLaw(a: A, b: A, c: A) = {
    val diffab = difference(a, b)
    val intBC = intersection(b, c)
    val left = diffab.flatMap(intersection(_, c))

    val listEq = Eq.fromUniversalEquals[List[A]]
    if (listEq.eqv(diffab, a :: Nil) && intersection(a, b).nonEmpty) {
      // diffab is an upperbound, so hard to say what the law
      // should be in that case, if (a - b) = a, then
      // clearly we expect (a n c) == (a n c) - (b n c)
      // so, b n c has to not intersect with a, but it might
    } else if (isTop(a) && intBC.isEmpty) {
      // in patterns, we "cast" ill-typed comparisions
      // since we can don't care about cases that don't
      // type-check. But this can make this law fail:
      // if we have _ - b, we assume the difference is
      // on the same type, but if c is a different type
      // the intersection may be (_ - b) n c = 0
      // but (_ n c) = c, and b n c = 0
      val leftEqC = differenceAll(unifyUnion(left), c :: Nil).isEmpty
      assert(left.isEmpty || leftEqC)
    } else {
      val intAC = intersection(a, c)
      val right = differenceAll(intAC, intBC)

      // since a - b can be a lose bound, we also see a - b == a
      // some times, in which case, (a - b) n c = a n c
      val leftu = unifyUnion(left)
      if (listEq.eqv(leftu, unifyUnion(intAC))) {
        ()
      } else {
        val rightu = unifyUnion(right)
        assertEquals(
          leftu,
          rightu,
          s"diffAB = $diffab, intAC = $intAC, intBC = $intBC"
        )
      }
    }
  }

  /*
   * This isn't true in general because difference is an upper-bound
   * and you have differences on both sides of the equation.
   * It is *usually* true, but we can't write a law for that
  test("(a - b) n c = (a n c) - (b n c)") {
    forAll(genItem, genItem, genItem)(diffIntersectionLaw(_, _, _))
  }
   */

  def missingBranchesIfAddedRegressions: List[List[A]] = Nil

  test(
    "missing branches, if added are total and none of the missing are unreachable"
  ) {

    def law(top: A, pats: List[A]) = {

      val rest = missingBranches(top :: Nil, pats)
      val rest1 = missingBranches(top :: Nil, pats ::: rest)
      if (rest1.isEmpty) {
        val unreach = unreachableBranches(pats ::: rest)
        assertEquals(
          unreach.filter(rest.toSet),
          Nil,
          s"\n\nrest = ${rest}\n\ninit: ${pats}"
        )
      } else {
        fail(s"after adding ${rest} we still need ${rest1}")
      }
    }

    top.foreach { t =>
      val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genItem))
      forAll(pats)(law(t, _))

      missingBranchesIfAddedRegressions.foreach(law(t, _))
    }
  }

  test("missing branches are distinct") {
    val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genItem))

    forAll(pats, pats) { (top, pats) =>
      val rest = missingBranches(top, pats)
      assertEquals(rest, rest.distinct)
    }
  }

  test("relate consistency") {
    forAll(genItem, genItem, eqUnion) { (a, b, eqv) =>
      val relAb = setOps.relate(a, b)
      assertEquals(setOps.relate(b, a).invert, relAb)
      relAb match {
        case Rel.Same =>
          val intr = setOps.intersection(a, b)
          assert(setOps.subset(a, b))
          assert(setOps.subset(b, a))
          assert(!setOps.disjoint(a, b))
          assert(eqv.eqv(intr, a :: Nil))
          assert(eqv.eqv(intr, b :: Nil))
          assert(eqv.eqv(a :: Nil, b :: Nil))
        case Rel.Sub =>
          val intr = setOps.intersection(a, b)
          assert(setOps.subset(a, b))
          assert(!setOps.disjoint(a, b))
          assert(eqv.eqv(intr, a :: Nil))
          val diffB = setOps.difference(b, a)
          assert(!eqv.eqv(diffB, Nil))
        case Rel.Super =>
          val intr = setOps.intersection(a, b)
          assert(setOps.subset(b, a))
          assert(!setOps.disjoint(a, b))
          assert(eqv.eqv(intr, b :: Nil))
          val diffA = setOps.difference(a, b)
          assert(!eqv.eqv(diffA, Nil))
        case Rel.Disjoint =>
          val intr = setOps.intersection(a, b)
          assert(intr.isEmpty)
          assert(setOps.disjoint(a, b))
          val diffA = setOps.difference(a, b)
          val diffB = setOps.difference(b, a)
          assert(eqv.eqv(diffA, a :: Nil))
          assert(eqv.eqv(diffB, b :: Nil))
          assert(!eqv.eqv(a :: Nil, b :: Nil))
        case Rel.Intersects =>
          val intr = setOps.intersection(a, b)
          val diffA = setOps.difference(a, b)
          val diffB = setOps.difference(b, a)

          assert(!eqv.eqv(intr, a :: Nil), s"intr = $intr")
          assert(!eqv.eqv(intr, b :: Nil), s"intr = $intr")
          assert(!eqv.eqv(a :: Nil, b :: Nil))
          assert(intr.nonEmpty, s"a = $a, b = $b , intr = $intr")
          assert(diffA.nonEmpty)
          assert(diffB.nonEmpty)
      }
    }
  }
}

class DistinctSetOpsTest extends SetOpsLaws[Byte] {
  val setOps: SetOps[Byte] = SetOps.distinct[Byte]

  val genItem: Gen[Byte] = Gen.choose(Byte.MinValue, Byte.MaxValue)

  val eqUnion: Gen[Eq[List[Byte]]] = Gen.const(new Eq[List[Byte]] {
    def eqv(left: List[Byte], right: List[Byte]) =
      Eq.fromUniversalEquals[Set[Byte]].eqv(left.toSet, right.toSet)
  })
}

class FiniteSetOpsTest extends SetOpsLaws[Set[Int]] {
  val setOps: SetOps[Set[Int]] = SetOps.fromFinite(0 to 9)

  val genItem: Gen[Set[Int]] = {
    // don't generate empty sets, items that are empty aren't lawful
    // the ways the laws are written
    val gi = Gen.choose(0, 9)
    Gen.zip(gi, Gen.listOf(gi)).map { case (h, t) =>
      t.toSet + h
    }
  }

  val eqUnion: Gen[Eq[List[Set[Int]]]] = Gen.const(new Eq[List[Set[Int]]] {
    def eqv(left: List[Set[Int]], right: List[Set[Int]]) =
      Eq.fromUniversalEquals[Set[Int]].eqv(
        left.foldLeft(Set.empty[Int])(_ | _),
        right.foldLeft(Set.empty[Int])(_ | _)
      )
  })
}

class IMapSetOpsTest extends SetOpsLaws[Byte] {
  val setOps: SetOps[Byte] =
    SetOps.imap(
      SetOps.distinct[Byte],
      (b: Byte) => (b ^ 0xff).toByte,
      (b: Byte) => (b ^ 0xff).toByte
    )

  val genItem: Gen[Byte] = Gen.choose(Byte.MinValue, Byte.MaxValue)

  val eqUnion: Gen[Eq[List[Byte]]] = Gen.const(new Eq[List[Byte]] {
    def eqv(left: List[Byte], right: List[Byte]) =
      Eq.fromUniversalEquals[Set[Byte]].eqv(left.toSet, right.toSet)
  })
}

class ProductSetOpsTest extends SetOpsLaws[(Boolean, Boolean)] {
  val setOps: SetOps[(Boolean, Boolean)] =
    SetOps.product(SetOps.distinct[Boolean], SetOps.distinct[Boolean])

  val genItem: Gen[(Boolean, Boolean)] =
    Gen.oneOf((false, false), (false, true), (true, false), (true, true))

  val eqUnion: Gen[Eq[List[(Boolean, Boolean)]]] =
    Gen.const(new Eq[List[(Boolean, Boolean)]] {
      def eqv(left: List[(Boolean, Boolean)], right: List[(Boolean, Boolean)]) =
        Eq.fromUniversalEquals[Set[(Boolean, Boolean)]]
          .eqv(left.toSet, right.toSet)
    })
}

class UnitSetOpsTest extends SetOpsLaws[Unit] {
  val setOps: SetOps[Unit] = SetOps.unit(())

  val genItem: Gen[Unit] = Gen.const(())

  val eqUnion: Gen[Eq[List[Unit]]] = Gen.const(new Eq[List[Unit]] {
    def eqv(left: List[Unit], right: List[Unit]) =
      Eq.fromUniversalEquals[Set[Unit]].eqv(left.toSet, right.toSet)
  })
}

case class Predicate[A](toFn: A => Boolean) { self =>
  def apply(a: A): Boolean = toFn(a)
  def &&(that: Predicate[A]): Predicate[A] =
    Predicate(a => self(a) && that(a))
  def ||(that: Predicate[A]): Predicate[A] =
    Predicate(a => self(a) || that(a))
  def -(that: Predicate[A]): Predicate[A] =
    Predicate(a => self(a) && !that(a))
  def unary_! : Predicate[A] =
    Predicate(a => !self(a))

  def product[B](that: Predicate[B]): Predicate[(A, B)] =
    Predicate { case (a, b) => self(a) && that(b) }
}

object Predicate {
  def genPred[A: Cogen]: Gen[Predicate[A]] =
    Gen.function1(Gen.oneOf(false, true)).map(Predicate(_))

  implicit def arbPred[A: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(genPred[A])
}

class SetOpsTests extends munit.ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(500)
      .withMaxDiscardRatio(10)

  test("allPerms is correct") {
    forAll(Gen.choose(0, 6).flatMap(Gen.listOfN(_, Arbitrary.arbitrary[Int]))) {
      is0 =>
        // make everything distinct
        val is = is0.zipWithIndex
        val perms = SetOps.allPerms(is)

        def fact(i: Int, acc: Int): Int =
          if (i <= 1) acc
          else fact(i - 1, i * acc)

        assertEquals(perms.length, fact(is0.size, 1))

        perms.foreach { p =>
          assertEquals(p.sorted, is.sorted)
        }
        val pi = perms.zipWithIndex

        for {
          (p1, i1) <- pi
          (p2, i2) <- pi
        } {
          val listEq = Eq.fromUniversalEquals[List[(Int, Int)]]
          assert((i1 >= i2 || !listEq.eqv(p1, p2)))
        }
    }
  }

  test(
    "greedySearch finds the optimal path if lookahead is greater than size"
  ) {
    // we need a non-commutative operation to test this
    // use 2x2 matrix multiplication
    def mult(
        left: Vector[Vector[Double]],
        right: Vector[Vector[Double]]
    ): Vector[Vector[Double]] = {
      def dot(v1: Vector[Double], v2: Vector[Double]) =
        v1.iterator.zip(v2.iterator).map { case (a, b) => a * b }.sum

      def trans(v1: Vector[Vector[Double]]) =
        Vector(Vector(v1(0)(0), v1(1)(0)), Vector(v1(0)(1), v1(1)(1)))

      val res = Vector(Vector(0.0, 0.0), Vector(0.0, 0.0))

      val data = for {
        (r, ri) <- left.zipWithIndex
        (c, ci) <- trans(right).zipWithIndex
      } yield ((ri, ci), dot(r, c))

      data.foldLeft(res) { case (v, ((r, c), d)) =>
        v.updated(r, v(r).updated(c, d))
      }
    }

    def norm(left: Vector[Vector[Double]]): Double =
      left.map(_.map(x => x * x).sum).sum

    val genMat: Gen[Vector[Vector[Double]]] = {
      val elem = Gen.choose(-1.0, 1.0)
      Gen.listOfN(4, elem).map { vs =>
        Vector(
          Vector(vs(0), vs(1)),
          Vector(vs(2), vs(3))
        )
      }
    }

    forAll(genMat, Gen.listOfN(5, genMat)) { (v0, prods) =>
      val ord = Ordering.by[Vector[Vector[Double]], Double](norm)
      val res = SetOps.greedySearch(5, v0, prods)((v, ps) =>
        ps.foldLeft(v)(mult(_, _))
      )(using ord)
      val normRes = norm(res)
      val naive = norm(prods.foldLeft(v0)(mult(_, _)))
      assert(normRes <= naive)
    }
  }

  test("test A - (B | C) <= ((A - B) | (A - C)) - (B n C)") {
    forAll { (pa: Predicate[Byte], pb: Predicate[Byte], pc: Predicate[Byte]) =>
      val left = pa - (pb || pc)
      val right = ((pa - pb) || (pa - pc)) - (pb && pc)
      val checks = (0 until 256).map(_.toByte)
      checks.foreach { b =>
        val ba = pa(b)
        val bb = pb(b)
        val bc = pc(b)
        if (!right(b)) {
          assert(
            !left(b),
            s"ba = $ba, bb = $bb, bc = $bc, ${left(b)} != ${right(b)}"
          )
        }
      }
    }
  }

  test("test A - (B | C) >= ((A - B) | (A - C))") {
    forAll { (pa: Predicate[Byte], pb: Predicate[Byte], pc: Predicate[Byte]) =>
      val left = pa - (pb || pc)
      val right = ((pa - pb) || (pa - pc))
      val checks = (0 until 256).map(_.toByte)
      checks.foreach { b =>
        val ba = pa(b)
        val bb = pb(b)
        val bc = pc(b)
        if (left(b)) {
          assert(
            right(b),
            s"ba = $ba, bb = $bb, bc = $bc, ${left(b)} != ${right(b)}"
          )
        }
      }
    }
  }

  test("A - (B | C) = (A - B) n (A - C)") {
    forAll { (pa: Predicate[Byte], pb: Predicate[Byte], pc: Predicate[Byte]) =>
      val left = pa - (pb || pc)
      val right = (pa - pb) && (pa - pc)
      val checks = (0 until 256).map(_.toByte)
      checks.foreach { b =>
        val ba = pa(b)
        val bb = pb(b)
        val bc = pc(b)
        assertEquals(
          left(b),
          right(b),
          s"ba = $ba, bb = $bb, bc = $bc, ${left(b)} != ${right(b)}"
        )
      }
    }
  }

  test("(A | B) - C = (A - C) | (B - C)") {
    forAll { (pa: Predicate[Byte], pb: Predicate[Byte], pc: Predicate[Byte]) =>
      val left = (pa || pb) - pc
      val right = (pa - pc) || (pb - pc)
      val checks = (0 until 256).map(_.toByte)
      checks.foreach { b =>
        val ba = pa(b)
        val bb = pb(b)
        val bc = pc(b)
        if (!right(b)) {
          assert(
            !left(b),
            s"ba = $ba, bb = $bb, bc = $bc, ${left(b)} != ${right(b)}"
          )
        }
      }
    }
  }
  test("A1 x B1 - A2 x B2 = (A1 n A2)x(B1 - B2) u (A1 - A2)xB1") {
    forAll {
      (
          a1: Predicate[Byte],
          a2: Predicate[Byte],
          b1: Predicate[Byte],
          b2: Predicate[Byte],
          checks: List[(Byte, Byte)]
      ) =>
        val left = a1.product(b1) - a2.product(b2)
        val right = (a1 && a2).product(b1 - b2) || (a1 - a2).product(b1)
        checks.foreach { ab =>
          assertEquals(left(ab), right(ab))
        }
    }
  }
}
