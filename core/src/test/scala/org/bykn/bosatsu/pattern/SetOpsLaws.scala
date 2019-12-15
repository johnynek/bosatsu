package org.bykn.bosatsu.pattern

import cats.Eq
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite

abstract class SetOpsLaws[A] extends FunSuite {
  val setOps: SetOps[A]

  def genItem: Gen[A]

  def eqUnion: Gen[Eq[List[A]]]

  def eqA: Gen[Eq[A]] =
    eqUnion.map(Eq.by { a: A => a :: Nil }(_))

  import setOps._

  private implicit val arbSetOpsItem: Arbitrary[A] = Arbitrary(genItem)


  def intersectionIsCommutative(a1: A, a2: A, eqA: Eq[List[A]]) = {
    val a12 = intersection(a1, a2)
    val a21 = intersection(a2, a1)

    assert(eqA.eqv(a12, a21), s"$a12 != $a21")
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
    forAll(genItem, genItem, genItem, eqUnion)(intersectionIsAssociative(_, _, _, _))
  }

  test("unify union makes size <= input") {
    forAll(Gen.listOf(genItem)) { (ps: List[A]) =>
      val unified = unifyUnion(ps)

      assert(ps.size >= unified.size, s"unified = $unified")
    }
  }

  test("difference is idempotent: (a - b) = c, c - b == c") {
    forAll(genItem, genItem) { (a, b) =>
      val c = unifyUnion(difference(a, b))
      val c1 = unifyUnion(differenceAll(c, b :: Nil))
      assert(c == c1)
    }
  }

  def selfDifferenceLaw(p1: A, p2: A) = {
    if (p1 == p2) {
      assert(difference(p1, p2) == Nil)
    }
    assert(difference(p1, p1) == Nil)
    assert(difference(p2, p2) == Nil)
  }

  test("a - a == 0") {
    forAll(genItem, genItem)(selfDifferenceLaw(_, _))
  }

  test("a n a == a") {
    forAll(genItem) { (a: A) =>
      assert(intersection(a, a) == List(a))
    }
  }

  test("x - top = 0") {
    top.foreach { t =>
      forAll(genItem) { (x) =>
        assert(difference(x, t) == Nil)
      }
    }

    forAll(genItem, genItem) { (x: A, y: A) =>
      if (isTop(y)) assert(difference(x, y).isEmpty)
    }
  }

  test("if a n b = 0 then a - b = a") {
    def law(p1: A, p2: A, eqU: Eq[List[A]]) = {
      val inter = intersection(p1, p2)
      val diff = difference(p1, p2)

      if (inter.isEmpty) {
        assert(eqU.eqv(diff, p1 :: Nil), s"diff = $diff")
      }

      // difference is an upper bound, so this is not true
      // although we wish it were
      /*
      if (diff.map(_.normalize).distinct == p1.normalize :: Nil) {
        // intersection is 0
        assert(inter == Nil)
      }
      */
    }

    forAll(genItem, genItem, eqUnion)(law(_, _, _))
  }

  test("x - y = z, then x - y - z = 0") {
    forAll(genItem, genItem) { (x: A, y: A) =>
      val z = difference(x, y)
      val z1 = unifyUnion(differenceAll(z, z))
      assert(z1 == Nil, s"z = $z")
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
    }

    assert(intSub == diffSub)
  }

  test("subset consistency: a n b == a <=> a - b = 0") {
    forAll(genItem, genItem, eqUnion)(subsetConsistencyLaw(_, _, _))
  }

  test("difference returns distinct values") {
    forAll(genItem, genItem) { (a, b) =>
      val c = difference(a, b)
      assert(c == c.distinct)
    }
  }

  test("intersection returns distinct values") {
    forAll(genItem, genItem) { (a, b) =>
      val c = intersection(a, b)
      assert(c == c.distinct)
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
        }
      }
    }
  }

  // (a - b) n c == (a n c) - (b n c)
  def diffIntersectionLaw(a: A, b: A, c: A) = {
    val diffab = difference(a, b)
    val intBC = intersection(b, c)
    val left = diffab.flatMap(intersection(_, c))

    if ((diffab == (a :: Nil)) && (intersection(a, b).nonEmpty)) {
      // diffab is an upperbound, so hard to say what the law
      // should be in that case, if (a - b) = a, then
      // clearly we expect (a n c) == (a n c) - (b n c)
      // so, b n c has to not intersect with a, but it might
    }
    else if (isTop(a) && intBC.isEmpty) {
      // in patterns, we "cast" ill-typed comparisions
      // since we can don't care about cases that don't
      // type-check. But this can make this law fail:
      // if we have _ - b, we assume the difference is
      // on the same type, but if c is a different type
      // the intersection may be (_ - b) n c = 0
      // but (_ n c) = c, and b n c = 0
      assert((left == Nil) || (unifyUnion(left) == (c :: Nil)))
    }
    else {
      val intAC = intersection(a, c)
      val right = differenceAll(intAC, intBC)

      val leftu = unifyUnion(left)
      val rightu = unifyUnion(right)
      assert(leftu == rightu)
    }
  }

  test("(a - b) n c = (a n c) - (b n c)") {
    forAll(genItem, genItem, genItem)(diffIntersectionLaw(_, _, _))
  }

  test("missing branches, if added are total and none of the missing are unreachable") {

    def law(top: A, pats: List[A]) = {

      val rest = missingBranches(top :: Nil, pats)
      val rest1 = missingBranches(top :: Nil, pats ::: rest)
      if (rest1.isEmpty) {
        val unreach = unreachableBranches(pats ::: rest)
        assert(unreach.filter(rest.toSet) == Nil, s"\n\nrest = ${rest}\n\ninit: ${pats}")
      }
      else {
        fail(s"after adding ${rest} we still need ${rest1}")
      }
    }

    top.foreach { t =>
      val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genItem))
      forAll(pats)(law(t, _))
    }
  }

  test("missing branches are distinct") {
    val pats = Gen.choose(0, 10).flatMap(Gen.listOfN(_, genItem))

    forAll(pats, pats) { (top, pats) =>
      val rest = missingBranches(top, pats)
      assert(rest == rest.distinct)
    }
  }
}

class DistinctSetOpsTest extends SetOpsLaws[Byte] {
  val setOps: SetOps[Byte] = SetOps.distinct[Byte]

  val genItem: Gen[Byte] = Gen.choose(Byte.MinValue, Byte.MaxValue)

  val eqUnion: Gen[Eq[List[Byte]]] = Gen.const(new Eq[List[Byte]] {
    def eqv(left: List[Byte], right: List[Byte]) =
      left.toSet == right.toSet
  })
}

class IMapSetOpsTest extends SetOpsLaws[Byte] {
  val setOps: SetOps[Byte] =
    SetOps.imap(SetOps.distinct[Byte],
      { b: Byte => (b ^ 0xFF).toByte },
      { b: Byte => (b ^ 0xFF).toByte })

  val genItem: Gen[Byte] = Gen.choose(Byte.MinValue, Byte.MaxValue)

  val eqUnion: Gen[Eq[List[Byte]]] = Gen.const(new Eq[List[Byte]] {
    def eqv(left: List[Byte], right: List[Byte]) =
      left.toSet == right.toSet
  })
}

class ProductSetOpsTest extends SetOpsLaws[(Boolean, Boolean)] {
  val setOps: SetOps[(Boolean, Boolean)] = SetOps.product(SetOps.distinct[Boolean], SetOps.distinct[Boolean])

  val genItem: Gen[(Boolean, Boolean)] =
    Gen.oneOf((false, false), (false, true), (true, false), (true, true))

  val eqUnion: Gen[Eq[List[(Boolean, Boolean)]]] =
    Gen.const(new Eq[List[(Boolean, Boolean)]] {
      def eqv(left: List[(Boolean, Boolean)], right: List[(Boolean, Boolean)]) =
        left.toSet == right.toSet
    })
}

class UnitSetOpsTest extends SetOpsLaws[Unit] {
  val setOps: SetOps[Unit] = SetOps.unit(())

  val genItem: Gen[Unit] = Gen.const(())

  val eqUnion: Gen[Eq[List[Unit]]] = Gen.const(new Eq[List[Unit]] {
    def eqv(left: List[Unit], right: List[Unit]) =
      left.toSet == right.toSet
  })
}
