package org.bykn.bosatsu.pattern

import cats.Eq
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks.forAll
import org.scalatest.FunSuite

abstract class SetOpsLaws[A] extends FunSuite {
  val setOps: SetOps[A]

  def genItem: Gen[A]

  def eqUnion: Gen[Eq[List[A]]]


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
    def law(p1: A, p2: A) = {
      val inter = intersection(p1, p2)
      val diff = difference(p1, p2)

      if (inter.isEmpty) {
        assert(diff == p1 :: Nil)
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

    forAll(genItem, genItem)(law(_, _))
  }

  test("x - y = z, then x - y - z = 0") {
    forAll(genItem, genItem) { (x: A, y: A) =>
      val z = difference(x, y)
      val z1 = unifyUnion(differenceAll(z, z))
      assert(z1 == Nil, s"z = $z")
    }
  }

  test("subset consistency: a n b == a <=> a - b = 0") {
    def isSubsetIntr(a: A, b: A) =
      intersection(a, b) == (a :: Nil)

    def isSubsetDiff(a: A, b: A) =
      difference(a, b).isEmpty

    def law(a: A, b: A) = {
      val intSub = isSubsetIntr(a, b)
      val diffSub = isSubsetDiff(a, b)

      if (subset(a, b)) {
        assert(intSub)
        assert(diffSub)
      }

      assert(intSub == diffSub)
    }

    forAll(genItem, genItem)(law(_, _))
  }

  // test("no missing/unused paradox") {
  //   /*
  //    * We don't want to produce a list of missing branches, but then add them
  //    * and find we have unused branches
  //    */
  //   val smallList: Gen[List[Pattern]] =
  //     for {
  //       cnt <- Gen.choose(1, 2)
  //       list <- Gen.listOfN(cnt, genPat)
  //     } yield list

  //   def diff(as: List[Pattern], bs: List[Pattern]): List[Pattern] =
  //     for {
  //       a <- as
  //       b <- bs
  //       c <- difference(a, b)
  //     } yield c

  //   def intr(as: List[Pattern], bs: List[Pattern]): List[Pattern] =
  //     for {
  //       a <- as
  //       b <- bs
  //       c <- intersection(a, b)
  //     } yield c

  //   forAll(genPat, smallList) { (h, t) =>
  //     val pats = h :: t
  //     val missing = diff(List(Pattern.Wild), pats)
  //     if (missing.nonEmpty) {
  //       // this cannot be a subset of pats
  //       val isSubSet = diff(missing, pats)
  //       assert(isSubSet != Nil)
  //     }
  //   }
  // }
}
