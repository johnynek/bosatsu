package dev.bosatsu.set

import org.scalacheck.Prop.forAll
import cats.Eq
import org.scalacheck.{Arbitrary, Gen}

import cats.syntax.all._

import Rel.{Sub, Super, Same, Intersects, Disjoint}

object RelGen {
  val genRel: Gen[Rel] =
    Gen.oneOf(Sub, Super, Same, Intersects, Disjoint)

  implicit val arbRel: Arbitrary[Rel] = Arbitrary(genRel)
}

class RelLaws extends munit.ScalaCheckSuite {
  import RelGen._

  property("|+| is associative") {
    forAll { (a: Rel, b: Rel, c: Rel) =>
      assertEquals((a |+| b) |+| c, a |+| (b |+| c))
    }
  }

  property("|+| is commutative") {
    forAll { (a: Rel, b: Rel) =>
      assertEquals(a |+| b, b |+| a)
    }
  }

  property("|+| is idempotent") {
    forAll { (a: Rel) =>
      assertEquals(a |+| a, a)
    }
  }

  property("|+| and lazyCombine are the same") {
    forAll { (a: Rel, b: Rel) =>
      assertEquals(a |+| b, a.lazyCombine(b))
    }
  }

  property("invert.invert is identity") {
    forAll { (a: Rel) =>
      assertEquals(a.invert.invert, a)
    }
  }

  property("Same is bottom element") {
    forAll { (a: Rel) =>
      assertEquals(a |+| Same, a)
    }
  }

  property("Disjoint is top element") {
    forAll { (a: Rel) =>
      assertEquals(a |+| Disjoint, Disjoint)
    }
  }

  property("a.invert |+| b.invert == (a |+| b).invert") {
    forAll { (a: Rel, b: Rel) =>
      assertEquals(a.invert |+| b.invert, (a |+| b).invert)
    }
  }
}

abstract class GenIntersectionRelLaws extends munit.ScalaCheckSuite { laws =>
  type S
  type E

  implicit def arbSet: Arbitrary[S]
  implicit def arbElement: Arbitrary[E]

  def contains(s: S, e: E): Boolean
  def lift(e: E): S
  def intersection(s1: S, s2: S): S
  def relatable: Relatable[S]

  // if this is true, we assert that (x === y) == (x == y)
  // on each equality check
  def strictEquals: Boolean

  implicit private class Ops(self: S) {
    def &(that: S): S = intersection(self, that)
    def <:>(that: S): Rel = {
      val res = relatable.relate(self, that)
      if (strictEquals) {
        val eqS = Eq.fromUniversalEquals[S]
        assertEquals(
          eqS.eqv(self, that),
          res == Same,
          s"self = $self, that = $that"
        )
      }
      res
    }
    def contains(e: E): Boolean = laws.contains(self, e)
    def ===(that: S): Boolean = (self <:> that) == Rel.Same
  }

  property("(x & y) = (y & x)") {
    forAll { (x: S, y: S) =>
      assert((x & y) === (y & x))
    }
  }

  property("(x & x) = x") {
    forAll { (x: S) =>
      assert((x & x) === x)
    }
  }

  property("((x & y) & z) = (x & (y & z))") {
    forAll { (x: S, y: S, z: S) =>
      assert(((x & y) & z) === (x & (y & z)))
    }
  }

  property("(x <:> y).invert == (y <:> x)") {
    forAll { (x: S, y: S) =>
      assertEquals((x <:> y).invert, y <:> x)
    }
  }

  property("lift(e).contains(e)") {
    forAll { (e: E) =>
      assert(lift(e).contains(e))
    }
  }

  property("(x <= y) = ((x & y) = x)") {
    forAll { (x: S, y: S) =>
      assertEquals((x <:> y).isSubtype, (x & y) === x)
    }
  }

  property("x <= y, y <= z implies x <= z") {
    forAll { (x: S, y: S, z: S) =>
      if ((x <:> y).isSubtype && (y <:> z).isSubtype) {
        assert((x <:> z).isSubtype)
      } else {
        ()
      }
    }
  }

  property("x < y, y <= z implies x < z") {
    forAll { (x: S, y: S, z: S) =>
      if ((x <:> y).isStrictSubtype && (y <:> z).isSubtype) {
        assert((x <:> z).isStrictSubtype)
      } else {
        ()
      }

      if ((x <:> y).isSubtype && (y <:> z).isStrictSubtype) {
        assert((x <:> z).isStrictSubtype)
      } else {
        ()
      }
    }
  }

  property("x & y <= x") {
    forAll { (x: S, y: S) =>
      assert(((x & y) <:> x).isSubtype)
    }
  }

  property("contains/intersection homomorphism") {
    forAll { (t1: S, t2: S, j: E) =>
      assertEquals((t1 & t2).contains(j), t1.contains(j) && t2.contains(j))
    }
  }

  property("(x contains a) = (x intersects point(a))") {
    forAll { (x: S, a: E) =>
      val res = x <:> lift(a) match {
        case Super | Same | Intersects => true
        case Disjoint | Sub            => false
      }
      assertEquals(x.contains(a), res)
    }
  }

  property("rel and contains are consistent") {
    forAll { (t1: S, t2: S, j: E) =>
      t1 <:> t2 match {
        case Rel.Sub =>
          if (t1.contains(j)) assert(t2.contains(j))
        case Rel.Same =>
          assertEquals(t1.contains(j), t2.contains(j))
        case Rel.Super =>
          if (t2.contains(j)) assert(t1.contains(j))
        case Rel.Disjoint =>
          assert(!(t1.contains(j) && t2.contains(j)))
        case Rel.Intersects =>
          // 0, 1 or both could match
          ()
      }
    }
  }
}

abstract class GenRelLaws extends GenIntersectionRelLaws { laws =>
  def union(s1: S, s2: S): S

  implicit private class Ops(self: S) {
    def |(that: S): S = union(self, that)
    def &(that: S): S = intersection(self, that)
    def <:>(that: S): Rel = {
      val res = relatable.relate(self, that)
      if (strictEquals) {
        val eqS = Eq.fromUniversalEquals[S]
        assertEquals(
          eqS.eqv(self, that),
          res == Same,
          s"self = $self, that = $that"
        )
      }
      res
    }
    def contains(e: E): Boolean = laws.contains(self, e)
    def ===(that: S): Boolean = (self <:> that) == Rel.Same
  }

  property("(x | x) = x") {
    forAll { (x: S) =>
      assert((x | x) === x)
    }
  }

  property("(x | y) = (y | x)") {
    forAll { (x: S, y: S) =>
      assert((x | y) === (y | x))
    }
  }

  property("((x | y) | z) = (x | (y | z))") {
    forAll { (x: S, y: S, z: S) =>
      assert(((x | y) | z) === (x | (y | z)))
    }
  }

  property("(x contains a) ==> (x | point(a) == x)") {
    forAll { (x: S, a: E) =>
      if (x.contains(a)) {
        val y = lift(a)
        assert((x | y) === x)
        assert((x <:> y).isSupertype)
      }
    }
  }

  property("point(a) | point(b) is a strict super type or a == b") {
    forAll { (a: E, b: E) =>
      val sa = lift(a)
      val sb = lift(b)
      val both = sa | sb
      if ((both <:> sa).isStrictSupertype) {
        assertEquals(sa <:> sb, Rel.Disjoint)
      } else {
        assertEquals(sa <:> sb, Rel.Same)
      }
      if ((sa <:> sb).isSupertype) {
        // if one is a supertype, then they are the same
        assertEquals(sa <:> sb, Rel.Same)
      } else {
        assert(!(sb <:> sa).isSupertype)
      }
    }
  }

  property("(x <= y) = ((x | y) = y)") {
    forAll { (x: S, y: S) =>
      assertEquals((x <:> y).isSubtype, (x | y) === y)
    }
  }

  property("x | y >= x") {
    forAll { (x: S, y: S) =>
      assert(((x | y) <:> x).isSupertype)
    }
  }

  property("contains | homomorphism") {
    forAll { (t1: S, t2: S, j: E) =>
      assertEquals((t1 | t2).contains(j), t1.contains(j) || t2.contains(j))
    }
  }

  property("distibutive (t1 | t2) & t3 = (t1 & t3) | (t2 & t3) homomorphism") {
    forAll { (t1: S, t2: S, t3: S, j: E) =>
      assertEquals(
        ((t1 | t2) & t3).contains(j),
        ((t1 & t3) | (t2 & t3)).contains(j)
      )
    }
  }

  property("distibutive (t1 & t2) | t3 = (t1 | t3) & (t2 | t3) homomorphism") {
    forAll { (t1: S, t2: S, t3: S, j: E) =>
      assertEquals(
        ((t1 & t2) | t3).contains(j),
        ((t1 | t3) & (t2 | t3)).contains(j)
      )
    }
  }

  property("distibutive (t1 | t2) & t3 = (t1 & t3) | (t2 & t3) rel") {
    forAll { (t1: S, t2: S, t3: S) =>
      assertEquals(((t1 | t2) & t3) <:> ((t1 & t3) | (t2 & t3)), Rel.Same)
    }
  }

  property("distibutive (t1 & t2) | t3 = (t1 | t3) & (t2 | t3) rel") {
    forAll { (t1: S, t2: S, t3: S) =>
      assertEquals(((t1 & t2) | t3) <:> ((t1 | t3) & (t2 | t3)), Rel.Same)
    }
  }

  property("x & (x | y) == x") {
    forAll { (x: S, y: S) =>
      assertEquals((x & (x | y)) <:> x, Rel.Same)
    }
  }

  property("x | (x & y) == x") {
    forAll { (x: S, y: S) =>
      assertEquals((x | (x & y)) <:> x, Rel.Same)
    }
  }

}

// This checks the testing mechanisms and support
// code using Set, where intersection and union are
// trivial
abstract class SetGenRelLaws[A](implicit
    val arbSet: Arbitrary[Set[A]],
    val arbElement: Arbitrary[A]
) extends GenRelLaws { setgenrellaws =>
  type S = Set[A]
  type E = A

  def strictEquals = true
  def contains(s: S, e: E): Boolean = s(e)
  def lift(e: E): S = Set(e)
  def intersection(s1: S, s2: S): S = s1.intersect(s2)
  def union(s1: S, s2: S): S = s1.union(s2)

  val relatable = Relatable.setRelatable[A]

  // Test the UnionRelModule

  val urm = new Relatable.UnionRelModule[S] {
    def relatable = setgenrellaws.relatable
    def deunion(a: S): Either[(S, S) => Rel.SuperOrSame, (S, S)] =
      if (a.size > 1) Right((Set(a.head), a.tail))
      else
        Left((s1, s2) =>
          if (Eq.fromUniversalEquals[S].eqv(a, (s1 | s2))) Rel.Same
          else Rel.Super
        )

    def cheapUnion(head: S, tail: List[S]): S = tail.foldLeft(head)(_ | _)

    def intersect(a: S, b: S): S = a.intersect(b)

    def isEmpty(s: S) = s.isEmpty
  }

  property("test unionRelCompare") {
    forAll { (s1: S, s2: S, s3: S) =>
      assertEquals(
        urm.unionRelCompare(s1, s2, s3),
        relatable.relate(s1, s2 | s3)
      )
    }
  }
}

class ByteSetGenRelLaws extends SetGenRelLaws[Byte]
class IntSetGenRelLaws extends SetGenRelLaws[Int]
class BoolSetGenRelLaws extends SetGenRelLaws[Boolean]

class ListUnionRelatableTests extends munit.ScalaCheckSuite {

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(5000)
      .withMaxDiscardRatio(10)

  implicit val arbByte: Arbitrary[Byte] =
    // Here there are only 2^10 = 1024 possible sets of these bytes
    // the goal is to make tests run faster
    Arbitrary(Gen.choose(0.toByte, 9.toByte))

  implicit val setRel: Relatable[Set[Byte]] =
    Relatable.setRelatable[Byte]

  implicit val relByte: Relatable[Byte] =
    Relatable.fromUniversalEquals

  val listRel: Relatable[List[Byte]] = Relatable.listUnion[Byte](
    _ => false,
    (i1, i2) => if (i1 == i2) i1 :: Nil else Nil,
    i => Left(_.distinct == (i :: Nil))
  )

  property("listRel agrees with setRel") {
    forAll { (s1: Set[Byte], s2: Set[Byte]) =>
      assertEquals(listRel.relate(s1.toList, s2.toList), setRel.relate(s1, s2))
    }
  }

  val listSetRel: Relatable[List[Set[Byte]]] = Relatable.listUnion[Set[Byte]](
    _.isEmpty,
    { (i1, i2) =>
      val i = i1 & i2
      if (i.isEmpty) Nil else (i :: Nil)
    },
    { i =>
      val sz = i.size
      // we can actually solve this both ways
      // exercise both paths by only using splitting
      // when the hash code ends in 1
      if ((sz >= 2) && ((i.hashCode & 1) == 1)) {
        val (l, r) = i.toList.splitAt(sz / 2)
        Right((l.toSet, r.toSet))
      } else {
        // these is a either a single value or empty
        // set which is >= so the fold results in
        // a set that is empty or has one value
        Left(is => is.foldLeft(Set.empty[Byte])(_ | _) == i)
      }
    }
  )

  def smallList[A: Arbitrary]: Gen[List[A]] =
    Gen
      .geometric(4.0)
      .flatMap(Gen.listOfN(_, Arbitrary.arbitrary[A]))

  property("listUnion works with Set elements") {
    // this is more complex if each element is a set
    // this is similar to how we use unionRelMod
    // in code since each item it itself a set

    forAll(smallList[Set[Byte]], smallList[Set[Byte]]) {
      (s1: List[Set[Byte]], s2: List[Set[Byte]]) =>
        assertEquals(
          listSetRel.relate(s1, s2),
          setRel.relate(s1.combineAll, s2.combineAll)
        )
    }
  }
}
