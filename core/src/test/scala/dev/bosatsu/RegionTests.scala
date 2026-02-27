package dev.bosatsu

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import cats.Show
import cats.Hash
import cats.Eq

class RegionTests extends munit.ScalaCheckSuite {

  implicit val arbRegion: Arbitrary[Region] =
    Arbitrary(for {
      s <- Arbitrary.arbitrary[Int]
      e <- Arbitrary.arbitrary[Int]
    } yield Region(s, e))

  property("Region(s, e).start == s") {
    forAll { (s: Int, e: Int) =>
      val r = Region(s, e)
      assertEquals(r.start, s)
    }
  }

  property("Region(s, e).end == e") {
    forAll { (s: Int, e: Int) =>
      val r = Region(s, e)
      assertEquals(r.end, e)
    }
  }

  property("Region(s, e).withEnd(e2) == Region(s, e2)") {
    forAll { (s: Int, e: Int, e2: Int) =>
      val r = Region(s, e).withEnd(e2)
      assertEquals(r.start, s)
      assertEquals(r.end, e2)
    }
  }

  property("r1 + r2 == Region(r1.start, r2.end)") {
    forAll { (r1: Region, r2: Region) =>
      val r3 = r1 + r2
      assertEquals(r3.start, r1.start)
      assertEquals(r3.end, r2.end)
    }
  }

  property("r1 - r2 logic") {
    forAll { (r1: Region, r2: Region) =>
      val r3 = r1 - r2
      if (r1.start < r2.start && r2.start <= r1.end) {
        assertEquals(r3.start, r1.start)
        assertEquals(r3.end, r2.start - 1)
      } else {
        assert(r3.eqv(r1))
      }
    }
  }

  property("Region.regionSemigroup is associative") {
    forAll { (r1: Region, r2: Region, r3: Region) =>
      val semi = Region.regionSemigroup
      val left = semi.combine(semi.combine(r1, r2), r3)
      val right = semi.combine(r1, semi.combine(r2, r3))
      assert(left.eqv(right))
    }
  }

  property("Region follows OrderingLaws") {
    forAll { (r1: Region, r2: Region, r3: Region) =>
      OrderingLaws.law(r1, r2, r3)(using Region.ordering)
    }
  }

  property("Region.ordering matches Region.regionOrder") {
    forAll { (r1: Region, r2: Region) =>
      assertEquals(Region.ordering.compare(r1, r2), Region.regionOrder.compare(r1, r2))
    }
  }

  property("Region.regionShow matches [start, end)") {
    forAll { (s: Int, e: Int) =>
      val r = Region(s, e)
      assertEquals(Show[Region].show(r), s"[$s, $e)")
    }
  }

  property("Region.regionHash matches Long hash") {
    forAll { (r: Region) =>
      val h = Hash[Region]
      // Long.hashCode is what UniversalHashCode uses for Longs
      assertEquals(h.hash(r), r.asInstanceOf[Long].hashCode)
    }
  }

  property("Hash.eqv is the same as .eqv") {
    forAll { (r1: Region, r2: Region) =>
      assertEquals(Hash[Region].eqv(r1, r2), r1.eqv(r2))
    }
  }

  test("Order has higher priority than Hash for Eq") {
    // This confirms that Eq[Region] is resolved to Order[Region]
    val eq = Eq[Region]
    assert(eq.isInstanceOf[cats.kernel.Order[?]])
  }
}
