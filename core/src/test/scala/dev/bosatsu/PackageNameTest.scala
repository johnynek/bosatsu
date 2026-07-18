package dev.bosatsu

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.contravariant.*
import dev.bosatsu.rankn.NTypeGen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class PackageNameTest extends munit.ScalaCheckSuite {
  private val legacyOrder: Order[PackageName] =
    Order[NonEmptyList[String]].contramap[PackageName](_.parts)

  implicit val arbPackageName: Arbitrary[PackageName] =
    Arbitrary(NTypeGen.packageNameGen)

  property(
    "PackageName equality agrees with legacy parts equality for valid names"
  ) {
    forAll { (left: PackageName, right: PackageName) =>
      assertEquals(left == right, left.parts.toList == right.parts.toList)
      if (left == right) {
        assertEquals(left.hashCode, right.hashCode)
      }
    }
  }

  property(
    "PackageName ordering agrees with legacy parts ordering for valid names"
  ) {
    forAll { (left: PackageName, right: PackageName, third: PackageName) =>
      assertEquals(
        Integer.signum(Order[PackageName].compare(left, right)),
        Integer.signum(legacyOrder.compare(left, right))
      )
      OrderingLaws.forOrder(left, right, third)
    }
  }
}
