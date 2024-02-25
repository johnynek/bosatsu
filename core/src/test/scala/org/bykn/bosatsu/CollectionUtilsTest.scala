package org.bykn.bosatsu

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.scalatest.funsuite.AnyFunSuite

class CollectionUtilsTest extends AnyFunSuite {
  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)
  // PropertyCheckConfiguration(minSuccessful = 5)

  test("listToUnique works for maps converted to lists") {
    forAll { (m: Map[Int, Int]) =>
      assert(CollectionUtils.listToUnique(m.toList)(_._1, _._2, "").get == m)
    }
  }
}
