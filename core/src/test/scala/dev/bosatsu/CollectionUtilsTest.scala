package dev.bosatsu

import org.scalacheck.Prop.forAll

class CollectionUtilsTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)
  // PropertyCheckConfiguration(minSuccessful = 5)

  test("listToUnique works for maps converted to lists") {
    forAll { (m: Map[Int, Int]) =>
      assertEquals(
        CollectionUtils.listToUnique(m.toList)(_._1, _._2, "").get,
        m
      )
    }
  }
}
