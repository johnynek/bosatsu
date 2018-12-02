package org.bykn.bosatsu

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }

class TypeRefTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 500000)
    PropertyCheckConfiguration(minSuccessful = 5000)

  import Generators.typeRefGen

  test("Ordering is lawful") {
    forAll(typeRefGen, typeRefGen, typeRefGen) { (a, b, c) =>
      OrderingLaws.law(a, b, c)
    }
  }
}
