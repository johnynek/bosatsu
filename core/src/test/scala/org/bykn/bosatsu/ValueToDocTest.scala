package org.bykn.bosatsu

import org.scalatest.prop.PropertyChecks.{forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

import rankn.NTypeGen

class ValueToDocTest extends FunSuite {

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000)

  test("never throw when converting to doc") {
    val vd = ValueToDoc({ _ => None})

    forAll(NTypeGen.genPredefType, GenValue.genValue) { (t, v) =>
      vd.toDoc(t)(v)
      succeed
    }
  }
  // TODO it would be nice to test some handwritten examples
}
