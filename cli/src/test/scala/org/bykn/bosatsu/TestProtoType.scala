package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.NTypeGen
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class TestProtoType extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)
    //PropertyCheckConfiguration(minSuccessful = 5)

  test("we can roundtrip through proto") {
    forAll(NTypeGen.genDepth03) { tpe =>
      val maybeProto = ProtoConverter.typeToProto(tpe)
      assert(maybeProto.isSuccess, maybeProto.toString)
      val proto = maybeProto.get

      val maybeBack = ProtoConverter.typeFromProto(proto)
      assert(maybeBack.isSuccess, maybeBack.toString)
      val orig = maybeBack.get

      assert(tpe == orig)
    }
  }
}
