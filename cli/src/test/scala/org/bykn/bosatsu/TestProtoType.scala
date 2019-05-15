package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.NTypeGen
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import scala.util.Try

class TestProtoType extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 5000)
    PropertyCheckConfiguration(minSuccessful = 500)
    //PropertyCheckConfiguration(minSuccessful = 5)

  def law[A, B](a: A, fn: A => Try[B], gn: B => Try[A]) = {
    val maybeProto = fn(a)
    assert(maybeProto.isSuccess, maybeProto.toString)
    val proto = maybeProto.get

    val maybeBack = gn(proto)
    assert(maybeBack.isSuccess, maybeBack.toString)
    val orig = maybeBack.get

    assert(a == orig)
  }

  test("we can roundtrip types through proto") {
    forAll(NTypeGen.genDepth03) { tpe =>
      law(tpe, ProtoConverter.typeToProto _, ProtoConverter.typeFromProto _)
    }
  }

  test("we can roundtrip interfaces through proto") {
    forAll(Generators.interfaceGen) { iface =>
      law(iface, ProtoConverter.interfaceToProto _, ProtoConverter.interfaceFromProto _)
    }
  }
}
