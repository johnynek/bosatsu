package org.bykn.bosatsu.pattern

import cats.Eq
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class IntLaws extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 500)

  val genBI: Gen[BigInteger] =
    Gen.choose(-128, 128)
      .map(new BigInteger(_))

  test("a = div(a, b) * b + mod(a, b)") {
    forAll(genBI, genBI) { (a, b) =>
      val a1 = PredefImpl.divBigInteger(a, b).times(b) + PredefImpl.modBigInteger(a, b)
      assert(a1 == a)
    }
  }
}
