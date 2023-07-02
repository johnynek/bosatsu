package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Cogen, Gen}
import Value._

object GenValue {

  val cogenValue: Cogen[Value] =
    Cogen[Int].contramap { (v: Value) => v.hashCode }

  lazy val genProd: Gen[ProductValue] =
    Gen.lzy(
      Gen.oneOf(
        Gen.const(UnitValue),
        genValue.flatMap { v => genProd.map(ConsValue(v, _)) }
      )
    )

  lazy val genValue: Gen[Value] = {
    val recur = Gen.lzy(genValue)
    val genEnumLike = Gen.choose(0, 1024).map(SumValue(_, UnitValue))

    val genSum =
      for {
        i <- Gen.choose(0, 1024)
        v <- genProd
      } yield SumValue(i, v)

    val genExt: Gen[Value] =
      Gen.oneOf(
        Gen.choose(Int.MinValue, Int.MaxValue).map(VInt(_)),
        Arbitrary.arbitrary[String].map(Str(_))
      )

    val genFn: Gen[FnValue] = {
      val fn: Gen[Value => Value] = Gen.function1(recur)(cogenValue)

      fn.map(FnValue(_))
    }

    Gen.oneOf(genEnumLike, genProd, genSum, genExt, genFn)
  }
}
