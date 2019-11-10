package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import Value._

object GenValue {

  val cogenValue: Cogen[Value] =
    Cogen[Int].contramap { v: Value => v.hashCode }

  lazy val genProd: Gen[ProductValue] =
    Gen.lzy(Gen.oneOf(Gen.const(UnitValue),
      genValue.flatMap { v => genProd.map(ConsValue(v, _)) }))

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
        Arbitrary.arbitrary[String].map(Str(_)))

    val genFn: Gen[FnValue] = {
      val fn: Gen[Value => Value] = Gen.function1(recur)(cogenValue)

      fn.map { valueFn =>

        FnValue { eval => eval.map(valueFn(_)) }
      }
    }

    Gen.oneOf(genEnumLike, genProd, genSum, genExt, genFn)
  }
}

class ValueTest extends FunSuite {
  import GenValue.genValue

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  test("SumValue.toString is what we expect") {
    forAll(Gen.choose(0, 1024), GenValue.genProd) { (i, p) =>
      assert(SumValue(i, p).toString == s"SumValue($i, $p)")
    }
  }

  test("Value.equals is false if the class isn't right") {
    forAll(genValue, genValue) { (v1, v2) =>
      if (v1.getClass != v2.getClass) assert(v1 != v2)
      else if (v1 == v2) assert(v1.getClass == v2.getClass)
    }
  }

  test("VOption works") {
    forAll(genValue) { v =>
      VOption.some(v) match {
        case VOption(Some(v1)) => assert(v1 == v)
        case other => fail(s"expected Some($v) got $other")
      }
    }

    forAll(genValue) { v =>
      VOption.unapply(v) match {
        case None => ()
        case Some(None) =>
          assert(v == VOption.none)
        case Some(Some(v1)) =>
          assert(v == VOption.some(v1))
      }
    }

    assert(VOption.unapply(VOption.none) == Some(None))
  }

  test("VList works") {
    forAll(Gen.listOf(genValue)) { vs =>
      VList(vs) match {
        case VList(vs1) => assert(vs1 == vs)
        case other => fail(s"expected VList($vs) got $other")
      }
    }

    forAll(genValue) { v =>
      VList.unapply(v) match {
        case None => ()
        case Some(Nil) =>
          assert(v == VList.VNil)
        case Some(v1) =>
          assert(v == VList(v1))
      }
    }

    assert(VList.unapply(VList.VNil) == Some(Nil))
  }
}
