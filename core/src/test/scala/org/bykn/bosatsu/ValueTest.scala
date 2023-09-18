package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import Value._
import org.scalatest.funsuite.AnyFunSuite

class ValueTest extends AnyFunSuite {
  import GenValue.genValue

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // PropertyCheckConfiguration(minSuccessful = 5000)
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
        case other             => fail(s"expected Some($v) got $other")
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
        case other      => fail(s"expected VList($vs) got $other")
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

  val stringValue: Gen[List[(String, Value)]] =
    Gen.listOf(Gen.zip(Arbitrary.arbitrary[String], genValue))

  test("VDict round trips") {
    forAll(stringValue) { svs =>
      val dictv = VDict.fromStringKeys(svs)
      val svsDedup = svs.toMap.toList.sortBy(_._1)

      dictv match {
        case VDict(asMap) =>
          val asList = asMap.toList.map {
            case (ExternalValue(s: String), v) => (s, v)
            case kv =>
              sys.error(s"expected a string key, found: $kv")
          }
          assert(asList == svsDedup)
        case other =>
          assert(false, s"$other didn't match VDict")
      }
    }
  }

  test("Tuple.fromList/unapply works") {
    forAll(Gen.listOf(genValue)) { vs =>
      Tuple.fromList(vs) match {
        case Tuple(items) =>
          assert(items == vs)
        case notTuple =>
          assert(false, s"not tuple: $notTuple")
      }
    }
  }

  test("strOrdFn round trips correctly") {
    val ord = VDict.keyOrderingFromOrdFn(VDict.strOrdFn)

    // the above should compare keys of tuples
    forAll(stringValue) { svs =>
      val svsSorted = svs.sortBy(_._1)

      val sortByOrd = svs.sortBy { case (k, _) => ExternalValue(k): Value }(ord)

      assert(svsSorted == sortByOrd)
    }
  }
}
