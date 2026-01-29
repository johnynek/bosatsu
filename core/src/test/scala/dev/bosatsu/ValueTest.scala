package dev.bosatsu

import cats.Eq
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Value._

class ValueTest extends munit.ScalaCheckSuite {
  import GenValue.genValue

  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 5000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)

  test("SumValue.toString is what we expect") {
    forAll(Gen.choose(0, 1024), GenValue.genProd) { (i, p) =>
      assertEquals(SumValue(i, p).toString, s"SumValue($i, $p)")
    }
  }

  test("Value.equals is false if the class isn't right") {
    forAll(genValue, genValue) { (v1, v2) =>
      given Eq[Class[? <: Value]] =
        // Safe: Class equality is reference identity.
        Eq.fromUniversalEquals
      if (v1.getClass =!= v2.getClass) assert(v1 != v2)
      else if (v1 == v2) assertEquals(v1.getClass, v2.getClass)
    }
  }

  test("VOption works") {
    val propSome = forAll(genValue) { v =>
      VOption.some(v) match {
        case VOption(Some(v1)) => assertEquals(v1, v)
        case other             => fail(s"expected Some($v) got $other")
      }
    }

    val propUnapply = forAll(genValue) { v =>
      VOption.unapply(v) match {
        case None       => ()
        case Some(None) =>
          assertEquals(v, VOption.none)
        case Some(Some(v1)) =>
          assertEquals(v, VOption.some(v1))
      }
    }

    assertEquals(VOption.unapply(VOption.none), Some(None))
    org.scalacheck.Prop.all(propSome, propUnapply)
  }

  test("VList works") {
    val propList = forAll(Gen.listOf(genValue)) { vs =>
      VList(vs) match {
        case VList(vs1) => assertEquals(vs1, vs)
        case other      => fail(s"expected VList($vs) got $other")
      }
    }

    val propUnapply = forAll(genValue) { v =>
      VList.unapply(v) match {
        case None      => ()
        case Some(Nil) =>
          assertEquals(v, VList.VNil)
        case Some(v1) =>
          assertEquals(v, VList(v1))
      }
    }

    assertEquals(VList.unapply(VList.VNil), Some(Nil))
    org.scalacheck.Prop.all(propList, propUnapply)
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
            case kv                            =>
              sys.error(s"expected a string key, found: $kv")
          }
          assertEquals(asList, svsDedup)
        case other =>
          assert(false, s"$other didn't match VDict")
      }
    }
  }

  test("Tuple.fromList/unapply works") {
    forAll(Gen.listOf(genValue)) { vs =>
      Tuple.fromList(vs) match {
        case Tuple(items) =>
          assertEquals(items, vs)
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

      val sortByOrd =
        svs.sortBy { case (k, _) => ExternalValue(k): Value }(using ord)

      assertEquals(svsSorted, sortByOrd)
    }
  }
}
