package dev.bosatsu

import org.scalacheck.Prop.forAll
import org.typelevel.jawn.ast.{JValue, JParser}

import GenJson._

class JsonJawnTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)

  def matches(j1: Json, j2: JValue): Unit = {
    import Json._
    j1 match {
      case JString(str)     => assertEquals(j2.asString, str); ()
      case JNumberStr(nstr) =>
        assertEquals(BigDecimal(nstr), j2.asBigDecimal); ()
      case JNull       => assert(j2.isNull); ()
      case JBool.True  => assert(j2.asBoolean); ()
      case JBool.False => assert(!j2.asBoolean); ()
      case JArray(js)  =>
        js.zipWithIndex.foreach { case (j, idx) =>
          matches(j, j2.get(idx))
        }
      case JObject(map) =>
        map.toMap.foreach { case (k, v) =>
          matches(v, j2.get(k))
        }
    }
  }

  test("Jawn can parse any of the json strings we generate") {
    forAll { (j: Json) =>
      val str = j.render
      val jvalue = JParser.parseUnsafe(str)
      matches(j, jvalue)
    }
  }
}
