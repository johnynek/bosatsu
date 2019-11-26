package org.bykn.bosatsu

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.prop.PropertyChecks.{forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite
import org.typelevel.jawn.ast.{JValue, JParser}

class JsonTest extends FunSuite {

  implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  def genJson(depth: Int): Gen[Json] = {
    val genString = Gen.listOf(Gen.choose(1.toChar, 127.toChar)).map(_.mkString)
    val str = genString.map(Json.JString(_))
    val nd0 = Arbitrary.arbitrary[Double].map(Json.JNumber(_))
    val nd1 = Arbitrary.arbitrary[Int].map { i => Json.JNumber(i.toDouble) }
    val nd2 = Arbitrary.arbitrary[Double].map { d => Json.JNumberStr(d.toString) }
    val nd3 = Arbitrary.arbitrary[Int].map { i => Json.JNumberStr(i.toString) }
    val b = Gen.oneOf(Json.JBool(true), Json.JBool(false))

    val d0 = Gen.oneOf(str, nd0, nd1, nd2, nd3, b, Gen.const(Json.JNull))
    if (depth <= 0) d0
    else {
      val recurse = Gen.lzy(genJson(depth - 1))
      val collectionSize = Gen.choose(0, depth * depth)
      val ary = collectionSize.flatMap(Gen.listOfN(_, recurse).map { l => Json.JArray(l.toVector) })
      val map = collectionSize.flatMap(Gen.listOfN(_, Gen.zip(genString, recurse)).map { m => Json.JObject(m) })
      Gen.frequency((10, d0), (1, ary), (1, map))
    }
  }

  implicit val arbJson: Arbitrary[Json] =
    Arbitrary(Gen.choose(0, 4).flatMap(genJson(_)))

  implicit def shrinkJson(
    implicit ss: Shrink[String],
    sd: Shrink[Double]): Shrink[Json] =
    Shrink[Json](new Function1[Json, Stream[Json]] {
      def apply(j: Json): Stream[Json] = {
        import Json._
        j match {
          case JString(str) => ss.shrink(str).map(JString(_))
          case JNumber(n) => sd.shrink(n).map(JNumber(_))
          case JNumberStr(nstr) => Stream.empty
          case JNull => Stream.empty
          case JBool(_) => Stream.empty
          case JArray(js) =>
            (0 until js.size).toStream.map { sz =>
              JArray(js.take(sz))
            }
          case JObject(mapList) =>
            (0 until mapList.size).toStream.map { sz =>
              JObject(mapList.take(sz))
            }
        }
      }
    })

  def matches(j1: Json, j2: JValue): Unit = {
    import Json._
    j1 match {
      case JString(str) => assert(j2.asString == str); ()
      case JNumber(n) => assert(j2.asDouble == n); ()
      case JNumberStr(nstr) => assert(BigDecimal(nstr) == j2.asBigDecimal); ()
      case JNull => assert(j2.isNull); ()
      case JBool(t) => assert(j2.asBoolean == t); ()
      case JArray(js) =>
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

  test("we can parse all the json we generate") {
    forAll { (j: Json) =>
      val str = j.render
      Json.parser.parse(str) match {
        case fastparse.all.Parsed.Success(j1, _) =>
          // JNumber and JNumberStr can be confused by parsing
          // since we prefer JNumber on parse if we don't lose
          // precision
          assert(j1.render == str)
        case other => fail(s"failed to parse:\n\n$str\n\n$j\n\nerror: $other")
      }
    }
  }
}
