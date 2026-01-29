package dev.bosatsu

import org.scalacheck.{Arbitrary, Gen, Shrink}

object GenJson {

  val genJsonNumber: Gen[Json.JNumberStr] = {
    def cat(gs: List[Gen[String]]): Gen[String] =
      gs match {
        case Nil       => Gen.const("")
        case h :: tail => Gen.zip(h, cat(tail)).map { case (a, b) => a + b }
      }

    val digit09 = Gen.oneOf('0' to '9').map(_.toString)
    val digit19 = Gen.oneOf('1' to '9').map(_.toString)
    val digits = Gen.listOf(digit09).map(_.mkString)
    val digits1 = Gen.zip(digit09, Gen.listOf(digit09)).map { case (h, t) =>
      (h :: t).mkString
    }
    val int = Gen.frequency(
      (1, Gen.const("0")),
      (20, Gen.zip(digit19, digits).map { case (h, t) => h + t })
    )
    val frac = digits1.map("." + _)

    def opt(g: Gen[String]): Gen[String] =
      Gen.oneOf(true, false).flatMap {
        case true  => g
        case false => Gen.const("")
      }
    val exp = cat(List(Gen.oneOf("e", "E"), opt(Gen.oneOf("+", "-")), digits1))
    cat(List(opt(Gen.const("-")), int, opt(frac), opt(exp)))
      .map(Json.JNumberStr(_))
  }

  def genJson(depth: Int): Gen[Json] = {
    val genString = Gen.listOf(Gen.choose(1.toChar, 127.toChar)).map(_.mkString)
    val str = genString.map(Json.JString(_))
    val nd1 = Arbitrary.arbitrary[Long].map(i => Json.JNumberStr(i.toString))
    val nd2 =
      Arbitrary.arbitrary[Double].map(d => Json.JNumberStr(d.toString))
    val nd3 = Arbitrary.arbitrary[Int].map(i => Json.JNumberStr(i.toString))
    val b = Gen.oneOf(Json.JBool(true), Json.JBool(false))

    val d0 = Gen.oneOf(str, nd1, nd2, nd3, b, Gen.const(Json.JNull))
    if (depth <= 0) d0
    else {
      val recurse = Gen.lzy(genJson(depth - 1))
      val collectionSize = Gen.choose(0, depth * depth)
      val ary = collectionSize.flatMap(
        Gen.listOfN(_, recurse).map(l => Json.JArray(l.toVector))
      )
      val map = collectionSize.flatMap { sz =>
        Gen
          .listOfN(sz, Gen.zip(genString, recurse))
          .map(m => Json.JObject(m).normalize)
      }
      Gen.frequency((10, d0), (1, ary), (1, map))
    }
  }

  implicit val arbJson: Arbitrary[Json] =
    Arbitrary(Gen.choose(0, 4).flatMap(genJson(_)))

  implicit def shrinkJson(implicit
      ss: Shrink[String]
  ): Shrink[Json] =
    Shrink.withLazyList { j =>
      import Json._
      j match {
        case JString(str) =>
          LazyList.from(ss.shrink(str)).map(JString(_))
        case JNumberStr(_)            => LazyList.empty
        case JNull                    => LazyList.empty
        case JBool.True | JBool.False => LazyList.empty
        case JArray(js)               =>
          (0 until js.size).to(LazyList).map { sz =>
            JArray(js.take(sz))
          }
        case JObject(mapList) =>
          (0 until mapList.size).to(LazyList).map { sz =>
            JObject(mapList.take(sz))
          }
      }
    }

  lazy val genPath: Gen[Json.Path] =
    Gen.oneOf(
      Gen.const(Json.Path.Root),
      for {
        idx <- Gen.choose(0, Int.MaxValue)
        of <- genPath
      } yield Json.Path.Index(of, idx),
      for {
        key <- Gen.asciiPrintableStr
        of <- genPath
      } yield Json.Path.Key(of, key)
    )
}
