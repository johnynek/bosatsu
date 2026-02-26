package dev.bosatsu

import dev.bosatsu.rankn.Type

class ValueToJsonTest extends munit.FunSuite {
  private val conv = ValueToJson(_ => None)
  private val arrayType: Type = Type.TyApply(
    Type.const(
      PackageName.parts("Bosatsu", "Collection", "Array"),
      TypeName("Array")
    ),
    Type.IntType
  )
  private val bytesType: Type =
    Type.const(PackageName.parts("Bosatsu", "IO", "Bytes"), TypeName("Bytes"))

  test("char toJson reports ill-typed runtime values") {
    val toJson = conv.toJson(Type.CharType) match {
      case Right(fn) => fn
      case Left(err) => fail(s"expected Char json conversion support: $err")
    }

    toJson(Value.Str("ab")) match {
      case Left(_)     => ()
      case Right(json) =>
        fail(s"expected ill-typed Char conversion failure, got: ${json.render}")
    }
  }

  test("array toJson reports ill-typed runtime values") {
    val toJson = conv.toJson(arrayType) match {
      case Right(fn) => fn
      case Left(err) => fail(s"expected Array json conversion support: $err")
    }

    toJson(Value.UnitValue) match {
      case Left(_)     => ()
      case Right(json) =>
        fail(s"expected ill-typed Array conversion failure, got: ${json.render}")
    }
  }

  test("array toValue reports ill-typed json values") {
    val toValue = conv.toValue(arrayType) match {
      case Right(fn) => fn
      case Left(err) => fail(s"expected Array json parsing support: $err")
    }

    toValue(Json.JString("not-an-array")) match {
      case Left(_)      => ()
      case Right(value) =>
        fail(s"expected ill-typed Array parsing failure, got: $value")
    }
  }

  test("bytes toJson renders as json integer array") {
    val toJson = conv.toJson(bytesType) match {
      case Right(fn) => fn
      case Left(err) => fail(s"expected Bytes json conversion support: $err")
    }
    val bytes = Value.ExternalValue(
      PredefImpl.BytesValue(Array[Byte](0.toByte, 1.toByte, 255.toByte), 0, 3)
    )
    toJson(bytes) match {
      case Right(Json.JArray(values)) =>
        assertEquals(
          values.toList,
          List(
            Json.JNumberStr("0"),
            Json.JNumberStr("1"),
            Json.JNumberStr("255")
          )
        )
      case Right(other) =>
        fail(s"expected bytes json array, got: $other")
      case Left(err) =>
        fail(s"unexpected bytes toJson failure: $err")
    }
  }

  test("bytes toValue rejects ints outside [0,255]") {
    val toValue = conv.toValue(bytesType) match {
      case Right(fn) => fn
      case Left(err) => fail(s"expected Bytes json parsing support: $err")
    }

    val tooLarge = toValue(Json.JArray(Vector(Json.JNumberStr("256"))))
    assert(tooLarge.isLeft, tooLarge.toString)

    val negative = toValue(Json.JArray(Vector(Json.JNumberStr("-1"))))
    assert(negative.isLeft, negative.toString)
  }
}
