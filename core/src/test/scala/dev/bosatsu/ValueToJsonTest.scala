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
}
