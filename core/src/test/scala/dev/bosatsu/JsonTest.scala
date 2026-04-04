package dev.bosatsu

class JsonTest extends munit.FunSuite {
  import Json.{JBool, JNumberStr, JObject, JString, Path, Reader, Writer}

  test("Reader has shared codecs for basic scala and json types") {
    assertEquals(Reader[Int].read(Path.Root, JNumberStr("42")), Right(42))
    assertEquals(Reader[Boolean].read(Path.Root, JBool.True), Right(true))
    assertEquals(Reader[Json].read(Path.Root, JString("x")), Right(JString("x")))
  }

  test("Writer has shared codecs for basic scala and java types") {
    assertEquals(Writer.write(42), JNumberStr("42"))
    assertEquals(Writer.write(false), JBool.False)
    assertEquals(
      Writer.write(Map("b" -> "two", "a" -> "one")),
      JObject(("a" -> JString("one")) :: ("b" -> JString("two")) :: Nil)
    )
  }
}
