package dev.bosatsu

import dev.bosatsu.codegen.clang.CcConf

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

  case class DerivedConfig(
      cc_path: String,
      flags: List[String],
      iflags: List[String],
      note: Option[String]
  ) derives Json.Reader, Json.Writer

  case class DerivedNullableConfig(
      cc_path: String,
      note: Nullable[String]
  ) derives Json.Reader, Json.Writer

  test("derived writer uses case class field names and omits none fields") {
    val config = DerivedConfig(
      cc_path = "cc",
      flags = List("-O2"),
      iflags = List("-Iinclude"),
      note = None
    )

    val json = Json.Writer.write(config)

    assertEquals(
      json,
      Json.JObject(
        ("cc_path" -> Json.JString("cc")) ::
          ("flags" -> Json.JArray(Vector(Json.JString("-O2")))) ::
          ("iflags" -> Json.JArray(Vector(Json.JString("-Iinclude")))) ::
          Nil
      )
    )
  }

  test("derived reader treats missing optional fields as none") {
    val json = Json.JObject(
      ("cc_path" -> Json.JString("cc")) ::
        ("flags" -> Json.JArray(Vector(Json.JString("-O2")))) ::
        ("iflags" -> Json.JArray(Vector.empty)) ::
        Nil
    )

    assertEquals(
      Json.Reader[DerivedConfig].read(Json.Path.Root, json),
      Right(
        DerivedConfig(
          cc_path = "cc",
          flags = List("-O2"),
          iflags = Nil,
          note = None
        )
      )
    )
  }

  test("derived reader treats missing nullable fields as empty") {
    val json = Json.JObject(
      ("cc_path" -> Json.JString("cc")) :: Nil
    )

    val decoded = Json.Reader[DerivedNullableConfig]
      .read(Json.Path.Root, json)
      .toOption
      .getOrElse(sys.error("expected successful decode"))

    assertEquals(decoded.cc_path, "cc")
    assert(decoded.note.isNull)
  }

  test("derived writer emits nullable empty fields as explicit null") {
    val config = DerivedNullableConfig(
      cc_path = "cc",
      note = Nullable.empty
    )

    assertEquals(
      Json.Writer.write(config),
      Json.JObject(
        ("cc_path" -> Json.JString("cc")) ::
          ("note" -> Json.JNull) ::
          Nil
      )
    )
  }

  test("CcConf round-trips with derived reader and writer") {
    val conf = CcConf(
      cc_path = "cc",
      flags = List("-O2"),
      iflags = List("-Iinclude"),
      libs = List("-lm"),
      os = "linux"
    )

    val json = Json.JObject(
      ("cc_path" -> Json.JString("cc")) ::
        ("flags" -> Json.JArray(Vector(Json.JString("-O2")))) ::
        ("iflags" -> Json.JArray(Vector(Json.JString("-Iinclude")))) ::
        ("libs" -> Json.JArray(Vector(Json.JString("-lm")))) ::
        ("os" -> Json.JString("linux")) ::
        Nil
    )

    assertEquals(CcConf.parse(json), Right(conf))
    assertEquals(Json.Writer.write(conf), json)
  }

  test("strict JSON documents use JSON string escapes without changing the permissive parser") {
    val valid = List(
      "\"\\/\"" -> JString("/"),
      "\"\\b\\f\\n\\r\\t\\\"\\\\\"" -> JString("\b\f\n\r\t\"\\"),
      "\"\\u96ea\\uD83D\\uDE42\"" -> JString("雪🙂"),
      "{\"key\\u002fname\":[\"value\\/part\"]}" -> Json.JObject(
        List("key/name" -> Json.JArray(Vector(JString("value/part"))))
      )
    )
    valid.foreach { case (document, expected) =>
      assertEquals(Json.strictParserFile.parseAll(document), Right(expected), document)
    }

    val invalidEscapes = List(
      "\"\\x41\"",
      "\"\\o41\"",
      "\"\\U00000041\"",
      "\"\\'\"",
      "\"\\q\"",
      "\"\\u12\"",
      "\"\\u12G4\""
    )
    invalidEscapes.foreach { document =>
      assert(Json.strictParserFile.parseAll(document).isLeft, document)
    }

    (0 until 0x20).foreach { control =>
      val document = "\"a" + control.toChar + "b\""
      assert(Json.strictParserFile.parseAll(document).isLeft, s"accepted control U+${control.toHexString}")
    }

    assertEquals(Json.parserFile.parseAll("\"\\x41\""), Right(JString("A")))
    assertEquals(Json.parserFile.parseAll("\"raw\nnewline\""), Right(JString("raw\nnewline")))
    assert(Json.parserFile.parseAll("\"\\/\"").isLeft)
  }
}
