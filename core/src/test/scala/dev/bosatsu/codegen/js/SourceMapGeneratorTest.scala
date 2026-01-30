package dev.bosatsu.codegen.js

class SourceMapGeneratorTest extends munit.FunSuite {
  import SourceMapGenerator._

  test("VLQ.encode handles zero") {
    assertEquals(VLQ.encode(0), "A")
  }

  test("VLQ.encode handles positive numbers") {
    assertEquals(VLQ.encode(1), "C")
    assertEquals(VLQ.encode(2), "E")
    assertEquals(VLQ.encode(15), "e")
    assertEquals(VLQ.encode(16), "gB")  // Requires continuation
    assertEquals(VLQ.encode(31), "+B")
    assertEquals(VLQ.encode(32), "gC")
  }

  test("VLQ.encode handles negative numbers") {
    assertEquals(VLQ.encode(-1), "D")
    assertEquals(VLQ.encode(-2), "F")
    assertEquals(VLQ.encode(-15), "f")
  }

  test("VLQ.encodeSegment encodes multiple values") {
    // Common pattern: column 0, source 0, line 0, column 0
    assertEquals(VLQ.encodeSegment(0, 0, 0, 0), "AAAA")
    // Column 5, source 0, line 1, column 0
    assertEquals(VLQ.encodeSegment(5, 0, 1, 0), "KACA")
  }

  test("SourceMap.toJSON produces valid format") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List("foo", "bar"),
      mappings = List(
        Mapping(0, 0, 0, 0, 0, Some(0)),
        Mapping(0, 5, 0, 0, 5, Some(1))
      )
    )

    val json = map.toJSON
    assert(json.contains(""""version": 3"""))
    assert(json.contains(""""file": "output.js""""))
    assert(json.contains(""""sources": ["input.bosatsu"]"""))
    assert(json.contains(""""names": ["foo","bar"]"""))
    assert(json.contains(""""mappings":"""))
  }

  test("SourceMap handles multiple lines") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List(),
      mappings = List(
        Mapping(0, 0, 0, 0, 0),
        Mapping(1, 0, 0, 1, 0),
        Mapping(2, 0, 0, 2, 0)
      )
    )

    val json = map.toJSON
    // Should have semicolons separating lines
    val mappingsMatch = """"mappings": "([^"]+)"""".r
    mappingsMatch.findFirstMatchIn(json) match {
      case Some(m) =>
        val mappings = m.group(1)
        assertEquals(mappings.count(_ == ';'), 2)
      case None =>
        fail("No mappings found in JSON")
    }
  }

  test("SourceMap.toInlineComment produces data URL") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List(),
      mappings = List()
    )

    val comment = map.toInlineComment
    assert(comment.startsWith("//# sourceMappingURL=data:application/json;charset=utf-8;base64,"))
  }

  test("Builder accumulates sources") {
    val builder = new Builder("output.js")
    val idx1 = builder.addSource("file1.bosatsu")
    val idx2 = builder.addSource("file2.bosatsu")
    val idx1Again = builder.addSource("file1.bosatsu")

    assertEquals(idx1, 0)
    assertEquals(idx2, 1)
    assertEquals(idx1Again, 0)  // Should return same index

    val map = builder.build()
    assertEquals(map.sources, List("file1.bosatsu", "file2.bosatsu"))
  }

  test("Builder accumulates names") {
    val builder = new Builder("output.js")
    val idx1 = builder.addName("foo")
    val idx2 = builder.addName("bar")
    val idx1Again = builder.addName("foo")

    assertEquals(idx1, 0)
    assertEquals(idx2, 1)
    assertEquals(idx1Again, 0)

    val map = builder.build()
    assertEquals(map.names, List("foo", "bar"))
  }

  test("Builder creates mappings") {
    val builder = new Builder("output.js")
    val srcIdx = builder.addSource("input.bosatsu")

    builder.addMapping(0, 0, srcIdx, 0, 0)
    builder.addMapping(0, 10, srcIdx, 0, 5)
    builder.addMapping(1, 0, srcIdx, 1, 0, Some("myFunc"))

    val map = builder.build()
    assertEquals(map.mappings.length, 3)
    assertEquals(map.names, List("myFunc"))
  }

  test("Builder includes sourcesContent when provided") {
    val builder = new Builder("output.js")
    builder.addSource("input.bosatsu", Some("x = 1\n"))

    val map = builder.build()
    assertEquals(map.sourcesContent, Some(List("x = 1\n")))
  }

  test("SourceMap.toJSON includes sourcesContent with escaping") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List(),
      mappings = List(),
      sourcesContent = Some(List("x = 1\ny = 2"))
    )

    val json = map.toJSON
    // JSON format includes escaped newline
    assert(json.contains(""""sourcesContent":["x = 1\ny = 2"]"""))
  }

  test("SourceMap.toJSON handles null in sourcesContent") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu", "missing.bosatsu"),
      names = List(),
      mappings = List(),
      sourcesContent = Some(List("x = 1", null))
    )

    val json = map.toJSON
    assert(json.contains(""""sourcesContent":["x = 1",null]"""))
  }

  test("SourceMap.toJSON escapes special characters in sourcesContent") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List(),
      mappings = List(),
      sourcesContent = Some(List("\"quoted\" and \\ backslash and \t tab and \r carriage"))
    )

    val json = map.toJSON
    // Should have escaped quotes, backslashes, tabs, carriage returns
    assert(json.contains("\\\"quoted\\\""))
    assert(json.contains("\\\\"))
    assert(json.contains("\\t"))
    assert(json.contains("\\r"))
  }

  test("empty mappings produce empty string") {
    val map = SourceMap(
      file = "output.js",
      sources = List(),
      names = List(),
      mappings = List()
    )

    val json = map.toJSON
    assert(json.contains(""""mappings": """""))
  }

  test("relative encoding between segments on same line") {
    val map = SourceMap(
      file = "output.js",
      sources = List("input.bosatsu"),
      names = List(),
      mappings = List(
        Mapping(0, 0, 0, 0, 0),
        Mapping(0, 5, 0, 0, 5)  // +5 col, same source, same line, +5 col
      )
    )

    val json = map.toJSON
    // First segment: 0,0,0,0 = AAAA
    // Second segment: 5,0,0,5 = KAAK (relative)
    assert(json.contains("AAAA,KAAK"))
  }
}
