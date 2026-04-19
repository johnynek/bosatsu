package dev.bosatsu

class Issue2282Test extends ParserTestBase {
  private def parsePattern(str: String): Pattern.Parsed =
    Parser.unsafeParse(Pattern.matchParser, str)

  test("issue 2282: match patterns allow multiline unions") {
    val expected = parsePattern("Foo | Bar | Baz")

    parseTestAll(
      Pattern.matchParser,
      """Foo |
        |  Bar |
        |  Baz""".stripMargin,
      expected
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
        |  case Foo |
        |    Bar |
        |    Baz:
        |
        |    quux""".stripMargin
    )
  }

  test("issue 2282: struct patterns allow multiline internal whitespace") {
    parseTestAll(
      Pattern.matchParser,
      """Foo(
        |  Bar
        |)""".stripMargin,
      parsePattern("Foo(Bar)")
    )

    parseTestAll(
      Pattern.matchParser,
      """Foo(
        |  Bar,
        |  Baz,
        |)""".stripMargin,
      parsePattern("Foo(Bar, Baz,)")
    )

    parseTestAll(
      Pattern.matchParser,
      """Foo {
        |  left: Bar,
        |  right: Baz,
        |}""".stripMargin,
      parsePattern("Foo { left: Bar, right: Baz, }")
    )

    roundTrip(
      Declaration.parser(""),
      """match x:
        |  case Foo(
        |      Bar
        |  ):
        |    quux
        |  case Foo(
        |      Bar,
        |      Baz,
        |  ):
        |    quux
        |  case Foo {
        |      left: Bar,
        |      right: Baz,
        |  }:
        |    quux""".stripMargin
    )
  }
}
