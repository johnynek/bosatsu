package dev.bosatsu

import cats.data.Validated

class ParserHintsTest extends munit.FunSuite {

  private def parseFailure(source: String): Parser.Error.ParseFailure =
    Parser.parse(Package.parser(None), source) match {
      case Validated.Valid(_) =>
        fail(s"expected parse failure for:\n$source")
      case Validated.Invalid(errs) =>
        errs.head match {
          case pf @ Parser.Error.ParseFailure(_, _, _) => pf
        }
    }

  test("else if after else gets a hint about ':' and elif") {
    val source =
      """package Foo
        |
        |x = (
        |  if True: 1
        |  else if False: 2
        |)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("after else expected ':'")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("elif <condition>:")),
      hints.mkString("\n")
    )

    val shown = pf.showContext(LocationMap.Colorize.None).render(120)
    assert(shown.contains("after else expected ':'"), shown)
  }

  test("missing ':' after if does not trigger else-if hint") {
    val source =
      """package Foo
        |
        |x = (
        |  if True 1
        |  else: 2
        |)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("missing ':' after if header")),
      hints.mkString("\n")
    )
    assert(
      !hints.exists(_.contains("elif <condition>:")),
      hints.mkString("\n")
    )
  }

  test("missing ':' after inline if expression gets if-header hint") {
    val source =
      """package Foo
        |
        |x = if foo(x)
        |      bar
        |    else:
        |      baz
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("missing ':' after if header")),
      hints.mkString("\n")
    )
  }

  test("missing ':' after loop header gets loop-header hint") {
    val source =
      """package Foo
        |
        |def len(lst):
        |  loop lst
        |    case []: 0
        |    case [_, *tail]: len(tail)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("missing ':' after loop header")),
      hints.mkString("\n")
    )
  }

  test("elseif and elsif get hint to use elif") {
    List("elseif", "elsif").foreach { badElif =>
      val source =
        s"""package Foo
           |
           |x = (
           |  if True: 1
           |  $badElif False: 2
           |  else: 3
           |)
           |""".stripMargin

      val pf = parseFailure(source)
      val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
      assert(
        hints.exists(_.contains("use 'elif'")),
        hints.mkString("\n")
      )
    }
  }

  test("single equals in condition with Int suggests eq_Int") {
    val source =
      """package Foo
        |
        |x = (
        |  if x = 2: 1
        |  else: 2
        |)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("For Ints, use `eq_Int(a, b)`")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("operator == = eq_Int")),
      hints.mkString("\n")
    )
  }

  test("single equals in condition without Int suggests equality function") {
    val source =
      """package Foo
        |
        |x = (
        |  if left = right: 1
        |  else: 2
        |)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("Use an equality function call")),
      hints.mkString("\n")
    )
    assert(
      !hints.exists(_.contains("For Ints, use `eq_Int(a, b)`")),
      hints.mkString("\n")
    )
  }

  test("zero-arg def gets actionable hint") {
    val source =
      """package Foo
        |
        |def usage() -> String:
        |  "usage"
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("zero-arg def syntax is not supported")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("usage = ...")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("def usage(_): ...")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("usage(())")),
      hints.mkString("\n")
    )
  }

  test("literal github actions expression in string suggests escaping '${'") {
    val source =
      """package Foo
        |
        |s = "${{ matrix.java }}"
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("parse failed after '${'")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("""which starts string interpolation (`${x}`)""")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("""write `\${`""")),
      hints.mkString("\n")
    )
  }

  test("invalid interpolation expression suggests maybe literal '${'") {
    val source =
      """package Foo
        |
        |s = "${foo.bar}"
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("parse failed after '${'")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("""write `\${`""")),
      hints.mkString("\n")
    )
  }
}
