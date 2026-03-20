package dev.bosatsu

import cats.data.Validated

class ParserHintsTest extends munit.FunSuite {

  private def parseFailure(source: String): Parser.Error.ParseFailure =
    Parser.parse(Package.parser, source) match {
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

  test("using match in a condition suggests matches") {
    val source =
      """package Foo
        |
        |x = (
        |  if value match Foo: 1
        |  else: 0
        |)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      hints.exists(_.contains("meant 'matches'")),
      hints.mkString("\n")
    )
  }

  test("using matches as a match header suggests match") {
    val source =
      """package Foo
        |
        |x = matches value:
        |  case _: 1
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    val shown = pf.showContext(LocationMap.Colorize.None).render(120)
    assert(
      hints.exists(_.contains("meant 'match'")),
      hints.mkString("\n") + "\n" + shown
    )
  }

  test("zero-arg defs are supported") {
    val source =
      """package Foo
        |
        |def usage():
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    assert(
      !hints.exists(_.contains("zero-arg def syntax is not supported")),
      hints.mkString("\n")
    )
  }

  test("literal github actions expression in string suggests $$ for a literal dollar") {
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
      hints.exists(_.contains("`$foo` and `$.foo`")),
      hints.mkString("\n")
    )
    assert(
      hints.exists(_.contains("`$$`")),
      hints.mkString("\n")
    )
  }

  test("invalid interpolation expression suggests $$ for a literal dollar") {
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
      hints.exists(_.contains("`$$`")),
      hints.mkString("\n")
    )
  }

  test("ambiguous raw interpolation suggests braces or $$") {
    List(
      """package Foo
        |
        |s = '$foo(bar)'
        |""".stripMargin,
      """package Foo
        |
        |s = '$.foo(bar)'
        |""".stripMargin
    ).foreach { source =>
      val pf = parseFailure(source)
      val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
      val shown = pf.showContext(LocationMap.Colorize.None).render(120)

      assert(
        hints.exists(_.contains("single bindable")),
        hints.mkString("\n") + "\n" + shown
      )
      assert(
        hints.exists(h => h.contains("`${...}`") && h.contains("`$.{...}`")),
        hints.mkString("\n") + "\n" + shown
      )
      assert(
        hints.exists(_.contains("`$$`")),
        hints.mkString("\n") + "\n" + shown
      )
    }
  }

  test("missing trailing expression after nested def gets hint") {
    val source =
      """package Foo
        |
        |def flat_map(ll: LazyList[a], fn: a -> LazyList[b]) -> LazyList[b]:
        |    def loop(first: Int, ll):
        |        match (first, ll):
        |            case (_, Empty): Empty
        |            case (_, Cons(h, t)):
        |                match fn(get_Lazy(h)):
        |                    case Empty: todo(())
        |                    case not_empty:
        |                        Concat(not_empty, lazy(() -> flat_map(get_Lazy(t), fn)))
        |            case (_, Concat(first, second)):
        |                concat_lazy(flat_map(first, fn), lazy(() -> flat_map(get_Lazy(second), fn)))
        |            case (f, Mapped(ll, fn1)):
        |                if cmp_Int(f, 0) matches GT:
        |                    loop(0, run_map(ll, fn1))
        |                else:
        |                    Empty
        |
        |# invariant: neither current nor pending should be empty
        |def uncons_step(rem: Int, current: LazyList[a], pending: List[LazyList[a]]) -> Option[(Lazy[a], LazyList[a])]:
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    val shown = pf.showContext(LocationMap.Colorize.None).render(120)
    assert(
      hints.exists(_.contains("def ended without a final expression")),
      hints.mkString("\n") + "\n" + shown
    )
    assert(
      shown.contains("def ended without a final expression"),
      shown
    )
  }

  test("unexpected indentation in def body gets dedicated hint") {
    val source =
      """package Foo
        |
        |def map(ll: LazyList[a], fn: a -> b) -> LazyList[b]:
        |    LazyList(sz, vll) = ll
        |    vll1 = match vll:
        |        case Empty: Empty
        |        case Mapped(mll, fn1): Mapped(mll, x -> fn(fn1(x)))
        |        case notEmptyMapped: Mapped(notEmptyMapped, fn)
        |     LazyList(sz, vll1)
        |""".stripMargin

    val pf = parseFailure(source)
    val hints = ParserHints.hints(source, pf.locations, pf).map(_.render(120))
    val shown = pf.showContext(LocationMap.Colorize.None).render(120)
    assert(
      hints.exists(_.contains("unexpected indentation")),
      hints.mkString("\n") + "\n" + shown
    )
    assert(
      shown.contains("unexpected indentation"),
      shown
    )
  }
}
