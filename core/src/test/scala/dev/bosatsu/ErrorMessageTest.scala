package dev.bosatsu

import cats.Show
import cats.data.Validated
import dev.bosatsu.LocationMap.Colorize

class ErrorMessageTest extends munit.FunSuite with ParTest {

  private def unusedLetMessage(source: String): String = {
    val parsed = Parser.parse(Package.parser(None), source) match {
      case Validated.Valid((lm, pack)) =>
        (("0", lm), pack) :: Nil
      case Validated.Invalid(errs)     =>
        fail(s"parse failed: $errs")
    }

    val withPre = PackageMap.withPredefA(("predef", LocationMap("")), parsed)
    val withPrePaths = withPre.map { case ((path, _), p) => (path, p) }

    implicit val showString: Show[String] = Show.fromToString
    val errsOpt = PackageMap.resolveThenInfer(withPrePaths, Nil).left
    val errs = errsOpt.getOrElse(fail("expected unused let error"))
    val sourceMap = PackageMap.buildSourceMap(withPre)
    val msgOpt = errs.toList.collectFirst {
      case e @ PackageError.UnusedLets(_, _) =>
        e.message(sourceMap, Colorize.None)
      case e @ PackageError.UnusedLetError(_, _) =>
        e.message(sourceMap, Colorize.None)
    }
    msgOpt.getOrElse(fail(s"expected unused let error, found: $errs"))
  }

  test("unused top-level let points to the whole binding") {
    val source =
      """package A
        |export main
        |
        |x = 1
        |main = 2
        |""".stripMargin

    val message = unusedLetMessage(source)
    assert(message.contains("unused let binding: x"), message)
    assert(message.contains("x = 1"), message)

    val pointerLines = message.linesIterator.filter(_.contains("^")).toList
    assertEquals(pointerLines.length, 1, message)

    val pointerWidth = pointerLines.head.count(_ == '^')
    assert(pointerWidth >= 3, message)
  }
}
