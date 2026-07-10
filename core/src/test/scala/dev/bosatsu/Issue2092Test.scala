package dev.bosatsu

import dev.bosatsu.LocationMap.Colorize

class Issue2092Test extends munit.FunSuite {
  private val color = Colorize.None

  test("issue 2092: outputForTimed includes inline package and total timings") {
    val report = Test.outputForTimed(
      List(
        PackageName.parts("Alpha", "Main") -> Some(
          (Test.Assertion(true, "alpha pass"), 123000000L)
        ),
        PackageName.parts("Beta", "Main") -> Some(
          (Test.Assertion(true, "beta pass"), 4000000L)
        )
      ),
      color,
      totalElapsedNanos = 127000000L
    )

    val out = report.doc.render(120)
    assert(out.contains("Alpha/Main: 0.123s"), out)
    assert(out.contains("Beta/Main: 0.004s"), out)
    assert(out.contains("in 0.127s"), out)
  }

  test(
    "issue 2092: quiet mode keeps failures and timings while hiding passing details"
  ) {
    val report = Test.outputForTimed(
      List(
        PackageName.parts("Quiet", "Main") -> Some(
          (
            Test.Suite(
              "quiet",
              List(
                Test.Assertion(true, "pass detail"),
                Test.Assertion(false, "boom")
              )
            ),
            5000000L
          )
        )
      ),
      color,
      totalElapsedNanos = 5000000L,
      quiet = true
    )

    val out = report.doc.render(120)
    assert(out.contains("boom"), out)
    assert(!out.contains("pass detail"), out)
    assert(out.contains("Quiet/Main: 0.005s"), out)
    assert(out.contains("in 0.005s"), out)
  }

  test(
    "issue 2092: packages with missing tests are excluded from timing rows"
  ) {
    val report = Test.outputForTimed(
      List(
        PackageName.parts("Missing", "Main") -> None,
        PackageName.parts("Ran", "Main") -> Some(
          (Test.Assertion(true, "ran"), 1000000L)
        )
      ),
      color,
      totalElapsedNanos = 1000000L
    )

    val out = report.doc.render(120)
    assert(out.contains("packages with missing tests: Missing/Main"), out)
    assert(out.contains("Ran/Main: 0.001s"), out)
    assert(!out.contains("Missing/Main: 0."), out)
  }
}
