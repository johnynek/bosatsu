package dev.bosatsu

import dev.bosatsu.scalawasiz3.{Z3Result, Z3Solver}

class Z3SolverIntegrationTest extends munit.FunSuite {

  private def parseFirstStatus(stdout: String): Option[String] =
    stdout.linesIterator
      .map(_.trim)
      .find {
        case "sat" | "unsat" | "unknown" => true
        case _                           => false
      }

  private def runAndGetStatus(smt2: String): (String, String, String) = {
    val solver = Z3Solver.default
    solver.runSmt2(smt2) match {
      case Z3Result.Success(stdout, stderr, _) =>
        val status = parseFirstStatus(stdout).getOrElse(
          fail(
            s"missing sat status in solver output\nstdout:\n$stdout\nstderr:\n$stderr"
          )
        )
        (status, stdout, stderr)

      case Z3Result.Failure(msg, _, stdout, stderr, _) =>
        fail(s"solver failure: $msg\nstdout:\n$stdout\nstderr:\n$stderr")
    }
  }

  private def assertStatus(
      smt2: String,
      expectedStatus: String,
      expectModel: Boolean = false,
      expectedSymbolsInModel: List[String] = Nil
  ): Unit = {
    val (status, stdout, stderr) = runAndGetStatus(smt2)
    assertEquals(status, expectedStatus, s"stdout:\n$stdout\nstderr:\n$stderr")
    if (expectModel) {
      assert(
        stdout.contains("define-fun"),
        s"expected model-like define-fun output in stdout:\n$stdout"
      )
      expectedSymbolsInModel.foreach { symbol =>
        assert(
          stdout.contains(symbol),
          s"expected model to mention $symbol\nstdout:\n$stdout"
        )
      }
    }
  }

  test("design doc query A proves next_i >= 0 (unsat)") {
    assertStatus(
      """
        |(set-logic QF_LIA)
        |(declare-fun intValue () Int)
        |(declare-fun next_i () Int)
        |
        |; path condition at recursive call
        |(assert (> intValue 0))
        |(assert (> next_i 0))
        |(assert (< next_i intValue))
        |
        |; negate goal
        |(assert (not (>= next_i 0)))
        |
        |(check-sat)
        |""".stripMargin,
      "unsat"
    )
  }

  test("design doc query B proves next_i < intValue (unsat)") {
    assertStatus(
      """
        |(set-logic QF_LIA)
        |(declare-fun intValue () Int)
        |(declare-fun next_i () Int)
        |
        |(assert (> intValue 0))
        |(assert (> next_i 0))
        |(assert (< next_i intValue))
        |
        |(assert (not (< next_i intValue)))
        |
        |(check-sat)
        |""".stripMargin,
      "unsat"
    )
  }

  test("design doc failing strict-decrease example is sat with a model") {
    assertStatus(
      """
        |(set-logic QF_LIA)
        |(declare-fun intValue () Int)
        |(declare-fun next_i () Int)
        |
        |(assert (> intValue 0))
        |(assert (> next_i 0))
        |
        |; negate goal
        |(assert (not (< next_i intValue))) ; equivalent to next_i >= intValue
        |
        |(check-sat)
        |(get-model)
        |""".stripMargin,
      "sat",
      expectModel = true,
      expectedSymbolsInModel = List("intValue", "next_i")
    )
  }

  test("design doc divide-and-conquer split check is unsat") {
    assertStatus(
      """
        |(set-logic ALL)
        |(declare-const n1 Int)
        |(declare-const n2 Int)
        |(declare-const n3 Int)
        |
        |(assert (>= n1 2))
        |(assert (= n2 (div n1 2)))
        |(assert (= n3 (- n1 n2)))
        |
        |(assert (not (and (<= 1 n2) (< n2 n1)
        |                  (<= 1 n3) (< n3 n1))))
        |
        |(check-sat)
        |""".stripMargin,
      "unsat"
    )
  }

  test("design doc obligation query shape works with concrete PC and GOAL") {
    assertStatus(
      """
        |(set-logic QF_LIA)
        |(declare-fun i () Int)
        |(declare-fun next_i () Int)
        |(assert (> i 0))
        |(assert (= next_i i))
        |(assert (not (< next_i i)))
        |(check-sat)
        |(get-model)
        |""".stripMargin,
      "sat",
      expectModel = true,
      expectedSymbolsInModel = List("i", "next_i")
    )
  }
}
