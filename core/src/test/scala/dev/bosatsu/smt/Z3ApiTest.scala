package dev.bosatsu.smt

class Z3ApiTest extends munit.FunSuite {
  import SmtCommand._
  import SmtExpr._

  private val successRunner: Z3Api.RunSmt2 =
    _ => Right(Z3Api.SolverOutput(stdout = "unsat\n", stderr = ""))

  test("render script with define-fun and ite") {
    val x = Var[SmtSort.IntSort]("x")
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        DefineFun(
          "clamp_nonneg",
          Vector("x" -> SmtSort.IntS),
          SmtSort.IntS,
          Ite(
            Gte(x, IntConst(0)),
            x,
            IntConst(0)
          )
        ),
        Assert(
          EqInt(
            App[SmtSort.IntSort]("clamp_nonneg", Vector(IntConst(-7))),
            IntConst(0)
          )
        ),
        CheckSat
      )
    )

    val out = SmtLibRender.renderScript(script)
    assert(out.contains("(define-fun clamp_nonneg"))
    assert(out.contains("(ite (>= x 0) x 0)"))
    assert(out.contains("(assert (= (clamp_nonneg (- 7)) 0))"))
  }

  test("unsat output parses to structured result") {
    val script = SmtScript(Vector(CheckSat))
    Z3Api.run(script, successRunner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Unsat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }

  test("sat output with get-model parses model s-expressions") {
    val solverOut =
      """sat
        |(model
        |  (define-fun x () Int
        |    3)
        |)
        |""".stripMargin

    val runner: Z3Api.RunSmt2 =
      _ => Right(Z3Api.SolverOutput(stdout = solverOut, stderr = ""))

    val script = SmtScript(Vector(CheckSat, GetModel))
    Z3Api.run(script, runner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assert(res.model.nonEmpty)
        assert(res.model.exists(_.nonEmpty))
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }

  test("invalid status output is reported") {
    val runner: Z3Api.RunSmt2 =
      _ => Right(Z3Api.SolverOutput(stdout = "not-a-status\n", stderr = ""))
    val script = SmtScript(Vector(CheckSat))
    val out = Z3Api.run(script, runner)
    assert(out.isLeft)
  }

  test("execution failure is propagated") {
    val expected = Z3Api.RunError.ExecutionFailure(
      message = "z3 crashed",
      stdout = "partial",
      stderr = "boom"
    )
    val runner: Z3Api.RunSmt2 = _ => Left(expected)
    val script = SmtScript(Vector(CheckSat))
    val out = Z3Api.run(script, runner)
    assertEquals(out, Left(expected))
  }

  test("unknown status parses to structured result") {
    val runner: Z3Api.RunSmt2 =
      _ => Right(Z3Api.SolverOutput(stdout = "unknown\n", stderr = ""))
    val script = SmtScript(Vector(CheckSat))
    Z3Api.run(script, runner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Unknown)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }

  test("status token must be delimited") {
    val runner: Z3Api.RunSmt2 =
      _ => Right(Z3Api.SolverOutput(stdout = "satish\n", stderr = ""))
    val script = SmtScript(Vector(CheckSat))
    val out = Z3Api.run(script, runner)
    out match {
      case Left(_: Z3Api.RunError.InvalidOutput) =>
        ()
      case _ =>
        fail(s"expected InvalidOutput, got: $out")
    }
  }

  test("sat output with malformed model returns parse failure") {
    val runner: Z3Api.RunSmt2 =
      _ => Right(
        Z3Api.SolverOutput(
          stdout =
            """sat
              |(model
              |  (define-fun x () Int 3)
              |""".stripMargin,
          stderr = ""
        )
      )

    val script = SmtScript(Vector(CheckSat, GetModel))
    val out = Z3Api.run(script, runner)
    out match {
      case Left(_: Z3Api.RunError.ModelParseFailure) =>
        ()
      case _ =>
        fail(s"expected ModelParseFailure, got: $out")
    }
  }

  test("sat output without get-model does not parse remainder as model") {
    val runner: Z3Api.RunSmt2 =
      _ =>
        Right(
          Z3Api.SolverOutput(
            stdout =
              """sat
                |(model
                |  (define-fun x () Int 7)
                |)
                |""".stripMargin,
            stderr = ""
          )
        )

    val script = SmtScript(Vector(CheckSat))
    Z3Api.run(script, runner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }

  test("runSmt2 parses status but never parses model") {
    val runner: Z3Api.RunSmt2 =
      _ =>
        Right(
          Z3Api.SolverOutput(
            stdout =
              """sat
                |(model
                |  (define-fun x () Int 11)
                |)
                |""".stripMargin,
            stderr = ""
          )
        )

    Z3Api.runSmt2("(check-sat)\n(get-model)\n", runner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }

  test("run with parseModel=false skips malformed model parsing") {
    val runner: Z3Api.RunSmt2 =
      _ => Right(Z3Api.SolverOutput(stdout = "sat\n(model\n", stderr = ""))
    val script = SmtScript(Vector(CheckSat, GetModel))
    Z3Api.run(script, parseModel = false, runner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"expected success, got error: ${err.message}")
    }
  }
}
