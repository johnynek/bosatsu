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
}
