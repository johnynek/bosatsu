package dev.bosatsu.smt

import dev.bosatsu.scalawasiz3.{Z3Result, Z3Solver}
import scala.concurrent.duration.DurationInt

class Z3ApiTest extends munit.FunSuite {
  import SmtCommand._
  import SmtExpr._

  override val munitTimeout = 2.minutes

  private val liveRunner: Z3Api.RunSmt2 = { smt2 =>
    Z3Solver.default.runSmt2(smt2) match {
      case Z3Result.Success(stdout, stderr, _) =>
        Right(Z3Api.SolverOutput(stdout, stderr))
      case Z3Result.Failure(msg, _, stdout, stderr, _) =>
        Left(Z3Api.RunError.ExecutionFailure(msg, stdout, stderr))
    }
  }

  private def runOrFail(script: SmtScript): Z3Api.StructuredResult =
    Z3Api.run(script, liveRunner) match {
      case Right(res) => res
      case Left(err)  => fail(s"unexpected Z3Api error: ${err.message}")
    }

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

  test("unsat output parses to structured result with live solver") {
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        Assert(
          And(
            Vector(
              BoolConst(true),
              Not(BoolConst(true))
            )
          )
        ),
        CheckSat
      )
    )

    val res = runOrFail(script)
    assertEquals(res.status, Z3Api.Status.Unsat)
    assertEquals(res.model, None)
  }

  test("sat output with get-model parses model with live solver") {
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        DeclareConst("x", SmtSort.IntS),
        Assert(Gt(Var[SmtSort.IntSort]("x"), IntConst(0))),
        CheckSat,
        GetModel
      )
    )

    val res = runOrFail(script)
    assertEquals(res.status, Z3Api.Status.Sat)
    assert(res.model.nonEmpty)
    assert(res.model.exists(_.nonEmpty))
  }

  test("runSmt2 convenience path parses status with live solver") {
    val smt2 =
      """(set-logic QF_LIA)
        |(declare-const x Int)
        |(assert (> x 1))
        |(check-sat)
        |(get-model)
        |""".stripMargin

    Z3Api.runSmt2(smt2, liveRunner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"unexpected error: ${err.message}")
    }
  }

  test("malformed SMT program returns an error") {
    val smt2 =
      """(set-logic QF_LIA)
        |(assert true)
        |(check-sat
        |""".stripMargin

    val out = Z3Api.runSmt2(smt2, liveRunner)
    assert(out.isLeft)
  }

  test("parseModel=false skips model extraction with live solver") {
    val script = SmtScript(
      Vector(
        SetLogic("QF_LIA"),
        DeclareConst("x", SmtSort.IntS),
        Assert(Gte(Var[SmtSort.IntSort]("x"), IntConst(10))),
        CheckSat,
        GetModel
      )
    )

    Z3Api.run(script, parseModel = false, liveRunner) match {
      case Right(res) =>
        assertEquals(res.status, Z3Api.Status.Sat)
        assertEquals(res.model, None)
      case Left(err)  =>
        fail(s"unexpected error: ${err.message}")
    }
  }
}
