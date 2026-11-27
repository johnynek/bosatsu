package dev.bosatsu.smt

import dev.bosatsu.scalawasiz3.{Z3Platform, Z3Result}

/** z3, through scalawasiz3, as the reference the in-house decision procedures
  * (IntDecide, LinearBounds, pathImplies) are checked against. The compiler
  * itself never runs z3.
  */
object Z3Oracle {
  import SmtExpr.*
  import SmtCommand.*

  private lazy val solver = Z3Platform.create()

  val runner: Z3Api.RunSmt2 = { smt2 =>
    solver.runSmt2(smt2) match {
      case Z3Result.Success(stdout, stderr, _) =>
        Right(Z3Api.SolverOutput(stdout, stderr))
      case Z3Result.Failure(msg, _, stdout, stderr, _) =>
        Left(Z3Api.RunError.ExecutionFailure(msg, stdout, stderr))
    }
  }

  // QF_LIA does not allow products of terms or division by a term
  def isNonlinear(e: SmtExpr[?]): Boolean = {
    def isConst(i: IntExpr) = LinearBounds.linear(i).exists(_.coeffs.isEmpty)
    e match {
      case Mul(args) =>
        args.count(a => !isConst(a)) > 1 || args.exists(isNonlinear)
      case Div(n, d)     => !isConst(d) || isNonlinear(n) || isNonlinear(d)
      case Mod(n, d)     => !isConst(d) || isNonlinear(n) || isNonlinear(d)
      case Add(args)     => args.exists(isNonlinear)
      case Sub(args)     => args.exists(isNonlinear)
      case Ite(p, a, b)  => isNonlinear(p) || isNonlinear(a) || isNonlinear(b)
      case Lt(a, b)      => isNonlinear(a) || isNonlinear(b)
      case Lte(a, b)     => isNonlinear(a) || isNonlinear(b)
      case Gt(a, b)      => isNonlinear(a) || isNonlinear(b)
      case Gte(a, b)     => isNonlinear(a) || isNonlinear(b)
      case EqInt(a, b)   => isNonlinear(a) || isNonlinear(b)
      case EqBool(a, b)  => isNonlinear(a) || isNonlinear(b)
      case Xor(a, b)     => isNonlinear(a) || isNonlinear(b)
      case Implies(a, b) => isNonlinear(a) || isNonlinear(b)
      case Not(a)        => isNonlinear(a)
      case And(args)     => args.exists(isNonlinear)
      case Or(args)      => args.exists(isNonlinear)
      case _             => false
    }
  }

  /** "facts and not goal", normalized as the recursion check once normalized
    * what it sent to z3, with QF_NIA when a term isn't linear.
    */
  def script(
      goal: BoolExpr,
      facts: Iterable[BoolExpr],
      ints: Iterable[String],
      bools: Iterable[String]
  ): SmtScript = {
    val pathCond = normalizeBoolForSolver(And(facts.toVector))
    val goal1 = normalizeBoolForSolver(goal)
    val logic =
      if (isNonlinear(pathCond) || isNonlinear(goal1)) SetLogic("QF_NIA")
      else SetLogic.QF_LIA
    SmtScript(
      Vector(logic) ++
        ints.iterator.map(DeclareConst(_, SmtSort.IntS)) ++
        bools.iterator.map(DeclareConst(_, SmtSort.BoolS)) ++
        Vector(Assert(pathCond), Assert(Not(goal1)), CheckSat)
    )
  }

  /** z3's answer for "facts imply goal": Right(Some(true)) if facts and not
    * goal is unsat, Right(Some(false)) if sat, Right(None) if z3 says unknown,
    * and Left if z3 failed (the embedded z3 traps on some valid queries, such
    * as division by a variable).
    */
  def implies(
      goal: BoolExpr,
      facts: Iterable[BoolExpr],
      ints: Iterable[String],
      bools: Iterable[String]
  ): Either[String, Option[Boolean]] =
    Z3Api.run(
      script(goal, facts, ints, bools),
      parseModel = false,
      runner
    ) match {
      case Right(res) =>
        Right(res.status match {
          case Z3Api.Status.Unsat   => Some(true)
          case Z3Api.Status.Sat     => Some(false)
          case Z3Api.Status.Unknown => None
        })
      case Left(err) => Left(err.message)
    }
}
