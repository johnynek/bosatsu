package dev.bosatsu.smt

object Z3Api {
  sealed trait Status derives CanEqual
  object Status {
    case object Sat extends Status
    case object Unsat extends Status
    case object Unknown extends Status
  }

  final case class SolverOutput(stdout: String, stderr: String)

  sealed trait RunError {
    def message: String
  }
  object RunError {
    final case class ExecutionFailure(
        message: String,
        stdout: String,
        stderr: String
    ) extends RunError
    final case class InvalidOutput(
        message: String,
        stdout: String,
        stderr: String
    ) extends RunError
    final case class ModelParseFailure(
        message: String,
        stdout: String,
        stderr: String
    ) extends RunError
  }

  final case class StructuredResult(
      status: Status,
      model: Option[Vector[SExpr]],
      smt2: String,
      stdout: String,
      stderr: String
  )

  type RunSmt2 = String => Either[RunError.ExecutionFailure, SolverOutput]

  def run(script: SmtScript, runSmt2: RunSmt2): Either[RunError, StructuredResult] =
    run(script, parseModel = true, runSmt2)

  def run(
      script: SmtScript,
      parseModel: Boolean,
      runSmt2: RunSmt2
  ): Either[RunError, StructuredResult] = {
    val smt2 = SmtLibRender.renderScript(script)
    runSmt2(smt2).flatMap(parse(_, smt2, parseModel && hasGetModel(script)))
  }

  def runSmt2(smt2: String, runSmt2Fn: RunSmt2): Either[RunError, StructuredResult] =
    runSmt2Fn(smt2).flatMap(parse(_, smt2, parseModel = false))

  private def parse(
      output: SolverOutput,
      smt2: String,
      parseModel: Boolean
  ): Either[RunError, StructuredResult] = {
    parseStatusAndRemainder(output.stdout).flatMap { case (status, rest) =>
      val modelEither: Either[RunError, Option[Vector[SExpr]]] =
        status match {
          case Status.Sat if parseModel =>
            val trimmed = rest.trim
            if (trimmed.isEmpty) Right(None)
            else {
              SExprParser.parseAll(trimmed) match {
                case Left(err) =>
                  Left(
                    RunError.ModelParseFailure(
                      err.message,
                      output.stdout,
                      output.stderr
                    )
                  )
                case Right(parsed) =>
                  Right(Some(parsed))
              }
            }
          case _ =>
            Right(None)
        }

      modelEither.map { model =>
        StructuredResult(status, model, smt2, output.stdout, output.stderr)
      }
    }
  }

  private def hasGetModel(script: SmtScript): Boolean =
    script.commands.contains(SmtCommand.GetModel)

  private def parseStatusAndRemainder(
      stdout: String
  ): Either[RunError.InvalidOutput, (Status, String)] = {
    val trimmed = stdout.dropWhile(_.isWhitespace)
    if (trimmed.isEmpty)
      Left(RunError.InvalidOutput("empty solver output", stdout, ""))
    else {
      val statusTokens =
        List("sat" -> Status.Sat, "unsat" -> Status.Unsat, "unknown" -> Status.Unknown)

      statusTokens.iterator
        .map { case (token, status) =>
          readToken(trimmed, token).map(rest => (status, rest))
        }
        .collectFirst { case Some(value) => value }
        .toRight {
          RunError.InvalidOutput(
            s"expected leading status token in solver output, found: ${trimmed.take(120)}",
            stdout,
            ""
          )
        }
    }
  }

  private def readToken(in: String, token: String): Option[String] =
    if (in.startsWith(token) && boundary(in, token.length)) Some(in.substring(token.length))
    else None

  private def boundary(in: String, idx: Int): Boolean =
    (idx >= in.length) || in.charAt(idx).isWhitespace
}
