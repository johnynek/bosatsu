package dev.bosatsu.smt

sealed trait SmtCommand derives CanEqual
object SmtCommand {
  final case class SetLogic(logic: String) extends SmtCommand
  final case class DeclareConst[S <: SmtSort](name: String, sort: S)
      extends SmtCommand
  final case class DeclareFun[S <: SmtSort](
      name: String,
      args: Vector[SmtSort],
      result: S
  ) extends SmtCommand
  final case class DefineFun[S <: SmtSort](
      name: String,
      args: Vector[(String, SmtSort)],
      result: S,
      body: SmtExpr[S]
  ) extends SmtCommand
  final case class Assert(expr: SmtExpr[SmtSort.BoolSort]) extends SmtCommand
  case object CheckSat extends SmtCommand
  case object GetModel extends SmtCommand
}
