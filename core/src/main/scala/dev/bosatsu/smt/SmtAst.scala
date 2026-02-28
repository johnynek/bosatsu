package dev.bosatsu.smt

import dev.bosatsu.rankn

sealed trait SmtSort derives CanEqual
object SmtSort {
  case object IntS extends SmtSort
  case object BoolS extends SmtSort

  type IntSort = IntS.type
  type BoolSort = BoolS.type
}

sealed trait SmtExpr[S <: SmtSort] derives CanEqual
object SmtExpr {
  type IntExpr = SmtExpr[SmtSort.IntSort]
  type BoolExpr = SmtExpr[SmtSort.BoolSort]

  final case class IntConst(value: BigInt) extends SmtExpr[SmtSort.IntSort]
  final case class BoolConst(value: Boolean) extends SmtExpr[SmtSort.BoolSort]

  final case class Var[S <: SmtSort](name: String) extends SmtExpr[S]
  final case class App[S <: SmtSort](name: String, args: Vector[SmtExpr[?]])
      extends SmtExpr[S]

  final case class Ite[S <: SmtSort](
      cond: BoolExpr,
      ifTrue: SmtExpr[S],
      ifFalse: SmtExpr[S]
  ) extends SmtExpr[S]

  // Int built-ins
  final case class Add(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Sub(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Mul(args: Vector[IntExpr]) extends SmtExpr[SmtSort.IntSort]
  final case class Div(num: IntExpr, den: IntExpr) extends SmtExpr[SmtSort.IntSort]
  final case class Mod(num: IntExpr, den: IntExpr) extends SmtExpr[SmtSort.IntSort]

  // Int comparisons
  final case class Lt(left: IntExpr, right: IntExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class Lte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Gt(left: IntExpr, right: IntExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class Gte(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqInt(left: IntExpr, right: IntExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class EqBool(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]

  // Bool built-ins
  final case class Not(expr: BoolExpr) extends SmtExpr[SmtSort.BoolSort]
  final case class And(args: Vector[BoolExpr]) extends SmtExpr[SmtSort.BoolSort]
  final case class Or(args: Vector[BoolExpr]) extends SmtExpr[SmtSort.BoolSort]
  final case class Xor(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]
  final case class Implies(left: BoolExpr, right: BoolExpr)
      extends SmtExpr[SmtSort.BoolSort]
}

final case class LoweredBool(
    expr: SmtExpr[SmtSort.BoolSort],
    decls: Vector[(String, SmtSort)]
)

sealed trait LowerErr derives CanEqual
object LowerErr {
  final case class Unsupported(node: String) extends LowerErr
  final case class TypeMismatch(found: rankn.Type) extends LowerErr
}

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

final case class SmtScript(commands: Vector[SmtCommand]) {
  def ++(other: SmtScript): SmtScript =
    SmtScript(commands ++ other.commands)

  def append(cmd: SmtCommand): SmtScript =
    SmtScript(commands :+ cmd)
}
object SmtScript {
  val empty: SmtScript = SmtScript(Vector.empty)
}
