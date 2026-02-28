package dev.bosatsu.smt

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
