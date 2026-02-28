package dev.bosatsu.smt

final case class LoweredBool(
    expr: SmtExpr[SmtSort.BoolSort],
    decls: Vector[(String, SmtSort)]
)
