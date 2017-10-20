package org.bykn.hindleymilner

object Syntax {
  sealed abstract class Expr
  object Expr {
    case class IfElse(cond: Expr, ifTrue: Expr, ifFalse: Expr)
  }

  sealed abstract class Statement
  object Statement {
    case class Comment(message: String) extends Statement
    case class LetEq(identifier: String, expr: Expr) extends Statement
  }
}
