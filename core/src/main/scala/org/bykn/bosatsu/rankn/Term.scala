package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.bykn.bosatsu.Pattern

sealed abstract class Term {
  import Term._

  final def isAtomic: Boolean =
    this match {
      case Var(_) | Lit(_) => true
      case _ => false
    }
}

object Term {
  sealed abstract class Literal
  object Literal {
    def apply(fromLong: Long): Literal = Integer(fromLong)
    def apply(fromBool: Boolean): Literal = Bool(fromBool)

    case class Integer(toLong: Long) extends Literal
    case class Bool(toBool: Boolean) extends Literal
  }

  case class Var(name: String) extends Term
  case class Lit(toLiteral: Literal) extends Term
  case class App(fn: Term, arg: Term) extends Term
  case class Lam(name: String, result: Term) extends Term
  case class ALam(name: String, tpe: Type, result: Term) extends Term
  case class Let(name: String, value: Term, in: Term) extends Term
  case class Ann(term: Term, tpe: Type) extends Term
  case class If(cond: Term, ifTrue: Term, ifFalse: Term) extends Term
  case class Match(term: Term, branches: NonEmptyList[(Pattern[String], Term)]) extends Term

  object Lit {
    def apply(l: Long): Lit = Lit(Literal(l))
    def apply(b: Boolean): Lit = Lit(Literal(b))
  }

}
