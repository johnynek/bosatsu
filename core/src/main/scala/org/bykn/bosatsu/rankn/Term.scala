package org.bykn.bosatsu.rankn

sealed abstract class Term {
  import Term._

  final def isAtomic: Boolean =
    this match {
      case Var(_) | Lit(_) => true
      case _ => false
    }
}

object Term {
  case class Var(name: String) extends Term
  case class Lit(toLong: Long) extends Term
  case class App(fn: Term, arg: Term) extends Term
  case class Lam(name: String, result: Term) extends Term
  case class ALam(name: String, tpe: Type, result: Term) extends Term
  case class Let(name: String, value: Term, in: Term) extends Term
  case class Ann(term: Term, tpe: Type) extends Term
}
