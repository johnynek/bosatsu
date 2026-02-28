package dev.bosatsu.smt

import org.typelevel.paiges.Doc

sealed trait SExpr derives CanEqual
object SExpr {
  final case class Atom(value: String) extends SExpr
  final case class List(values: Vector[SExpr]) extends SExpr

  private def bareAtomChar(ch: Char): Boolean =
    (!ch.isWhitespace) && (ch != '(') && (ch != ')') && (ch != ';')

  // "Valid atom" for direct unquoted emission in SMT-LIB.
  def isValidBareAtom(value: String): Boolean =
    value.nonEmpty && value.forall(bareAtomChar) &&
      (value.head != '"') && (value.head != '|')

  def toDoc(expr: SExpr): Doc =
    expr match {
      case Atom(value) => Doc.text(value)
      case List(items) =>
        Doc.char('(') +
          Doc.intercalate(Doc.char(' '), items.map(toDoc)) +
          Doc.char(')')
    }

  def toDocAll(values: Vector[SExpr]): Doc =
    Doc.intercalate(Doc.hardLine, values.map(toDoc))

  def render(expr: SExpr, width: Int = 120): String =
    toDoc(expr).render(width)

  def renderAll(values: Vector[SExpr], width: Int = 120): String =
    toDocAll(values).render(width)
}
