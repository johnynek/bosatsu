package org.bykn.bosatsu

import cats.Order
import fastparse.all._
import org.typelevel.paiges.{ Doc, Document }

import Parser.{lowerIdent, upperIdent}

import cats.implicits._

sealed abstract class Identifier {
  def asString: String

  def sourceCodeRepr: String =
    Identifier.document.document(this).renderWideStream.mkString

  override def equals(that: Any): Boolean =
    that match {
      case ident: Identifier =>
        asString == ident.asString
      case _ => false
    }

  override val hashCode: Int = asString.hashCode

  def toBindable: Option[Identifier.Bindable] =
    this match {
      case b: Identifier.Bindable => Some(b)
      case _ => None
    }

  def toConstructor: Option[Identifier.Constructor] =
    this match {
      case c: Identifier.Constructor => Some(c)
      case _ => None
    }
}

object Identifier {
  /**
   * These are names that can appear in bindings. Importantly,
   * we can't bind constructor names except to define types
   */
  sealed abstract class Bindable extends Identifier

  final case class Constructor(asString: String) extends Identifier
  final case class Name(asString: String) extends Bindable
  final case class Backticked(asString: String) extends Bindable
  final case class Operator(asString: String) extends Bindable

  private[this] val opPrefix = Doc.text("operator ")

  implicit def document[A <: Identifier]: Document[A] =
    Document.instance[A] {
      case Backticked(lit) =>
        Doc.char('`') + Doc.text(Parser.escape('`', lit)) + Doc.char('`')
      case Constructor(n) => Doc.text(n)
      case Name(n) => Doc.text(n)
      case Operator(n) => opPrefix + Doc.text(n)
    }

  val nameParser: P[Name] =
    lowerIdent.map { n => Name(n.intern) }

  val consParser: P[Constructor] =
    upperIdent.map { c => Constructor(c.intern) }

  /**
   * This is used to apply operators, it is the
   * raw operator tokens without an `operator` prefix
   */
  val rawOperator: P[Operator] =
    Operators.operatorToken.map { op => Operator(op.intern) }

  /**
   * the keyword operator preceding a rawOperator
   */
  val operator: P[Operator] =
    P("operator" ~ Parser.spaces ~ rawOperator)

  /**
   * Name, Backticked or non-raw operator
   */
  val bindableParser: P[Bindable] =
    // operator has to come first to not look like a Name
    operator | nameParser | Parser.escapedString('`').map { b => Backticked(b.intern) }

  val parser: P[Identifier] =
    bindableParser | consParser

  /**
   * Build an Identifier by parsing a string
   */
  def unsafe(str: String): Identifier =
    unsafeParse(parser, str)

  def unsafeParse[A <: Identifier](pa: P[A], str: String): A =
    pa.parse(str) match {
      case Parsed.Success(ident, idx) if idx == str.length =>
        ident
      case Parsed.Success(_, idx) =>
        sys.error(s"partial parse of $str ignores: ${str.substring(idx)}")
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx: (${str.substring(idx)}) with trace: ${extra.traced.trace}")
    }

  implicit def order[A <: Identifier]: Order[A] =
    Order.by[A, String](_.asString)

  implicit def ordering[A <: Identifier]: Ordering[A] =
    order[A].toOrdering
}
