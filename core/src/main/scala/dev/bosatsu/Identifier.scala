package dev.bosatsu

import cats.Order
import cats.parse.{Parser0 => P0, Parser => P}
import org.typelevel.paiges.{Doc, Document}

import Parser.{lowerIdent, upperIdent}

import cats.implicits._

sealed abstract class Identifier derives CanEqual {
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
      case _                      => None
    }

  def toConstructor: Option[Identifier.Constructor] =
    this match {
      case c: Identifier.Constructor => Some(c)
      case _                         => None
    }
}

object Identifier {

  /** These are names that can appear in bindings. Importantly, we can't bind
    * constructor names except to define types
    */
  sealed abstract class Bindable extends Identifier

  final case class Constructor(asString: String) extends Identifier
  final case class Name(asString: String) extends Bindable
  final case class Backticked(asString: String) extends Bindable
  final case class Operator(asString: String) extends Bindable

  private val opPrefix = Doc.text("operator ")

  object Bindable {
    implicit def bindableOrder: Order[Bindable] =
      Identifier.order
  }

  implicit def document[A <: Identifier]: Document[A] =
    Document.instance[A] {
      case Backticked(lit) =>
        Doc.char('`') + Doc.text(Parser.escape('`', lit)) + Doc.char('`')
      case Constructor(n) => Doc.text(n)
      case Name(n)        => Doc.text(n)
      case Operator(n)    => opPrefix + Doc.text(n)
    }

  val nameParser: P[Name] =
    lowerIdent.map(n => Name(n.intern))

  val consParser: P[Constructor] =
    upperIdent.map(c => Constructor(c.intern))

  /** This is used to apply operators, it is the raw operator tokens without an
    * `operator` prefix
    */
  val rawOperator: P[Operator] =
    Operators.operatorToken.map(op => Operator(op.intern))

  /** the keyword operator preceding a rawOperator
    */
  val operator: P[Operator] =
    (P.string("operator").soft *> Parser.spaces) *> rawOperator

  /** Name, Backticked or non-raw operator
    */
  val bindableParser: P[Bindable] =
    // operator has to come first to not look like a Name
    P.oneOf(operator :: nameParser :: Parser.escapedString('`').map { b =>
      Backticked(b.intern)
    } :: Nil)

  val parser: P[Identifier] =
    bindableParser.orElse(consParser)

  val bindableWithSynthetic: P[Bindable] =
    bindableParser.orElse((P.char('_') ~ P.anyChar.rep).string.map(Name(_)))

  val parserWithSynthetic: P[Identifier] =
    bindableWithSynthetic.orElse(consParser)

  // When we are allocating new names, we want
  // them to be similar
  def appendToName(i: Bindable, suffix: String): Bindable =
    i match {
      case Backticked(b) => Backticked(b + suffix)
      case _             =>
        // try to stay the same
        val p = operator.orElse(nameParser)
        val cand = i.sourceCodeRepr + suffix
        p.parseAll(cand) match {
          case Right(ident) => ident
          case _            =>
            // just turn it into a Backticked
            Backticked(i.asString + suffix)
        }
    }

  /** Build an Identifier by parsing a string
    */
  def unsafe(str: String): Identifier =
    unsafeParse(parser, str)

  def unsafeBindable(str: String): Bindable =
    unsafeParse(bindableParser, str)

  def optionParse[A](pa: P0[A], str: String): Option[A] =
    Parser.optionParse(pa, str)

  def unsafeParse[A](pa: P0[A], str: String): A =
    Parser.unsafeParse(pa, str)

  implicit def order[A <: Identifier]: Order[A] =
    Order.by[A, String](_.asString)

  implicit def ordering[A <: Identifier]: Ordering[A] =
    order[A].toOrdering

  implicit val showIdentifier: cats.Show[Identifier] =
    cats.Show.show(_.sourceCodeRepr)

  def synthetic(name: String): Bindable = {
    Require(name.nonEmpty)
    Name("_" + name)
  }
}
