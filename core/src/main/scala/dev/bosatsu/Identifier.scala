package dev.bosatsu

import cats.Order
import cats.parse.{Parser0 => P0, Parser => P}
import dev.bosatsu.hashing.Hashable
import org.typelevel.paiges.{Doc, Document}
import scala.quoted.{Expr, Quotes}

import Parser.{lowerIdent, upperIdent}

import cats.implicits._

sealed abstract class Identifier derives CanEqual {
  def asString: String

  def sourceCodeRepr: String =
    Identifier.document.document(this).renderWideStream.mkString

  final override def equals(that: Any): Boolean =
    that match {
      case ident: Identifier =>
        (this eq ident) || Identifier.sameIdentity(this, ident)
      case _ => false
    }

  final override val hashCode: Int = Identifier.identityHash(this)

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
  final case class Synthetic(asString: String) extends Bindable

  // Keep legacy by-text identity for user-authored names, but make synthetics
  // disjoint so compiler-generated binders cannot collide with user identifiers.
  private[bosatsu] def sameIdentity(
      left: Identifier,
      right: Identifier
  ): Boolean =
    (left, right) match {
      case (Synthetic(sl), Synthetic(sr)) => sl == sr
      case (Synthetic(_), _) | (_, Synthetic(_)) =>
        false
      case _ =>
        left.asString == right.asString
    }

  // hashCode must follow the same split identity policy as equals.
  private[bosatsu] def identityHash(ident: Identifier): Int =
    ident match {
      case Synthetic(s) =>
        scala.util.hashing.MurmurHash3.stringHash(
          s,
          0x3294d30f
        )
      case _ =>
        ident.asString.hashCode
    }

  private def hashableKey(ident: Identifier): String =
    ident match {
      case Synthetic(s) =>
        s
      case _ =>
        ident.sourceCodeRepr
    }

  private def identityOrderKey(ident: Identifier): (Int, String) =
    ident match {
      case Synthetic(s) =>
        (0, s)
      case _ =>
        (1, ident.asString)
    }

  given Hashable[Identifier] = Hashable.by(hashableKey)

  object Constructor {
    given Hashable[Constructor] =
      Hashable[Identifier].narrow[Constructor]
  }

  private val opPrefix = Doc.text("operator ")

  object Bindable {
    implicit def bindableOrder: Order[Bindable] =
      Identifier.order

    given Hashable[Bindable] =
      Hashable[Identifier].narrow[Bindable]

    val allBinderNames: LazyList[String] = {
      val letters = ('a' to 'z').to(LazyList).map(_.toString)
      val allIntegers = LazyList.iterate(0L)(_ + 1L)
      val lettersWithNumber =
        for {
          num <- allIntegers
          l <- letters
        } yield s"$l$num"

      letters #::: lettersWithNumber
    }

    def syntheticIterator: Iterator[Bindable] =
      allBinderNames.iterator.map(Identifier.synthetic)

    def freshSyntheticIterator(
        avoid: Bindable => Boolean
    ): Iterator[Bindable] =
      syntheticIterator.filterNot(avoid)

    def freshSyntheticIterator(
        avoid: collection.Set[Bindable]
    ): Iterator[Bindable] =
      freshSyntheticIterator(avoid.apply)

    def prefixedSyntheticIterator(prefix: String): Iterator[Bindable] =
      Iterator
        .from(0)
        .map(i => Identifier.synthetic(s"${prefix}_$i"))

    def freshPrefixedSyntheticIterator(
        prefix: String,
        avoid: Bindable => Boolean
    ): Iterator[Bindable] =
      prefixedSyntheticIterator(prefix).filterNot(avoid)

    def freshPrefixedSyntheticIterator(
        prefix: String,
        avoid: collection.Set[Bindable]
    ): Iterator[Bindable] =
      freshPrefixedSyntheticIterator(prefix, avoid.apply)
  }

  implicit def document[A <: Identifier]: Document[A] =
    Document.instance[A] {
      case Backticked(lit) =>
        Doc.char('`') + Doc.text(Parser.escape('`', lit)) + Doc.char('`')
      case Constructor(n) => Doc.text(n)
      case Name(n)        => Doc.text(n)
      case Synthetic(n)   => Doc.text(n)
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

  private def maybeInternSynthetic(str: String): String =
    if (str.length < 3) str.intern else str

  val bindableWithSynthetic: P[Bindable] =
    bindableParser.orElse(
      (P.char('_') ~ P.anyChar.rep).string.map { s =>
        Synthetic(maybeInternSynthetic(s))
      }
    )

  val parserWithSynthetic: P[Identifier] =
    bindableWithSynthetic.orElse(consParser)

  // When we are allocating new names, we want
  // them to be similar
  def appendToName(i: Bindable, suffix: String): Bindable =
    i match {
      case Backticked(b) => Backticked(b + suffix)
      case Synthetic(s)  => Synthetic(s + suffix)
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

  /** Build an Identifier from a compile-time string literal.
    *
    * The literal must parse as either a [[Name]] or [[Constructor]], and the
    * parsed `asString` must exactly match the provided literal content.
    */
  transparent inline def literal(inline str: String): Identifier =
    ${ literalImpl('str) }

  private def literalImpl(
      strExpr: Expr[String]
  )(using q: Quotes): Expr[Identifier] = {
    import q.reflect.report

    strExpr.value match {
      case Some(str) =>
        parser.parseAll(str) match {
          case Right(Name(name)) if name == str =>
            '{ Name(${ Expr(str) }) }
          case Right(Constructor(cons)) if cons == str =>
            '{ Constructor(${ Expr(str) }) }
          case Right(Name(name)) =>
            report.errorAndAbort(
              s"""Identifier.literal("$str") parsed as Name("$name"), which does not round-trip exactly."""
            )
          case Right(Constructor(cons)) =>
            report.errorAndAbort(
              s"""Identifier.literal("$str") parsed as Constructor("$cons"), which does not round-trip exactly."""
            )
          case Right(other) =>
            report.errorAndAbort(
              s"""Identifier.literal("$str") must parse as Name or Constructor, but parsed as ${other.sourceCodeRepr}."""
            )
          case Left(err) =>
            report.errorAndAbort(
              s"""Identifier.literal("$str") is invalid: $err"""
            )
        }
      case None =>
        report.errorAndAbort(
          s"Identifier.literal requires a string literal, but found: ${strExpr.show}"
        )
    }
  }

  def optionParse[A](pa: P0[A], str: String): Option[A] =
    Parser.optionParse(pa, str)

  def unsafeParse[A](pa: P0[A], str: String): A =
    Parser.unsafeParse(pa, str)

  def isSynthetic(b: Bindable): Boolean =
    b match {
      case Synthetic(_) => true
      case _            => false
    }

  implicit def order[A <: Identifier]: Order[A] =
    Order.by[A, (Int, String)](a => identityOrderKey(a))

  implicit def ordering[A <: Identifier]: Ordering[A] =
    order[A].toOrdering

  implicit val showIdentifier: cats.Show[Identifier] =
    cats.Show.show(_.sourceCodeRepr)

  def synthetic(name: String): Bindable = {
    Require(name.nonEmpty)
    Synthetic(maybeInternSynthetic("_" + name))
  }
}
