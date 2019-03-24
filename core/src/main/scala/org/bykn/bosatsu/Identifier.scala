package org.bykn.bosatsu

import org.typelevel.paiges.{ Doc, Document }
import fastparse.all._

import Parser.{lowerIdent, upperIdent}

sealed abstract class Identifier {
  def asString: String
}

object Identifier {
  /**
   * These are names that can appear in bindings. Importantly,
   * we can't bind constructor names except to define types
   */
  sealed abstract class Bindable extends Identifier

  final case class Constructor(asString: String) extends Identifier
  final case class Name(asString: String) extends Bindable

  implicit val document: Document[Identifier] =
    Document.instance[Identifier] { ident => Doc.text(ident.asString) }

  val bindableParser: P[Bindable] =
    lowerIdent.map(Name(_))

  val consParser: P[Constructor] =
    upperIdent.map(Constructor(_))

  val parser: P[Identifier] =
    bindableParser | consParser

  /**
   * Build an Identifier by parsing a string
   */
  def unsafe(str: String): Identifier =
    parser.parse(str) match {
      case Parsed.Success(ident, idx) if idx == str.length =>
        ident
      case Parsed.Success(_, idx) =>
        sys.error(s"partial parse of $str ignores: ${str.substring(idx)}")
      case Parsed.Failure(exp, idx, extra) =>
        sys.error(s"failed to parse: $str: $exp at $idx: (${str.substring(idx)}) with trace: ${extra.traced.trace}")
    }
}
