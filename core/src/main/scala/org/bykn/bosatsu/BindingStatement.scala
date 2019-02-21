package org.bykn.bosatsu

import org.typelevel.paiges.{ Doc, Document }
import fastparse.all._
import Parser.{ Indy, maybeSpace }

import cats.implicits._
import org.bykn.fastparse_cats.StringInstances._

case class BindingStatement[B, T](name: B, value: Declaration, in: T)

object BindingStatement {
  private[this] val eqDoc = Doc.text(" = ")

  implicit def document[A: Document, T: Document]: Document[BindingStatement[A, T]] =
    Document.instance[BindingStatement[A, T]] { let =>
      import let._
      Document[A].document(name) + eqDoc + value.toDoc + Document[T].document(in)
    }

  // This is something we check after variables
  def bindingParser[B, T](parser: Indy[Declaration], rest: Indy[T]): Indy[B => BindingStatement[B, T]] = {
    val eqP = P("=" ~ !"=")

    (Indy.lift(P(maybeSpace ~ eqP ~ maybeSpace)) *> parser)
      .product(rest)
      .map { case (value, rest) =>

        { (bname: B) => BindingStatement(bname, value, rest) }
      }
  }
}

