package dev.bosatsu

import cats.Order
import cats.implicits._
import dev.bosatsu.hashing.Hashable

case class TypeName(ident: Identifier.Constructor) {
  def asString: String = ident.asString
}

object TypeName {
  implicit val typeNameOrder: Order[TypeName] =
    Order.by((tn: TypeName) => tn.ident)

  implicit val typeNameOrdering: Ordering[TypeName] =
    Ordering.by((tn: TypeName) => tn.ident)

  def apply(str: String): TypeName =
    Identifier.consParser.parseAll(str) match {
      case Right(c) => TypeName(c)
      case Left(e)  => sys.error(s"invalid TypeName: $e")
    }

  given Hashable[TypeName] =
    Hashable.by(_.ident)
}
