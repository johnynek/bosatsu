package org.bykn.bosatsu

import cats.Order
import cats.implicits._

case class TypeName(ident: Identifier.Constructor)

object TypeName {
  implicit val typeNameOrder: Order[TypeName] =
    Order.by { tn: TypeName => tn.ident }

  implicit val typeNameOrdering: Ordering[TypeName] =
    Ordering.by { tn: TypeName => tn.ident }
}

case class Unique(id: Long) {
  def next: Unique =
    if (id == Long.MaxValue) sys.error("overflow")
    else Unique(id + 1L)
}

