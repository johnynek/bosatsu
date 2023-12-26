package org.bykn.bosatsu

import cats.Order
import cats.implicits.*

case class TypeName(ident: Identifier.Constructor)

object TypeName {
  implicit val typeNameOrder: Order[TypeName] =
    Order.by { (tn: TypeName) => tn.ident }

  implicit val typeNameOrdering: Ordering[TypeName] =
    Ordering.by { (tn: TypeName) => tn.ident }
}
