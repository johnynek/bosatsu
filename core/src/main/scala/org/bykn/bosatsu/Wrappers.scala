package org.bykn.bosatsu

import cats.Order
import cats.implicits._

case class ConstructorName(asString: String)

object ConstructorName {
  implicit val orderCN: Order[ConstructorName] = Order[String].contramap[ConstructorName](_.asString)
  implicit val orderingCN: Ordering[ConstructorName] = orderCN.toOrdering
}

case class ParamName(asString: String)
case class TypeName(asString: String)

object TypeName {

  implicit val typeNameOrder: Order[TypeName] =
    Order.by { tn: TypeName => tn.asString }

  implicit val typeNameOrdering: Ordering[TypeName] =
    Ordering.by { tn: TypeName => tn.asString }
}

case class Unique(id: Long) {
  def next: Unique =
    if (id == Long.MaxValue) sys.error("overflow")
    else Unique(id + 1L)
}

