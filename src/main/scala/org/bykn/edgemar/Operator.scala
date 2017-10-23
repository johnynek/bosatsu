package org.bykn.edgemar

sealed abstract class Operator(val asString: String, val tpe: Type)
object Operator {
  private val intintint =
    Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.intT))

  case object Plus extends Operator("+", intintint)
  case object Sub extends Operator("-", intintint)
  case object Mul extends Operator("*", intintint)
  case object Eql extends Operator("==",
    Type.Arrow(Type.intT, Type.Arrow(Type.intT, Type.boolT)))

  def typeOf(o: Operator): Type = o.tpe

  lazy val allOps: List[Operator] =
    List(Plus, Sub, Mul, Eql)
}

