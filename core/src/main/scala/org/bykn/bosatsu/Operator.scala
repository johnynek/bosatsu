package org.bykn.bosatsu

import org.bykn.bosatsu.rankn.Type

sealed abstract class Operator(val asString: String, val tpe: Type)
object Operator {
  private val intintint =
    Type.Fun(Type.IntType, Type.Fun(Type.IntType, Type.IntType))

  case object Plus extends Operator("+", intintint)
  case object Sub extends Operator("-", intintint)
  case object Mul extends Operator("*", intintint)
  case object Eql extends Operator("==",
    Type.Fun(Type.IntType, Type.Fun(Type.IntType, Type.BoolType)))

  def typeOf(o: Operator): Type = o.tpe

  lazy val allOps: List[Operator] =
    List(Plus, Sub, Mul, Eql)
}

