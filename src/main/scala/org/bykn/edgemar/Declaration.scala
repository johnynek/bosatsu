package org.bykn.edgemar

import cats.data.NonEmptyList

sealed abstract class TypeRef
object TypeRef {
  case class TypeVar(v: String) extends TypeRef {
    require(v.charAt(0).isLower)
  }
  case class TypeName(v: String) extends TypeRef {
    require(v.charAt(0).isUpper)
  }
  case class TypeArrow(from: TypeRef, to: TypeRef) extends TypeRef
  case class TypeApply(of: TypeRef, args: NonEmptyList[TypeRef]) extends TypeRef
}

/**
 * Represents the syntax of declarations
 */
sealed abstract class Declaration

object Declaration {
  case class Apply(on: Declaration, args: NonEmptyList[Declaration]) extends Declaration
  case class Binding(name: String, value: Declaration, emptyLines: Int, in: Declaration) extends Declaration
  case class Comment(message: String, on: Declaration) extends Declaration
  case class DefFn(name: String,
    args: List[(String, Option[TypeRef])],
    retType: Option[TypeRef], result: Declaration) extends Declaration
  case class FfiLambda(lang: String, callsite: String, tpe: TypeRef) extends Declaration
  case class Lambda(style: String, args: NonEmptyList[String], body: Declaration) extends Declaration
  case class LiteralBool(toBoolean: Boolean) extends Declaration
  case class LiteralInt(asString: String) extends Declaration
  case class Package(name: String,
    exports: List[String],
    emptyLines: Int,
    decl: Declaration) extends Declaration
  case class Parens(of: Declaration) extends Declaration
  case class Var(name: String) extends Declaration
  case object EndOfFile extends Declaration
}
