package org.bykn.edgemar

import cats.data.NonEmptyList

sealed abstract class TypeError
object TypeError {
  case class UnificationFail(left: Type, right: Type) extends TypeError
  // This is a logic error that should really never happen
  case class UnificationMismatch(lefts: List[Type], rights: List[Type]) extends TypeError
  case class InfiniteType(name: String, tpe: Type) extends TypeError
  case class Unbound(name: String) extends TypeError
  case class UnknownConstuctor(cname: ConstructorName) extends TypeError
  case class TypeConstructorCollision(matches: NonEmptyList[ConstructorName], env: TypeEnv) extends TypeError
  case class NonTotalMatch(matches: NonEmptyList[ConstructorName], expected: NonEmptyList[ConstructorName]) extends TypeError
  case class InsufficientPatternBind(cname: ConstructorName, params: List[String], expectedTypes: List[Type], in: DefinedType) extends TypeError
}

