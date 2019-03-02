package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.PackageName

case class ParsedTypeEnv[+A](allDefinedTypes: List[DefinedType[A]], externalDefs: List[(PackageName, String, Type)]) {
  def addDefinedType[A1 >: A](dt: DefinedType[A1]): ParsedTypeEnv[A1] =
    copy(allDefinedTypes = dt :: allDefinedTypes)

  def addExternalValue(pn: PackageName, name: String, tpe: Type): ParsedTypeEnv[A] =
    copy(externalDefs = (pn, name, tpe) :: externalDefs)
}

object ParsedTypeEnv {
  val Empty: ParsedTypeEnv[Nothing] = ParsedTypeEnv(Nil, Nil)

  def empty[A]: ParsedTypeEnv[A] = Empty
}
