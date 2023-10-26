package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{PackageName, Identifier}

import Identifier.Bindable

case class ParsedTypeEnv[+A](
    allDefinedTypes: List[DefinedType[A]],
    externalDefs: List[(PackageName, Bindable, Type)]
) {
  def addDefinedType[A1 >: A](dt: DefinedType[A1]): ParsedTypeEnv[A1] =
    copy(allDefinedTypes = dt :: allDefinedTypes)

  def addExternalValue(
      pn: PackageName,
      name: Bindable,
      tpe: Type
  ): ParsedTypeEnv[A] =
    copy(externalDefs = (pn, name, tpe) :: externalDefs)
}

object ParsedTypeEnv {
  val Empty: ParsedTypeEnv[Nothing] = ParsedTypeEnv(Nil, Nil)

  def empty[A]: ParsedTypeEnv[A] = Empty
}
