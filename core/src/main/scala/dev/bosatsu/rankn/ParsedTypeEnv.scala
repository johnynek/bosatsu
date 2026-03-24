package dev.bosatsu.rankn

import dev.bosatsu.{Identifier, PackageName}

import Identifier.Bindable

case class ParsedTypeEnv[+A](
    allDefinedTypes: List[DefinedType[A]],
    typeAliases: List[TypeAlias[A]],
    orderedTypes: List[ParsedTypeEnv.TypeStatement[A]],
    externalDefs: List[(PackageName, Bindable, Type)]
) {
  def addDefinedType[A1 >: A](dt: DefinedType[A1]): ParsedTypeEnv[A1] =
    copy(
      allDefinedTypes = dt :: allDefinedTypes,
      orderedTypes = ParsedTypeEnv.TypeStatement.Defined(dt) :: orderedTypes
    )

  def addTypeAlias[A1 >: A](ta: TypeAlias[A1]): ParsedTypeEnv[A1] =
    copy(
      typeAliases = ta :: typeAliases,
      orderedTypes = ParsedTypeEnv.TypeStatement.Alias(ta) :: orderedTypes
    )

  def addExternalValue(
      pn: PackageName,
      name: Bindable,
      tpe: Type
  ): ParsedTypeEnv[A] =
    copy(externalDefs = (pn, name, tpe) :: externalDefs)
}

object ParsedTypeEnv {
  sealed abstract class TypeStatement[+A] {
    def toTypeConst: Type.Const.Defined
  }
  object TypeStatement {
    final case class Defined[+A](definedType: DefinedType[A])
        extends TypeStatement[A] {
      def toTypeConst: Type.Const.Defined = definedType.toTypeConst
    }

    final case class Alias[+A](typeAlias: dev.bosatsu.rankn.TypeAlias[A])
        extends TypeStatement[A] {
      def toTypeConst: Type.Const.Defined = typeAlias.toTypeConst
    }
  }

  val Empty: ParsedTypeEnv[Nothing] = ParsedTypeEnv(Nil, Nil, Nil, Nil)

  def empty[A]: ParsedTypeEnv[A] = Empty
}
