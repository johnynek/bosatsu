package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}

case class DefinedType(
  packageName: PackageName,
  name: TypeName,
  typeParams: List[Type.Var],
  constructors: List[(ConstructorName, List[(ParamName, Type)])]) {

  /**
   * This is not the full type, since the full type
   * has a ForAll(typeParams, ... in front if the
   * typeParams is nonEmpty
   */
  def toTypeConst: Type.Const.Defined =
    Type.Const.Defined(packageName, name.asString)

  def toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  def toOpaque: DefinedType =
    copy(constructors = Nil)
}
