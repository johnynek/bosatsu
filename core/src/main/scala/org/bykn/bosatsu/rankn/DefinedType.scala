package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}

case class DefinedType(
  packageName: PackageName,
  name: TypeName,
  typeParams: List[Type.Var.Bound],
  constructors: List[(ConstructorName, List[(ParamName, Type)], Type)]) {
  /**
   * This is not the full type, since the full type
   * has a ForAll(typeParams, ... in front if the
   * typeParams is nonEmpty
   */
  def toTypeConst: Type.Const.Defined =
    DefinedType.toTypeConst(packageName, name)

  def toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  def toOpaque: DefinedType =
    copy(constructors = Nil)
}

object DefinedType {
  def toTypeConst(pn: PackageName, nm: TypeName): Type.Const.Defined =
    Type.Const.Defined(pn, nm.asString)

  def constructorValueType(pn: PackageName, name: TypeName, tparams: List[Type.Var.Bound], fnParams: List[Type]): Type = {
    val tc: Type = Type.TyConst(toTypeConst(pn, name))

    def loop(params: List[Type]): Type =
       params match {
         case Nil =>
           tparams.foldLeft(tc) { (res, v) =>
             Type.TyApply(res, Type.TyVar(v))
           }
         case h :: tail =>
           Type.Fun(h, loop(tail))
       }

    val resT = loop(fnParams)
    tparams match {
      case Nil => resT
      case b :: tail => Type.ForAll(NonEmptyList(b, tail), resT)
    }
  }
}
