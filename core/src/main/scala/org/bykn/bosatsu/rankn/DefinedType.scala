package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}

case class DefinedType(
  packageName: PackageName,
  name: TypeName,
  typeParams: List[Type.Var.Bound],
  constructors: List[(ConstructorName, List[(ParamName, Type)], Type)]) {

  require(typeParams.distinct == typeParams, typeParams.toString)

  /**
   * A type with exactly one constructor is a struct
   */
  def isStruct: Boolean = constructors.lengthCompare(1) == 0
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

  /**
   * This may be a ForAll type if there are typeParams
   */
  def fullType: Type =
    Type.forAll(typeParams, toTypeTyConst)
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
    Type.forAll(tparams, resT)
  }
}
