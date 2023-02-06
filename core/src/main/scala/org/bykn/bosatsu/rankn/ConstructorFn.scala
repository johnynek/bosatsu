package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{TypeName, PackageName}
import org.bykn.bosatsu.Identifier.{Bindable, Constructor}

final case class ConstructorFn(
  name: Constructor,
  args: List[(Bindable, Type)]) {

  def isZeroArg: Boolean = args == Nil

  def isSingleArg: Boolean = args.lengthCompare(1) == 0

  def hasSingleArgType(t: Type): Boolean =
    args match {
      case (_, t0) :: Nil => t == t0
      case _ => false
    }

  def arity: Int = args.length

  def fnType(packageName: PackageName, typeName: TypeName, dtTypeParams: List[Type.Var.Bound]): Type = {
    val tc: Type = Type.const(packageName, typeName)

    def loop(params: List[Type]): Type =
       params match {
         case Nil =>
           dtTypeParams.foldLeft(tc) { (res, v) =>
             Type.TyApply(res, Type.TyVar(v))
           }
         case h :: tail =>
           Type.Fun(h, loop(tail))
       }

    val resT = loop(args.map(_._2))
    Type.forAll(dtTypeParams, resT)
  }
}