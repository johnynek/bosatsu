package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.{TypeName, PackageName}
import org.bykn.bosatsu.Identifier.{Bindable, Constructor}

final case class ConstructorFn(
  name: Constructor,
  args: List[(Bindable, Type)],
  fnType: Type) {

  def isZeroArg: Boolean = args == Nil

  def isSingleArg: Boolean = args.lengthCompare(1) == 0

  def hasSingleArgType(t: Type): Boolean =
    args match {
      case (_, t0) :: Nil => t == t0
      case _ => false
    }
}

object ConstructorFn {

  def build(packageName: PackageName, typeName: TypeName, tparams: List[Type.Var.Bound], name: Constructor, fnParams: List[(Bindable, Type)]): ConstructorFn = {
    val tc: Type = Type.const(packageName, typeName)

    def loop(params: List[Type]): Type =
       params match {
         case Nil =>
           tparams.foldLeft(tc) { (res, v) =>
             Type.TyApply(res, Type.TyVar(v))
           }
         case h :: tail =>
           Type.Fun(h, loop(tail))
       }

    val resT = loop(fnParams.map(_._2))
    val fnType = Type.forAll(tparams, resT)

    ConstructorFn(name, fnParams, fnType)
  }
}
