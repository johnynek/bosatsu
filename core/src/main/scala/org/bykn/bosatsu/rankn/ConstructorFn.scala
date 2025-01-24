package org.bykn.bosatsu.rankn

import org.bykn.bosatsu.PackageName
import org.bykn.bosatsu.Identifier.{Bindable, Constructor}

final case class ConstructorFn(
    name: Constructor,
    args: List[(Bindable, Type)]
) {

  def isZeroArg: Boolean = args == Nil

  def isSingleArg: Boolean = args.lengthCompare(1) == 0

  def hasSingleArgType(t: Type): Boolean =
    args match {
      case (_, t0) :: Nil => t == t0
      case _              => false
    }

  def arity: Int = args.length

  def depPackages: List[PackageName] =
    args.flatMap { case (_, t) => Type.packageNamesIn(t) }.distinct
}
