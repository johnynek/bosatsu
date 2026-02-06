package dev.bosatsu.rankn

import cats.Eq
import dev.bosatsu.PackageName
import dev.bosatsu.Identifier.{Bindable, Constructor}

final case class ConstructorFn[+A](
    name: Constructor,
    args: List[(Bindable, Type)],
    exists: List[(Type.Var.Bound, A)] = Nil
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

object ConstructorFn {
  implicit def eqConstructorFn[A]: Eq[ConstructorFn[A]] =
    Eq.fromUniversalEquals
}
