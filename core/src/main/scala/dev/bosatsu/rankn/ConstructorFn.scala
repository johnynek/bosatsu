package dev.bosatsu.rankn

import cats.Eq
import dev.bosatsu.PackageName
import dev.bosatsu.Identifier.{Bindable, Constructor}
import cats.syntax.all._

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
  implicit def eqConstructorFn[A: Eq]: Eq[ConstructorFn[A]] =
    Eq.instance { (left, right) =>
      (left.name == right.name) &&
      (left.args == right.args) &&
      (left.exists.lengthCompare(right.exists) == 0) &&
      left.exists.iterator.zip(right.exists.iterator).forall {
        case ((lb, la), (rb, ra)) => (lb === rb) && (la === ra)
      }
    }
}
