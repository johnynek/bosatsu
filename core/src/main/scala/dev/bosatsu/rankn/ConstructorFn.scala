package dev.bosatsu.rankn

import cats.Eq
import dev.bosatsu.PackageName
import dev.bosatsu.Identifier.{Bindable, Constructor}
import cats.syntax.all._

final case class ConstructorParam(
    name: Bindable,
    tpe: Type,
    defaultBinding: Option[Bindable],
    defaultType: Option[Type] = None
) derives CanEqual {
  def signatureTypes: List[Type] = tpe :: defaultType.toList

  def depPackages: List[PackageName] =
    signatureTypes.flatMap(Type.packageNamesIn).distinct
}

final case class ConstructorFn[+A](
    name: Constructor,
    args: List[ConstructorParam],
    exists: List[(Type.Var.Bound, A)] = Nil
) {

  def isZeroArg: Boolean = args == Nil

  def isSingleArg: Boolean = args.lengthCompare(1) == 0

  def hasSingleArgType(t: Type): Boolean =
    args match {
      case arg :: Nil => t == arg.tpe
      case _          => false
    }

  def arity: Int = args.length

  def typeConstants: List[Type.Const] =
    Type.allConsts(args.flatMap(_.signatureTypes)).map(_.tpe)

  def depPackages: List[PackageName] =
    typeConstants.map(_.toDefined.packageName).distinct
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
