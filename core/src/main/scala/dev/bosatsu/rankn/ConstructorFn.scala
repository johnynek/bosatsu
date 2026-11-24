package dev.bosatsu.rankn

import cats.Eq
import cats.data.NonEmptyList
import dev.bosatsu.{Kind, PackageName}
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

  /** The existentials and field types of `cfn` applied to `args`, the type
    * arguments that `paramSub` maps its type parameters to.
    *
    * Unlike a quantifier inside a type, an existential is not named apart
    * from the arguments by `normalize`. So existentials that share a name
    * with a free variable of `args` (or with a variable a field uses
    * without declaring it) are renamed first. Otherwise a reader of the
    * field types would take them for that outer variable. Other names need
    * no avoiding: an outer variable that occurs in no field can't be
    * captured.
    */
  def openAt(
      cfn: ConstructorFn[Kind.Arg],
      paramSub: Map[Type.Var, Type],
      args: List[Type]
  ): (List[(Type.Var.Bound, Kind)], List[Type]) = {
    val fields = cfn.args.map(_.tpe)
    NonEmptyList.fromList(cfn.exists.map { case (b, ka) => (b, ka.kind) }) match {
      case None =>
        (Nil, fields.map(Type.substituteVar(_, paramSub)))
      case Some(exists) =>
        val existNames = exists.iterator.map(_._1).toSet
        val avoid =
          Type.freeBoundTyVars(args).toSet ++
            Type.freeBoundTyVars(fields).filterNot(existNames)
        val (rename, exists1) = Type.unshadow(exists, avoid)
        val sub = paramSub ++ rename
        (exists1.toList, fields.map(Type.substituteVar(_, sub)))
    }
  }

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
