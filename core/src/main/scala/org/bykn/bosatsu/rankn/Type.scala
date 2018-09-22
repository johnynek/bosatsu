package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList

sealed abstract class Type

object Type {
  type Rho = Type // no top level ForAll
  type Tau = Type // no forall anywhere

  case class ForAll(vars: NonEmptyList[Var], in: Rho) extends Type
  case class Fun(arg: Type, res: Type) extends Type
  case class TyConst(tpe: Const) extends Type
  case class TyVar(toVar: Var) extends Type
  case class TyMeta(toMeta: Meta) extends Type

  sealed abstract class Const
  object Const {
    case object IntType extends Const
    case object BoolType extends Const
  }

  sealed abstract class Var
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(id: Long) extends Var
  }

  case class Meta(id: Long, ref: Ref[Option[Type]])

  def metaTvs(s: List[Type]): Set[Meta] = ???
}
