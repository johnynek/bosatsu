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

  val intType: Type = TyConst(Const.IntType)

  sealed abstract class Const
  object Const {
    case object IntType extends Const
    case object BoolType extends Const
  }

  sealed abstract class Var {
    def name: String
  }
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(name: String, id: Long) extends Var
  }

  val allBinders: Stream[Var.Bound] = {
    val letters = ('a' to 'z').toStream
    val allIntegers = Stream.iterate(0L)(_ + 1L)
    val lettersWithNumber =
      for {
        num <- allIntegers
        l <- letters
      } yield Var.Bound(s"$l$num")

    letters.map { c => Var.Bound(c.toString) } #::: lettersWithNumber
  }

  case class Meta(id: Long, ref: Ref[Option[Type]])

  def metaTvs(s: List[Type]): Set[Meta] = ???
}
