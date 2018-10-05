package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import org.bykn.bosatsu.PackageName

sealed abstract class Type

object Type {
  type Rho = Type // no top level ForAll
  type Tau = Type // no forall anywhere

  case class ForAll(vars: NonEmptyList[Var.Bound], in: Rho) extends Type {
    in match {
      case ForAll(_, _) => sys.error(s"invalid nested ForAll")
      case _ => ()
    }
  }
  case class TyConst(tpe: Const) extends Type
  case class TyVar(toVar: Var) extends Type
  case class TyMeta(toMeta: Meta) extends Type
  case class TyApply(on: Type, arg: Type) extends Type

  /**
   * These are upper-case to leverage scala's pattern
   * matching on upper-cased vals
   */
  val IntType: Type = TyConst(Const.predef("Integer"))
  val BoolType: Type = TyConst(Const.predef("Boolean"))
  val StrType: Type = TyConst(Const.predef("String"))
  val FnType: Type = TyConst(Const.predef("Fn"))

  object Fun {
    def unapply(t: Type): Option[(Type, Type)] =
      t match {
        case TyApply(TyApply(FnType, from), to) =>
          Some((from, to))
        case _ => None
      }

    def apply(from: Type, to: Type): Type =
      TyApply(TyApply(FnType, from), to)
  }


  sealed abstract class Const
  object Const {
    case class Defined(packageName: PackageName, name: String) extends Const

    def predef(name: String): Defined =
      Defined(PackageName.predef, name)
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

  def metaTvs(s: List[Type]): Set[Meta] = {
    @annotation.tailrec
    def go(check: List[Type], acc: Set[Meta]): Set[Meta] =
      check match {
        case Nil => acc
        case ForAll(_, r) :: tail => go(r :: tail, acc)
        case TyApply(a, r) :: tail => go(a :: r :: tail, acc)
        case TyMeta(m) :: tail => go(tail, acc + m)
        case _ :: tail => go(tail, acc)
      }
    go(s, Set.empty)
  }
}
