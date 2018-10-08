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
   * Return the Bound and Skolem variables that
   * are free in the given list of types
   */
  def freeTyVars(ts: List[Type]): List[Type.Var] = {

    // usually we can recurse in a loop, but sometimes not
    def cheat(ts: List[Type], bound: Set[Type.Var.Bound], acc: List[Type.Var]): List[Type.Var] =
      go(ts, bound, acc)

    @annotation.tailrec
    def go(ts: List[Type], bound: Set[Type.Var.Bound], acc: List[Type.Var]): List[Type.Var] =
      ts match {
        case Nil => acc
        case Type.TyVar(tv) :: rest =>
          // we only check here, we don't add
          val isBound =
            tv match {
              case b@Type.Var.Bound(_) => bound(b)
              case Type.Var.Skolem(_, _) => false
            }
          if (isBound) go(rest, bound, acc)
          else go(rest, bound, tv :: acc)
        case Type.TyApply(a, b) :: rest => go(a :: b :: rest, bound, acc)
        case Type.ForAll(tvs, ty) :: rest =>
          val acc1 = cheat(ty :: Nil, bound ++ tvs.toList, acc)
          // note, tvs ARE NOT bound in rest
          go(rest, bound, acc1)
        case (Type.TyMeta(_) | Type.TyConst(_)) :: rest => go(rest, bound, acc)
      }

    ts.foldLeft(List.empty[Type.Var]) { (acc, t) =>
      go(t :: Nil, Set.empty, acc)
    }
    // reverse and distinct in one go
    .foldLeft((Set.empty[Type.Var], List.empty[Type.Var])) {
      case (res@(vset, vs), v) if vset(v) => res
      case ((vset, vs), v) => (vset + v, v :: vs)
    }
    ._2
  }

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

  /**
   * Report bound variables which are used in quantify. When we
   * infer a sigma type
   */
  def tyVarBinders(tpes: List[Type]): Set[Type.Var] = {
    @annotation.tailrec
    def loop(tpes: List[Type], acc: Set[Type.Var]): Set[Type.Var] =
      tpes match {
        case Nil => acc
        case Type.ForAll(tvs, body) :: rest =>
          loop(rest, acc ++ tvs.toList)
        case Type.TyApply(arg, res) :: rest =>
          loop(arg :: res :: rest, acc)
        case _ :: rest => loop(rest, acc)
      }
    loop(tpes, Set.empty)
  }

}
