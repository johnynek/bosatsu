package org.bykn.bosatsu.rankn

import cats.data.NonEmptyList
import cats.Eq
import org.bykn.bosatsu.{PackageName, Lit, TypeName, Identifier}
import scala.collection.immutable.SortedSet

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

  implicit val typeOrdering: Ordering[Type] =
    new Ordering[Type] {
      val boundOrd: Ordering[Var.Bound] =
        Ordering[String].on[Var.Bound] { case Var.Bound(v) => v }

      val list = org.bykn.bosatsu.ListOrdering.onType(boundOrd)

      def compare(a: Type, b: Type): Int =
        (a, b) match {
          case (ForAll(v0, i0), ForAll(v1, i1)) =>
            val c = list.compare(v0.toList, v1.toList)
            if (c == 0) compare(i0, i1) else c
          case (ForAll(_, _), _) => -1
          case (TyConst(Const.Defined(p0, n0)), TyConst(Const.Defined(p1, n1))) =>
            val c = Ordering[PackageName].compare(p0, p1)
            if (c == 0) Ordering[TypeName].compare(n0, n1) else c
          case (TyConst(_), ForAll(_, _)) => 1
          case (TyConst(_), _) => -1
          case (TyVar(v0), TyVar(v1)) =>
            Ordering[Var].compare(v0, v1)
          case (TyVar(_), ForAll(_, _) | TyConst(_)) => 1
          case (TyVar(_), _) => -1
          case (TyMeta(Meta(i0, _)), TyMeta(Meta(i1, _))) =>
            java.lang.Long.compare(i0, i1)
          case (TyMeta(_), TyApply(_, _)) => -1
          case (TyMeta(_), _) => 1
          case (TyApply(a0, b0), TyApply(a1, b1)) =>
            val c = compare(a0, a1)
            if (c == 0) compare(b0, b1) else c
          case (TyApply(_, _), _) => 1
        }
    }


  def constantsOf(t: Type): List[Const] =
    t match {
      case ForAll(_, t) => constantsOf(t)
      case TyApply(on, arg) => constantsOf(on) ::: constantsOf(arg)
      case TyConst(c) => c :: Nil
      case TyVar(_) | TyMeta(_) => Nil
    }

  def hasNoVars(t: Type): Boolean =
    t match {
      case TyConst(c) => true
      case TyVar(_) | TyMeta(_) => false
      case TyApply(on, arg) => hasNoVars(on) && hasNoVars(arg)
      case fa@ForAll(_, _) => freeTyVars(fa :: Nil).isEmpty
    }

  @annotation.tailrec
  final def forAll(vars: List[Var.Bound], in: Type): Type =
    vars match {
      case Nil => in
      case ne@(h :: tail) =>
        in match {
          case Type.ForAll(nes, tt) => forAll(ne ::: nes.toList, tt)
          case notForAll => Type.ForAll(NonEmptyList(h, tail), notForAll)
        }
    }

  implicit val typeEq: Eq[Type] =
    new Eq[Type] {
      def eqv(left: Type, right: Type): Boolean =
        left == right
    }

  def getTypeOf(lit: Lit): Type =
    lit match {
      case Lit.Integer(_) => Type.IntType
      case Lit.Str(_) => Type.StrType
    }

  /**
   * types are var, meta, or const, or applied or forall on one of
   * those. This returns the Type.TyConst found
   * by recursing
   */
  @annotation.tailrec
  final def rootConst(t: Type): Option[Type.TyConst] =
    t match {
      case tyc@TyConst(_) => Some(tyc)
      case TyVar(_) | TyMeta(_) => None
      case ForAll(_, r) => rootConst(r)
      case TyApply(left, _) => rootConst(left)
    }

  def applicationArgs(t: Type): (Type, List[Type]) = {
    @annotation.tailrec
    def loop(t: Type, tail: List[Type]): (Type, List[Type]) =
      t match {
        case TyApply(left, right) => loop(left, right :: tail)
        case notApply => (notApply, tail)
      }
    loop(t, Nil)
  }

  def substituteVar(t: Type, env: Map[Type.Var, Type]): Type =
    t match {
      case TyApply(on, arg) => TyApply(substituteVar(on, env), substituteVar(arg, env))
      case v@TyVar(n) => env.getOrElse(n, v)
      case ForAll(ns, rho) =>
        val boundSet: Set[Var] = ns.toList.toSet
        val env1 = env.filterKeys { v => !boundSet(v) }
        substituteVar(rho, env1) match {
          case ForAll(ns1, r1) =>
            ForAll(ns ::: ns1, r1)
          case notForAll =>
            ForAll(ns, notForAll)
        }
      case m@TyMeta(_) => m
      case c@TyConst(_) => c
    }

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

    go(ts, Set.empty, Nil)
      .reverse
      .distinct
  }

  /**
   * Return the Bound variables that
   * are free in the given list of types
   */
  def freeBoundTyVars(ts: List[Type]): List[Type.Var.Bound] =
    freeTyVars(ts).collect { case b@Type.Var.Bound(_) => b }

  /**
   * These are upper-case to leverage scala's pattern
   * matching on upper-cased vals
   */
  val IntType: Type = TyConst(Const.predef("Int"))
  val BoolType: Type = TyConst(Const.predef("Bool"))
  val StrType: Type = TyConst(Const.predef("String"))
  val FnType: Type = TyConst(Const.predef("Fn"))
  val ListType: Type = TyConst(Const.predef("List"))
  val DictType: Type = TyConst(Const.predef("Dict"))
  val OptionType: Type = TyConst(Const.predef("Option"))
  val UnitType = TyConst(Type.Const.predef("Unit"))
  val TupleConsType = TyConst(Type.Const.predef("TupleCons"))

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

  object Tuple {
    def unapply(t: Type): Option[List[Type]] =
      t match {
        case UnitType => Some(Nil)
        case TyApply(TyApply(TupleConsType, h), t) =>
          unapply(t) match {
            case None => None
            case Some(ts) => Some(h :: ts)
          }
        case _ => None
      }

    def apply(ts: List[Type]): Type =
      ts match {
        case Nil => UnitType
        case h :: tail =>
          val tailT = apply(tail)
          TyApply(TyApply(TupleConsType, h), tailT)
      }
  }

  object OptionT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(OptionType, t) => Some(t)
        case _ => None
      }
  }

  object DictT {
    def unapply(t: Type): Option[(Type, Type)] =
      t match {
        case TyApply(TyApply(DictType, kt), vt) => Some((kt, vt))
        case _ => None
      }
  }

  object ListT {
    def unapply(t: Type): Option[Type] =
      t match {
        case TyApply(ListType, t) => Some(t)
        case _ => None
      }
  }

  sealed abstract class Const
  object Const {
    case class Defined(packageName: PackageName, name: TypeName) extends Const

    def predef(name: String): Defined =
      Defined(PackageName.predef, TypeName(Identifier.Constructor(name)))
  }

  sealed abstract class Var {
    def name: String
  }
  object Var {
    case class Bound(name: String) extends Var
    case class Skolem(name: String, id: Long) extends Var

    implicit val varOrdering: Ordering[Var] =
      new Ordering[Var] {
        def compare(a: Var, b: Var): Int =
          (a, b) match {
            case (Bound(a), Bound(b)) => a.compareTo(b)
            case (Bound(_), _) => -1
            case (Skolem(n0, i0), Skolem(n1, i1)) =>
              val c = java.lang.Long.compare(i0, i1)
              if (c == 0) n0.compareTo(n1) else c
            case (Skolem(_, _), _) => 1
          }
      }
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

  def alignBinders[A](items: NonEmptyList[A], avoid: Set[Var.Bound]): NonEmptyList[(A, Var.Bound)] = {
    val sz = items.size
    // for some reason on 2.11 we need to do .iterator or this will be an infinite loop
    val bs = NonEmptyList.fromListUnsafe(allBinders.iterator.filterNot(avoid).take(sz).toList)
    NonEmptyList((items.head, bs.head), items.tail.zip(bs.tail))
  }

  case class Meta(id: Long, ref: Ref[Option[Type]])

  object Meta {
    implicit val orderingMeta: Ordering[Meta] =
      Ordering.by { m: Meta => m.id }
  }

  /**
   * Final the set of all of Metas inside the list of given types
   */
  def metaTvs(s: List[Type]): SortedSet[Meta] = {
    @annotation.tailrec
    def go(check: List[Type], acc: SortedSet[Meta]): SortedSet[Meta] =
      check match {
        case Nil => acc
        case ForAll(_, r) :: tail => go(r :: tail, acc)
        case TyApply(a, r) :: tail => go(a :: r :: tail, acc)
        case TyMeta(m) :: tail => go(tail, acc + m)
        case _ :: tail => go(tail, acc)
      }
    go(s, SortedSet.empty)
  }

  /**
   * Report bound variables which are used in quantify. When we
   * infer a sigma type
   */
  def tyVarBinders(tpes: List[Type]): Set[Type.Var.Bound] = {
    @annotation.tailrec
    def loop(tpes: List[Type], acc: Set[Type.Var.Bound]): Set[Type.Var.Bound] =
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
