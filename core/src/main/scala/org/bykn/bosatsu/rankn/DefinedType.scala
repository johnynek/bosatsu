package org.bykn.bosatsu.rankn

import cats.{Applicative, Eval, Traverse}
import org.bykn.bosatsu.{TypeName, PackageName}
import scala.collection.immutable.SortedMap

import cats.implicits._

final case class DefinedType[+A](
  packageName: PackageName,
  name: TypeName,
  annotatedTypeParams: List[(Type.Var.Bound, A)],
  constructors: List[ConstructorFn]) {

  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  require(typeParams.distinct == typeParams, typeParams.toString)

  /**
   * A type with exactly one constructor is a struct
   */
  val isStruct: Boolean = constructors.lengthCompare(1) == 0

  /**
   * A newtype is just a wrapper for another type.
   * It could be removed statically from the program
   */
  val isNewType: Boolean =
    constructors match {
      case cf :: Nil => cf.isSingleArg
      case _ => false
    }

  /**
   * This is not the full type, since the full type
   * has a ForAll(typeParams, ... in front if the
   * typeParams is nonEmpty
   */
  val toTypeConst: Type.Const.Defined =
    DefinedType.toTypeConst(packageName, name)

  val toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  /**
   * Is this type Nat like: enum Nat: Zero, Succ(n: Nat)
   */
  val natLike: DefinedType.NatLike =
    constructors match {
      case c0 :: c1 :: Nil =>
        // exactly two constructor functions
        if (c0.isZeroArg && c1.hasSingleArgType(toTypeTyConst)) DefinedType.NatLike.ZeroFirst
        else if (c1.isZeroArg && c0.hasSingleArgType(toTypeTyConst)) DefinedType.NatLike.ZeroLast
        else DefinedType.NatLike.Not
      case _ => DefinedType.NatLike.Not
    }
}

object DefinedType {
  sealed abstract class NatLike
  object NatLike {
    final case object Not extends NatLike
    final case class Is(val zeroFirst: Boolean) extends NatLike {
      @inline def zeroLast: Boolean = !zeroFirst
      def idxIsZero(idx: Int): Boolean =
        (zeroFirst && (idx == 0)) || (zeroLast && (idx == 1))
    }
    val ZeroFirst: Is = Is(true)
    val ZeroLast: Is = Is(false)
  }

  def toTypeConst(pn: PackageName, nm: TypeName): Type.Const.Defined =
    Type.Const.Defined(pn, nm)

  def listToMap[A](dts: List[DefinedType[A]]): SortedMap[(PackageName, TypeName), DefinedType[A]] =
    SortedMap(dts.map { dt => (dt.packageName, dt.name) -> dt }: _*)

  implicit val definedTypeTraverse: Traverse[DefinedType] =
    new Traverse[DefinedType] {
      val listTup = Traverse[List].compose[(Type.Var.Bound, ?)]
      def traverse[F[_]: Applicative, A, B](da: DefinedType[A])(fn: A => F[B]): F[DefinedType[B]] =
        listTup.traverse(da.annotatedTypeParams)(fn).map { ap =>
          da.copy(annotatedTypeParams = ap)
        }

      def foldRight[A, B](fa: DefinedType[A], b: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
        listTup.foldRight(fa.annotatedTypeParams, b)(fn)

      def foldLeft[A, B](fa: DefinedType[A], b: B)(fn: (B, A) => B): B =
        listTup.foldLeft(fa.annotatedTypeParams, b)(fn)

      override def map[A, B](fa: DefinedType[A])(fn: A => B): DefinedType[B] =
        fa.copy(annotatedTypeParams = listTup.map(fa.annotatedTypeParams)(fn))
    }
}
