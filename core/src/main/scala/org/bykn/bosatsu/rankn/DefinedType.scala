package org.bykn.bosatsu.rankn

import cats.{Applicative, Eval, Traverse}
import org.bykn.bosatsu.{TypeName, PackageName, Identifier}
import scala.collection.immutable.SortedMap

import Identifier.Constructor

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
   * This is not the full type, since the full type
   * has a ForAll(typeParams, ... in front if the
   * typeParams is nonEmpty
   */
  val toTypeConst: Type.Const.Defined =
    DefinedType.toTypeConst(packageName, name)

  val toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  /**
   * A type with exactly one constructor is a struct
   */
  def isStruct: Boolean = dataFamily == DataFamily.Struct

  val dataRepr: Constructor => DataRepr =
    constructors match {
      case cf :: Nil =>
        if (cf.isSingleArg) Function.const(DataRepr.NewType)
        else Function.const(DataRepr.Struct(cf.arity))
      case c0 :: c1 :: Nil =>
        // exactly two constructor functions
        if (c0.isZeroArg && c1.hasSingleArgType(toTypeTyConst)) {
          val zero = c0.name
          val one = c1.name

          { cons => if (cons == zero) DataRepr.ZeroNat else DataRepr.SuccNat }
        }
        else if (c1.isZeroArg && c0.hasSingleArgType(toTypeTyConst)) {
          val zero = c1.name
          val one = c0.name

          { cons => if (cons == zero) DataRepr.ZeroNat else DataRepr.SuccNat }
        }
        else {
           val zero = c0.name
           val zrep = DataRepr.Enum(0, c0.arity)
           val orep = DataRepr.Enum(1, c1.arity)

          { cons => if (cons == zero) zrep else orep }
        }
      case cons =>
        val mapping = cons.zipWithIndex.map { case (c, idx) => c.name -> DataRepr.Enum(idx, c.arity) }.toMap

        mapping
    }

  val dataFamily: DataFamily =
    constructors match {
      case cf :: Nil =>
        if (cf.isSingleArg) DataFamily.NewType
        else DataFamily.Struct
      case c0 :: c1 :: Nil =>
        // exactly two constructor functions
        if (c0.isZeroArg && c1.hasSingleArgType(toTypeTyConst)) DataFamily.Nat
        else if (c1.isZeroArg && c0.hasSingleArgType(toTypeTyConst)) DataFamily.Nat
        else DataFamily.Enum
      case cons => DataFamily.Enum
  }
}

object DefinedType {
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
