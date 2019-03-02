package org.bykn.bosatsu.rankn

import cats.{Applicative, Eval, Traverse}
import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}
import scala.collection.immutable.SortedMap

import cats.implicits._

case class DefinedType[+A](
  packageName: PackageName,
  name: TypeName,
  annotatedTypeParams: List[(Type.Var.Bound, A)],
  constructors: List[(ConstructorName, List[(ParamName, Type)], Type)]) {

  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  require(typeParams.distinct == typeParams, typeParams.toString)

  /**
   * A type with exactly one constructor is a struct
   */
  def isStruct: Boolean = constructors.lengthCompare(1) == 0
  /**
   * This is not the full type, since the full type
   * has a ForAll(typeParams, ... in front if the
   * typeParams is nonEmpty
   */
  def toTypeConst: Type.Const.Defined =
    DefinedType.toTypeConst(packageName, name)

  def toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  def toOpaque: DefinedType[A] =
    copy(constructors = Nil)
}

object DefinedType {
  def toTypeConst(pn: PackageName, nm: TypeName): Type.Const.Defined =
    Type.Const.Defined(pn, nm.asString)

  def constructorValueType(pn: PackageName, name: TypeName, tparams: List[Type.Var.Bound], fnParams: List[Type]): Type = {
    val tc: Type = Type.TyConst(toTypeConst(pn, name))

    def loop(params: List[Type]): Type =
       params match {
         case Nil =>
           tparams.foldLeft(tc) { (res, v) =>
             Type.TyApply(res, Type.TyVar(v))
           }
         case h :: tail =>
           Type.Fun(h, loop(tail))
       }

    val resT = loop(fnParams)
    Type.forAll(tparams, resT)
  }

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
