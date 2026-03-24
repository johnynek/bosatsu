package dev.bosatsu.rankn

import cats.{Applicative, Eval, Foldable, Traverse}
import cats.implicits._
import dev.bosatsu.{Kind, PackageName, TypeName}
import scala.collection.immutable.SortedMap

final case class TypeAlias[+A](
    packageName: PackageName,
    name: TypeName,
    annotatedTypeParams: List[(Type.Var.Bound, A)],
    rhs: Type
) {
  val typeParams: List[Type.Var.Bound] =
    annotatedTypeParams.map(_._1)

  val toTypeConst: Type.Const.Defined =
    Type.Const.Defined(packageName, name)

  val toTypeTyConst: Type.TyConst =
    Type.TyConst(toTypeConst)

  lazy val dependsOn: List[Type.TyConst] =
    Type.allConsts(rhs :: Nil).filterNot(_ == toTypeTyConst)

  def kindOf(implicit ev: A <:< Kind.Arg): Kind =
    Kind(annotatedTypeParams.map { case (_, a) => ev(a) }*)

  def expandWith(args: List[Type]): Option[Type] =
    if (args.lengthCompare(typeParams.length) < 0) None
    else {
      val (appliedArgs, rest) = args.splitAt(typeParams.length)
      val substitutions =
        typeParams.iterator
          .zip(appliedArgs.iterator)
          .map { case (tv, arg) => (tv: Type.Var) -> arg }
          .toMap
      Some(Type.applyAll(Type.substituteVar(rhs, substitutions), rest))
    }

  def depPackages: List[PackageName] =
    (packageName :: Type.packageNamesIn(rhs)).distinct
}

object TypeAlias {
  def listToMap[A](
      aliases: List[TypeAlias[A]]
  ): SortedMap[(PackageName, TypeName), TypeAlias[A]] =
    SortedMap(aliases.map(ta => (ta.packageName, ta.name) -> ta)*)

  def toKindMap[F[_]: Foldable](
      aliases: F[TypeAlias[Kind.Arg]]
  ): Map[Type.Const.Defined, Kind] =
    aliases
      .foldLeft(
        Map.newBuilder[Type.Const.Defined, Kind]
      )((b, ta) => b += ((ta.toTypeConst, ta.kindOf)))
      .result()

  given Traverse[TypeAlias] with {
    private val listTup = Traverse[List].compose[[X] =>> (Type.Var.Bound, X)]

    def traverse[F[_]: Applicative, A, B](
        fa: TypeAlias[A]
    )(fn: A => F[B]): F[TypeAlias[B]] =
      listTup.traverse(fa.annotatedTypeParams)(fn).map { params =>
        TypeAlias(fa.packageName, fa.name, params, fa.rhs)
      }

    def foldRight[A, B](fa: TypeAlias[A], b: Eval[B])(
        fn: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      listTup.foldRight(fa.annotatedTypeParams, b)(fn)

    def foldLeft[A, B](fa: TypeAlias[A], b: B)(fn: (B, A) => B): B =
      listTup.foldLeft(fa.annotatedTypeParams, b)(fn)
  }
}
