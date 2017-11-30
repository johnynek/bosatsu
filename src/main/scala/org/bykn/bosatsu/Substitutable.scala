package org.bykn.bosatsu

import cats.data.NonEmptyList
import cats.{Functor, Foldable}
import cats.implicits._

trait Substitutable[T] {
  def apply(sub: Subst, t: T): T
  def typeVars(t: T): Set[String]

  def occurs(tvar: String, t: T): Boolean =
    typeVars(t).contains(tvar)
}

object Substitutable {
  def apply[T](implicit s: Substitutable[T]): Substitutable[T] = s

  def generalize[T: Substitutable](te: T, t: Type): Scheme = {
    val missing = Substitutable[Type].typeVars(t) -- Substitutable[T].typeVars(te)
    Scheme(missing.toList.sorted, t)
  }

  implicit val forType: Substitutable[Type] =
    new Substitutable[Type] {
      import Type._

      def apply(sub: Subst, t: Type): Type =
        t match {
          case Arrow(from, to) => Arrow(apply(sub, from), apply(sub, to))
          case d@Declared(_, _) => d
          case TypeApply(hk, arg) => TypeApply(apply(sub, hk), apply(sub, arg))
          //case TypeLambda(param, in) => TypeLambda(param, apply(sub, in))
          case v@Var(name) => sub.getOrElse(name, v)
        }

      def typeVars(t: Type) =
        t match {
          case Arrow(from, to) => typeVars(from) | typeVars(to)
          case Declared(_, _) => Set.empty
          case TypeApply(hk, arg) => typeVars(hk) | typeVars(arg)
          //case TypeLambda(_, in) => typeVars(in)
          case Var(name) => Set(name)
        }
    }


  implicit val forScheme: Substitutable[Scheme] =
    new Substitutable[Scheme] {
      def apply(sub: Subst, t: Scheme) = {
        // all the names in scheme are shadows so we need
        // to remove them:
        val newSubst = Subst(t.vars.foldLeft(sub.toMap)(_ - _))
        Scheme(t.vars, Substitutable[Type].apply(newSubst, t.result))
      }
      def typeVars(s: Scheme) =
        Substitutable[Type].typeVars(s.result) -- s.vars
    }

  implicit val forConstructorName: Substitutable[ConstructorName] = opaqueSubstitutable
  implicit val forParamName: Substitutable[ParamName] = opaqueSubstitutable

  implicit val forDefinedType: Substitutable[DefinedType] =
    new Substitutable[DefinedType] {
      def apply(sub: Subst, t: DefinedType) = {
        // all the names in typeParams are shadows so we need
        // to remove them:
        val newSubst = Subst(t.typeParams.map(_.name).foldLeft(sub.toMap)(_ - _))
        val newCons = Substitutable[List[(ConstructorName, List[(ParamName, Type)])]].apply(newSubst, t.constructors)
        t.copy(constructors = newCons)
      }

      def typeVars(d: DefinedType) = d.typeParams.iterator.map(_.name).toSet
    }

  def fromMapFold[F[_]: Functor: Foldable, A: Substitutable]: Substitutable[F[A]] =
    new Substitutable[F[A]] {
      def apply(sub: Subst, t: F[A]): F[A] =
        t.map(Substitutable[A].apply(sub, _))

      def typeVars(as: F[A]) =
        as.foldLeft(Set.empty[String]) { _ | Substitutable[A].typeVars(_) }
    }


  implicit def forList[A: Substitutable]: Substitutable[List[A]] =
    fromMapFold[List, A]

  implicit def forNonEmptyList[A: Substitutable]: Substitutable[NonEmptyList[A]] =
    fromMapFold[NonEmptyList, A]

  implicit def forSet[A: Substitutable]: Substitutable[Set[A]] =
    new Substitutable[Set[A]] {
      def apply(sub: Subst, t: Set[A]): Set[A] =
        t.map(Substitutable[A].apply(sub, _))

      def typeVars(as: Set[A]) =
        as.foldLeft(Set.empty[String]) { _ | Substitutable[A].typeVars(_) }
    }

  implicit def forPair[A: Substitutable, B: Substitutable]: Substitutable[(A, B)] =
    new Substitutable[(A, B)] {
      def apply(sub: Subst, t: (A, B)): (A, B) = {
        val a = Substitutable[A].apply(sub, t._1)
        val b = Substitutable[B].apply(sub, t._2)
        (a, b)
      }

      def typeVars(ab: (A, B)) =
        Substitutable[A].typeVars(ab._1) | Substitutable[B].typeVars(ab._2)
    }

  implicit def forMap[K, V: Substitutable]: Substitutable[Map[K, V]] =
    fromMapFold[Map[K, ?], V]

  implicit val forTypeEnv: Substitutable[TypeEnv] =
    new Substitutable[TypeEnv] {
      implicit def eSub[A: Substitutable, B: Substitutable]: Substitutable[Either[A, B]] =
        new Substitutable[Either[A, B]] {
          def apply(sub: Subst, eab: Either[A, B]) =
            eab match {
              case Right(b) => Right(Substitutable[B].apply(sub, b))
              case Left(a) => Left(Substitutable[A].apply(sub, a))
            }
          def typeVars(eab: Either[A, B]) =
            eab match {
              case Right(b) => Substitutable[B].typeVars(b)
              case Left(a) => Substitutable[A].typeVars(a)
            }
        }

      def apply(sub: Subst, te: TypeEnv): TypeEnv =
        TypeEnv(
          te.packageName,
          Substitutable[Map[String, Scheme]].apply(sub, te.values),
          Substitutable[Map[(PackageName, ConstructorName), DefinedType]].apply(sub, te.constructors),
          Substitutable[Map[(PackageName, TypeName), DefinedType]].apply(sub, te.definedTypes),
          Substitutable[Map[String, Either[(ConstructorName, DefinedType), DefinedType]]].apply(sub, te.imported)
        )

      def typeVars(te: TypeEnv) =
        Substitutable[Map[String, Scheme]].typeVars(te.values) |
        Substitutable[Map[(PackageName, ConstructorName), DefinedType]].typeVars(te.constructors) |
        Substitutable[Map[(PackageName, TypeName), DefinedType]].typeVars(te.definedTypes) |
        Substitutable[Map[String, Either[(ConstructorName, DefinedType), DefinedType]]].typeVars(te.imported)
    }

  implicit val forConstraint: Substitutable[Constraint] =
    new Substitutable[Constraint] {
      def apply(sub: Subst, c: Constraint): Constraint =
        Constraint(Substitutable[Type].apply(sub, c.left), Substitutable[Type].apply(sub, c.right), c.leftRegion, c.rightRegion)
      def typeVars(c: Constraint) =
        Substitutable[Type].typeVars(c.left) | Substitutable[Type].typeVars(c.right)
    }


  def opaqueSubstitutable[T]: Substitutable[T] =
    new Substitutable[T] {
      def apply(sub: Subst, t: T): T = t
      def typeVars(t: T) = Set.empty
    }

  implicit val forUnit: Substitutable[Unit] =
    opaqueSubstitutable[Unit]
}
