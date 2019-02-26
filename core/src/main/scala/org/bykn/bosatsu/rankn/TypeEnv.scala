package org.bykn.bosatsu.rankn

import cats.{Applicative, Eval, Traverse}
import org.bykn.bosatsu.{ConstructorName, TypeName, PackageName, ParamName}
import scala.collection.immutable.SortedMap

import cats.implicits._

// TODO, we should be using SortedMap for reproducibility

case class TypeEnv[+A](
  values: SortedMap[(PackageName, String), Type],
  constructors: SortedMap[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A], Type)],
  definedTypes: SortedMap[(PackageName, TypeName), DefinedType[A]]) {

  def localValuesOf(p: PackageName): SortedMap[String, Type] =
    (SortedMap.newBuilder[String, Type] ++= values.iterator.collect { case ((pn, n), v) if pn == p => (n, v) }).result

  def addDefinedType[A1 >: A](dt: DefinedType[A1]): TypeEnv[A1] = {
    val dt1 = definedTypes.updated((dt.packageName, dt.name), dt)
    val cons1 =
      dt.constructors
        .foldLeft(constructors: SortedMap[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[A1], Type)]) {
          case (cons0, (cname, params, vtpe)) =>
            cons0.updated((dt.packageName, cname), (params, dt, vtpe))
        }
    // here we have to actually add the constructor values:
    val v1 = dt.constructors.foldLeft(values) { case (v0, (cn, _, tpe)) =>
      v0.updated((dt.packageName, cn.asString), tpe)
    }

    copy(constructors = cons1, definedTypes = dt1, values = v1)
  }

  /**
   * External values cannot be inferred and have to be fully
   * annotated
   */
  def addExternalValue(pack: PackageName, name: String, t: Type): TypeEnv[A] =
    copy(values = values.updated((pack, name), t))

  // TODO to support parameter named patterns we'd need to know the
  // parameter names
  lazy val typeConstructors: SortedMap[(PackageName, ConstructorName), (List[Type.Var], List[Type], Type.Const.Defined)] =
    constructors.map { case (pc, (params, dt, _)) =>
      (pc,
        (dt.typeParams,
          params.map(_._2),
          dt.toTypeConst))
    }

  def definedTypeFor(c: (PackageName, ConstructorName)): Option[DefinedType[A]] =
    typeConstructors.get(c).flatMap { case (_, _, d) => toDefinedType(d) }

  def toDefinedType(t: Type.Const.Defined): Option[DefinedType[A]] =
    definedTypes.get((t.packageName, TypeName(t.name)))

  def ++[A1 >: A](that: TypeEnv[A1]): TypeEnv[A1] =
    TypeEnv(values ++ that.values,
      constructors ++ that.constructors,
      definedTypes ++ that.definedTypes)
}

object TypeEnv {
  val empty: TypeEnv[Nothing] =
    TypeEnv(
      SortedMap.empty[(PackageName, String), Type],
      SortedMap.empty[(PackageName, ConstructorName), (List[(ParamName, Type)], DefinedType[Nothing], Type)],
      SortedMap.empty[(PackageName, TypeName), DefinedType[Nothing]])

  implicit val typeEnvTraverse: Traverse[TypeEnv] =
    new Traverse[TypeEnv] {
      implicit def tup3Traverse[X, Y]: Traverse[(X, ?, Y)] =
        new Traverse[(X, ?, Y)] {
          def traverse[F[_]: Applicative, A, B](ta: (X, A, Y))(fn: A => F[B]): F[(X, B, Y)] =
            fn(ta._2).map((ta._1, _, ta._3))
          def foldRight[A, B](fa: (X, A, Y), b: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
            fn(fa._2, b)
          def foldLeft[A, B](fa: (X, A, Y), b: B)(fn: (B, A) => B): B =
            fn(b, fa._2)
        }

      val cTrav = Traverse[SortedMap[(PackageName, ConstructorName), ?]]
        .compose[(List[(ParamName, Type)], ?, Type)]
        .compose[DefinedType]

      val dtTrav = Traverse[SortedMap[(PackageName, TypeName), ?]].compose[DefinedType]
      def traverse[F[_]: Applicative, A, B](da: TypeEnv[A])(fn: A => F[B]): F[TypeEnv[B]] =
        (cTrav.traverse(da.constructors)(fn), dtTrav.traverse(da.definedTypes)(fn)).mapN { (cs, ds) =>
          da.copy(constructors = cs, definedTypes = ds)
        }

      def foldRight[A, B](fa: TypeEnv[A], b: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
        val eb = dtTrav.foldRight(fa.definedTypes, b)(fn)
        cTrav.foldRight(fa.constructors, eb)(fn)
      }

      def foldLeft[A, B](fa: TypeEnv[A], b: B)(fn: (B, A) => B): B = {
        val b1 = cTrav.foldLeft(fa.constructors, b)(fn)
        dtTrav.foldLeft(fa.definedTypes, b1)(fn)
      }
    }
}
