package org.bykn.bosatsu

import cats.data.{NonEmptyList, ValidatedNel}
import org.bykn.bosatsu.rankn.{ DefinedType, Type, TypeEnv }

import cats.implicits._

/**
 * This checks to make sure any recursion of types is legal
 * Note, since packages from a DAG, we know that anything
 * in imports cannot refer to packageDefinedTypes
 */
object TypeRecursionCheck {

  def check(imports: TypeEnv[Unit],
    packageDefinedTypes: List[DefinedType[Unit]]): ValidatedNel[NonEmptyList[DefinedType[Unit]], Unit] = {

    val typeMap = DefinedType.listToMap(packageDefinedTypes)
    /*
     * Check that the types defined here are not circular.
     * Since the packages already form a DAG we know
     * that we don't need to check across package boundaries
     */
    def typeDepends(dt: DefinedType[Unit]): List[DefinedType[Unit]] =
      (for {
        cons <- dt.constructors
        Type.Const.Defined(p, n) <- cons._2.flatMap { case (_, t) => Type.constantsOf(t) }
        dt1 <- typeMap.get((p, TypeName(n))).toList
      } yield dt1).distinct

    packageDefinedTypes.traverse_ { dt =>
      Tree.dagToTree(dt)(typeDepends _)
    }
  }
}
