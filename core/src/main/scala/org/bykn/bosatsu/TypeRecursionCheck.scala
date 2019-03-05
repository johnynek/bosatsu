package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import org.bykn.bosatsu.rankn.{ DefinedType, Type, TypeEnv }

import cats.implicits._

/**
 * This checks to make sure any recursion of types is legal
 * Note, since packages from a DAG, we know that anything
 * in imports cannot refer to packageDefinedTypes
 */
object TypeRecursionCheck {

  def check[A](imports: TypeEnv[A],
    packageDefinedTypes: List[DefinedType[A]]): ValidatedNel[NonEmptyList[DefinedType[A]], Unit] = {

    val typeMap = DefinedType.listToMap(packageDefinedTypes)
    /*
     * Check that the types defined here are not circular.
     * Since the packages already form a DAG we know
     * that we don't need to check across package boundaries
     */
    def typeDepends(dt: DefinedType[A]): List[DefinedType[A]] =
      (for {
        cons <- dt.constructors
        Type.Const.Defined(p, n) <- cons._2.flatMap { case (_, t) => Type.constantsOf(t) }
        dt1 <- typeMap.get((p, TypeName(n))).toList
      } yield dt1).distinct

    packageDefinedTypes.traverse_ { dt =>
      graph.Tree.dagToTree(dt)(typeDepends _)
    }
  }

  /**
   * If we only have recursion along covariant or phantom paths, this is okay, and cannot
   * be used to break totality
   */
  def checkLegitRecursion(
    imports: TypeEnv[Variance],
    packageDefinedTypes: List[DefinedType[Variance]]): ValidatedNel[NonEmptyList[DefinedType[Variance]], Unit] = {

    val typeMap = DefinedType.listToMap(packageDefinedTypes)
    /*
     * Check that the types defined here are not circular.
     * Since the packages already form a DAG we know
     * that we don't need to check across package boundaries
     */
    def typeDepends(dt: DefinedType[Variance]): List[(Variance, DefinedType[Variance])] = {
      /**
       * enum L: E, N(head: a, tail: L[a])
       *
       * struct Tree(root: a, children: L[Tree[a]])
       *
       * Tree[a] -> + L[Tree[a]]
       * L[Tree[a]] -> + Tree[a], +L[Tree[a]]
       */
      // val next = for {
      //   cons <- dt.constructors
      //   (_, consArgs, _) = cons
      //   Type.Const.Defined(p, n) <- consArgs.flatMap { case (_, t) => Type.constantsOf(t) }
      //   dt1 <- typeMap.get((p, TypeName(n))).toList
      // } yield dt1

      // next.distinct
      ???
    }

    def checkCycle(
      start: DefinedType[Variance],
      loop: NonEmptyList[(Variance, DefinedType[Variance])]): ValidatedNel[NonEmptyList[DefinedType[Variance]], Unit] = {

      val loopVariance = loop.tail.foldLeft(loop.head._1) { case (v0, (v1, _)) => v0 * v1 }
      if ((loopVariance == Variance.in) || (loopVariance == Variance.contra)) {
        Validated.invalidNel(start :: loop.map(_._2))
      }
      else Validated.valid(())
    }

    packageDefinedTypes.traverse_ { dt =>
      val cycles = graph.Paths.allCycles(dt)(typeDepends _)
      cycles.traverse_(checkCycle(dt, _))
    }
  }
}
