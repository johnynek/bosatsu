package org.bykn.bosatsu

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import org.bykn.bosatsu.rankn.{DefinedType, Type, TypeEnv}
import org.bykn.bosatsu.graph.Paths

import cats.syntax.all._

/**
 * This checks to make sure any recursion of types is legal
 * Note, since packages from a DAG, we know that anything
 * in imports cannot refer to packageDefinedTypes
 */
object TypeRecursionCheck {

  /**
   * We require that defined types form a DAG excluding any links to themselves.
   *
   * For self references, we only have recursion along covariant or phantom paths,
   * this is okay, and cannot be used to break totality
   */
  def checkLegitRecursion(
    imports: TypeEnv[Variance],
    packageDefinedTypes: List[DefinedType[Variance]]): ValidatedNel[NonEmptyList[DefinedType[Variance]], Unit] = {

    val typeMap = DefinedType.listToMap(packageDefinedTypes)

    // Check DAG-ness of non-self-loops in current package,
    // imports are already a DAG
    def typeLocalDepends(dt: DefinedType[Variance]): List[DefinedType[Variance]] = {
      val depends = for {
        cons <- dt.constructors
        Type.Const.Defined(p, n) <- cons.args.flatMap { case (_, t) => Type.constantsOf(t) }
        dt1 <- typeMap.get((p, n)).toList // we only need edges into this package,
      } yield dt1

      // we handle self loops separtely
      depends.distinct.filterNot(_ == dt)
    }

    val assertDag: ValidatedNel[NonEmptyList[DefinedType[Variance]], Unit] =
      packageDefinedTypes.traverse_ { dt =>
        // TODO, this can be optimized to check the entire graph at once
        NonEmptyList.fromList(Paths.allCycle0(dt)(typeLocalDepends _)) match {
          case None => Validated.valid(())
          case Some(paths) =>
            Validated.invalid(paths.map(dt :: _))
        }
      }

    def getDT(dt: Type.Const.Defined): Option[DefinedType[Variance]] = {
      val tn = dt.name
      typeMap.get((dt.packageName, tn))
        .orElse(imports.getType(dt.packageName, tn))
    }

    def buildEdges(dt: DefinedType[Variance]): List[(DefinedType[Variance], Variance, DefinedType[Variance])] = {
      /*
       * enum L: E, N(head: a, tail: L[a])
       *
       * struct Tree(root: a, children: L[Tree[a]])
       *
       * Tree[a] -> + L[Tree[a]] => Tree -> + L, L -> + Tree
       */
      def tpeToEdge(
        src: DefinedType[Variance],
        v: Variance,
        t: Type): List[(DefinedType[Variance], Variance, DefinedType[Variance])] = {
          import Type._
          t match {
            case Fun(a, b) =>
              tpeToEdge(src, v * Variance.contra, a) :::
                tpeToEdge(src, v * Variance.co, b)
            case TyConst(dest@Const.Defined(_, _)) =>
              getDT(dest) match {
                case None => Nil
                case Some(destDT) => (src, v, destDT) :: Nil
              }
            case TyVar(_) | TyMeta(_) => Nil
            case rest@(ForAll(_, _) | TyApply(_, _)) =>
              def build(t: Type, targs: List[Type]): (Option[DefinedType[Variance]], List[(Variance, Type)]) = {
                val (left, newArgs) = Type.applicationArgs(t)
                val args = newArgs ::: targs
                lazy val worstCaseArgs = args.map((Variance.in, _))
                left match {
                  case FnType => (None, List(Variance.contra, Variance.co).zip(args))
                  case TyConst(dt@Const.Defined(_, _)) =>
                    getDT(dt) match {
                      case None =>
                        (None, worstCaseArgs)
                      case s@Some(dt) =>
                        val vars = dt.annotatedTypeParams.map(_._2)
                        (s, vars.zip(args))
                    }
                  case ForAll(_, fa) =>
                    build(fa, args)
                  case _ => (None, worstCaseArgs)
                }
              }

              val (dest, vargs) = build(rest, Nil)
              val next = vargs.flatMap { case (v1, a) => tpeToEdge(src, v * v1, a) }
              dest.fold(next) { dtDest =>
                (dt, v, dtDest) :: next
              }
          }
        }

      for {
        cons <- dt.constructors
        argType <- cons.args
        (_, tpe) = argType
        edge <-tpeToEdge(dt, Variance.co, tpe)
      } yield edge

    }

    // Here we are just checking if we can find a loop from dt to dt that is covariant
    def checkSelfRecursions(dt: DefinedType[Variance]): ValidatedNel[NonEmptyList[DefinedType[Variance]], Unit] = {
      val graph = buildEdges(dt)
        .groupBy(_._1)
        .iterator
        .map { case (src, edges) =>
          (src, edges.map { case (_, v, r) => (v, r) })
        }
        .toMap

      val loops = Paths.allCycles(dt)(graph.getOrElse(_, Nil))
      // filter bad loops:
      val bad = loops.filter { loop =>
        val variance =
          loop.toList
            .iterator
            .map(_._1)
            .reduce(_ * _) // reduce is safe since we have a non-empty list
        (variance == Variance.in) || (variance == Variance.contra)
      }

      NonEmptyList.fromList(bad) match {
        case None => Validated.valid(())
        case Some(badPaths) =>
          Validated.invalid(badPaths.map { path => dt :: path.map(_._2) })
      }
    }

    val assertRecursions =
      packageDefinedTypes.traverse_(checkSelfRecursions _)

    assertDag *> assertRecursions

  }
}
