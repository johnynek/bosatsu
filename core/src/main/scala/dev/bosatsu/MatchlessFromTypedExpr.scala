package dev.bosatsu

import Identifier.{Bindable, Constructor}
import cats.Order

import cats.implicits._

object MatchlessFromTypedExpr {
  type Compiled[+A] = Map[PackageName, List[(Bindable, Matchless.Expr[A])]]
  // compile a set of packages given a set of external remappings
  def compileRaw[K: Order, A](
      from: K,
      pm: PackageMap.Typed[A]
  )(implicit ec: Par.EC): Compiled[K] =
    Par.await(startCompileRaw(from, pm))

  /** Start compileRaw's per-package tasks without waiting for them. Callers
    * compiling several package maps can start them all and await once, rather
    * than awaiting inside a task on the same pool.
    */
  private[bosatsu] def startCompileRaw[K: Order, A](
      from: K,
      pm: PackageMap.Typed[A]
  )(implicit ec: Par.EC): Par.F[Compiled[K]] = {

    val gdr = pm.getDataRepr
    val ifaceTypeEnvs: Map[PackageName, rankn.TypeEnv[Kind.Arg]] =
      pm.toMap.iterator
        .flatMap { case (_, pack) =>
          pack.imports.iterator.map(_.pack)
        }
        .map { iface =>
          iface.name -> ExportedName.typeEnvFromExports(
            iface.name,
            iface.exports
          )
        }
        .toMap

    // NOTE: when compiling libraries, pm contains only local implementations.
    // Dependency packages are present only as imported interfaces on each
    // package, so pm.getDataRepr cannot see their constructors. Matchless
    // needs constructor DataRepr for pattern compilation (e.g. Bosatsu/Num/Nat
    // Zero/Succ), otherwise it throws "could not find Constructor(...) in
    // global data types". We therefore fall back to constructor info derived
    // from imported interfaces to cover interface-only deps.
    val variantOf: (PackageName, Constructor) => Option[rankn.DataRepr] =
      (pn, cons) =>
        gdr(pn, cons).orElse {
          ifaceTypeEnvs
            .get(pn)
            .flatMap(_.getConstructor(pn, cons).map(_._1.dataRepr(cons)))
        }

    val allItemsList = pm.toMap.toList
      .traverse { case (pname, pack) =>
        val lets = pack.lets

        Par.start {
          val exprs: List[(Bindable, Matchless.Expr[K])] =
            rankn.RefSpace.allocCounter
              .flatMap { c =>
                lets
                  .traverse { case (name, rec, te) =>
                    Matchless
                      .fromLetRaw(from, name, rec, te, variantOf, c)
                      .map((name, _))
                  }
              }
              .run
              .value

          (pname, exprs)
        }
      }

    allItemsList.map(_.toMap)
  }

  def compile[K: Order, A](
      from: K,
      pm: PackageMap.Typed[A],
      localPassOptions: Matchless.LocalPassOptions
  )(implicit ec: Par.EC): Compiled[K] =
    cleanup(compileRaw(from, pm), localPassOptions)

  private[bosatsu] def cleanup[K: Order](raw: Compiled[K], localPassOptions: Matchless.LocalPassOptions): Compiled[K] =
    raw.view.mapValues { lets =>
      lets.map { case (name, expr) =>
        (name, Matchless.postLoweringCleanup(expr, localPassOptions))
      }
    }.toMap
}
