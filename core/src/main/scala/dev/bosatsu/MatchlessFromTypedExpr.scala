package dev.bosatsu

import Identifier.{Bindable, Constructor}
import cats.Order

import cats.implicits._

object MatchlessFromTypedExpr {
  type Compiled[+A] = Map[PackageName, List[(Bindable, Matchless.Expr[A])]]
  // compile a set of packages given a set of external remappings
  def compile[K: Order, A](
      from: K,
      pm: PackageMap.Typed[A]
  )(implicit ec: Par.EC): Compiled[K] = {

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
        val sourceHashIdent =
          Package
            .sourceHashIdentOf(pack)
            .getOrElse(Matchless.SourceInfo.emptyHashIdent)

        Par.start {
          val exprs: List[(Bindable, Matchless.Expr[K])] =
            rankn.RefSpace.allocCounter
              .flatMap { c =>
                lets
                  .traverse { case (name, rec, te) =>
                    // TODO: add from so we can resolve packages correctly
                    Matchless
                      .fromLet(
                        from = from,
                        name = name,
                        rec = rec,
                        te = te,
                        sourceHashIdent = sourceHashIdent,
                        variantOf = variantOf,
                        makeAnon = c
                      )
                      .map((name, _))
                  }
              }
              .run
              .value

          (pname, exprs)
        }
      }

    val allItems = allItemsList.map(_.toMap)
    Par.await(allItems)
  }
}
