package org.bykn.bosatsu

import Identifier.Bindable

import cats.implicits._

object MatchlessFromTypedExpr {
  type Compiled[+A] = Map[PackageName, List[(Bindable, Matchless.Expr[A])]]
  // compile a set of packages given a set of external remappings
  def compile[K, A](
      from: K,
      pm: PackageMap.Typed[A]
  )(implicit ec: Par.EC): Compiled[K] = {

    val gdr = pm.getDataRepr

    // on JS Par.F[A] is actually Id[A], so we need to hold hands a bit

    val allItemsList = pm.toMap.toList
      .traverse[Par.F, (PackageName, List[(Bindable, Matchless.Expr[K])])] {
        case (pname, pack) =>
          val lets = pack.lets

          Par.start {
            val exprs: List[(Bindable, Matchless.Expr[K])] =
              rankn.RefSpace.allocCounter
                .flatMap { c =>
                  lets
                    .traverse { case (name, rec, te) =>
                      // TODO: add from so we can resolve packages correctly
                      Matchless
                        .fromLet(from, name, rec, te, gdr, c)
                        .map((name, _))
                    }
                }
                .run
                .value

            (pname, exprs)
          }
      }

    // JS needs this to not see through the Par.F as Id
    // really we want Par.F to be an opaque type
    val allItems = cats.Functor[Par.F].map(allItemsList)(_.toMap)

    Par.await(allItems)
  }
}
