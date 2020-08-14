package org.bykn.bosatsu

import scala.concurrent.ExecutionContext

import Identifier.Bindable

import cats.implicits._

object MatchlessFromTypedExpr {
  // compile a set of packages given a set of external remappings
  def compile[A](pm: PackageMap.Typed[A])(implicit ec: ExecutionContext): Map[PackageName, List[(Bindable, Matchless.Expr)]] = {

    val gdr = pm.getDataRepr

    val allItems = pm.toMap
      .toList
      .traverse { case (pname, pack) =>
        val lets = pack.program.lets

        Par.start {
          val exprs: List[(Bindable, Matchless.Expr)] =
            rankn.RefSpace
              .allocCounter
              .flatMap { c =>
                lets
                  .traverse {
                    case (name, rec, te) =>
                      Matchless.fromLet(name, rec, te, gdr, c)
                       .map((name, _))
                  }
            }
            .run
            .value

          (pname, exprs)
        }
      }
      .map(_.toMap)

    Par.await(allItems)
  }
}
