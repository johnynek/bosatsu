package dev.bosatsu.codegen

import cats.{Order, Show}
import cats.data.NonEmptyList
import dev.bosatsu.{
  Identifier,
  MatchlessFromTypedExpr,
  PackageName,
  PackageMap,
  Par
}
import dev.bosatsu.rankn.Type
import scala.collection.immutable.{SortedMap, SortedSet}

import cats.syntax.all._

trait CompilationSource[A] { self =>
  type ScopeKey
  def namespace(a: A): CompilationNamespace[ScopeKey]
}

object CompilationSource {

  def apply[A](implicit cs: CompilationSource[A]): CompilationSource[A] {
    type ScopeKey = cs.ScopeKey
  } = cs

  def namespace[A, S](a: A)(implicit
      cs: CompilationSource[A] { type ScopeKey = S }
  ): CompilationNamespace[S] =
    cs.namespace(a)

  implicit def packageMapSrc[A](implicit
      ec: Par.EC
  ): CompilationSource[PackageMap.Typed[A]] { type ScopeKey = Unit } =
    new CompilationSource[PackageMap.Typed[A]] { self =>
      type ScopeKey = Unit

      def namespace(pm: PackageMap.Typed[A]): CompilationNamespace[Unit] =
        new CompilationNamespace[Unit] {
          implicit val keyOrder: Ordering[ScopeKey] = new Ordering[Unit] {
            def compare(x: Unit, y: Unit): Int = 0
          }
          given Order[ScopeKey] = Order.fromOrdering(using keyOrder)
          val keyShow: Show[ScopeKey] = Show.show(_ => "root")

          def identOf(k: Unit, pn: PackageName): NonEmptyList[String] = pn.parts
          def depFor(src: Unit, pn: PackageName): Unit = ()
          def rootKey: Unit = ()

          lazy val compiled = SortedMap(
            () -> MatchlessFromTypedExpr.compile((), pm)
          )

          lazy val topoSort = pm.topoSort.map(p => ((), p))

          lazy val testValues = pm.testValues

          def mainValues(
              mainTypeFn: Type => Boolean
          ): Map[PackageName, (Identifier.Bindable, Type)] =
            pm.toMap.iterator.flatMap { case (n, p) =>
              val optEval = p.lets.findLast { case (_, _, te) =>
                // TODO this should really e checking that te.getType <:< a key (https://github.com/johnynek/bosatsu/issues/430)
                // in the map.
                mainTypeFn(te.getType)
              }
              optEval.map { case (b, _, te) =>
                (n, (b, te.getType))
              }
            }.toMap

          lazy val externals: SortedMap[ScopeKey, Map[PackageName, List[
            (Identifier.Bindable, Type)
          ]]] =
            SortedMap(() -> pm.allExternals)

          def treeShake(
              roots: Set[(PackageName, Identifier)]
          ): CompilationNamespace[Unit] =
            self.namespace(PackageMap.treeShake(pm, roots))

          def rootPackages: SortedSet[PackageName] = pm.toMap.keySet
        }
    }
}
