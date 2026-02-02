package dev.bosatsu.codegen

import cats.Show
import cats.data.NonEmptyList
import dev.bosatsu.{PackageName, Identifier, MatchlessFromTypedExpr}
import dev.bosatsu.rankn.Type
import dev.bosatsu.graph.Toposort
import scala.collection.immutable.{SortedMap, SortedSet}

trait CompilationNamespace[K] {
  implicit def keyOrder: Ordering[K]
  def keyShow: Show[K]

  def identOf(k: K, pn: PackageName): NonEmptyList[String]
  def depFor(src: K, pn: PackageName): K
  def rootKey: K

  def isRoot(k: K): Boolean = keyOrder.equiv(k, rootKey)

  def globalIdent(k: K, pn: PackageName): NonEmptyList[String] =
    identOf(depFor(k, pn), pn)

  def topoSort: Toposort.Result[(K, PackageName)]
  def compiled: SortedMap[K, MatchlessFromTypedExpr.Compiled[K]]
  def testValues: Map[PackageName, Identifier.Bindable]
  def mainValues(
      mainTypeFn: Type => Boolean
  ): Map[PackageName, (Identifier.Bindable, Type)]
  def externals
      : SortedMap[K, Map[PackageName, List[(Identifier.Bindable, Type)]]]
  def treeShake(roots: Set[(PackageName, Identifier)]): CompilationNamespace[K]

  def rootPackages: SortedSet[PackageName]
}
