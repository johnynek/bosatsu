package dev.bosatsu.codegen.clang

import cats.data.NonEmptyList
import dev.bosatsu.codegen.CompilationNamespace
import dev.bosatsu.graph.Toposort
import dev.bosatsu.rankn.Type
import dev.bosatsu.{Identifier, MatchlessFromTypedExpr, PackageName}
import scala.collection.immutable.{SortedMap, SortedSet}

class ExternalNamespaceTest extends munit.FunSuite {
  test("externals use non-versioned namespace") {
    type K = String
    val root = "root"
    val dep = "dep_v1"
    val pkg = PackageName.parts("Bosatsu", "Prog")
    val extName: Identifier.Bindable = Identifier.Name("print")
    val extType = Type.Fun(Type.StrType, Type.UnitType)

    val externalsMap
        : SortedMap[K, Map[PackageName, List[(Identifier.Bindable, Type)]]] =
      SortedMap(dep -> Map(pkg -> List((extName, extType))))

    val ns = new CompilationNamespace[K] {
      implicit val keyOrder: Ordering[K] = Ordering.String

      def identOf(k: K, pn: PackageName): NonEmptyList[String] =
        if (k == root) pn.parts else pn.parts.prependList(List(s"_$k"))

      def depFor(src: K, pn: PackageName): K = src
      def rootKey: K = root

      def topoSort: Toposort.Result[(K, PackageName)] =
        Toposort.Success(Vector.empty)
      def compiled
          : SortedMap[K, MatchlessFromTypedExpr.Compiled[K]] = SortedMap.empty
      def testValues: Map[PackageName, Identifier.Bindable] = Map.empty
      def mainValues(
          mainTypeFn: Type => Boolean
      ): Map[PackageName, (Identifier.Bindable, Type)] = Map.empty
      def externals
          : SortedMap[K, Map[PackageName, List[(Identifier.Bindable, Type)]]] =
        externalsMap
      def treeShake(
          roots: Set[(PackageName, Identifier)]
      ): CompilationNamespace[K] =
        this
      def rootPackages: SortedSet[PackageName] = SortedSet.empty
    }

    val externalsStub = new ClangGen(ns).generateExternalsStub
    assertEquals(
      externalsStub.keys.toList,
      List("bosatsu_ext_Bosatsu_l_Prog.h")
    )

    val rendered = externalsStub("bosatsu_ext_Bosatsu_l_Prog.h").render(80)
    assert(rendered.contains("___bsts_g_Bosatsu_l_Prog_l_print"))
    assert(!rendered.contains(dep))
  }
}
