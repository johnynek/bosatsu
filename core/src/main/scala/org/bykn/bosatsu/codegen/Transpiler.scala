package org.bykn.bosatsu.codegen

import cats.data.NonEmptyList
import com.monovore.decline.Opts
import org.bykn.bosatsu.{
  MatchlessFromTypedExpr,
  PackageName,
  PackageMap,
  Par,
  PlatformIO
}
import org.typelevel.paiges.Doc
import scala.collection.immutable.{SortedMap, SortedSet}
import org.bykn.bosatsu.Identifier
import org.bykn.bosatsu.rankn.Type
import org.bykn.bosatsu.graph.Toposort

import cats.syntax.all._

trait Transpiler {
  type Args[F[_], P]

  // this gives the argument for reading files into strings
  // this is a bit limited, but good enough for now
  def opts[F[_], P](plat: PlatformIO[F, P]): Opts[Transpiler.Optioned[F, P]]

  // return paths to be resolved against the base output path
  def renderAll[F[_], P, S](
      outDir: P,
      pm: S,
      args: Args[F, P]
  )(implicit ec: Par.EC, CS: CompilationSource[S]): F[List[(P, Doc)]]
}

object Transpiler {
  def optioned[F[_], P](t: Transpiler)(argsP: t.Args[F, P]): Optioned[F, P] =
    new Optioned[F, P] {
      val transpiler: t.type = t
      val args = argsP
    }

  sealed abstract class Optioned[F[_], P] { self =>
    val transpiler: Transpiler
    def args: transpiler.Args[F, P]
  }
}

trait CompilationNamespace[K] {
  implicit def keyOrder: Ordering[K]

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

trait CompilationSource[A] { self =>
  type ScopeKey
  def namespace(a: A): CompilationNamespace[ScopeKey]

}

object CompilationSource {

  def apply[A](implicit cs: CompilationSource[A]): CompilationSource[A] = cs

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
                // TODO this should really e checking that te.getType <:< a key
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
