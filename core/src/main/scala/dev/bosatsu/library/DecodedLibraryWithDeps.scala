package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.{NonEmptyList, StateT}
import cats.syntax.all._
import dev.bosatsu.{
  Identifier,
  MatchlessFromTypedExpr,
  PackageName,
  PackageMap,
  Par
}
import dev.bosatsu.codegen.{CompilationNamespace, CompilationSource}
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.graph.{Dag, Memoize, Toposort}
import dev.bosatsu.rankn.Type
import dev.bosatsu.tool.CliException
import org.typelevel.paiges.Doc
import scala.collection.immutable.{SortedMap, SortedSet}

case class DecodedLibraryWithDeps[A](
    lib: DecodedLibrary[A],
    deps: SortedMap[(Name, Version), DecodedLibraryWithDeps[A]]
) {
  def name: Name = lib.name
  def version: Version = lib.version

  val nameVersion: (Name, Version) = (name, version)

  def depFor(pn: PackageName): Option[DecodedLibraryWithDeps[A]] =
    if (lib.implementations.toMap.contains(pn)) Some(this)
    else
      deps.collectFirstSome { dep =>
        if (dep.lib.interfaces.exists(_.name == pn)) Some(dep)
        else None
      }

  lazy val allKeys: SortedSet[(Name, Version)] =
    deps.iterator.map(_._2.allKeys).foldLeft(SortedSet(nameVersion))(_ | _)

  def compile(implicit
      ec: Par.EC
  ): MatchlessFromTypedExpr.Compiled[(Name, Version)] =
    MatchlessFromTypedExpr.compile(nameVersion, lib.implementations)

  def filterLets(
      keep: ((Name, Version)) => Option[((PackageName, Identifier)) => Boolean]
  ): Option[DecodedLibraryWithDeps[A]] =
    keep(nameVersion).map { pred =>
      copy(
        lib = lib.copy(implementations =
          PackageMap.filterLets(lib.implementations, pred)
        ),
        deps = deps.transform((_, dep) => dep.filterLets(keep)).collect {
          case (k, Some(v)) => (k, v)
        }
      )
    }
}

object DecodedLibraryWithDeps {
  def decodeAll[F[_]](
      protoLib: Hashed[Algo.Blake3, proto.Library]
  )(load: proto.LibDependency => F[Hashed[Algo.Blake3, proto.Library]])(implicit
      F: MonadError[F, Throwable]
  ): F[DecodedLibraryWithDeps[Algo.Blake3]] =
    DecodedLibrary.decode(protoLib).flatMap(decodeAll(_)(load))

  def decodeAll[F[_]](
      dec: DecodedLibrary[Algo.Blake3]
  )(load: proto.LibDependency => F[Hashed[Algo.Blake3, proto.Library]])(implicit
      F: MonadError[F, Throwable]
  ): F[DecodedLibraryWithDeps[Algo.Blake3]] = {
    type Key = (Name, Version)
    type Value = DecodedLibraryWithDeps[Algo.Blake3]
    type S = Map[Key, Value]
    type Cached[T] = StateT[F, S, T]

    val getS: Cached[S] = StateT.get

    def get(k: Key): Cached[Option[Value]] =
      getS.map(_.get(k))

    def decodeAndStore(
        decoded: DecodedLibrary[Algo.Blake3]
    ): Cached[Value] =
      for {
        deps <-
          (decoded.protoLib.privateDependencies.toList ::: decoded.protoLib.publicDependencies.toList)
            .traverse(fetchDep(_))
        depMap = deps.iterator
          .map(dec => dec.nameVersion -> dec)
          .to(SortedMap)
        result = DecodedLibraryWithDeps(decoded, depMap)
        _ <- StateT.modify[F, S](
          _.updated(result.nameVersion, result)
        )
      } yield result

    def fetchDep(dep: proto.LibDependency): Cached[Value] = {
      val fV = dep.desc.flatMap(_.version) match {
        case Some(v) => F.pure(Version.fromProto(v))
        case None    =>
          F.raiseError[Version](
            CliException(
              "missing version",
              Doc.text(
                show"while loading dependency ${dep.name} with hashes=${dep.desc.toList.flatMap(_.hashes).mkString(",")}: missing version."
              )
            )
          )
      }

      for {
        v <- StateT.liftF[F, S, Version](fV)
        cached <- get((Name(dep.name), v))
        res <- cached match {
          case Some(d) => StateT.pure[F, S, Value](d)
          case None    =>
            StateT
              .liftF(load(dep).flatMap(DecodedLibrary.decode(_)))
              .flatMap(decodeAndStore(_))
        }
      } yield res
    }

    decodeAndStore(dec).runA(Map.empty)
  }

  implicit def decodedLibraryWithDepsCompilationSource[A](implicit
      EC: Par.EC
  ): CompilationSource[DecodedLibraryWithDeps[A]] =
    new CompilationSource[DecodedLibraryWithDeps[A]] {
      type ScopeKey = (Name, Version)

      def namespace(
          a: DecodedLibraryWithDeps[A]
      ): CompilationNamespace[ScopeKey] =
        new CompilationNamespace[ScopeKey] {
          override def toString: String =
            s"DecodedLibraryWithDepsCompilationSource($a)"

          implicit val keyOrder: Ordering[ScopeKey] =
            Ordering.Tuple2(using
              summon[Ordering[Name]],
              summon[Ordering[Version]]
            )

          val depForKey: ScopeKey => Option[DecodedLibraryWithDeps[A]] =
            Memoize.memoizeDagHashedConcurrent[ScopeKey, Option[
              DecodedLibraryWithDeps[A]
            ]] { (nameV, _) =>
              def search(
                  d: DecodedLibraryWithDeps[A]
              ): Option[DecodedLibraryWithDeps[A]] =
                if (keyOrder.equiv(d.nameVersion, nameV)) Some(d)
                else {
                  d.deps.collectFirstSome(child => search(child))
                }

              search(a)
            }

          // encoded in a way such that the front can never collide with a valid PackageName
          // which must start with an upper ident
          def keyParts(sk: ScopeKey): List[String] =
            s"_${sk._1.name}" :: sk._2.render :: Nil

          def identOf(k: ScopeKey, pn: PackageName): NonEmptyList[String] =
            if (isRoot(k)) pn.parts
            else
              pn.parts.prependList(keyParts(k))

          def depFor(src: ScopeKey, pn: PackageName): ScopeKey =
            depForKey(src).flatMap(_.depFor(pn)) match {
              case Some(value) => value.nameVersion
              case None        =>
                // This should never happen for well compiled
                // packages, for now, just returning rootKey
                rootKey
            }

          def rootKey: ScopeKey = a.nameVersion

          lazy val topoSort: Toposort.Result[(ScopeKey, PackageName)] = {
            val allPacks: SortedSet[(ScopeKey, PackageName)] = for {
              k <- a.allKeys
              dec <- depForKey(k).iterator
              pack <- dec.lib.implementations.toMap.keySet
            } yield (k, pack)

            Toposort.sort(allPacks) { case (scopeKey, pack) =>
              // What does (scopeKey, pack) depend on?
              for {
                dlwd0 <- depForKey(scopeKey).toList
                dlwd1 <- dlwd0.depFor(pack).toList
                pack <- dlwd1.lib.implementations.toMap.get(pack).toList
                depPack <- pack.allImportPacks
                depPackSrc <- dlwd1.depFor(depPack).toList
              } yield (depPackSrc.nameVersion, depPack)
            }
          }

          private def allDeps: Iterator[DecodedLibraryWithDeps[A]] =
            a.allKeys.iterator.flatMap { key =>
              depForKey(key)
            }

          lazy val compiled
              : SortedMap[ScopeKey, MatchlessFromTypedExpr.Compiled[ScopeKey]] =
            allDeps
              .map { dep =>
                (dep.nameVersion, Par.start(dep.compile))
              }
              .to(SortedMap)
              .transform((_, p) => Par.await(p))

          lazy val testValues: Map[PackageName, Identifier.Bindable] =
            a.lib.implementations.testValues

          def mainValues(
              mainTypeFn: Type => Boolean
          ): Map[PackageName, (Identifier.Bindable, Type)] =
            a.lib.implementations.toMap.iterator.flatMap { case (n, p) =>
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
            allDeps
              .map { dep =>
                (dep.nameVersion, dep.lib.implementations.allExternals)
              }
              .to(SortedMap)

          def treeShake(
              roots: Set[(PackageName, Identifier)]
          ): CompilationNamespace[ScopeKey] = {
            type Ident = (ScopeKey, (PackageName, Identifier))

            def dependency(a: Ident): Iterable[Ident] = {
              val (sk, (pn, ident)) = a
              ident match {
                case b: Identifier.Bindable =>
                  depForKey(sk).toList.flatMap { dl =>
                    dl.lib.implementations.toMap.get(pn) match {
                      case Some(pack) =>
                        pack.program._1
                          .getLet(b)
                          .toList
                          .flatMap(_._2.globals)
                          .flatMap { case globalIdent @ (pn, _) =>
                            dl.depFor(pn).map { depDl =>
                              (depDl.nameVersion, globalIdent)
                            }
                          }
                      case None => Nil
                    }
                  }
                case _ => Nil
              }
            }

            val keep
                : Map[(Name, Version), SortedSet[(PackageName, Identifier)]] =
              Dag
                .transitiveSet(roots.toList.map(i => (rootKey, i)))(dependency)
                .groupBy(_._1)
                .transform((_, vs) => vs.map(_._2))

            a.filterLets(keep.get(_)) match {
              case Some(a1) => namespace(a1)
              case None     =>
                // we have filtered everything out
                namespace(
                  DecodedLibraryWithDeps(
                    lib = a.lib.copy(
                      implementations = PackageMap.empty,
                      interfaces = Nil
                    ),
                    deps = SortedMap.empty
                  )
                )
            }
          }

          def rootPackages: SortedSet[PackageName] =
            a.lib.implementations.toMap.keySet
        }
    }
}
