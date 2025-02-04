package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.StateT
import cats.syntax.all._
import org.bykn.bosatsu.hashing.{Algo, Hashed, HashValue}
import org.bykn.bosatsu.tool.CliException
import org.bykn.bosatsu.{Package, PackageName, PackageMap, ProtoConverter}
import scala.collection.immutable.{SortedMap, SortedSet}
import org.typelevel.paiges.Doc

case class DecodedLibrary[A](
    name: Name,
    version: Version,
    hashValue: HashValue[A],
    protoLib: proto.Library,
    interfaces: List[Package.Interface],
    implementations: PackageMap.Typed[Unit]
) {
  lazy val publicPackageNames: SortedSet[PackageName] =
    interfaces.iterator.map(_.name).to(SortedSet)
}

object DecodedLibrary {
  def decode[F[_], A: Algo](
      protoLib: Hashed[A, proto.Library]
  )(implicit F: MonadError[F, Throwable]): F[DecodedLibrary[A]] =
    F.fromTry(
      ProtoConverter
        .packagesFromProto(
          protoLib.arg.exportedIfaces,
          protoLib.arg.internalPackages
        )
    ).flatMap { case (ifs, impls) =>
      // TODO: should verify somewhere that all the package names are distinct, but since this is presumed to be
      // a good library maybe that's a waste

      protoLib.arg.descriptor.flatMap(_.version) match {
        case Some(protoV) =>
          F.pure(
            DecodedLibrary[A](
              Name(protoLib.arg.name),
              Version.fromProto(protoV),
              protoLib.hash,
              protoLib.arg,
              ifs,
              PackageMap(
                impls.iterator.map(pack => (pack.name, pack)).to(SortedMap)
              )
            )
          )
        case None =>
          F.raiseError(
            CliException(
              "missing version",
              Doc.text(
                show"while decoding library ${protoLib.arg.name} with hash ${protoLib.hash.toIdent} has missing version."
              )
            )
          )
      }
    }
}

case class DecodedLibraryWithDeps[A](
    lib: DecodedLibrary[A],
    deps: SortedMap[(Name, Version), DecodedLibraryWithDeps[A]]
) {
  def name: Name = lib.name
  def version: Version = lib.version
}

object DecodedLibraryWithDeps {
  def decodeAll[F[_]](
      protoLib: Hashed[Algo.Blake3, proto.Library]
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
        protoLib: Hashed[Algo.Blake3, proto.Library]
    ): Cached[Value] =
      for {
        root <- StateT.liftF(DecodedLibrary.decode[F, Algo.Blake3](protoLib))
        deps <-
          (protoLib.arg.privateDependencies.toList ::: protoLib.arg.publicDependencies.toList)
            .traverse(fetchDep(_))
        depMap = deps.iterator
          .map(dec => (dec.name, dec.version) -> dec)
          .to(SortedMap)
        result = DecodedLibraryWithDeps(root, depMap)
        _ <- StateT.modify[F, S](
          _.updated((root.name, root.version), result)
        )
      } yield result

    def fetchDep(dep: proto.LibDependency): Cached[Value] = {
      val fV = dep.desc.flatMap(_.version) match {
        case Some(v) => F.pure(Version.fromProto(v))
        case None =>
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
          case None =>
            StateT.liftF(load(dep)).flatMap(decodeAndStore(_))
        }
      } yield res
    }

    decodeAndStore(protoLib).runA(Map.empty)
  }
}
