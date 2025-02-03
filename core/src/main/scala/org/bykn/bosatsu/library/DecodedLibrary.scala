package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.syntax.all._
import org.bykn.bosatsu.hashing.{Hashed, HashValue}
import org.bykn.bosatsu.{Package, PackageName, PackageMap, ProtoConverter}
import scala.collection.immutable.{SortedMap, SortedSet}

case class DecodedLibrary[A](
    hashValue: HashValue[A],
    protoLib: proto.Library,
    interfaces: List[Package.Interface],
    implementations: PackageMap.Typed[Unit]
) {
  lazy val publicPackageNames: SortedSet[PackageName] =
    interfaces.iterator.map(_.name).to(SortedSet)
}

object DecodedLibrary {
  def decode[F[_], A](
      protoLib: Hashed[A, proto.Library]
  )(implicit F: MonadError[F, Throwable]): F[DecodedLibrary[A]] =
    F.fromTry(
      ProtoConverter
        .packagesFromProto(
          protoLib.arg.exportedIfaces,
          protoLib.arg.internalPackages
        )
    ).map { case (ifs, impls) =>
      // TODO: should verify somewhere that all the package names are distinct, but since this is presumed to be
      // a good library maybe that's a waste
      DecodedLibrary[A](
        protoLib.hash,
        protoLib.arg,
        ifs,
        PackageMap(impls.iterator.map(pack => (pack.name, pack)).to(SortedMap))
      )
    }
}
