package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
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
