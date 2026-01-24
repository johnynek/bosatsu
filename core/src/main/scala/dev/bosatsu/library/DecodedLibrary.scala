package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.syntax.all._
import dev.bosatsu.hashing.{Algo, Hashed, HashValue}
import dev.bosatsu.tool.CliException
import dev.bosatsu.{
  Kind,
  Package,
  PackageName,
  PackageMap,
  ProtoConverter,
  Referant
}
import scala.collection.immutable.{SortedMap, SortedSet}
import org.typelevel.paiges.Doc

case class DecodedLibrary[A](
    name: Name,
    version: Version,
    hashValue: HashValue[A],
    protoLib: proto.Library,
    interfaces: List[Package.Interface],
    implementations: PackageMap.Typed[Any]
) {
  lazy val interfaceMap: PackageMap.Interface =
    PackageMap.fromIterable[Nothing, Nothing, Referant[Kind.Arg], Unit](
      interfaces
    )

  lazy val publicPackageNames: SortedSet[PackageName] =
    interfaceMap.toMap.keySet

  lazy val toHashed: Hashed[A, proto.Library] = Hashed(hashValue, protoLib)
}

object DecodedLibrary {

  def versionOf[A: Algo](
      protoLib: Hashed[A, proto.Library]
  ): Either[Throwable, Version] =
    protoLib.arg.descriptor.flatMap(_.version) match {
      case Some(protoV) => Right(Version.fromProto(protoV))
      case None         =>
        Left(
          CliException(
            "missing version",
            Doc.text(
              show"while decoding library ${protoLib.arg.name} with hash ${protoLib.hash.toIdent} has missing version."
            )
          )
        )
    }

  def decode[F[_], A: Algo](
      protoLib: Hashed[A, proto.Library]
  )(implicit F: MonadError[F, Throwable]): F[DecodedLibrary[A]] =
    for {
      ifsImpls <- F.fromTry(
        ProtoConverter
          .packagesFromProto(
            protoLib.arg.exportedIfaces,
            protoLib.arg.internalPackages
          )
      )
      (ifs, impls) = ifsImpls
      // TODO: should verify somewhere that all the package names are distinct, but since this is presumed to be
      // a good library maybe that's a waste
      version <- F.fromEither(versionOf(protoLib))
    } yield DecodedLibrary[A](
      Name(protoLib.arg.name),
      version,
      protoLib.hash,
      protoLib.arg,
      ifs,
      PackageMap(
        impls.iterator.map(pack => (pack.name, pack)).to(SortedMap)
      )
    )
}
