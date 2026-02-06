package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.MonadError
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
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

  def toDep(using algo: Algo[A]): proto.LibDependency = {
    val desc0 = protoLib.descriptor.getOrElse(proto.LibDescriptor())
    val depVersion = desc0.version.orElse(Some(version.toProto))
    val depHashes = (hashValue.toIdent :: desc0.hashes.toList).distinct
    proto.LibDependency(
      name = name.name,
      desc = Some(
        proto.LibDescriptor(
          version = depVersion,
          hashes = depHashes,
          uris = desc0.uris
        )
      )
    )
  }
}

object DecodedLibrary {
  enum DepClosureError {
    case MissingVersion(dep: DecodedLibrary[Algo.Blake3])
    case MissingTransitiveDep(name: Name, version: Version)
  }

  final case class PublicDepRef(
      dep: proto.LibDependency,
      name: Name,
      version: Version
  ) {
    def depKey: (Name, Version) = (name, version)
  }

  object DepClosureError {
    def toDoc(err: DepClosureError): Doc =
      err match {
        case DepClosureError.MissingVersion(dep) =>
          Doc.text(
            s"dependency ${dep.name.name} ${dep.version.render} has a public dependency without a version."
          )
        case DepClosureError.MissingTransitiveDep(name, version) =>
          Doc.text(
            s"missing transitive dependency ${name.name} ${version.render}."
          )
      }

    def toDoc(errs: NonEmptyChain[DepClosureError]): Doc =
      Doc.intercalate(Doc.line, errs.toNonEmptyList.toList.map(toDoc))
  }

  private type DepKey = (Name, Version)

  private def publicDepsOf(
      lib: DecodedLibrary[Algo.Blake3]
  ): List[proto.LibDependency] =
    lib.protoLib.publicDependencies.toList :::
      lib.protoLib.unusedTransitivePublicDependencies.toList

  def publicDepReferences(
      lib: DecodedLibrary[Algo.Blake3]
  ): ValidatedNec[DepClosureError, List[PublicDepRef]] =
    publicDepsOf(lib).traverse { dep =>
      dep.desc.flatMap(_.version) match {
        case Some(v) =>
          Validated.valid(
            PublicDepRef(dep, Name(dep.name), Version.fromProto(v))
          )
        case None    =>
          Validated.invalidNec(DepClosureError.MissingVersion(lib))
      }
    }

  def publicDepClosure(
      startLibs: List[DecodedLibrary[Algo.Blake3]],
      depMap: Map[DepKey, DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[DepClosureError, List[DecodedLibrary[Algo.Blake3]]] = {
    type SeenMap = SortedMap[DepKey, DecodedLibrary[Algo.Blake3]]

    @annotation.tailrec
    def loop(
        todo: List[DecodedLibrary[Algo.Blake3]],
        seen: SeenMap,
        errors: List[DepClosureError]
    ): (SeenMap, List[DepClosureError]) =
      todo match {
        case Nil =>
          (seen, errors)
        case lib :: rest =>
          val key = (lib.name, lib.version)
          if (seen.contains(key)) loop(rest, seen, errors)
          else {
            val seen1 = seen.updated(key, lib)
            publicDepReferences(lib) match {
              case Validated.Invalid(errs) =>
                loop(rest, seen1, errs.toNonEmptyList.toList ::: errors)
              case Validated.Valid(depRefs) =>
                val (nextTodo, nextErrors) =
                  depRefs.foldLeft((rest, errors)) {
                    case ((todo0, errs0), depRef) =>
                      val depKey = depRef.depKey
                      depMap.get(depKey) match {
                        case Some(depLib) =>
                          if (seen1.contains(depKey) || todo0.exists(d =>
                              Ordering[DepKey]
                                .equiv(
                                  (d.name, d.version),
                                  (depLib.name, depLib.version)
                                )
                            ))
                            (todo0, errs0)
                          else (depLib :: todo0, errs0)
                        case None         =>
                          (
                            todo0,
                            DepClosureError
                              .MissingTransitiveDep(depKey._1, depKey._2) :: errs0
                          )
                      }
                  }

                loop(nextTodo, seen1, nextErrors)
            }
          }
      }

    val initSeen = SortedMap.empty[DepKey, DecodedLibrary[Algo.Blake3]]
    val (closure, errs0) = loop(startLibs, initSeen, Nil)
    NonEmptyChain.fromSeq(errs0.reverse.distinct) match {
      case Some(errs) => Validated.invalid(errs)
      case None       => Validated.valid(closure.values.toList)
    }
  }

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
