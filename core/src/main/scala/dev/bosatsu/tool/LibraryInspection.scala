package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.bosatsu.{Bytes, Json, Kind, PackageName, PackageSignatures, PlatformIO}
import dev.bosatsu.graph.CanPromise
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{
  DecodedLibrary,
  DecodedLibraryWithDeps,
  LibraryDependencyVisibility,
  LibraryIdentity,
  Name,
  VerifiedLibraryGraph,
  VerifiedLibrarySnapshot,
  Version,
  libraryIdentityOrdering
}
import dev.bosatsu.tool.PackageBindings.{
  PublicIndex,
  PublicPackage,
  PublicProvider
}
import scala.collection.immutable.SortedMap
import scala.util.Try

/** Caller-declared files available to a full library inspection. The same
  * identity may have multiple paths so the verifier can authenticate aliases
  * and reject conflicting bytes instead of silently selecting one.
  */
private[bosatsu] object LibraryInspection {
  final case class DeclaredLibraryFile[Path](
      name: Name,
      version: Version,
      path: Path,
      hashes: List[String]
  )

  final case class DependencyContext[Path](libraries: List[DeclaredLibraryFile[Path]])

  /** Authenticated imported inputs shared by graph validation and compilation.
    * Signature bundles are the exact parsed bytes checked against the
    * authenticated public projections, keyed by their declared library.
    */
  final case class ImportedInputs[Path](
      graph: Option[VerifiedLibraryGraph[Path]],
      publicIndex: PublicIndex,
      signatureBundles: SortedMap[LibraryIdentity, proto.Interfaces]
  )

  private final case class RawLibraryFile(
      name: Name,
      version: Version,
      path: String,
      hashes: List[String]
  )

  private final case class RawDependencyContext(libraries: List[RawLibraryFile])


  private given Ordering[LibraryIdentity] = libraryIdentityOrdering

  private final case class DescriptorEntry(
      visibility: LibraryDependencyVisibility,
      identity: LibraryIdentity,
      hashes: List[String],
      uris: List[String]
  )

  private def visibilityName(visibility: LibraryDependencyVisibility): String =
    visibility match {
      case LibraryDependencyVisibility.Public                 => "public"
      case LibraryDependencyVisibility.Private                => "private"
      case LibraryDependencyVisibility.UnusedTransitivePublic => "unused_transitive_public"
    }

  private def identityJson(identity: LibraryIdentity): Json =
    Json.JObject(List(
      "name" -> Json.JString(identity._1.name),
      "version" -> Json.JString(identity._2.render)
    ))

  private def stringsJson(values: IterableOnce[String]): Json =
    Json.JArray(values.iterator.map(Json.JString(_)).toVector)

  private def identitiesJson(values: Iterable[LibraryIdentity]): Json =
    Json.JArray(values.iterator.map(identityJson).toVector)

  private def compareStrings(left: List[String], right: List[String]): Int = {
    @annotation.tailrec
    def loop(leftRest: List[String], rightRest: List[String]): Int =
      (leftRest, rightRest) match {
        case (Nil, Nil) => 0
        case (Nil, _)   => -1
        case (_, Nil)   => 1
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          val comparison = leftHead.compareTo(rightHead)
          if (comparison == 0) loop(leftTail, rightTail) else comparison
      }
    loop(left, right)
  }

  private def descriptorJson(snapshot: VerifiedLibrarySnapshot[?]): Json =
    snapshot.protoLib.descriptor match {
      case None => Json.JNull
      case Some(value) => Json.JObject(List(
        "version" -> value.version.fold[Json](Json.JNull)(_ =>
          Json.JString(snapshot.version.render)
        ),
        "hashes" -> stringsJson(value.hashes),
        "uris" -> stringsJson(value.uris)
      ))
    }

  private def dependencyEntries(
      library: proto.Library,
      identity: LibraryIdentity
  ): Either[String, List[DescriptorEntry]] = {
    val dependencies =
      library.publicDependencies.toList.map(LibraryDependencyVisibility.Public -> _) :::
        library.privateDependencies.toList.map(LibraryDependencyVisibility.Private -> _) :::
        library.unusedTransitivePublicDependencies.toList.map(
          LibraryDependencyVisibility.UnusedTransitivePublic -> _
        )

    dependencies.traverse { case (visibility, dependency) =>
      val owner = s"${identity._1.name} ${identity._2.render}"
      for {
        descriptor <- dependency.desc.toRight(
          s"missing descriptor for dependency '${dependency.name}' of $owner"
        )
        version <- descriptor.version.toRight(
          s"missing version for dependency '${dependency.name}' of $owner"
        )
        targetVersion <- Version.fromProtoChecked(version).toRight(
          s"invalid version for dependency '${dependency.name}' of $owner"
        )
        _ <- Either.cond(
          descriptor.hashes.nonEmpty,
          (),
          s"missing expected hashes for dependency '${dependency.name}' of $owner"
        )
      } yield DescriptorEntry(
        visibility,
        (Name(dependency.name), targetVersion),
        descriptor.hashes.toList,
        descriptor.uris.toList
      )
    }.map(_.sortWith { (left, right) =>
      val visibility = left.visibility.rank.compare(right.visibility.rank)
      if (visibility != 0) visibility < 0
      else {
        val identity = summon[Ordering[LibraryIdentity]].compare(
          left.identity,
          right.identity
        )
        if (identity != 0) identity < 0
        else {
          val hashes = compareStrings(left.hashes, right.hashes)
          if (hashes != 0) hashes < 0
          else compareStrings(left.uris, right.uris) < 0
        }
      }
    })
  }

  /** Version-1 file-only descriptor metadata for an already authenticated
    * root. This deliberately inspects only the root snapshot; callers may use
    * the returned descriptors to acquire children before constructing a full
    * graph.
    */
  def descriptorProjection[Path](
      snapshot: VerifiedLibrarySnapshot[Path]
  ): Either[String, Json] = {
    val identity = (snapshot.name, snapshot.version)
    for {
      entries <- dependencyEntries(snapshot.protoLib, identity)
    } yield Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "identity" -> identityJson(identity),
      "digests" -> Json.JObject(List(
        "sha256" -> Json.JString(snapshot.sha256.hex),
        "blake3" -> Json.JString(snapshot.blake3.hex)
      )),
      "descriptor" -> descriptorJson(snapshot),
      "dependencies" -> Json.JArray(entries.iterator.map { entry =>
        Json.JObject(List(
          "identity" -> identityJson(entry.identity),
          "visibility" -> Json.JString(visibilityName(entry.visibility)),
          "hashes" -> stringsJson(entry.hashes),
          "uris" -> stringsJson(entry.uris)
        ))
      }.toVector)
    ))
  }

  /** The existing protobuf signature format is the public interface
    * projection. It intentionally contains no artifact paths, hashes, or
    * implementation data.
    */
  def signatureProjection(
      library: DecodedLibrary[Algo.Blake3]
  ): Try[proto.Interfaces] =
    PackageSignatures.encode(library.interfaces)

  private def providerPaths[Path](
      graph: VerifiedLibraryGraph[Path],
      decoded: SortedMap[LibraryIdentity, DecodedLibrary[Algo.Blake3]],
      referring: LibraryIdentity,
      packageName: PackageName
  ): List[(LibraryIdentity, List[LibraryIdentity])] = {
    @annotation.tailrec
    def loop(
        pending: List[(LibraryIdentity, List[LibraryIdentity])],
        seen: Set[LibraryIdentity],
        providers: SortedMap[LibraryIdentity, List[LibraryIdentity]]
    ): SortedMap[LibraryIdentity, List[LibraryIdentity]] =
      pending match {
        case Nil => providers
        case (current, _) :: rest if seen(current) =>
          loop(rest, seen, providers)
        case (current, witness) :: rest =>
          val nextProviders =
            if (decoded.get(current).exists(_.interfaces.exists(_.name == packageName)))
              providers.updated(current, witness)
            else providers
          val next = graph.dependencyEdges(current).iterator
            .filter(_.visibility != LibraryDependencyVisibility.Private)
            .map(edge => edge.targetIdentity -> (witness :+ edge.targetIdentity))
            .toList
          loop(rest ::: next, seen + current, nextProviders)
      }

    loop(List(referring -> List(referring)), Set.empty, SortedMap.empty).toList
  }

  /** Compute typed public package ownership in each decoded scope. Public and
    * legacy unused-transitive-public edges are followed; private implementation
    * edges remain present only in the implementation projection.
    */
  def publicIndex[Path](
      graph: VerifiedLibraryGraph[Path],
      decoded: SortedMap[LibraryIdentity, DecodedLibrary[Algo.Blake3]],
      pathText: Path => String
  ): Either[String, PublicIndex] = {
    def oneLibrary(identity: LibraryIdentity): Either[String, List[PublicPackage]] =
      decoded.get(identity).toRight(
        s"missing decoded library ${identity._1.name} ${identity._2.render}"
      ).flatMap { library =>
        library.interfaces.sortBy(_.name.asString).traverse { iface =>
          val exposed = iface.visibleDepPackages[Kind.Arg]
            .filterNot(_ == iface.name)
            .distinct
            .sorted
          exposed.traverse { packageName =>
            if (packageName == PackageName.PredefName) {
              Right(packageName -> PublicProvider.CompilerPredef)
            } else {
              val providers = providerPaths(graph, decoded, identity, packageName)
              val sourcePath = graph.snapshot(identity).fold(identity._1.name) { snapshot =>
                pathText(snapshot.path)
              }
              providers match {
                case Nil => Left(
                  s"missing provider for exposed package ${packageName.asString} " +
                    s"from ${identity._1.name} ${identity._2.render} at $sourcePath"
                )
                case (provider, witness) :: Nil =>
                  Right(packageName -> PublicProvider.Library(provider, witness))
                case many =>
                  val details = many.map { case (provider, witness) =>
                    val path = witness.map(id => s"${id._1.name} ${id._2.render}").mkString(" -> ")
                    s"${provider._1.name} ${provider._2.render} via $path"
                  }.mkString("; ")
                  Left(
                    s"ambiguous providers for exposed package ${packageName.asString} " +
                      s"from ${identity._1.name} ${identity._2.render} at $sourcePath: $details"
                  )
              }
            }
          }.map { edges =>
            PublicPackage(
              identity,
              iface.name,
              SortedMap.from(edges)(using summon[Ordering[PackageName]])
            )
          }
        }
      }

    graph.identities.traverse(oneLibrary).map { entries =>
      PackageBindings.indexFromPackages(entries.flatten, graph.identities)
    }
  }

  /** Preserve the established version-1 exposure projection while exposing
    * its typed records to scoped binding validation.
    */
  def exposureProjection[Path](
      graph: VerifiedLibraryGraph[Path],
      decoded: SortedMap[LibraryIdentity, DecodedLibrary[Algo.Blake3]],
      pathText: Path => String
  ): Either[String, Json] =
    publicIndex(graph, decoded, pathText).map(_.toJson)

  /** List package membership independently from both public signatures and
    * full artifact references.
    */
  def membershipProjection[Path](
      graph: VerifiedLibraryGraph[Path],
      decoded: SortedMap[LibraryIdentity, DecodedLibrary[Algo.Blake3]]
  ): Either[String, Json] =
    graph.identities.traverse { identity =>
      decoded.get(identity).toRight(
        s"missing decoded library ${identity._1.name} ${identity._2.render}"
      ).map { library =>
        Json.JObject(List(
          "identity" -> identityJson(identity),
          "internal_packages" -> stringsJson(
            library.internalPackageNames.iterator.map(_.asString)
          ),
          "exported_packages" -> stringsJson(
            library.publicPackageNames.iterator.map(_.asString)
          )
        ))
      }
    }.map { entries =>
      Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"),
        "libraries" -> Json.JArray(entries.toVector)
      ))
    }

  /** Reference the verified source artifacts and every scoped edge rather
    * than defining a second codec for library implementations.
    */
  def implementationProjection[Path](
      graph: VerifiedLibraryGraph[Path],
      pathText: Path => String
  ): Json = {
    val artifacts = graph.identities.map { identity =>
      val snapshot = graph.snapshot(identity).get
      val paths = graph.aliases(identity).map(s => pathText(s.path)).distinct.sorted
      Json.JObject(List(
        "identity" -> identityJson(identity),
        "sha256" -> Json.JString(snapshot.sha256.hex),
        "blake3" -> Json.JString(snapshot.blake3.hex),
        "paths" -> stringsJson(paths)
      ))
    }
    val edges = graph.edges.map { edge =>
      Json.JObject(List(
        "from" -> identityJson(edge.referringIdentity),
        "to" -> identityJson(edge.targetIdentity),
        "visibility" -> Json.JString(visibilityName(edge.visibility)),
        "hashes" -> stringsJson(edge.expectedHashes),
        "uris" -> stringsJson(edge.mirrorUris),
        "witness_path" -> identitiesJson(edge.witnessPath)
      ))
    }
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "root" -> identityJson(graph.rootIdentity),
      "artifacts" -> Json.JArray(artifacts.toVector),
      "edges" -> Json.JArray(edges.toVector)
    ))
  }

  private val rawLibraryFileReader: Json.Reader[RawLibraryFile] =
    BuildManifest.obj("declared library file") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from,
          "declared library file",
          Set("name", "version", "path", "hashes")
        )
        name <- from.field[Name]("name")(using BuildManifest.libraryName)
        version <- from.field[Version]("version")
        path <- from.field[String]("path")(using BuildManifest.nonEmpty)
        hashes <- from.field[List[String]]("hashes")(
          using Json.Reader.listReader(using BuildManifest.nonEmpty)
        )
        _ <- Either.cond(hashes.nonEmpty, (),
          ("expected at least one recorded hash", from.j, from.path.key("hashes")))
      } yield RawLibraryFile(name, version, path, hashes)
    }

  private val rawContextReader: Json.Reader[RawDependencyContext] =
    BuildManifest.obj("library dependency context") { from =>
      for {
        _ <- BuildManifest.version1(from)
        _ <- BuildManifest.onlyFields(
          from,
          "library dependency context",
          Set("schema_version", "libraries")
        )
        libraries <- from.field[List[RawLibraryFile]]("libraries")(
          using Json.Reader.listReader(using rawLibraryFileReader)
        )
      } yield RawDependencyContext(libraries)
    }

  /** Read only the context JSON. Paths are interpreted by the platform's
    * argument parser; the declarations do not trigger file reads or fetching.
    */
  def readDependencyContext[F[_], Path](
      platform: PlatformIO[F, Path],
      file: Path
  ): F[DependencyContext[Path]] = {
    import platform.{moduleIOMonad, showPath}

    BuildManifest.read(platform, file, rawContextReader).flatMap { raw =>
      raw.libraries.zipWithIndex.traverse { case (library, index) =>
        moduleIOMonad.fromEither(
          platform.path(library.path).toEither.leftMap { errors =>
            CliException.Basic(
              show"invalid dependency path '${library.path}' in $file at " +
                s"/libraries[$index].path: ${errors.toList.mkString(", ")}"
            )
          }.map { path =>
            DeclaredLibraryFile(library.name, library.version, path, library.hashes)
          }
        )
      }.map(DependencyContext(_))
    }
  }

  /** Authenticate all imported scopes and retain the signature bundles that
    * were checked against their authenticated projections. Consumers must use
    * these parsed bundles rather than reopening the signature paths after
    * verification.
    */
  def loadImportedInputs[F[_], Path](
      platform: PlatformIO[F, Path],
      scopeMap: PackageBindings.ScopeMap[Path],
      dependencyContext: DependencyContext[Path],
      toolchain: ToolchainManifest
  )(using CanPromise[F]): F[ImportedInputs[Path]] = {
    import platform.{moduleIOMonad, showPath}

    val importedIdentities = scopeMap.imports.keysIterator.toList
    val roots = importedIdentities.traverse { identity =>
      val declared = dependencyContext.libraries.filter(library =>
        summon[Ordering[LibraryIdentity]].equiv((library.name, library.version), identity)
      )
      if (declared.isEmpty)
        moduleIOMonad.raiseError[List[VerifiedLibrarySnapshot[Path]]](
          CliException.Basic(
            s"missing declared library artifact for ${identity._1.name} ${identity._2.render}"
          )
        )
      else
        declared.traverse { library =>
          VerifiedLibrarySnapshot.verify(
            platform,
            library.path,
            library.name,
            library.version,
            library.hashes
          )
        }
    }.map(_.flatten)

    roots.flatMap {
      case Nil =>
        val empty = PackageBindings.indexFromPackages(Nil, Nil)
        moduleIOMonad.pure(ImportedInputs(
          None,
          empty,
          SortedMap.empty[LibraryIdentity, proto.Interfaces]
        ))
      case first :: rest =>
        val nonEmptyRoots = NonEmptyList(first, rest)
        for {
          graph <- VerifiedLibraryGraph.verifyRoots(platform, nonEmptyRoots, dependencyContext)
          (_, decoded) <- DecodedLibraryWithDeps.decodeVerifiedGraphWithLibraries(graph)
          publicIndex <- moduleIOMonad.fromEither(
            LibraryInspection.publicIndex(graph, decoded, platform.pathToString)
              .leftMap(CliException.Basic(_))
          )
          _ <- moduleIOMonad.fromEither(
            ToolchainManifest.validateCoreReferences(graph, toolchain.core)
              .leftMap(CliException.Basic(_))
          )
          signatureBundles <- scopeMap.imports.valuesIterator.toList.traverse { imported =>
            decoded.get(imported.identity) match {
              case None =>
                moduleIOMonad.raiseError[(LibraryIdentity, proto.Interfaces)](CliException.Basic(
                  s"imported scope ${imported.identity._1.name} ${imported.identity._2.render} " +
                    "is absent from the authenticated graph"
                ))
              case Some(library) =>
                moduleIOMonad.fromTry(LibraryInspection.signatureProjection(library)).flatMap { expected =>
                  platform.readBytes(imported.signature.path).adaptError { case error =>
                    CliException.Basic(
                      show"cannot read imported interface ${imported.signature.path}: ${error.getMessage}"
                    )
                  }.flatMap { bytes =>
                    moduleIOMonad.fromTry(Try(proto.Interfaces.parseFrom(bytes.unsafeReadOnlyView))).flatMap { actual =>
                      if (Bytes.fromMessage(actual).sameBytes(Bytes.fromMessage(expected)))
                        moduleIOMonad.pure(imported.identity -> actual)
                      else moduleIOMonad.raiseError(CliException.Basic(
                        show"imported interface ${imported.signature.path} does not match " +
                          s"${imported.identity._1.name} ${imported.identity._2.render}"
                      ))
                    }
                  }
                }
            }
          }
        } yield ImportedInputs(
          Some(graph),
          publicIndex,
          SortedMap.from(signatureBundles)
        )
    }
  }
}
