package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import dev.bosatsu.PlatformIO
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.tool.CliException
import dev.bosatsu.tool.LibraryInspection
import scala.util.Try
import scala.collection.immutable.SortedMap
import scala.collection.mutable

private[bosatsu] type LibraryIdentity = (Name, Version)

private[bosatsu] enum LibraryDependencyVisibility derives CanEqual {
  case Public, Private, UnusedTransitivePublic

  def rank: Int = this match {
    case Public                  => 0
    case Private                 => 1
    case UnusedTransitivePublic  => 2
  }
}

/** A library artifact whose exact bytes have been checked against external
  * pins.
  */
private[bosatsu] final class VerifiedLibrarySnapshot[Path] private (
    val path: Path,
    val name: Name,
    val version: Version,
    val blake3: HashValue[Algo.Blake3],
    val sha256: HashValue[Algo.Sha256],
    val protoLib: proto.Library,
    private val bytes: Array[Byte]
) {
  /** Compare aliases by their bytes, not only by their digest identifiers. */
  private[bosatsu] def hasSameBytes(other: VerifiedLibrarySnapshot[?]): Boolean =
    java.util.Arrays.equals(bytes, other.bytes)

  private[bosatsu] def authenticatePins(
      path: String,
      expectedHashes: List[String]
  ): Either[String, Unit] =
    authenticateAs(path, name, version, expectedHashes)

  private[bosatsu] def authenticateAs(
      path: String,
      expectedName: Name,
      expectedVersion: Version,
      expectedHashes: List[String]
  ): Either[String, Unit] =
    for {
      _ <- Either.cond(
        Ordering[Name].equiv(Name(protoLib.name), expectedName),
        (),
        s"pinned library $path has name '${protoLib.name}', expected '${expectedName.name}'"
      )
      actualVersion <- Library.getVersion(protoLib).toRight(
        s"pinned library $path has no version, expected ${expectedVersion.render}"
      )
      _ <- Either.cond(
        Version.versionOrder.eqv(actualVersion, expectedVersion),
        (),
        s"pinned library $path has version ${actualVersion.render}, expected ${expectedVersion.render}"
      )
      _ <- VerifiedLibrarySnapshot.checkPins(
        path,
        expectedHashes,
        blake3,
        sha256
      )
    } yield ()
}

private[bosatsu] object VerifiedLibrarySnapshot {
  private def supportedPin(
      raw: String
  ): Either[String, Option[(String, String)]] = {
    val algorithm =
      if (raw.startsWith("blake3:") || raw == "blake3") then Some("blake3")
      else if (raw.startsWith("sha256:") || raw == "sha256") then Some("sha256")
      else None

    algorithm match {
      case None => Right(None)
      case Some(name) =>
        Algo.parseIdent.parseAll(raw) match {
          case Right(parsed) => Right(Some(name -> parsed.value.hex))
          case Left(_)       => Left(s"malformed $name hash '$raw'")
        }
    }
  }

  private[bosatsu] def checkPins(
      path: String,
      expectedHashes: List[String],
      blake3: HashValue[Algo.Blake3],
      sha256: HashValue[Algo.Sha256]
  ): Either[String, Unit] = {
    val parsed = expectedHashes.traverse(supportedPin)
      .leftMap(error => s"$error for $path")
    parsed.flatMap { all =>
      val supported = all.flatten
      if (expectedHashes.isEmpty) Left(s"no expected hashes were supplied for $path")
      else if (supported.isEmpty)
        Left(s"no supported expected hashes were supplied for $path")
      else {
        supported.traverse_ { case (name, expected) =>
          val actual = if (name == "blake3") blake3.hex else sha256.hex
          Either.cond(
            expected == actual,
            (),
            s"$name pin mismatch for $path: expected $expected, actual $actual"
          )
        }
      }
    }
  }

  /** Read once, authenticate and parse the same private byte snapshot. */
  def verify[F[_], Path](
      platform: PlatformIO[F, Path],
      path: Path,
      expectedName: Name,
      expectedVersion: Version,
      expectedHashes: List[String]
  ): F[VerifiedLibrarySnapshot[Path]] = {
    import platform.{moduleIOMonad, showPath}

    platform.readBytes(path)
      .adaptError { case error =>
        CliException.Basic(show"cannot read pinned library $path: ${error.getMessage}")
      }
      .flatMap { readBytes =>
        val snapshotBytes = readBytes.clone()
        val blake3 = Algo.hashBytes[Algo.Blake3](snapshotBytes)
        val sha256 = Algo.hashBytes[Algo.Sha256](snapshotBytes)
        val pathString = showPath.show(path)
        val result = for {
          _ <- checkPins(pathString, expectedHashes, blake3, sha256)
          library <- Try(proto.Library.parseFrom(snapshotBytes)).toEither.leftMap {
            error => s"cannot parse pinned library $pathString: ${error.getMessage}"
          }
          _ <- Either.cond(
            library.name == expectedName.name,
            (),
            s"pinned library $pathString has name '${library.name}', expected '${expectedName.name}'"
          )
          actualVersion <- Library.getVersion(library).toRight(
            s"pinned library $pathString has no version, expected ${expectedVersion.render}"
          )
          _ <- Either.cond(
            Version.versionOrder.eqv(actualVersion, expectedVersion),
            (),
            s"pinned library $pathString has version ${actualVersion.render}, expected ${expectedVersion.render}"
          )
        } yield library

        moduleIOMonad.fromEither(result.leftMap(CliException.Basic(_))).map {
          library =>
            new VerifiedLibrarySnapshot(
              path,
              expectedName,
              expectedVersion,
              blake3,
              sha256,
              library,
              snapshotBytes
            )
        }
      }
  }
}

/** One authenticated incoming descriptor. The original descriptor is kept so
  * callers retain hashes and mirror URIs; `selectedFile` names the canonical
  * declared alias whose verified bytes represent this identity.
  */
private[bosatsu] final case class VerifiedLibraryEdge[Path](
    referringIdentity: LibraryIdentity,
    visibility: LibraryDependencyVisibility,
    targetIdentity: LibraryIdentity,
    descriptor: proto.LibDependency,
    expectedHashes: List[String],
    mirrorUris: List[String],
    selectedFile: Path,
    witnessPath: List[LibraryIdentity]
)

private[bosatsu] final class VerifiedLibraryGraph[Path] private (
    val rootIdentity: LibraryIdentity,
    private val nodeMap: SortedMap[
      LibraryIdentity,
      VerifiedLibraryGraph.Node[Path]
    ],
    val edges: List[VerifiedLibraryEdge[Path]],
    val decodeOrder: List[LibraryIdentity]
) {
  private val edgesBySource = edges.groupBy(_.referringIdentity)

  def identities: List[LibraryIdentity] = nodeMap.keysIterator.toList

  def snapshot(identity: LibraryIdentity): Option[VerifiedLibrarySnapshot[Path]] =
    nodeMap.get(identity).map(_.primary)

  def dependencyEdges(
      identity: LibraryIdentity
  ): List[VerifiedLibraryEdge[Path]] =
    edgesBySource.getOrElse(identity, Nil)

  private[bosatsu] def aliases(
      identity: LibraryIdentity
  ): List[VerifiedLibrarySnapshot[Path]] =
    nodeMap.get(identity).fold(List.empty[VerifiedLibrarySnapshot[Path]])(_.aliases)
}

private[bosatsu] object VerifiedLibraryGraph {
  private type Identity = LibraryIdentity
  private type Context[Path] = LibraryInspection.DependencyContext[Path]
  private type Declared[Path] = LibraryInspection.DeclaredLibraryFile[Path]

  private given Ordering[Identity] =
    Ordering.Tuple2(using summon[Ordering[Name]], summon[Ordering[Version]])

  private final case class Node[Path](
      primary: VerifiedLibrarySnapshot[Path],
      aliases: List[VerifiedLibrarySnapshot[Path]],
      witness: List[Identity]
  )

  private final case class PendingEdge(
      visibility: LibraryDependencyVisibility,
      descriptor: proto.LibDependency,
      target: Identity,
      hashes: List[String],
      uris: List[String]
  )

  private final case class BuildState[Path](
      queue: List[Identity],
      nodes: SortedMap[Identity, Node[Path]],
      edges: List[VerifiedLibraryEdge[Path]]
  )

  private def identityText(identity: Identity): String =
    s"${identity._1.name} ${identity._2.render}"

  private def witnessText(path: List[Identity]): String =
    path.map(identityText).mkString(" -> ")

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

  private def pendingEdges(
      library: proto.Library,
      from: Identity,
      witness: List[Identity]
  ): Either[String, List[PendingEdge]] = {
    val all =
      library.publicDependencies.toList.map(LibraryDependencyVisibility.Public -> _) :::
        library.privateDependencies.toList.map(LibraryDependencyVisibility.Private -> _) :::
        library.unusedTransitivePublicDependencies.toList.map(
          LibraryDependencyVisibility.UnusedTransitivePublic -> _
        )

    all.traverse { case (visibility, dependency) =>
      val context = s"dependency of ${identityText(from)} via ${witnessText(witness)}"
      for {
        descriptor <- dependency.desc.toRight(
          s"missing descriptor for dependency '${dependency.name}' in $context"
        )
        versionProto <- descriptor.version.toRight(
          s"missing version for dependency '${dependency.name}' in $context"
        )
        _ <- Either.cond(
          descriptor.hashes.nonEmpty,
          (),
          s"missing expected hashes for dependency '${dependency.name}' in $context"
        )
      } yield PendingEdge(
        visibility,
        dependency,
        (Name(dependency.name), Version.fromProto(versionProto)),
        descriptor.hashes.toList,
        descriptor.uris.toList
      )
    }.map { pending =>
      val targetOrdering = summon[Ordering[Identity]]
      pending.sortWith { (left, right) =>
        val visibilityCompare = left.visibility.rank.compare(right.visibility.rank)
        if (visibilityCompare != 0) visibilityCompare < 0
        else {
          val targetCompare = targetOrdering.compare(left.target, right.target)
          if (targetCompare != 0) targetCompare < 0
          else {
            val hashesCompare = compareStrings(left.hashes, right.hashes)
            if (hashesCompare != 0) hashesCompare < 0
            else compareStrings(left.uris, right.uris) < 0
          }
        }
      }
    }
  }

  private[bosatsu] def topologicalOrder(
      root: Identity,
      identities: List[Identity],
      dependencyEdges: List[(Identity, Identity)],
      witnesses: Map[Identity, List[Identity]]
  ): Either[String, List[Identity]] = {
    val dependencies = mutable.Map.empty[Identity, Set[Identity]]
    val dependents = mutable.Map.empty[Identity, Set[Identity]]
    identities.foreach { identity =>
      dependencies.update(identity, Set.empty)
      dependents.update(identity, Set.empty)
    }
    dependencyEdges.foreach { case (referring, target) =>
      dependencies.update(
        referring,
        dependencies(referring) + target
      )
      dependents.update(
        target,
        dependents(target) + referring
      )
    }

    val remaining = mutable.Map.from(dependencies.iterator.map { case (key, deps) =>
      key -> deps.size
    })
    val ready = mutable.TreeSet.empty[Identity]
    remaining.iterator.collect { case (identity, 0) => identity }.foreach(ready.add)
    val result = List.newBuilder[Identity]
    var count = 0
    while (ready.nonEmpty) {
      val identity = ready.head
      ready -= identity
      result += identity
      count += 1
      dependents(identity).toList.sorted.foreach { dependent =>
        val next = remaining(dependent) - 1
        remaining.update(dependent, next)
        if (next == 0) ready += dependent
      }
    }
    if (count == identities.size) Right(result.result())
    else {
      val cyclic = remaining.iterator.collect { case (identity, left) if left > 0 => identity }
        .toList.sorted
      val first = cyclic.headOption.fold(identityText(root))(identityText)
      val witness = witnesses.get(cyclic.headOption.getOrElse(root))
        .fold(identityText(root))(witnessText)
      Left(
        s"dependency cycle prevents ordering ${cyclic.map(identityText).mkString(", ")} " +
          s"(reachable via $witness; first unresolved identity $first)"
      )
    }
  }

  private def topology(
      root: Identity,
      nodes: SortedMap[Identity, Node[?]],
      edges: List[VerifiedLibraryEdge[?]]
  ): Either[String, List[Identity]] =
    topologicalOrder(
      root,
      nodes.keysIterator.toList,
      edges.map(edge => edge.referringIdentity -> edge.targetIdentity),
      nodes.iterator.map { case (identity, node) => identity -> node.witness }.toMap
    )

  /** Authenticate the reachable descriptor graph before any decoder identity
    * cache is consulted. Every declared alias for a reachable identity is
    * checked, and every incoming edge is checked against all of its snapshots.
    */
  def verify[F[_], Path](
      platform: PlatformIO[F, Path],
      root: VerifiedLibrarySnapshot[Path],
      context: Context[Path]
  ): F[VerifiedLibraryGraph[Path]] = {
    import platform.{moduleIOMonad, pathOrdering, showPath}
    val F = moduleIOMonad
    val rootIdentity = (root.name, root.version)

    val declarations = context.libraries.foldLeft(
      SortedMap.empty[Identity, List[Declared[Path]]]
    ) { (all, declaration) =>
      val identity = (declaration.name, declaration.version)
      all.updated(identity, declaration :: all.getOrElse(identity, Nil))
    }

    def pathText(path: Path): String = showPath.show(path)

    // Keep the authenticated byte snapshot for each selected path for the
    // whole graph operation. A caller cannot make one file get read again by
    // declaring it under another identity later in the traversal.
    var snapshotsByPath = SortedMap(root.path -> root)

    def verifyPath(
        path: Path,
        identity: Identity,
        hashes: List[String],
        witness: List[Identity]
    ): F[VerifiedLibrarySnapshot[Path]] =
      snapshotsByPath.get(path) match {
        case Some(snapshot) =>
          F.fromEither(
            snapshot
              .authenticateAs(pathText(path), identity._1, identity._2, hashes)
              .leftMap { error =>
                CliException.Basic(s"$error via ${witnessText(witness)}")
              }
          ).as(snapshot)
        case None =>
          VerifiedLibrarySnapshot
            .verify(platform, path, identity._1, identity._2, hashes)
            .adaptError { case error =>
              CliException.Basic(
                s"${error.getMessage} (declared dependency via ${witnessText(witness)})"
              )
            }
            .map { snapshot =>
              snapshotsByPath = snapshotsByPath.updated(path, snapshot)
              snapshot
            }
      }

    def loadAliases(
        identity: Identity,
        witness: List[Identity],
        knownRoot: Option[VerifiedLibrarySnapshot[Path]]
    ): F[Node[Path]] = {
      val files = declarations.getOrElse(identity, Nil)
      val byPath = files.foldLeft(SortedMap.empty[Path, List[Declared[Path]]]) {
        (all, declaration) =>
          all.updated(
            declaration.path,
            declaration :: all.getOrElse(declaration.path, Nil)
          )
      }
      byPath.toList.traverse { case (path, aliases) =>
        val pins = aliases.flatMap(_.hashes).distinct
        val snapshotF = verifyPath(path, identity, pins, witness)
        snapshotF.map(path -> _)
      }.flatMap { loaded =>
        val rootSnapshot = knownRoot.toList
        val snapshots = (rootSnapshot ::: loaded.map(_._2)).foldLeft(
          SortedMap.empty[Path, VerifiedLibrarySnapshot[Path]]
        )((all, snapshot) => all.updated(snapshot.path, snapshot)).values.toList
        snapshots.headOption match {
          case None =>
            F.raiseError[Node[Path]](
              CliException.Basic(
                s"missing declared file for ${identityText(identity)} " +
                  s"via ${witnessText(witness)}"
              )
            )
          case Some(primary) =>
            val conflicting = snapshots.find(snapshot => !snapshot.hasSameBytes(primary))
            conflicting match {
              case Some(other) =>
                F.raiseError[Node[Path]](
                  CliException.Basic(
                    s"conflicting files claim ${identityText(identity)}: " +
                      s"${pathText(primary.path)} and ${pathText(other.path)} " +
                      s"via ${witnessText(witness)}"
                  )
                )
              case None =>
                F.pure(Node(primary, snapshots, witness))
            }
        }
      }
    }

    def edgeFor(
        from: Identity,
        fromNode: Node[Path],
        pending: PendingEdge,
        targetNode: Node[Path]
    ): F[VerifiedLibraryEdge[Path]] = {
      val edgeWitness = fromNode.witness :+ pending.target
      targetNode.aliases.traverse_ { snapshot =>
        F.fromEither(
          snapshot.authenticatePins(pathText(snapshot.path), pending.hashes).leftMap { error =>
            CliException.Basic(
              s"$error for edge ${identityText(from)} -${pending.visibility}-> " +
                s"${identityText(pending.target)} via ${witnessText(edgeWitness)}"
            )
          }
        )
      }.as(
        VerifiedLibraryEdge(
          from,
          pending.visibility,
          pending.target,
          pending.descriptor,
          pending.hashes,
          pending.uris,
          targetNode.primary.path,
          edgeWitness
        )
      )
    }

    val rootWitness = List(rootIdentity)
    loadAliases(rootIdentity, rootWitness, Some(root)).flatMap { rootNode =>
      F.tailRecM(
        BuildState(
          List(rootIdentity),
          SortedMap(rootIdentity -> rootNode),
          Nil
        )
      ) { state =>
        state.queue match {
          case Nil =>
            val graphEdges = state.edges.sortWith { (left, right) =>
              val fromCompare = summon[Ordering[Identity]].compare(
                left.referringIdentity,
                right.referringIdentity
              )
              if (fromCompare != 0) fromCompare < 0
              else if (left.visibility.rank != right.visibility.rank)
                left.visibility.rank < right.visibility.rank
              else {
                val targetCompare = summon[Ordering[Identity]].compare(
                  left.targetIdentity,
                  right.targetIdentity
                )
                if (targetCompare != 0) targetCompare < 0
                else {
                  val hashCompare = compareStrings(left.expectedHashes, right.expectedHashes)
                  if (hashCompare != 0) hashCompare < 0
                  else compareStrings(left.mirrorUris, right.mirrorUris) < 0
                }
              }
            }
            topology(rootIdentity, state.nodes, graphEdges) match {
              case Left(error) => F.raiseError[Either[BuildState[Path], VerifiedLibraryGraph[Path]]](
                  CliException.Basic(error)
                )
              case Right(order) =>
                F.pure(
                  Right(
                    new VerifiedLibraryGraph(
                      rootIdentity,
                      state.nodes,
                      graphEdges,
                      order
                    )
                  )
                )
            }
          case current :: rest =>
            val node = state.nodes(current)
            F.fromEither(
              pendingEdges(node.primary.protoLib, current, node.witness)
                .leftMap(CliException.Basic(_))
            ).flatMap { pending =>
              pending.foldLeftM(
                (state.nodes, state.edges, List.empty[Identity])
              ) { case ((nodes, edges, newlyFound), descriptor) =>
                val targetF = nodes.get(descriptor.target) match {
                  case Some(existing) => F.pure(existing -> false)
                  case None =>
                    val targetWitness = node.witness :+ descriptor.target
                    val rootAlias =
                      if (summon[Ordering[Identity]].equiv(descriptor.target, rootIdentity))
                        Some(root)
                      else None
                    loadAliases(descriptor.target, targetWitness, rootAlias)
                      .map(_ -> true)
                }
                targetF.flatMap { case (targetNode, isNew) =>
                  edgeFor(current, node, descriptor, targetNode).map { edge =>
                    val nextNodes =
                      if (isNew) nodes.updated(descriptor.target, targetNode) else nodes
                    val nextQueue =
                      if (isNew) newlyFound :+ descriptor.target else newlyFound
                    (nextNodes, edges :+ edge, nextQueue)
                  }
                }
              }.map { case (nodes, edges, newlyFound) =>
                Left(BuildState(rest ::: newlyFound, nodes, edges))
              }
            }
        }
      }
    }
  }
}
