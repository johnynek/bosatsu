package dev.bosatsu.tool

import cats.MonadThrow
import cats.syntax.all._
import dev.bosatsu.{PackageName, PlatformIO}
import dev.bosatsu.Json
import dev.bosatsu.library.{LibraryDependencyVisibility, VerifiedLibraryGraph}
import dev.bosatsu.tool.PackageBindings.{
  CheckedScopeMap,
  LibraryIdentity,
  PackageOwner,
  PublicPackage,
  PublicProvider,
  Scope as BindingScope
}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

/** A structural manifest describes a local group in the loose implementation
  * scope. Imported libraries have their own versioned scopes and will need a
  * distinct node kind. Treating their private contents as loose groups would
  * reject legal compositions.
  */
private[bosatsu] object GroupGraph {
  final case class Member(source: String, packageName: PackageName)
  final case class Edge[P](label: String, manifest: P)
  final case class Node[P](label: String, members: List[Member], deps: List[Edge[P]])

  /** Local group and imported library scopes occupy different namespaces.
    * Library edges retain their visibility for implementation composition;
    * only the authenticated public packages participate in member visibility.
    */
  sealed trait TopologyNode[P]
  final case class LocalGroup[P](file: P, node: Node[P]) extends TopologyNode[P]
  final case class ImportedLibrary[P](
      identity: LibraryIdentity,
      artifact: P,
      dependencies: List[(LibraryDependencyVisibility, LibraryIdentity)],
      packages: List[PublicPackage]
  ) extends TopologyNode[P]

  final case class MixedTopology[P](
      localGroups: List[LocalGroup[P]],
      importedLibraries: SortedMap[LibraryIdentity, ImportedLibrary[P]]
  )

  enum View derives CanEqual {
    case Topology, DirectMembers
    private[GroupGraph] lazy val reader = nodeReader(this)
  }

  enum Scope(val depView: View) derives CanEqual {
    case Local extends Scope(View.DirectMembers)
    case Transitive extends Scope(View.Topology)
  }

  private given Json.Reader[Member] = BuildManifest.obj("group member") { from =>
    (from.field[String]("source")(using BuildManifest.nonEmpty),
      from.field[PackageName]("package")).mapN(Member(_, _))
  }

  private given Json.Reader[Edge[String]] = BuildManifest.obj("group dependency") { from =>
    (from.field[String]("label")(using BuildManifest.nonEmpty),
      from.field[String]("manifest")(using BuildManifest.nonEmpty)).mapN(Edge(_, _))
  }

  private def nodeReader(view: View): Json.Reader[Node[String]] =
    BuildManifest.obj("group node") { from =>
      for {
        _ <- BuildManifest.version1(from)
        kind <- from.field[String]("kind")
        _ <- Either.cond(kind == "group" || (view == View.DirectMembers && kind == "group-members"), (),
          (s"unsupported graph node kind '$kind' for $view", from.j, from.path.key("kind")))
        label <- from.field[String]("label")(using BuildManifest.nonEmpty)
        members <- from.field[List[Member]]("members")
        _ <- Either.cond(members.nonEmpty, (),
          ("expected at least one group member", from.j, from.path.key("members")))
        // Local checks stop at the direct groups. Only the full topology may
        // justify traversing an edge or certifying that a group is a leaf.
        deps <- if (kind == "group-members") Right(Nil) else from.field[List[Edge[String]]]("deps")
      } yield Node(label, members, deps)
    }

  def readNode[F[_], P](platform: PlatformIO[F, P], file: P, view: View): F[Node[P]] = {
    import platform.{moduleIOMonad, showPath}
    BuildManifest.read(platform, file, view.reader).flatMap { node =>
      moduleIOMonad.fromEither(node.deps.traverse { edge =>
        platform.path(edge.manifest).toEither.bimap(
          errs => CliException.Basic(show"invalid dependency path in $file for ${edge.label}: ${errs.toList.mkString(", ")}"),
          path => Edge(edge.label, path)
        )
      }.map(Node(node.label, node.members, _)))
    }
  }

  private type Route = List[String]

  private def display(route: Route): String = route.reverseIterator.mkString(" -> ")

  private final case class Claim(member: Member, route: Route) {
    def describe: String = s"${display(route)} (${member.source}, ${member.packageName.asString})"
  }

  private sealed trait Work[P]
  private final case class Visit[P](edge: Edge[P], parents: Route) extends Work[P]
  private final case class Leave[P](file: P) extends Work[P]

  /** Each read is authorized by the command's explicit input list. Edge paths
    * use the same request-relative coordinate system as those arguments.
    * A local action opens the root and its immediate dependencies. A full
    * action opens each reachable manifest once even if many paths lead to it.
    */
  private def validateLocalGraph[F[_], P](
      root: P,
      declared: Set[P],
      scope: Scope,
      showFile: P => String
  )(read: (P, View) => F[Node[P]])(using F: MonadThrow[F]): F[List[LocalGroup[P]]] =
    read(root, View.Topology).flatMap { rootNode =>
      // All mutable state belongs to this execution of the effect. Routes
      // share tails: only render the two full paths when reporting a conflict.
      val allowed = declared + root
      val files = mutable.Map.empty[P, String]
      val labels = mutable.Map.empty[String, (P, Route)]
      val active = mutable.Set.empty[P]
      val sources = mutable.Map.empty[String, Claim]
      val packages = mutable.Map.empty[PackageName, Claim]
      val localGroups = mutable.ListBuffer.empty[LocalGroup[P]]

      def enter(file: P, node: Node[P], parents: Route, rest: List[Work[P]]): List[Work[P]] = {
        val route = node.label :: parents
        labels.get(node.label).foreach { case (previous, first) =>
          throw CliException.Basic(
            s"group ${node.label} has two manifests: ${showFile(previous)} via ${display(first)} and " +
              s"${showFile(file)} via ${display(route)}"
          )
        }
        labels.update(node.label, (file, route))
        files.update(file, node.label)
        localGroups += LocalGroup(file, node)
        node.members.foreach { member =>
          if (member.packageName.isPredef)
            throw CliException.Basic(s"${display(route)} claims implicit Bosatsu/Predef in ${member.source}")
          val claim = Claim(member, route)
          sources.get(member.source).foreach { prior =>
            throw CliException.Basic(s"duplicate source '${member.source}': ${prior.describe} and ${claim.describe}")
          }
          packages.get(member.packageName).foreach { prior =>
            throw CliException.Basic(
              s"duplicate package ${member.packageName.asString}: ${prior.describe} and ${claim.describe}"
            )
          }
          sources.update(member.source, claim)
          packages.update(member.packageName, claim)
        }
        if (parents.isEmpty || scope == Scope.Transitive) {
          active += file
          node.deps.map(Visit(_, route)) ::: Leave(file) :: rest
        } else rest
      }

      def requireLabel(expected: String, actual: String, file: P, parents: Route): Unit =
        if (actual != expected) throw CliException.Basic(
          s"${display(expected :: parents)} expects group $expected at ${showFile(file)}, found $actual"
        )

      F.catchNonFatal(enter(root, rootNode, Nil, Nil)).flatMap { initial =>
        F.tailRecM[List[Work[P]], List[LocalGroup[P]]](initial) {
          case Nil =>
            if (allowed.size == files.size) F.pure(Right(localGroups.toList))
            else {
              val extra = allowed.iterator.filterNot(files.contains).map(showFile).toList.sorted
              F.raiseError(CliException.Basic(s"unreachable node manifests: ${extra.mkString(", ")}"))
            }
          case Leave(file) :: rest =>
            val _ = active.remove(file)
            F.pure(Left(rest))
          case Visit(Edge(label, file), parents) :: rest =>
            files.get(file) match {
              case Some(actual) => F.catchNonFatal {
                requireLabel(label, actual, file, parents)
                if (active(file)) throw CliException.Basic(s"cyclic group dependency: ${display(label :: parents)}")
                Left(rest)
              }
              case None if !allowed(file) =>
                F.raiseError(CliException.Basic(
                  s"missing declared --node ${showFile(file)} for ${display(label :: parents)}"
                ))
              case None =>
                read(file, scope.depView).flatMap { node =>
                  F.catchNonFatal {
                    requireLabel(label, node.label, file, parents)
                    Left(enter(file, node, parents, rest))
                  }
                }
            }
        }
      }
    }

  /** Check a local graph without exposing its collected topology to legacy
    * callers. Membership-only checks retain their existing bounded reads.
    */
  def validate[F[_], P](
      root: P,
      declared: Set[P],
      scope: Scope,
      showFile: P => String
  )(read: (P, View) => F[Node[P]])(using F: MonadThrow[F]): F[Unit] =
    validateLocalGraph(root, declared, scope, showFile)(read).void

  private def identityOrdering: Ordering[LibraryIdentity] =
    Ordering.Tuple2(using summon[Ordering[dev.bosatsu.library.Name]],
      summon[Ordering[dev.bosatsu.library.Version]])

  private def libraryText(identity: LibraryIdentity): String =
    s"${identity._1.name}@${identity._2.render}"

  private def scopeText(scope: BindingScope): String = scope match {
    case BindingScope.Member(name) => s"member ${name.asString}"
    case BindingScope.Library(identity) => s"library ${libraryText(identity)}"
    case BindingScope.Predef => "compiler Predef"
  }

  private def routeText(route: List[BindingScope]): String =
    route.map(scopeText).mkString(" -> ")

  private def appendExposureRoute(
      prefix: List[BindingScope],
      witness: List[LibraryIdentity]
  ): List[BindingScope] = {
    val suffix = witness.map(BindingScope.Library(_))
    (prefix.lastOption, suffix.headOption) match {
      case (Some(last), Some(first)) if last == first => prefix.dropRight(1) ::: suffix
      case _ => prefix ::: suffix
    }
  }

  private def mixedTopology[P](
      localGroups: List[LocalGroup[P]],
      graph: Option[VerifiedLibraryGraph[P]],
      publicIndex: PackageBindings.PublicIndex
  ): Either[String, MixedTopology[P]] = {
    val imported = graph.fold[Either[String, List[(LibraryIdentity, ImportedLibrary[P])]]](Right(Nil)) { verified =>
      verified.identities.traverse { identity =>
        verified.snapshot(identity).toRight(
          s"missing authenticated snapshot for ${libraryText(identity)}"
        ).map { snapshot =>
          val edges = verified.dependencyEdges(identity).map(edge => edge.visibility -> edge.targetIdentity)
          val packages = publicIndex.packagesFor(BindingScope.Library(identity))
          identity -> ImportedLibrary(identity, snapshot.path, edges, packages)
        }
      }
    }
    imported.map { entries =>
      MixedTopology(
        localGroups,
        SortedMap.from(entries)(using identityOrdering)
      )
    }
  }

  private[bosatsu] def validateVisibleReleases[P](
      topology: MixedTopology[P],
      selections: List[(PackageName, List[(PackageOwner, List[BindingScope])])]
  ): Either[String, Unit] = {
    val localPackages = SortedSet.from(
      topology.localGroups.iterator.flatMap(_.node.members.iterator.map(_.packageName))
    )(using summon[Ordering[PackageName]])
    def validateMember(
        member: PackageName,
        roots: List[(PackageOwner, List[BindingScope])]
    ): Either[String, Unit] = {
      val releases = mutable.Map.empty[
        dev.bosatsu.library.Name,
        (dev.bosatsu.library.Version, LibraryIdentity, List[BindingScope])
      ]
      val visited = mutable.Set.empty[PackageOwner]
      val pending = mutable.Queue.from(roots)
      var result: Either[String, Unit] = Right(())

      while (pending.nonEmpty && result.isRight) {
        val (owner, route) = pending.dequeue()
        owner.scope match {
          case BindingScope.Library(identity) =>
            if (localPackages.contains(owner.packageName)) {
              result = Left(
                s"imported provider ${scopeText(owner.scope)} also claims local package " +
                  owner.packageName.asString
              )
            } else {
              val previous = releases.get(identity._1)
              previous match {
                case Some((version, priorIdentity, priorRoute))
                    if summon[Ordering[dev.bosatsu.library.Version]].compare(version, identity._2) != 0 =>
                  result = Left(
                    s"member ${member.asString} sees two releases of library ${identity._1.name}: " +
                      s"${libraryText(priorIdentity)} via ${routeText(priorRoute)} and " +
                      s"${libraryText(identity)} via ${routeText(route)}"
                  )
                case None => releases.update(identity._1, (identity._2, identity, route))
                case _ => ()
              }

              if (result.isRight && visited.add(owner)) {
                topology.importedLibraries.get(identity) match {
                  case None =>
                    result = Left(
                      s"selected provider ${libraryText(identity)} is absent from the authenticated graph"
                    )
                  case Some(imported) =>
                    imported.packages.find(_.packageName == owner.packageName) match {
                      case None =>
                        result = Left(
                          s"authenticated scope ${libraryText(identity)} has no public package " +
                            owner.packageName.asString
                        )
                      case Some(pkg) =>
                        pkg.exposures.foreach {
                          case (_, PublicProvider.CompilerPredef) => ()
                          case (packageName, PublicProvider.Library(provider, witness)) =>
                            pending.enqueue((
                              PackageOwner(BindingScope.Library(provider), packageName),
                              appendExposureRoute(route, witness)
                            ))
                        }
                    }
                }
              }
            }
          case _ => ()
        }
      }
      result
    }

    selections.traverse_ { case (member, roots) => validateMember(member, roots) }
  }

  private def validateMemberReleases[P](
      topology: MixedTopology[P],
      checked: CheckedScopeMap[P]
  ): Either[String, Unit] = {
    val selections = checked.scopeMap.members.toList.map { member =>
      val roots = checked.bindings.iterator.collect {
        case (key, binding) if key.referrer == BindingScope.Member(member) =>
          (PackageOwner(binding.owner.scope, key.requiredPackage), binding.witnessPath)
      }.toList
      member -> roots
    }
    validateVisibleReleases(topology, selections)
  }

  /** Validate a local topology together with the authenticated imported
    * scopes selected for its members. Imported implementation dependencies
    * remain graph nodes but do not become type-visible without an exposure.
    */
  def validateMixed[F[_], P](
      root: P,
      declared: Set[P],
      scope: Scope,
      checked: CheckedScopeMap[P],
      graph: Option[VerifiedLibraryGraph[P]],
      showFile: P => String
  )(read: (P, View) => F[Node[P]])(using F: MonadThrow[F]): F[Unit] = {
    import cats.syntax.all._
    validateLocalGraph(root, declared, scope, showFile)(read).flatMap { localGroups =>
      val rootMembers = localGroups.headOption.toList.flatMap(_.node.members.map(_.packageName))
        .sorted(using summon[Ordering[PackageName]])
      val declaredMembers = checked.scopeMap.members.toList
      val memberMatch = rootMembers.lengthCompare(declaredMembers.length) == 0 &&
        rootMembers.zip(declaredMembers).forall { case (left, right) =>
          summon[Ordering[PackageName]].compare(left, right) == 0
        }
      if (!memberMatch)
        F.raiseError(CliException.Basic(
          "scope-map members do not match the root group membership"
        ))
      else
        F.fromEither(
          mixedTopology(localGroups, graph, checked.publicIndex).leftMap(CliException.Basic(_))
        ).flatMap { topology =>
          F.fromEither(validateMemberReleases(topology, checked).leftMap(CliException.Basic(_)))
        }
    }
  }
}
