package dev.bosatsu.tool

import cats.MonadThrow
import cats.syntax.all._
import dev.bosatsu.{PackageName, PlatformIO}
import dev.bosatsu.Json
import scala.collection.mutable

/** A structural manifest describes a local group in the loose implementation
  * scope. Imported libraries have their own versioned scopes and will need a
  * distinct node kind. Treating their private contents as loose groups would
  * reject legal compositions.
  */
private[bosatsu] object GroupGraph {
  final case class Member(source: String, packageName: PackageName)
  final case class Edge[P](label: String, manifest: P)
  final case class Node[P](
      label: String,
      members: List[Member],
      deps: List[Edge[P]]
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
    (
      from.field[String]("source")(using BuildManifest.nonEmpty),
      from.field[PackageName]("package")
    ).mapN(Member(_, _))
  }

  private given Json.Reader[Edge[String]] =
    BuildManifest.obj("group dependency") { from =>
      (
        from.field[String]("label")(using BuildManifest.nonEmpty),
        from.field[String]("manifest")(using BuildManifest.nonEmpty)
      ).mapN(Edge(_, _))
    }

  private def nodeReader(view: View): Json.Reader[Node[String]] =
    BuildManifest.obj("group node") { from =>
      for {
        _ <- BuildManifest.version1(from)
        kind <- from.field[String]("kind")
        _ <- Either.cond(
          kind == "group" || (view == View.DirectMembers && kind == "group-members"),
          (),
          (
            s"unsupported graph node kind '$kind' for $view",
            from.j,
            from.path.key("kind")
          )
        )
        label <- from.field[String]("label")(using BuildManifest.nonEmpty)
        members <- from.field[List[Member]]("members")
        _ <- Either.cond(
          members.nonEmpty,
          (),
          (
            "expected at least one group member",
            from.j,
            from.path.key("members")
          )
        )
        // Local checks stop at the direct groups. Only the full topology may
        // justify traversing an edge or certifying that a group is a leaf.
        deps <-
          if (kind == "group-members") Right(Nil)
          else from.field[List[Edge[String]]]("deps")
      } yield Node(label, members, deps)
    }

  def readNode[F[_], P](
      platform: PlatformIO[F, P],
      file: P,
      view: View
  ): F[Node[P]] = {
    import platform.{moduleIOMonad, showPath}
    BuildManifest.read(platform, file, view.reader).flatMap { node =>
      moduleIOMonad.fromEither(
        node.deps
          .traverse { edge =>
            platform
              .path(edge.manifest)
              .toEither
              .bimap(
                errs =>
                  CliException.Basic(
                    show"invalid dependency path in $file for ${edge.label}: ${errs.toList.mkString(", ")}"
                  ),
                path => Edge(edge.label, path)
              )
          }
          .map(Node(node.label, node.members, _))
      )
    }
  }

  private type Route = List[String]

  private def display(route: Route): String =
    route.reverseIterator.mkString(" -> ")

  final private case class Claim(member: Member, route: Route) {
    def describe: String =
      s"${display(route)} (${member.source}, ${member.packageName.asString})"
  }

  sealed private trait Work[P]
  final private case class Visit[P](edge: Edge[P], parents: Route)
      extends Work[P]
  final private case class Leave[P](file: P) extends Work[P]

  /** Each read is authorized by the command's explicit input list. Edge paths
    * use the same request-relative coordinate system as those arguments. A
    * local action opens the root and its immediate dependencies. A full action
    * opens each reachable manifest once even if many paths lead to it.
    */
  def validate[F[_], P](
      root: P,
      declared: Set[P],
      scope: Scope,
      showFile: P => String
  )(read: (P, View) => F[Node[P]])(using F: MonadThrow[F]): F[Unit] =
    read(root, View.Topology).flatMap { rootNode =>
      // All mutable state belongs to this execution of the effect. Routes
      // share tails: only render the two full paths when reporting a conflict.
      val allowed = declared + root
      val files = mutable.Map.empty[P, String]
      val labels = mutable.Map.empty[String, (P, Route)]
      val active = mutable.Set.empty[P]
      val sources = mutable.Map.empty[String, Claim]
      val packages = mutable.Map.empty[PackageName, Claim]

      def enter(
          file: P,
          node: Node[P],
          parents: Route,
          rest: List[Work[P]]
      ): List[Work[P]] = {
        val route = node.label :: parents
        labels.get(node.label).foreach { case (previous, first) =>
          throw CliException.Basic(
            s"group ${node.label} has two manifests: ${showFile(previous)} via ${display(first)} and " +
              s"${showFile(file)} via ${display(route)}"
          )
        }
        labels.update(node.label, (file, route))
        files.update(file, node.label)
        node.members.foreach { member =>
          if (member.packageName.isPredef)
            throw CliException.Basic(
              s"${display(route)} claims implicit Bosatsu/Predef in ${member.source}"
            )
          val claim = Claim(member, route)
          sources.get(member.source).foreach { prior =>
            throw CliException.Basic(
              s"duplicate source '${member.source}': ${prior.describe} and ${claim.describe}"
            )
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

      def requireLabel(
          expected: String,
          actual: String,
          file: P,
          parents: Route
      ): Unit =
        if (actual != expected)
          throw CliException.Basic(
            s"${display(expected :: parents)} expects group $expected at ${showFile(file)}, found $actual"
          )

      F.catchNonFatal(enter(root, rootNode, Nil, Nil)).flatMap { initial =>
        F.tailRecM[List[Work[P]], Unit](initial) {
          case Nil =>
            if (allowed.size == files.size) F.pure(Right(()))
            else {
              val extra = allowed.iterator
                .filterNot(files.contains)
                .map(showFile)
                .toList
                .sorted
              F.raiseError(
                CliException
                  .Basic(s"unreachable node manifests: ${extra.mkString(", ")}")
              )
            }
          case Leave(file) :: rest =>
            val _ = active.remove(file)
            F.pure(Left(rest))
          case Visit(Edge(label, file), parents) :: rest =>
            files.get(file) match {
              case Some(actual) =>
                F.catchNonFatal {
                  requireLabel(label, actual, file, parents)
                  if (active(file))
                    throw CliException.Basic(
                      s"cyclic group dependency: ${display(label :: parents)}"
                    )
                  Left(rest)
                }
              case None if !allowed(file) =>
                F.raiseError(
                  CliException.Basic(
                    s"missing declared --node ${showFile(file)} for ${display(label :: parents)}"
                  )
                )
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
}
