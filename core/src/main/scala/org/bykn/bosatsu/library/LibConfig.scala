package org.bykn.bosatsu.library

import org.bykn.bosatsu.{Json, PackageName}
import java.util.regex.Pattern
import _root_.bosatsu.{TypedAst => proto}
import java.util.regex.PatternSyntaxException

case class LibConfig(
    name: String,
    repoUri: String,
    nextVersion: Version,
    exportedPackages: List[LibConfig.PackageFilter],
    allPackages: List[LibConfig.PackageFilter],
    publicDeps: List[proto.LibDependency],
    privateDeps: List[proto.LibDependency],
    history: proto.LibHistory
)

object LibConfig {
  import ProtoJsonReaders._

  sealed abstract class PackageFilter {
    def accepts(pn: PackageName): Boolean
    def asString: String
  }
  object PackageFilter {
    case class Name(name: PackageName) extends PackageFilter {
      def accepts(pn: PackageName): Boolean = pn == name
      def asString: String = name.asString
    }
    case class Regex(pattern: Pattern) extends PackageFilter {
      def accepts(pn: PackageName): Boolean =
        pattern.matcher(pn.asString).matches()
      def asString: String = pattern.pattern()
    }

    def fromString(str: String): Either[String, PackageFilter] =
      PackageName.parse(str) match {
        case Some(value) => Right(Name(value))
        case None =>
          try Right(Regex(Pattern.compile(str)))
          catch {
            case pse: PatternSyntaxException =>
              Left(
                s"could not parse as package name or regex:\n${pse.getMessage()}"
              )
          }
      }

    implicit val packageFilterJsonReader: Json.Reader[PackageFilter] =
      Json.Reader[String].mapEither("PackageFilter")(fromString(_))

    implicit val packageFilterJsonWriter: Json.Writer[PackageFilter] =
      Json.Writer[String].contramap[PackageFilter](_.asString)
  }

  def init(name: String, repoUri: String, ver: Version): LibConfig =
    LibConfig(
      name = name,
      repoUri = repoUri,
      nextVersion = ver,
      Nil,
      Nil,
      Nil,
      Nil,
      proto.LibHistory(None, None, None, None, Nil)
    )

  implicit class LibHistoryMethods(private val history: proto.LibHistory)
      extends AnyVal {
    def isEmpty: Boolean =
      history.previousMajor.isEmpty &&
        history.previousMinor.isEmpty &&
        history.previousPatch.isEmpty &&
        history.previousPrerelease.isEmpty &&
        history.others.isEmpty
  }

  implicit val libConfigWriter: Json.Writer[LibConfig] =
    Json.Writer[LibConfig] { lc =>
      import Json.Writer.write
      import lc._

      Json.JObject(
        ("name" -> write(name)) ::
          ("repo_uri" -> write(repoUri)) ::
          ("next_version" -> write(nextVersion)) ::
          ("exported_packages" -> write(exportedPackages)) ::
          ("all_packages" -> write(allPackages)) ::
          (if (publicDeps.isEmpty) Nil
           else ("public_deps" -> write(publicDeps)) :: Nil) :::
          (if (privateDeps.isEmpty) Nil
           else ("private_deps" -> write(privateDeps)) :: Nil) :::
          (if (history.isEmpty) Nil
           else ("history" -> write(history)) :: Nil) :::
          Nil
      )
    }
  implicit val libConfigReader: Json.Reader[LibConfig] =
    new Json.Reader.Obj[LibConfig] {
      def describe: String = "LibConfig"
      def readObj(
          from: Json.Reader.FromObj
      ): Either[(String, Json, Json.Path), LibConfig] =
        for {
          name <- from.field[String]("name")
          repoUri <- from.field[String]("repo_uri")
          nextVersion <- from.field[Version]("next_version")
          exportedPackages <- from.field[List[PackageFilter]](
            "exported_packages"
          )
          allPackages <- from.field[List[PackageFilter]]("all_packages")
          publicDeps <- from.optional[List[proto.LibDependency]]("public_deps")
          privateDeps <- from.optional[List[proto.LibDependency]](
            "private_deps"
          )
          history <- from.optional[proto.LibHistory]("history")
        } yield LibConfig(
          name = name,
          repoUri = repoUri,
          nextVersion = nextVersion,
          exportedPackages = exportedPackages,
          allPackages = allPackages,
          publicDeps = publicDeps.toList.flatten,
          privateDeps = privateDeps.toList.flatten,
          history = history.getOrElse(proto.LibHistory())
        )
    }
}
