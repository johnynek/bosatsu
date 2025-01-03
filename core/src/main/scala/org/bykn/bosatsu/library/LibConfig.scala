package org.bykn.bosatsu.library

import org.bykn.bosatsu.{Json, PackageName}
import _root_.bosatsu.{TypedAst => proto}

case class LibConfig(
  name: String,
  repoUri: String,
  exportedPackages: List[PackageName],
  internalPackages: List[PackageName],
  publicDeps: List[proto.LibDependency],
  privateDeps: List[proto.LibDependency],
)

object LibConfig {
  import ProtoJsonReaders._

  def init(name: String, repoUri: String): LibConfig =
    LibConfig(name = name, repoUri = repoUri, Nil, Nil, Nil, Nil)

  implicit val libConfigWriter: Json.Writer[LibConfig] =
    Json.Writer[LibConfig] { lc =>
      import Json.Writer.write
      import lc._

      implicit val pnWriter = Json.Writer.from[PackageName](pn => Json.JString(pn.asString))

      Json.JObject(
        ("name" -> write(name)) ::
        ("repo_uri" -> write(repoUri)) ::
        ("exported_packages" -> write(exportedPackages)) ::
        ("internal_packages" -> write(internalPackages)) ::
        (if (publicDeps.isEmpty) Nil else ("public_deps" -> write(publicDeps)) :: Nil) :::
        (if (privateDeps.isEmpty) Nil else ("private_deps" -> write(privateDeps)) :: Nil) :::
        Nil
      )  
    }
  implicit val libConfigReader: Json.Reader[LibConfig] =
    new Json.Reader.Obj[LibConfig] {

      implicit val pnReader: Json.Reader[PackageName] =
        Json.Reader.fromParser("package_name", PackageName.parser)

      def describe: String = "LibConfig"
      def readObj(from: Json.Reader.FromObj): Either[(String, Json, Json.Path), LibConfig] =
        for {
          name <- from.field[String]("name")
          repoUri <- from.field[String]("repo_uri")
          exportedPackages <- from.field[List[PackageName]]("exported_packages")
          internalPackages <- from.field[List[PackageName]]("internal_packages")
          publicDeps <- from.optional[List[proto.LibDependency]]("public_deps")
          privateDeps <- from.optional[List[proto.LibDependency]]("private_deps")
        } yield LibConfig(
          name = name,
          repoUri = repoUri,
          exportedPackages = exportedPackages,
          internalPackages = internalPackages,
          publicDeps = publicDeps.toList.flatten,
          privateDeps = privateDeps.toList.flatten
        )
    }
}