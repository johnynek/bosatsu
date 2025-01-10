package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all._
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import org.bykn.bosatsu.{Json, Package, PackageName, ProtoConverter}
import org.bykn.bosatsu.tool.CliException
import org.typelevel.paiges.{Doc, Document}
import scala.util.{Failure, Success, Try}

case class LibConfig(
  name: Name,
  repoUri: String,
  nextVersion: Version,
  exportedPackages: List[LibConfig.PackageFilter],
  allPackages: List[LibConfig.PackageFilter],
  publicDeps: List[proto.LibDependency],
  privateDeps: List[proto.LibDependency],
  history: proto.LibHistory
) {
  /**
    * validate then unvalidatedAssemble
    */
  def assemble(
    vcsIdent: String, 
    previous: Option[proto.Library],
    packs: List[Package.Typed[Unit]],
    deps: List[proto.Library]): ValidatedNec[LibConfig.Error, proto.Library] = {
      val validated = validate(previous, packs, deps)
      val depth = previous match {
        case None => 0
        case Some(prevLib) => prevLib.depth + 1
      }

      val valProto = unvalidatedAssemble(depth, vcsIdent, packs) match {
        case Right(value) => Validated.valid(value)
        case Left(err) => Validated.invalidNec(LibConfig.Error.ProtoError(err))
      }

      validated *> valProto
    }


  /**
    * This checks the following properties and if they are set, builds the library
    * 1. all the included packs are set in allPackages
    * 2. the maximum version in history < nextVersion
    * 3. history is consistent with previous
    * 4. the version is semver compatible with previous
    * 5. the only packages that appear on exportedPackages apis are in exportedPackages or publicDeps
    * 6. all public deps appear somewhere on an API
    * 7. all private deps are used somewhere
    * 8. hashes of dependencies match
    * 9. there are no duplicate named dependencies
    */
  def validate(
    previous: Option[proto.Library],
    packs: List[Package.Typed[Unit]],
    deps: List[proto.Library]): ValidatedNec[LibConfig.Error, Unit] = ???

  // just build the library without any validations
  def unvalidatedAssemble(depth: Int, vcsIdent: String, packs: List[Package.Typed[Unit]]): Either[Throwable, proto.Library] = {
    import LibConfig.LibHistoryMethods

    val sortPack = packs.sortBy(_.name)
    val ifs = sortPack.traverseFilter { pack =>
        if (exportedPackages.exists(_.accepts(pack.name))) {
          val iface = pack.toIface
          ProtoConverter.interfaceToProto(iface).toEither.map(Some(_))
        }
        else Right(None)
      }

    val protoPacksE = sortPack.traverse(ProtoConverter.packageToProto(_).toEither)

    (ifs, protoPacksE).mapN { (ifaces, protoPacks) =>
      proto.Library(
        name = name.name,
        depth = depth,
        vcsIndent = vcsIdent,
        repoUri = repoUri,
        descriptor = Some(proto.LibDescriptor(version = Some(nextVersion.toProto))),
        exportedIfaces = ifaces,
        internalPackages = protoPacks,
        publicDependencies = publicDeps.sortBy(_.name),
        privateDependencies = privateDeps.sortBy(_.name),
        history = if (history.isEmpty) None else Some(history)
      )
    }
  }
}

object LibConfig {
  sealed abstract class Error
  object Error {
    case class ProtoError(error: Throwable) extends Error

    def errorsToDoc(nec: NonEmptyChain[Error]): Doc =
      Doc.intercalate(Doc.hardLine + Doc.hardLine,
        nec.toChain.toList.mapWithIndex((e, idx) => Doc.text(s"${idx + 1}. ") + docError.document(e).nested(4)))

    implicit val docError: Document[Error] =
      Document.instance {
        case ProtoError(e) => Doc.text(s"error encoding to proto: ${e.getMessage}")
      }

    implicit val showError: cats.Show[Error] =
      cats.Show[Error](e => docError.document(e).render(80))

    def toTry[A](vnec: ValidatedNec[Error, A]): Try[A] =
      vnec match {
        case Validated.Valid(a) => Success(a)
        case Validated.Invalid(errs) =>
          val stderr = errorsToDoc(errs)
          Failure(CliException(show"library errors: ${errs}", err = stderr))
      }
  }

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
      def accepts(pn: PackageName): Boolean = pattern.matcher(pn.asString).matches()
      def asString: String = pattern.pattern()
    }

    def fromString(str: String): Either[String, PackageFilter] =
      PackageName.parse(str) match {
        case Some(value) => Right(Name(value))
        case None =>
          try Right(Regex(Pattern.compile(str)))
          catch {
            case pse: PatternSyntaxException =>
              Left(s"could not parse as package name or regex:\n${pse.getMessage()}")
          }
      }

    implicit val packageFilterJsonReader: Json.Reader[PackageFilter] =
      Json.Reader[String].mapEither("PackageFilter")(fromString(_))

    implicit val packageFilterJsonWriter: Json.Writer[PackageFilter] =
      Json.Writer[String].contramap[PackageFilter](_.asString)
  }

  def init(name: Name, repoUri: String, ver: Version): LibConfig =
    LibConfig(name = name, repoUri = repoUri, nextVersion = ver, Nil, Nil, Nil, Nil, proto.LibHistory(None, None, None, None, Nil))

  implicit class LibHistoryMethods(private val history: proto.LibHistory) extends AnyVal {
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
        (if (publicDeps.isEmpty) Nil else ("public_deps" -> write(publicDeps)) :: Nil) :::
        (if (privateDeps.isEmpty) Nil else ("private_deps" -> write(privateDeps)) :: Nil) :::
        (if (history.isEmpty) Nil else ("history" -> write(history)) :: Nil) :::
        Nil
      )  
    }
  implicit val libConfigReader: Json.Reader[LibConfig] =
    new Json.Reader.Obj[LibConfig] {
      def describe: String = "LibConfig"
      def readObj(from: Json.Reader.FromObj): Either[(String, Json, Json.Path), LibConfig] =
        for {
          name <- from.field[Name]("name")
          repoUri <- from.field[String]("repo_uri")
          nextVersion <- from.field[Version]("next_version")
          exportedPackages <- from.field[List[PackageFilter]]("exported_packages")
          allPackages <- from.field[List[PackageFilter]]("all_packages")
          publicDeps <- from.optional[List[proto.LibDependency]]("public_deps")
          privateDeps <- from.optional[List[proto.LibDependency]]("private_deps")
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