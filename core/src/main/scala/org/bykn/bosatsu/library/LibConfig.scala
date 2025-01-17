package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.syntax.all._
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import org.bykn.bosatsu.{Json, Package, PackageName, ProtoConverter}
import org.bykn.bosatsu.tool.CliException
import org.bykn.bosatsu.hashing.{Hashed, Algo}
import org.typelevel.paiges.{Doc, Document}
import scala.util.{Failure, Success, Try}

import LibConfig.{Error, LibMethods, LibHistoryMethods}

case class LibConfig(
  name: Name,
  repoUri: String,
  nextVersion: Version,
  exportedPackages: List[LibConfig.PackageFilter],
  allPackages: List[LibConfig.PackageFilter],
  publicDeps: List[proto.LibDependency],
  privateDeps: List[proto.LibDependency],
) {
  
  /**
    * validate then unvalidatedAssemble
    */
  def assemble(
    vcsIdent: String, 
    previous: Option[Hashed[Algo.Sha256, proto.Library]],
    packs: List[Package.Typed[Unit]],
    deps: List[Hashed[Algo.Sha256, proto.Library]]): ValidatedNec[Error, proto.Library] = {
      val validated = validate(previous, packs, deps)

      val valProto = unvalidatedAssemble(previous, vcsIdent, packs) match {
        case Right(value) => Validated.valid(value)
        case Left(err) => Validated.invalidNec(Error.ProtoError(err): Error)
      }

      validated *> valProto
    }


  /**
    * This checks the following properties and if they are set, builds the library
    * 1. all the included packs are set in allPackages
    * 2. the maximum version in history < nextVersion
    * 3. the version is semver compatible with previous
    * 4. the only packages that appear on exportedPackages apis are in exportedPackages or publicDeps
    * 5. all public deps appear somewhere on an API
    * 6. all private deps are used somewhere
    * 7. hashes of dependencies match
    * 8. there are no duplicate named dependencies
    */
  def validate(
    previous: Option[Hashed[Algo.Sha256, proto.Library]],
    packs: List[Package.Typed[Unit]],
    deps: List[Hashed[Algo.Sha256, proto.Library]]): ValidatedNec[Error, Unit] = {

      def inv(e: Error): ValidatedNec[Error, Nothing] = Validated.invalidNec(e)

      val prop1 =
        packs.filterNot(p => allPackages.exists(_.accepts(p.name))) match {
          case Nil => Validated.unit
          case h :: t => inv(Error.ExtraPackages(NonEmptyList(h, t)))
        }

      val prop2 = previous.traverse_ { case Hashed(_, p) =>
        val prevLt = p.version match {
          case Some(prevV) =>
            if (Ordering[Version].lt(prevV, nextVersion)) Validated.unit 
            else inv(Error.VersionNotIncreasing("previous library", prevV, nextVersion))
          case None =>
            inv(Error.InvalidPreviousLib("missing version", p))
        }
        
        val histLt = p.history match {
          case Some(history) =>
            history.allVersions.traverse_ { prevV =>
              if (Ordering[Version].lt(prevV, nextVersion)) Validated.unit 
              else inv(Error.VersionNotIncreasing("history version", prevV, nextVersion))
            }
          case None => Validated.unit
        }

        prevLt *> histLt
      }

      val prop3 = previous match {
        case Some(Hashed(_, prevLib)) =>
          prevLib.version match {
            case Some(prevVersion) =>
              if (prevVersion.justBefore(nextVersion)) Validated.unit
              else {
                Validated.invalidNec(Error.VersionNotAdjacent(prevVersion, nextVersion))
              }
            case None =>
              Validated.invalidNec(Error.InvalidPreviousLib("missing version", prevLib))
          }
        case None =>
          // Then the version can be anything
          Validated.unit
      }

      prop1 *> prop2 *> prop3
    }

  // just build the library without any validations
  def unvalidatedAssemble(previous: Option[Hashed[Algo.Sha256, proto.Library]], vcsIdent: String, packs: List[Package.Typed[Unit]]): Either[Throwable, proto.Library] = {
    val depth = previous match {
      case None => 0
      case Some(Hashed(_, prevLib)) => prevLib.depth + 1
    }

    val thisHistory = previous match {
      case None => proto.LibHistory()
      case Some(Hashed(_, p)) =>
        val prevHistory = p.history.getOrElse(proto.LibHistory())
        val desc = p.descriptor match {
          case Some(desc) => desc
          case None =>
            // this should never happen after validation
            return Left(new Exception(s"invalid previous missing descriptor: $p"))
        }
        prevHistory.nextHistory(desc)
    }

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
        history = Some(thisHistory)
      )
    }
  }
}

object LibConfig {
  sealed abstract class Error
  object Error {
    case class ExtraPackages(nel: NonEmptyList[Package.Typed[Unit]]) extends Error
    case class ProtoError(error: Throwable) extends Error
    case class VersionNotIncreasing(note: String, previous: Version, current: Version) extends Error
    case class VersionNotAdjacent(previous: Version, current: Version) extends Error
    case class InvalidPreviousLib(note: String, previous: proto.Library) extends Error

    def errorsToDoc(nec: NonEmptyChain[Error]): Doc =
      Doc.intercalate(Doc.hardLine + Doc.hardLine,
        nec.toChain.toList.mapWithIndex((e, idx) => Doc.text(s"${idx + 1}. ") + docError.document(e).nested(4)))

    implicit val docError: Document[Error] =
      Document.instance {
        case ExtraPackages(nel) =>
          Doc.text("unexpected extra packages: ") +
            Doc.intercalate(Doc.comma + Doc.line, nel.toList.map(p => Doc.text(show"${p.name}")))
              .grouped.nested(4)
        case VersionNotIncreasing(note, previous, current) =>
          Doc.text(show"version not increasing $note:") + (Doc.line +
            Doc.text(show"$previous is not less than $current")).grouped.nested(4)
        case VersionNotAdjacent(previous, current) =>
          Doc.text(show"previous version not adjacent:") + (Doc.line +
            Doc.text(show"$previous") + Doc.line + Doc.text("is not adjacent to") + Doc.line + Doc.text(show"$current")).grouped.nested(4)
        case InvalidPreviousLib(note, _) => 
          Doc.text(s"invalid previous library: $note")
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
    LibConfig(name = name, repoUri = repoUri, nextVersion = ver, Nil, Nil, Nil, Nil)

  implicit class LibHistoryMethods(private val history: proto.LibHistory) extends AnyVal {
    def isEmpty: Boolean =
      history.previousMajor.isEmpty &&
      history.previousMinor.isEmpty &&
      history.previousPatch.isEmpty &&
      history.previousPrerelease.isEmpty &&
      history.others.isEmpty

    def allDescriptors: List[proto.LibDescriptor] =
      history.previousPrerelease.toList :::
        history.previousPatch.toList :::
        history.previousMinor.toList :::
        history.previousMajor.toList :::
        history.others.toList

    def allVersions: List[Version] =
      allDescriptors.flatMap(_.version).map(Version.fromProto(_)) 

    def nextHistory(prevDesc: proto.LibDescriptor): proto.LibHistory = ???
  }

  implicit class LibMethods(private val lib: proto.Library) extends AnyVal {
    def version: Option[Version] = lib.descriptor.flatMap(_.version).map(Version.fromProto(_))
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
        } yield LibConfig(
          name = name,
          repoUri = repoUri,
          nextVersion = nextVersion,
          exportedPackages = exportedPackages,
          allPackages = allPackages,
          publicDeps = publicDeps.toList.flatten,
          privateDeps = privateDeps.toList.flatten,
        )
    }
}