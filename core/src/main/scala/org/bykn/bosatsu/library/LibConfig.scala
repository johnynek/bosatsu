package org.bykn.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.syntax.all._
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import org.bykn.bosatsu.{Json, Package, PackageName, ProtoConverter}
import org.bykn.bosatsu.tool.CliException
import org.bykn.bosatsu.hashing.{Hashed, HashValue, Algo}
import org.typelevel.paiges.{Doc, Document}
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.SortedMap

import LibConfig.{Error, LibMethods, LibHistoryMethods, ValidationResult}

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
    previous: Option[Hashed[Algo.Blake3, proto.Library]],
    packs: List[Package.Typed[Unit]],
    deps: List[Hashed[Algo.Blake3, proto.Library]]): ValidatedNec[Error, proto.Library] =
      validate(previous, packs, deps)
        .andThen { vr =>
          unvalidatedAssemble(previous, vcsIdent, packs, vr.unusedTransitiveDeps.iterator.map(_._2).toList) match {
            case Right(value) => Validated.valid(value)
            case Left(err) => Validated.invalidNec(Error.ProtoError(err): Error)
          }
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
    * 9. there is a valid solution for transitive dependencies
    */
  def validate(
    previous: Option[Hashed[Algo.Blake3, proto.Library]],
    packs: List[Package.Typed[Unit]],
    deps: List[Hashed[Algo.Blake3, proto.Library]]): ValidatedNec[Error, ValidationResult] = {

      import Error.inv

      val exportedPacks: List[Package.Typed[Unit]] =
        packs.filter(p => exportedPackages.exists(_.accepts(p.name)))

      val privatePacks: List[Package.Typed[Unit]] =
        packs.filter(p => allPackages.exists(_.accepts(p.name)))

      val nameToDep = deps.groupByNel(_.arg.name)

      val publicDepLibs: List[proto.Library] =
        publicDeps.flatMap { dep =>
          nameToDep.get(dep.name) match {
            case None =>
              // this will be a validation error later
              Nil
            case Some(libs) => libs.head.arg :: Nil
          }
        }

      val privateDepLibs: List[proto.Library] =
        privateDeps.flatMap { dep =>
          nameToDep.get(dep.name) match {
            case None =>
              // this will be a validation error later
              Nil
            case Some(libs) => libs.head.arg :: Nil
          }
        }

      // TODO: we should check that all these are distinct
      val packToLibName: Map[PackageName, Name] =
        (exportedPacks.iterator.map { p => (p.name, name) } ++
          (publicDepLibs.iterator ++ privateDepLibs.iterator).flatMap { lib =>
            lib.exportedIfaces.flatMap { iface => PackageName.parse(ProtoConverter.iname(iface)) }
              .map((_, Name(lib.name)))
          }
        )
        .toMap

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
                inv(Error.VersionNotAdjacent(prevVersion, nextVersion))
              }
            case None =>
              inv(Error.InvalidPreviousLib("missing version", prevLib))
          }
        case None =>
          // Then the version can be anything
          Validated.unit
      }

      // 4. the only packages that appear on exportedPackages apis are in exportedPackages or publicDeps
      val prop4 = {
        val validDepPacks: List[PackageName] =
          PackageName.PredefName :: exportedPacks.map(_.name) :::
            publicDepLibs.flatMap { lib =>
              lib.exportedIfaces.flatMap { iface =>
                PackageName.parse(ProtoConverter.iname(iface))
              }  
            }

        val validSet = validDepPacks.toSet

        exportedPacks.traverse_ { p =>
          val depOn = p.visibleDepPackages 
          val invalidDeps = depOn.filterNot(validSet)
          invalidDeps.traverse_ { badPn =>
            val msg = 
              if (privateDepLibs.exists(_.exportedIfaces.exists { iface => ProtoConverter.iname(iface) === badPn.asString })) "package from private dependency"
              else if (privatePacks.exists(_.name === badPn)) "private package in this library"
              else "unknown package"

            inv(Error.IllegalVisibleDep(msg, p, badPn))
          }
        }
      }

      val usedBy: Map[Option[Name], NonEmptyList[PackageName]] =
        (for {
          p <- packs
          visPack <- p.allImportPacks
          libName = packToLibName.get(visPack)
        } yield (libName, p)).groupByNel(_._1).view.mapValues(_.map(_._2.name)).toMap

      // 5. all public deps appear somewhere on an API
      val prop5 = {
        val exportedLibs = (for {
          p <- exportedPacks
          visPack <- p.visibleDepPackages
          libName = packToLibName.get(visPack)
        } yield libName).toSet

        publicDeps.traverse_ { dep =>
          val optName = Some(Name(dep.name))
          if (exportedLibs.contains(optName)) Validated.unit 
          else if (usedBy.contains(optName)) {
            // this should be a private dep
            inv(Error.PrivateDepMarkedPublic(dep))
          }
          else {
            // this is unused
            inv(Error.UnusedPublicDep(dep))
          }
        }
      }
      // 6. all private deps are used somewhere
      val prop6 = 
        privateDeps.traverse_ { dep =>
          val optName = Some(Name(dep.name))
          if (usedBy.contains(optName)) Validated.unit
          else {
            // this is unused private
            inv(Error.UnusedPrivateDep(dep))
          }
        }

      /*
       * 7. hashes of dependencies match
       * 8. there are no duplicate named dependencies
       */
      val prop7_8 = {
        val pubs = publicDeps.groupByNel(_.name)
        val privs = privateDeps.groupByNel(_.name)
        // nothing is private and public
        val both = pubs.keySet & privs.keySet
        val noOverlap =
          both.traverse_ { o =>
            inv(Error.DuplicateDep("both public and private", o, proto.LibDescriptor()))
          }

        val allOne = pubs.traverse_(libs =>
          libs.tail.traverse_(e => inv(Error.DuplicateDep(
            "public dep",
            e.name, e.desc.getOrElse(proto.LibDescriptor()))))
        ) *> privs.traverse_(libs =>
          libs.tail.traverse_(e => inv(Error.DuplicateDep("private dep", e.name,
            e.desc.getOrElse(proto.LibDescriptor()))))
        ) *> nameToDep.traverse_(libs =>
          libs.tail.traverse_(e => inv(Error.DuplicateDep("argument dep", e.arg.name,
            e.arg.descriptor.getOrElse(proto.LibDescriptor()))))
        )

        def checkDep(note: String, dep: proto.LibDependency) = 
          nameToDep.get(dep.name) match {
            case None =>
              inv(Error.MissingDep(note = note, dep = dep))
            case Some(deps) =>
              val hash = deps.head.hash.toIdent
              if (dep.desc.exists(_.hashes.exists(_ == hash))) Validated.unit
              else {
                // the hash doesn't match
                inv(Error.DepHashMismatch(
                  note,
                  dep,
                  deps.head.hash,
                  deps.head.arg
                ))
              }
          }

        val pubGood = pubs.traverse_ { libs => checkDep("public dep", libs.head) }
        val privGood = privs.traverse_ { libs => checkDep("private dep", libs.head) }

        noOverlap *> allOne *> pubGood *> privGood
      }

      // 9. there is a valid solution for transitive dependencies
      val prop9 =
        LibConfig.unusedTransitiveDeps(publicDepLibs)

      prop1 *> prop2 *> prop3 *> prop4 *> prop5 *> prop6 *> prop7_8 *> prop9.map(ValidationResult(_))
    }

  // just build the library without any validations
  def unvalidatedAssemble(previous: Option[Hashed[Algo.Blake3, proto.Library]], vcsIdent: String, packs: List[Package.Typed[Unit]], unusedTrans: List[proto.LibDependency]): Either[Throwable, proto.Library] = {
    val depth = previous match {
      case None => 0
      case Some(Hashed(_, prevLib)) => prevLib.depth + 1
    }

    val thisHistory = previous match {
      case None => proto.LibHistory()
      case Some(Hashed(hash, p)) =>
        val prevHistory = p.history.getOrElse(proto.LibHistory())
        val v = p.descriptor match {
          case Some(desc) =>
            desc.version match {
              case Some(v) => v
              case None => 
                // this should never happen after validation
                return Left(new Exception(s"invalid previous missing version: $p"))
            }
          case None =>
            // this should never happen after validation
            return Left(new Exception(s"invalid previous missing descriptor: $p"))
        }
        val desc = proto.LibDescriptor(
          version = Some(v),
          hashes = List(hash.toIdent)
        )
        prevHistory.nextHistory(desc, nextVersion)
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
        unusedTransitivePublicDependencies = unusedTrans,
        history = Some(thisHistory)
      )
    }
  }
}

object LibConfig {

  case class ValidationResult(unusedTransitiveDeps: SortedMap[String, proto.LibDependency])

  sealed abstract class Error
  object Error {
    case class ExtraPackages(nel: NonEmptyList[Package.Typed[Unit]]) extends Error
    case class ProtoError(error: Throwable) extends Error
    case class VersionNotIncreasing(note: String, previous: Version, current: Version) extends Error
    case class VersionNotAdjacent(previous: Version, current: Version) extends Error
    case class InvalidPreviousLib(note: String, previous: proto.Library) extends Error
    case class DuplicateDep(note: String, name: String, desc: proto.LibDescriptor) extends Error
    case class MissingDep(note: String, dep: proto.LibDependency) extends Error
    case class DepHashMismatch(note: String, dep: proto.LibDependency, foundHash: HashValue[Algo.Blake3], found: proto.Library) extends Error
    case class IllegalVisibleDep(note: String, pack: Package.Typed[Unit], invalid: PackageName) extends Error
    case class NoValidVersion(name: String, publicDep: Option[proto.LibDescriptor], versions: NonEmptyList[proto.LibDependency]) extends Error
    case class UnusedPublicDep(dep: proto.LibDependency) extends Error
    case class UnusedPrivateDep(dep: proto.LibDependency) extends Error
    case class PrivateDepMarkedPublic(dep: proto.LibDependency) extends Error

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
        case DuplicateDep(note, name, desc) => 
          Doc.text(s"duplicate dependency name=${name}, desc=${desc}: $note")
        case MissingDep(note, dep) =>
          Doc.text(s"dependency ${dep.name} not found in args: $note")
        case DepHashMismatch(note, dep, foundHash, _) =>
          Doc.text(s"hash mismatch: $note. lib name=${dep.name}, found hash=${foundHash.hex} expecteded ${dep.desc.toList.flatMap(_.hashes)}.")
        case IllegalVisibleDep(note, pack, invalid) =>
          Doc.text(show"illegate visible dep: $note. in package ${pack.name} non-public package name escapes: $invalid")
        case NoValidVersion(name: String, publicDep: Option[proto.LibDescriptor], versions: NonEmptyList[proto.LibDependency]) =>
          Doc.text(show"no valid common version of public transitive dep name=$name, public dependency = ${publicDep.flatMap(_.parsedVersion)}, transitive deps=${versions.map(_.desc.flatMap(_.parsedVersion))}")
        case UnusedPublicDep(dep: proto.LibDependency) =>
          Doc.text(show"public dep ${dep.name} is not used publicly or privately.")
        case UnusedPrivateDep(dep: proto.LibDependency) =>
          Doc.text(show"private dep ${dep.name} is not used publicly or privately.")
        case PrivateDepMarkedPublic(dep: proto.LibDependency) =>
          Doc.text(show"pubic dep ${dep.name} is only used privately.")
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

    def inv(e: Error): ValidatedNec[Error, Nothing] = Validated.invalidNec(e)
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

  /**
    * Compute the list of unused transitive dependencies if we can solve for them 
    */
  def unusedTransitiveDeps(publicDeps: List[proto.Library]): ValidatedNec[Error, SortedMap[String, proto.LibDependency]] = {
    val usedDeps = publicDeps.iterator.map { lib => (lib.name, lib.descriptor) }.toMap
    
    val allTransitiveDeps = (
          publicDeps.map(_.toDep) :::
          publicDeps.flatMap(_.unusedTransitivePublicDependencies)
        )
        .groupByNel(_.name)

    allTransitiveDeps.traverse { deps =>
      val name = deps.head.name  
      val selectedVersion = deps.head.desc.flatMap(_.parsedVersion).flatMap { v0 =>
        deps.tail.foldM(v0) { (max, dep) =>
          dep.desc.flatMap(_.parsedVersion).flatMap(cats.PartialOrder.pmax(max, _))
        }
      }

      selectedVersion match {
        case None =>
          Error.inv(Error.NoValidVersion(name, usedDeps.get(name).flatten, deps))
        case Some(v) =>
          val selectedDep = deps.find { dep => dep.desc.flatMap(_.parsedVersion) === Some(v) }
          selectedDep match {
            case Some(dep) => Validated.valid(dep)
            case None => sys.error(s"invariant violation: selected a version that isn't there: $deps")
          }
      }
    }
    .map { allGoodDeps =>
      allGoodDeps.filterNot { case (k, _) => usedDeps.contains(k) }
    }
  }

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

    def nextHistory(prevDesc: proto.LibDescriptor, nextVersion: Version): proto.LibHistory = {
      val prevOptV = prevDesc.version
      val prevVersion = prevOptV.map(Version.fromProto(_)).getOrElse(Version.zero)
      require(Ordering[Version].lt(prevVersion, nextVersion), s"invalid version ordering: $prevVersion not < $nextVersion")

      if (prevVersion.major == nextVersion.major) {
        if (prevVersion.minor == nextVersion.minor) {
          if (prevVersion.patch == nextVersion.patch) {
            // must be pre-release
            val all = allDescriptors.filterNot{desc => (desc.version != prevOptV) &&
              (desc.version != history.previousMajor.flatMap(_.version)) &&
              (desc.version != history.previousMinor.flatMap(_.version)) &&
              (desc.version != history.previousPrerelease.flatMap(_.version))
            }
            proto.LibHistory(
              previousMajor = history.previousMajor,
              previousMinor = history.previousMinor,
              previousPatch = history.previousPatch,
              previousPrerelease = Some(prevDesc),
              others = all.sortBy(_.version.map(Version.fromProto(_)))
            )
          }
          else {
            // we are bumping patch
            val all = allDescriptors.filterNot{desc => (desc.version != prevOptV) &&
              (desc.version != history.previousMajor.flatMap(_.version)) &&
              (desc.version != history.previousMinor.flatMap(_.version))
            }
            proto.LibHistory(
              previousMajor = history.previousMajor,
              previousMinor = history.previousMinor,
              previousPatch = Some(prevDesc),
              others = all.sortBy(_.version.map(Version.fromProto(_)))
            )
          }
        }
        else {
          // we are bumping minor
          val all = allDescriptors.filterNot{desc => (desc.version != prevOptV) &&
            (desc.version != history.previousMajor.flatMap(_.version))
          }
          proto.LibHistory(
            previousMajor = history.previousMajor,
            previousMinor = Some(prevDesc),
            others = all.sortBy(_.version.map(Version.fromProto(_)))
          )
        }
      }
      else {
        // we are bumping major versions
        val all = allDescriptors.filterNot(_.version != prevOptV)
        proto.LibHistory(
          previousMajor = Some(prevDesc),
          others = all.sortBy(_.version.map(Version.fromProto(_)))
        )
      }
    }
  }

  implicit class LibDescriptorMethods(private val desc: proto.LibDescriptor) extends AnyVal {
    def parsedVersion: Option[Version] =
      desc.version.map(Version.fromProto(_))
      
    def versionOrZero: Version =
      parsedVersion match {
        case Some(v) => v
        case None => Version.zero
      }
  }

  implicit class LibMethods(private val lib: proto.Library) extends AnyVal {
    def toDep: proto.LibDependency =
      proto.LibDependency(name = lib.name, desc = lib.descriptor)

    def version: Option[Version] = lib.descriptor.flatMap(_.parsedVersion)
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