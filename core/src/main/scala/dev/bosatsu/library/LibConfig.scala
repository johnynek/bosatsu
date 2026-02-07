package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.Semigroup
import cats.data.{NonEmptyChain, NonEmptyList, Validated, ValidatedNec}
import cats.syntax.all._
import java.util.regex.Pattern
import java.util.regex.PatternSyntaxException
import dev.bosatsu.{
  Json,
  Kind,
  Package,
  PackageMap,
  PackageName,
  ProtoConverter,
  Require
}
import dev.bosatsu.tool.CliException
import dev.bosatsu.rankn.TypeEnv
import dev.bosatsu.hashing.{HashValue, Algo}
import org.typelevel.paiges.{Doc, Document}
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.SortedMap

import LibConfig.{
  Error,
  LibMethods,
  LibHistoryMethods,
  LibPath,
  LibRef,
  ValidationResult
}

case class LibConfig(
    name: Name,
    repoUri: String,
    nextVersion: Version,
    previous: Option[proto.LibDescriptor],
    exportedPackages: List[LibConfig.PackageFilter],
    allPackages: List[LibConfig.PackageFilter],
    publicDeps: List[proto.LibDependency],
    privateDeps: List[proto.LibDependency],
    defaultMain: Option[PackageName]
) {

  /** validate then unvalidatedAssemble
    */
  def assemble(
      vcsIdent: String,
      previous: Option[DecodedLibrary[Algo.Blake3]],
      packs: List[Package.Typed[Any]],
      deps: List[DecodedLibrary[Algo.Blake3]],
      publicDepClosureLibs: List[DecodedLibrary[Algo.Blake3]],
      prevPublicDepLibs: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, proto.Library] =
    validate(previous, packs, deps, publicDepClosureLibs, prevPublicDepLibs)
      .andThen { vr =>
        unvalidatedAssemble(
          previous,
          vcsIdent,
          packs,
          vr.unusedTransitiveDeps.iterator.map(_._2).toList
        ) match {
          case Right(value) => Validated.valid(value)
          case Left(err) => Validated.invalidNec(Error.ProtoError(err): Error)
        }
      }

  // Validates that the maximum version in history < nextVersion
  def validatePreviousHist(
      previous: Option[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, Unit] = {
    import Error.inv
    val withPrev = previous.traverse_ { dec =>
      val p = dec.protoLib
      val prevLt = p.version match {
        case Some(prevV) =>
          if (Ordering[Version].lt(prevV, nextVersion)) Validated.unit
          else
            inv(
              Error.VersionNotIncreasing("previous library", prevV, nextVersion)
            )
        case None =>
          inv(Error.InvalidPreviousLib("missing version", p))
      }

      val histLt = p.history match {
        case Some(history) =>
          history.allVersions.traverse_ { prevV =>
            if (Ordering[Version].lt(prevV, nextVersion)) Validated.unit
            else
              inv(
                Error
                  .VersionNotIncreasing("history version", prevV, nextVersion)
              )
          }
        case None => Validated.unit
      }

      prevLt *> histLt
    }

    val matchesPrev =
      (previous, this.previous) match {
        case (None, None)            => Validated.unit
        case (Some(dec), Some(desc)) =>
          val decV = dec.protoLib.version
          val expectedV = desc.version.map(Version.fromProto(_))
          if (decV === expectedV) {
            val hashIdent = dec.hashValue.toIdent
            if (desc.hashes.exists(_ == hashIdent)) {
              Validated.unit
            } else {
              inv(
                Error.InvalidPreviousLib(
                  show"declared hashes ${desc.hashes} in config doesn't match binary previous=${hashIdent}",
                  dec.protoLib
                )
              )
            }
          } else {
            // the previous version doesn't match
            inv(
              Error.InvalidPreviousLib(
                show"declared previous version=${expectedV} in config doesn't match binary previous=${decV}",
                dec.protoLib
              )
            )
          }

        case (None, Some(dec)) =>
          inv(Error.MissingExpectedPrevious(dec))
        case (Some(dec), None) =>
          inv(
            Error.InvalidPreviousLib(
              show"config doesn't declare a previous, but one was passed.",
              dec.protoLib
            )
          )
      }

    withPrev *> matchesPrev
  }

  /** This checks the following properties and if they are set, builds the
    * library
    *   7. hashes of dependencies match
    *   8. there are no duplicate named dependencies
    *   9. there is a valid solution for transitive dependencies
    */
  def validateDeps(
      deps: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, ValidationResult] = {
    import Error.inv

    val nameToDep = deps.groupByNel(_.protoLib.name)

    val publicDepLibs: List[DecodedLibrary[Algo.Blake3]] =
      publicDeps.flatMap { dep =>
        nameToDep.get(dep.name) match {
          case None =>
            // this will be a validation error later
            Nil
          case Some(libs) => libs.head :: Nil
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
          inv(
            Error.DuplicateDep(
              "both public and private",
              o,
              proto.LibDescriptor()
            )
          )
        }

      val allOne = pubs.traverse_(libs =>
        libs.tail.traverse_(e =>
          inv(
            Error.DuplicateDep(
              "public dep",
              e.name,
              e.desc.getOrElse(proto.LibDescriptor())
            )
          )
        )
      ) *> privs.traverse_(libs =>
        libs.tail.traverse_(e =>
          inv(
            Error.DuplicateDep(
              "private dep",
              e.name,
              e.desc.getOrElse(proto.LibDescriptor())
            )
          )
        )
      ) *> nameToDep.traverse_(libs =>
        libs.tail.traverse_(e =>
          inv(
            Error.DuplicateDep(
              "argument dep",
              e.protoLib.name,
              e.protoLib.descriptor.getOrElse(proto.LibDescriptor())
            )
          )
        )
      )

      def checkDep(note: String, dep: proto.LibDependency) =
        nameToDep.get(dep.name) match {
          case None =>
            inv(Error.MissingDep(note = note, dep = dep))
          case Some(deps) =>
            val hash = deps.head.hashValue.toIdent
            if (dep.desc.exists(_.hashes.exists(_ == hash))) Validated.unit
            else {
              // the hash doesn't match
              inv(
                Error.DepHashMismatch(
                  note,
                  dep,
                  deps.head.hashValue,
                  deps.head.protoLib
                )
              )
            }
        }

      val pubGood = pubs.traverse_(libs => checkDep("public dep", libs.head))
      val privGood = privs.traverse_(libs => checkDep("private dep", libs.head))

      noOverlap *> allOne *> pubGood *> privGood
    }

    // 9. there is a valid solution for transitive dependencies
    val prop9 =
      LibConfig.unusedTransitiveDeps(publicDepLibs)

    prop7_8 *> prop9.map(ValidationResult(_))
  }

  /** This checks the following properties and if they are set, builds the
    * library
    *   1. all the included packs are set in allPackages
    *   2. no public package is exported by more than one library in the public
    *      dependency closure of public and private dependencies
    *   3. the version is semver compatible with previous
    *   4. the only packages that appear on exportedPackages apis are in
    *      exportedPackages or publicDeps
    *   5. all public deps appear somewhere on an API
    *   6. all private deps are used somewhere
    */
  def validatePacks(
      previous: Option[DecodedLibrary[Algo.Blake3]],
      packs: List[Package.Typed[Any]],
      deps: List[DecodedLibrary[Algo.Blake3]],
      publicDepClosureLibs: List[DecodedLibrary[Algo.Blake3]],
      prevPublicDepLibs: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, Unit] = {

    import Error.inv
    val packsNoPredef =
      packs.filterNot(_.name == PackageName.PredefName)

    val exportedPacks: List[Package.Typed[Any]] =
      packsNoPredef.filter(p => exportedPackages.exists(_.accepts(p.name)))

    val privatePacks: List[Package.Typed[Any]] =
      packsNoPredef.filter(p => allPackages.exists(_.accepts(p.name)))

    val nameToDep = deps.groupByNel(_.protoLib.name)

    val publicDepLibs: List[DecodedLibrary[Algo.Blake3]] =
      publicDeps.flatMap { dep =>
        nameToDep.get(dep.name) match {
          case None =>
            // this will be a validation error later
            Nil
          case Some(libs) => libs.head :: Nil
        }
      }

    val privateDepLibs: List[DecodedLibrary[Algo.Blake3]] =
      privateDeps.flatMap { dep =>
        nameToDep.get(dep.name) match {
          case None =>
            // this will be a validation error later
            Nil
          case Some(libs) => libs.head :: Nil
        }
      }

    val thisRef = LibRef(name, nextVersion)

    def depVersion(dep: proto.LibDependency): Version =
      dep.desc.flatMap(_.version) match {
        case None    => Version.zero
        case Some(v) => Version.fromProto(v)
      }

    val depMap: Map[LibRef, DecodedLibrary[Algo.Blake3]] =
      publicDepClosureLibs.iterator.map { lib =>
        (LibRef(lib.name, lib.version), lib)
      }.toMap

    val directRefs: List[LibRef] =
      (publicDeps ::: privateDeps).flatMap { dep =>
        val ref = LibRef(Name(dep.name), depVersion(dep))
        if (depMap.contains(ref)) ref :: Nil else Nil
      }.distinct

    @annotation.tailrec
    def bfs(
        todo: List[(LibRef, NonEmptyList[LibRef])],
        seen: Map[LibRef, NonEmptyList[LibRef]]
    ): Map[LibRef, NonEmptyList[LibRef]] =
      todo match {
        case Nil                 => seen
        case (ref, path) :: rest =>
          if (seen.contains(ref)) bfs(rest, seen)
          else {
            val next = depMap.get(ref).toList.flatMap { lib =>
              val deps =
                lib.protoLib.publicDependencies.toList :::
                  lib.protoLib.unusedTransitivePublicDependencies.toList
              deps.flatMap { dep =>
                val depRef = LibRef(Name(dep.name), depVersion(dep))
                if (depMap.contains(depRef)) (depRef, path :+ depRef) :: Nil
                else Nil
              }
            }
            bfs(rest ::: next, seen.updated(ref, path))
          }
      }

    val depPathMap: Map[LibRef, NonEmptyList[LibRef]] = {
      val init = directRefs.map(ref => (ref, NonEmptyList.of(thisRef, ref)))
      bfs(init, Map(thisRef -> NonEmptyList.one(thisRef)))
    }

    def pathFor(ref: LibRef): NonEmptyList[LibRef] =
      depPathMap.getOrElse(
        ref,
        if (ref == thisRef) NonEmptyList.one(thisRef)
        else NonEmptyList.of(thisRef, ref)
      )

    def duplicatePackageCheck(
        note: String,
        packs: Iterable[(PackageName, LibRef)]
    ): ValidatedNec[Error, Unit] = {
      val byPack =
        packs.toList.groupBy(_._1).iterator.flatMap { case (pn, pairs) =>
          val libs = pairs.iterator.map(_._2).toList.distinct
          val paths = libs.map(ref => LibPath(ref, pathFor(ref)))
          NonEmptyList.fromList(paths).filter(_.tail.nonEmpty).map(pn -> _)
        }

      byPack.toList
        .traverse_ { case (pn, libs) =>
          inv(Error.DuplicatePackage(note, pn, libs))
        }
    }

    val publicClosurePacks =
      publicDepClosureLibs.iterator.flatMap { lib =>
        val ref = LibRef(lib.name, lib.version)
        lib.interfaces.map(iface => (iface.name, ref))
      }.toList

    val directPacks =
      (exportedPacks.iterator.map(p => (p.name, thisRef)) ++
        (publicDepLibs.iterator ++ privateDepLibs.iterator).flatMap { lib =>
          val ref = LibRef(lib.name, lib.version)
          lib.interfaces.map(iface => (iface.name, ref))
        }).toList

    val publicClosurePairs =
      (exportedPacks.iterator.map(p =>
        (p.name, thisRef)
      ) ++ publicClosurePacks).toList

    val prop2 =
      duplicatePackageCheck("public dependency closure", publicClosurePairs)

    val packToLibNameV =
      duplicatePackageCheck("dependencies", directPacks).andThen { _ =>
        Validated.valid(
          directPacks.iterator.map { case (pn, ref) => (pn, ref.name) }.toMap
        )
      }

    val prop1 =
      packsNoPredef.filterNot(p => allPackages.exists(_.accepts(p.name))) match {
        case Nil    => Validated.unit
        case h :: t => inv(Error.ExtraPackages(NonEmptyList(h, t)))
      }

    val prop3 = previous match {
      case Some(dec) =>
        val prevLib = dec.protoLib
        prevLib.version match {
          case Some(prevVersion) =>
            if (prevVersion.justBefore(nextVersion)) {
              val dk = prevVersion.diffKindTo(nextVersion)
              if (!dk.isMajor) {
                val diff = LibConfig.validNextVersion(
                  dec,
                  dk,
                  exportedPacks,
                  publicDepLibs,
                  prevPublicDepLibs
                )

                if (diff.isValid) diff
                else {
                  // suggest a version that will work
                  val valid =
                    if (
                      dk.isPatch &&
                      LibConfig
                        .validNextVersion(
                          dec,
                          Version.DiffKind.Minor,
                          exportedPacks,
                          publicDepLibs,
                          prevPublicDepLibs
                        )
                        .isValid
                    ) {
                      // if it's patch and minor would have worked
                      Version.DiffKind.Minor
                    } else {
                      Version.DiffKind.Major
                    }

                  diff *> inv(
                    Error.MinimumValidVersion(
                      prevVersion,
                      prevVersion.next(valid)
                    )
                  )
                }
              } else {
                // a major version change allows anything
                Validated.unit
              }
            } else {
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
          publicDepLibs.flatMap(lib => lib.interfaces.map(_.name))

      val validSet = validDepPacks.toSet

      exportedPacks.traverse_ { p =>
        val depOn = p.visibleDepPackages
        val invalidDeps = depOn.filterNot(validSet)
        invalidDeps.traverse_ { badPn =>
          val msg =
            if (
              privateDepLibs.exists(_.interfaces.exists { iface =>
                iface.name == badPn
              })
            ) "package from private dependency"
            else if (privatePacks.exists(_.name == badPn))
              "private package in this library"
            else "unknown package"

          inv(Error.IllegalVisibleDep(msg, p, badPn))
        }
      }
    }

    val prop5_6 = packToLibNameV.andThen { packToLibName =>
      val usedBy: Map[Option[Name], NonEmptyList[PackageName]] =
        (for {
          p <- packsNoPredef
          visPack <- p.allImportPacks
          libName = packToLibName.get(visPack)
        } yield (libName, p))
          .groupByNel(_._1)
          .view
          .mapValues(_.map(_._2.name))
          .toMap

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
          } else {
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

      prop5 *> prop6
    }

    prop1 *> prop2 *> prop4.andThen(_ => prop3) *> prop5_6
  }

  /** call validatePacks, validatePreviousHist, validateDeps Note: we can cache
    * the results in principle of validateDeps and validatePreviousHist based on
    * the hash of the config and the hash of the library inputs. So, we only
    * need to validatePacks on change of code (that said, those validations may
    * be so fast you don't care).
    */
  def validate(
      previous: Option[DecodedLibrary[Algo.Blake3]],
      packs: List[Package.Typed[Any]],
      deps: List[DecodedLibrary[Algo.Blake3]],
      publicDepClosureLibs: List[DecodedLibrary[Algo.Blake3]],
      prevPublicDepLibs: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, ValidationResult] =
    validatePacks(
      previous,
      packs,
      deps,
      publicDepClosureLibs,
      prevPublicDepLibs
    ) *>
      validatePreviousHist(previous) *>
      validateDeps(deps)

  // just build the library without any validations
  def unvalidatedAssemble[A](
      previous: Option[DecodedLibrary[Algo.Blake3]],
      vcsIdent: String,
      packs: List[Package.Typed[A]],
      unusedTrans: List[proto.LibDependency]
  ): Either[Throwable, proto.Library] = {
    val depth = previous match {
      case None      => 0
      case Some(dec) => dec.protoLib.depth + 1
    }

    val thisHistory = previous match {
      case None      => proto.LibHistory()
      case Some(dec) =>
        val desc = proto.LibDescriptor(
          version = Some(dec.version.toProto),
          hashes = List(dec.hashValue.toIdent)
        )
        val p = dec.protoLib
        val prevHistory = p.history.getOrElse(proto.LibHistory())
        prevHistory.nextHistory(desc, nextVersion)
    }

    val sortPack = packs.sortBy(_.name)
    val ifs = sortPack.traverseFilter { pack =>
      if (exportedPackages.exists(_.accepts(pack.name))) {
        val iface = pack.toIface
        ProtoConverter.interfaceToProto(iface).toEither.map(Some(_))
      } else Right(None)
    }

    val protoPacksE =
      sortPack.traverse(ProtoConverter.packageToProto(_).toEither)

    (ifs, protoPacksE).mapN { (ifaces, protoPacks) =>
      proto.Library(
        name = name.name,
        depth = depth,
        vcsIndent = vcsIdent,
        repoUri = repoUri,
        descriptor =
          Some(proto.LibDescriptor(version = Some(nextVersion.toProto))),
        exportedIfaces = ifaces,
        internalPackages = protoPacks,
        publicDependencies = publicDeps.sortBy(_.name),
        privateDependencies = privateDeps.sortBy(_.name),
        unusedTransitivePublicDependencies = unusedTrans,
        history = Some(thisHistory),
        defaultMain = defaultMain.fold("")(_.asString)
      )
    }
  }
}

object LibConfig {

  case class LibRef(name: Name, version: Version) derives CanEqual
  case class LibPath(ref: LibRef, path: NonEmptyList[LibRef])

  case class ValidationResult(
      unusedTransitiveDeps: SortedMap[String, proto.LibDependency]
  )

  sealed abstract class Error
  object Error {
    case class ExtraPackages(nel: NonEmptyList[Package.Typed[Any]])
        extends Error
    case class ProtoError(error: Throwable) extends Error
    case class VersionNotIncreasing(
        note: String,
        previous: Version,
        current: Version
    ) extends Error
    case class VersionNotAdjacent(previous: Version, current: Version)
        extends Error
    case class InvalidPreviousLib(note: String, previous: proto.Library)
        extends Error
    case class DuplicateDep(
        note: String,
        name: String,
        desc: proto.LibDescriptor
    ) extends Error
    case class DuplicatePackage(
        note: String,
        pack: PackageName,
        libs: NonEmptyList[LibPath]
    ) extends Error
    case class MissingDep(note: String, dep: proto.LibDependency) extends Error
    case class DepHashMismatch(
        note: String,
        dep: proto.LibDependency,
        foundHash: HashValue[Algo.Blake3],
        found: proto.Library
    ) extends Error
    case class IllegalVisibleDep(
        note: String,
        pack: Package.Typed[Any],
        invalid: PackageName
    ) extends Error
    case class NoValidVersion(
        name: String,
        publicDep: Option[proto.LibDescriptor],
        versions: NonEmptyList[proto.LibDependency]
    ) extends Error
    case class UnusedPublicDep(dep: proto.LibDependency) extends Error
    case class UnusedPrivateDep(dep: proto.LibDependency) extends Error
    case class PrivateDepMarkedPublic(dep: proto.LibDependency) extends Error
    case class DeletedPublicDependency(dep: proto.LibDependency) extends Error
    case class AddedPublicDependencyInPatch(dep: proto.LibDependency)
        extends Error
    case class InvalidPublicDepChange(
        name: String,
        diffKind: Version.DiffKind,
        oldDeps: NonEmptyList[proto.LibDependency],
        newDeps: NonEmptyList[proto.LibDependency]
    ) extends Error
    case class DowngradedPublicDependency(
        name: String,
        previous: Version,
        downgrades: NonEmptyList[Version]
    ) extends Error
    case class CannotDecodeLibraryIfaces(lib: proto.Library, err: Throwable)
        extends Error
    case class InvalidDiff(diffKind: Version.DiffKind, diff: Diff) extends Error
    case class ApiDiffError(err: ApiDiff.Error) extends Error
    case class MinimumValidVersion(prevVersion: Version, minimumValid: Version)
        extends Error
    case class MissingExpectedPrevious(desc: proto.LibDescriptor) extends Error

    def errorsToDoc(nec: NonEmptyChain[Error]): Doc = {
      val errors = nec.toChain.toList
      val (summaryErrors, numberedErrors) = errors.partition {
        case MinimumValidVersion(_, _) => true
        case _                         => false
      }

      def numberedDoc(errs: List[Error]): Option[Doc] =
        NonEmptyChain
          .fromSeq(errs)
          .map { nec =>
            Doc.intercalate(
              Doc.hardLine + Doc.hardLine,
              nec.toChain.toList.mapWithIndex((e, idx) =>
                Doc.text(s"${idx + 1}. ") + docError.document(e).nested(4)
              )
            )
          }

      def summaryDoc(errs: List[Error]): Option[Doc] =
        NonEmptyChain
          .fromSeq(errs)
          .map(nec =>
            Doc.intercalate(
              Doc.hardLine,
              nec.toChain.toList.map(docError.document)
            )
          )

      (numberedDoc(numberedErrors), summaryDoc(summaryErrors)) match {
        case (Some(n), Some(s)) => n + Doc.hardLine + Doc.hardLine + s
        case (Some(n), None)    => n
        case (None, Some(s))    => s
        case (None, None)       => Doc.empty
      }
    }

    implicit val docError: Document[Error] =
      Document.instance {
        case ExtraPackages(nel) =>
          Doc.text("unexpected extra packages: ") +
            Doc
              .intercalate(
                Doc.comma + Doc.line,
                nel.toList.map(p => Doc.text(show"${p.name}"))
              )
              .grouped
              .nested(4)
        case VersionNotIncreasing(note, previous, current) =>
          Doc.text(show"version not increasing $note:") + (Doc.line +
            Doc.text(show"$previous is not less than $current")).grouped
            .nested(4)
        case VersionNotAdjacent(previous, current) =>
          Doc.text(show"previous version not adjacent:") + (Doc.line +
            Doc.text(show"$previous") + Doc.line + Doc.text(
              "is not adjacent to"
            ) + Doc.line + Doc.text(show"$current")).grouped.nested(4)
        case InvalidPreviousLib(note, _) =>
          Doc.text(s"invalid previous library: $note")
        case ProtoError(e) =>
          Doc.text(s"error encoding to proto: ${e.getMessage}")
        case DuplicateDep(note, name, desc) =>
          Doc.text(s"duplicate dependency name=${name}, desc=${desc}: $note")
        case DuplicatePackage(note, pack, libs) =>
          def showRef(ref: LibRef): String = show"${ref.name}:${ref.version}"
          def showPath(path: NonEmptyList[LibRef]): String =
            path.toList.map(showRef).mkString(" -> ")

          val items = libs.toList.map { lp =>
            Doc.text(showRef(lp.ref)) + Doc.text(" via ") + Doc.text(
              showPath(lp.path)
            )
          }

          Doc.text(
            show"package $pack exported by libraries ($note):"
          ) + Doc.line +
            Doc.intercalate(Doc.line, items).nested(4)
        case MissingDep(note, dep) =>
          Doc.text(s"dependency ${dep.name} not found in args: $note")
        case DepHashMismatch(note, dep, foundHash, _) =>
          Doc.text(
            s"hash mismatch: $note. lib name=${dep.name}, found hash=${foundHash.hex} expecteded ${dep.desc.toList.flatMap(_.hashes)}."
          )
        case IllegalVisibleDep(note, pack, invalid) =>
          Doc.text(
            show"illegate visible dep: $note. in package ${pack.name} non-public package name escapes: $invalid"
          )
        case NoValidVersion(
              name: String,
              publicDep: Option[proto.LibDescriptor],
              versions: NonEmptyList[proto.LibDependency]
            ) =>
          Doc.text(
            show"no valid common version of public transitive dep name=$name, public dependency = ${publicDep.flatMap(_.parsedVersion)}, transitive deps=${versions.map(_.desc.flatMap(_.parsedVersion))}"
          )
        case UnusedPublicDep(dep: proto.LibDependency) =>
          Doc.text(
            show"public dep ${dep.name} is not used publicly or privately."
          )
        case UnusedPrivateDep(dep: proto.LibDependency) =>
          Doc.text(
            show"private dep ${dep.name} is not used publicly or privately."
          )
        case PrivateDepMarkedPublic(dep: proto.LibDependency) =>
          Doc.text(show"pubic dep ${dep.name} is only used privately.")
        case DeletedPublicDependency(dep: proto.LibDependency) =>
          Doc.text(
            show"deleted a public dependency: ${dep.name} outside of a major bump."
          )
        case AddedPublicDependencyInPatch(dep: proto.LibDependency) =>
          Doc.text(
            show"added a public dependency: ${dep.name} in a patch bump."
          )
        case InvalidPublicDepChange(
              name: String,
              diffKind: Version.DiffKind,
              oldDeps: NonEmptyList[proto.LibDependency],
              newDeps: NonEmptyList[proto.LibDependency]
            ) =>
          def vs(
              nel: NonEmptyList[proto.LibDependency]
          ): NonEmptyList[Option[Version]] =
            nel.map(_.desc.flatMap(_.version).map(Version.fromProto(_)))

          Doc.text(
            show"dependency $name isn't a valid ${diffKind.name} change: old=${vs(oldDeps)} new=${vs(newDeps)})"
          )
        case DowngradedPublicDependency(name, previous, downgrades) =>
          Doc.text(
            show"dependency $name was downgraded from $previous to ${downgrades.toList.mkString(", ")}"
          )
        case CannotDecodeLibraryIfaces(lib, err) =>
          Doc.text(
            show"cannot decode library interfaces in ${lib.name}: ${err.getMessage}"
          )

        case InvalidDiff(diffKind: Version.DiffKind, diff: Diff) =>
          (
            Doc.text(
              show"when doing ${diffKind.name} invalid diff:"
            ) + Doc.line + diff.toDoc
          ).nested(4).grouped
        case ApiDiffError(err) =>
          (Doc.text("api diff error:") + Doc.line + err.toDoc).nested(4).grouped

        case MinimumValidVersion(prevVersion: Version, minimumValid: Version) =>
          Doc.text(
            show"previous version was $prevVersion require $minimumValid to accept the API changes"
          )
        case MissingExpectedPrevious(desc: proto.LibDescriptor) =>
          Doc.text(
            s"no previous library binary found, but config expects ${desc}"
          )
      }

    implicit val showError: cats.Show[Error] =
      cats.Show.show[Error](e => docError.document(e).render(80))

    def toTry[A](vnec: ValidatedNec[Error, A]): Try[A] =
      vnec match {
        case Validated.Valid(a)      => Success(a)
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
      def accepts(pn: PackageName): Boolean =
        pattern.matcher(pn.asString).matches()
      def asString: String = pattern.pattern()
    }

    def fromString(str: String): Either[String, PackageFilter] =
      PackageName.parse(str) match {
        case Some(value) => Right(Name(value))
        case None        =>
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

  def init(name: Name, repoUri: String, ver: Version): LibConfig =
    LibConfig(
      name = name,
      repoUri = repoUri,
      nextVersion = ver,
      previous = None,
      Nil,
      LibConfig.PackageFilter.Regex(Pattern.compile(".*")) :: Nil,
      Nil,
      Nil,
      None
    )

  /** Compute the list of unused transitive dependencies if we can solve for
    * them
    */
  def unusedTransitiveDeps(
      publicDeps: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, SortedMap[String, proto.LibDependency]] = {
    val usedDeps =
      publicDeps.iterator
        .map(lib => (lib.protoLib.name, lib.protoLib.descriptor))
        .toMap

    val allTransitiveDeps = (
      publicDeps.map(_.protoLib.toDep) :::
        publicDeps.flatMap(_.protoLib.unusedTransitivePublicDependencies)
    )
      .groupByNel(_.name)

    allTransitiveDeps
      .traverse { deps =>
        val name = deps.head.name
        val selectedVersion =
          deps.head.desc.flatMap(_.parsedVersion).flatMap { v0 =>
            deps.tail.foldM(v0) { (max, dep) =>
              dep.desc
                .flatMap(_.parsedVersion)
                .flatMap(cats.PartialOrder.pmax(max, _))
            }
          }

        selectedVersion match {
          case None =>
            Error.inv(
              Error.NoValidVersion(name, usedDeps.get(name).flatten, deps)
            )
          case Some(v) =>
            val selectedDep =
              deps.find(dep => dep.desc.flatMap(_.parsedVersion) === Some(v))
            selectedDep match {
              case Some(dep) => Validated.valid(dep)
              case None      =>
                sys.error(
                  s"invariant violation: selected a version that isn't there: $deps"
                )
            }
        }
      }
      .map { allGoodDeps =>
        allGoodDeps.filterNot { case (k, _) => usedDeps.contains(k) }
      }
  }

  def validNextVersion(
      prevDec: DecodedLibrary[Algo.Blake3],
      dk: Version.DiffKind,
      exportedPacks: List[Package.Typed[Any]],
      publicDepLibs: List[DecodedLibrary[Algo.Blake3]],
      prevPublicDepLibs: List[DecodedLibrary[Algo.Blake3]]
  ): ValidatedNec[Error, Unit] = {
    val prevLib = prevDec.protoLib
    val oldPublicVersions =
      (prevLib.publicDependencies.toList ::: prevLib.unusedTransitivePublicDependencies.toList)
        .groupByNel(_.name)
    val newPublicVersions =
      (publicDepLibs.map(_.protoLib.toDep) ::: publicDepLibs.flatMap(
        _.protoLib.unusedTransitivePublicDependencies
      )).distinct.groupByNel(_.name)

    val allLibNames = oldPublicVersions.keySet | newPublicVersions.keySet

    val compatPublicDepChange = allLibNames.toList.traverse_ { name =>
      (oldPublicVersions.get(name), newPublicVersions.get(name)) match {
        case (None, Some(newDeps)) =>
          if (dk.isPatch) {
            // we can't add new dependencies in a patch release
            Error.inv(Error.AddedPublicDependencyInPatch(newDeps.head))
          } else {
            // we can add new dependencies safely in minor and major bumps
            Validated.unit
          }
        case (Some(oldDeps), None) =>
          if (!dk.isMajor) {
            // we can't safely delete dependencies because we could later re-add an earlier version and that would be incompatible
            Error.inv(Error.DeletedPublicDependency(oldDeps.head))
          } else Validated.unit
        case (Some(oldDep), Some(newDep)) =>
          val oldV: Version = oldDep
            .map(
              _.desc
                .flatMap(_.version)
                .map(Version.fromProto(_))
                .getOrElse(Version.zero)
            )
            .reduce[Version](using
              Semigroup.instance(summon[cats.Order[Version]].max)
            )
          val newV = newDep.map(
            _.desc
              .flatMap(_.version)
              .map(Version.fromProto(_))
              .getOrElse(Version.zero)
          )
          val downgraded =
            newV.filter(v => cats.Order[Version].lt(v, oldV))
          NonEmptyList.fromList(downgraded) match {
            case Some(nel) =>
              Error.inv(
                Error.DowngradedPublicDependency(
                  name,
                  oldV,
                  nel
                )
              )
            case None =>
              if (newV.forall(v => oldV.diffKindTo(v) <= dk)) {
                Validated.unit
              } else {
                Error.inv(
                  Error.InvalidPublicDepChange(name, dk, oldDep, newDep)
                )
              }
          }
        case (None, None) =>
          sys.error("unreachable since name came from somewhere")
      }
    }

    val depTypes: TypeEnv[Kind.Arg] =
      publicDepLibs
        .foldMap { lib =>
          lib.interfaces.foldMap(_.exportedTypeEnv)
        }
    val predefTypes: TypeEnv[Kind.Arg] =
      Package.interfaceOf(PackageMap.predefCompiled).exportedTypeEnv

    val prevIfaces = prevDec.interfaces
    compatPublicDepChange *> {
      val prevExports = prevIfaces.iterator
        .map(iface => (iface.name, iface.exports))
        .to(SortedMap)
      val prevDepTypes: TypeEnv[Kind.Arg] =
        prevPublicDepLibs
          .foldMap { lib =>
            lib.interfaces.foldMap(_.exportedTypeEnv)
          }
      val prevTE = predefTypes ++ prevDepTypes ++ prevIfaces.foldMap(
        _.exportedTypeEnv
      )

      val currExports = exportedPacks.iterator
        .map(pack => (pack.name, pack.exports))
        .to(SortedMap)
      val currTE = predefTypes ++ depTypes ++ exportedPacks.foldMap(
        _.exportedTypeEnv
      )

      val diffV =
        ApiDiff(prevExports, prevTE, currExports, currTE).leftMap {
          _.map(Error.ApiDiffError(_))
        }

      diffV.andThen { diff =>
        val badDiffs = diff
          .badDiffs(dk) { diff =>
            Error.InvalidDiff(dk, diff)
          }
          .toVector

        badDiffs.traverse_(Error.inv(_))
      }
    }
  }

  implicit class LibHistoryMethods(private val history: proto.LibHistory)
      extends AnyVal {
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

    def nextHistory(
        prevDesc: proto.LibDescriptor,
        nextVersion: Version
    ): proto.LibHistory = {
      val prevOptV = prevDesc.version
      val prevVersion =
        prevOptV.map(Version.fromProto(_)).getOrElse(Version.zero)
      Require(
        Ordering[Version].lt(prevVersion, nextVersion),
        s"invalid version ordering: $prevVersion not < $nextVersion"
      )

      given cats.Eq[proto.Version] = cats.Eq.fromUniversalEquals

      def sameVersion(
          left: Option[proto.Version],
          right: Option[proto.Version]
      ): Boolean =
        (left, right) match {
          case (Some(l), Some(r)) => l === r
          case (None, None)       => true
          case _                  => false
        }

      if (prevVersion.major == nextVersion.major) {
        if (prevVersion.minor == nextVersion.minor) {
          if (prevVersion.patch == nextVersion.patch) {
            // must be pre-release
            val all = allDescriptors.filterNot { desc =>
              (!sameVersion(desc.version, prevOptV)) &&
              (!sameVersion(
                desc.version,
                history.previousMajor.flatMap(_.version)
              )) &&
              (!sameVersion(
                desc.version,
                history.previousMinor.flatMap(_.version)
              )) &&
              (!sameVersion(
                desc.version,
                history.previousPrerelease.flatMap(_.version)
              ))
            }
            proto.LibHistory(
              previousMajor = history.previousMajor,
              previousMinor = history.previousMinor,
              previousPatch = history.previousPatch,
              previousPrerelease = Some(prevDesc),
              others = all.sortBy(_.version.map(Version.fromProto(_)))
            )
          } else {
            // we are bumping patch
            val all = allDescriptors.filterNot { desc =>
              (!sameVersion(desc.version, prevOptV)) &&
              (!sameVersion(
                desc.version,
                history.previousMajor.flatMap(_.version)
              )) &&
              (!sameVersion(
                desc.version,
                history.previousMinor.flatMap(_.version)
              ))
            }
            proto.LibHistory(
              previousMajor = history.previousMajor,
              previousMinor = history.previousMinor,
              previousPatch = Some(prevDesc),
              others = all.sortBy(_.version.map(Version.fromProto(_)))
            )
          }
        } else {
          // we are bumping minor
          val all = allDescriptors.filterNot { desc =>
            (!sameVersion(desc.version, prevOptV)) &&
            (!sameVersion(
              desc.version,
              history.previousMajor.flatMap(_.version)
            ))
          }
          proto.LibHistory(
            previousMajor = history.previousMajor,
            previousMinor = Some(prevDesc),
            others = all.sortBy(_.version.map(Version.fromProto(_)))
          )
        }
      } else {
        // we are bumping major versions
        val all =
          allDescriptors.filter(desc => sameVersion(desc.version, prevOptV))
        proto.LibHistory(
          previousMajor = Some(prevDesc),
          others = all.sortBy(_.version.map(Version.fromProto(_)))
        )
      }
    }
  }

  implicit class LibDescriptorMethods(private val desc: proto.LibDescriptor)
      extends AnyVal {
    def parsedVersion: Option[Version] =
      desc.version.map(Version.fromProto(_))

    def versionOrZero: Version =
      parsedVersion match {
        case Some(v) => v
        case None    => Version.zero
      }
  }

  implicit class LibMethods(private val lib: proto.Library) extends AnyVal {
    def toDep: proto.LibDependency =
      proto.LibDependency(name = lib.name, desc = lib.descriptor)

    def version: Option[Version] = lib.descriptor.flatMap(_.parsedVersion)
  }

  implicit val libConfigWriter: Json.Writer[LibConfig] =
    Json.Writer.from[LibConfig] { lc =>
      import Json.Writer.write
      import lc._

      implicit val writePn: Json.Writer[PackageName] =
        Json.Writer[String].contramap[PackageName](_.asString)

      Json.JObject(
        ("name" -> write(name)) ::
          ("repo_uri" -> write(repoUri)) ::
          ("next_version" -> write(nextVersion)) ::
          (previous match {
            case None    => Nil
            case Some(p) => ("previous" -> write(p)) :: Nil
          }) :::
          ("exported_packages" -> write(exportedPackages)) ::
          ("all_packages" -> write(allPackages)) ::
          (if (publicDeps.isEmpty) Nil
           else ("public_deps" -> write(publicDeps)) :: Nil) :::
          (if (privateDeps.isEmpty) Nil
           else ("private_deps" -> write(privateDeps)) :: Nil) :::
          (defaultMain match {
            case None     => Nil
            case Some(dm) => ("default_main" -> write(dm)) :: Nil
          }) :::
          Nil
      )
    }
  implicit val libConfigReader: Json.Reader[LibConfig] =
    new Json.Reader.Obj[LibConfig] {
      implicit val readPn: Json.Reader[PackageName] =
        Json.Reader[String].mapEither("PackageName") { str =>
          PackageName.parse(str) match {
            case None        => Left(s"could not parse $str into package name")
            case Some(value) => Right(value)
          }
        }
      def describe: String = "LibConfig"
      def readObj(
          from: Json.Reader.FromObj
      ): Either[(String, Json, Json.Path), LibConfig] =
        for {
          name <- from.field[Name]("name")
          repoUri <- from.field[String]("repo_uri")
          nextVersion <- from.field[Version]("next_version")
          previous <- from.optional[proto.LibDescriptor]("previous")
          exportedPackages <- from.field[List[PackageFilter]](
            "exported_packages"
          )
          allPackages <- from.field[List[PackageFilter]]("all_packages")
          publicDeps <- from.optional[List[proto.LibDependency]]("public_deps")
          privateDeps <- from.optional[List[proto.LibDependency]](
            "private_deps"
          )
          defaultMain <- from.optional[PackageName]("default_main")
        } yield LibConfig(
          name = name,
          repoUri = repoUri,
          nextVersion = nextVersion,
          previous = previous,
          exportedPackages = exportedPackages,
          allPackages = allPackages,
          publicDeps = publicDeps.toList.flatten,
          privateDeps = privateDeps.toList.flatten,
          defaultMain = defaultMain
        )
    }
}
