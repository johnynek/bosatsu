package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.syntax.all._
import dev.bosatsu.rankn.{Type, TypeEnv}
import dev.bosatsu.tool.CliException
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/** The compile-time view of a group. Published libraries and implementation
  * bundles keep their full-fidelity codec.
  */
object PackageSignatures {
  def canonical(iface: Package.Interface): Package.Interface = {
    val publicConstructors = iface.exports.iterator.collect {
      case ExportedName.Constructor(_, Referant.Constructor(dt, _)) =>
        dt.toTypeConst
    }.toSet
    val exports = iface.exports.map {
      case ExportedName.TypeName(n, Referant.DefinedT(dt))
          if !dt.isOpaque && !publicConstructors(dt.toTypeConst) =>
        ExportedName.TypeName(n, Referant.DefinedT(dt.toOpaque))
      case other => other
    }
    // Export declarations are a set. Constructor order *inside* a type
    // determines its runtime tags and must survive serialization.
    iface.copy(exports = exports.sortBy(e => (e.name, e.nameKind)))
  }

  private[bosatsu] lazy val predef: Package.Interface =
    Package.interfaceOf(PackageMap.predefCompiled)

  private lazy val predefTypes = ExportedName.typeEnvFromExports(predef.name, predef.exports)

  def encode(interfaces: List[Package.Interface]): Try[proto.Interfaces] =
    interfaces.sortBy(_.name).traverse { iface =>
      val public = canonical(iface)
      ProtoConverter.interfaceToProto(public).map(
        _.withSignatureDependencies(
          proto.InterfaceDependencies(public.exposedDepPackages.map(_.asString))
        )
      )
    }.map(proto.Interfaces(_))

  private[bosatsu] def viewOf(
      entry: proto.Interface,
      iface: Package.Interface
  ): Package.Interface =
    if (entry.signatureDependencies.isDefined) canonical(iface) else iface

  private def hasLocalDefinitions(entry: proto.Interface, owner: PackageName): Boolean = {
    val names = entry.strings.toIndexedSeq
    val declarations = entry.definedTypes.iterator.map(_.typeConst) ++
      entry.typeAliases.iterator.map(_.typeConst)
    declarations.forall(_.flatMap(tc => names.lift(tc.packageName - 1)).contains(owner.asString))
  }

  private[bosatsu] def checkedView(
      entry: proto.Interface,
      iface: Package.Interface
  ): Try[Package.Interface] =
    entry.signatureDependencies match {
      case None => Success(iface)
      case Some(index) =>
        if (!hasLocalDefinitions(entry, iface.name))
          Failure(CliException.Basic(s"invalid definition owner in ${iface.name.asString}"))
        else if (index.exposes.toList != iface.exposedDepPackages.map(_.asString))
          Failure(CliException.Basic(s"incorrect exposure index for ${iface.name.asString}"))
        else Success(canonical(iface))
    }

  // Older signatures have no index. Only these three forms need a foreign
  // definition to decode an export. Type constants themselves are symbolic.
  private[bosatsu] def foreignOwners(entry: proto.Interface): List[String] = {
    val names = entry.strings.toIndexedSeq
    entry.exports.iterator.flatMap(_.referant).flatMap { ref =>
      val sum = ref.referant
      sum.definedType.flatMap(_.value.importedDefinedType).map(_.packageName)
        .orElse(sum.typeAlias.flatMap(_.value.importedTypeAlias).map(_.packageName))
        .orElse(sum.constructor.flatMap(_.value.importedConstructor).map(_.packageName))
    }.map { i =>
      names.lift(i - 1).getOrElse(throw CliException.Basic(s"invalid foreign package index $i"))
    }.toList.distinct.sorted
  }

  /** Select before semantic decoding. Each source contributes its own direct
    * requirements, including every local member even if another member imports
    * it. Retain the source filename of each provider for diagnostics.
    */
  def decodeRequired[P: Show](
      bundles: List[(P, proto.Interfaces)],
      requirements: List[(PackageName, List[PackageName])]
  ): Try[List[Package.Interface]] = Try {
    def name(s: String, file: P): PackageName =
      PackageName.parse(s).getOrElse {
        throw CliException.Basic(s"invalid signature package name '$s' in ${file.show}")
      }

    val providers = bundles.iterator.flatMap { case (file, bundle) =>
      bundle.interfaces.iterator.map { entry =>
        val pn = entry.strings.lift(entry.packageName - 1).flatMap(PackageName.parse)
          .getOrElse(throw CliException.Basic(s"invalid root package name in ${file.show}"))
        (pn, (file, entry))
      }
    }.toList.groupMap(_._1)(_._2)
    val local = requirements.iterator.map(_._1).toSet
    val active = mutable.Set.empty[PackageName]
    val decoded = mutable.Map.empty[PackageName, (Package.Interface, TypeEnv[Kind.Arg])]

    def typesOf(tc: Type.Const): Try[TypeEnv[Kind.Arg]] = tc match {
      case Type.Const.Defined(p, _) if p.isPredef => Success(predefTypes)
      case Type.Const.Defined(p, _) =>
        decoded.get(p).map(_._2).toRight(
          CliException.Basic(s"missing declared interface for ${p.asString}")
        ).toTry
    }

    // An explicit stack keeps long re-export chains stack safe. Finish a
    // root only after its providers are available for foreign references.
    sealed trait Work
    case class Visit(p: PackageName, parents: List[PackageName]) extends Work
    case class Decode(path: List[PackageName], file: P, entry: proto.Interface) extends Work
    case class Finish(iface: Package.Interface, file: P) extends Work

    var todo: List[Work] = requirements.flatMap { case (p, deps) =>
      deps.map(Visit(_, p :: Nil))
    }
    while (todo.nonEmpty) {
      val work = todo.head
      todo = todo.tail
      work match {
        case Visit(p, parents) =>
          if (!local(p) && !p.isPredef && !decoded.contains(p)) {
            val path = p :: parents
            def route = path.reverseIterator.map(_.asString).mkString(" -> ")
            if (!active.add(p))
              throw CliException.Basic(s"cyclic interface dependencies: $route")
            val (file, entry) = providers.get(p) match {
              case Some(pair :: Nil) => pair
              case Some(many) =>
                val files = many.map(_._1.show).sorted.mkString(", ")
                throw CliException.Basic(s"ambiguous interface $route in $files")
              case None => throw CliException.Basic(s"missing interface: $route")
            }
            val before = entry.signatureDependencies match {
              case Some(index) => index.exposes.iterator.map(name(_, file)).toList
              case None =>
                foreignOwners(entry).map(name(_, file))
            }
            todo = before.map(Visit(_, path)) ::: Decode(path, file, entry) :: todo
          }
        case Decode(path, file, entry) =>
          val result = ProtoConverter.interfaceFromProto0(
            tc => typesOf(tc).flatMap { env =>
              env.toDefinedType(tc).toRight(CliException.Basic(s"missing exported type $tc")).toTry
            },
            tc => typesOf(tc).flatMap { env =>
              env.toTypeAlias(tc)
                .toRight(CliException.Basic(s"missing exported type alias $tc")).toTry
            },
            entry
          )
          val iface = result match {
            case Success(i) =>
              if (entry.signatureDependencies.isDefined) i else canonical(i)
            case Failure(e) =>
              val route = path.reverseIterator.map(_.asString).mkString(" -> ")
              throw CliException.Basic(s"cannot decode $route in ${file.show}: ${e.getMessage}")
          }
          if (entry.signatureDependencies.isEmpty && !hasLocalDefinitions(entry, iface.name)) {
            // Older encodings can point a local table index at a foreign
            // definition. Re-encode the selected root using symbolic foreign
            // references, then bind those references against their real owners.
            // The encoder emits only locally owned tables, so this runs once.
            val symbolic = ProtoConverter.interfaceToProto(iface).fold(
              e => throw CliException.Basic(s"invalid legacy signature in ${file.show}: ${e.getMessage}"),
              identity
            )
            val owners = foreignOwners(symbolic).map(name(_, file))
            todo = owners.map(Visit(_, path)) ::: Decode(path, file, symbolic) :: todo
          } else {
            // An unindexed entry needs its foreign referants first. Recover
            // its other exposures from the authoritative typed exports.
            val remaining =
              if (entry.signatureDependencies.isEmpty) iface.exposedDepPackages else Nil
            todo = remaining.map(Visit(_, path)) ::: Finish(iface, file) :: todo
          }
        case Finish(iface, file) =>
          val p = iface.name
          // Value schemes and alias RHSs carry nominal constants rather than
          // referant pointers. Verify that the supplied owner exports them too.
          iface.exports.foreach { exp =>
            exp.tag.publicTypeConstants.foreach { tc =>
              val owner = tc.toDefined.packageName
              if (owner != p && !owner.isPredef && !local(owner) &&
                  !decoded.get(owner).exists(_._2.hasTypeConst(tc)))
                throw CliException.Basic(
                  s"${p.asString} in ${file.show} needs missing exported type $tc"
                )
            }
            exp.tag match {
              case Referant.Constructor(dt, cf) if dt.packageName != p =>
                // The family describes all ABI tags, including any constructor
                // the owning interface did not export as a callable name.
                val exported = typesOf(dt.toTypeConst).toOption.flatMap(
                  _.getConstructor(dt.packageName, cf.name)
                )
                if (!exported.exists(_._1.toTypeConst == dt.toTypeConst))
                  throw CliException.Basic(
                    s"${p.asString} in ${file.show} needs missing exported constructor " +
                      s"${dt.packageName.asString}::${cf.name.asString}"
                  )
              case _ => ()
            }
          }
          decoded.update(p, (iface, ExportedName.typeEnvFromExports(p, iface.exports)))
          val _ = active.remove(p)
      }
    }
    decoded.valuesIterator.map(_._1).toList.sortBy(_.name)
  }
}
