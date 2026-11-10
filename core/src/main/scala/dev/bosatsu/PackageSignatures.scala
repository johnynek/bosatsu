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
  private val exportOrder = Ordering.by[ExportedName[Referant[Kind.Arg]], (Identifier, String)](
    e => (e.name, e.nameKind)
  )

  private def publicConstructors(iface: Package.Interface): Set[Type.Const] =
    iface.exports.iterator.collect {
      case ExportedName.Constructor(_, Referant.Constructor(dt, _)) => dt.toTypeConst
    }.toSet

  private[bosatsu] def isCanonical(iface: Package.Interface): Boolean = {
    val exports = iface.exports
    lazy val public = publicConstructors(iface)
    exports.iterator.zip(exports.iterator.drop(1)).forall(exportOrder.lteq) &&
      exports.forall {
        case ExportedName.TypeName(_, Referant.DefinedT(dt)) => dt.isOpaque || public(dt.toTypeConst)
        case _ => true
      }
  }

  def canonical(iface: Package.Interface): Package.Interface = {
    val public = publicConstructors(iface)
    val exports = iface.exports.map {
      case ExportedName.TypeName(n, Referant.DefinedT(dt))
          if !dt.isOpaque && !public(dt.toTypeConst) =>
        ExportedName.TypeName(n, Referant.DefinedT(dt.toOpaque))
      case other => other
    }
    // Export declarations are a set. Constructor order *inside* a type
    // determines its runtime tags and must survive serialization.
    iface.copy(exports = exports.sorted(using exportOrder))
  }

  private[bosatsu] lazy val predef: Package.Interface =
    Package.interfaceOf(PackageMap.predefCompiled)

  private def encodeRoot(iface: Package.Interface): Try[proto.Interface] = {
    val public = canonical(iface)
    ProtoConverter.interfaceToProto(public).map(_.withSignatureDependencies(
      proto.InterfaceDependencies(public.exposedDepPackages.map(_.asString))
    ))
  }

  def encode(interfaces: List[Package.Interface]): Try[proto.Interfaces] =
    interfaces.sortBy(_.name).traverse(encodeRoot).map(proto.Interfaces(_))

  private[bosatsu] def viewOf(
      entry: proto.Interface,
      iface: Package.Interface
  ): Package.Interface =
    if (entry.signatureDependencies.isDefined) canonical(iface) else iface

  private def hasLocalDefinitions(entry: proto.Interface, owner: String): Boolean = {
    val names = entry.strings.toIndexedSeq
    val declarations = entry.definedTypes.iterator.map(_.typeConst) ++
      entry.typeAliases.iterator.map(_.typeConst)
    declarations.forall(_.flatMap(tc => names.lift(tc.packageName - 1)).contains(owner))
  }

  private[bosatsu] def checkedView(
      entry: proto.Interface,
      iface: Package.Interface
  ): Try[Package.Interface] =
    entry.signatureDependencies match {
      case None => Success(iface)
      case Some(index) =>
        if (!hasLocalDefinitions(entry, iface.name.asString))
          Failure(CliException.Basic(s"invalid definition owner in ${iface.name.asString}"))
        else if (index.exposes.toList != iface.exposedDepPackages.map(_.asString))
          Failure(CliException.Basic(s"incorrect exposure index for ${iface.name.asString}"))
        else Success(canonical(iface))
    }

  // Old writers sometimes used local table pointers for re-exports. Translate
  // those pointers to identities before looking up types, even in the whole-file
  // reader. Only the owning package can supply their current definitions.
  private[bosatsu] def symbolicReferences(entry: proto.Interface): proto.Interface =
    if (entry.signatureDependencies.isDefined || hasLocalDefinitions(entry, ProtoConverter.iname(entry)))
      entry
    else {
      val strings = entry.strings.toIndexedSeq
      val dts = entry.definedTypes.toIndexedSeq
      val aliases = entry.typeAliases.toIndexedSeq
      val owner = strings.lift(entry.packageName - 1)
      def foreign(tc: proto.TypeConst): Boolean = strings.lift(tc.packageName - 1) != owner

      def symbolic(ref: proto.Referant): Option[proto.Referant] =
        ref.referant match {
          case proto.Referant.Referant.DefinedType(dt) =>
            for {
              i <- dt.value.localDefinedTypePtr
              tc <- dts.lift(i - 1).flatMap(_.typeConst) if foreign(tc)
            } yield ref.withDefinedType(dt.withImportedDefinedType(tc))
          case proto.Referant.Referant.TypeAlias(alias) =>
            for {
              i <- alias.value.localTypeAliasPtr
              tc <- aliases.lift(i - 1).flatMap(_.typeConst) if foreign(tc)
            } yield ref.withTypeAlias(alias.withImportedTypeAlias(tc))
          case proto.Referant.Referant.Constructor(ctor) =>
            for {
              ptr <- ctor.value.localConstructor
              dt <- dts.lift(ptr.definedTypePtr - 1)
              tc <- dt.typeConst if foreign(tc)
              cf <- dt.constructors.lift(ptr.constructorPtr - 1)
            } yield ref.withConstructor(ctor.withImportedConstructor(
              proto.ImportedConstructor(tc.packageName, tc.typeName, cf.name)
            ))
          case _ => None
        }

      entry.withExports(entry.exports.map { exp =>
        exp.referant.flatMap(symbolic).fold(exp)(exp.withReferant)
      })
    }

  private def foreignOwner(strings: IndexedSeq[String], exp: proto.ExportedName): Option[String] =
    exp.referant.flatMap { ref =>
      val sum = ref.referant
      sum.definedType.flatMap(_.value.importedDefinedType).map(_.packageName)
        .orElse(sum.typeAlias.flatMap(_.value.importedTypeAlias).map(_.packageName))
        .orElse(sum.constructor.flatMap(_.value.importedConstructor).map(_.packageName))
    }.map(i => strings.lift(i - 1).getOrElse(
      throw CliException.Basic(s"invalid foreign package index $i")
    ))

  private[bosatsu] def foreignOwners(entry: proto.Interface): List[String] = {
    val strings = entry.strings.toIndexedSeq
    entry.exports.iterator.flatMap(foreignOwner(strings, _)).toList.distinct.sorted
  }

  private[bosatsu] final class PublicInterface(publicView: => Package.Interface) {
    lazy val iface: Package.Interface = publicView
    lazy val types: TypeEnv[Kind.Arg] = ExportedName.typeEnvFromExports(iface.name, iface.exports)
  }
  private[bosatsu] type Providers = Map[PackageName, PublicInterface]
  private[bosatsu] lazy val implicitProviders: Providers =
    Map(predef.name -> PublicInterface(canonical(predef)))

  private def decode(entry: proto.Interface, providers: Providers): Try[Package.Interface] = {
    def types(tc: Type.Const) = providers.get(tc.toDefined.packageName).map(_.types)
    ProtoConverter.interfaceFromProto0(
      tc => types(tc).flatMap(_.toDefinedType(tc)).toRight(
        CliException.Basic(s"missing exported type $tc")).toTry,
      tc => types(tc).flatMap(_.toTypeAlias(tc)).toRight(
        CliException.Basic(s"missing exported type alias $tc")).toTry,
      entry
    )
  }

  private final class Root(val name: PackageName, val file: String, wire: => proto.Interface) {
    // Unselected siblings are indexed by name only, including legacy entries.
    private lazy val entry = symbolicReferences(wire)

    // A re-export's remaining requirements are reachable through its owner,
    // including the RHS of an authoritative alias. Without symbolic re-exports
    // the decoded local exports are already the complete legacy interface.
    private lazy val inspection: (List[String], Option[Package.Interface]) =
      entry.signatureDependencies match {
        case Some(index) => (index.exposes.toList, None)
        case None =>
          val strings = entry.strings.toIndexedSeq
          val (foreign, local) = entry.exports.toList.partitionMap { exp =>
            foreignOwner(strings, exp).toLeft(exp)
          }
          val partial = decode(entry.withExports(local), Map.empty).get
          val deps = (foreign ::: partial.exposedDepPackages.map(_.asString)).distinct.sorted
          (deps, Option.when(foreign.isEmpty)(partial))
      }

    def dependencies: List[String] = inspection._1

    def bind(providers: Providers): PublicInterface = {
      val raw = inspection._2.getOrElse {
        decode(entry, providers).recover { case e =>
          throw CliException.Basic(s"cannot decode ${name.asString} in $file: ${e.getMessage}")
        }.get
      }
      val iface = if (entry.signatureDependencies.isDefined) raw else canonical(raw)
      iface.exports.foreach { exp =>
        exp.tag.publicTypeConstants.foreach { tc =>
          val owner = tc.toDefined.packageName
          if (owner != name && !providers.get(owner).exists(_.types.hasTypeConst(tc)))
            throw CliException.Basic(s"${name.asString} in $file needs missing exported type $tc")
        }
        exp.tag match {
          case Referant.Constructor(dt, cf) if dt.packageName != name =>
            val exported = providers.get(dt.packageName).flatMap(
              _.types.getConstructor(dt.packageName, cf.name)
            )
            if (!exported.exists(_._1.toTypeConst == dt.toTypeConst))
              throw CliException.Basic(
                s"${name.asString} in $file needs missing exported constructor " +
                  s"${dt.packageName.asString}::${cf.name.asString}"
              )
          case _ => ()
        }
      }
      PublicInterface(iface)
    }
  }

  /** Within a stage, materialize external interfaces in dependency order, then
    * compile the sources together. Only an external interface referring back
    * to a source requires a stage boundary. Its actual owner must finish first.
    */
  private[bosatsu] final class Stage private[PackageSignatures] (
      roots: List[Root],
      val sources: List[PackageName]
  ) {
    def bind(available: Providers): Try[Providers] = Try {
      roots.foldLeft(available)((acc, root) => acc.updated(root.name, root.bind(acc)))
    }
  }

  private[bosatsu] def plan[P: Show](
      bundles: List[(P, proto.Interfaces)],
      libraries: List[(P, List[Package.Interface])],
      sources: List[(PackageName, List[PackageName])],
      builtinPredef: Boolean
  ): Try[List[Stage]] = Try {
    val rawRoots = bundles.flatMap { case (file, bundle) =>
      bundle.interfaces.toList.map { entry =>
        val pn = entry.strings.lift(entry.packageName - 1).flatMap(PackageName.parse)
          .getOrElse(throw CliException.Basic(s"invalid root package name in ${file.show}"))
        pn -> Root(pn, file.show, entry)
      }
    }
    // A decoded library can contain many unused packages. Only convert a
    // selected root when binding it against the owners in this compilation.
    val libraryRoots = libraries.flatMap { case (file, ifaces) =>
      ifaces.map(i => i.name -> Root(i.name, file.show, encodeRoot(i).get))
    }
    val providers = (rawRoots ::: libraryRoots).groupMap(_._1)(_._2)
    val locals = sources.toMap
    val active = mutable.Set.empty[PackageName]
    val stages = mutable.Map.empty[PackageName, Int]
    val rootsByStage = mutable.Map.empty[Int, mutable.ListBuffer[Root]]
    val sourcesByStage = mutable.Map.empty[Int, mutable.ListBuffer[PackageName]]
    sealed trait Work
    case class Visit(p: PackageName, path: List[PackageName]) extends Work
    case class Finish(p: PackageName, deps: List[PackageName], root: Option[Root]) extends Work
    var todo: List[Work] = sources.map { case (p, _) => Visit(p, Nil) }
    while (todo.nonEmpty) {
      val head = todo.head
      todo = todo.tail
      head match {
        case Visit(p, path) if !stages.contains(p) =>
          val route = p :: path
          def display = route.reverseIterator.map(_.asString).mkString(" -> ")
          if (!active.add(p))
            throw CliException.Basic(s"cyclic interface dependencies: $display")
          val (deps, root) = locals.get(p) match {
            case Some(ds) => (ds, None)
            case None => providers.get(p) match {
              case Some(r :: Nil) =>
                val ds = Try(r.dependencies.map { dep =>
                  PackageName.parse(dep).getOrElse(throw CliException.Basic(s"invalid package '$dep'"))
                }).recover { case e =>
                  throw CliException.Basic(s"cannot decode $display in ${r.file}: ${e.getMessage}")
                }.get
                (ds, Some(r))
              case Some(rs) =>
                throw CliException.Basic(s"ambiguous interface $display in ${rs.map(_.file).sorted.mkString(", ")}")
              case None => throw CliException.Basic(s"missing interface: $display")
            }
          }
          // Sources implicitly import Predef. Visit it first so its definitions
          // are available to decode files, including files with unindexed types.
          // A foreign interface that Predef itself exposes can bind first.
          val edges =
            if (builtinPredef) deps.filterNot(_.isPredef)
            else if (root.isEmpty && !p.isPredef) (PackageName.PredefName :: deps).distinct
            else deps
          todo = edges.map(Visit(_, route)) ::: Finish(p, edges, root) :: todo
        case Visit(_, _) => ()
        case Finish(p, deps, root) =>
          val stage = deps.foldLeft(0) { (level, dep) =>
            val boundary = if (root.isDefined && locals.contains(dep)) 1 else 0
            level.max(stages(dep) + boundary)
          }
          root match {
            case Some(r) => rootsByStage.getOrElseUpdate(stage, mutable.ListBuffer.empty) += r
            case None => sourcesByStage.getOrElseUpdate(stage, mutable.ListBuffer.empty) += p
          }
          stages.update(p, stage)
          val _ = active.remove(p)
      }
    }
    (0 to stages.valuesIterator.maxOption.getOrElse(-1)).toList.map { i =>
      new Stage(
        rootsByStage.get(i).fold(List.empty[Root])(_.toList),
        sourcesByStage.get(i).fold(List.empty[PackageName])(_.toList)
      )
    }
  }
}
