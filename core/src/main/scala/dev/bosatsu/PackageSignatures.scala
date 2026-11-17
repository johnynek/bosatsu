package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.{Type, TypeEnv}
import dev.bosatsu.tool.CliException
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/** The compile-time view of a group. Published libraries and implementation
  * bundles keep their full-fidelity codec.
  */
object PackageSignatures {
  private val exportOrder =
    Ordering.by[ExportedName[Referant[Kind.Arg]], (Identifier, String)](e =>
      (e.name, e.nameKind)
    )

  private def publicConstructors(iface: Package.Interface): Set[Type.Const] =
    iface.exports.iterator.collect {
      case ExportedName.Constructor(_, Referant.Constructor(dt, _)) =>
        dt.toTypeConst
    }.toSet

  private[bosatsu] def isCanonical(iface: Package.Interface): Boolean = {
    val exports = iface.exports
    lazy val public = publicConstructors(iface)
    exports.iterator.zip(exports.iterator.drop(1)).forall(exportOrder.lteq) &&
    exports.forall {
      case ExportedName.TypeName(_, Referant.DefinedT(dt)) =>
        dt.isOpaque || public(dt.toTypeConst)
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
    ProtoConverter
      .interfaceToProto(public)
      .map(
        _.withSignatureDependencies(
          proto.InterfaceDependencies(public.exposedDepPackages.map(_.asString))
        )
      )
  }

  def encode(interfaces: List[Package.Interface]): Try[proto.Interfaces] =
    interfaces.sortBy(_.name).traverse(encodeRoot).map(proto.Interfaces(_))

  private[bosatsu] def requirement(
      iface: Package.Interface
  ): Try[proto.InterfaceRequirement] =
    encodeRoot(iface).map { entry =>
      proto.InterfaceRequirement(
        iface.name.asString,
        Algo.hashBytes[Algo.Blake3](entry.toByteArray).hex
      )
    }

  private[bosatsu] def viewOf(
      entry: proto.Interface,
      iface: Package.Interface
  ): Package.Interface =
    if (entry.signatureDependencies.isDefined) canonical(iface) else iface

  private def hasLocalDefinitions(
      entry: proto.Interface,
      owner: String
  ): Boolean = {
    val names = entry.strings.toIndexedSeq
    val declarations = entry.definedTypes.iterator.map(_.typeConst) ++
      entry.typeAliases.iterator.map(_.typeConst)
    declarations.forall(
      _.flatMap(tc => names.lift(tc.packageName - 1)).contains(owner)
    )
  }

  private[bosatsu] def checkedView(
      entry: proto.Interface,
      iface: Package.Interface
  ): Try[Package.Interface] =
    entry.signatureDependencies match {
      case None        => Success(iface)
      case Some(index) =>
        if (!hasLocalDefinitions(entry, iface.name.asString))
          Failure(
            CliException.Basic(
              s"invalid definition owner in ${iface.name.asString}"
            )
          )
        else if (
          index.exposes.toList != iface.exposedDepPackages.map(_.asString)
        )
          Failure(
            CliException.Basic(
              s"incorrect exposure index for ${iface.name.asString}"
            )
          )
        else Success(canonical(iface))
    }

  // Old writers sometimes used local table pointers for re-exports. Translate
  // those pointers to identities before looking up types, even in the whole-file
  // reader. Only the owning package can supply their current definitions.
  private[bosatsu] def symbolicReferences(
      entry: proto.Interface
  ): proto.Interface =
    if (
      entry.signatureDependencies.isDefined || hasLocalDefinitions(
        entry,
        ProtoConverter.iname(entry)
      )
    )
      entry
    else {
      val strings = entry.strings.toIndexedSeq
      val dts = entry.definedTypes.toIndexedSeq
      val aliases = entry.typeAliases.toIndexedSeq
      val owner = strings.lift(entry.packageName - 1)
      def foreign(tc: proto.TypeConst): Boolean =
        strings.lift(tc.packageName - 1) != owner

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
            } yield ref.withConstructor(
              ctor.withImportedConstructor(
                proto.ImportedConstructor(tc.packageName, tc.typeName, cf.name)
              )
            )
          case _ => None
        }

      entry.withExports(entry.exports.map { exp =>
        exp.referant.flatMap(symbolic).fold(exp)(exp.withReferant)
      })
    }

  private def foreignOwner(
      strings: IndexedSeq[String],
      exp: proto.ExportedName
  ): Option[String] =
    exp.referant
      .flatMap { ref =>
        val sum = ref.referant
        sum.definedType
          .flatMap(_.value.importedDefinedType)
          .map(_.packageName)
          .orElse(
            sum.typeAlias.flatMap(_.value.importedTypeAlias).map(_.packageName)
          )
          .orElse(
            sum.constructor
              .flatMap(_.value.importedConstructor)
              .map(_.packageName)
          )
      }
      .map(i =>
        strings
          .lift(i - 1)
          .getOrElse(
            throw CliException.Basic(s"invalid foreign package index $i")
          )
      )

  private[bosatsu] def foreignOwners(entry: proto.Interface): List[String] = {
    val strings = entry.strings.toIndexedSeq
    entry.exports.iterator
      .flatMap(foreignOwner(strings, _))
      .toList
      .distinct
      .sorted
  }

  final private[bosatsu] class PublicInterface(
      publicView: => Package.Interface
  ) {
    lazy val iface: Package.Interface = publicView
    lazy val types: TypeEnv[Kind.Arg] =
      ExportedName.typeEnvFromExports(iface.name, iface.exports)
    lazy val kinds: Map[Type.Const.Defined, Kind] = types.toKindMap
  }
  private[bosatsu] type Providers = Map[PackageName, PublicInterface]
  private[bosatsu] lazy val implicitProviders: Providers =
    Map(predef.name -> PublicInterface(canonical(predef)))

  private[bosatsu] def decode(
      entry: proto.Interface,
      ownerTypes: PackageName => Try[TypeEnv[Kind.Arg]]
  ): Try[Package.Interface] =
    ProtoConverter
      .interfaceFromProto0(
        tc =>
          ownerTypes(tc.toDefined.packageName).flatMap(
            _.toDefinedType(tc)
              .toRight(CliException.Basic(s"missing exported type $tc"))
              .toTry
          ),
        tc =>
          ownerTypes(tc.toDefined.packageName).flatMap(
            _.toTypeAlias(tc)
              .toRight(CliException.Basic(s"missing exported type alias $tc"))
              .toTry
          ),
        entry
      )
      .flatMap { iface =>
        // A public constructor family carries the layout of every case. Only
        // constructors explicitly exported by the owner can be re-exported.
        iface.exports
          .traverse_ { exp =>
            exp.tag match {
              case Referant.Constructor(dt, cf)
                  if dt.packageName != iface.name =>
                ownerTypes(dt.packageName).flatMap { env =>
                  val found = env.getConstructor(dt.packageName, cf.name)
                  Either
                    .cond(
                      found.exists(_._1.toTypeConst == dt.toTypeConst),
                      (),
                      CliException.Basic(
                        show"missing exported constructor ${dt.packageName}::${cf.name}"
                      )
                    )
                    .toTry
                }
              case _ => Success(())
            }
          }
          .as(iface)
      }

  final private[bosatsu] case class Input[P](path: P, label: Option[String]) {
    def display(using Show[P]): String =
      label.fold(path.show)(l => show"$path ($l)")
  }

  final private class Root(
      val name: PackageName,
      val file: String,
      wire: proto.Interface
  ) {
    // Unselected siblings are indexed by name only, including legacy entries.
    private lazy val entry = symbolicReferences(wire)

    // A re-export's remaining requirements are reachable through its owner,
    // including the RHS of an authoritative alias. Without symbolic re-exports
    // the decoded local exports are already the complete legacy interface.
    private lazy val inspection: (List[String], Option[Package.Interface]) =
      entry.signatureDependencies match {
        case Some(index) => (index.exposes.toList, None)
        case None        =>
          val strings = entry.strings.toIndexedSeq
          val (foreign, local) = entry.exports.toList.partitionMap { exp =>
            foreignOwner(strings, exp).toLeft(exp)
          }
          val partial = decode(
            entry.withExports(local),
            pn =>
              Failure(
                CliException.Basic(
                  show"unexpected foreign reference to $pn in local exports"
                )
              )
          ).get
          val deps = (foreign ::: partial.exposedDepPackages.map(
            _.asString
          )).distinct.sorted
          (deps, Option.when(foreign.isEmpty)(partial))
      }

    def dependencies: List[String] = inspection._1

    def bind(providers: Providers): PublicInterface = {
      val raw = inspection._2.getOrElse {
        decode(
          entry,
          pn =>
            providers
              .get(pn)
              .map(_.types)
              .toRight(CliException.Basic(show"missing interface for $pn"))
              .toTry
        ).recover { case e =>
          throw CliException.Basic(
            s"cannot decode ${name.asString} in $file: ${e.getMessage}"
          )
        }.get
      }
      val iface =
        if (entry.signatureDependencies.isDefined) raw else canonical(raw)
      val public = PublicInterface(iface)
      // Values and alias bodies must be proper types when bound to the actual
      // owners. Legacy constructor fields may mention private local types
      // whose kinds were never serialized. Keep checking the known arguments
      // and result kinds of applications even when one argument is private.
      type CheckedKind = Either[String, Option[Kind]]
      def checker(allowPrivateLocal: Boolean): Type => CheckedKind =
        Type.foldKinds[CheckedKind](
          k => Right(Some(k)),
          v => Left(s"unbound variable $v"),
          tc => {
            val id = tc.tpe.toDefined
            val local = id.packageName == name
            val kind =
              if (local) public.kinds.get(id)
              else providers.get(id.packageName).flatMap(_.kinds.get(id))
            if (kind.isDefined || (local && allowPrivateLocal)) Right(kind)
            else Left(s"missing exported type $id")
          }
        ) { (_, fn, arg) =>
          (fn, arg).tupled.flatMap {
            case (Some(left), Some(right)) =>
              Kind
                .validApply(left, right, "applied a proper type")(k =>
                  s"expected kind ${k.arg.kind.toDoc.render(80)}, found ${right.toDoc.render(80)}"
                )
                .map(Some(_))
            case (Some(Kind.Type), None) => Left("applied a proper type")
            case (Some(Kind.Cons(_, result)), None) => Right(Some(result))
            case (None, _)                          => Right(None)
          }
        }

      lazy val publicKind = checker(allowPrivateLocal = false)
      lazy val fieldKind = checker(allowPrivateLocal = true)
      def requireValueKind(exp: Identifier, kind: CheckedKind): Unit =
        kind
          .flatMap {
            case Some(k) if !k.isType =>
              Left(s"expected a proper type, found kind ${k.toDoc.render(80)}")
            case _ => Right(())
          }
          .left
          .foreach(msg =>
            throw CliException.Basic(
              show"invalid signature ${name}::${exp} in $file: $msg"
            )
          )

      iface.exports.foreach { exp =>
        exp.tag match {
          case Referant.Value(tpe) =>
            requireValueKind(exp.name, publicKind(tpe))
          case Referant.TypeAliasT(ta) =>
            val params = ta.annotatedTypeParams.map { case (v, k) =>
              (v, k.kind)
            }
            requireValueKind(exp.name, publicKind(Type.forAll(params, ta.rhs)))
          case Referant.Constructor(dt, cf) =>
            val params = (dt.annotatedTypeParams ::: cf.exists).map {
              case (v, k) => (v, k.kind)
            }
            cf.args.foreach(_.signatureTypes.foreach { tpe =>
              requireValueKind(exp.name, fieldKind(Type.forAll(params, tpe)))
            })
          case Referant.DefinedT(_) => ()
        }
      }
      public
    }
  }

  /** Within a stage, materialize external interfaces in dependency order, then
    * compile the sources together. Only an external interface referring back to
    * a source requires a stage boundary. Its actual owner must finish first.
    */
  final private[bosatsu] class Stage private[PackageSignatures] (
      roots: List[Root],
      val sources: List[PackageName]
  ) {
    def bind(available: Providers): Try[Providers] = Try {
      roots.foldLeft(available)((acc, root) =>
        acc.updated(root.name, root.bind(acc))
      )
    }
  }

  /** The selected owner of each external package, before files are combined.
    * Local members and implicit Predef have no entry. A physical file can
    * supply several selected packages or just one root of a wide signature.
    */
  final private[bosatsu] case class Resolution[P](
      stages: List[Stage],
      origins: Map[PackageName, Input[P]]
  ) {
    def checkUsed(declared: List[Input[P]])(using Show[P]): Try[Unit] = {
      val used = origins.valuesIterator.map(_.path).toSet
      val extra =
        declared.filterNot(input => used(input.path)).map(_.display).sorted
      if (extra.isEmpty) Success(())
      else
        Failure(
          CliException.Basic(
            s"unused direct interfaces: ${extra.mkString(", ")}"
          )
        )
    }
  }

  private[bosatsu] def plan[P: Show](
      bundles: List[(Input[P], proto.Interfaces)],
      sources: List[(PackageName, List[PackageName])],
      builtinPredef: Boolean
  ): Try[Resolution[P]] = Try {
    val providers = bundles
      .flatMap { case (file, bundle) =>
        val display = file.display
        bundle.interfaces.toList.map { entry =>
          val pn = entry.strings
            .lift(entry.packageName - 1)
            .flatMap(PackageName.parse)
            .getOrElse(
              throw CliException.Basic(s"invalid root package name in $display")
            )
          pn -> (file, Root(pn, display, entry))
        }
      }
      .groupMap(_._1)(_._2)
    val origins = mutable.Map.empty[PackageName, Input[P]]
    val locals = sources.toMap
    val active = mutable.Set.empty[PackageName]
    val stages = mutable.Map.empty[PackageName, Int]
    val rootsByStage = mutable.Map.empty[Int, mutable.ListBuffer[Root]]
    val sourcesByStage = mutable.Map.empty[Int, mutable.ListBuffer[PackageName]]
    sealed trait Work
    case class Visit(p: PackageName, path: List[PackageName]) extends Work
    case class Finish(
        p: PackageName,
        deps: List[PackageName],
        root: Option[Root]
    ) extends Work
    var todo: List[Work] = sources.map { case (p, _) => Visit(p, Nil) }
    while (todo.nonEmpty) {
      val head = todo.head
      todo = todo.tail
      head match {
        case Visit(p, path) if !stages.contains(p) =>
          val route = p :: path
          def display = {
            val chain = route.reverseIterator.map(_.asString).mkString(" -> ")
            val supplied = route.reverseIterator
              .flatMap { pn =>
                origins
                  .get(pn)
                  .map(input => s"${pn.asString} from ${input.display}")
              }
              .mkString(", ")
            if (supplied.isEmpty) chain else s"$chain [$supplied]"
          }
          if (!active.add(p))
            throw CliException.Basic(s"cyclic interface dependencies: $display")
          val (deps, root) = locals.get(p) match {
            case Some(ds) => (ds, None)
            case None     =>
              providers.get(p) match {
                case Some((input, r) :: Nil) =>
                  origins.update(p, input)
                  val ds = Try(r.dependencies.map { dep =>
                    PackageName
                      .parse(dep)
                      .getOrElse(
                        throw CliException.Basic(s"invalid package '$dep'")
                      )
                  }).recover { case e =>
                    throw CliException.Basic(
                      s"cannot decode $display in ${r.file}: ${e.getMessage}"
                    )
                  }.get
                  (ds, Some(r))
                case Some(rs) =>
                  throw CliException.Basic(
                    s"ambiguous interface $display in ${rs.map(_._1.display).sorted.mkString(", ")}"
                  )
                case None =>
                  throw CliException.Basic(
                    s"unknown package ${p.asString}, missing interface: $display"
                  )
              }
          }
          // Sources implicitly import Predef. Visit it first so its definitions
          // are available to decode files, including files with unindexed types.
          // A foreign interface that Predef itself exposes can bind first.
          val edges =
            if (builtinPredef) deps.filterNot(_.isPredef)
            else if (root.isEmpty && !p.isPredef)
              (PackageName.PredefName :: deps).distinct
            else deps
          todo = edges.map(Visit(_, route)) ::: Finish(p, edges, root) :: todo
        case Visit(_, _)           => ()
        case Finish(p, deps, root) =>
          val stage = deps.foldLeft(0) { (level, dep) =>
            val boundary = if (root.isDefined && locals.contains(dep)) 1 else 0
            level.max(stages(dep) + boundary)
          }
          root match {
            case Some(r) =>
              rootsByStage.getOrElseUpdate(stage, mutable.ListBuffer.empty) += r
            case None =>
              sourcesByStage.getOrElseUpdate(
                stage,
                mutable.ListBuffer.empty
              ) += p
          }
          stages.update(p, stage)
          val _ = active.remove(p)
      }
    }
    val ordered =
      (0 to stages.valuesIterator.maxOption.getOrElse(-1)).toList.map { i =>
        new Stage(
          rootsByStage.get(i).fold(List.empty[Root])(_.toList),
          sourcesByStage.get(i).fold(List.empty[PackageName])(_.toList)
        )
      }
    Resolution(ordered, origins.toMap)
  }
}
