package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.graph.Toposort
import dev.bosatsu.library.{Name, Version}
import dev.bosatsu.rankn.{Type, TypeEnv}
import dev.bosatsu.tool.CliException
import dev.bosatsu.tool.PackageBindings
import dev.bosatsu.tool.PackageBindings.{CheckedBinding, CheckedScopeMap, PackageOwner, Scope}
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

  private[bosatsu] def requirement(iface: Package.Interface): Try[proto.InterfaceRequirement] =
    encodeRoot(iface).map { entry =>
      proto.InterfaceRequirement(iface.name.asString, Algo.hashBytes[Algo.Blake3](entry.toByteArray).hex)
    }

  private[bosatsu] final case class InterfaceDigest(
      packageName: PackageName,
      blake3: String
  )

  private[bosatsu] final case class RequirementBinding(
      referrer: Scope,
      packageName: PackageName,
      owner: PackageOwner
  )

  private[bosatsu] final case class MemberRequirements(
      member: PackageName,
      bindings: List[RequirementBinding],
      interfaces: List[InterfaceDigest]
  )

  private[bosatsu] sealed trait ParsedBuildRequirements
  private[bosatsu] object ParsedBuildRequirements {
    final case class Legacy(interfaces: List[InterfaceDigest]) extends ParsedBuildRequirements
    final case class Scoped(members: List[MemberRequirements]) extends ParsedBuildRequirements
  }

  private def scopeToProto(scope: Scope): proto.BuildScope = scope match {
    case Scope.Member(name) => proto.BuildScope(kind = "member", memberName = name.asString)
    case Scope.Library((name, version)) => proto.BuildScope(
      kind = "library",
      libraryName = name.name,
      libraryVersion = Some(version.toProto)
    )
    case Scope.Predef => proto.BuildScope(kind = "predef")
  }

  /** Serialize the exact checked owners retained by scoped compilation. */
  private[bosatsu] def buildRequirements[Path](
      group: dev.bosatsu.tool.CompilerApi.CompiledGroup[Path]
  ): Try[proto.BuildRequirements] = group.context match {
    case dev.bosatsu.tool.CompilerApi.CompilationContext.Legacy() =>
      group.dependencies.traverse(requirement).map(proto.BuildRequirements(_))
    case dev.bosatsu.tool.CompilerApi.CompilationContext.Scoped(members) =>
      members.toList.sortBy(_._1).traverse { case (member, compiled) =>
        val bindings = compiled.bindings.sortBy(_.key)(using PackageBindings.bindingKeyOrdering)
        val externalOwners = bindings.iterator.collect {
          case binding if binding.owner.scope.isInstanceOf[Scope.Library] => binding.owner
        }.toList
        val ownersByName = externalOwners.foldLeft(
          scala.collection.immutable.SortedMap.empty[PackageName, PackageOwner]
        ) { (owners, owner) =>
          owners.get(owner.packageName) match {
            case Some(existing) if !PackageBindings.samePackageOwner(existing, owner) =>
              throw CliException.Basic(
                s"conflicting serialized owners for ${owner.packageName.asString} in ${member.asString}"
              )
            case _ => owners.updated(owner.packageName, owner)
          }
        }
        val dependencies = compiled.publicDependencies.iterator.map(iface => iface.name -> iface).toMap
        val interfaceRecords = ownersByName.valuesIterator.toList.traverse { owner =>
          dependencies.get(owner.packageName).fold[Try[proto.InterfaceRequirement]](
            Failure(CliException.Basic(
              s"missing selected public interface ${owner.packageName.asString} for ${member.asString}"
            ))
          )(requirement)
        }
        interfaceRecords.map { interfaces =>
          val bindingRecords = bindings.map { binding =>
            proto.ScopedBindingRequirement(
              referrer = Some(scopeToProto(binding.key.referrer)),
              packageName = binding.key.requiredPackage.asString,
              owner = Some(scopeToProto(binding.owner.scope)),
              ownerPackageName = binding.owner.packageName.asString
            )
          }
          proto.MemberBuildRequirements(
            memberName = member.asString,
            bindings = bindingRecords,
            interfaces = interfaces
          )
        }
      }.map(members => proto.BuildRequirements(scopedVersion = 1, members = members))
  }

  /** Decode legacy and scoped metadata without erasing the format distinction. */
  private[bosatsu] def parseBuildRequirements(
      requirements: proto.BuildRequirements
  ): Try[ParsedBuildRequirements] = Try {
    def parsePackage(raw: String, what: String): PackageName =
      PackageName.parse(raw).getOrElse(throw CliException.Basic(s"invalid $what '$raw'"))

    def parseDigest(raw: proto.InterfaceRequirement): InterfaceDigest = {
      val name = parsePackage(raw.packageName, "interface requirement package")
      if (name.isPredef)
        throw CliException.Basic("Predef cannot have an interface requirement")
      if (Algo.parseHashHex(Algo.blake3Algo).parseAll(raw.interfaceBlake3).isLeft)
        throw CliException.Basic(s"invalid BLAKE3 interface requirement for $name")
      InterfaceDigest(name, raw.interfaceBlake3)
    }

    def parseDigests(records: Seq[proto.InterfaceRequirement]): List[InterfaceDigest] = {
      val parsed = records.toList.map(parseDigest)
      val names = parsed.map(_.packageName).sorted
      if (names.zip(names.drop(1)).exists { case (left, right) => left == right })
        throw CliException.Basic("duplicate interface requirement")
      parsed.sortBy(_.packageName)
    }

    def parseScope(raw: proto.BuildScope): Scope = raw.kind match {
      case "member" =>
        if (raw.memberName.isEmpty || raw.libraryName.nonEmpty || raw.libraryVersion.isDefined)
          throw CliException.Basic("contradictory member scope fields")
        Scope.Member(parsePackage(raw.memberName, "scope member"))
      case "library" =>
        if (raw.libraryName.isEmpty || raw.memberName.nonEmpty || raw.libraryVersion.isEmpty)
          throw CliException.Basic("contradictory library scope fields")
        val version = Version.fromProtoChecked(raw.libraryVersion.get).getOrElse(
          throw CliException.Basic("invalid library version in build requirements")
        )
        Scope.Library(Name(raw.libraryName) -> version)
      case "predef" =>
        if (raw.memberName.nonEmpty || raw.libraryName.nonEmpty || raw.libraryVersion.isDefined)
          throw CliException.Basic("contradictory Predef scope fields")
        Scope.Predef
      case other => throw CliException.Basic(s"unknown requirement scope kind '$other'")
    }

    requirements.scopedVersion match {
      case 0 =>
        if (requirements.members.nonEmpty)
          throw CliException.Basic("legacy build requirements cannot contain member records")
        ParsedBuildRequirements.Legacy(parseDigests(requirements.interfaces))
      case 1 =>
        if (requirements.interfaces.nonEmpty)
          throw CliException.Basic("scoped build requirements cannot contain legacy interfaces")
        val members = requirements.members.toList.map { record =>
          val member = parsePackage(record.memberName, "requirement member")
          val bindings = record.bindings.toList.map { raw =>
            val referrer = parseScope(raw.referrer.getOrElse(
              throw CliException.Basic("missing binding referrer scope")
            ))
            if (referrer == Scope.Predef)
              throw CliException.Basic("Predef cannot be a binding referrer")
            val packageName = parsePackage(raw.packageName, "binding package")
            val ownerScope = parseScope(raw.owner.getOrElse(
              throw CliException.Basic("missing binding owner scope")
            ))
            val ownerPackage = parsePackage(raw.ownerPackageName, "binding owner package")
            if (packageName != ownerPackage)
              throw CliException.Basic("binding package and owner package disagree")
            RequirementBinding(referrer, packageName, PackageOwner(ownerScope, ownerPackage))
          }
          val bindingKeys = bindings.map(binding =>
            PackageBindings.BindingKey(binding.referrer, binding.packageName)
          ).sorted(using PackageBindings.bindingKeyOrdering)
          if (bindingKeys.zip(bindingKeys.drop(1)).exists { case (left, right) =>
                PackageBindings.bindingKeyOrdering.compare(left, right) == 0
              })
            throw CliException.Basic(s"duplicate binding record for ${member.asString}")

          val ownerByPackage = bindings.foldLeft(
            scala.collection.immutable.SortedMap.empty[PackageName, PackageOwner]
          ) { (owners, binding) =>
            owners.get(binding.packageName) match {
              case Some(existing) if !PackageBindings.samePackageOwner(existing, binding.owner) =>
                throw CliException.Basic(
                  s"contradictory owners for ${binding.packageName.asString} in ${member.asString}"
                )
              case _ => owners.updated(binding.packageName, binding.owner)
            }
          }
          val interfaces = parseDigests(record.interfaces)
          val externalNames = ownerByPackage.valuesIterator.collect {
            case PackageOwner(Scope.Library(_), packageName) => packageName
          }.toList.distinct.sorted
          val interfaceNames = interfaces.map(_.packageName)
          if (externalNames != interfaceNames)
            throw CliException.Basic(
              s"interface requirements do not match selected library owners for ${member.asString}"
            )
          bindings.foreach { binding =>
            binding.owner.scope match {
              case Scope.Member(owner) if owner != binding.owner.packageName =>
                throw CliException.Basic("source binding owner and package disagree")
              case Scope.Predef if !binding.owner.packageName.isPredef =>
                throw CliException.Basic("Predef binding owner must be Bosatsu/Predef")
              case _ => ()
            }
          }
          MemberRequirements(member, bindings.sortBy(binding =>
            PackageBindings.BindingKey(binding.referrer, binding.packageName)
          )(using PackageBindings.bindingKeyOrdering), interfaces)
        }.sortBy(_.member)
        val memberNames = members.map(_.member)
        if (memberNames.zip(memberNames.drop(1)).exists { case (left, right) => left == right })
          throw CliException.Basic("duplicate member requirement record")
        val memberSet = memberNames.toSet
        members.iterator.flatMap(_.bindings).foreach { binding =>
          binding.referrer match {
            case Scope.Member(name) if !memberSet(name) =>
              throw CliException.Basic(s"unknown binding referrer member ${name.asString}")
            case _ => ()
          }
          binding.owner.scope match {
            case Scope.Member(name) if !memberSet(name) =>
              throw CliException.Basic(s"unknown binding owner member ${name.asString}")
            case _ => ()
          }
        }
        ParsedBuildRequirements.Scoped(members)
      case version =>
        throw CliException.Basic(s"unsupported scoped build requirements version $version")
    }
  }

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
    lazy val kinds: Map[Type.Const.Defined, Kind] = types.toKindMap
  }
  private[bosatsu] type Providers = Map[PackageName, PublicInterface]
  private[bosatsu] lazy val implicitProviders: Providers =
    Map(predef.name -> PublicInterface(canonical(predef)))

  private[bosatsu] def decode(
      entry: proto.Interface, ownerTypes: PackageName => Try[TypeEnv[Kind.Arg]]
  ): Try[Package.Interface] =
    ProtoConverter.interfaceFromProto0(
      tc => ownerTypes(tc.toDefined.packageName).flatMap(_.toDefinedType(tc).toRight(
        CliException.Basic(s"missing exported type $tc")).toTry),
      tc => ownerTypes(tc.toDefined.packageName).flatMap(_.toTypeAlias(tc).toRight(
        CliException.Basic(s"missing exported type alias $tc")).toTry),
      entry
    ).flatMap { iface =>
      // A public constructor family carries the layout of every case. Only
      // constructors explicitly exported by the owner can be re-exported.
      iface.exports.traverse_ { exp =>
        exp.tag match {
          case Referant.Constructor(dt, cf) if dt.packageName != iface.name =>
            ownerTypes(dt.packageName).flatMap { env =>
              val found = env.getConstructor(dt.packageName, cf.name)
              Either.cond(found.exists(_._1.toTypeConst == dt.toTypeConst), (),
                CliException.Basic(show"missing exported constructor ${dt.packageName}::${cf.name}")).toTry
            }
          case _ => Success(())
        }
      }.as(iface)
    }

  private[bosatsu] final case class Input[P](path: P, label: Option[String]) {
    def display(using Show[P]): String = label.fold(path.show)(l => show"$path ($l)")
  }

  private[bosatsu] final class Root(val name: PackageName, val file: String, wire: proto.Interface) {
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
          val partial = decode(entry.withExports(local), pn => Failure(
            CliException.Basic(show"unexpected foreign reference to $pn in local exports"))).get
          val deps = (foreign ::: partial.exposedDepPackages.map(_.asString)).distinct.sorted
          (deps, Option.when(foreign.isEmpty)(partial))
      }

    def dependencies: List[String] = inspection._1

    def selectedEntry: proto.Interface = entry

    def bind(providers: Providers): PublicInterface = {
      val raw = inspection._2.getOrElse {
        decode(entry, pn => providers.get(pn).map(_.types).toRight(
          CliException.Basic(show"missing interface for $pn")).toTry).recover { case e =>
          throw CliException.Basic(s"cannot decode ${name.asString} in $file: ${e.getMessage}")
        }.get
      }
      val iface = if (entry.signatureDependencies.isDefined) raw else canonical(raw)
      val public = PublicInterface(iface)
      // Values and alias bodies must be proper types when bound to the actual
      // owners. Legacy constructor fields may mention private local types
      // whose kinds were never serialized. Keep checking the known arguments
      // and result kinds of applications even when one argument is private.
      type CheckedKind = Either[String, Option[Kind]]
      def checker(allowPrivateLocal: Boolean): Type => CheckedKind =
        Type.foldKinds[CheckedKind](k => Right(Some(k)), v => Left(s"unbound variable $v"), tc => {
          val id = tc.tpe.toDefined
          val local = id.packageName == name
          val kind = if (local) public.kinds.get(id) else providers.get(id.packageName).flatMap(_.kinds.get(id))
          if (kind.isDefined || (local && allowPrivateLocal)) Right(kind)
          else Left(s"missing exported type $id")
        }) { (_, fn, arg) =>
          (fn, arg).tupled.flatMap {
            case (Some(left), Some(right)) =>
              Kind.validApply(left, right, "applied a proper type")(
                k => s"expected kind ${k.arg.kind.toDoc.render(80)}, found ${right.toDoc.render(80)}"
              ).map(Some(_))
            case (Some(Kind.Type), None) => Left("applied a proper type")
            case (Some(Kind.Cons(_, result)), None) => Right(Some(result))
            case (None, _) => Right(None)
          }
        }

      lazy val publicKind = checker(allowPrivateLocal = false)
      lazy val fieldKind = checker(allowPrivateLocal = true)
      def requireValueKind(exp: Identifier, kind: CheckedKind): Unit =
        kind.flatMap {
          case Some(k) if !k.isType => Left(s"expected a proper type, found kind ${k.toDoc.render(80)}")
          case _ => Right(())
        }.left.foreach(msg => throw CliException.Basic(show"invalid signature ${name}::${exp} in $file: $msg"))

      iface.exports.foreach { exp =>
        exp.tag match {
          case Referant.Value(tpe) => requireValueKind(exp.name, publicKind(tpe))
          case Referant.TypeAliasT(ta) =>
            val params = ta.annotatedTypeParams.map { case (v, k) => (v, k.kind) }
            requireValueKind(exp.name, publicKind(Type.forAll(params, ta.rhs)))
          case Referant.Constructor(dt, cf) =>
            val params = (dt.annotatedTypeParams ::: cf.exists).map { case (v, k) => (v, k.kind) }
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

  /** The selected owner of each external package, before files are combined.
    * Local members and implicit Predef have no entry. A physical file can
    * supply several selected packages or just one root of a wide signature.
    */
  private[bosatsu] final case class Resolution[P](stages: List[Stage], origins: Map[PackageName, Input[P]]) {
    def checkUsed(declared: List[Input[P]])(using Show[P]): Try[Unit] = {
      val used = origins.valuesIterator.map(_.path).toSet
      val extra = declared.filterNot(input => used(input.path)).map(_.display).sorted
      if (extra.isEmpty) Success(())
      else Failure(CliException.Basic(s"unused direct interfaces: ${extra.mkString(", ")}"))
    }
  }

  private[bosatsu] final case class SourceRequirements(
      member: PackageName,
      imports: List[PackageName],
      exposes: List[PackageName]
  )

  private[bosatsu] final case class SelectedInterface[P](
      owner: PackageOwner,
      input: Input[P],
      entry: proto.Interface,
      root: Root
  )

  /** The checked external interfaces visible while inferring one source.
    * Source dependencies stay identified by member name so the compiler can
    * obtain their public API from its existing source DAG.
    */
  private[bosatsu] final case class MemberSelection[P](
      member: PackageName,
      interfaces: List[SelectedInterface[P]],
      sourceDependencies: List[PackageName],
      bindings: List[CheckedBinding[P]]
  )

  private[bosatsu] final case class ScopedResolution[P](
      members: Map[PackageName, MemberSelection[P]]
  ) {
    def checkUsed(declared: List[Input[P]])(using Show[P], Ordering[P]): Try[Unit] = {
      val used = members.valuesIterator.flatMap(_.interfaces.iterator.map(_.input.path)).toList
      val extra = declared.filterNot(input => used.exists(path => summon[Ordering[P]].compare(path, input.path) == 0))
        .map(_.display).sorted
      if (extra.isEmpty) Success(())
      else Failure(CliException.Basic(s"unused direct interfaces: ${extra.mkString(", ")}"))
    }

    /** Bind only one member's checked interfaces, after its source providers
      * have been inferred. This preserves the existing Root decoder and its
      * kind/layout checks while allowing a foreign API to refer back to a
      * source member.
      */
    def bindMember(
        member: PackageName,
        sourceInterfaces: List[Package.Interface],
        builtinPredef: Boolean
    ): Try[List[Package.Interface]] = Try {
      val selected = members.getOrElse(member,
        throw CliException.Basic(s"missing scoped selection for ${member.asString}"))
      val roots = selected.interfaces.map(_.root)
      val rootsByName = roots.iterator.map(root => root.name -> root).toMap
      val rootNames = rootsByName.keys.toList.sorted
      def dependencies(root: Root): List[PackageName] = root.dependencies.map { raw =>
        PackageName.parse(raw).getOrElse(
          throw CliException.Basic(s"invalid package '$raw' in ${root.name.asString}"))
      }.filter(rootsByName.contains)
      val sorted = Toposort.sort(rootNames)(name => dependencies(rootsByName(name)))
      val layers = sorted.toSuccess.getOrElse {
        throw CliException.Basic(
          s"cyclic interface dependencies: ${sorted.loopNodes.map(_.asString).mkString(", ")}"
        )
      }
      val initial = sourceInterfaces.foldLeft(
        if (builtinPredef) implicitProviders else Map.empty[PackageName, PublicInterface]
      ) { (available, iface) =>
        available.updated(iface.name, new PublicInterface(canonical(iface)))
      }
      val bound = layers.iterator.flatMap(_.toList).foldLeft(initial) { (available, name) =>
        available.updated(name, rootsByName(name).bind(available))
      }
      selected.interfaces.map(iface => bound(iface.owner.packageName).iface)
    }
  }

  /** Resolve each source's imports and exposed API requirements using the
    * already checked caller-relative bindings. Only selected roots are
    * decoded; an unselected sibling in the same physical bundle is inert.
    */
  private[bosatsu] def planScoped[P: Show: Ordering](
      bundles: List[(Input[P], proto.Interfaces)],
      sources: List[SourceRequirements],
      checked: CheckedScopeMap[P],
      builtinPredef: Boolean
  ): Try[ScopedResolution[P]] = Try {
    val sourceNames = sources.iterator.map(_.member).toSet
    if (sources.size != sourceNames.size)
      throw CliException.Basic("duplicate source member in scoped interface selection")
    if (sourceNames != checked.scopeMap.members.toSet)
      throw CliException.Basic("scope-map members do not match source members")

    val roots = bundles.flatMap { case (input, bundle) =>
      val display = input.display
      bundle.interfaces.toList.map { entry =>
        val name = entry.strings.lift(entry.packageName - 1).flatMap(PackageName.parse)
          .getOrElse(throw CliException.Basic(s"invalid root package name in $display"))
        (input, Root(name, display, entry))
      }
    }
    val rootOrdering = Ordering.Tuple2(using summon[Ordering[P]], summon[Ordering[PackageName]])
    val rootsByPathAndName = roots.foldLeft(
      scala.collection.immutable.SortedMap.empty[(P, PackageName), List[(Input[P], Root)]](using rootOrdering)
    ) { case (index, pair @ (input, root)) =>
      val key = (input.path, root.name)
      index.updated(key, index.getOrElse(key, Nil) :+ pair)
    }
    val sourceByName = sources.iterator.map(source => source.member -> source).toMap
    val members = sources.map { source =>
      val selected = mutable.Map.empty[PackageName, SelectedInterface[P]]
      val sourceDependencies = mutable.Set.empty[PackageName]
      val selectedBindingKeys = mutable.TreeSet.empty[PackageBindings.BindingKey](using
        PackageBindings.bindingKeyOrdering
      )
      val visibleOwners = mutable.Map.empty[PackageName, PackageOwner]
      val releases = mutable.Map.empty[PackageBindings.LibraryIdentity, List[Scope]]
      val releaseByName = mutable.Map.empty[Name, PackageBindings.LibraryIdentity]
      val seen = mutable.TreeSet.empty[PackageBindings.BindingKey](using
        PackageBindings.bindingKeyOrdering
      )
      final case class Need(referrer: Scope, packageName: PackageName, path: List[Scope])

      val initial = (source.imports ::: source.exposes).distinct.sorted
      val todo = mutable.ArrayDeque.from(initial.map(name =>
        Need(Scope.Member(source.member), name, List(Scope.Member(source.member)))
      ))
      def scopeText(scope: Scope): String = scope match {
        case Scope.Member(name) => s"member ${name.asString}"
        case Scope.Library((name, version)) => s"library ${name.name}@${version.render}"
        case Scope.Predef => "compiler Predef"
      }
      def routeText(path: List[Scope]): String = path.map(scopeText).mkString(" -> ")

      def visible(owner: PackageOwner, binding: CheckedBinding[P]): Unit = {
        val pn = owner.packageName
        visibleOwners.get(pn) match {
          case Some(existing) if !PackageBindings.samePackageOwner(existing, owner) =>
            throw CliException.Basic(
              s"conflicting visible owners for ${pn.asString}: ${scopeText(existing.scope)} and " +
                s"${scopeText(owner.scope)}"
            )
          case _ => visibleOwners.update(pn, owner)
        }
        owner.scope match {
          case Scope.Library(identity @ (name, _)) =>
            releaseByName.get(name) match {
              case Some(previous) if !PackageBindings.sameLibraryIdentity(previous, identity) =>
                val previousPath = routeText(releases.getOrElse(previous, Nil))
                throw CliException.Basic(
                  s"conflicting visible releases for ${name.name}: " +
                    s"${previous._1.name}@${previous._2.render} via $previousPath; " +
                    s"${identity._1.name}@${identity._2.render} via ${routeText(binding.witnessPath)}"
                )
              case _ => releaseByName.update(name, identity)
            }
            releases.update(identity, binding.witnessPath)
          case _ => ()
        }
      }

      while (todo.nonEmpty) {
        val need = todo.removeHead()
        val key = PackageBindings.BindingKey(need.referrer, need.packageName)
        if (seen.add(key)) {
          val binding = if (need.packageName.isPredef && builtinPredef &&
              !checked.bindings.contains(key)) None
            else Some(checked.bindings.getOrElse(key,
              throw CliException.Basic(
                s"missing interface binding for ${need.packageName.asString}: ${routeText(need.path)}"
              )))
          binding.foreach { binding =>
            val _ = selectedBindingKeys.add(key)
            visible(binding.owner, binding)
            binding.owner.scope match {
              case Scope.Member(provider) =>
                if (!sourceByName.contains(provider))
                  throw CliException.Basic(s"unknown source member ${provider.asString}")
                sourceDependencies += provider
                // A source dependency contributes only its declared public
                // exposures. Its private imports remain in its own context.
                val exposed = sourceByName(provider).exposes.distinct.sorted
                todo.appendAll(exposed.map(name => Need(
                  Scope.Member(provider), name, need.path :+ Scope.Member(provider)
                )))
              case Scope.Predef =>
                if (need.packageName != PackageName.PredefName)
                  throw CliException.Basic("compiler Predef can only provide Bosatsu/Predef")
                if (!builtinPredef)
                  throw CliException.Basic("compiler Predef binding requires builtin Predef")
              case Scope.Library(identity) =>
                val interfaceSource = binding.interface.getOrElse(
                  throw CliException.Basic(s"missing interface path for ${need.packageName.asString}")
                )
                val candidates = rootsByPathAndName.getOrElse(
                  (interfaceSource.path, need.packageName), Nil
                )
                val (input, root) = candidates match {
                  case one :: Nil => one
                  case Nil => throw CliException.Basic(
                    s"unknown package ${need.packageName.asString}, missing interface: ${routeText(binding.witnessPath)}"
                  )
                  case many => throw CliException.Basic(
                    s"ambiguous interface ${need.packageName.asString} in " +
                      many.map(_._1.display).sorted.mkString(", ")
                  )
                }
                val owner = PackageOwner(Scope.Library(identity), need.packageName)
                val publicPackage = checked.publicIndex.packages.getOrElse(owner,
                  throw CliException.Basic(s"missing inspected public package ${need.packageName.asString}"))
                val declaredExposures = root.dependencies.distinct.sorted
                // Inspection retains the implicit Predef provider so its
                // ownership is authenticated, while serialized signature
                // dependencies deliberately omit compiler-provided Predef.
                val indexedExposures = publicPackage.exposures.keysIterator
                  .filterNot(_.isPredef).map(_.asString).toList.sorted
                if (declaredExposures != indexedExposures)
                  throw CliException.Basic(
                    s"signature exposure index disagrees for ${need.packageName.asString} in ${input.display}"
                  )
                selected.get(need.packageName) match {
                  case Some(existing) if !PackageBindings.samePackageOwner(existing.owner, owner) ||
                        summon[Ordering[P]].compare(existing.input.path, input.path) != 0 =>
                    throw CliException.Basic(
                      s"conflicting visible owners for ${need.packageName.asString}: " +
                        s"${scopeText(existing.owner.scope)} and ${scopeText(owner.scope)}"
                    )
                  case _ =>
                    selected.update(need.packageName, SelectedInterface(
                      owner, input, root.selectedEntry, root
                    ))
                }
                publicPackage.exposures.keysIterator.foreach { exposed =>
                  todo.append(Need(
                    Scope.Library(identity), exposed, need.path :+ Scope.Library(identity)
                  ))
                }
              }
          }
        }
      }
      source.member -> MemberSelection(
        source.member,
        selected.values.toList.sortBy(_.owner.packageName),
        sourceDependencies.toList.sorted,
        checked.bindings.iterator.collect {
          case (key, binding) if selectedBindingKeys.contains(key) => binding
        }.toList
      )
    }.toMap
    ScopedResolution(members)
  }

  private[bosatsu] def plan[P: Show](
      bundles: List[(Input[P], proto.Interfaces)],
      sources: List[(PackageName, List[PackageName])],
      builtinPredef: Boolean
  ): Try[Resolution[P]] = Try {
    val providers = bundles.flatMap { case (file, bundle) =>
      val display = file.display
      bundle.interfaces.toList.map { entry =>
        val pn = entry.strings.lift(entry.packageName - 1).flatMap(PackageName.parse)
          .getOrElse(throw CliException.Basic(s"invalid root package name in $display"))
        pn -> (file, Root(pn, display, entry))
      }
    }.groupMap(_._1)(_._2)
    val origins = mutable.Map.empty[PackageName, Input[P]]
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
          def display = {
            val chain = route.reverseIterator.map(_.asString).mkString(" -> ")
            val supplied = route.reverseIterator.flatMap { pn =>
              origins.get(pn).map(input => s"${pn.asString} from ${input.display}")
            }.mkString(", ")
            if (supplied.isEmpty) chain else s"$chain [$supplied]"
          }
          if (!active.add(p))
            throw CliException.Basic(s"cyclic interface dependencies: $display")
          val (deps, root) = locals.get(p) match {
            case Some(ds) => (ds, None)
            case None => providers.get(p) match {
              case Some((input, r) :: Nil) =>
                origins.update(p, input)
                val ds = Try(r.dependencies.map { dep =>
                  PackageName.parse(dep).getOrElse(throw CliException.Basic(s"invalid package '$dep'"))
                }).recover { case e =>
                  throw CliException.Basic(s"cannot decode $display in ${r.file}: ${e.getMessage}")
                }.get
                (ds, Some(r))
              case Some(rs) =>
                throw CliException.Basic(s"ambiguous interface $display in ${rs.map(_._1.display).sorted.mkString(", ")}")
              case None => throw CliException.Basic(s"unknown package ${p.asString}, missing interface: $display")
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
    val ordered = (0 to stages.valuesIterator.maxOption.getOrElse(-1)).toList.map { i =>
      new Stage(
        rootsByStage.get(i).fold(List.empty[Root])(_.toList),
        sourcesByStage.get(i).fold(List.empty[PackageName])(_.toList)
      )
    }
    Resolution(ordered, origins.toMap)
  }
}
