package dev.bosatsu.tool

import cats.{Eq, Order, Show}
import cats.syntax.all._
import dev.bosatsu.{Json, PackageName, PlatformIO}
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.library.{Name, Version}
import java.nio.charset.StandardCharsets
import scala.collection.immutable.{SortedMap, SortedSet}

/** Caller-relative package ownership and the public facts used to check it.
  * Paths, labels, and witness routes are retained as provenance; none are part
  * of a scope or package owner's identity.
  */
private[bosatsu] object PackageBindings {
  type LibraryIdentity = (Name, Version)

  private val libraryIdentityOrdering: Ordering[LibraryIdentity] =
    Ordering.Tuple2(using Name.orderingName, Version.versionOrdering)

  private val libraryIdentityOrder: Order[LibraryIdentity] =
    Order.fromOrdering(using libraryIdentityOrdering)

  private val libraryIdentityShow: Show[LibraryIdentity] =
    Show.show { case (name, version) => s"${name.name}@${version.render}" }

  private def identityJson(identity: LibraryIdentity): Json =
    Json.JObject(List(
      "name" -> Json.JString(identity._1.name),
      "version" -> Json.JString(identity._2.render)
    ))

  private def identitiesJson(identities: IterableOnce[LibraryIdentity]): Json =
    Json.JArray(identities.iterator.map(identityJson).toVector)

  enum Scope derives CanEqual {
    case Member(name: PackageName)
    case Library(identity: LibraryIdentity)
    case Predef
  }

  private val scopeOrdering: Ordering[Scope] = new Ordering[Scope] {
    def compare(left: Scope, right: Scope): Int = (left, right) match {
      case (Scope.Member(a), Scope.Member(b)) => summon[Ordering[PackageName]].compare(a, b)
      case (Scope.Member(_), _)               => -1
      case (_, Scope.Member(_))               => 1
      case (Scope.Library(a), Scope.Library(b)) => libraryIdentityOrdering.compare(a, b)
      case (Scope.Library(_), Scope.Predef)     => -1
      case (Scope.Predef, Scope.Library(_))     => 1
      case (Scope.Predef, Scope.Predef)         => 0
    }
  }

  given Eq[Scope] = Eq.instance((left, right) => scopeOrdering.compare(left, right) == 0)
  given Order[Scope] = Order.fromOrdering(using scopeOrdering)
  given Show[Scope] = Show.show {
    case Scope.Member(name)   => show"member $name"
    case Scope.Library(id)    => show"library ${libraryIdentityShow.show(id)}"
    case Scope.Predef         => "compiler Predef"
  }

  final case class PackageOwner(scope: Scope, packageName: PackageName)

  private val packageOwnerOrdering: Ordering[PackageOwner] =
    new Ordering[PackageOwner] {
      def compare(left: PackageOwner, right: PackageOwner): Int = {
        val scope = scopeOrdering.compare(left.scope, right.scope)
        if (scope != 0) scope
        else summon[Ordering[PackageName]].compare(left.packageName, right.packageName)
      }
    }

  private val packageOwnerOrder: Order[PackageOwner] =
    Order.fromOrdering(using packageOwnerOrdering)

  private val packageOwnerShow: Show[PackageOwner] =
    Show.show(owner => show"${owner.scope}:${owner.packageName}")

  def sameLibraryIdentity(left: LibraryIdentity, right: LibraryIdentity): Boolean =
    libraryIdentityOrder.eqv(left, right)

  def samePackageOwner(left: PackageOwner, right: PackageOwner): Boolean =
    packageOwnerOrder.eqv(left, right)

  enum PublicProvider derives CanEqual {
    case CompilerPredef
    case Library(identity: LibraryIdentity, witnessPath: List[LibraryIdentity])
  }

  final case class PublicPackage(
      identity: LibraryIdentity,
      packageName: PackageName,
      exposures: SortedMap[PackageName, PublicProvider]
  ) {
    def owner: PackageOwner = PackageOwner(Scope.Library(identity), packageName)
  }

  /** The canonical public package/exposure index returned by inspection. */
  final case class PublicIndex(
      packages: SortedMap[PackageOwner, PublicPackage],
      /** Exact inspected scopes, including libraries with no public packages.
        * The version-1 JSON projection cannot encode an empty scope; its reader
        * reconstructs only identities present in package-owner records.
        */
      identities: SortedSet[LibraryIdentity]
  ) {
    def packagesFor(scope: Scope): List[PublicPackage] =
      packages.valuesIterator.filter(pkg => summon[Eq[Scope]].eqv(pkg.owner.scope, scope)).toList

    def owns(owner: PackageOwner): Boolean = packages.contains(owner)

    def toJson: Json = publicIndexJson(this)
  }

  def indexFromPackages(
      packages: List[PublicPackage],
      identities: IterableOnce[LibraryIdentity] = Nil
  ): PublicIndex = {
    val packageMap = SortedMap.from(
      packages.map(pkg => pkg.owner -> pkg)
    )(using packageOwnerOrdering)
    val owners = packageMap.keysIterator.collect {
      case PackageOwner(Scope.Library(identity), _) => identity
    }
    val allIdentities = identities.iterator.concat(owners).foldLeft(
      SortedSet.empty[LibraryIdentity](using libraryIdentityOrdering)
    )(_ + _)
    PublicIndex(packageMap, allIdentities)
  }

  private def publicProviderEq(left: PublicProvider, right: PublicProvider): Boolean =
    (left, right) match {
      case (PublicProvider.CompilerPredef, PublicProvider.CompilerPredef) => true
      case (PublicProvider.Library(a, aPath), PublicProvider.Library(b, bPath)) =>
        libraryIdentityOrder.eqv(a, b) && aPath.length == bPath.length &&
          aPath.zip(bPath).forall { case (x, y) => libraryIdentityOrder.eqv(x, y) }
      case _ => false
    }

  private def publicPackageEq(left: PublicPackage, right: PublicPackage): Boolean =
    packageOwnerOrder.eqv(left.owner, right.owner) &&
      left.exposures.size == right.exposures.size &&
      left.exposures.iterator.zip(right.exposures.iterator).forall {
        case ((leftName, leftProvider), (rightName, rightProvider)) =>
          summon[Order[PackageName]].eqv(leftName, rightName) &&
            publicProviderEq(leftProvider, rightProvider)
      }

  private def publicIndexJson(index: PublicIndex): Json = {
    val packages = index.packages.valuesIterator.map { pkg =>
      val exposures = pkg.exposures.iterator.map { case (name, provider) =>
        val (providerJson, witness) = provider match {
          case PublicProvider.CompilerPredef =>
            Json.JObject(List(
              "kind" -> Json.JString("compiler"),
              "name" -> Json.JString("Predef")
            )) -> List.empty[LibraryIdentity]
          case PublicProvider.Library(providerIdentity, witnessPath) =>
            identityJson(providerIdentity) -> witnessPath
        }
        Json.JObject(List(
          "package" -> Json.JString(name.asString),
          "provider" -> providerJson,
          "witness_path" -> identitiesJson(witness)
        ))
      }.toVector
      Json.JObject(List(
        "library" -> identityJson(pkg.identity),
        "package" -> Json.JString(pkg.packageName.asString),
        "owner" -> identityJson(pkg.identity),
        "exposes" -> Json.JArray(exposures)
      ))
    }.toVector
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "packages" -> Json.JArray(packages)
    ))
  }

  final case class InterfaceSource[Path](path: Path, label: Option[String])

  final case class ImportedScope[Path](
      identity: LibraryIdentity,
      signature: InterfaceSource[Path],
      publicIndexPath: Path
  )

  final case class BindingKey(referrer: Scope, requiredPackage: PackageName)

  private[bosatsu] val bindingKeyOrdering: Ordering[BindingKey] = new Ordering[BindingKey] {
    def compare(left: BindingKey, right: BindingKey): Int = {
      val scope = scopeOrdering.compare(left.referrer, right.referrer)
      if (scope != 0) scope
      else summon[Ordering[PackageName]].compare(left.requiredPackage, right.requiredPackage)
    }
  }

  final case class ScopeDependency(
      from: Scope,
      packageName: PackageName,
      to: Scope
  )

  final case class BindingRequest[Path](
      key: BindingKey,
      provider: Scope,
      interface: Option[InterfaceSource[Path]]
  )

  private val scopeDependencyOrdering: Ordering[ScopeDependency] =
    new Ordering[ScopeDependency] {
      def compare(left: ScopeDependency, right: ScopeDependency): Int = {
        val from = scopeOrdering.compare(left.from, right.from)
        if (from != 0) from
        else {
          val packageName = summon[Ordering[PackageName]].compare(
            left.packageName, right.packageName
          )
          if (packageName != 0) packageName else scopeOrdering.compare(left.to, right.to)
        }
      }
    }

  final case class CheckedBinding[Path](
      key: BindingKey,
      owner: PackageOwner,
      interface: Option[InterfaceSource[Path]],
      witnessPath: List[Scope]
  )

  private val bindingOwnerOrdering: Ordering[(BindingKey, PackageOwner)] =
    new Ordering[(BindingKey, PackageOwner)] {
      def compare(
          left: (BindingKey, PackageOwner),
          right: (BindingKey, PackageOwner)
      ): Int = {
        val binding = bindingKeyOrdering.compare(left._1, right._1)
        if (binding != 0) binding
        else packageOwnerOrdering.compare(left._2, right._2)
      }
    }

  private def scopeIdentityJson(scope: Scope): Json = scope match {
    case Scope.Member(name) => Json.JObject(List(
      "kind" -> Json.JString("member"),
      "member" -> Json.JString(name.asString)
    ))
    case Scope.Library(identity) => Json.JObject(List(
      "kind" -> Json.JString("library"),
      "identity" -> identityJson(identity)
    ))
    case Scope.Predef => Json.JObject(List("kind" -> Json.JString("predef")))
  }

  /** Hash selected caller-relative keys and owners, excluding path and
    * diagnostic provenance retained by CheckedBinding.
    */
  def selectedBindingFingerprint(
      bindings: IterableOnce[CheckedBinding[?]]
  ): HashValue[Algo.Blake3] = {
    val selected = bindings.iterator
      .map(binding => binding.key -> binding.owner)
      .toList
      .sorted(using bindingOwnerOrdering)
      .map { case (key, owner) =>
        Json.JObject(List(
          "referrer" -> scopeIdentityJson(key.referrer),
          "package" -> Json.JString(key.requiredPackage.asString),
          "owner" -> Json.JObject(List(
            "scope" -> scopeIdentityJson(owner.scope),
            "package" -> Json.JString(owner.packageName.asString)
          ))
        ))
      }
    val bytes = Json.JArray(selected.toVector).render.getBytes(StandardCharsets.UTF_8)
    Algo.blake3Algo.hashBytes(bytes)
  }

  /** Legacy callers remain explicitly unscoped until they opt into a checked
    * scope map.
    */
  sealed trait BindingMode[+Path]
  object BindingMode {
    case object Legacy extends BindingMode[Nothing]
    final case class Scoped[Path](value: CheckedScopeMap[Path]) extends BindingMode[Path]
  }

  final case class ScopeMap[Path](
      members: SortedSet[PackageName],
      dependencies: List[ScopeDependency],
      imports: SortedMap[LibraryIdentity, ImportedScope[Path]],
      publicIndex: PublicIndex,
      directInterfaces: SortedSet[Path],
      bindings: SortedMap[BindingKey, BindingRequest[Path]]
  )

  final case class CheckedScopeMap[Path](
      scopeMap: ScopeMap[Path],
      publicIndex: PublicIndex,
      bindings: SortedMap[BindingKey, CheckedBinding[Path]]
  )

  /** Complete public closure for one consuming member. The retained routes
    * start at that member, so later consumers can report conflicts without
    * reconstructing provenance from the caller's declaration order.
    */
  final case class MemberClosure[Path](
      bindings: SortedMap[BindingKey, CheckedBinding[Path]],
      owners: SortedMap[PackageName, PackageOwner],
      routes: SortedMap[PackageOwner, List[Scope]]
  )

  private def appendRoute(prefix: List[Scope], suffix: List[Scope]): List[Scope] =
    (prefix.lastOption, suffix.headOption) match {
      case (Some(last), Some(first)) if summon[Eq[Scope]].eqv(last, first) =>
        prefix ::: suffix.tail
      case _ => prefix ::: suffix
    }

  private def routeText(route: List[Scope]): String =
    route.map(_.show).mkString(" -> ")

  private def compareRoutes(left: List[Scope], right: List[Scope]): Int = {
    val common = left.iterator.zip(right.iterator).map { case (a, b) =>
      scopeOrdering.compare(a, b)
    }.find(_ != 0)
    common.getOrElse(left.length.compare(right.length))
  }

  /** Resolve roots independently of the supplied binding list, then follow
    * only the selected providers' public exposures. Local providers require
    * explicit exposure metadata: `None` means the caller did not supply it,
    * while `Some(Nil)` is a known empty public API.
    */
  def resolveMemberClosures[Path](
      requirements: List[(PackageName, List[PackageName])],
      checked: CheckedScopeMap[Path],
      memberExposures: Map[PackageName, Option[List[PackageName]]],
      builtinPredef: Boolean,
      localPackages: Set[PackageName] = Set.empty
  ): Either[String, SortedMap[PackageName, MemberClosure[Path]]] = {
    final case class Need(key: BindingKey, routeToReferrer: List[Scope])
    final case class Visible(owner: PackageOwner, route: List[Scope])

    val duplicateMembers = duplicate(requirements.map(_._1), summon[Ordering[PackageName]])
    if (duplicateMembers.nonEmpty) return Left("duplicate member closure requirements")

    val orderedRequirements = requirements.sortBy(_._1)
    var resolved = SortedMap.empty[PackageName, MemberClosure[Path]](
      using summon[Ordering[PackageName]]
    )
    var requirementIndex = 0
    while (requirementIndex < orderedRequirements.size) {
      val (member, roots) = orderedRequirements(requirementIndex)
      requirementIndex += 1
      val memberScope = Scope.Member(member)
      val todo = scala.collection.mutable.ArrayDeque.from(
        roots.distinct.sorted.map(name => Need(BindingKey(memberScope, name), List(memberScope)))
      )
      val seen = scala.collection.mutable.TreeSet.empty[BindingKey](using bindingKeyOrdering)
      val bindings = scala.collection.mutable.TreeMap.empty[BindingKey, CheckedBinding[Path]](
        using bindingKeyOrdering
      )
      val owners = scala.collection.mutable.TreeMap.empty[PackageName, Visible](
        using summon[Ordering[PackageName]]
      )
      val releases = scala.collection.mutable.Map.empty[Name, (LibraryIdentity, Visible)]
      var error: Option[String] = None

      def describe(visible: Visible): String =
        s"${packageOwnerShow.show(visible.owner)} via ${routeText(visible.route)}"

      def addVisible(owner: PackageOwner, route: List[Scope]): Unit = {
        val visible = Visible(owner, route)
        owner.scope match {
          case Scope.Library(_) if localPackages(owner.packageName) =>
            error = Some(
              s"imported provider ${packageOwnerShow.show(owner)} via ${routeText(route)} " +
                s"also claims local package ${owner.packageName.asString} via " +
                routeText(List(Scope.Member(owner.packageName)))
            )
          case _ => ()
        }
        owners.get(owner.packageName) match {
          case Some(previous) if !samePackageOwner(previous.owner, owner) =>
            error = Some(
              s"conflicting visible owners for ${owner.packageName.asString}: " +
                s"${describe(previous)} and ${describe(visible)}"
            )
          case Some(previous) if compareRoutes(route, previous.route) < 0 =>
            owners.update(owner.packageName, visible)
          case None => owners.update(owner.packageName, visible)
          case _ => ()
        }

        owner.scope match {
          case Scope.Library(identity @ (name, _)) =>
            releases.get(name) match {
              case Some((previousIdentity, previous))
                  if !sameLibraryIdentity(previousIdentity, identity) =>
                error = Some(
                  s"conflicting visible releases for ${name.name}: member ${member.asString} " +
                    s"sees two releases of library ${name.name}: " +
                    s"${describe(previous)} and ${describe(visible)}"
                )
              case Some((_, previous)) if compareRoutes(route, previous.route) < 0 =>
                releases.update(name, identity -> visible)
              case None => releases.update(name, identity -> visible)
              case _ => ()
            }
          case _ => ()
        }
      }

      while (todo.nonEmpty && error.isEmpty) {
        val need = todo.removeHead()
        val key = need.key
        if (seen.add(key)) {
          val binding = checked.bindings.get(key)
          if (binding.isEmpty && key.requiredPackage.isPredef && builtinPredef) ()
          else if (binding.isEmpty) {
            error = Some(
              s"missing interface binding for ${key.requiredPackage.asString}: " +
                routeText(need.routeToReferrer)
            )
          } else {
            val selected = binding.get
            val route = appendRoute(need.routeToReferrer, selected.witnessPath)
            if (selected.owner.packageName != key.requiredPackage) {
              error = Some(
                s"binding for ${key.requiredPackage.asString} selects " +
                  packageOwnerShow.show(selected.owner)
              )
            } else {
              bindings.update(key, selected.copy(witnessPath = route))
              addVisible(selected.owner, route)

              if (error.isEmpty) selected.owner.scope match {
                case Scope.Predef =>
                  if (!selected.owner.packageName.isPredef)
                    error = Some("compiler Predef can only provide Bosatsu/Predef")
                  else if (!builtinPredef)
                    error = Some("compiler Predef binding requires builtin Predef")
                case Scope.Member(provider) =>
                  memberExposures.get(provider).flatten match {
                    case None =>
                      error = Some(
                        s"missing public exposure metadata for source provider ${provider.asString}"
                      )
                    case Some(exposures) =>
                      todo.appendAll(exposures.distinct.sorted.filterNot(_.isPredef).map { name =>
                        Need(BindingKey(Scope.Member(provider), name), route)
                      })
                  }
                case Scope.Library(identity) =>
                  val owner = selected.owner
                  checked.publicIndex.packages.get(owner) match {
                    case None =>
                      error = Some(
                        s"selected provider ${libraryIdentityShow.show(identity)} has no public package " +
                          owner.packageName.asString
                      )
                    case Some(publicPackage) =>
                      val exposures = publicPackage.exposures.iterator.collect {
                        case (name, PublicProvider.Library(_, _)) if !name.isPredef => name
                      }.toList
                      todo.appendAll(exposures.map { name =>
                        Need(BindingKey(Scope.Library(identity), name), route)
                      })
                  }
              }
            }
          }
        }
      }

      error match {
        case Some(message) => return Left(message)
        case None =>
          val sortedOwners = SortedMap.from(owners.iterator.map { case (name, visible) =>
            name -> visible.owner
          })(using summon[Ordering[PackageName]])
          val sortedRoutes = SortedMap.from(owners.valuesIterator.map { visible =>
            visible.owner -> visible.route
          })(using packageOwnerOrdering)
          resolved = resolved.updated(member, MemberClosure(
            SortedMap.from(bindings)(using bindingKeyOrdering), sortedOwners, sortedRoutes
          ))
      }
    }
    Right(resolved)
  }

  private final case class RawImportedScope(
      identity: LibraryIdentity,
      signaturePath: String,
      publicIndexPath: String,
      label: Option[String]
  )

  private final case class RawBinding(
      key: BindingKey,
      provider: Scope,
      interfacePath: Option[String]
  )

  private final case class ResolvedBinding[Path](
      key: BindingKey,
      provider: Scope,
      interfacePath: Option[Path]
  )

  private final case class RawScopeMap(
      members: List[PackageName],
      dependencies: List[ScopeDependency],
      imports: List[RawImportedScope],
      bindings: List[RawBinding]
  )

  private type Result[A] = BuildManifest.Result[A]

  private val libraryNameReader: Json.Reader[Name] = new Json.Reader[Name] {
    val describe = "nonempty library name"
    def read(path: Json.Path, value: Json): Result[Name] =
      BuildManifest.nonEmpty.read(path, value).map(Name(_))
  }

  private val identityReader: Json.Reader[LibraryIdentity] =
    BuildManifest.obj("library identity") { from =>
      for {
        _ <- BuildManifest.onlyFields(from, "library identity", Set("name", "version"))
        name <- from.field[Name]("name")(using libraryNameReader)
        version <- from.field[Version]("version")
      } yield name -> version
    }

  private val scopeReader: Json.Reader[Scope] = new Json.Reader[Scope] {
    val describe = "scope identity"
    def read(path: Json.Path, value: Json): Result[Scope] = value match {
      case obj: Json.JObject =>
        val from = Json.Reader.FromObj(path, obj)
        from.field[String]("kind")(using BuildManifest.nonEmpty).flatMap {
          case "member" =>
            for {
              _ <- BuildManifest.onlyFields(from, "member scope", Set("kind", "member"))
              name <- from.field[PackageName]("member")
            } yield Scope.Member(name)
          case "library" =>
            for {
              _ <- BuildManifest.onlyFields(from, "library scope", Set("kind", "identity"))
              identity <- from.field[LibraryIdentity]("identity")(using identityReader)
            } yield Scope.Library(identity)
          case "predef" =>
            BuildManifest.onlyFields(from, "Predef scope", Set("kind")).as(Scope.Predef)
          case other => Left((s"unknown scope kind '$other'", value, path.key("kind")))
        }
      case _ => Left(("expected scope identity object", value, path))
    }
  }

  private sealed trait RawProvider
  private object RawProvider {
    final case class Compiler(name: String) extends RawProvider
    final case class Library(identity: LibraryIdentity) extends RawProvider
  }

  private val rawProviderReader: Json.Reader[RawProvider] = new Json.Reader[RawProvider] {
    val describe = "public provider"
    def read(path: Json.Path, value: Json): Result[RawProvider] = value match {
      case obj: Json.JObject =>
        val from = Json.Reader.FromObj(path, obj)
        obj.toMap.get("kind") match {
          case Some(Json.JString("compiler")) =>
            for {
              _ <- BuildManifest.onlyFields(from, "compiler provider", Set("kind", "name"))
              name <- from.field[String]("name")(using BuildManifest.nonEmpty)
            } yield RawProvider.Compiler(name)
          case Some(Json.JString(other)) =>
            Left((s"unknown public provider kind '$other'", value, path.key("kind")))
          case Some(_) =>
            Left(("expected public provider kind string", value, path.key("kind")))
          case None => identityReader.read(path, value).map(RawProvider.Library(_))
        }
      case _ => Left(("expected public provider object", value, path))
    }
  }

  private final case class RawExposure(
      packageName: PackageName,
      provider: RawProvider,
      witnessPath: List[LibraryIdentity]
  )

  private val rawExposureReader: Json.Reader[RawExposure] =
    BuildManifest.obj("public exposure") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from, "public exposure", Set("package", "provider", "witness_path")
        )
        packageName <- from.field[PackageName]("package")
        provider <- from.field[RawProvider]("provider")(using rawProviderReader)
        witness <- from.field[List[LibraryIdentity]]("witness_path")(
          using Json.Reader.listReader(using identityReader)
        )
      } yield RawExposure(packageName, provider, witness)
    }

  private final case class RawPublicPackage(
      library: LibraryIdentity,
      packageName: PackageName,
      owner: LibraryIdentity,
      exposures: List[RawExposure]
  )

  private val rawPublicPackageReader: Json.Reader[RawPublicPackage] =
    BuildManifest.obj("public package index entry") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from, "public package entry", Set("library", "package", "owner", "exposes")
        )
        library <- from.field[LibraryIdentity]("library")(using identityReader)
        packageName <- from.field[PackageName]("package")
        owner <- from.field[LibraryIdentity]("owner")(using identityReader)
        exposures <- from.field[List[RawExposure]]("exposes")(
          using Json.Reader.listReader(using rawExposureReader)
        )
      } yield RawPublicPackage(library, packageName, owner, exposures)
    }

  private val publicIndexReader: Json.Reader[PublicIndex] =
    BuildManifest.obj("public package index") { from =>
      for {
        _ <- BuildManifest.version1(from)
        _ <- BuildManifest.onlyFields(from, "public package index", Set("schema_version", "packages"))
        entries <- from.field[List[RawPublicPackage]]("packages")(
          using Json.Reader.listReader(using rawPublicPackageReader)
        )
        index <- entries.foldM(
          SortedMap.empty[PackageOwner, PublicPackage](using packageOwnerOrdering)
        ) { (packages, raw) =>
          val ownerMatches = libraryIdentityOrder.eqv(raw.library, raw.owner)
          val checkedExposures = raw.exposures.foldM(
            SortedMap.empty[PackageName, PublicProvider](using summon[Ordering[PackageName]])
          ) { (exposures, exposure) =>
            val provider = exposure.provider match {
              case RawProvider.Compiler("Predef") if summon[Order[PackageName]].eqv(
                    exposure.packageName, PackageName.PredefName
                  ) &&
                  exposure.witnessPath.isEmpty => Right(PublicProvider.CompilerPredef)
              case RawProvider.Compiler(name) =>
                Left(s"invalid compiler provider '$name' for ${exposure.packageName.asString}")
              case RawProvider.Library(identity) =>
                val routeStartsAtOwner = exposure.witnessPath.headOption.exists(
                  libraryIdentityOrder.eqv(_, raw.library)
                )
                val routeEndsAtProvider = exposure.witnessPath.lastOption.exists(
                  libraryIdentityOrder.eqv(_, identity)
                )
                Either.cond(
                  routeStartsAtOwner && routeEndsAtProvider,
                  PublicProvider.Library(identity, exposure.witnessPath),
                  s"invalid witness path for exposed package ${exposure.packageName.asString} " +
                    s"from ${libraryIdentityShow.show(raw.library)} to ${libraryIdentityShow.show(identity)}"
                )
            }
            provider.flatMap { checked =>
              Either.cond(
                !exposures.contains(exposure.packageName),
                exposures.updated(exposure.packageName, checked),
                s"duplicate exposure ${exposure.packageName.asString} in " +
                  s"${libraryIdentityShow.show(raw.library)} package ${raw.packageName.asString}"
              )
            }
          }
          for {
            _ <- Either.cond(ownerMatches, (),
              s"public package owner does not match library ${libraryIdentityShow.show(raw.library)}")
            exposures <- checkedExposures
            key = PackageOwner(Scope.Library(raw.library), raw.packageName)
            _ <- Either.cond(!packages.contains(key), (),
              s"duplicate public package ${raw.packageName.asString} in ${libraryIdentityShow.show(raw.library)}")
          } yield packages.updated(key, PublicPackage(raw.library, raw.packageName, exposures))
        }.leftMap(message => (message, from.j, from.path))
      } yield indexFromPackages(index.valuesIterator.toList)
    }

  private val rawImportedScopeReader: Json.Reader[RawImportedScope] =
    BuildManifest.obj("imported scope") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from, "imported scope", Set("identity", "signature_path", "public_index_path", "label")
        )
        identity <- from.field[LibraryIdentity]("identity")(using identityReader)
        signaturePath <- from.field[String]("signature_path")(using BuildManifest.nonEmpty)
        publicIndexPath <- from.field[String]("public_index_path")(using BuildManifest.nonEmpty)
        label <- from.optional[String]("label")(using BuildManifest.nonEmpty)
      } yield RawImportedScope(identity, signaturePath, publicIndexPath, label)
    }

  private val rawScopeDependencyReader: Json.Reader[ScopeDependency] =
    BuildManifest.obj("scope dependency") { from =>
      for {
        _ <- BuildManifest.onlyFields(from, "scope dependency", Set("from", "package", "to"))
        source <- from.field[Scope]("from")(using scopeReader)
        packageName <- from.field[PackageName]("package")
        target <- from.field[Scope]("to")(using scopeReader)
      } yield ScopeDependency(source, packageName, target)
    }

  private val rawBindingReader: Json.Reader[RawBinding] =
    BuildManifest.obj("package binding") { from =>
      for {
        _ <- BuildManifest.onlyFields(
          from, "package binding", Set("referrer", "package", "provider", "interface_path")
        )
        referrer <- from.field[Scope]("referrer")(using scopeReader)
        packageName <- from.field[PackageName]("package")
        provider <- from.field[Scope]("provider")(using scopeReader)
        interfacePath <- from.optional[String]("interface_path")(using BuildManifest.nonEmpty)
      } yield RawBinding(BindingKey(referrer, packageName), provider, interfacePath)
    }

  private val rawScopeMapReader: Json.Reader[RawScopeMap] =
    BuildManifest.obj("scope map") { from =>
      for {
        _ <- BuildManifest.version1(from)
        _ <- BuildManifest.onlyFields(
          from, "scope map", Set("schema_version", "members", "dependencies", "imports", "bindings")
        )
        members <- from.field[List[PackageName]]("members")(
          using Json.Reader.listReader(using summon[Json.Reader[PackageName]])
        )
        dependencies <- from.field[List[ScopeDependency]]("dependencies")(
          using Json.Reader.listReader(using rawScopeDependencyReader)
        )
        imports <- from.field[List[RawImportedScope]]("imports")(
          using Json.Reader.listReader(using rawImportedScopeReader)
        )
        bindings <- from.field[List[RawBinding]]("bindings")(
          using Json.Reader.listReader(using rawBindingReader)
        )
      } yield RawScopeMap(members, dependencies, imports, bindings)
    }

  private def duplicate[A](
      values: List[A],
      ordering: Ordering[A]
  ): Option[A] = {
    val sorted = values.sorted(using ordering)
    sorted.zip(sorted.drop(1)).collectFirst {
      case (left, right) if ordering.compare(left, right) == 0 => right
    }
  }

  private def providerCandidates(
      referrer: Scope,
      requiredPackage: PackageName,
      dependencies: List[ScopeDependency],
      publicIndex: PublicIndex,
      memberExposures: Map[PackageName, Option[List[PackageName]]]
  ): SortedMap[PackageOwner, List[Scope]] = {
    val initial = referrer match {
      case Scope.Member(_) =>
        dependencies.collect {
          case edge if summon[Eq[Scope]].eqv(edge.from, referrer) =>
            val packageName = edge.packageName
            val target = edge.to
            PackageOwner(target, packageName) -> List(referrer, target)
        } ::: List(
          PackageOwner(Scope.Predef, PackageName.PredefName) -> List(referrer, Scope.Predef)
        )
      case Scope.Library(_) =>
        publicIndex.packagesFor(referrer).map(pkg => pkg.owner -> List(referrer))
      case Scope.Predef =>
        List(PackageOwner(Scope.Predef, PackageName.PredefName) -> List(Scope.Predef))
    }

    @annotation.tailrec
    def loop(
        pending: List[(PackageOwner, List[Scope])],
        seen: SortedSet[PackageOwner],
        candidates: SortedMap[PackageOwner, List[Scope]]
    ): SortedMap[PackageOwner, List[Scope]] = pending match {
      case Nil => candidates
      case (owner, _) :: rest if seen(owner) => loop(rest, seen, candidates)
      case (owner, path) :: rest =>
        val found =
          if (summon[Order[PackageName]].eqv(owner.packageName, requiredPackage))
            candidates.updated(owner, path)
          else candidates
        val children = owner.scope match {
          case Scope.Member(name) =>
            memberExposures.get(name).flatten.toList.flatten.flatMap { exposed =>
              dependencies.collect {
                case edge if edge.from == owner.scope && edge.packageName == exposed =>
                  val nextOwner = PackageOwner(edge.to, exposed)
                  nextOwner -> appendRoute(path, List(owner.scope, edge.to))
              }
            }
          case Scope.Library(_) => publicIndex.packages.get(owner).toList.flatMap { pkg =>
            pkg.exposures.toList.flatMap {
              case (packageName, PublicProvider.CompilerPredef) =>
                List(PackageOwner(Scope.Predef, packageName) -> appendRoute(path, List(Scope.Predef)))
              case (packageName, PublicProvider.Library(identity, witness)) =>
                val nextOwner = PackageOwner(Scope.Library(identity), packageName)
                val witnessScopes = witness.map(Scope.Library(_))
                if (publicIndex.owns(nextOwner))
                  List(nextOwner -> appendRoute(path, witnessScopes))
                else Nil
            }
          }
          case Scope.Predef => Nil
        }
        loop(rest ::: children, seen + owner, found)
    }

    loop(
      initial,
      SortedSet.empty[PackageOwner](using packageOwnerOrdering),
      SortedMap.empty[PackageOwner, List[Scope]](using packageOwnerOrdering)
    )
  }

  private def validateRawScopeMap[Path](
      raw: RawScopeMap,
      resolvedBindings: List[ResolvedBinding[Path]],
      directInterfaces: SortedSet[Path],
      imports: SortedMap[LibraryIdentity, ImportedScope[Path]],
      publicIndex: PublicIndex,
      pathOrder: Ordering[Path]
  ): Either[String, ScopeMap[Path]] = {
    val members = SortedSet.from(raw.members)(using summon[Ordering[PackageName]])
    val dependencyMap = SortedMap.from(
      raw.dependencies.map(edge => edge -> ())
    )(using scopeDependencyOrdering)
    val bindings = resolvedBindings.foldM(
      SortedMap.empty[BindingKey, BindingRequest[Path]](using bindingKeyOrdering)
    ) { (checked, rawBinding) =>
      val key = rawBinding.key
      val provider = rawBinding.provider
      def known(scope: Scope): Boolean = scope match {
        case Scope.Member(name) => members.contains(name)
        case Scope.Library(identity) => imports.contains(identity)
        case Scope.Predef => true
      }
      val targetOwner = PackageOwner(provider, key.requiredPackage)
      val providerImport = provider match {
        case Scope.Library(identity) => imports.get(identity)
        case _                       => None
      }
      val interfaceResult: Either[String, Option[InterfaceSource[Path]]] =
        (provider, rawBinding.interfacePath, providerImport) match {
          case (Scope.Library(identity), _, None) =>
            Left(s"binding provider ${libraryIdentityShow.show(identity)} is not a declared import")
          case (Scope.Library(identity), None, Some(_)) =>
            Left(s"binding for ${key.requiredPackage.asString} from ${libraryIdentityShow.show(identity)} " +
              "requires interface_path")
          case (Scope.Library(identity), Some(path), Some(imported)) =>
            Either.cond(
              pathOrder.compare(path, imported.signature.path) == 0,
              Some(imported.signature),
              s"binding interface path does not match provider ${libraryIdentityShow.show(identity)}"
            )
          case (Scope.Member(name), None, _) if summon[Order[PackageName]].eqv(
                name, key.requiredPackage
              ) => Right(None)
          case (Scope.Member(name), _, _) =>
            Left(s"source provider ${name.asString} does not own ${key.requiredPackage.asString}")
          case (Scope.Predef, None, _) if summon[Order[PackageName]].eqv(
                key.requiredPackage, PackageName.PredefName
              ) => Right(None)
          case (Scope.Predef, _, _) => Left("compiler Predef can only provide Bosatsu/Predef")
        }

      for {
        _ <- Either.cond(!checked.contains(key), (),
          s"duplicate binding key for ${key.requiredPackage.asString} from ${key.referrer.show}")
        _ <- Either.cond(known(key.referrer), (), s"unknown binding referrer ${key.referrer.show}")
        _ <- Either.cond(known(provider), (), s"unknown binding provider ${provider.show}")
        _ <- Either.cond(
          !summon[Eq[Scope]].eqv(key.referrer, Scope.Predef),
          (),
          "compiler Predef cannot be a binding referrer"
        )
        interface <- interfaceResult
        _ <- interface match {
          case Some(source) => Either.cond(directInterfaces.contains(source.path), (),
            s"binding interface ${source.path} is not in --direct-interface")
          case None => Right(())
        }
        _ <- Either.cond(
          provider match {
            case Scope.Library(_) => publicIndex.owns(targetOwner)
            case _                => true
          },
          (),
          s"${provider.show} does not own package ${key.requiredPackage.asString} in the public index"
        )
      } yield checked.updated(key, BindingRequest(key, provider, interface))
    }

    for {
      _ <- duplicate(raw.members, summon[Ordering[PackageName]]).toLeft(())
        .leftMap(_ => "duplicate local member identity")
      _ <- duplicate(raw.imports.map(_.identity), libraryIdentityOrdering).toLeft(())
        .leftMap(_ => "duplicate imported scope identity")
      _ <- duplicate(raw.dependencies, scopeDependencyOrdering).toLeft(())
        .leftMap(_ => "duplicate scope dependency edge")
      _ <- duplicate(raw.bindings.map(_.key), bindingKeyOrdering).toLeft(())
        .leftMap(_ => "duplicate binding key")
      _ <- raw.dependencies.traverse_ { edge =>
        val fromIsMember = edge.from match {
          case Scope.Member(_) => true
          case _               => false
        }
        val fromKnown = edge.from match {
          case Scope.Member(name) => members.contains(name)
          case _                  => false
        }
        val targetOwnsPackage = edge.to match {
          case Scope.Member(name) => summon[Order[PackageName]].eqv(name, edge.packageName)
          case Scope.Library(id) =>
            imports.contains(id) && publicIndex.owns(
              PackageOwner(Scope.Library(id), edge.packageName)
            )
          case Scope.Predef => summon[Order[PackageName]].eqv(
              edge.packageName, PackageName.PredefName
            )
        }
        Either.cond(
          fromIsMember && fromKnown && targetOwnsPackage,
          (),
          s"invalid scope dependency ${edge.from.show} imports ${edge.packageName.asString} " +
            s"from ${edge.to.show}"
        )
      }
      _ <- raw.bindings.traverse_ { binding =>
        val known = binding.key.referrer match {
          case Scope.Member(name) => members.contains(name)
          case Scope.Library(id) => imports.contains(id)
          case Scope.Predef => false
        }
        Either.cond(known, (), s"unknown binding referrer ${binding.key.referrer.show}")
      }
      checked <- bindings
    } yield ScopeMap(
      members,
      dependencyMap.keysIterator.toList,
      imports,
      publicIndex,
      directInterfaces,
      checked
    )
  }

  private def samePublicIndex(left: PublicIndex, right: PublicIndex): Boolean =
    // Identity membership is checked against the inspected graph separately;
    // only package facts are represented in the legacy JSON projection.
    left.packages.size == right.packages.size &&
      left.packages.iterator.zip(right.packages.iterator).forall {
        case ((leftOwner, leftPackage), (rightOwner, rightPackage)) =>
          packageOwnerOrder.eqv(leftOwner, rightOwner) && publicPackageEq(leftPackage, rightPackage)
      }

  private def validateBindingsAgainstInspection[Path](
      scopeMap: ScopeMap[Path],
      inspectedIndex: PublicIndex,
      actualDependencies: List[ScopeDependency],
      memberExposures: Map[PackageName, Option[List[PackageName]]]
  ): Either[String, CheckedScopeMap[Path]] = {
    for {
      _ <- Either.cond(samePublicIndex(scopeMap.publicIndex, inspectedIndex), (),
        "scope-map public index does not match the inspected library index")
      _ <- Either.cond(
        scopeMap.imports.keysIterator.forall(inspectedIndex.identities.contains),
        (),
        "an imported scope is missing from the inspected public index"
      )
      bindings <- scopeMap.bindings.iterator.toList.foldM(
        SortedMap.empty[BindingKey, CheckedBinding[Path]](using bindingKeyOrdering)
      ) { case (checked, (key, request)) =>
        val provider = request.provider
        val owner = PackageOwner(provider, key.requiredPackage)
        val candidates = providerCandidates(
          key.referrer,
          key.requiredPackage,
          actualDependencies,
          inspectedIndex,
          memberExposures
        )
        val candidateCount = candidates.size
        val knownProvider = provider match {
          case Scope.Member(name) => scopeMap.members.contains(name)
          case Scope.Library(identity) => scopeMap.imports.contains(identity)
          case Scope.Predef => true
        }
        val knownReferrer = key.referrer match {
          case Scope.Member(name) => scopeMap.members.contains(name)
          case Scope.Library(identity) => scopeMap.imports.contains(identity)
          case Scope.Predef => false
        }
        for {
          _ <- Either.cond(knownReferrer, (), s"unknown binding referrer ${key.referrer.show}")
          _ <- Either.cond(knownProvider, (), s"unknown binding provider ${provider.show}")
          _ <- Either.cond(
            provider match {
              case Scope.Library(_) => inspectedIndex.owns(owner)
              case _                => true
            },
            (),
            s"${provider.show} does not own package ${key.requiredPackage.asString} in inspected index"
          )
          _ <- Either.cond(candidateCount > 0, (),
            s"no public dependency path from ${key.referrer.show} to ${packageOwnerShow.show(owner)}")
          _ <- Either.cond(candidateCount == 1, (),
            s"ambiguous providers for ${key.requiredPackage.asString} from ${key.referrer.show}: " +
              candidates.iterator.map { case (candidate, route) =>
                s"${packageOwnerShow.show(candidate)} via ${routeText(route)}"
              }.mkString(", "))
          _ <- Either.cond(candidates.contains(owner), (),
            s"binding selects ${packageOwnerShow.show(owner)}, but the public dependency path selects " +
              candidates.iterator.map { case (candidate, route) =>
                s"${packageOwnerShow.show(candidate)} via ${routeText(route)}"
              }.mkString(", "))
        } yield checked.updated(key, CheckedBinding(key, owner, request.interface, candidates(owner)))
      }
    } yield CheckedScopeMap(scopeMap, inspectedIndex, bindings)
  }

  /** Turn parsed declarations into checked bindings only after the caller has
    * inspected/authenticated the imported graph and supplied source-derived
    * local dependency edges. The public projection file named by a scope map
    * is a declared input, never an authentication capability by itself.
    */
  def validateAgainstInspection[Path](
      scopeMap: ScopeMap[Path],
      inspectedIndex: PublicIndex,
      sourceDependencies: List[ScopeDependency],
      memberExposures: Map[PackageName, Option[List[PackageName]]] = Map.empty
  ): Either[String, CheckedScopeMap[Path]] = {
    val declaredDependencies = scopeMap.dependencies.sorted(using scopeDependencyOrdering)
    val actualDependencies = sourceDependencies.sorted(using scopeDependencyOrdering)
    val dependenciesMatch =
      duplicate(sourceDependencies, scopeDependencyOrdering).isEmpty &&
        declaredDependencies.length == actualDependencies.length &&
        declaredDependencies.zip(actualDependencies).forall { case (declared, actual) =>
          scopeDependencyOrdering.compare(declared, actual) == 0
        }

    Either.cond(dependenciesMatch, (),
      "scope-map dependency edges do not match source-derived dependencies")
      .flatMap(_ => validateBindingsAgainstInspection(
        scopeMap, inspectedIndex, actualDependencies, memberExposures
      ))
  }

  /** Graph validation has no source headers to derive local edges from. Its
    * dependency declarations are the structural roots and are checked by the
    * complete closure resolver after the full topology is read.
    */
  def validateDeclaredBindingsAgainstInspection[Path](
      scopeMap: ScopeMap[Path],
      inspectedIndex: PublicIndex,
      memberExposures: Map[PackageName, Option[List[PackageName]]]
  ): Either[String, CheckedScopeMap[Path]] =
    validateBindingsAgainstInspection(
      scopeMap, inspectedIndex, scopeMap.dependencies, memberExposures
    )

  /** Read and check a version-1 scope map. Every public index and interface
    * path comes from an explicit path in the map; interface targets must also
    * be present in the caller's direct-interface list.
    */
  def read[F[_], Path](
      platform: PlatformIO[F, Path],
      file: Path,
      directInterfaces: List[Path]
  ): F[ScopeMap[Path]] = {
    import platform.{moduleIOMonad, pathOrdering, showPath}

    val orderedInterfaces = SortedSet.from(directInterfaces)(using pathOrdering)

    def pathFrom(raw: String, field: String): Either[String, Path] =
      platform.path(raw).toEither.leftMap(errors =>
        s"invalid $field '$raw': ${errors.toList.mkString(", ")}"
      )

    def readPaths(raw: RawScopeMap): F[(SortedMap[LibraryIdentity, ImportedScope[Path]], PublicIndex)] =
      moduleIOMonad.fromEither(
        raw.imports.foldM(
          SortedMap.empty[LibraryIdentity, ImportedScope[Path]](using libraryIdentityOrdering)
        ) { (resolved, imported) =>
          for {
            signature <- pathFrom(imported.signaturePath, "signature path")
            publicIndex <- pathFrom(imported.publicIndexPath, "public index path")
            _ <- Either.cond(orderedInterfaces.contains(signature), (),
              s"imported signature $signature is not in --direct-interface")
            _ <- Either.cond(!resolved.contains(imported.identity), (),
              s"duplicate imported scope ${libraryIdentityShow.show(imported.identity)}")
          } yield resolved.updated(imported.identity, ImportedScope(
            imported.identity,
            InterfaceSource(signature, imported.label),
            publicIndex
          ))
        }.leftMap(CliException.Basic(_))
      ).flatMap { imports =>
        val indexPaths = SortedSet.from(imports.valuesIterator.map(_.publicIndexPath))(using pathOrdering)
        indexPaths.toList.foldM(
          SortedMap.empty[Path, PublicIndex](using pathOrdering)
        ) { (indexes, indexPath) =>
          BuildManifest.read(platform, indexPath, publicIndexReader)
            .map(index => indexes.updated(indexPath, index))
        }.flatMap { indexes =>
          val merged = indexes.valuesIterator.toList.foldM(
            SortedMap.empty[PackageOwner, PublicPackage](using packageOwnerOrdering)
          ) { (all, index) =>
            index.packages.iterator.toList.foldM(all) { case (current, (owner, pkg)) =>
              current.get(owner) match {
                case None => Right(current.updated(owner, pkg))
                case Some(existing) if publicPackageEq(existing, pkg) => Right(current)
                case Some(_) => Left(s"conflicting public index records for ${packageOwnerShow.show(owner)}")
              }
            }
          }
          val checkedIndex = merged.flatMap { all =>
            val index = indexFromPackages(
              all.valuesIterator.toList,
              indexes.valuesIterator.flatMap(_.identities)
            )
            all.valuesIterator.toList.traverse_ { pkg =>
              pkg.exposures.toList.traverse_ {
                case (_, PublicProvider.CompilerPredef) => Right(())
                case (packageName, PublicProvider.Library(identity, _)) =>
                  Either.cond(
                    index.owns(PackageOwner(Scope.Library(identity), packageName)),
                    (),
                    s"public exposure ${packageName.asString} points to missing provider " +
                      s"${libraryIdentityShow.show(identity)}"
                  )
              }
            }.as(index)
          }
          moduleIOMonad.fromEither(checkedIndex.leftMap(message => CliException.Basic(message)))
            .map(all => imports -> all)
        }
      }

    BuildManifest.read(platform, file, rawScopeMapReader).flatMap { raw =>
      readPaths(raw).flatMap { case (imports, publicIndex) =>
        val resolvedBindings = raw.bindings.traverse { binding =>
          binding.interfacePath.traverse(path => pathFrom(path, "binding interface path"))
            .map(path => ResolvedBinding(binding.key, binding.provider, path))
        }.leftMap(CliException.Basic(_))
        moduleIOMonad.fromEither(resolvedBindings).flatMap { bindings =>
          val checked = validateRawScopeMap(
            raw,
            bindings,
            orderedInterfaces,
            imports,
            publicIndex,
            pathOrdering
          )
          moduleIOMonad.fromEither(checked.leftMap { message =>
            CliException.Basic(show"invalid scope map $file: $message")
          })
        }
      }
    }
  }
}
