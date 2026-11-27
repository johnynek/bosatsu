package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyList
import dev.bosatsu.cache.CompileCache
import dev.bosatsu.Identifier.{Constructor, Name}
import dev.bosatsu.{Bytes, Json, MemoryMain}
import dev.bosatsu.library.{LibraryIdentity, Name as LibraryName, Version}
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DataRepr, DefinedType, Type, TypeAlias}
import dev.bosatsu.tool.PackageBindings
import scala.collection.immutable.SortedMap
import scala.util.Try

class PackageSignaturesTest extends munit.FunSuite {
  private type BindingEff[A] = Either[Throwable, A]
  private type BindingPath = Chain[String]
  private val bindingPlatform = MemoryMain[BindingEff].platformIO
  private given Show[BindingPath] = Show.show(_.toList.mkString("/"))
  private given Ordering[BindingPath] = Ordering.by(_.toList.mkString("/"))

  private val owner = PackageName.parts("Owner")
  private val facade = PackageName.parts("Facade")
  private val client = PackageName.parts("Client")

  private def interface(
      name: PackageName, exports: List[ExportedName[Referant[Kind.Arg]]]
  ): Package.Interface = Package(name, Nil, exports, ())

  private val zero = ConstructorFn(Constructor("Zero"), Nil, Nil)
  private val more = ConstructorFn(
    Constructor("More"),
    List(ConstructorParam(
      Name("count"), Type.IntType, Some(Name("default_count")), Some(Type.IntType)
    )),
    List((Type.Var.Bound("element"), Kind.Type.co))
  )
  private val flag = DefinedType(
    owner, TypeName(Constructor("Flag")), List((Type.Var.Bound("a"), Kind.Type.phantom)),
    List(zero, more)
  )
  private val alias = TypeAlias(
    owner, TypeName(Constructor("Flags")), Nil, Type.TyApply(flag.toTypeTyConst, Type.IntType)
  )
  private val ownerIface = interface(owner, List(
    ExportedName.TypeName(flag.name.ident, Referant.DefinedT(flag)),
    ExportedName.Constructor(zero.name, Referant.Constructor(flag, zero)),
    ExportedName.Constructor(more.name, Referant.Constructor(flag, more)),
    ExportedName.TypeName(alias.name.ident, Referant.TypeAliasT(alias))
  ))
  private val facadeIface = interface(facade, List(
    ExportedName.TypeName(Constructor("ForeignFlag"), Referant.DefinedT(flag)),
    ExportedName.Constructor(Constructor("ForeignZero"), Referant.Constructor(flag, zero)),
    ExportedName.TypeName(Constructor("ForeignFlags"), Referant.TypeAliasT(alias))
  ))
  private def success[A](result: Try[A]): A =
    result.fold(e => fail(e.toString), identity)

  private def signature(ifaces: Package.Interface*): proto.Interfaces =
    success(PackageSignatures.encode(ifaces.toList))

  private def path(raw: String): BindingPath =
    Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private def identityJson(identity: LibraryIdentity): Json = Json.JObject(List(
    "name" -> Json.JString(identity._1.name),
    "version" -> Json.JString(identity._2.render)
  ))

  private def scopeJson(scope: PackageBindings.Scope): Json = scope match {
    case PackageBindings.Scope.Member(member) => Json.JObject(List(
      "kind" -> Json.JString("member"), "package" -> Json.JString(member.asString)
    ))
    case PackageBindings.Scope.Local(name) => Json.JObject(List(
      "kind" -> Json.JString("local-signature"), "package" -> Json.JString(name.asString)
    ))
    case PackageBindings.Scope.Library(identity) => Json.JObject(List(
      "kind" -> Json.JString("library"), "identity" -> identityJson(identity)
    ))
    case PackageBindings.Scope.Predef => Json.JObject(List("kind" -> Json.JString("predef")))
  }

  private def dependencyJson(
      from: PackageBindings.Scope,
      packageName: PackageName,
      to: PackageBindings.Scope
  ): Json = Json.JObject(List(
    "from" -> scopeJson(from),
    "package" -> Json.JString(packageName.asString),
    "to" -> scopeJson(to)
  ))

  private def bindingJson(
      from: PackageBindings.Scope,
      packageName: PackageName,
      to: PackageBindings.Scope,
      interface: Option[String]
  ): Json = Json.JObject(List(
    "referrer" -> scopeJson(from),
    "package" -> Json.JString(packageName.asString),
    "provider" -> scopeJson(to),
    "interface_path" -> interface.fold[Json](Json.JNull)(Json.JString(_))
  ))

  private def importedJson(
      identity: LibraryIdentity,
      signaturePath: String
  ): Json = Json.JObject(List(
    "identity" -> identityJson(identity),
    "signature_path" -> Json.JString(signaturePath),
    "exposures_path" -> Json.JString("public-index.json")
  ))

  private def checkedScopeMap(
      members: List[PackageName],
      dependencies: List[Json],
      imports: List[Json],
      bindings: List[Json],
      index: PackageBindings.PublicIndex,
      directInterfaces: List[String],
      localInterfaces: List[PackageBindings.LocalInterface[BindingPath]]
  ): PackageBindings.CheckedScopeMap[BindingPath] = {
    val scopeMap = Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "members" -> Json.JArray(members.map(m => Json.JString(m.asString)).toVector),
      "dependencies" -> Json.JArray(dependencies.toVector),
      "imports" -> Json.JArray(imports.toVector),
      "bindings" -> Json.JArray(bindings.toVector)
    ))
    val files = List(
      "scope-map.json" -> scopeMap.render,
      "public-index.json" -> index.toJson.render
    )
    val state = files.foldLeft(MemoryMain.State.empty) { case (current, (file, contents)) =>
      current.withFile(path(file), MemoryMain.FileContent.Bytes(Bytes.fromUTF8String(contents))).get
    }
    val parsed = PackageBindings.read(
      bindingPlatform, path("scope-map.json"), directInterfaces.map(path)
    ).runA(state).toOption.get
    PackageBindings.validateAgainstInspection(
      parsed, index, parsed.dependencies, localInterfaces = localInterfaces,
      memberExposures = Map.empty
    )
      .fold(error => fail(error), identity)
  }

  private def libraryIdentity(name: String, versionText: String): LibraryIdentity =
    LibraryName(name) -> Version.parser.parseAll(versionText).toOption.get

  test("scoped selection keeps byte-identical providers separate for disjoint members") {
    val left = PackageName.parts("Left")
    val right = PackageName.parts("Right")
    val api = PackageName.parts("Shared/Api")
    val sharedOne = libraryIdentity("shared", "1.0.0")
    val sharedTwo = libraryIdentity("shared", "2.0.0")
    val providerOne = PackageBindings.Scope.Library(sharedOne)
    val providerTwo = PackageBindings.Scope.Library(sharedTwo)
    val leftScope = PackageBindings.Scope.Member(left)
    val rightScope = PackageBindings.Scope.Member(right)
    val index = PackageBindings.indexFromPackages(List(
      PackageBindings.PublicPackage(
        sharedOne, api, SortedMap.empty(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        sharedTwo, api, SortedMap.empty(using summon[Ordering[PackageName]])
      )
    ), identities = Nil)
    val scopeMap = checkedScopeMap(
      List(left, right),
      List(
        dependencyJson(leftScope, api, providerOne),
        dependencyJson(rightScope, api, providerTwo)
      ),
      List(importedJson(sharedOne, "shared-v1.sig"), importedJson(sharedTwo, "shared-v2.sig")),
      List(
        bindingJson(leftScope, api, providerOne, Some("shared-v1.sig")),
        bindingJson(rightScope, api, providerTwo, Some("shared-v2.sig"))
      ),
      index,
      List("shared-v1.sig", "shared-v2.sig", "unused.sig"),
      localInterfaces = Nil
    )
    val apiType = DefinedType(api, TypeName(Constructor("Value")), Nil, Nil)
    val apiIface = interface(api, List(
      ExportedName.TypeName(apiType.name.ident, Referant.DefinedT(apiType))
    ))
    val sameSignature = signature(apiIface)
    val bundles = List(
      PackageSignatures.Input(path("shared-v1.sig"), Some("//:shared_v1")) -> sameSignature,
      PackageSignatures.Input(path("shared-v2.sig"), Some("//:shared_v2")) -> sameSignature,
      PackageSignatures.Input(path("unused.sig"), Some("//:unused")) -> proto.Interfaces(Nil)
    )
    val sources = List(
      PackageSignatures.SourceRequirements(left, List(api), Nil),
      PackageSignatures.SourceRequirements(right, List(api), Nil)
    )

    val selected = success(PackageSignatures.planScoped(bundles, sources, scopeMap, builtinPredef = true))
    assertEquals(selected.members(left).interfaces.map(_.input.path), List(path("shared-v1.sig")))
    assertEquals(selected.members(right).interfaces.map(_.input.path), List(path("shared-v2.sig")))
    assert(PackageBindings.samePackageOwner(
      selected.members(left).interfaces.head.owner,
      PackageBindings.PackageOwner(providerOne, api)
    ))
    assert(PackageBindings.samePackageOwner(
      selected.members(right).interfaces.head.owner,
      PackageBindings.PackageOwner(providerTwo, api)
    ))
    assertEquals(
      selected.checkUsed(bundles.map(_._1)).failed.get.getMessage,
      "unused direct interfaces: unused.sig (//:unused)"
    )

    val reversed = success(PackageSignatures.planScoped(
      bundles.reverse,
      sources.reverse,
      scopeMap,
      builtinPredef = true
    ))
    assertEquals(reversed.members(left).interfaces.map(_.input.path), List(path("shared-v1.sig")))
    assertEquals(reversed.members(right).interfaces.map(_.input.path), List(path("shared-v2.sig")))
  }

  test("scoped selection follows only the selected public exposure chain") {
    val clientName = PackageName.parts("Client")
    val facade = PackageName.parts("Facade")
    val types = PackageName.parts("Types")
    val leaf = PackageName.parts("Leaf")
    val facadeLibrary = libraryIdentity("facade", "1.0.0")
    val sharedLibrary = libraryIdentity("shared", "1.0.0")
    val clientScope = PackageBindings.Scope.Member(clientName)
    val facadeScope = PackageBindings.Scope.Library(facadeLibrary)
    val sharedScope = PackageBindings.Scope.Library(sharedLibrary)
    val typeName = TypeName(Constructor("Payload"))
    val index = PackageBindings.indexFromPackages(List(
      PackageBindings.PublicPackage(
        facadeLibrary,
        facade,
        SortedMap.from(List(
          types -> PackageBindings.PublicProvider.Library(
            sharedLibrary, List(facadeLibrary, sharedLibrary)
          )
        ))(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        sharedLibrary,
        types,
        SortedMap.from(List(
          leaf -> PackageBindings.PublicProvider.Library(sharedLibrary, List(sharedLibrary))
        ))(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        sharedLibrary, leaf, SortedMap.empty(using summon[Ordering[PackageName]])
      )
    ), identities = Nil)
    val checked = checkedScopeMap(
      List(clientName),
      List(dependencyJson(clientScope, facade, facadeScope)),
      List(
        importedJson(facadeLibrary, "facade.sig"),
        importedJson(sharedLibrary, "shared.sig")
      ),
      List(
        bindingJson(clientScope, facade, facadeScope, Some("facade.sig")),
        bindingJson(facadeScope, types, sharedScope, Some("shared.sig")),
        bindingJson(sharedScope, leaf, sharedScope, Some("shared.sig"))
      ),
      index,
      List("facade.sig", "shared.sig"),
      localInterfaces = Nil
    )
    val facadeAlias = TypeAlias(facade, typeName, Nil, Type.const(types, typeName))
    val typesAlias = TypeAlias(types, typeName, Nil, Type.const(leaf, typeName))
    val leafAlias = TypeAlias(leaf, typeName, Nil, Type.IntType)
    val facadeIface = interface(facade, List(
      ExportedName.TypeName(typeName.ident, Referant.TypeAliasT(facadeAlias))
    ))
    val typesIface = interface(types, List(
      ExportedName.TypeName(typeName.ident, Referant.TypeAliasT(typesAlias))
    ))
    val leafIface = interface(leaf, List(
      ExportedName.TypeName(typeName.ident, Referant.TypeAliasT(leafAlias))
    ))
    val sibling = PackageName.parts("Sibling")
    val extra = PackageName.parts("Extra")
    val bundles = List(
      PackageSignatures.Input(path("facade.sig"), None) -> signature(
        facadeIface, interface(sibling, Nil)
      ),
      PackageSignatures.Input(path("shared.sig"), None) -> signature(
        typesIface, leafIface, interface(extra, Nil)
      )
    )

    val selected = success(PackageSignatures.planScoped(
      bundles,
      List(PackageSignatures.SourceRequirements(clientName, List(facade), Nil)),
      checked,
      builtinPredef = true
    ))
    assertEquals(
      selected.members(clientName).interfaces.map(_.owner.packageName),
      List(facade, leaf, types).sorted
    )
    assertEquals(selected.members(clientName).interfaces.map(_.input.path).distinct, List(
      path("facade.sig"), path("shared.sig")
    ))
    assertEquals(success(selected.checkUsed(bundles.map(_._1))), ())
  }

  test("scoped selection carries external local signature roots through their exposures") {
    val clientName = PackageName.parts("Client")
    val facadeName = PackageName.parts("Local/Facade")
    val typesName = PackageName.parts("Local/Types")
    val clientScope = PackageBindings.Scope.Member(clientName)
    val facadeScope = PackageBindings.Scope.Local(facadeName)
    val typesScope = PackageBindings.Scope.Local(typesName)
    val tokenName = TypeName(Constructor("Token"))
    val token = DefinedType(typesName, tokenName, Nil, Nil)
    val typesIface = interface(typesName, List(
      ExportedName.TypeName(tokenName.ident, Referant.DefinedT(token))
    ))
    val facadeIface = interface(facadeName, List(
      ExportedName.Binding(Name("token"), Referant.Value(Type.const(typesName, tokenName)))
    ))
    val bundles = List(
      PackageSignatures.Input(path("facade.sig"), Some("//local:facade")) -> signature(facadeIface),
      PackageSignatures.Input(path("types.sig"), Some("//local:types")) -> signature(typesIface)
    )
    val localRoots = success(PackageSignatures.localInterfaces(bundles))
    val index = PackageBindings.indexFromPackages(Nil, identities = Nil)
    val checked = checkedScopeMap(
      List(clientName),
      List(dependencyJson(clientScope, facadeName, facadeScope)),
      Nil,
      List(
        bindingJson(clientScope, facadeName, facadeScope, Some("facade.sig")),
        bindingJson(facadeScope, typesName, typesScope, Some("types.sig"))
      ),
      index,
      List("facade.sig", "types.sig"),
      localRoots
    )
    val sources = List(PackageSignatures.SourceRequirements(clientName, List(facadeName), Nil))
    val selected = success(PackageSignatures.planScoped(bundles, sources, checked, builtinPredef = true))
    assertEquals(
      selected.members(clientName).interfaces.map(_.owner.packageName),
      List(facadeName, typesName).sorted
    )
    assertEquals(
      selected.members(clientName).interfaces.map(_.input.path).distinct.sorted,
      List(path("facade.sig"), path("types.sig")).sorted
    )
    assertEquals(success(selected.checkUsed(bundles.map(_._1))), ())
    val reordered = success(PackageSignatures.planScoped(
      bundles.reverse, sources, checked, builtinPredef = true
    ))
    assertEquals(
      reordered.members(clientName).interfaces.map(_.owner.packageName),
      selected.members(clientName).interfaces.map(_.owner.packageName)
    )

    val duplicateRoots = success(PackageSignatures.localInterfaces(
      bundles ::: List(PackageSignatures.Input(path("facade-copy.sig"), None) -> signature(facadeIface))
    ))
    val ambiguous = PackageBindings.validateAgainstInspection(
      checked.scopeMap.copy(
        directInterfaces = checked.scopeMap.directInterfaces + path("facade-copy.sig")
      ), index, checked.scopeMap.dependencies,
      Map(clientName -> Some(Nil)), duplicateRoots
    )
    assert(ambiguous.swap.toOption.get.contains("ambiguous local signature Local/Facade"))

    val missing = PackageBindings.validateAgainstInspection(
      checked.scopeMap, index, checked.scopeMap.dependencies,
      Map(clientName -> Some(Nil)), List(localRoots.last)
    )
    assert(missing.swap.toOption.get.contains("no directly declared interface contains local package Local/Facade"))

    val conflictingSource = PackageBindings.validateAgainstInspection(
      checked.scopeMap.copy(members = checked.scopeMap.members + facadeName),
      index,
      checked.scopeMap.dependencies,
      Map(clientName -> Some(Nil)),
      localRoots
    )
    assert(conflictingSource.isLeft)
  }

  test("one member cannot select two visible releases of the same library") {
    val clientName = PackageName.parts("Client")
    val facadeOne = PackageName.parts("FacadeOne")
    val facadeTwo = PackageName.parts("FacadeTwo")
    val apiOne = PackageName.parts("Shared/ApiOne")
    val apiTwo = PackageName.parts("Shared/ApiTwo")
    val facadeOneLibrary = libraryIdentity("facade-one", "1.0.0")
    val facadeTwoLibrary = libraryIdentity("facade-two", "1.0.0")
    val sharedOne = libraryIdentity("shared", "1.0.0")
    val sharedTwo = libraryIdentity("shared", "2.0.0")
    val clientScope = PackageBindings.Scope.Member(clientName)
    val facadeOneScope = PackageBindings.Scope.Library(facadeOneLibrary)
    val facadeTwoScope = PackageBindings.Scope.Library(facadeTwoLibrary)
    val sharedOneScope = PackageBindings.Scope.Library(sharedOne)
    val sharedTwoScope = PackageBindings.Scope.Library(sharedTwo)
    def exposure(
        packageName: PackageName,
        provider: LibraryIdentity,
        witness: List[LibraryIdentity]
    ) = SortedMap.from(List(
      packageName -> PackageBindings.PublicProvider.Library(provider, witness)
    ))(using summon[Ordering[PackageName]])
    val index = PackageBindings.indexFromPackages(List(
      PackageBindings.PublicPackage(
        facadeOneLibrary, facadeOne, exposure(apiOne, sharedOne, List(facadeOneLibrary, sharedOne))
      ),
      PackageBindings.PublicPackage(
        facadeTwoLibrary, facadeTwo, exposure(apiTwo, sharedTwo, List(facadeTwoLibrary, sharedTwo))
      ),
      PackageBindings.PublicPackage(
        sharedOne, apiOne, SortedMap.empty(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        sharedTwo, apiTwo, SortedMap.empty(using summon[Ordering[PackageName]])
      )
    ), identities = Nil)
    val checked = checkedScopeMap(
      List(clientName),
      List(
        dependencyJson(clientScope, facadeOne, facadeOneScope),
        dependencyJson(clientScope, facadeTwo, facadeTwoScope)
      ),
      List(
        importedJson(facadeOneLibrary, "facade-one.sig"),
        importedJson(facadeTwoLibrary, "facade-two.sig"),
        importedJson(sharedOne, "shared-one.sig"),
        importedJson(sharedTwo, "shared-two.sig")
      ),
      List(
        bindingJson(clientScope, facadeOne, facadeOneScope, Some("facade-one.sig")),
        bindingJson(clientScope, facadeTwo, facadeTwoScope, Some("facade-two.sig")),
        bindingJson(facadeOneScope, apiOne, sharedOneScope, Some("shared-one.sig")),
        bindingJson(facadeTwoScope, apiTwo, sharedTwoScope, Some("shared-two.sig"))
      ),
      index,
      List("facade-one.sig", "facade-two.sig", "shared-one.sig", "shared-two.sig"),
      localInterfaces = Nil
    )
    val facadeOneType = TypeAlias(
      facadeOne, TypeName(Constructor("Payload")), Nil,
      Type.const(apiOne, TypeName(Constructor("Value")))
    )
    val facadeTwoType = TypeAlias(
      facadeTwo, TypeName(Constructor("Payload")), Nil,
      Type.const(apiTwo, TypeName(Constructor("Value")))
    )
    val bundles = List(
      PackageSignatures.Input(path("facade-one.sig"), None) -> signature(interface(
        facadeOne, List(ExportedName.TypeName(
          facadeOneType.name.ident, Referant.TypeAliasT(facadeOneType)
        ))
      )),
      PackageSignatures.Input(path("facade-two.sig"), None) -> signature(interface(
        facadeTwo, List(ExportedName.TypeName(
          facadeTwoType.name.ident, Referant.TypeAliasT(facadeTwoType)
        ))
      )),
      PackageSignatures.Input(path("shared-one.sig"), None) -> signature(interface(apiOne, Nil)),
      PackageSignatures.Input(path("shared-two.sig"), None) -> signature(interface(apiTwo, Nil))
    )

    val error = PackageSignatures.planScoped(
      bundles,
      List(PackageSignatures.SourceRequirements(
        clientName, List(facadeOne, facadeTwo), Nil
      )),
      checked,
      builtinPredef = true
    ).failed.get.getMessage
    assert(error.contains("conflicting visible releases for shared"), error)
    assert(error.contains("shared@1.0.0"), error)
    assert(error.contains("shared@2.0.0"), error)
    assert(error.contains("facade-one@1.0.0"), error)
    assert(error.contains("facade-two@1.0.0"), error)
  }

  test("a source dependency shares its exposed owners but keeps private imports local") {
    val clientName = PackageName.parts("Client")
    val wrapper = PackageName.parts("Wrapper")
    val types = PackageName.parts("Types")
    val secret = PackageName.parts("Secret")
    val sharedLibrary = libraryIdentity("shared", "1.0.0")
    val hiddenLibrary = libraryIdentity("hidden", "1.0.0")
    val clientScope = PackageBindings.Scope.Member(clientName)
    val wrapperScope = PackageBindings.Scope.Member(wrapper)
    val sharedScope = PackageBindings.Scope.Library(sharedLibrary)
    val hiddenScope = PackageBindings.Scope.Library(hiddenLibrary)
    val index = PackageBindings.indexFromPackages(List(
      PackageBindings.PublicPackage(
        sharedLibrary, types, SortedMap.empty(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        hiddenLibrary, secret, SortedMap.empty(using summon[Ordering[PackageName]])
      )
    ), identities = Nil)
    val checked = checkedScopeMap(
      List(clientName, wrapper),
      List(
        dependencyJson(clientScope, wrapper, wrapperScope),
        dependencyJson(wrapperScope, types, sharedScope),
        dependencyJson(wrapperScope, secret, hiddenScope)
      ),
      List(
        importedJson(sharedLibrary, "types.sig"),
        importedJson(hiddenLibrary, "secret.sig")
      ),
      List(
        bindingJson(clientScope, wrapper, wrapperScope, None),
        bindingJson(wrapperScope, types, sharedScope, Some("types.sig")),
        bindingJson(wrapperScope, secret, hiddenScope, Some("secret.sig"))
      ),
      index,
      List("types.sig", "secret.sig"),
      localInterfaces = Nil
    )
    val typeDef = DefinedType(types, TypeName(Constructor("T")), Nil, Nil)
    val secretDef = DefinedType(secret, TypeName(Constructor("S")), Nil, Nil)
    val bundles = List(
      PackageSignatures.Input(path("types.sig"), None) -> signature(interface(
        types, List(ExportedName.TypeName(typeDef.name.ident, Referant.DefinedT(typeDef)))
      )),
      PackageSignatures.Input(path("secret.sig"), None) -> signature(interface(
        secret, List(ExportedName.TypeName(secretDef.name.ident, Referant.DefinedT(secretDef)))
      ))
    )
    val sources = List(
      PackageSignatures.SourceRequirements(clientName, List(wrapper), Nil),
      PackageSignatures.SourceRequirements(wrapper, List(types, secret), List(types))
    )

    val selected = success(PackageSignatures.planScoped(bundles, sources, checked, builtinPredef = true))
    assertEquals(selected.members(clientName).sourceDependencies, List(wrapper))
    assertEquals(selected.members(clientName).interfaces.map(_.owner.packageName), List(types))
    assertEquals(selected.members(wrapper).interfaces.map(_.owner.packageName), List(secret, types))
  }

  private def decodeRequired(
      bundles: List[(String, proto.Interfaces)], requirements: List[(PackageName, List[PackageName])]
  ): Try[List[Package.Interface]] =
    for {
      resolution <- PackageSignatures.plan(
        bundles.map { case (path, bundle) => PackageSignatures.Input(path, None) -> bundle },
        requirements, builtinPredef = true
      )
      providers <- resolution.stages.foldLeft(Try(PackageSignatures.implicitProviders))(
        (acc, stage) => acc.flatMap(stage.bind)
      )
    } yield providers.valuesIterator.map(_.iface).filterNot(_.name.isPredef).toList.sortBy(_.name)

  private def read(bundles: (String, proto.Interfaces)*): Try[List[Package.Interface]] =
    decodeRequired(bundles.toList, List(client -> List(facade)))

  test("external types, constructors and aliases use their owning provider") {
    val front = signature(facadeIface)
    val entry = front.interfaces.head
    assertEquals(entry.definedTypes.size, 0)
    assertEquals(entry.typeAliases.size, 0)
    assertEquals(entry.getSignatureDependencies.exposes.toList, List("Owner"))
    assert(read("front.sig" -> front).failed.get.getMessage.contains("Client -> Facade -> Owner"))

    val original = signature(ownerIface)
    val result = success(read("owner.sig" -> original, "front.sig" -> front))
    assertEquals(result, List(facadeIface, ownerIface).map(PackageSignatures.canonical))
    val ownerType = result.last.exports.flatMap(_.tag.definedType).head
    assertEquals(ownerType.annotatedTypeParams, flag.annotatedTypeParams)
    assertEquals(ownerType.constructors, List(zero, more))
    assertEquals(ownerType.dataRepr(zero.name), DataRepr.Enum(0, 0, List(0, 1)))

    // Changing the actual owner changes the decoded facts even when the
    // re-exporting package's bytes stay the same. Removing an export fails.
    val changedAlias = alias.copy(rhs = Type.IntType)
    val changed = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(_)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(changedAlias))
      case exp => exp
    })
    val rebound = success(read("front.sig" -> front, "owner.sig" -> signature(changed)))
    assertEquals(rebound.head.exports.flatMap(_.tag.typeAlias), List(changedAlias))
    val incomplete = ownerIface.copy(exports = ownerIface.exports.filter(_.tag.typeAlias.isEmpty))
    assert(read("owner.sig" -> signature(incomplete), "front.sig" -> front).isFailure)
    val partial = ownerIface.copy(exports = ownerIface.exports.filterNot(_.name == zero.name))
    val hidden = read("owner.sig" -> signature(partial), "front.sig" -> front)
    assert(hidden.failed.get.getMessage.contains("missing exported constructor Owner::Zero"))
  }

  test("whole-file signature decoding rejects a foreign re-export of a hidden constructor") {
    val partial = ownerIface.copy(exports = ownerIface.exports.filterNot(_.name == zero.name))
    val bundle = signature(partial, facadeIface)
    val result = ProtoConverter.interfacesFromProto(bundle)
    assert(result.isFailure, "the whole-file reader exposed Owner::Zero")
    assert(result.failed.get.getMessage.contains("Owner::Zero"))
  }

  test("constructor default-helper types participate in exposure checking") {
    val field = ConstructorParam(
      Name("field"), Type.IntType, Some(Name("default_field")), Some(alias.rhs)
    )
    val ctor = ConstructorFn(Constructor("Wrapped"), List(field), Nil)
    val dt = DefinedType(facade, TypeName(ctor.name), Nil, List(ctor))
    val wrapper = interface(facade, List(
      ExportedName.TypeName(ctor.name, Referant.DefinedT(dt)),
      ExportedName.Constructor(ctor.name, Referant.Constructor(dt, ctor))
    ))
    val bundle = signature(wrapper)
    assertEquals(bundle.interfaces.head.getSignatureDependencies.exposes.toList, List("Owner"))
    assert(read("wrapper" -> bundle).isFailure)
    val decoded = success(read("wrapper" -> bundle, "owner" -> signature(ownerIface)))
    assertEquals(decoded.head, PackageSignatures.canonical(wrapper))
  }

  test("strict interface attribution counts the original file once for all selected roots") {
    val packed = PackageSignatures.Input("packed.sig", Some("//:packed"))
    val unused = PackageSignatures.Input("extra.sig", Some("//:extra"))
    val sibling = PackageName.parts("Sibling")
    val local = PackageName.parts("Local")
    val bundles = List(
      packed -> signature(facadeIface, ownerIface, interface(sibling, Nil)),
      unused -> signature(interface(local, Nil), PackageSignatures.predef)
    )
    val sources = List(client -> List(local, facade, PackageName.PredefName), local -> Nil)
    val selected = success(PackageSignatures.plan(bundles, sources, builtinPredef = true))
    assertEquals(selected.origins, Map(facade -> packed, owner -> packed))
    assertEquals(success(selected.checkUsed(List(packed))), ())
    assertEquals(selected.checkUsed(List(packed, unused)).failed.get.getMessage,
      "unused direct interfaces: extra.sig (//:extra)")
    val reversed = success(PackageSignatures.plan(bundles.reverse, sources.reverse, builtinPredef = true))
    assertEquals(reversed.origins, selected.origins)
  }

  test("strict interface checks use path identity even when diagnostic names coincide") {
    final case class FileId(id: Int)
    given Show[FileId] = Show.show(_ => "packages.sig")
    val selected = PackageSignatures.Input(FileId(1), Some("//:group"))
    val other = PackageSignatures.Input(FileId(2), Some("//:group"))
    val resolution = success(PackageSignatures.plan(
      List(selected -> signature(ownerIface), other -> signature()),
      List(client -> List(owner)), builtinPredef = true
    ))
    assertEquals(resolution.origins, Map(owner -> selected))
    assertEquals(resolution.checkUsed(List(selected, other)).failed.get.getMessage,
      "unused direct interfaces: packages.sig (//:group)")
  }

  test("unindexed signatures recover exposures only for selected roots") {
    val missingType = Type.const(PackageName.parts("Missing"), TypeName(Constructor("T")))
    val unused = interface(PackageName.parts("Sibling"),
      List(ExportedName.Binding(Name("unused"), Referant.Value(missingType))))
    val old = success(ProtoConverter.interfacesToProto(List(facadeIface, unused)))
    val owners = success(ProtoConverter.interfacesToProto(List(ownerIface)))
    assert(old.interfaces.forall(_.signatureDependencies.isEmpty))
    val selected = success(read("old.sig" -> old, "owner.sig" -> owners))
    assertEquals(selected.map(_.name), List(facade, owner))
  }

  test("embedded foreign definitions in a legacy signature are rebound to the declared owner") {
    val old = success(ProtoConverter.interfaceToProto(ownerIface))
    val embedded = old.copy(strings = old.strings :+ facade.asString, packageName = old.strings.size + 1)
    val bundle = proto.Interfaces(List(embedded))
    assert(read("legacy" -> bundle).failed.get.getMessage.contains("Client -> Facade -> Owner"))

    val authoritative = alias.copy(rhs = Type.StrType)
    val updated = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(_)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(authoritative))
      case exp => exp
    })
    val roots = success(read("legacy" -> bundle, "owner" -> signature(updated)))
    assertEquals(roots.head.exports.flatMap(_.tag.typeAlias), List(authoritative))
  }

  test("an explicitly supplied Predef can expose an independent type owner") {
    val publicType = flag.toOpaque.copy(annotatedTypeParams = Nil)
    val owned = interface(owner, List(ExportedName.TypeName(publicType.name.ident, Referant.DefinedT(publicType))))
    val predef = interface(PackageName.PredefName, List(
      ExportedName.Binding(Name("provided"), Referant.Value(publicType.toTypeTyConst))
    ))
    val resolution = success(PackageSignatures.plan(
      List(PackageSignatures.Input("custom.sig", None) -> signature(predef, owned)), List(client -> Nil), builtinPredef = false
    ))
    val restored = resolution.stages.foldLeft(Map.empty[PackageName, PackageSignatures.PublicInterface])(
      (providers, stage) => success(stage.bind(providers))
    )
    assertEquals(restored(PackageName.PredefName).iface, predef)
    assertEquals(restored(owner).iface, owned)
  }

  test("the compiler cache fingerprints resolved foreign definitions supplied by typed callers") {
    val changed = facadeIface.copy(exports = facadeIface.exports.map {
      case ExportedName.TypeName(n, Referant.TypeAliasT(ta)) =>
        ExportedName.TypeName(n, Referant.TypeAliasT(ta.copy(rhs = Type.StrType)))
      case e => e
    })
    // The distributable signature deliberately records only the symbolic owner.
    assertEquals(signature(facadeIface), signature(changed))
    assertNotEquals(success(CompileCache.interfaceHash(facadeIface)), success(CompileCache.interfaceHash(changed)))
    val conflicting = facadeIface.copy(exports = ExportedName.TypeName(
      Constructor("OtherAlias"), Referant.TypeAliasT(alias.copy(rhs = Type.StrType))
    ) :: facadeIface.exports)
    assert(CompileCache.interfaceHash(conflicting).failed.get.getMessage.contains("conflicting definition"))
  }

  test("public changes affect bytes and constructor tag order survives export reordering") {
    val normal = signature(ownerIface, facadeIface)
    val reversedExports = ownerIface.copy(exports = ownerIface.exports.reverse)
    assertEquals(signature(facadeIface, reversedExports), normal)

    val reordered = flag.copy(constructors = flag.constructors.reverse)
    val changed = ownerIface.copy(exports = ownerIface.exports.map {
      case ExportedName.TypeName(n, Referant.DefinedT(_)) =>
        ExportedName.TypeName(n, Referant.DefinedT(reordered))
      case ExportedName.Constructor(n, Referant.Constructor(_, cf)) =>
        ExportedName.Constructor(n, Referant.Constructor(reordered, cf))
      case e => e
    })
    assertNotEquals(signature(changed), signature(ownerIface))
  }

  test("missing type definitions, forged local pointers and stale indexes fail") {
    val value = interface(facade, List(ExportedName.Binding(Name("flags"), Referant.Value(alias.rhs))))
    val single = signature(value)
    val wrong = read("value.sig" -> single, "owner.sig" -> signature(interface(owner, Nil)))
    assert(wrong.failed.get.getMessage.contains("missing exported type"))
    val stale = single.copy(interfaces = single.interfaces.map(
      _.withSignatureDependencies(proto.InterfaceDependencies())
    ))
    assert(read("stale.sig" -> stale).failed.get.getMessage.contains("incorrect exposure index"))
    val own = signature(ownerIface).interfaces.head
    val forged = own.copy(
      strings = own.strings :+ facade.asString,
      packageName = own.strings.size + 1
    )
    val impersonation = read("forged.sig" -> proto.Interfaces(List(forged)))
    assert(impersonation.failed.get.getMessage.contains("definition owner"))
    val ambiguous = read("a.sig" -> single, "b.sig" -> single)
    assert(ambiguous.failed.get.getMessage.contains("a.sig, b.sig"))
  }

  test("a private constructor field type does not mask a stale kind in a sibling argument") {
    val hidden = Type.const(facade, TypeName(Constructor("Hidden")))
    def wrapper(result: Type): proto.Interfaces = {
      val field = ConstructorParam(Name("apply"), Type.Fun(NonEmptyList.one(hidden), result), None)
      val ctor = ConstructorFn(Constructor("Wrap"), field :: Nil, Nil)
      val dt = DefinedType(facade, TypeName(ctor.name), Nil, ctor :: Nil)
      signature(interface(facade, List(
        ExportedName.TypeName(ctor.name, Referant.DefinedT(dt)),
        ExportedName.Constructor(ctor.name, Referant.Constructor(dt, ctor))
      )))
    }
    val owningSignature = signature(ownerIface)
    assert(read("wrap" -> wrapper(alias.rhs), "owner" -> owningSignature).isSuccess)
    assert(read("wrap" -> wrapper(flag.toTypeTyConst), "owner" -> owningSignature).isFailure)
  }

  test("checking the kind of a deeply nested public value is stack safe") {
    val depth = 6000
    // Build the table in wire order so the fixture does not recursively encode
    // the nested type while testing the reader.
    val leaves = Vector(
      proto.Type().withTypeConst(proto.TypeConst(2, 3)),
      proto.Type().withTypeConst(proto.TypeConst(2, 4))
    )
    val applications = Vector.tabulate(depth)(i =>
      proto.Type().withTypeApply(proto.TypeApply(left = 1, right = 2 + i)))
    val entry = proto.Interface(
      strings = Vector("Facade", "Bosatsu/Predef", "List", "Int", "deep"),
      packageName = 1,
      types = leaves ++ applications,
      exports = List(proto.ExportedName(name = 5, referant = Some(proto.Referant().withValue(2 + depth)))),
      signatureDependencies = Some(proto.InterfaceDependencies())
    )
    val result =
      try read("deep.sig" -> proto.Interfaces(List(entry)))
      catch { case _: StackOverflowError => fail("stack overflow validating nested List applications") }
    assertEquals(success(result).map(_.name), List(facade))
  }

  test("whole-file and selected readers handle a deep legacy re-export chain in either order") {
    val depth = 3000
    val own = TypeName(Constructor("Own"))
    def dt(i: Int) = DefinedType(PackageName.parts(f"P$i%04d"), own, Nil, Nil)
    val entries = (0 until depth).toList.map { i =>
      val current = dt(i)
      val local = ExportedName.TypeName(own.ident, Referant.DefinedT(current))
      val next = Option.when(i + 1 < depth)(
        ExportedName.TypeName(Constructor("Next"), Referant.DefinedT(dt(i + 1)))
      )
      success(ProtoConverter.interfaceToProto(interface(current.packageName, local :: next.toList)))
    }
    for (ordered <- List(entries, entries.reverse)) {
      val chain = proto.Interfaces(ordered)
      val whole = success(ProtoConverter.interfacesFromProto(chain))
      val selected = success(decodeRequired(List("chain" -> chain), List(client -> List(dt(0).packageName))))
      assertEquals(selected, whole.map(PackageSignatures.canonical).sortBy(_.name))
      assertEquals(selected.size, depth)
    }
  }

  test("index traversal is stack safe and diagnoses exposure cycles") {
    val depth = 3000
    def name(i: Int): PackageName = PackageName.parts(s"P$i")
    def root(i: Int, next: Option[PackageName]): Package.Interface = {
      val pn = name(i)
      val dt = DefinedType(pn, TypeName(Constructor("T")), Nil, Nil)
      val typ = ExportedName.TypeName(dt.name.ident, Referant.DefinedT(dt))
      val binding = next.toList.map { dep =>
        ExportedName.Binding(Name("next"), Referant.Value(Type.const(dep, dt.name)))
      }
      interface(pn, typ :: binding)
    }
    val roots = (0 until depth).toList.map { i =>
      root(i, Option.when(i + 1 < depth)(name(i + 1)))
    }
    def decode(bundle: proto.Interfaces) =
      decodeRequired(List("chain" -> bundle), List(client -> List(name(0))))
    assertEquals(success(decode(success(PackageSignatures.encode(roots)))).size, depth)
    val cycle = signature(root(0, Some(name(1))), root(1, Some(name(0))))
    assert(decode(cycle).failed.get.getMessage.contains("Client -> P0 -> P1 -> P0"))
  }

  test("build requirements preserve legacy and scoped forms and reject contradictions") {
    val digest = "a" * 64
    val legacy = proto.BuildRequirements(interfaces = Seq(
      proto.InterfaceRequirement("Owner", digest)
    ))
    success(PackageSignatures.parseBuildRequirements(legacy)) match {
      case PackageSignatures.ParsedBuildRequirements.Legacy(interfaces) =>
        assertEquals(interfaces, List(PackageSignatures.InterfaceDigest(owner, digest)))
      case other => fail(s"expected legacy requirements, found $other")
    }

    val emptyScoped = proto.BuildRequirements(
      scopedVersion = 1,
      members = Seq(proto.MemberBuildRequirements(memberName = "Client"))
    )
    success(PackageSignatures.parseBuildRequirements(emptyScoped)) match {
      case PackageSignatures.ParsedBuildRequirements.Scoped(members) =>
        assertEquals(members.map(_.member), List(client))
        assertEquals(members.head.bindings, Nil)
        assertEquals(members.head.interfaces, Nil)
      case other => fail(s"expected scoped requirements, found $other")
    }

    val sharedApi = PackageName.parts("Shared/Api")
    def scopedMember(name: String, version: Long): proto.MemberBuildRequirements =
      proto.MemberBuildRequirements(
        memberName = name,
        bindings = Seq(proto.ScopedBindingRequirement(
          referrer = Some(proto.BuildScope(proto.BuildScope.Value.Member(name))),
          packageName = sharedApi.asString,
          owner = Some(proto.BuildScope(
            proto.BuildScope.Value.Library(proto.LibIdentity("shared", Some(proto.Version(major = version))))
          ))
        )),
        interfaces = Seq(proto.InterfaceRequirement(sharedApi.asString, digest))
      )
    val disjointOwners = proto.BuildRequirements(
      scopedVersion = 1,
      members = Seq(scopedMember("Left", 1), scopedMember("Right", 2))
    )
    success(PackageSignatures.parseBuildRequirements(disjointOwners)) match {
      case PackageSignatures.ParsedBuildRequirements.Scoped(members) =>
        assertEquals(members.map(_.member), List(PackageName.parts("Left"), PackageName.parts("Right")))
        assertEquals(members.map(_.bindings.head.owner.scope), List(
          PackageBindings.Scope.Library(libraryIdentity("shared", "1.0.0")),
          PackageBindings.Scope.Library(libraryIdentity("shared", "2.0.0"))
        ))
      case other => fail(s"expected scoped requirements, found $other")
    }

    val duplicateMember = emptyScoped.copy(members = Seq(
      proto.MemberBuildRequirements(memberName = "Client"),
      proto.MemberBuildRequirements(memberName = "Client")
    ))
    assert(PackageSignatures.parseBuildRequirements(duplicateMember).failed.get.getMessage
      .contains("duplicate member"))
    val contradictoryOwners = emptyScoped.copy(members = Seq(
      scopedMember("Client", 1).copy(bindings = Seq(
        scopedMember("Client", 1).bindings.head,
        scopedMember("Client", 2).bindings.head.copy(
          referrer = Some(proto.BuildScope(
            proto.BuildScope.Value.Library(proto.LibIdentity("facade", Some(proto.Version(major = 1))))
          ))
        )
      ))
    ))
    assert(PackageSignatures.parseBuildRequirements(contradictoryOwners).failed.get.getMessage
      .contains("contradictory owners"))
    val unsupportedVersion = emptyScoped.copy(scopedVersion = 2)
    assert(PackageSignatures.parseBuildRequirements(unsupportedVersion).failed.get.getMessage
      .contains("unsupported scoped"))
    val mixedLegacyAndScoped = emptyScoped.copy(interfaces = Seq(
      proto.InterfaceRequirement("Owner", digest)
    ))
    assert(PackageSignatures.parseBuildRequirements(mixedLegacyAndScoped).failed.get.getMessage
      .contains("cannot contain legacy"))
    val invalidHash = legacy.copy(interfaces = Seq(proto.InterfaceRequirement("Owner", "not-a-hash")))
    assert(PackageSignatures.parseBuildRequirements(invalidHash).failed.get.getMessage
      .contains("invalid BLAKE3"))
    val invalidLibraryVersion = proto.BuildRequirements(
      scopedVersion = 1,
      members = Seq(proto.MemberBuildRequirements(
        memberName = "Client",
        bindings = Seq(proto.ScopedBindingRequirement(
          referrer = Some(proto.BuildScope(proto.BuildScope.Value.Member("Client"))),
          packageName = "Owner",
          owner = Some(proto.BuildScope(
            proto.BuildScope.Value.Library(proto.LibIdentity("owner", Some(proto.Version(major = -1))))
          ))
        )),
        interfaces = Seq(proto.InterfaceRequirement("Owner", digest))
      ))
    )
    assert(PackageSignatures.parseBuildRequirements(invalidLibraryVersion).failed.get.getMessage
      .contains("invalid library version"))

    def withOwner(owner: proto.BuildScope): proto.BuildRequirements =
      invalidLibraryVersion.copy(members = invalidLibraryVersion.members.map(member =>
        member.copy(bindings = member.bindings.map(_.copy(owner = Some(owner))))
      ))
    val unnamedLibrary = withOwner(proto.BuildScope(
      proto.BuildScope.Value.Library(proto.LibIdentity("", Some(proto.Version(major = 1))))
    ))
    assert(PackageSignatures.parseBuildRequirements(unnamedLibrary).failed.get.getMessage
      .contains("missing library name"))
    val emptyScope = withOwner(proto.BuildScope())
    assert(PackageSignatures.parseBuildRequirements(emptyScope).failed.get.getMessage
      .contains("missing requirement scope"))
  }

  test("scoped requirements round trip through the shared encoder") {
    import PackageBindings.Scope
    val shared = LibraryName("shared") -> Version(1, 2, 3)
    val client = PackageName.parts("Client")
    val facade = PackageName.parts("Facade")
    val types = PackageName.parts("Local", "Types")
    val requirements = List(PackageSignatures.MemberRequirements(
      client,
      List(
        PackageSignatures.RequirementBinding(Scope.Member(client), facade, Scope.Library(shared)),
        PackageSignatures.RequirementBinding(Scope.Library(shared), types, Scope.Local(types))
      ),
      List(
        PackageSignatures.InterfaceDigest(facade, "c" * 64),
        PackageSignatures.InterfaceDigest(types, "d" * 64)
      )
    ))
    val encoded = PackageSignatures.scopedRequirementsToProto(requirements)
    assertEquals(
      encoded.members.flatMap(_.bindings).map(_.owner.map(_.value)),
      Seq(
        Some(proto.BuildScope.Value.Library(proto.LibIdentity("shared", Some(Version(1, 2, 3).toProto)))),
        Some(proto.BuildScope.Value.LocalSignature("Local/Types"))
      )
    )
    PackageSignatures.parseBuildRequirements(encoded).get match {
      case PackageSignatures.ParsedBuildRequirements.Scoped(decoded) =>
        assertEquals(decoded, requirements)
      case other => fail(s"expected scoped requirements, found $other")
    }
  }

  test("build requirements reject records that cannot be one member's resolved closure") {
    val digest = "b" * 64
    def memberScope(name: String) = proto.BuildScope(proto.BuildScope.Value.Member(name))
    def libraryScope(name: String, major: Long) = proto.BuildScope(
      proto.BuildScope.Value.Library(proto.LibIdentity(name, Some(proto.Version(major = major))))
    )
    def binding(referrer: proto.BuildScope, pkg: String, owner: proto.BuildScope) =
      proto.ScopedBindingRequirement(
        referrer = Some(referrer),
        packageName = pkg,
        owner = Some(owner)
      )
    def scoped(members: proto.MemberBuildRequirements*) =
      proto.BuildRequirements(scopedVersion = 1, members = members)
    def failure(requirements: proto.BuildRequirements): String =
      PackageSignatures.parseBuildRequirements(requirements).failed.get.getMessage

    // Different package names cannot hide two releases of one library.
    val twoReleases = scoped(proto.MemberBuildRequirements(
      memberName = "Client",
      bindings = Seq(
        binding(memberScope("Client"), "Shared/One", libraryScope("shared", 1)),
        binding(memberScope("Client"), "Shared/Two", libraryScope("shared", 2))
      ),
      interfaces = Seq(
        proto.InterfaceRequirement("Shared/One", digest),
        proto.InterfaceRequirement("Shared/Two", digest)
      )
    ))
    assert(failure(twoReleases).contains("conflicting visible releases for shared in Client"),
      failure(twoReleases))

    // A bundle member's package cannot also be owned by a library.
    val libraryClaimsMember = scoped(
      proto.MemberBuildRequirements(
        memberName = "Client",
        bindings = Seq(binding(memberScope("Client"), "Owner", libraryScope("owner", 1))),
        interfaces = Seq(proto.InterfaceRequirement("Owner", digest))
      ),
      proto.MemberBuildRequirements(memberName = "Owner")
    )
    assert(failure(libraryClaimsMember).contains("library owner of Owner conflicts with bundle member"),
      failure(libraryClaimsMember))

    // A library referrer must be a scope this member's closure selected.
    val facadeExposure = binding(libraryScope("facade", 1), "Shared/Api", libraryScope("shared", 1))
    val unjustified = scoped(proto.MemberBuildRequirements(
      memberName = "Client",
      bindings = Seq(facadeExposure),
      interfaces = Seq(proto.InterfaceRequirement("Shared/Api", digest))
    ))
    assert(failure(unjustified).contains("unjustified binding referrer library facade@1.0.0 in Client"),
      failure(unjustified))
    val justified = scoped(proto.MemberBuildRequirements(
      memberName = "Client",
      bindings = Seq(
        binding(memberScope("Client"), "Facade", libraryScope("facade", 1)),
        facadeExposure
      ),
      interfaces = Seq(
        proto.InterfaceRequirement("Facade", digest),
        proto.InterfaceRequirement("Shared/Api", digest)
      )
    ))
    assert(PackageSignatures.parseBuildRequirements(justified).isSuccess)

    // Bindings that only justify each other are not reachable from the member.
    val cyclic = scoped(proto.MemberBuildRequirements(
      memberName = "Client",
      bindings = Seq(
        binding(libraryScope("facade", 1), "Facade/One", libraryScope("facade", 1)),
        binding(libraryScope("facade", 1), "Facade/Two", libraryScope("facade", 1))
      ),
      interfaces = Seq(
        proto.InterfaceRequirement("Facade/One", digest),
        proto.InterfaceRequirement("Facade/Two", digest)
      )
    ))
    assert(failure(cyclic).contains("unjustified binding referrer library facade@1.0.0 in Client"),
      failure(cyclic))

    // Another member may refer only through a source owner it selected.
    val wrapperExposure = binding(memberScope("Wrapper"), "Shared/Api", libraryScope("shared", 1))
    val unjustifiedMember = scoped(
      proto.MemberBuildRequirements(
        memberName = "Client",
        bindings = Seq(wrapperExposure),
        interfaces = Seq(proto.InterfaceRequirement("Shared/Api", digest))
      ),
      proto.MemberBuildRequirements(memberName = "Wrapper")
    )
    assert(failure(unjustifiedMember).contains("unjustified binding referrer member Wrapper in Client"),
      failure(unjustifiedMember))
    val justifiedMember = scoped(
      proto.MemberBuildRequirements(
        memberName = "Client",
        bindings = Seq(binding(memberScope("Client"), "Wrapper", memberScope("Wrapper")), wrapperExposure),
        interfaces = Seq(proto.InterfaceRequirement("Shared/Api", digest))
      ),
      proto.MemberBuildRequirements(memberName = "Wrapper")
    )
    assert(PackageSignatures.parseBuildRequirements(justifiedMember).isSuccess)
  }
}
