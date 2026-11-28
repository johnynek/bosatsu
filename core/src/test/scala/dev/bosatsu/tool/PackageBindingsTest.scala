package dev.bosatsu.tool

import cats.data.Chain
import dev.bosatsu.{Bytes, Json, MemoryMain, PackageName}
import dev.bosatsu.library.{LibraryIdentity, Name, Version}
import scala.collection.immutable.SortedMap

class PackageBindingsTest extends munit.FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val platform = MemoryMain[ErrorOr].platformIO
  private val libOne = Name("shared") -> version("1.0.0")
  private val libTwo = Name("shared") -> version("2.0.0")
  private val libApi = Name("api") -> version("1.0.0")

  private def version(text: String): Version =
    Version.parser.parseAll(text).toOption.get

  private def packageName(text: String): PackageName =
    PackageName.parse(text).get

  private def path(raw: String): Path =
    Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private def identityJson(identity: LibraryIdentity): Json =
    Json.JObject(List(
      "name" -> Json.JString(identity._1.name),
      "version" -> Json.JString(identity._2.render)
    ))

  private def scopeJson(scope: PackageBindings.Scope): Json = scope match {
    case PackageBindings.Scope.Member(member) => Json.JObject(List(
      "kind" -> Json.JString("member"),
      "package" -> Json.JString(member.asString)
    ))
    case PackageBindings.Scope.Local(name) => Json.JObject(List(
      "kind" -> Json.JString("local-signature"),
      "package" -> Json.JString(name.asString)
    ))
    case PackageBindings.Scope.Library(identity) => Json.JObject(List(
      "kind" -> Json.JString("library"),
      "identity" -> identityJson(identity)
    ))
    case PackageBindings.Scope.Predef => Json.JObject(List("kind" -> Json.JString("predef")))
  }

  private def publicIndex(
      entries: List[(LibraryIdentity, String, List[(String, LibraryIdentity, List[LibraryIdentity])])]
  ): PackageBindings.PublicIndex =
    PackageBindings.indexFromPackages(entries.map { case (identity, name, exposures) =>
      val resolvedExposures = SortedMap.from(exposures.map { case (packageText, provider, witness) =>
        packageName(packageText) -> PackageBindings.PublicProvider.Library(provider, witness)
      })(using summon[Ordering[PackageName]])
      PackageBindings.PublicPackage(identity, packageName(name), resolvedExposures)
    }, identities = Nil)

  private def apiIndex: PackageBindings.PublicIndex =
    PackageBindings.indexFromPackages(List(
      PackageBindings.PublicPackage(
        libApi,
        packageName("Api"),
        SortedMap.from(List(
          packageName("Types") -> PackageBindings.PublicProvider.Library(libOne, List(libApi, libOne))
        ))(using summon[Ordering[PackageName]])
      ),
      PackageBindings.PublicPackage(
        libOne,
        packageName("Types"),
        SortedMap.empty(using summon[Ordering[PackageName]])
      )
    ), identities = Nil)

  private def importJson(
      identity: LibraryIdentity,
      signature: String
  ): Json = Json.JObject(List(
    "identity" -> identityJson(identity),
    "signature_path" -> Json.JString(signature),
    "exposures_path" -> Json.JString("public-index.json")
  ))

  private def dependencyJson(
      from: PackageBindings.Scope,
      packageText: String,
      to: PackageBindings.Scope
  ): Json = Json.JObject(List(
    "from" -> scopeJson(from),
    "package" -> Json.JString(packageText),
    "to" -> scopeJson(to)
  ))

  private def bindingJson(
      referrer: PackageBindings.Scope,
      packageText: String,
      provider: PackageBindings.Scope,
      interface: Option[String]
  ): Json = Json.JObject(List(
    "referrer" -> scopeJson(referrer),
    "package" -> Json.JString(packageText),
    "provider" -> scopeJson(provider),
    "interface_path" -> interface.fold[Json](Json.JNull)(Json.JString(_))
  ))

  private def scopeMapJson(
      members: List[String],
      dependencies: List[Json],
      imports: List[Json],
      bindings: List[Json]
  ): Json = Json.JObject(List(
    "schema_version" -> Json.JNumberStr("1"),
    "members" -> Json.JArray(members.map(Json.JString(_)).toVector),
    "dependencies" -> Json.JArray(dependencies.toVector),
    "imports" -> Json.JArray(imports.toVector),
    "bindings" -> Json.JArray(bindings.toVector)
  ))

  private def state(scopeMap: Json, index: PackageBindings.PublicIndex): MemoryMain.State = {
    val files = List(
      "scope-map.json" -> scopeMap.render,
      "public-index.json" -> index.toJson.render
    )
    files.foldLeft(MemoryMain.State.empty) { case (current, (file, contents)) =>
      current.withFile(
        path(file),
        MemoryMain.FileContent.Bytes(Bytes.fromUTF8String(contents))
      ).get
    }
  }

  private def read(
      scopeMap: Json,
      index: PackageBindings.PublicIndex,
      directInterfaces: List[String]
  ): ErrorOr[PackageBindings.ScopeMap[Path]] =
    PackageBindings.read(platform, path("scope-map.json"), directInterfaces.map(path))
      .runA(state(scopeMap, index))

  private val app = PackageBindings.Scope.Member(packageName("App"))
  private val apiScope = PackageBindings.Scope.Library(libApi)
  private val typesScope = PackageBindings.Scope.Library(libOne)

  private def apiScopeMap(
      importOrder: List[Int],
      bindingOrder: List[Int]
  ): Json = {
    val dependencies = List(dependencyJson(app, "Api", apiScope))
    val imports = List(importJson(libApi, "api.sig"), importJson(libOne, "types.sig"))
    val bindings = List(
      bindingJson(app, "Api", apiScope, Some("api.sig")),
      bindingJson(app, "Types", typesScope, Some("types.sig")),
      bindingJson(apiScope, "Types", typesScope, Some("types.sig"))
    )
    scopeMapJson(
      List("App"),
      dependencies,
      importOrder.map(imports(_)),
      bindingOrder.map(bindings(_))
    )
  }

  test("inspection records drive checked bindings and witness paths") {
    val index = apiIndex
    val parsed = read(apiScopeMap(importOrder = List(0, 1), bindingOrder = List(0, 1, 2)), index, List("api.sig", "types.sig")).toOption.get
    val checked = PackageBindings.validateAgainstInspection(parsed, index, parsed.dependencies, memberExposures = Map.empty, localInterfaces = Nil)
      .fold(error => fail(error), identity)

    val apiKey = PackageBindings.BindingKey(app, packageName("Api"))
    val typesKey = PackageBindings.BindingKey(app, packageName("Types"))
    val importedTypesKey = PackageBindings.BindingKey(apiScope, packageName("Types"))
    assertEquals(parsed.publicIndex.toJson.render, index.toJson.render)
    assertEquals(checked.bindings(apiKey).owner.scope, apiScope)
    assertEquals(checked.bindings(typesKey).owner.scope, typesScope)
    assertEquals(checked.bindings(importedTypesKey).owner.scope, typesScope)
    assertEquals(
      checked.bindings(typesKey).witnessPath,
      List(app, apiScope, typesScope)
    )
    assertNotEquals(parsed.imports(libApi).signature.path, parsed.imports(libOne).signature.path)

    val mismatch = PackageBindings.validateAgainstInspection(parsed, index, Nil, memberExposures = Map.empty, localInterfaces = Nil)
      .left.toOption.get
    assert(mismatch.contains("do not match source-derived dependencies"), mismatch)
  }

  test("declaration order does not affect canonical scope bindings") {
    val index = apiIndex
    val direct = List("api.sig", "types.sig")
    val baseline = {
      val parsed = read(apiScopeMap(importOrder = List(0, 1), bindingOrder = List(0, 1, 2)), index, direct).toOption.get
      PackageBindings.validateAgainstInspection(parsed, index, parsed.dependencies, memberExposures = Map.empty, localInterfaces = Nil)
        .toOption.get.bindings.toList
    }
    val importsOrders = List(List(0, 1), List(1, 0))
    val bindingOrders = List(0, 1, 2).permutations.toList

    importsOrders.foreach { importOrder =>
      bindingOrders.foreach { bindingOrder =>
        val parsed = read(apiScopeMap(importOrder, bindingOrder), index, direct).toOption.get
        val checked = PackageBindings.validateAgainstInspection(
          parsed,
          index,
          parsed.dependencies,
          memberExposures = Map.empty,
          localInterfaces = Nil
        ).toOption.get
        assertEquals(checked.bindings.toList, baseline)
      }
    }
  }

  test("same package from two exact releases remains legal for disjoint members") {
    val packageText = "Shared/Api"
    val index = publicIndex(List(
      (libOne, packageText, Nil),
      (libTwo, packageText, Nil)
    ))
    val first = PackageBindings.Scope.Member(packageName("Left"))
    val second = PackageBindings.Scope.Member(packageName("Right"))
    val scopeMap = scopeMapJson(
      List("Left", "Right"),
      List(
        dependencyJson(first, packageText, PackageBindings.Scope.Library(libOne)),
        dependencyJson(second, packageText, PackageBindings.Scope.Library(libTwo))
      ),
      List(importJson(libOne, "shared-v1.sig"), importJson(libTwo, "shared-v2.sig")),
      List(
        bindingJson(first, packageText, PackageBindings.Scope.Library(libOne), Some("shared-v1.sig")),
        bindingJson(second, packageText, PackageBindings.Scope.Library(libTwo), Some("shared-v2.sig"))
      )
    )
    val parsed = read(scopeMap, index, List("shared-v1.sig", "shared-v2.sig")).toOption.get
    val checked = PackageBindings.validateAgainstInspection(parsed, index, parsed.dependencies, memberExposures = Map.empty, localInterfaces = Nil)
      .fold(error => fail(error), identity)

    assertEquals(checked.bindings.size, 2)
    assertEquals(
      checked.bindings(PackageBindings.BindingKey(first, packageName(packageText))).owner.scope,
      PackageBindings.Scope.Library(libOne)
    )
    assertEquals(
      checked.bindings(PackageBindings.BindingKey(second, packageName(packageText))).owner.scope,
      PackageBindings.Scope.Library(libTwo)
    )
  }

  test("an imported scope with no exported packages is checked against inspection identities") {
    val scopeMap = scopeMapJson(
      List("App"),
      Nil,
      List(importJson(libOne, "empty.sig")),
      Nil
    )
    val parsed = read(scopeMap, PackageBindings.indexFromPackages(Nil, identities = Nil), List("empty.sig"))
      .toOption.get
    val inspected = PackageBindings.indexFromPackages(Nil, List(libOne))
    val checked = PackageBindings.validateAgainstInspection(parsed, inspected, Nil, memberExposures = Map.empty, localInterfaces = Nil)
      .fold(error => fail(error), identity)

    assert(checked.publicIndex.identities.contains(libOne))
    val missing = PackageBindings.validateAgainstInspection(
      parsed,
      PackageBindings.indexFromPackages(Nil, identities = Nil),
      Nil,
      memberExposures = Map.empty,
      localInterfaces = Nil
    ).left.toOption.get
    assert(missing.contains("imported scope is missing"), missing)
  }

  test("local source members can provide package bindings without an interface file") {
    val consumer = PackageBindings.Scope.Member(packageName("Consumer"))
    val provider = PackageBindings.Scope.Member(packageName("Provider"))
    val scopeMap = scopeMapJson(
      List("Consumer", "Provider"),
      List(dependencyJson(consumer, "Provider", provider)),
      Nil,
      List(bindingJson(consumer, "Provider", provider, None))
    )
    val parsed = read(scopeMap, PackageBindings.indexFromPackages(Nil, identities = Nil), Nil).toOption.get
    val checked = PackageBindings.validateAgainstInspection(
      parsed,
      PackageBindings.indexFromPackages(Nil, identities = Nil),
      parsed.dependencies,
      memberExposures = Map.empty,
      localInterfaces = Nil
    ).fold(error => fail(error), identity)
    val key = PackageBindings.BindingKey(consumer, packageName("Provider"))

    assertEquals(checked.bindings(key).owner.scope, provider)
    assertEquals(checked.bindings(key).interface, None)
  }

  test("closure resolution requires declared roots even when the binding list is empty") {
    val packageApi = packageName("Api")
    val scopeMap = scopeMapJson(
      List("App"),
      List(dependencyJson(app, "Api", apiScope)),
      List(importJson(libApi, "api.sig"), importJson(libOne, "types.sig")),
      Nil
    )
    val parsed = read(scopeMap, apiIndex, List("api.sig", "types.sig")).toOption.get
    val checked = PackageBindings.validateAgainstInspection(
      parsed, apiIndex, parsed.dependencies,
      memberExposures = Map.empty,
      localInterfaces = Nil
    ).fold(error => fail(error), identity)

    val closure = PackageBindings.resolveMemberClosures(
      List(packageName("App") -> List(packageApi)),
      checked,
      Map(packageName("App") -> Some(Nil)),
      builtinPredef = true,
      localPackages = Set.empty,
      requiredExposures = Map.empty
    )
    assert(closure.left.toOption.get.contains("missing interface binding for Api"))
  }

  test("duplicate keys, unknown scopes, and undeclared interfaces are rejected") {
    val index = apiIndex
    val duplicate = scopeMapJson(
      List("App"),
      List(dependencyJson(app, "Api", apiScope)),
      List(importJson(libApi, "api.sig"), importJson(libOne, "types.sig")),
      List(
        bindingJson(app, "Api", apiScope, Some("api.sig")),
        bindingJson(app, "Api", apiScope, Some("api.sig"))
      )
    )
    val duplicateError = read(duplicate, index, List("api.sig", "types.sig")).left.toOption
      .map(_.getMessage).getOrElse("")
    assert(duplicateError.contains("duplicate binding key"), duplicateError)

    val unknown = scopeMapJson(
      List("App"),
      List(dependencyJson(app, "Api", apiScope)),
      List(importJson(libApi, "api.sig"), importJson(libOne, "types.sig")),
      List(bindingJson(
        PackageBindings.Scope.Member(packageName("Missing")),
        "Api",
        apiScope,
        Some("api.sig")
      ))
    )
    val unknownError = read(unknown, index, List("api.sig", "types.sig")).left.toOption
      .map(_.getMessage).getOrElse("")
    assert(unknownError.contains("unknown binding referrer"), unknownError)

    val missingProvider = PackageBindings.Scope.Member(packageName("Missing"))
    val unknownProvider = scopeMapJson(
      List("App"),
      Nil,
      Nil,
      List(bindingJson(app, "Missing", missingProvider, None))
    )
    val providerError = read(unknownProvider, PackageBindings.indexFromPackages(Nil, identities = Nil), Nil)
      .left.toOption.map(_.getMessage).getOrElse("")
    assert(providerError.contains("unknown binding provider"), providerError)

    val undeclared = read(apiScopeMap(importOrder = List(0, 1), bindingOrder = List(0, 1, 2)), index, List("api.sig")).left.toOption
      .map(_.getMessage).getOrElse("")
    assert(undeclared.contains("not in --direct-interface"), undeclared)
  }

  test("invalid exact library versions are rejected while reading scope maps") {
    val invalidIdentity = Json.JObject(List(
      "name" -> Json.JString("shared"),
      "version" -> Json.JString("not-a-version")
    ))
    val invalidImport = Json.JObject(List(
      "identity" -> invalidIdentity,
      "signature_path" -> Json.JString("shared.sig"),
      "exposures_path" -> Json.JString("public-index.json")
    ))
    val scopeMap = scopeMapJson(List("App"), Nil, List(invalidImport), Nil)
    val error = read(scopeMap, PackageBindings.indexFromPackages(Nil, identities = Nil), List("shared.sig"))
      .left.toOption.get.getMessage

    assert(error.contains("couldn't parse semver"), error)
  }

  test("scope maps reject unknown import and scope fields") {
    val index = apiIndex
    def readError(scopeMap: Json): String =
      read(scopeMap, index, List("api.sig", "types.sig")).left.toOption.get.getMessage
    val labeledImport = Json.JObject(List(
      "identity" -> identityJson(libApi),
      "signature_path" -> Json.JString("api.sig"),
      "exposures_path" -> Json.JString("public-index.json"),
      "label" -> Json.JString("//imports:api")
    ))
    val labeled = scopeMapJson(List("App"), Nil, List(labeledImport), Nil)
    val labelError = readError(labeled)
    assert(labelError.contains("unknown imported scope fields: label"), labelError)

    val oldMember = Json.JObject(List(
      "kind" -> Json.JString("member"), "member" -> Json.JString("App")
    ))
    val oldScope = scopeMapJson(
      List("App"),
      List(Json.JObject(List(
        "from" -> oldMember, "package" -> Json.JString("Api"), "to" -> scopeJson(apiScope)
      ))),
      Nil,
      Nil
    )
    val scopeError = readError(oldScope)
    assert(scopeError.contains("unknown member scope fields: member"), scopeError)
  }

  test("forged provider edges are rejected against the inspected public index") {
    val libThree = Name("other") -> version("1.0.0")
    val forged = publicIndex(List(
      (libApi, "Api", List(("Types", libThree, List(libApi, libThree)))),
      (libOne, "Types", Nil),
      (libThree, "Types", Nil)
    ))
    val scopeMap = scopeMapJson(
      List("App"),
      List(dependencyJson(app, "Api", apiScope)),
      List(
        importJson(libApi, "api.sig"),
        importJson(libOne, "types.sig"),
        importJson(libThree, "other.sig")
      ),
      List(
        bindingJson(app, "Api", apiScope, Some("api.sig")),
        bindingJson(app, "Types", PackageBindings.Scope.Library(libThree), Some("other.sig"))
      )
    )
    val parsed = read(scopeMap, forged, List("api.sig", "types.sig", "other.sig")).toOption.get
    val error = PackageBindings.validateAgainstInspection(parsed, apiIndex, parsed.dependencies, memberExposures = Map.empty, localInterfaces = Nil)
      .left.toOption.get

    assert(error.contains("does not match the inspected library index"), error)
  }

  test("conflicting providers in one member are rejected even with different releases") {
    val api = "Shared/Api"
    val index = publicIndex(List((libOne, api, Nil), (libTwo, api, Nil)))
    val left = PackageBindings.Scope.Library(libOne)
    val right = PackageBindings.Scope.Library(libTwo)
    val scopeMap = scopeMapJson(
      List("App"),
      List(
        dependencyJson(app, api, left),
        dependencyJson(app, api, right)
      ),
      List(importJson(libOne, "v1.sig"), importJson(libTwo, "v2.sig")),
      List(bindingJson(app, api, left, Some("v1.sig")))
    )
    val parsed = read(scopeMap, index, List("v1.sig", "v2.sig")).toOption.get
    val error = PackageBindings.validateAgainstInspection(parsed, index, parsed.dependencies, memberExposures = Map.empty, localInterfaces = Nil)
      .left.toOption.get

    assert(error.contains("ambiguous providers"), error)
    assert(error.contains("library shared@1.0.0:Shared/Api via member App -> library shared@1.0.0"), error)
    assert(error.contains("library shared@2.0.0:Shared/Api via member App -> library shared@2.0.0"), error)
  }
}
