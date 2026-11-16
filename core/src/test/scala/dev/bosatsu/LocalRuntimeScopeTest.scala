package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{Chain, Ior, NonEmptyList}
import cats.implicits._
import dev.bosatsu.tool.{LocalRuntime, LocalRuntimeManifest}

/** Exercise the same file decoder on the JVM and Scala.js. The in-memory
  * filesystem contains only the listed bytes: it cannot discover other inputs.
  */
class LocalRuntimeScopeTest extends munit.FunSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]
  private val memory = MemoryMain.memoryPlatformIO[Result]
  private val platform = memory
  private val manifestPath = Chain.one("runtime.json")
  private def file(name: String): Path = Chain.one(name)
  private def wire(name: String): proto.Package =
    proto.Package(strings = List(name), packageName = 1)
  private def bundle(packs: proto.Package*): MemoryMain.FileContent =
    MemoryMain.FileContent.Bytes(Bytes.fromMessage(proto.Packages(packs.toList)))
  private def graph(files: String*): String =
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "kind" -> Json.JString("local"),
      "packages" -> Json.JArray(files.map(Json.JString(_)).toVector),
      "externals" -> Json.JArray(Vector.empty)
    )).render

  private def state(json: String, files: Seq[(String, MemoryMain.FileContent)]): MemoryMain.State = {
    val initial = MemoryMain.State.empty.withFile(manifestPath, MemoryMain.FileContent.Str(json)).get
    files.foldLeft(initial) { case (acc, (name, contents)) => acc.withFile(file(name), contents).get }
  }

  private def load(json: String, files: (String, MemoryMain.FileContent)*): Result[LocalRuntime[Path]] =
    LocalRuntimeManifest.read(platform, manifestPath)
      .flatMap(LocalRuntime.load(platform, _))
      .runA(state(json, files))

  private def checked[A](result: Result[A]): A = result.fold(err => fail(err.getMessage), identity)

  private def compilePackage(source: String, interfaces: List[Package.Interface]): Package.Compiled = {
    val parsed = Parser.unsafeParse(Package.parser, source)
    val result = Par.noParallelism {
      PackageMap.typeCheckParsed(
        NonEmptyList.one((("local-runtime", LocationMap(source)), parsed)),
        interfaces,
        "<predef>",
        CompileOptions.Default
      )
    }
    val packages = result match {
      case Ior.Left(errors) => fail(errors.toList.mkString("typecheck failed: ", ", ", ""))
      case Ior.Right(value) => value
      case Ior.Both(_, value) => value
    }
    packages.toMap(parsed.name)
  }

  private def rejects[A](result: Result[A], expected: String*): Unit = {
    val msg = result.swap.toOption.getOrElse(fail("accepted an invalid runtime scope")).getMessage
    expected.foreach(s => assert(msg.contains(s), msg))
  }

  test("source-free decode retains per-file membership and adds the builtin Predef implementation once") {
    val first = "snow 雪.pkg"
    val second = "dep.pkg"
    val input = List(first -> bundle(wire("Demo/Z"), wire("Demo/A")), second -> bundle(wire("Dep")))
    val scope = checked(load(graph(second, first, second), input*))
    val names = scope.members.view.mapValues(_.toList.map(_.asString)).toMap
    assertEquals(names, Map(file(first) -> List("Demo/A", "Demo/Z"), file(second) -> List("Dep")))
    assertEquals(scope.packages.toMap.keysIterator.map(_.asString).toList,
      List("Bosatsu/Predef", "Demo/A", "Demo/Z", "Dep"))
    assertEquals(scope.packages.toMap(PackageName.PredefName).lets.size, PackageMap.predefCompiled.lets.size)
    assert(scope.hasBinding(PackageName.PredefName, Identifier.Name("add")))
    assert(!scope.hasBinding(PackageName.parts("Dep"), Identifier.Name("add")))
    val reordered = checked(load(graph(first, second), input.reverse*))
    assertEquals(reordered.members, scope.members)
  }

  test("in-memory package readers validate scoped metadata before decoding packages") {
    val local = proto.Packages(
      packages = List(wire("Demo")),
      buildRequirements = Some(proto.BuildRequirements(
        scopedVersion = 1,
        members = List(proto.MemberBuildRequirements(memberName = "Demo"))
      ))
    )
    val imported = proto.Packages(
      packages = List(wire("Client").copy(
        strings = List("Client", "External"),
        imports = List(proto.Imports(packageName = 2))
      )),
      buildRequirements = Some(proto.BuildRequirements(
        scopedVersion = 1,
        members = List(proto.MemberBuildRequirements(
          memberName = "Client",
          bindings = List(proto.ScopedBindingRequirement(
            referrer = Some(proto.BuildScope(kind = "member", memberName = "Client")),
            packageName = "External",
            owner = Some(proto.BuildScope(
              kind = "library",
              libraryName = "shared",
              libraryVersion = Some(proto.Version(major = 1))
            )),
            ownerPackageName = "External"
          )),
          interfaces = List(proto.InterfaceRequirement("External", "0" * 64))
        ))
      ))
    )
    val localSignature = proto.Packages(
      packages = List(wire("Client").copy(
        strings = List("Client", "External"),
        imports = List(proto.Imports(packageName = 2))
      )),
      buildRequirements = Some(proto.BuildRequirements(
        scopedVersion = 1,
        members = List(proto.MemberBuildRequirements(
          memberName = "Client",
          bindings = List(proto.ScopedBindingRequirement(
            referrer = Some(proto.BuildScope(kind = "member", memberName = "Client")),
            packageName = "External",
            owner = Some(proto.BuildScope(kind = "local", memberName = "External")),
            ownerPackageName = "External"
          )),
          interfaces = List(proto.InterfaceRequirement("External", "0" * 64))
        ))
      ))
    )

    def read(bundle: proto.Packages): Result[List[Package.Compiled]] = {
      val content = MemoryMain.FileContent.Bytes(Bytes.fromMessage(bundle))
      val initial = MemoryMain.State.empty.withFile(file("packages.bundle"), content).get
      memory.readPackages(List(file("packages.bundle"))).runA(initial)
    }

    assertEquals(checked(read(local)).map(_.name.asString), List("Demo"))
    rejects(read(imported), "unavailable imported provider context")
    rejects(read(localSignature), "unavailable local signature context")
  }

  test("reject an explicit Predef even when no other group claims it") {
    rejects(load(graph("lib.pkg"), "lib.pkg" -> bundle(wire("Bosatsu/Predef"))),
      "lib.pkg", "Bosatsu/Predef")
  }

  test("reject duplicate owners across bundles before collapsing to a package map") {
    val a = "first.pkg" -> bundle(wire("Demo/Common"))
    val b = "second.pkg" -> bundle(wire("Demo/Common"))
    List(graph("first.pkg", "second.pkg"), graph("second.pkg", "first.pkg")).foreach { json =>
      rejects(load(json, a, b), "duplicate implementation", "Demo/Common", "first.pkg", "second.pkg")
    }
  }

  test("reject duplicate package entries inside one implementation bundle") {
    val member = wire("Demo/One")
    rejects(load(graph("suite.pkg"), "suite.pkg" -> bundle(member, member)),
      "duplicate implementation", "Demo/One", "suite.pkg")
  }

  test("name the owner of a missing implementation even when it is only a transitive dependency") {
    def imports(name: String, dep: String): proto.Package =
      wire(name).copy(strings = List(name, dep), imports = List(proto.Imports(packageName = 2)))
    val app = "app.pkg" -> bundle(imports("App", "Middle"))
    val middle = "middle.pkg" -> bundle(imports("Middle", "Absent"))
    rejects(load(graph("app.pkg", "middle.pkg"), app, middle), "missing package Absent required by Middle")
  }

  test("circular implementation dependencies fail before code generation") {
    val cycle = proto.Imports(packageName = 2)
    val a = wire("First").copy(strings = List("First", "Second"), imports = List(cycle))
    val b = wire("Second").copy(strings = List("Second", "First"), imports = List(cycle))
    rejects(load(graph("loop.pkg"), "loop.pkg" -> bundle(a, b)), "circular dependencies", "First", "Second")
  }

  test("runtime rejects scoped requirements that need unavailable imported providers") {
    val clientName = PackageName.parts("Client")
    val externalName = PackageName.parts("Demo/External")
    val external = compilePackage(
      "package Demo/External\nexport value\nvalue = 1\n",
      Nil
    )
    val externalInterface = Package.interfaceOf(external)
    val client = compilePackage(
      "package Client\nfrom Demo/External import value\nexport main\nmain = value\n",
      List(externalInterface)
    )
    val owner = proto.BuildScope(
      kind = "library",
      libraryName = "shared",
      libraryVersion = Some(proto.Version(major = 1))
    )
    val scopedRequirements = proto.BuildRequirements(
      scopedVersion = 1,
      members = List(proto.MemberBuildRequirements(
        memberName = clientName.asString,
        bindings = List(proto.ScopedBindingRequirement(
          referrer = Some(proto.BuildScope(kind = "member", memberName = clientName.asString)),
          packageName = externalName.asString,
          owner = Some(owner),
          ownerPackageName = externalName.asString
        )),
        interfaces = List(proto.InterfaceRequirement(externalName.asString, "0" * 64))
      ))
    )
    val clientBundle = (requirements: proto.BuildRequirements) =>
      MemoryMain.FileContent.Bytes(Bytes.fromMessage(
        proto.Packages(List(ProtoConverter.packageToProto(client).get), buildRequirements = Some(requirements))
      ))
    val externalBundle = MemoryMain.FileContent.Bytes(Bytes.fromMessage(
      proto.Packages(List(ProtoConverter.packageToProto(external).get))
    ))

    rejects(load(
      graph("client.pkg", "external.pkg"),
      "client.pkg" -> clientBundle(scopedRequirements),
      "external.pkg" -> externalBundle
    ), "unavailable imported provider context", externalName.asString)
  }

  test("runtime accepts self-contained scoped requirements") {
    val member = PackageName.parts("Local")
    val requirements = proto.BuildRequirements(
      scopedVersion = 1,
      members = List(proto.MemberBuildRequirements(memberName = member.asString))
    )
    val contents = MemoryMain.FileContent.Bytes(Bytes.fromMessage(proto.Packages(
      List(wire(member.asString)), buildRequirements = Some(requirements)
    )))
    val runtime = checked(load(graph("local.pkg"), "local.pkg" -> contents))
    assertEquals(runtime.members(file("local.pkg")).toList, List(member))
  }

  test("empty, corrupt and misidentified bundles have file diagnostics") {
    val name = "input.pkg"
    val json = graph(name)
    rejects(load(json), "cannot read implementation bundle", name)
    rejects(load(json, name -> bundle()), "empty implementation bundle", name)
    rejects(load(json, name -> bundle(wire("not/a/package"))), "invalid package name", name)
    rejects(load(json, name -> bundle(wire("Real").copy(packageName = 0))), "invalid package name", name)
    rejects(load(json, name -> MemoryMain.FileContent.Bytes(Bytes.copyFromArray(Array(10.toByte, 80.toByte)))),
      "cannot read implementation bundle", name)
    val signature = proto.Interfaces(List(proto.Interface(strings = List("Demo"), packageName = 1)))
    rejects(load(json, name -> MemoryMain.FileContent.Bytes(Bytes.fromMessage(signature))), name)
  }

  test("runtime manifest schema and scope errors precede all implementation reads") {
    val base = """{"schema_version":1,"kind":"local","packages":["lib.pkg"],"externals":[]}"""
    val cases = List(
      base.replace("\"local\"", "\"library\"") -> "unsupported runtime scope",
      base.replace("\"schema_version\":1", "\"schema_version\":2") -> "unsupported schema version",
      base.replace("\"externals\":[]", "\"externals\":[],\"libraries\":[\"v1.lib\"]") -> "unknown local runtime fields",
      base.replace("[\"lib.pkg\"]", "[]") -> "at least one implementation",
      base.replace("[\"lib.pkg\"]", "[\"\"]") -> "nonempty string",
      base.replace("[\"lib.pkg\"]", "\"lib.pkg\"") -> "packages",
      base.replace("\"externals\":[]", "\"externals\":null") -> "externals",
      base.replace("\"kind\":\"local\"", "\"kind\":\"local\",\"kind\":\"local\"") -> "repeated JSON fields"
    )
    cases.foreach { case (json, reason) => rejects(load(json), "runtime.json", reason) }
  }

  test("binding provider declarations validate package and symbol identities") {
    val base = """{"schema_version":1,"kind":"local","packages":["one.pkg"],"externals":[BINDINGS]}"""
    val valid = """{"package":"Demo/Api","symbols":["call"]}"""
    val one = "one.pkg" -> bundle(wire("Demo/Api"))
    val cases = List(
      valid.replace("Demo/Api", "bad") -> "package",
      valid.replace("\"call\"", "\"Ctor\"") -> "symbols",
      valid.replace("[\"call\"]", "[]") -> "at least one external symbol",
      valid.replace("[\"call\"]", "[\"call\",\"call\"]") -> "duplicate native binding",
      valid.replace("\"symbols\":", "\"scope\":\"private-v2\",\"symbols\":") -> "unknown native binding fields",
      s"$valid,$valid" -> "duplicate native binding",
      valid -> "Demo/Api::call has no external declaration",
      valid.replace("Demo/Api", "Missing") -> "missing package Missing"
    )
    cases.foreach { case (rows, expected) => rejects(load(base.replace("BINDINGS", rows), one), expected) }
  }
}
