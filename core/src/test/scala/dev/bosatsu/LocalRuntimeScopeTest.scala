package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import cats.implicits._
import dev.bosatsu.tool.{LocalRuntime, LocalRuntimeManifest}

/** Exercise the same file decoder on the JVM and Scala.js. The in-memory
  * filesystem contains only the listed bytes: it cannot discover other inputs.
  */
class LocalRuntimeScopeTest extends munit.FunSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]
  private val platform = MemoryMain.memoryPlatformIO[Result]
  private val manifestPath = Chain.one("runtime.json")
  private def file(name: String): Path = Chain.one(name)
  private def wire(name: String): proto.Package =
    proto.Package(strings = List(name), packageName = 1)
  private def bundle(packs: proto.Package*): MemoryMain.FileContent =
    MemoryMain.FileContent.Bytes(proto.Packages(packs.toList).toByteArray)
  private def graph(files: String*): String =
    Json
      .JObject(
        List(
          "schema_version" -> Json.JNumberStr("1"),
          "kind" -> Json.JString("local"),
          "packages" -> Json.JArray(files.map(Json.JString(_)).toVector),
          "externals" -> Json.JArray(Vector.empty)
        )
      )
      .render

  private def state(
      json: String,
      files: Seq[(String, MemoryMain.FileContent)]
  ): MemoryMain.State = {
    val initial = MemoryMain.State.empty
      .withFile(manifestPath, MemoryMain.FileContent.Str(json))
      .get
    files.foldLeft(initial) { case (acc, (name, contents)) =>
      acc.withFile(file(name), contents).get
    }
  }

  private def load(
      json: String,
      files: (String, MemoryMain.FileContent)*
  ): Result[LocalRuntime[Path]] =
    LocalRuntimeManifest
      .read(platform, manifestPath)
      .flatMap(LocalRuntime.load(platform, _))
      .runA(state(json, files))

  private def checked[A](result: Result[A]): A =
    result.fold(err => fail(err.getMessage), identity)

  private def rejects[A](result: Result[A], expected: String*): Unit = {
    val msg = result.swap.toOption
      .getOrElse(fail("accepted an invalid runtime scope"))
      .getMessage
    expected.foreach(s => assert(msg.contains(s), msg))
  }

  test(
    "source-free decode retains per-file membership and adds the builtin Predef implementation once"
  ) {
    val first = "snow 雪.pkg"
    val second = "dep.pkg"
    val input = List(
      first -> bundle(wire("Demo/Z"), wire("Demo/A")),
      second -> bundle(wire("Dep"))
    )
    val scope = checked(load(graph(second, first, second), input*))
    val names = scope.members.view.mapValues(_.toList.map(_.asString)).toMap
    assertEquals(
      names,
      Map(file(first) -> List("Demo/A", "Demo/Z"), file(second) -> List("Dep"))
    )
    assertEquals(
      scope.packages.toMap.keysIterator.map(_.asString).toList,
      List("Bosatsu/Predef", "Demo/A", "Demo/Z", "Dep")
    )
    assertEquals(
      scope.packages.toMap(PackageName.PredefName).lets.size,
      PackageMap.predefCompiled.lets.size
    )
    assert(scope.hasBinding(PackageName.PredefName, Identifier.Name("add")))
    assert(!scope.hasBinding(PackageName.parts("Dep"), Identifier.Name("add")))
    val reordered = checked(load(graph(first, second), input.reverse*))
    assertEquals(reordered.members, scope.members)
  }

  test("reject an explicit Predef even when no other group claims it") {
    rejects(
      load(graph("lib.pkg"), "lib.pkg" -> bundle(wire("Bosatsu/Predef"))),
      "lib.pkg",
      "Bosatsu/Predef"
    )
  }

  test(
    "reject duplicate owners across bundles before collapsing to a package map"
  ) {
    val a = "first.pkg" -> bundle(wire("Demo/Common"))
    val b = "second.pkg" -> bundle(wire("Demo/Common"))
    List(graph("first.pkg", "second.pkg"), graph("second.pkg", "first.pkg"))
      .foreach { json =>
        rejects(
          load(json, a, b),
          "duplicate implementation",
          "Demo/Common",
          "first.pkg",
          "second.pkg"
        )
      }
  }

  test("reject duplicate package entries inside one implementation bundle") {
    val member = wire("Demo/One")
    rejects(
      load(graph("suite.pkg"), "suite.pkg" -> bundle(member, member)),
      "duplicate implementation",
      "Demo/One",
      "suite.pkg"
    )
  }

  test(
    "name the owner of a missing implementation even when it is only a transitive dependency"
  ) {
    def imports(name: String, dep: String): proto.Package =
      wire(name).copy(
        strings = List(name, dep),
        imports = List(proto.Imports(packageName = 2))
      )
    val app = "app.pkg" -> bundle(imports("App", "Middle"))
    val middle = "middle.pkg" -> bundle(imports("Middle", "Absent"))
    rejects(
      load(graph("app.pkg", "middle.pkg"), app, middle),
      "missing package Absent required by Middle"
    )
  }

  test("circular implementation dependencies fail before code generation") {
    val cycle = proto.Imports(packageName = 2)
    val a = wire("First").copy(
      strings = List("First", "Second"),
      imports = List(cycle)
    )
    val b = wire("Second").copy(
      strings = List("Second", "First"),
      imports = List(cycle)
    )
    rejects(
      load(graph("loop.pkg"), "loop.pkg" -> bundle(a, b)),
      "circular dependencies",
      "First",
      "Second"
    )
  }

  test("empty, corrupt and misidentified bundles have file diagnostics") {
    val name = "input.pkg"
    val json = graph(name)
    rejects(load(json), "cannot read implementation bundle", name)
    rejects(load(json, name -> bundle()), "empty implementation bundle", name)
    rejects(
      load(json, name -> bundle(wire("not/a/package"))),
      "invalid package name",
      name
    )
    rejects(
      load(json, name -> bundle(wire("Real").copy(packageName = 0))),
      "invalid package name",
      name
    )
    rejects(
      load(
        json,
        name -> MemoryMain.FileContent.Bytes(Array(10.toByte, 80.toByte))
      ),
      "cannot read implementation bundle",
      name
    )
    val signature = proto.Interfaces(
      List(proto.Interface(strings = List("Demo"), packageName = 1))
    )
    rejects(
      load(json, name -> MemoryMain.FileContent.Bytes(signature.toByteArray)),
      name
    )
  }

  test(
    "runtime manifest schema and scope errors precede all implementation reads"
  ) {
    val base =
      """{"schema_version":1,"kind":"local","packages":["lib.pkg"],"externals":[]}"""
    val cases = List(
      base.replace("\"local\"", "\"library\"") -> "unsupported runtime scope",
      base.replace(
        "\"schema_version\":1",
        "\"schema_version\":2"
      ) -> "unsupported schema version",
      base.replace(
        "\"externals\":[]",
        "\"externals\":[],\"libraries\":[\"v1.lib\"]"
      ) -> "unknown local runtime fields",
      base.replace("[\"lib.pkg\"]", "[]") -> "at least one implementation",
      base.replace("[\"lib.pkg\"]", "[\"\"]") -> "nonempty string",
      base.replace("[\"lib.pkg\"]", "\"lib.pkg\"") -> "packages",
      base.replace("\"externals\":[]", "\"externals\":null") -> "externals",
      base.replace(
        "\"kind\":\"local\"",
        "\"kind\":\"local\",\"kind\":\"local\""
      ) -> "repeated JSON fields"
    )
    cases.foreach { case (json, reason) =>
      rejects(load(json), "runtime.json", reason)
    }
  }

  test("binding provider declarations validate package and symbol identities") {
    val base =
      """{"schema_version":1,"kind":"local","packages":["one.pkg"],"externals":[BINDINGS]}"""
    val valid = """{"package":"Demo/Api","symbols":["call"]}"""
    val one = "one.pkg" -> bundle(wire("Demo/Api"))
    val cases = List(
      valid.replace("Demo/Api", "bad") -> "package",
      valid.replace("\"call\"", "\"Ctor\"") -> "symbols",
      valid.replace("[\"call\"]", "[]") -> "at least one external symbol",
      valid.replace(
        "[\"call\"]",
        "[\"call\",\"call\"]"
      ) -> "duplicate native binding",
      valid.replace(
        "\"symbols\":",
        "\"scope\":\"private-v2\",\"symbols\":"
      ) -> "unknown native binding fields",
      s"$valid,$valid" -> "duplicate native binding",
      valid -> "Demo/Api::call has no external declaration",
      valid.replace("Demo/Api", "Missing") -> "missing package Missing"
    )
    cases.foreach { case (rows, expected) =>
      rejects(load(base.replace("BINDINGS", rows), one), expected)
    }
  }
}
