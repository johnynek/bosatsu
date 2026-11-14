package dev.bosatsu.tool

import cats.data.Chain
import dev.bosatsu.MemoryMain
import java.nio.charset.StandardCharsets
import org.scalacheck.{Gen, Prop}

class LibraryInspectionTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val platform = MemoryMain.memoryPlatformIO[Result]
  private val contextPath: Path = Chain.one("context.json")

  private def readContext(json: String): Result[LibraryInspection.DependencyContext[Path]] = {
    val state = MemoryMain.State.empty
      .withFile(
        contextPath,
        MemoryMain.FileContent.Bytes(json.getBytes(StandardCharsets.UTF_8))
      ).get
    LibraryInspection.readDependencyContext(platform, contextPath).runA(state)
  }

  private def entry(name: String, version: String, path: String, hash: String): String =
    s"""{"name":"$name","version":"$version","path":"$path","hashes":["$hash"]}"""

  private def context(libraries: String, extra: String = ""): String =
    s"""{"schema_version":1,"libraries":[$libraries]$extra}"""

  private def declarations(
      result: Result[LibraryInspection.DependencyContext[Path]]
  ): List[(String, String, String, List[String])] =
    result.toOption.get.libraries.map { library =>
      (
        library.name.name,
        library.version.render,
        platform.pathToString(library.path),
        library.hashes
      )
    }.sortBy { case (name, version, path, _) => (name, version, path) }

  private def rejects(result: Result[?], parts: String*): Unit = {
    val message = result.swap.toOption.flatMap(error => Option(error.getMessage))
      .getOrElse(fail("accepted invalid library dependency context"))
    parts.foreach(part => assert(message.contains(part), message))
  }

  test("dependency context preserves aliases and parses each path through the platform") {
    val sameIdentity = List(
      entry("parser", "1.0.0", "deps/parser-a.bosatsu_lib", "blake3:abc"),
      entry("parser", "1.0.0", "deps/parser-b.bosatsu_lib", "sha256:def"),
      entry("parser", "2.0.0", "deps/parser-v2.bosatsu_lib", "blake3:ghi")
    )
    val result = readContext(context(sameIdentity.mkString(",")))
    assertEquals(declarations(result).size, 3)
    assertEquals(
      declarations(result).map { case (name, version, path, _) => (name, version, path) },
      List(
        ("parser", "1.0.0", "deps/parser-a.bosatsu_lib"),
        ("parser", "1.0.0", "deps/parser-b.bosatsu_lib"),
        ("parser", "2.0.0", "deps/parser-v2.bosatsu_lib")
      )
    )
  }

  property("permuting context declarations preserves identities, aliases and pins") {
    val values = List(
      entry("parser", "1.0.0", "deps/parser-a.bosatsu_lib", "blake3:abc"),
      entry("parser", "1.0.0", "deps/parser-b.bosatsu_lib", "sha256:def"),
      entry("shared", "1.0.0", "deps/shared-v1.bosatsu_lib", "blake3:ghi"),
      entry("shared", "2.0.0", "deps/shared-v2.bosatsu_lib", "sha256:jkl")
    )
    Prop.forAll(Gen.pick(values.length, values)) { shuffled =>
      declarations(readContext(context(shuffled.mkString(",")))) == declarations(
        readContext(context(values.mkString(",")))
      )
    }
  }

  test("context schema, fields, identities, paths and repeated JSON keys are checked") {
    val valid = entry("parser", "1.0.0", "deps/parser.bosatsu_lib", "sha256:abc")

    rejects(
      readContext(context(valid).replace("\"schema_version\":1", "\"schema_version\":2")),
      "unsupported schema"
    )
    rejects(readContext(context(valid, ",\"unknown\":true")), "unknown library dependency context fields")
    rejects(
      readContext(context(valid.replace("\"hashes\"", "\"extra\":1,\"hashes\""))),
      "unknown declared library file fields"
    )
    rejects(
      readContext("""{"schema_version":1,"schema_version":1,"libraries":[]}"""),
      "repeated JSON fields",
      "schema_version"
    )
    rejects(
      readContext(context(valid.replace("\"hashes\"", "\"hashes\":[\"sha256:abc\"],\"hashes\""))),
      "repeated JSON fields",
      "hashes"
    )
    rejects(readContext(context(valid.replace("\"parser\"", "\"\""))), "nonempty string")
    rejects(readContext(context(valid.replace("1.0.0", "not-a-version"))), "couldn't parse semver")
    rejects(readContext(context(valid.replace("deps/parser.bosatsu_lib", ""))), "nonempty string")
    rejects(readContext(context(valid.replace("[\"sha256:abc\"]", "[]"))), "at least one recorded hash")
    rejects(readContext("{not json}"), "invalid JSON")
  }
}
