package dev.bosatsu.library

import dev.bosatsu.{Json, PackageName}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class LibConfigJvmTest extends munit.FunSuite {
  private def decodeConfig(jsonStr: String): LibConfig = {
    val json = Json.parserFile.parseAll(jsonStr) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse config json: $err")
    }

    Json.Reader[LibConfig].read(Json.Path.Root, json) match {
      case Right(value)         => value
      case Left((msg, got, jp)) =>
        fail(s"failed to decode config json: $msg, got=$got at $jp")
    }
  }

  private def packageName(str: String): PackageName =
    PackageName.parse(str).getOrElse(fail(s"invalid package name: $str"))

  test("core_alpha config exports only the retained public surface") {
    val path = Paths.get("test_workspace", "core_alpha_conf.json")
    val conf =
      decodeConfig(Files.readString(path, StandardCharsets.UTF_8))

    val expectedPackages = List(
      "Bosatsu/Char",
      "Bosatsu/Collection/Array",
      "Bosatsu/Eval",
      "Bosatsu/IO/Bytes",
      "Bosatsu/IO/Core",
      "Bosatsu/IO/Error",
      "Bosatsu/IO/Std",
      "Bosatsu/Json",
      "Bosatsu/Lazy",
      "Bosatsu/Num/Float64",
      "Bosatsu/Prog"
    ).map(packageName)

    // Bosatsu/Json is command-known, Bosatsu/IO/Std is deliberate public
    // surface, and Bosatsu/Num/Nat disappears after the local Json refactor.
    val expectedExports =
      expectedPackages.map(LibConfig.PackageFilter.Name(_))
    val natPackage = packageName("Bosatsu/Num/Nat")

    assertEquals(conf.exportedPackages, expectedExports)
    assertEquals(conf.allPackages.map(_.asString), List(".*"))
    assert(!conf.exportedPackages.exists(_.accepts(natPackage)))

    val previousVersion =
      conf.previous
        .flatMap(_.version)
        .map(Version.fromProto)
        .getOrElse(fail("expected core_alpha to declare a previous version"))

    assertEquals(previousVersion.diffKindTo(conf.nextVersion), Version.DiffKind.Major)
    assertEquals(conf.nextVersion, previousVersion.nextMajor)
  }
}
