package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import dev.bosatsu.MemoryMain
import dev.bosatsu.hashing.Algo
import org.scalacheck.{Gen, Prop}

class LibraryInspectionFixturesTest extends munit.ScalaCheckSuite {
  private type Result[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val platform = MemoryMain.memoryPlatformIO[Result]
  private val fixtures = LibraryInspectionFixtures.parsedLibraries

  private def path(raw: String): Path = Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private val fixtureState = LibraryInspectionFixtures.generatedFiles.foldLeft(MemoryMain.State.empty) {
    case (state, file) =>
      state.withFile(path(file.path), MemoryMain.FileContent.Bytes(file.bytes)).get
  }

  private def dependencies(library: proto.Library): List[proto.LibDependency] =
    library.publicDependencies.toList :::
      library.privateDependencies.toList :::
      library.unusedTransitivePublicDependencies.toList

  test("Parser fixtures retain visibility, a diamond and separate private versions") {
    val byIdentity = fixtures.map { case (file, lib, bytes, pins) =>
      ((lib.name, Library.getVersion(lib).get), (file, lib, bytes, pins))
    }.toMap
    val parser = byIdentity(("parser", Version(1, 0, 0)))._2
    val publicChild = byIdentity(("public_child", Version(1, 0, 0)))._2
    val privateChild = byIdentity(("private_child", Version(1, 0, 0)))._2
    val left = byIdentity(("left", Version(1, 0, 0)))._2
    val right = byIdentity(("right", Version(1, 0, 0)))._2

    assertEquals(parser.publicDependencies.map(_.name), Seq("public_child"))
    assertEquals(
      parser.privateDependencies.map(_.name).toSet,
      Set("private_child", "left", "right")
    )
    assertEquals(
      parser.unusedTransitivePublicDependencies.map(_.name).toSet,
      Set("common", "legacy")
    )
    assertEquals(publicChild.publicDependencies.map(_.name).toSet, Set("common", "legacy"))
    assertEquals(privateChild.privateDependencies.map(_.name), Seq("common"))
    assertEquals(left.privateDependencies.map(dep => (dep.name, Library.getVersion(dep).get)),
      Seq(("shared", Version(1, 0, 0))))
    assertEquals(right.privateDependencies.map(dep => (dep.name, Library.getVersion(dep).get)),
      Seq(("shared", Version(2, 0, 0))))

    val contextPath = path("test_workspace/inspect_library/parser-context.json")
    val context = dev.bosatsu.tool.LibraryInspection
      .readDependencyContext(platform, contextPath)
      .runA(fixtureState).toOption.get
    assertEquals(context.libraries.size, fixtures.size - 1)
    assertEquals(context.libraries.count(_.name.name == "shared"), 2)
    val rootContext = dev.bosatsu.tool.LibraryInspection
      .readDependencyContext(
        platform,
        path("test_workspace/inspect_library/parser-root.json")
      ).runA(fixtureState).toOption.get
    assertEquals(rootContext.libraries.map(_.name), List(Name("parser")))

    dependencies(parser).foreach { dep =>
      val version = Library.getVersion(dep).get
      val childPins = byIdentity((dep.name, version))._4
      assert(dep.desc.toList.flatMap(_.hashes).exists(childPins.contains), dep.toString)
    }
    assertEquals(Algo.hashBytes[Algo.Sha256](byIdentity(("parser", Version(1, 0, 0)))._3).hex,
      byIdentity(("parser", Version(1, 0, 0)))._4.last.stripPrefix("sha256:"))
  }

  property("each serialized artifact authenticates with its recorded pins") {
    Prop.forAll(Gen.choose(0, fixtures.length - 1)) { index =>
      val (file, library, _, pins) = fixtures(index)
      val result = VerifiedLibrarySnapshot.verify(
        platform,
        path(file),
        Name(library.name),
        Library.getVersion(library).get,
        pins
      ).runA(fixtureState)
      result.toOption.exists(snapshot => snapshot.sha256.hex == pins.last.stripPrefix("sha256:"))
    }
  }

  test("changing serialized fixture bytes without changing pins is rejected") {
    fixtures.foreach { case (file, library, bytes, pins) =>
      val modified = bytes ++ Array[Byte](0)
      val changedState = fixtureState.withFile(
        path(file),
        MemoryMain.FileContent.Bytes(modified)
      ).get
      val result = VerifiedLibrarySnapshot.verify(
        platform,
        path(file),
        Name(library.name),
        Library.getVersion(library).get,
        pins
      ).runA(changedState)
      assert(result.isLeft, file)
    }
  }
}
