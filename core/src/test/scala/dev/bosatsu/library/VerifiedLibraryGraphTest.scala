package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import dev.bosatsu.MemoryMain
import dev.bosatsu.hashing.Algo
import dev.bosatsu.tool.LibraryInspection
import org.scalacheck.{Gen, Prop}

class VerifiedLibraryGraphTest extends munit.ScalaCheckSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type Path = Chain[String]
  private type Fixture = (String, proto.Library, Array[Byte], List[String])

  private val platform = MemoryMain[ErrorOr].platformIO
  import platform.{canPromiseF, moduleIOMonad}

  private val fixtures = LibraryInspectionFixtures.parsedLibraries
  private val rootFixture = fixtures.find(_._2.name == "parser").get

  private def path(raw: String): Path =
    Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private def state(files: List[(String, Array[Byte])] = Nil): MemoryMain.State =
    (LibraryInspectionFixtures.generatedFiles.map(file => file.path -> file.bytes) ::: files)
      .foldLeft(MemoryMain.State.empty) { case (current, (rawPath, bytes)) =>
        current.withFile(path(rawPath), MemoryMain.FileContent.Bytes(bytes)).get
      }

  private def context(
      children: List[Fixture],
      extra: List[LibraryInspection.DeclaredLibraryFile[Path]] = Nil
  ): LibraryInspection.DependencyContext[Path] =
    LibraryInspection.DependencyContext(
      children.map { case (file, library, _, hashes) =>
        LibraryInspection.DeclaredLibraryFile(
          Name(library.name),
          Library.getVersion(library).get,
          path(file),
          hashes
        )
      } ::: extra
    )

  private def pins(bytes: Array[Byte]): List[String] =
    List(
      s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
      s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
    )

  private def verify(
      root: Fixture = rootFixture,
      libraries: List[Fixture] = fixtures.filterNot(_._1 == rootFixture._1),
      current: MemoryMain.State = state()
  ): ErrorOr[VerifiedLibraryGraph[Path]] = {
    val (rootPath, library, _, rootPins) = root
    VerifiedLibrarySnapshot
      .verify(
        platform,
        path(rootPath),
        Name(library.name),
        Library.getVersion(library).get,
        rootPins
      )
      .flatMap(snapshot =>
        VerifiedLibraryGraph.verify(platform, snapshot, context(libraries))
      )
      .runA(current)
  }

  private def message(result: ErrorOr[?]): String =
    result.swap.toOption.flatMap(error => Option(error.getMessage)).getOrElse {
      fail("expected verified graph construction to fail")
    }

  private def withFile(files: List[Fixture], rawPath: String, library: proto.Library): List[Fixture] = {
    val bytes = library.toByteArray
    val next = (rawPath, library, bytes, pins(bytes))
    next :: files.filterNot(_._1 == rawPath)
  }

  test("authenticates all visibility edges and decodes each private version in its own scope") {
    val result = verify().toOption.get
    assertEquals(result.identities.size, fixtures.size)
    assertEquals(result.edges.size, 11)
    assertEquals(
      result.edges.map(_.visibility).distinct.sortBy(_.rank),
      List(
        LibraryDependencyVisibility.Public,
        LibraryDependencyVisibility.Private,
        LibraryDependencyVisibility.UnusedTransitivePublic
      )
    )
    val shared = result.identities.filter(identity =>
      Ordering[Name].equiv(identity._1, Name("shared"))
    )
    assertEquals(shared, List((Name("shared"), Version(1, 0, 0)), (Name("shared"), Version(2, 0, 0))))

    val decoded = DecodedLibraryWithDeps
      .decodeVerifiedGraph[MemoryMain.StateF[ErrorOr]](result)
      .runA(state())
      .toOption.get
    val left = decoded.deps((Name("left"), Version(1, 0, 0)))
    val right = decoded.deps((Name("right"), Version(1, 0, 0)))
    assert(left.deps.contains((Name("shared"), Version(1, 0, 0))))
    assert(!left.deps.contains((Name("shared"), Version(2, 0, 0))))
    assert(right.deps.contains((Name("shared"), Version(2, 0, 0))))
    assert(!right.deps.contains((Name("shared"), Version(1, 0, 0))))
    assert(!decoded.deps.contains((Name("common"), Version(1, 0, 0))))
  }

  test("every second diamond edge is checked after its target was authenticated") {
    val parser = rootFixture._2
    val privateChild = fixtures.find(_._2.name == "private_child").get
    val corruptPrivateChild = privateChild._2.copy(
      privateDependencies = privateChild._2.privateDependencies.map { dependency =>
        dependency.copy(desc = dependency.desc.map(_.copy(hashes = List(s"sha256:${"0" * 64}"))))
      }
    )
    val corruptChildBytes = corruptPrivateChild.toByteArray
    val parserWithUpdatedChildPin = parser.copy(
      privateDependencies = parser.privateDependencies.map { dependency =>
        if (dependency.name == "private_child")
          dependency.copy(desc = dependency.desc.map(_.copy(hashes = List(pins(corruptChildBytes).head))))
        else dependency
      }
    )
    val parserBytes = parserWithUpdatedChildPin.toByteArray
    val changedRoot = (
      rootFixture._1,
      parserWithUpdatedChildPin,
      parserBytes,
      pins(parserBytes)
    )
    val changedChildren = withFile(
      fixtures.filterNot(_._1 == rootFixture._1),
      privateChild._1,
      corruptPrivateChild
    )
    val current = state(
      List(
        privateChild._1 -> corruptChildBytes,
        rootFixture._1 -> parserBytes
      )
    )
    val failure = message(verify(changedRoot, changedChildren, current))
    assert(failure.contains("sha256 pin mismatch"), failure)
    assert(failure.contains("private_child"), failure)
    assert(failure.contains("parser"), failure)
  }

  test("a referring library cannot see two providers for one package in its scope") {
    val parser = rootFixture._2
    val left = fixtures.find(_._2.name == "left").get._2
    val right = fixtures.find(_._2.name == "right").get._2
    val withAmbiguousProviders = parser.copy(
      privateDependencies = parser.privateDependencies.toList :::
        left.privateDependencies.toList ::: right.privateDependencies.toList
    )
    val bytes = withAmbiguousProviders.toByteArray
    val changedRoot = (rootFixture._1, withAmbiguousProviders, bytes, pins(bytes))
    val current = state(List(rootFixture._1 -> bytes))
    val graph = verify(root = changedRoot, current = current).toOption.get
    val failure = message(
      DecodedLibraryWithDeps
        .decodeVerifiedGraph[MemoryMain.StateF[ErrorOr]](graph)
        .runA(current)
    )
    assert(failure.contains("ambiguous dependency providers in parser"), failure)
    assert(failure.contains("shared 1.0.0"), failure)
    assert(failure.contains("shared 2.0.0"), failure)
  }

  test("missing package providers identify the referring library scope") {
    val parser = rootFixture._2.copy(publicDependencies = Nil)
    val bytes = parser.toByteArray
    val changedRoot = (rootFixture._1, parser, bytes, pins(bytes))
    val current = state(List(rootFixture._1 -> bytes))
    val graph = verify(root = changedRoot, current = current).toOption.get
    val failure = message(
      DecodedLibraryWithDeps
        .decodeVerifiedGraph[MemoryMain.StateF[ErrorOr]](graph)
        .runA(current)
    )
    assert(failure.contains("cannot decode library parser 1.0.0"), failure)
    assert(failure.contains("missing package Public/Child"), failure)
  }

  test("equal aliases coalesce while conflicting bytes identify both paths") {
    val common = fixtures.find(_._2.name == "common").get
    val aliasPath = "aliases/common-copy.bosatsu_lib"
    val alias = LibraryInspection.DeclaredLibraryFile(
      Name("common"),
      Version(1, 0, 0),
      path(aliasPath),
      common._4
    )
    val baseContext = fixtures.filterNot(_._1 == rootFixture._1)
    val equalContext = context(baseContext, List(alias))
    val rootSnapshot = VerifiedLibrarySnapshot
      .verify(
        platform,
        path(rootFixture._1),
        Name(rootFixture._2.name),
        Library.getVersion(rootFixture._2).get,
        rootFixture._4
      )
      .flatMap(snapshot => VerifiedLibraryGraph.verify(platform, snapshot, equalContext))
      .runA(state(List(aliasPath -> common._3)))
      .toOption.get
    assertEquals(rootSnapshot.aliases((Name("common"), Version(1, 0, 0))).size, 2)

    val conflictingLibrary = common._2.copy(repoUri = "https://example.invalid/conflicting-common")
    val conflictingBytes = conflictingLibrary.toByteArray
    val conflictingAlias = alias.copy(
      path = path("aliases/conflicting-common.bosatsu_lib"),
      hashes = pins(conflictingBytes)
    )
    val conflictingContext = context(baseContext, List(conflictingAlias))
    val conflict = VerifiedLibrarySnapshot
      .verify(
        platform,
        path(rootFixture._1),
        Name(rootFixture._2.name),
        Library.getVersion(rootFixture._2).get,
        rootFixture._4
      )
      .flatMap(snapshot => VerifiedLibraryGraph.verify(platform, snapshot, conflictingContext))
      .runA(state(List(conflictingAlias.path.toList.mkString("/") -> conflictingBytes)))
    val error = message(conflict)
    assert(error.contains("conflicting files claim common"), error)
    assert(error.contains(common._1), error)
    assert(error.contains("aliases/conflicting-common.bosatsu_lib"), error)
    assert(error.contains("parser"), error)
  }

  test("missing files are reported for public, private and legacy resolution edges") {
    val parser = rootFixture._2
    val direct = List(
      LibraryDependencyVisibility.Public -> "public_child",
      LibraryDependencyVisibility.Private -> "left",
      LibraryDependencyVisibility.UnusedTransitivePublic -> "common"
    )
    direct.foreach { case (visibility, name) =>
      val matchingEdges = visibility match {
        case LibraryDependencyVisibility.Public => parser.publicDependencies
        case LibraryDependencyVisibility.Private => parser.privateDependencies
        case LibraryDependencyVisibility.UnusedTransitivePublic =>
          parser.unusedTransitivePublicDependencies
      }
      assert(matchingEdges.exists(_.name == name))
      val removed = fixtures.filterNot(entry =>
        entry._1 == rootFixture._1 || entry._2.name == name
      )
      val failure = message(verify(libraries = removed))
      assert(failure.contains(s"missing declared file for $name"), failure)
      assert(failure.contains("parser"), failure)
      assert(parser.name == "parser")
    }
  }

  test("dependency descriptors must carry both an exact version and supported pins") {
    val parser = rootFixture._2
    val public = parser.publicDependencies.head
    val descriptor = public.desc.get
    val missingVersion = parser.copy(
      publicDependencies = List(public.copy(desc = Some(descriptor.copy(version = None))))
    )
    val missingPins = parser.copy(
      publicDependencies = List(public.copy(desc = Some(descriptor.copy(hashes = Nil))))
    )
    List(
      missingVersion -> "missing version for dependency 'public_child'",
      missingPins -> "missing expected hashes for dependency 'public_child'"
    ).foreach { case (library, expected) =>
      val bytes = library.toByteArray
      val changedRoot = (rootFixture._1, library, bytes, pins(bytes))
      val failure = message(
        verify(
          root = changedRoot,
          current = state(List(rootFixture._1 -> bytes))
        )
      )
      assert(failure.contains(expected), failure)
      assert(failure.contains("parser"), failure)
    }
  }

  test("cycle detection reports the authenticated path that reaches the cycle") {
    val root = (Name("cycle_root"), Version(1, 0, 0))
    val left = (Name("cycle_left"), Version(1, 0, 0))
    val right = (Name("cycle_right"), Version(1, 0, 0))
    val result = VerifiedLibraryGraph.topologicalOrder(
      root,
      List(root, left, right),
      List(root -> left, left -> right, right -> left),
      Map(
        root -> List(root),
        left -> List(root, left),
        right -> List(root, left, right)
      )
    )
    val error = result.swap.toOption.getOrElse(fail("expected a dependency cycle"))
    assert(error.contains("dependency cycle prevents ordering"), error)
    assert(error.contains("cycle_root 1.0.0 -> cycle_left 1.0.0"), error)
    assert(error.contains("cycle_right 1.0.0"), error)
  }

  property("context order does not affect canonical authenticated edge metadata") {
    val allChildren = fixtures.filterNot(_._1 == rootFixture._1)
    Prop.forAll(Gen.pick(allChildren.length, allChildren.indices)) { order =>
      val ordered = order.toList.map(allChildren)
      val reversed = ordered.reverse
      val first = verify(libraries = ordered).toOption.get
      val second = verify(libraries = reversed).toOption.get
      def canonical(graph: VerifiedLibraryGraph[Path]): List[String] =
        graph.edges.map { edge =>
          s"${edge.referringIdentity._1.name}:${edge.referringIdentity._2.render}:" +
            s"${edge.visibility}:${edge.targetIdentity._1.name}:${edge.targetIdentity._2.render}:" +
            s"${edge.expectedHashes.mkString(",")}:${edge.witnessPath.map(_._1.name).mkString("/")}"
        }
      canonical(first) == canonical(second)
    }
  }
}
