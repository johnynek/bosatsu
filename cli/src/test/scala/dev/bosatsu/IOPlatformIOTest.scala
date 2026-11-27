package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.{Eq, Hash}
import cats.collections.HashMap
import cats.effect.{IO, Resource}
import cats.implicits._
import cats.effect.unsafe.implicits.global
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, InputStream, PrintStream}
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class IOPlatformIOTest extends munit.ScalaCheckSuite {
  private val genRegion: Gen[Region] =
    for {
      start <- Gen.choose(0, 200)
      length <- Gen.choose(0, 50)
    } yield Region(start, start + length)

  val sortedEq: Eq[List[Package.Interface]] =
    new Eq[List[Package.Interface]] {
      given Eq[Package.Interface] =
        // Safe: Package.Interface is immutable and uses structural equals.
        Eq.fromUniversalEquals
      def eqv(l: List[Package.Interface], r: List[Package.Interface]) =
        // we are only sorting the left because we expect the right
        // to come out sorted
        l.sortBy(_.name.asString) === r
    }

  private def deleteRecursively(path: Path): Unit = {
    val walk = Files.walk(path)
    try {
      walk.sorted(Comparator.reverseOrder()).forEach { p =>
        Files.deleteIfExists(p)
        ()
      }
    } finally walk.close()
  }

  private def withTempDir[A](fn: Path => A): A = {
    val dir = Files.createTempDirectory("bosatsu-io-platform-")
    try fn(dir)
    finally deleteRecursively(dir)
  }

  def testWithTempFile(fn: Path => IO[Unit]): Unit = {
    val tempRes = Resource.make(IO.blocking {
      val f = File.createTempFile("proto_test", ".proto")
      f.toPath
    }) { path =>
      IO.blocking {
        val _ = path.toFile.delete
        ()
      }
    }

    tempRes.use(fn).unsafeRunSync()
  }

  property("binary stdout preserves every byte without adding text formatting") {
    forAll { (values: List[Byte]) =>
      val output = new ByteArrayOutputStream()
      val stream = new PrintStream(output)
      val platform = new IOPlatformIO(CompilerBuildInfo.constant) {
        override protected def stdout: PrintStream = stream
      }
      try {
        platform.writeStdoutBytes(Bytes.copyFromArray(values.toArray)).unsafeRunSync()
        assertEquals(output.toByteArray.toList, values)
      } finally stream.close()
    }
  }

  property("binary stdin is read as exact bytes") {
    forAll { (values: List[Byte]) =>
      val input = new ByteArrayInputStream(values.toArray)
      val platform = new IOPlatformIO(CompilerBuildInfo.constant) {
        override protected def stdin: InputStream = input
      }
      assertEquals(platform.readStdinBytes.unsafeRunSync().copyToArray.toList, values)
    }
  }

  test("file-key Hash preserves native identity across reads and aliases") {
    withTempDir { root =>
      val original = Files.writeString(root.resolve("original"), "contents")
      val other = Files.writeString(root.resolve("other"), "contents")
      val hard = Files.createLink(root.resolve("hard"), original)
      val symbolic = Files.createSymbolicLink(root.resolve("symbolic"), original)
      val paths = List(original, original, hard, symbolic, other)
      val keys = paths.traverse(IOPlatformIO.existingFileKey).unsafeRunSync().flatten
      assertEquals(keys.size, paths.size)
      // This fixture must exercise native identities. A resolved-path fallback
      // alone cannot equate distinct hard-link names.
      assert(keys.forall(_.isRight))
      given Hash[IOPlatformIO.FileKey] = IOPlatformIO.fileKeyHash
      val identity = Hash[IOPlatformIO.FileKey]
      keys.take(4).foreach { key =>
        assert(identity.eqv(key, keys.head))
        assertEquals(identity.hash(key), identity.hash(keys.head))
      }
      assert(!identity.eqv(keys.head, keys.last))
      val index = HashMap.fromSeq(keys.map(_ -> ()))
      assertEquals(index.size, 2)
    }
  }

  test("we can roundtrip interfaces through file") {
    forAll(Generators.smallDistinctByList(Generators.interfaceGen)(_.name)) {
      ifaces =>
        testWithTempFile { path =>
          for {
            _ <- IOPlatformIO.writeInterfaces(ifaces, path)
            ifaces1 <- IOPlatformIO.readInterfaces(path :: Nil)
            _ = assert(sortedEq.eqv(ifaces, ifaces1))
          } yield ()
        }
    }
  }

  test("we can roundtrip packages through proto on disk") {
    forAll(Generators.genPackage(genRegion, 3)) { packMap =>
      val packList = packMap.toList.sortBy(_._1).map(_._2)
      testWithTempFile { path =>
        for {
          _ <- IOPlatformIO.writePackages(packList, path)
          packList1 <- IOPlatformIO.readPackages(path :: Nil)
          psort = packList1.sortBy(_.name)
          _ = assertEquals(psort, packList)
        } yield ()
      }
    }
  }

  test("whole package readers retain and validate scoped bundle requirements") {
    val local = proto.Packages(
      packages = List(proto.Package(strings = List("Demo"), packageName = 1)),
      buildRequirements = Some(proto.BuildRequirements(
        scopedVersion = 1,
        members = List(proto.MemberBuildRequirements(memberName = "Demo"))
      ))
    )
    val imported = proto.Packages(
      packages = List(proto.Package(
        strings = List("Client", "External"),
        packageName = 1,
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

    testWithTempFile { path =>
      for {
        _ <- IOPlatformIO.writePackageBundle(local, path)
        localPackages <- IOPlatformIO.readPackages(List(path))
        _ = assertEquals(localPackages.map(_.name.asString), List("Demo"))
        _ <- IOPlatformIO.writePackageBundle(imported, path)
        rejected <- IOPlatformIO.readPackages(List(path)).attempt
        msg = rejected.swap.toOption.getOrElse(fail("accepted an imported scoped bundle")).getMessage
        _ = assert(msg.contains("unavailable imported provider context"), msg)
        malformed <- IOPlatformIO.writePackageBundle(
          local.copy(buildRequirements = Some(proto.BuildRequirements(scopedVersion = 2))), path
        ) *> IOPlatformIO.readPackages(List(path)).attempt
        malformedMsg = malformed.swap.toOption.getOrElse(fail("accepted an unsupported requirement version")).getMessage
        _ = assert(malformedMsg.contains("unsupported scoped build requirements version 2"), malformedMsg)
      } yield ()
    }
  }

  test("relativize handles src and ./src under an absolute root") {
    val root = Files.createTempDirectory("bosatsu-io-platform").toAbsolutePath
    val srcAbs = IOPlatformIO.resolve(root, Paths.get("src"))
    val srcDotAbs = IOPlatformIO.resolve(root, Paths.get("./src"))

    val relSrc = IOPlatformIO.relativize(root, srcAbs)
    assert(relSrc.nonEmpty)
    assertEquals(
      IOPlatformIO.resolve(root, relSrc.get).normalize,
      srcAbs.normalize
    )

    val relDotSrc = IOPlatformIO.relativize(root, srcDotAbs)
    assert(relDotSrc.nonEmpty)
    assertEquals(
      IOPlatformIO.resolve(root, relDotSrc.get).normalize,
      srcDotAbs.normalize
    )
  }

  test("gitTopLevelFrom finds a checkout root with a .git directory") {
    withTempDir { root =>
      Files.createDirectories(root.resolve(".git"))
      val nested = Files.createDirectories(root.resolve("a/b"))

      assertEquals(
        IOPlatformIO.gitTopLevelFrom(nested).unsafeRunSync(),
        Some(root)
      )
    }
  }

  test("gitTopLevelFrom finds a checkout root with a .git worktree file") {
    withTempDir { root =>
      Files.writeString(root.resolve(".git"), "gitdir: /tmp/worktree\n")
      val nested = Files.createDirectories(root.resolve("a/b"))

      assertEquals(
        IOPlatformIO.gitTopLevelFrom(nested).unsafeRunSync(),
        Some(root)
      )
    }
  }

  test("gitTopLevelFrom stops at filesystem root when no .git entry exists") {
    withTempDir { root =>
      val nested = Files.createDirectories(root.resolve("a/b"))

      assertEquals(
        IOPlatformIO.gitTopLevelFrom(nested).unsafeRunSync(),
        None
      )
    }
  }
}
