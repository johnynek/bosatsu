package dev.bosatsu

import cats.Eq
import cats.implicits._
import cats.effect.{IO, Resource}
import java.io.File
import java.nio.file.{Files, Path, Paths}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class IOPlatformIOTest extends munit.ScalaCheckSuite {
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

    // allow us to unsafeRunSync
    import cats.effect.unsafe.implicits.global
    tempRes.use(fn).unsafeRunSync()
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
    forAll(Generators.genPackage(Gen.const(()), 3)) { packMap =>
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

}
