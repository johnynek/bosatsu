package dev.bosatsu

import cats.Eq
import cats.implicits._
import cats.effect.{IO, Resource}
import java.io.File
import java.nio.file.{Files, Path, Paths}
import dev.bosatsu.hashing.Algo
import dev.bosatsu.IorMethods.IorExtension
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class IOPlatformIOTest extends munit.ScalaCheckSuite {
  private def stripTypedProvenance[A](pack: Package.Typed[A]): Package.Typed[Unit] = {
    val noExprTags = Package.typedFunctor.map(pack)(_ => ())
    Package.setProgramFrom(noExprTags, ())
  }

  private def typeCheckOne(src: String, packName: PackageName): Package.Typed[Declaration] =
    Par.noParallelism {
      val parsed = Parser.unsafeParse(Package.parser(None), src)
      val nel = cats.data.NonEmptyList.one((("test", LocationMap(src)), parsed))
      val pm = PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
      pm.toMap(packName)
    }

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
          psort = packList1.sortBy(_.name).map(stripTypedProvenance(_))
          expected = packList.map(stripTypedProvenance(_))
          _ = assertEquals(psort, expected)
        } yield ()
      }
    }
  }

  test("package file roundtrip preserves source hash and expression regions") {
    val packName = PackageName.parts("IO", "Regions")
    val src =
      """package IO/Regions
        |
        |export one
        |
        |one = 1
        |""".stripMargin
    val typed = typeCheckOne(src, packName)
    val hashIdent =
      Algo.hashBytes[Algo.Blake3]("io-platform-source".getBytes("UTF-8"))
        .toIdent(using Algo.blake3Algo)
    val withHash = Package.withSourceHashIdent(typed, Some(hashIdent))

    testWithTempFile { path =>
      for {
        _ <- IOPlatformIO.writePackages(withHash :: Nil, path)
        decoded <- IOPlatformIO.readPackages(path :: Nil)
        pack = decoded.find(_.name == packName).getOrElse(
          fail(s"missing decoded package: ${packName.asString}")
        )
        _ = assertEquals(Package.sourceHashIdent(pack), Some(hashIdent))
        _ = assert(
          pack.lets.exists { case (_, _, te) => te.tag.isInstanceOf[Region] },
          "expected at least one decoded let expression to carry Region metadata"
        )
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

}
