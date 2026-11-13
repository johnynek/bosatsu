package dev.bosatsu

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.tool.ExitCode
import fs2.io.file.{Files, Path}
import munit.FunSuite
import org.typelevel.paiges.Doc
import scala.scalajs.js
import _root_.bosatsu.{TypedAst => proto}

class Fs2PlatformIOTest extends FunSuite {
  test("verify-toolchain runs portably and preserves a stamp after validation failure") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: String*): IO[ExitCode] =
      IO.fromEither(
        module.run("tool" :: args.toList).leftMap(h => new IllegalArgumentException(h.toString))
      )
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val manifest = dir / "manifest.json"
      val compiler = dir / "compiler.jar"
      val stamp = dir / "stamp"
      val compilerBytes = "node candidate jar".getBytes("UTF-8")
      def sha(bytes: Array[Byte]): String = Algo.hashBytes[Algo.Sha256](bytes).hex
      val emptyHash = sha(Array.emptyByteArray)
      val manifestBytes =
        s"""{
           |"schema_version":1,
           |"compiler":{"uri":"unused","sha256":"${sha(compilerBytes)}"},
           |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptyHash"},
           |"c_runtime":{"uri":"unused","sha256":"$emptyHash"}
           |}""".stripMargin.getBytes("UTF-8")
      def verify: IO[ExitCode] = run(
        "verify-toolchain",
        "--manifest", manifest.toString,
        "--compiler-jar", compiler.toString,
        "--out", stamp.toString
      )
      for {
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes)
        _ <- Fs2PlatformIO.writeBytes(compiler, compilerBytes)
        valid <- verify.attempt
        _ = assertEquals(valid.toOption, Some(ExitCode.Success))
        stampContent <- Fs2PlatformIO.readUtf8(stamp)
        _ = assertEquals(stampContent, "ok")
        _ <- Fs2PlatformIO.writeDoc(stamp, Doc.text("sentinel"))
        _ <- Fs2PlatformIO.writeBytes(compiler, "altered candidate".getBytes("UTF-8"))
        invalid <- verify.attempt
        _ = assert(
          invalid.swap.toOption.exists(_.getMessage.contains("compiler jar SHA-256 mismatch")),
          invalid
        )
        previous <- Fs2PlatformIO.readUtf8(stamp)
        _ = assertEquals(previous, "sentinel")
      } yield ()
    }.unsafeToFuture()
  }

  test("verify-toolchain rejects stamp aliases for every requested component") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: List[String]): IO[ExitCode] =
      IO.fromEither(module.run("tool" :: args).leftMap(h => new IllegalArgumentException(h.toString)))
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val manifest = dir / "manifest.json"
      val compiler = dir / "compiler.jar"
      val firstSource = dir / "One.bosatsu"
      val secondSource = dir / "Two.bosatsu"
      val core = dir / "core.lib"
      val runtime = dir / "runtime.tar.gz"
      val compilerBytes = "node candidate jar".getBytes("UTF-8")
      val firstBytes = "package Example/One\n\nvalue = 1\n".getBytes("UTF-8")
      val secondBytes = "package Example/Two\n\nvalue = 2\n".getBytes("UTF-8")
      val coreBytes = proto.Library(
        name = "bosatsu",
        repoUri = "unused",
        descriptor = Some(proto.LibDescriptor(
          version = Some(Version.parser.parseAll("1.2.3").toOption.get.toProto)
        ))
      ).toByteArray
      val runtimeBytes = "node runtime archive".getBytes("UTF-8")
      def sha(bytes: Array[Byte]): String = Algo.hashBytes[Algo.Sha256](bytes).hex
      def manifestBytes(coreBytes: Array[Byte]): Array[Byte] = {
        val sourcePins =
          s"\"Example/One\":\"${sha(firstBytes)}\",\"Example/Two\":\"${sha(secondBytes)}\""
        s"""{
           |"schema_version":1,
           |"compiler":{"uri":"unused","sha256":"${sha(compilerBytes)}"},
           |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"${sha(coreBytes)}"},
           |"c_runtime":{"uri":"unused","sha256":"${sha(runtimeBytes)}"},
           |"core_sources":{$sourcePins}
           |}""".stripMargin.getBytes("UTF-8")
      }
      def verifyArgs(
          out: Path,
          binaryCore: Option[Path] = None,
          sources: List[Path] = Nil,
          archive: Option[Path] = None
      ): List[String] =
        List(
          "verify-toolchain",
          "--manifest", manifest.toString,
          "--compiler-jar", compiler.toString,
          "--out", out.toString
        ) ++ binaryCore.toList.flatMap(path => List("--core-library", path.toString)) ++
          sources.flatMap(path => List("--core-source", path.toString)) ++
          archive.toList.flatMap(path => List("--runtime-archive", path.toString))
      def rejectedAlias(out: Path, requested: List[Path], command: List[String]): IO[Unit] = {
        val paths = (requested :+ out).distinct
        for {
          before <- paths.traverse(path => Fs2PlatformIO.readBytes(path).map(path.toString -> _.toList)).map(_.toMap)
          result <- run(command).attempt
          _ = assert(
            result.swap.toOption.exists(_.getMessage.contains("refers to requested input")),
            result
          )
          after <- paths.traverse(path => Fs2PlatformIO.readBytes(path).map(path.toString -> _.toList)).map(_.toMap)
          _ = assertEquals(after, before)
        } yield ()
      }
      val requestedSources = List(firstSource, secondSource)
      val sourceAndRuntimeInputs = List(manifest, compiler, firstSource, secondSource, runtime)
      for {
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes(coreBytes))
        _ <- Fs2PlatformIO.writeBytes(compiler, compilerBytes)
        _ <- Fs2PlatformIO.writeBytes(firstSource, firstBytes)
        _ <- Fs2PlatformIO.writeBytes(secondSource, secondBytes)
        _ <- Fs2PlatformIO.writeBytes(runtime, runtimeBytes)
        _ <- sourceAndRuntimeInputs.traverse_(path =>
          rejectedAlias(path, sourceAndRuntimeInputs, verifyArgs(
            path,
            sources = requestedSources,
            archive = Some(runtime)
          ))
        )
        link = dir / "compiler-link.jar"
        _ <- fs.createSymbolicLink(link, compiler)
        _ <- rejectedAlias(
          link,
          sourceAndRuntimeInputs,
          verifyArgs(link, sources = requestedSources, archive = Some(runtime))
        )
        _ <- Fs2PlatformIO.writeBytes(core, coreBytes)
        _ <- Fs2PlatformIO.writeBytes(manifest, manifestBytes(coreBytes))
        _ <- rejectedAlias(core, List(manifest, compiler, core), verifyArgs(core, binaryCore = Some(core)))
      } yield ()
    }.unsafeToFuture()
  }

  test("emit-c resolves pending output aliases and creates the declared header directory") {
    val fs = Files.forIO
    val module = new MainModule(Fs2PlatformIO)
    def run(args: String*): IO[ExitCode] =
      IO.fromEither(module.run("tool" :: args.toList).leftMap(h => new IllegalArgumentException(h.toString)))
        .flatten.flatMap(_.report(Fs2PlatformIO))
    fs.tempDirectory.use { dir =>
      val source = dir / "Native.bosatsu"
      val bundle = dir / "native.pkg"
      val manifest = dir / "runtime.json"
      val c = dir / "runner.c"
      val header = dir / "bosatsu_ext_Native.h"
      val target = dir / "pending"
      def emit(out: Path, h: Path): IO[ExitCode] = run("emit-c", "--runtime-manifest", manifest.toString,
        "--test-packages", bundle.toString, "--out", out.toString, "--extern-header-out", h.toString)
      val graph = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"), "kind" -> Json.JString("local"),
        "packages" -> Json.JArray(Vector(Json.JString(bundle.toString))),
        "externals" -> Json.JArray(Vector(Json.JObject(List(
          "package" -> Json.JString("Native"), "symbols" -> Json.JArray(Vector(Json.JString("native_value")))
        ))))
      ))
      for {
        _ <- Fs2PlatformIO.writeDoc(source, Doc.text("package Native\nexternal def native_value(x: Int) -> Int\n" +
          "checks = Assertion(native_value(1) matches 1, \"ok\")\n"))
        _ <- run("compile-packages", "--src", source.toString, "--packages-out", bundle.toString,
          "--sig-out", (dir / "native.sig").toString)
        _ <- Fs2PlatformIO.writeDoc(manifest, graph.toDoc)
        _ <- fs.createSymbolicLink(c, target)
        _ <- fs.createSymbolicLink(header, target)
        bad <- emit(c, header).attempt
        _ = assert(bad.swap.exists(_.getMessage.contains("same file")), bad)
        created <- fs.exists(target)
        _ = assert(!created)
        caseDir = dir / "new"
        caseHeader = caseDir / "bosatsu_ext_Native.h"
        upper = caseDir / "BOSATSU_EXT_NATIVE.H"
        insensitive <- fs.exists(dir / "NATIVE.PKG")
        collision <- emit(upper, caseHeader).attempt
        _ = if (insensitive) assert(collision.isLeft, collision)
            else assertEquals(collision.toOption, Some(ExitCode.Success))
        present <- fs.exists(caseHeader)
        _ = assertEquals(present, !insensitive)
        nested = dir / "inc" / "bosatsu_ext_Native.h"
        good <- emit(dir / "good.c", nested)
        _ = assertEquals(good, ExitCode.Success)
        content <- Fs2PlatformIO.readUtf8(nested)
        _ = assert(content.contains("___bsts_g_Native_l_native__value("), content)
      } yield ()
    }.unsafeToFuture()
  }

  test("relativize handles src and ./src under an absolute root") {
    val root = Path(js.Dynamic.global.process.cwd().asInstanceOf[String])
    val srcAbs = Fs2PlatformIO.resolve(root, Path("src"))
    val srcDotAbs = Fs2PlatformIO.resolve(root, Path("./src"))

    val relSrc = Fs2PlatformIO.relativize(root, srcAbs)
    assert(relSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relSrc.get), srcAbs)

    val relDotSrc = Fs2PlatformIO.relativize(root, srcDotAbs)
    assert(relDotSrc.nonEmpty)
    assertEquals(Fs2PlatformIO.resolve(root, relDotSrc.get), srcDotAbs)
  }
}
