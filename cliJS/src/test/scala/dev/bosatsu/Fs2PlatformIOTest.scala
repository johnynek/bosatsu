package dev.bosatsu

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import dev.bosatsu.tool.ExitCode
import fs2.io.file.{Files, Path}
import munit.FunSuite
import org.typelevel.paiges.Doc
import scala.scalajs.js

class Fs2PlatformIOTest extends FunSuite {
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
