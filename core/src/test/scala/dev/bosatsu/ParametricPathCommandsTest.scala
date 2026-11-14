package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import cats.implicits._
import com.monovore.decline.Argument
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.tool.{LocalRuntime, LocalRuntimeManifest, Output}
import org.typelevel.paiges.Doc

class ParametricPathCommandsTest extends munit.FunSuite {
  private type Result[A] = Either[Throwable, A]
  private type F[A] = MemoryMain.StateF[Result][A]
  private val memory = MemoryMain.memoryPlatformIO[Result]

  // Each parse creates a fresh value. Only the platform's ordering and
  // rendering operations may inspect these paths, including on error paths.
  private final class Path(val parts: Chain[String]) {
    override def equals(that: Any): Boolean = throw new IllegalStateException("path equals")
    override def hashCode(): Int = throw new IllegalStateException("path hashCode")
    override def toString(): String = throw new IllegalStateException("path toString")
  }
  private def wrap(parts: Chain[String]): Path = new Path(parts)
  private def path(name: String): Path = wrap(Chain.one(name))

  // Adapt the in-memory filesystem without changing its effects or file keys.
  private val platform: PlatformIO[F, Path] = new PlatformIO[F, Path] {
    export memory.{moduleIOMonad, parallelF, canPromiseF, withEC, writeStdout, writeError,
      readStdinBytes, writeStdoutBytes, system, systemStdout, env, hostOs, hostArch,
      gitShaHead, println, errorln}
    type FileKey = memory.FileKey
    def fileKeyHash = memory.fileKeyHash
    def pathOrdering: Ordering[Path] = memory.pathOrdering.on(_.parts)
    val pathArg: Argument[Path] = new Argument[Path] {
      def defaultMetavar = "path"
      def read(value: String) = memory.path(value).map(wrap)
    }
    def pathToString(p: Path) = memory.pathToString(p.parts)
    def readUtf8(p: Path) = memory.readUtf8(p.parts)
    def readBytes(p: Path) = memory.readBytes(p.parts)
    def readPackages(paths: List[Path]) = memory.readPackages(paths.map(_.parts))
    def readLibrary(p: Path) = memory.readLibrary(p.parts)
    def fetchHash[A](algo: Algo[A], hash: HashValue[A], p: Path, uri: String) =
      memory.fetchHash(algo, hash, p.parts, uri)
    def fsDataType(p: Path) = memory.fsDataType(p.parts)
    def existingFileKey(p: Path) = memory.existingFileKey(p.parts)
    def pendingOutputPath(p: Path) = memory.pendingOutputPath(p.parts).map(wrap)
    def withTempDirectoryIn[A](parent: Path)(fn: Path => F[A]) =
      memory.withTempDirectoryIn(parent.parts)(p => fn(wrap(p)))
    def resolve(p: Path, child: String) = wrap(memory.resolve(p.parts, child))
    def resolve(p: Path, child: Path) = wrap(memory.resolve(p.parts, child.parts))
    def relativize(prefix: Path, deeper: Path) = memory.relativize(prefix.parts, deeper.parts).map(wrap)
    def parent(p: Path) = memory.parent(p.parts).map(wrap)
    def unfoldDir(p: Path) =
      memory.unfoldDir(p.parts).map(_.map(children => children.map(_.map(wrap))))
    def hasExtension(ext: String): Path => Boolean = p => memory.hasExtension(ext)(p.parts)
    def writeDoc(p: Path, doc: Doc) = memory.writeDoc(p.parts, doc)
    def writeDocAtomic(p: Path, doc: Doc) = memory.writeDocAtomic(p.parts, doc)
    def writeFileSetAtomic(files: List[(Path, Vector[Byte])]) =
      memory.writeFileSetAtomic(files.map { case (p, bytes) => p.parts -> bytes })
    def writeInterfaces(ifaces: List[Package.Interface], p: Path) = memory.writeInterfaces(ifaces, p.parts)
    def writeBytes(p: Path, bytes: Array[Byte]) = memory.writeBytes(p.parts, bytes)
    def writeLibrary(lib: proto.Library, p: Path) =
      memory.writeLibrary(lib, p.parts)
    def writePackages(packs: List[Package.Compiled], p: Path) = memory.writePackages(packs, p.parts)
    def withTempPrefix[A](prefix: String)(fn: Path => F[A]) =
      memory.withTempPrefix(prefix)(p => fn(wrap(p)))
  }

  private def checked[A](result: Result[A]): A =
    result.fold(err => fail(err.getMessage), identity)

  private lazy val fixture: MemoryMain.State = {
    val module = new MainModule(memory)
    val source = "package App\nchecks = Assertion(True, \"parametric paths\")\n"
    val initial = MemoryMain.State.empty
      .withFile(Chain.one("App.bosatsu"), MemoryMain.FileContent.Str(source)).get
      .withFile(Chain.one("runtime.json"), MemoryMain.FileContent.Str(
        """{"schema_version":1,"kind":"local","packages":["app.pkg","app.pkg"],"externals":[]}"""
      )).get
    val command = module.run(List("tool", "compile-packages", "--src", "App.bosatsu",
      "--sig-out", "app.sig", "--packages-out", "app.pkg")).fold(h => fail(h.toString), identity)
    checked(command.flatMap(_.report(memory)).runS(initial))
  }

  private def emit(selected: String, out: String, headers: List[String]): Result[MemoryMain.State] = {
    val args = List("tool", "emit-c", "--runtime-manifest", "runtime.json",
      "--test-packages", selected, "--out", out) ::: headers.flatMap(h => List("--extern-header-out", h))
    val command = new MainModule(platform).run(args).fold(h => fail(h.toString), identity)
    command.flatMap(_.report(platform)).runS(fixture)
  }

  private def rejects(result: Result[MemoryMain.State], message: String): Unit =
    result match {
      case Left(err) => assert(err.getMessage.contains(message), err.getMessage)
      case Right(_) => fail("accepted invalid emission paths")
    }

  test("runtime membership uses path ordering for independently parsed values") {
    val scope = checked(LocalRuntimeManifest.read(platform, path("runtime.json"))
      .flatMap(LocalRuntime.load(platform, _)).runA(fixture))
    assertEquals(scope.members(path("app.pkg")).toList.map(_.asString), List("App"))
    assertEquals(scope.members.size, 1)
  }

  test("test emission and output publication use platform path contracts") {
    val result = checked(emit("app.pkg", "out.c", Nil))
    val c = checked(memory.readUtf8(Chain.one("out.c")).runA(result))
    assert(c.contains("int main("), c)
  }

  test("atomic document reporting retains opaque paths through the in-memory platform") {
    val state = checked(Output.AtomicDoc(Doc.text("previous"), path("stamp"))
      .report(platform).runS(MemoryMain.State.empty))
    val rewritten = checked(Output.AtomicDoc(Doc.text("ok"), path("stamp"))
      .report(platform).runS(state))
    assertEquals(checked(platform.readUtf8(path("stamp")).runA(rewritten)), "ok")
  }

  test("atomic file-set reporting preserves parametric paths and publishes binary content") {
    val first = path("first.bin")
    val second = path("second.bin")
    val state = checked(
      Output.AtomicFileSet(List(first -> Vector[Byte](0, 1, -1), second -> Vector[Byte](2)))
        .report(platform).runS(MemoryMain.State.empty)
    )
    assertEquals(checked(platform.readBytes(first).runA(state)).toList, List[Byte](0, 1, -1))
    assertEquals(checked(platform.readBytes(second).runA(state)).toList, List[Byte](2))
  }

  test("a missing selector is compared and displayed through platform contracts") {
    rejects(emit("missing.pkg", "out.c", Nil), "test bundle missing.pkg is absent")
  }

  test("duplicate output arguments use path ordering") {
    rejects(emit("app.pkg", "out.c", List("out.c")), "distinct C and header output paths")
  }

  test("an output cannot replace a logically equal runtime input") {
    rejects(emit("app.pkg", "app.pkg", Nil), "output app.pkg is also a runtime input")
  }

  test("distinct prospective outputs reach header validation without universal hashing") {
    rejects(emit("app.pkg", "out.c", List("unknown.h")), "no declared external bindings generate header")
  }
}
