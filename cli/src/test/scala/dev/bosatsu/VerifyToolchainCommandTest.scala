package dev.bosatsu

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.tool.{ExitCode, Output}
import org.typelevel.paiges.Doc
import _root_.bosatsu.{TypedAst => proto}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

class VerifyToolchainCommandTest extends FileCommandFixtures {
  test("atomic document reporting uses staged publication without a file runner") {
    directory.use { root =>
      val platform = new IOPlatformIO
      val target = root.resolve("stamp")
      val blocked = root.resolve("directory")
      for {
        _ <- write(root, "stamp", "sentinel")
        result <- Output.AtomicDoc(Doc.text("ok"), target).report(platform)
        _ = assertEquals(result, ExitCode.Success)
        content <- read(root, "stamp")
        _ = assertEquals(content, "ok")
        _ <- IO.blocking { val _ = Files.createDirectory(blocked); () }
        failed <- Output.AtomicDoc(Doc.text("ok"), blocked).report(platform).attempt
        _ = assert(failed.isLeft, failed)
        _ = assert(Files.isDirectory(blocked))
        _ = assertEquals(listPaths(root), List("", "directory", "stamp"))
      } yield ()
    }.unsafeRunSync()
  }

  private val compilerBytes = "candidate compiler jar".getBytes(UTF_8)
  private val sourceOne =
    "package Example/One\nexport value\nvalue = 1\n".getBytes(UTF_8)
  private val sourceTwo =
    "package Example/Two\nexport value\nvalue = 2\n".getBytes(UTF_8)
  private val coreBytes = proto.Library(
    name = "bosatsu",
    repoUri = "unused",
    descriptor = Some(proto.LibDescriptor(
      version = Some(Version.parser.parseAll("1.2.3").toOption.get.toProto)
    ))
  ).toByteArray
  private val runtimeBytes = "runtime archive".getBytes(UTF_8)

  private def sha(bytes: Array[Byte]): String =
    Algo.hashBytes[Algo.Sha256](bytes).hex

  private def manifest(
      compiler: Array[Byte] = compilerBytes,
      core: Array[Byte] = Array.emptyByteArray,
      runtime: Array[Byte] = Array.emptyByteArray,
      sources: List[(String, Array[Byte])] = Nil
  ): String = {
    val sourceEntries = sources.map { case (name, bytes) =>
      s"\"$name\":\"${sha(bytes)}\""
    }.mkString(",")
    val sourceField =
      if (sources.isEmpty) ""
      else s",\n\"core_sources\":{$sourceEntries}"
    s"""{
       |"schema_version":1,
       |"compiler":{"uri":"unused","sha256":"${sha(compiler)}"},
       |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"${sha(core)}"},
       |"c_runtime":{"uri":"unused","sha256":"${sha(runtime)}"}$sourceField
       |}""".stripMargin
  }

  private def writeBytes(root: Path, relative: String, bytes: Array[Byte]): IO[Unit] =
    IO.blocking {
      val path = root.resolve(relative)
      Files.write(path, bytes)
      ()
    }

  private def baseFiles(root: Path): IO[Unit] =
    writeBytes(root, "manifest.json", manifest().getBytes(UTF_8)) *>
      writeBytes(root, "compiler.jar", compilerBytes)

  private def args(
      out: String = "stamp",
      manifestPath: String = "manifest.json",
      compilerPath: String = "compiler.jar",
      coreLibrary: Option[String] = None,
      coreSources: List[String] = Nil,
      runtimeArchive: Option[String] = None
  ): List[String] =
    List(
      "verify-toolchain",
      "--manifest", manifestPath,
      "--compiler-jar", compilerPath,
      "--out", out
    ) ++ coreLibrary.toList.flatMap(path => List("--core-library", path)) ++
      coreSources.flatMap(path => List("--core-source", path)) ++
      runtimeArchive.toList.flatMap(path => List("--runtime-archive", path))

  private def assertAliasRejected(
      root: Path,
      runner: FileCommandRunner,
      command: List[String],
      inputs: List[String]
  ): IO[Unit] =
    for {
      before <- IO.blocking(inputs.map(path => path -> Files.readAllBytes(root.resolve(path)).toList).toMap)
      result <- runner.run(root, command)
      _ = failure(result)
      _ = assert(result.stderr.contains("refers to requested input"), result.stderr)
      after <- IO.blocking(inputs.map(path => path -> Files.readAllBytes(root.resolve(path)).toList).toMap)
      _ = assertEquals(after, before)
    } yield ()

  test(
    "compiler-only verification writes a silent success stamp with unused files absent"
  ) {
    withRunner { (root, runner) =>
      for {
        _ <- baseFiles(root)
        result <- runner.run(root, args())
        _ = success(result)
        _ = assertEquals(result.stdout, "")
        _ = assertEquals(result.stderr, "")
        stamp <- read(root, "stamp")
        _ = assertEquals(stamp, "ok")
        _ = assert(!Files.exists(root.resolve("core.bosatsu_lib")))
        _ = assert(!Files.exists(root.resolve("runtime.tar.gz")))
      } yield ()
    }
  }

  test("repeatable core sources are checked and binary/source selection is rejected") {
    withRunner { (root, runner) =>
      val sourceArgs = args() ++ List(
        "--core-source", "One.bosatsu",
        "--core-source", "Two.bosatsu"
      )
      for {
        _ <- writeBytes(
          root,
          "manifest.json",
          manifest(sources = List(
            "Example/One" -> sourceOne,
            "Example/Two" -> sourceTwo
          )).getBytes(UTF_8)
        )
        _ <- writeBytes(root, "compiler.jar", compilerBytes)
        _ <- writeBytes(root, "One.bosatsu", sourceOne)
        _ <- writeBytes(root, "Two.bosatsu", sourceTwo)
        valid <- runner.run(root, sourceArgs)
        _ = success(valid)
        _ <- write(root, "stamp", "previous")
        incompatible <- runner.run(root, sourceArgs ++ List("--core-library", "missing.lib"))
        _ = failure(incompatible)
        _ = assert(
          incompatible.stderr.contains("choose either --core-library or --core-source"),
          incompatible.stderr
        )
        prior <- read(root, "stamp")
        _ = assertEquals(prior, "previous")
      } yield ()
    }
  }

  test("verification failures leave absent and existing stamps unchanged") {
    withRunner { (root, runner) =>
      for {
        _ <- baseFiles(root)
        _ <- writeBytes(root, "compiler.jar", "changed compiler".getBytes(UTF_8))
        absent <- runner.run(root, args("absent-stamp"))
        _ = failure(absent)
        _ = assert(absent.stderr.contains("compiler jar SHA-256 mismatch"), absent.stderr)
        _ = assert(!Files.exists(root.resolve("absent-stamp")))
        _ <- write(root, "stamp", "sentinel")
        existing <- runner.run(root, args())
        _ = failure(existing)
        preserved <- read(root, "stamp")
        _ = assertEquals(preserved, "sentinel")
      } yield ()
    }
  }

  test("verification stamps cannot alias any requested input") {
    withRunner { (root, runner) =>
      val manifestBytes = manifest(
        core = coreBytes,
        runtime = runtimeBytes,
        sources = List("Example/One" -> sourceOne, "Example/Two" -> sourceTwo)
      ).getBytes(UTF_8)
      for {
        _ <- writeBytes(root, "manifest.json", manifestBytes)
        _ <- writeBytes(root, "compiler.jar", compilerBytes)
        _ <- assertAliasRejected(root, runner, args(out = "manifest.json"), List("manifest.json", "compiler.jar"))
        _ <- assertAliasRejected(root, runner, args(out = "compiler.jar"), List("manifest.json", "compiler.jar"))
        _ <- assertAliasRejected(root, runner, args(out = "./compiler.jar"), List("manifest.json", "compiler.jar"))
        _ <- IO.blocking {
          Files.createSymbolicLink(root.resolve("compiler-link.jar"), Path.of("compiler.jar"))
          Files.createLink(root.resolve("compiler-hard.jar"), root.resolve("compiler.jar"))
        }
        _ <- assertAliasRejected(
          root,
          runner,
          args(out = "compiler-link.jar"),
          List("manifest.json", "compiler.jar", "compiler-link.jar")
        )
        _ <- assertAliasRejected(
          root,
          runner,
          args(out = "compiler-hard.jar"),
          List("manifest.json", "compiler.jar", "compiler-hard.jar")
        )
        _ <- writeBytes(root, "core.lib", coreBytes)
        _ <- assertAliasRejected(
          root,
          runner,
          args(out = "core.lib", coreLibrary = Some("core.lib")),
          List("manifest.json", "compiler.jar", "core.lib")
        )
        _ <- writeBytes(root, "One.bosatsu", sourceOne)
        _ <- writeBytes(root, "Two.bosatsu", sourceTwo)
        _ <- List("One.bosatsu", "Two.bosatsu").traverse_ { source =>
          assertAliasRejected(
            root,
            runner,
            args(out = source, coreSources = List("One.bosatsu", "Two.bosatsu")),
            List("manifest.json", "compiler.jar", "One.bosatsu", "Two.bosatsu")
          )
        }
        _ <- writeBytes(root, "runtime.tar.gz", runtimeBytes)
        _ <- assertAliasRejected(
          root,
          runner,
          args(out = "runtime.tar.gz", runtimeArchive = Some("runtime.tar.gz")),
          List("manifest.json", "compiler.jar", "runtime.tar.gz")
        )
      } yield ()
    }
  }

  test("failed publication is cleaned up and the same runner succeeds on retry") {
    withRunner { (root, runner) =>
      for {
        _ <- baseFiles(root)
        _ <- IO.blocking(Files.createDirectory(root.resolve("stamp")))
        rejected <- runner.run(root, args())
        _ = failure(rejected)
        _ = assert(
          rejected.stderr.contains("expected a regular output file"),
          rejected.stderr
        )
        _ = assert(Files.isDirectory(root.resolve("stamp")))
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
        _ <- IO.blocking(Files.delete(root.resolve("stamp")))
        retried <- runner.run(root, args())
        _ = success(retried)
        stamp <- read(root, "stamp")
        _ = assertEquals(stamp, "ok")
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test("required options are reported by the shared tool parser") {
    withRunner { (root, runner) =>
      runner.run(root, List("verify-toolchain")).map { result =>
        failure(result)
        assert(result.stderr.contains("--manifest"), result.stderr)
      }
    }
  }

  test("standalone and one-shot worker verification have identical results") {
    withRunner { (base, runner) =>
      val direct = base.resolve("direct")
      val worker = base.resolve("worker")
      val workerArgs = args()
      val workerParams = workerArgs.mkString("\n") + "\n"
      for {
        _ <- IO.blocking {
          Files.createDirectories(direct)
          Files.createDirectories(worker)
        }
        _ <- writeBytes(direct, "manifest.json", manifest().getBytes(UTF_8))
        _ <- writeBytes(direct, "compiler.jar", compilerBytes)
        _ <- writeBytes(worker, "manifest.json", manifest().getBytes(UTF_8))
        _ <- writeBytes(worker, "compiler.jar", compilerBytes)
        directResult <- runner.run(direct, workerArgs)
        _ <- write(worker, "worker.params", workerParams)
        invocation <- BazelWorker.readParameterFile(worker, "@worker.params")
        workerResult <- runner.runWorker(worker, invocation.commandArguments)
        _ = success(directResult)
        _ = assertEquals(workerResult, directResult)
        directStamp <- read(direct, "stamp")
        workerStamp <- read(worker, "stamp")
        _ = assertEquals(workerStamp, directStamp)
      } yield ()
    }
  }
}
