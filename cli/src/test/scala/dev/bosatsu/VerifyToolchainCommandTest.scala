package dev.bosatsu

import cats.effect.IO
import dev.bosatsu.hashing.Algo
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

class VerifyToolchainCommandTest extends FileCommandFixtures {
  private val compilerBytes = "candidate compiler jar".getBytes(UTF_8)
  private val sourceOne =
    "package Example/One\nexport value\nvalue = 1\n".getBytes(UTF_8)
  private val sourceTwo =
    "package Example/Two\nexport value\nvalue = 2\n".getBytes(UTF_8)

  private def sha(bytes: Array[Byte]): String =
    Algo.hashBytes[Algo.Sha256](bytes).hex

  private def manifest(
      compiler: Array[Byte] = compilerBytes,
      sources: List[(String, Array[Byte])] = Nil
  ): String = {
    val sourceEntries = sources.map { case (name, bytes) =>
      s"\"$name\":\"${sha(bytes)}\""
    }.mkString(",")
    val sourceField =
      if (sources.isEmpty) ""
      else s",\n\"core_sources\":{$sourceEntries}"
    val emptyHash = sha(Array.emptyByteArray)
    s"""{
       |"schema_version":1,
       |"compiler":{"uri":"unused","sha256":"${sha(compiler)}"},
       |"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptyHash"},
       |"c_runtime":{"uri":"unused","sha256":"$emptyHash"}$sourceField
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

  private def args(out: String = "stamp"): List[String] =
    List(
      "verify-toolchain",
      "--manifest", "manifest.json",
      "--compiler-jar", "compiler.jar",
      "--out", out
    )

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
