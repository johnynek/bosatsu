package dev.bosatsu

import cats.effect.{Deferred, IO}
import cats.syntax.all._
import dev.bosatsu.tool.{CliException, ExitCode}
import org.typelevel.paiges.Doc
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

class FileCommandRunnerTest extends FileCommandFixtures {
  private final class TrackingPrintStream(out: ByteArrayOutputStream)
      extends PrintStream(out, false, UTF_8) {
    var closed = false

    override def close(): Unit = {
      closed = true
      super.close()
    }
  }

  private val input = "src/Value.bosatsu"
  private val signature = "gen/Value.bosatsig"
  private val implementation = "gen/Value.bosatsu_package"

  private val check = List(
    "check",
    "--input",
    input,
    "--interface_out",
    signature,
    "--output",
    implementation
  )

  private def source(value: String): String =
    s"package Example/Value\n\nexport value\n\nvalue = $value\n"

  private def bytes(root: Path, name: String): IO[List[Byte]] =
    IO.blocking(Files.readAllBytes(root.resolve(name)).toList)

  test(
    "good, invalid, then edited input in the same JVM and at the same paths"
  ) {
    withRunner { (root, runner) =>
      // Re-evaluate the very same IO three times; a request is not a memo.
      val request = runner.run(root, check)
      for {
        _ <- write(root, input, source("1"))
        first <- request
        _ = success(first)
        _ = assertEquals(first.stderr, "")
        firstSig <- bytes(root, signature)
        firstImpl <- bytes(root, implementation)
        _ <- write(root, input, source("undefined_name"))
        invalid <- request
        _ = failure(invalid)
        _ = assert(
          invalid.stderr.contains(Path.of(input).toString),
          invalid.stderr
        )
        _ = assert(invalid.stderr.contains("undefined_name"), invalid.stderr)
        afterFailureSig <- bytes(root, signature)
        afterFailureImpl <- bytes(root, implementation)
        _ = assertEquals(afterFailureSig, firstSig)
        _ = assertEquals(afterFailureImpl, firstImpl)
        _ <- write(root, input, source("2"))
        last <- request
        _ = success(last)
        _ = assertEquals(last.stderr, "")
        lastSig <- bytes(root, signature)
        lastImpl <- bytes(root, implementation)
        _ = assertEquals(lastSig, firstSig)
        _ = assertNotEquals(lastImpl, firstImpl)
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test(
    "stable signatures retain their file timestamp after an implementation edit"
  ) {
    withRunner { (root, runner) =>
      val sigFile = root.resolve(signature)
      val past = FileTime.fromMillis(100000L)
      for {
        _ <- write(root, input, source("1"))
        first <- runner.run(root, check)
        _ = success(first)
        _ <- IO.blocking(Files.setLastModifiedTime(sigFile, past))
        originalTime <- IO.blocking(Files.getLastModifiedTime(sigFile))
        _ <- write(root, input, source("3"))
        second <- runner.run(root, check)
        _ = success(second)
        _ = assertEquals(Files.getLastModifiedTime(sigFile), originalTime)
        _ <- write(root, input, source("True"))
        third <- runner.run(root, check)
        _ = success(third)
        _ = assertNotEquals(Files.getLastModifiedTime(sigFile), originalTime)
      } yield ()
    }
  }

  test(
    "a reporting failure leaves existing outputs usable and a request can be retried"
  ) {
    withRunner { (root, runner) =>
      val blocked = root.resolve(implementation)
      for {
        _ <- write(root, input, source("1"))
        _ <- write(root, signature, "previous signature")
        _ <- IO.blocking(Files.createDirectory(blocked))
        rejected <- runner.run(root, check)
        _ = failure(rejected)
        _ = assert(
          rejected.stderr.contains("expected a regular output file"),
          rejected.stderr
        )
        original <- read(root, signature)
        _ = assertEquals(original, "previous signature")
        _ = assert(Files.isDirectory(blocked))
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
        _ <- IO.blocking(Files.delete(blocked))
        retried <- runner.run(root, check)
        _ = success(retried)
        ifaces <- IOPlatformIO.readInterfaces(List(root.resolve(signature)))
        packages <- IOPlatformIO.readPackages(List(blocked))
        _ = assertEquals(ifaces.map(_.name.asString), List("Example/Value"))
        _ = assertEquals(packages.map(_.name.asString), List("Example/Value"))
      } yield ()
    }
  }

  test("reject two artifact kinds targeting the same file") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, input, source("1"))
        _ <- write(root, signature, "previous")
        result <- runner.run(
          root,
          check.updated(check.size - 1, "gen/./Value.bosatsig")
        )
        _ = failure(result)
        _ = assert(result.stderr.contains("multiple outputs"), result.stderr)
        old <- read(root, signature)
        _ = assertEquals(old, "previous")
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test(
    "concurrent requests use their own directory, diagnostics, and pool lease"
  ) {
    withRunner { (root, runner) =>
      val a = root.resolve("a")
      val b = root.resolve("b")
      for {
        _ <- write(a, input, source("missing_only_in_a"))
        _ <- write(b, input, source("123"))
        both <- (runner.run(a, check), runner.run(b, check)).parTupled
        (bad, good) = both
        _ = failure(bad)
        _ = assert(bad.stderr.contains("missing_only_in_a"))
        _ = success(good)
        _ = assertEquals(good.stderr, "")
        _ = assert(!Files.exists(a.resolve(signature)))
        _ = assert(Files.isRegularFile(b.resolve(signature)))
        _ = assert(!Files.exists(root.resolve(signature)))
      } yield ()
    }
  }

  test(
    "request scratch is isolated below the working directory and cleaned on cancellation"
  ) {
    withRunner { (root, runner) =>
      val sink = new PrintStream(new ByteArrayOutputStream, false, UTF_8)
      runner.requestPlatform(root, sink, sink).use { platform =>
        for {
          live <- Deferred[IO, Path]
          fiber <- platform
            .withTempPrefix("request-") { scratch =>
              write(scratch, "partial", "unfinished") *>
                live.complete(scratch) *> IO.never[Unit]
            }
            .start
          scratch <- live.get
          _ = assertEquals(scratch.getParent, root)
          _ = assert(Files.exists(scratch.resolve("partial")))
          _ <- fiber.cancel
          _ = assert(!Files.exists(scratch))
          retried <- platform.withTempPrefix("request-")(IO.pure)
          _ = assertNotEquals(retried, scratch)
          _ = assert(!Files.exists(retried))
        } yield ()
      }
    }
  }

  test(
    "closing a request waits for its compiler promise fibers to finish cancelling"
  ) {
    withRunner { (root, runner) =>
      val sink = new PrintStream(new ByteArrayOutputStream, false, UTF_8)
      for {
        started <- Deferred[IO, Unit]
        stopped <- Deferred[IO, Unit]
        _ <- runner.requestPlatform(root, sink, sink).use { platform =>
          val promises = platform.canPromiseF
          val pending = promises.unsafeNewPromise[Unit]
          val work = (started.complete(()) *> IO.never[Unit])
            .onCancel(stopped.complete(()).void)
          promises.completeWith(pending, work) *> started.get
        }
        finished <- stopped.tryGet
        _ = assertEquals(finished, Some(()))
      } yield ()
    }
  }

  test("a second group compiles using a signature from the first") {
    withRunner { (root, runner) =>
      val use =
        """package Example/Use
          |
          |from Example/Value import value
          |export copy
          |
          |copy = value
          |""".stripMargin
      for {
        _ <- write(root, input, source("42"))
        first <- runner.run(root, check)
        _ = success(first)
        _ <- IO.blocking(Files.delete(root.resolve(implementation)))
        _ <- write(root, "src/Use.bosatsu", use)
        second <- runner.run(
          root,
          List(
            "check",
            "--input",
            "src/Use.bosatsu",
            "--interface",
            signature,
            "--output",
            "gen/Use.bosatsu_package"
          )
        )
        _ = success(second)
        _ = assert(Files.isRegularFile(root.resolve("gen/Use.bosatsu_package")))
      } yield ()
    }
  }

  test(
    "directory inputs stay relative to the request and diagnostics keep logical paths"
  ) {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "sources/nested/Bad.bosatsu", source("unknown_value"))
        result <- runner.run(
          root,
          List("check", "--input_all_subdir", "sources")
        )
        _ = failure(result)
        _ = assert(
          result.stderr.contains(
            Path.of("sources/nested/Bad.bosatsu").toString
          ),
          result.stderr
        )
        _ = assert(!result.stderr.contains(root.toString), result.stderr)
      } yield ()
    }
  }

  test("a binary read failure is contained; the next request still runs") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "broken.bosatsu_lib", "invalid protobuf")
        result <- runner.run(
          root,
          List("extract-iface", "--input", "broken.bosatsu_lib")
        )
        _ = failure(result)
        _ = assert(result.stderr.nonEmpty)
        _ = assert(!Files.exists(root.resolve("broken.bosatsu_ifacelib")))
        good <- runner.run(root, List("check", "--help"))
        _ = assert(good.stderr.contains("--interface_out"), good.stderr)
        _ = assert(!good.stderr.contains("protobuf"), good.stderr)
      } yield ()
    }
  }

  test(
    "parse errors and unsupported interactive commands return a status and diagnostic"
  ) {
    withRunner { (root, runner) =>
      for {
        parse <- runner.run(root, List("check", "--bad-flag"))
        _ = failure(parse)
        _ = assertEquals(parse.stdout, "")
        _ = assert(parse.stderr.contains("--bad-flag"), parse.stderr)
        interactive <- runner.run(root, List("protoc-plugin"))
        _ = failure(interactive)
        _ = assert(interactive.stderr.contains("expected a file command"))
        noCommand <- runner.run(root, Nil)
        _ = failure(noCommand)
        _ = assert(noCommand.stderr.contains("<none>"))
      } yield ()
    }
  }

  test("an overlong output path returns an error and the runner remains usable") {
    withRunner { (root, runner) =>
      assume(Files.getFileStore(root).supportsFileAttributeView("posix"))
      val headers = List("headers", "--src", input, "--out")
      for {
        _ <- write(root, input, source("1"))
        bad <- runner.run(root, headers :+ ("./" * 8000 + "out"))
        _ = failure(bad)
        _ = assert(bad.stderr.nonEmpty)
        good <- runner.run(root, headers :+ "index.json")
        _ = success(good)
        _ = assert(Files.isRegularFile(root.resolve("index.json")))
      } yield ()
    }
  }

  test("a deferred empty diagnostic writes zero stdout bytes") {
    withRunner { (root, runner) =>
      val out = new ByteArrayOutputStream
      val err = new ByteArrayOutputStream
      runner
        .requestPlatform(
          root,
          new PrintStream(out, false, UTF_8),
          new PrintStream(err, false, UTF_8)
        )
        .use { platform =>
          def report(stdout: Doc): IO[ExitCode] =
            CliException("failed", Doc.text("problem"), stdout, ExitCode.Error)
              .report(platform)

          for {
            code <- report(Doc.defer(Doc.empty))
            _ = assertEquals(code, ExitCode.Error)
            _ = assertEquals(out.size(), 0)
            _ = assertEquals(err.toString(UTF_8), "problem" + System.lineSeparator())
            _ <- report(Doc.defer(Doc.text("detail")))
            _ = assertEquals(out.toString(UTF_8), "detail" + System.lineSeparator())
          } yield ()
        }
    }
  }

  test("streaming and captured callers receive the same non-binary output") {
    withRunner { (root, runner) =>
      val args = List("deps", "--input", input)
      val stdout = new ByteArrayOutputStream
      val stderr = new ByteArrayOutputStream
      for {
        _ <- write(root, input, source("123"))
        captured <- runner.run(root, args)
        streamed <- runner.runTo(
          root,
          args,
          new PrintStream(stdout, false, UTF_8),
          new PrintStream(stderr, false, UTF_8)
        )
        _ = success(captured)
        _ = assertEquals(streamed, captured.exitCode)
        _ = assert(captured.stdout.contains("Example/Value"), captured.stdout)
        _ = assertEquals(stdout.toString(UTF_8), captured.stdout)
        _ = assertEquals(stderr.toString(UTF_8), captured.stderr)
      } yield ()
    }
  }

  test("streaming worker requests leave caller-owned streams open") {
    withRunner { (root, runner) =>
      val stdout = new TrackingPrintStream(new ByteArrayOutputStream)
      val stderr = new TrackingPrintStream(new ByteArrayOutputStream)
      for {
        _ <- write(root, input, source("123"))
        code <- runner.runWorkerTo(
          root,
          List("headers", "--src", input, "--out", "headers.json"),
          stdout,
          stderr
        )
        _ = assertEquals(code, ExitCode.Success)
        _ = assert(!stdout.closed)
        _ = assert(!stderr.closed)
        _ = assert(Files.isRegularFile(root.resolve("headers.json")))
        _ <- IO.blocking {
          stdout.close()
          stderr.close()
        }
      } yield ()
    }
  }
}
