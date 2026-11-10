package dev.bosatsu

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import dev.bosatsu.tool.ExitCode
import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/** Run commands with the same relative paths that appear in build actions. */
private[bosatsu] trait FileCommandFixtures extends munit.FunSuite {
  override def munitTimeout: Duration = 6.minutes

  protected def directory: Resource[IO, Path] =
    Resource.make(
      IO.blocking(Files.createTempDirectory("bosatsu-file-command-"))
    )(
      IOPlatformIO.deleteRecursively
    )

  protected def withRunner(test: (Path, FileCommandRunner) => IO[Unit]): Unit =
    directory
      .use { root =>
        FileCommandRunner.resource.use(test(root, _))
      }
      .unsafeRunSync()

  protected def write(root: Path, relative: String, content: String): IO[Unit] =
    IO.blocking {
      val path = root.resolve(relative)
      Files.createDirectories(path.getParent)
      Files.writeString(path, content)
      ()
    }

  protected def read(root: Path, relative: String): IO[String] =
    IO.blocking(Files.readString(root.resolve(relative)))

  protected def listPaths(root: Path): List[String] = {
    val walk = Files.walk(root)
    try {
      import scala.jdk.CollectionConverters._
      walk.iterator().asScala.map(root.relativize(_).toString).toList.sorted
    } finally walk.close()
  }

  protected def success(result: FileCommandRunner.Result): Unit =
    assertEquals(result.exitCode, ExitCode.Success, result.stderr)

  protected def failure(result: FileCommandRunner.Result): Unit =
    assertEquals(result.exitCode, ExitCode.Error, result.toString)
}
