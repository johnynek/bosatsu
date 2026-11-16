package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.effect.IO
import cats.syntax.all._
import dev.bosatsu.hashing.Algo
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters._

class InspectLibraryCommandTest extends FileCommandFixtures {
  private val fixtureDirectory = Paths.get("test_workspace/inspect_library")
  private val fixtureRelative = "test_workspace/inspect_library"
  private val rootLibrary = s"$fixtureRelative/parser-v1.0.0.bosatsu_lib"
  private val generatedOutputs = List(
    "parser.sig",
    "exposures.json",
    "membership.json",
    "implementation.json",
    "descriptor.json"
  )

  private def copyFixtures(root: Path): IO[Unit] = IO.blocking {
    val files = Files.list(fixtureDirectory)
    try
      files.iterator().asScala
        .filter(path => path.getFileName.toString != "README.md")
        .foreach { source =>
          val destination = root.resolve(fixtureRelative).resolve(source.getFileName)
          Files.createDirectories(destination.getParent)
          Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
        }
    finally files.close()
  }

  private def readBytes(root: Path, relative: String): IO[List[Byte]] =
    IO.blocking(Files.readAllBytes(root.resolve(relative)).toList)

  private def expectedHashes(root: Path): IO[List[String]] =
    readBytes(root, rootLibrary).map(bytes => hashes(bytes.toArray))

  private def hashes(bytes: Array[Byte]): List[String] = List(
    s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
    s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
  )

  private def args(hashes: List[String]): List[String] =
    List(
      "inspect-library",
      "--input", rootLibrary,
      "--expected-name", "parser",
      "--expected-version", "1.0.0"
    ) ++ hashes.flatMap(hash => List("--expected-hash", hash)) ++ List(
      "--dependency-context", s"$fixtureRelative/parser-context.json",
      "--sig-out", "parser.sig",
      "--exposures-out", "exposures.json",
      "--membership-out", "membership.json",
      "--implementation-out", "implementation.json",
      "--descriptor-out", "descriptor.json"
    )

  private def parameterInvocation(
      runner: FileCommandRunner,
      root: Path,
      arguments: List[String]
  ): IO[FileCommandRunner.Result] =
    write(root, "inspect.params", arguments.mkString("\n") + "\n") *>
      BazelWorker.readParameterFile(root, "@inspect.params").flatMap { invocation =>
        runner.runWorker(root, invocation.commandArguments)
      }

  test("inspection uses the same registered operation for direct, worker, and parameter dispatch") {
    withRunner { (root, runner) =>
      for {
        _ <- copyFixtures(root)
        hashes <- expectedHashes(root)
        command = args(hashes)
        direct <- runner.run(root, command)
        _ = success(direct)
        expected <- generatedOutputs.traverse(name => readBytes(root, name))
        worker <- runner.runWorker(root, command)
        _ = success(worker)
        fromWorker <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(fromWorker, expected)
        fromParams <- parameterInvocation(runner, root, command)
        _ = success(fromParams)
        fromParameterFile <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(fromParameterFile, expected)
        _ = assert(FileCommandRunner.supports("inspect-library"))
        _ = assert(BazelWorker.Operation.fromCommandName("inspect-library").nonEmpty)
      } yield ()
    }
  }

  test("verification and publication failures preserve every prior requested output") {
    withRunner { (root, runner) =>
      for {
        _ <- copyFixtures(root)
        hashes <- expectedHashes(root)
        command = args(hashes)
        initial <- runner.run(root, command)
        _ = success(initial)
        before <- generatedOutputs.traverse(name => readBytes(root, name))
        _ <- write(root, s"$fixtureRelative/common-v1.0.0.bosatsu_lib", "tampered artifact")
        invalid <- runner.run(root, command)
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains("pin mismatch"), invalid.stderr)
        afterVerificationFailure <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(afterVerificationFailure, before)
        _ <- copyFixtures(root)
        _ <- IO.blocking(Files.createDirectories(root.resolve("out/blocked-directory")))
        publicationArgs = command.updated(
          command.indexOf("--descriptor-out") + 1,
          "out/blocked-directory"
        )
        publicationFailure <- runner.run(root, publicationArgs)
        _ = failure(publicationFailure)
        _ = assert(publicationFailure.stderr.contains("regular output file"), publicationFailure.stderr)
        afterPublicationFailure <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(afterPublicationFailure, before)
        _ = assert(!listPaths(root).exists(_.contains(".bosatsu-stage-")))
      } yield ()
    }
  }

  test("malformed serialized identities preserve every prior requested output") {
    withRunner { (root, runner) =>
      for {
        _ <- copyFixtures(root)
        originalHashes <- expectedHashes(root)
        initial <- runner.run(root, args(originalHashes))
        _ = success(initial)
        before <- generatedOutputs.traverse(name => readBytes(root, name))
        invalidVersion = proto.Version(
          major = 1L,
          minor = 0L,
          patch = 0L,
          preRelease = "alpha."
        )
        originalRoot <- readBytes(root, rootLibrary)
        parsedRoot = proto.Library.parseFrom(originalRoot.toArray)
        malformedTarget = parsedRoot.copy(
          publicDependencies = parsedRoot.publicDependencies.map { dependency =>
            dependency.copy(
              desc = dependency.desc.map(_.copy(version = Some(invalidVersion)))
            )
          }
        )
        targetBytes = malformedTarget.toByteArray
        _ <- IO.blocking(Files.write(root.resolve(rootLibrary), targetBytes))
        targetFailure <- runner.run(root, args(hashes(targetBytes)))
        _ = failure(targetFailure)
        _ = assert(targetFailure.stderr.contains("invalid version"), targetFailure.stderr)
        afterTargetFailure <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(afterTargetFailure, before)
        _ <- copyFixtures(root)
        restoredRoot <- readBytes(root, rootLibrary)
        parsedRestoredRoot = proto.Library.parseFrom(restoredRoot.toArray)
        malformedRoot = parsedRestoredRoot.copy(
          descriptor = parsedRestoredRoot.descriptor.map(_.copy(version = Some(invalidVersion)))
        )
        rootBytes = malformedRoot.toByteArray
        _ <- IO.blocking(Files.write(root.resolve(rootLibrary), rootBytes))
        rootFailure <- runner.run(root, args(hashes(rootBytes)))
        _ = failure(rootFailure)
        _ = assert(rootFailure.stderr.contains("invalid version"), rootFailure.stderr)
        afterRootFailure <- generatedOutputs.traverse(name => readBytes(root, name))
        _ = assertEquals(afterRootFailure, before)
      } yield ()
    }
  }

  test("inspection rejects outputs that alias the root, context, or dependency files") {
    withRunner { (root, runner) =>
      for {
        _ <- copyFixtures(root)
        hashes <- expectedHashes(root)
        command = args(hashes)
        protectedInputs = List(
          rootLibrary,
          s"$fixtureRelative/parser-context.json",
          s"$fixtureRelative/common-v1.0.0.bosatsu_lib"
        )
        _ <- protectedInputs.traverse_ { input =>
          val index = command.indexOf("--descriptor-out") + 1
          val alias = command.updated(index, input)
          runner.run(root, alias).map { result =>
            failure(result)
            assert(result.stderr.contains("refers to runtime input"), result.stderr)
          }
        }
        duplicate = command.updated(command.indexOf("--descriptor-out") + 1, "parser.sig")
        duplicateResult <- runner.run(root, duplicate)
        _ = failure(duplicateResult)
        _ = assert(duplicateResult.stderr.contains("multiple outputs refer"), duplicateResult.stderr)
      } yield ()
    }
  }
}
