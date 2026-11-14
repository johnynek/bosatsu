package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{LibraryInspectionFixtures, Name}
import dev.bosatsu.tool.ExitCode

class InspectLibraryCommandTest extends munit.FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type Path = Chain[String]

  private val module = MemoryMain[ErrorOr]
  private val fixtures = LibraryInspectionFixtures.parsedLibraries
  private val parser = fixtures.find(_._2.name == "parser").get

  private def path(raw: String): Path = Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private val fixtureState = LibraryInspectionFixtures.generatedFiles.foldLeft(
    MemoryMain.State.empty
  ) { case (state, file) =>
    state.withFile(path(file.path), MemoryMain.FileContent.Bytes(file.bytes)).get
  }

  private def args(output: String, descriptorOnly: Boolean): List[String] = {
    inspectionArgs(output, descriptorOnly, parser._4)
  }

  private def inspectionArgs(
      output: String,
      descriptorOnly: Boolean,
      hashes: List[String]
  ): List[String] = {
    val (rootPath, library, _, _) = parser
    List(
      "tool",
      "inspect-library",
      "--input",
      rootPath,
      "--expected-name",
      Name(library.name).name,
      "--expected-version",
      "1.0.0"
    ) ++ hashes.flatMap(hash => List("--expected-hash", hash)) ++
      (if (descriptorOnly) List("--descriptors-only")
       else List("--dependency-context", "test_workspace/inspect_library/parser-context.json")) ++
      List("--descriptor-out", output) ++
      (if (descriptorOnly) Nil else List(
        "--sig-out", "parser.sig",
        "--exposures-out", "exposures.json",
        "--membership-out", "membership.json",
        "--implementation-out", "implementation.json"
      ))
  }

  private def signatureOnlyArgs(hashes: List[String]): List[String] = {
    val (rootPath, library, _, _) = parser
    List(
      "tool",
      "inspect-library",
      "--input",
      rootPath,
      "--expected-name",
      Name(library.name).name,
      "--expected-version",
      "1.0.0"
    ) ++ hashes.flatMap(hash => List("--expected-hash", hash)) ++ List(
      "--dependency-context",
      "test_workspace/inspect_library/parser-context.json",
      "--sig-out",
      "signature-only.sig"
    )
  }

  private def pins(bytes: Array[Byte]): List[String] = List(
    s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
    s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
  )

  private def failed(result: ErrorOr[?]): String =
    result.swap.toOption.flatMap(error => Option(error.getMessage)).getOrElse(
      fail("expected inspection to reject malformed serialized version")
    )

  private def run(
      command: List[String],
      state: MemoryMain.State
  ): ErrorOr[(MemoryMain.State, ExitCode)] =
    module.run(command) match {
      case Left(help) => Left(new Exception(s"unexpected help: $help"))
      case Right(action) =>
        action.run(state).flatMap { case (next, output) =>
          module.reportOutput(output).run(next)
        }
    }

  private def text(state: MemoryMain.State, name: String): String =
    state.get(path(name)) match {
      case Some(Right(MemoryMain.FileContent.Bytes(value))) =>
        new String(value, java.nio.charset.StandardCharsets.UTF_8)
      case Some(Right(MemoryMain.FileContent.Str(value))) => value
      case other => fail(s"expected JSON output at $name, found $other")
    }

  private def bytes(state: MemoryMain.State, name: String): Array[Byte] =
    state.get(path(name)) match {
      case Some(Right(MemoryMain.FileContent.Bytes(value))) => value
      case other => fail(s"expected binary output at $name, found $other")
    }

  test("full inspection runs through the in-memory command and emits independent projections") {
    val result = run(args("descriptor.json", descriptorOnly = false), fixtureState)
      .fold(error => fail(error.getMessage), identity)
    assertEquals(result._2, ExitCode.Success)
    assert(proto.Interfaces.parseFrom(bytes(result._1, "parser.sig")).interfaces.nonEmpty)
    assert(text(result._1, "descriptor.json").contains("unused_transitive_public"))
    assert(text(result._1, "exposures.json").contains("witness_path"))
    assert(text(result._1, "membership.json").contains("Parser"))
    val implementation = text(result._1, "implementation.json")
    assert(implementation.contains("\"name\": \"shared\""), implementation)
    assert(implementation.contains("\"version\": \"2.0.0\""), implementation)
  }

  test("descriptor-only inspection authenticates the root without child files or typed decoding") {
    val (rootPath, _, bytes, _) = parser
    val rootOnly = MemoryMain.State.empty.withFile(
      path(rootPath),
      MemoryMain.FileContent.Bytes(bytes)
    ).get
    val result = run(args("root-descriptor.json", descriptorOnly = true), rootOnly)
      .fold(error => fail(error.getMessage), identity)
    assertEquals(result._2, ExitCode.Success)
    assert(text(result._1, "root-descriptor.json").contains("public_child"))
  }

  test("serialized root and dependency versions fail before descriptor or signature output") {
    val invalidVersion = proto.Version(
      major = 1L,
      minor = 0L,
      patch = 0L,
      preRelease = "alpha."
    )
    val malformedRoot = parser._2.copy(
      descriptor = parser._2.descriptor.map(_.copy(version = Some(invalidVersion)))
    )
    val malformedRootBytes = malformedRoot.toByteArray
    val rootOnly = MemoryMain.State.empty.withFile(
      path(parser._1),
      MemoryMain.FileContent.Bytes(malformedRootBytes)
    ).get
    val rootDescriptorFailure = failed(run(
      inspectionArgs(
        "bad-root.json",
        descriptorOnly = true,
        hashes = pins(malformedRootBytes)
      ),
      rootOnly
    ))
    assert(rootDescriptorFailure.contains("invalid version"), rootDescriptorFailure)
    assert(rootDescriptorFailure.contains(parser._1), rootDescriptorFailure)

    val rootFullFailure = failed(run(
      signatureOnlyArgs(pins(malformedRootBytes)),
      fixtureState.withFile(
        path(parser._1),
        MemoryMain.FileContent.Bytes(malformedRootBytes)
      ).get
    ))
    assert(rootFullFailure.contains("invalid version"), rootFullFailure)
    assert(rootFullFailure.contains(parser._1), rootFullFailure)

    val malformedTargets = List(
      parser._2.copy(publicDependencies = parser._2.publicDependencies.map { dependency =>
        dependency.copy(desc = dependency.desc.map(_.copy(version = Some(invalidVersion))))
      }),
      parser._2.copy(privateDependencies = parser._2.privateDependencies.map { dependency =>
        dependency.copy(desc = dependency.desc.map(_.copy(version = Some(invalidVersion))))
      }),
      parser._2.copy(
        unusedTransitivePublicDependencies = parser._2.unusedTransitivePublicDependencies.map(
          dependency => dependency.copy(
            desc = dependency.desc.map(_.copy(version = Some(invalidVersion)))
          )
        )
      )
    )

    malformedTargets.foreach { library =>
      val bytes = library.toByteArray
      val descriptorOnlyState = MemoryMain.State.empty.withFile(
        path(parser._1),
        MemoryMain.FileContent.Bytes(bytes)
      ).get
      val descriptorFailure = failed(run(
        inspectionArgs("bad-target.json", descriptorOnly = true, hashes = pins(bytes)),
        descriptorOnlyState
      ))
      assert(descriptorFailure.contains("invalid version"), descriptorFailure)
      assert(descriptorFailure.contains("parser 1.0.0"), descriptorFailure)

      val fullFailure = failed(run(signatureOnlyArgs(pins(bytes)), fixtureState.withFile(
        path(parser._1),
        MemoryMain.FileContent.Bytes(bytes)
      ).get))
      assert(fullFailure.contains("invalid version"), fullFailure)
      assert(fullFailure.contains("parser 1.0.0"), fullFailure)
    }
  }
}
