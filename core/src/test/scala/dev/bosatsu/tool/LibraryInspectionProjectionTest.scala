package dev.bosatsu.tool

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Chain
import cats.syntax.all._
import dev.bosatsu.Json
import dev.bosatsu.MemoryMain
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{
  DecodedLibraryWithDeps,
  Library,
  LibraryInspectionFixtures,
  Name,
  VerifiedLibraryGraph,
  VerifiedLibrarySnapshot
}

class LibraryInspectionProjectionTest extends munit.FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type Path = Chain[String]
  private type Fixture = (String, proto.Library, Array[Byte], List[String])

  private val platform = MemoryMain[ErrorOr].platformIO
  import platform.{canPromiseF, moduleIOMonad}

  private val fixtures = LibraryInspectionFixtures.parsedLibraries
  private val rootFixture = fixtures.find(_._2.name == "parser").get
  private val rootOnlyState = MemoryMain.State.empty.withFile(
    path(rootFixture._1),
    MemoryMain.FileContent.Bytes(rootFixture._3)
  ).get

  private def path(raw: String): Path =
    Chain.fromSeq(raw.split("/", -1).toIndexedSeq)

  private def state(extra: List[(String, Array[Byte])] = Nil): MemoryMain.State =
    (LibraryInspectionFixtures.generatedFiles.map(file => file.path -> file.bytes) ::: extra)
      .foldLeft(MemoryMain.State.empty) { case (current, (rawPath, bytes)) =>
        current.withFile(path(rawPath), MemoryMain.FileContent.Bytes(bytes)).get
      }

  private def readContext(
      current: MemoryMain.State
  ): ErrorOr[LibraryInspection.DependencyContext[Path]] =
    LibraryInspection.readDependencyContext(
      platform,
      path("test_workspace/inspect_library/parser-context.json")
    ).runA(current)

  private def graph(
      root: Fixture,
      context: LibraryInspection.DependencyContext[Path],
      current: MemoryMain.State
  ): ErrorOr[VerifiedLibraryGraph[Path]] = {
    val (rootPath, library, _, hashes) = root
    VerifiedLibrarySnapshot
      .verify(
        platform,
        path(rootPath),
        Name(library.name),
        Library.getVersion(library).get,
        hashes
      )
      .flatMap(VerifiedLibraryGraph.verify(platform, _, context))
      .runA(current)
  }

  private final case class Projections(
      signature: Array[Byte],
      exposure: Json,
      membership: Json,
      implementation: Json
  )

  private def projections(
      root: Fixture,
      context: LibraryInspection.DependencyContext[Path],
      current: MemoryMain.State
  ): ErrorOr[Projections] =
    graph(root, context, current).flatMap { verified =>
      DecodedLibraryWithDeps
        .decodeVerifiedGraphWithLibraries[MemoryMain.StateF[ErrorOr]](verified)
        .runA(current)
        .flatMap { case (_, decoded) =>
          for {
            signature <- LibraryInspection.signatureProjection(decoded(verified.rootIdentity))
              .toEither.leftMap[Throwable](error =>
                new Exception(Option(error.getMessage).getOrElse(error.toString))
              )
            exposure <- LibraryInspection.exposureProjection(
              verified,
              decoded,
              platform.pathToString
            ).leftMap[Throwable](error => new Exception(error))
            membership <- LibraryInspection.membershipProjection(verified, decoded)
              .leftMap[Throwable](error => new Exception(error))
          } yield Projections(
            signature.toByteArray,
            exposure,
            membership,
            LibraryInspection.implementationProjection(verified, platform.pathToString)
          )
        }
    }

  private def field(json: Json, name: String): Json = json match {
    case obj: Json.JObject => obj.toMap.getOrElse(name, fail(s"missing JSON field '$name': $json"))
    case _                 => fail(s"expected JSON object, found $json")
  }

  private def array(json: Json): Vector[Json] = json match {
    case Json.JArray(values) => values
    case _                   => fail(s"expected JSON array, found $json")
  }

  private def string(json: Json): String = json match {
    case Json.JString(value) => value
    case _                   => fail(s"expected JSON string, found $json")
  }

  private def record(
      projection: Json,
      libraryName: String,
      libraryVersion: String,
      packageName: String
  ): Json =
    array(field(projection, "packages")).find { candidate =>
      val identity = field(candidate, "library")
      string(field(identity, "name")) == libraryName &&
        string(field(identity, "version")) == libraryVersion &&
        string(field(candidate, "package")) == packageName
    }.getOrElse(fail(s"missing $libraryName $libraryVersion package $packageName"))

  private def editPrivateHelper(library: proto.Library): proto.Library = {
    val package0 = library.internalPackages.head
    val nameIndex = package0.strings.indexOf("private_helper") + 1
    assert(nameIndex > 0, "private_helper should be present in the Parser fixture")
    val let = package0.lets.find(_.name == nameIndex).get
    val expressionIndex = let.expr - 1
    val expression = package0.expressions(expressionIndex)
    val literalExpression = expression.value match {
      case proto.TypedExpr.Value.LiteralExpr(value) => value
      case other => fail(s"expected private_helper to be a literal expression, found $other")
    }
    val changedLiteral = literalExpression.copy(
      literal = Some(proto.Literal(proto.Literal.Value.IntValueAs64(2L)))
    )
    val changedExpression = expression.copy(
      value = proto.TypedExpr.Value.LiteralExpr(changedLiteral)
    )
    val changedPackage = package0.copy(
      expressions = package0.expressions.updated(expressionIndex, changedExpression)
    )
    library.copy(internalPackages = List(changedPackage))
  }

  private def pins(bytes: Array[Byte]): List[String] = List(
    s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
    s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
  )

  test("descriptor projection authenticates only the pinned root before exposing children") {
    val (rootPath, library, _, hashes) = rootFixture
    val result = VerifiedLibrarySnapshot
      .verify(
        platform,
        path(rootPath),
        Name(library.name),
        Library.getVersion(library).get,
        hashes
      )
      .runA(rootOnlyState)
      .flatMap(snapshot =>
        LibraryInspection.descriptorProjection(snapshot).leftMap[Throwable](error =>
          new Exception(error)
        )
      )

    val projection = result.fold(error => fail(error.getMessage), identity)
    assertEquals(field(projection, "schema_version"), Json.JNumberStr("1"))
    assert(string(field(field(projection, "identity"), "name")) == "parser")
    assert(projection.render.contains("public_child"), projection.render)
    assert(projection.render.contains("unused_transitive_public"), projection.render)
    assert(
      projection.render.contains(rootFixture._4.last.stripPrefix("sha256:")),
      projection.render
    )
  }

  test("typed exposures recover legacy exports and keep private implementation edges scoped") {
    val current = state()
    val context = readContext(current).toOption.get
    val output = projections(rootFixture, context, current).toOption.get

    val child = record(output.exposure, "public_child", "1.0.0", "Public/Child")
    val common = array(field(child, "exposes")).find { edge =>
      string(field(edge, "package")) == "Common/Util"
    }.getOrElse(fail(child.render))
    assertEquals(string(field(field(common, "provider"), "name")), "common")
    assertEquals(array(field(common, "witness_path")).size, 2)

    val parser = record(output.exposure, "parser", "1.0.0", "Parser")
    val privateChild = record(output.exposure, "private_child", "1.0.0", "Private/Child")
    List(parser, privateChild).foreach { entry =>
      assert(!array(field(entry, "exposes")).exists(edge =>
        string(field(edge, "package")) == "Common/Util"
      ), entry.render)
    }

    val membership = output.membership.render
    assert(membership.contains("shared"), membership)
    assert(membership.contains("1.0.0"), membership)
    assert(membership.contains("2.0.0"), membership)
    val implementation = output.implementation.render
    assert(implementation.contains("unused_transitive_public"), implementation)
    assert(implementation.contains("sha256"), implementation)
    val artifacts = array(field(output.implementation, "artifacts"))
    val sharedVersions = artifacts.flatMap { artifact =>
      val identity = field(artifact, "identity")
      Option.when(string(field(identity, "name")) == "shared")(
        string(field(identity, "version"))
      )
    }
    assertEquals(sharedVersions, Vector("1.0.0", "2.0.0"))
    val sharedEdges = array(field(output.implementation, "edges")).flatMap { edge =>
      val from = field(edge, "from")
      val to = field(edge, "to")
      Option.when(string(field(to, "name")) == "shared")(
        (string(field(from, "name")), string(field(to, "version")))
      )
    }
    assertEquals(sharedEdges.toSet, Set(("left", "1.0.0"), ("right", "2.0.0")))
    val verified = graph(rootFixture, context, current).toOption.get
    val decoded = DecodedLibraryWithDeps
      .decodeVerifiedGraphWithLibraries[MemoryMain.StateF[ErrorOr]](verified)
      .runA(current)
      .toOption.get._2(verified.rootIdentity)
    assertEquals(
      LibraryInspection.signatureProjection(decoded.copy(interfaces = Nil)).toOption.get,
      proto.Interfaces(Nil)
    )
  }

  test("reordered declarations yield byte-identical canonical projections") {
    val current = state()
    val context = readContext(current).toOption.get
    val first = projections(rootFixture, context, current).toOption.get
    val reverseContext = context.copy(libraries = context.libraries.reverse)
    val reversed = projections(rootFixture, reverseContext, current).toOption.get

    assert(first.signature.sameElements(reversed.signature))
    assertEquals(first.exposure.render, reversed.exposure.render)
    assertEquals(first.membership.render, reversed.membership.render)
    assertEquals(first.implementation.render, reversed.implementation.render)
  }

  test("private implementation edits change artifact metadata but not signature bytes") {
    val current = state()
    val context = readContext(current).toOption.get
    val baseline = projections(rootFixture, context, current).toOption.get

    val editedLibrary = editPrivateHelper(rootFixture._2)
    val editedBytes = editedLibrary.toByteArray
    val editedRoot = (rootFixture._1, editedLibrary, editedBytes, pins(editedBytes))
    val editedState = state(List(rootFixture._1 -> editedBytes))
    val editedContext = readContext(editedState).toOption.get
    val changed = projections(editedRoot, editedContext, editedState).toOption.get

    assert(baseline.signature.sameElements(changed.signature))
    assertNotEquals(baseline.implementation.render, changed.implementation.render)
  }
}
