package dev.bosatsu

import cats.effect.IO
import cats.syntax.all._
import _root_.bosatsu.{TypedAst => proto}
import dev.bosatsu.Identifier.Name
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.Version
import dev.bosatsu.rankn.{DefinedType, Type}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.file.attribute.FileTime
import scala.jdk.CollectionConverters._

class ValidateGraphCommandTest extends FileCommandFixtures {
  private val rootFile = "graph/app.node.json"
  private val depFile = "graph/generated files/葉.node.json"
  private val leafFile = "external/dependency/leaf.node.json"
  private val stamp = "out/validated"
  private val inspectionFixtureDirectory = Paths.get("test_workspace/inspect_library")
  private val inspectionFixtureRoot = "test_workspace/inspect_library"

  private def group(label: String, pkg: String, source: String, edges: (String, String)*): String = {
    val deps = edges.toVector.map { case (dep, manifest) =>
      Json.JObject(List("label" -> Json.JString(dep), "manifest" -> Json.JString(manifest)))
    }
    val member = Json.JObject(List("package" -> Json.JString(pkg), "source" -> Json.JString(source)))
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "kind" -> Json.JString("group"),
      "label" -> Json.JString(label),
      "members" -> Json.JArray(Vector(member)),
      "deps" -> Json.JArray(deps)
    )).render
  }

  private def groupWithMembers(
      label: String,
      members: List[(String, String, Option[List[String]])]
  ): String = Json.JObject(List(
    "schema_version" -> Json.JNumberStr("1"),
    "kind" -> Json.JString("group"),
    "label" -> Json.JString(label),
    "members" -> Json.JArray(members.map { case (pkg, source, exposes) =>
      val fields = List("package" -> Json.JString(pkg), "source" -> Json.JString(source))
      Json.JObject(fields ::: exposes.toList.map(items =>
        "exposes" -> Json.JArray(items.map(Json.JString(_)).toVector)
      ))
    }.toVector),
    "deps" -> Json.JArray(Vector.empty)
  )).render

  private def validate(runner: FileCommandRunner, root: Path, nodes: List[String], local: Boolean) =
    runner.run(root, List("validate-graph", "--root", rootFile, "--out", stamp) :::
      nodes.flatMap(n => List("--node", n)) ::: Option.when(local)("--local").toList)

  private def copyInspectionFixtures(root: Path): IO[Unit] = IO.blocking {
    val files = Files.list(inspectionFixtureDirectory)
    try
      files.iterator().asScala.filter(_.getFileName.toString != "README.md").foreach { source =>
        val destination = root.resolve(inspectionFixtureRoot).resolve(source.getFileName)
        Files.createDirectories(destination.getParent)
        Files.copy(source, destination, StandardCopyOption.REPLACE_EXISTING)
      }
    finally files.close()
  }

  private def writeBytes(root: Path, file: String, bytes: Array[Byte]): IO[Unit] = IO.blocking {
    val destination = root.resolve(file)
    Files.createDirectories(destination.getParent)
    Files.write(destination, bytes)
    ()
  }

  private def libraryEntries(root: Path, file: String): IO[Vector[Json]] =
    read(root, file).map { content =>
      Json.parserFile.parseAll(content).toOption.get match {
        case obj: Json.JObject => obj.getOrNull("libraries") match {
            case Json.JArray(entries) => entries
            case other => throw new AssertionError(s"expected libraries array, found $other")
          }
        case other => throw new AssertionError(s"expected context object, found $other")
      }
    }

  private def writeImportedParserInputs(root: Path): IO[Unit] =
    for {
      rootEntries <- libraryEntries(root, s"$inspectionFixtureRoot/parser-root.json")
      dependencyEntries <- libraryEntries(root, s"$inspectionFixtureRoot/parser-context.json")
      context = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"),
        "libraries" -> Json.JArray(rootEntries ++ dependencyEntries)
      ))
      _ <- write(root, "graph/dependencies.json", context.render)
    } yield ()

  private def hashes(bytes: Array[Byte]): List[String] = List(
    s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
    s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
  )

  test("mixed validation accepts its complete input set and rejects partial configuration") {
    withRunner { (root, runner) =>
      val emptySha = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
      val toolchain =
        s"""{"schema_version":1,"compiler":{"uri":"unused","sha256":"$emptySha"},"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptySha"},"c_runtime":{"uri":"unused","sha256":"$emptySha"}}"""
      val mixed = List(
        "validate-graph",
        "--root", rootFile,
        "--scope-map", "graph/scope-map.json",
        "--dependency-context", "graph/dependencies.json",
        "--toolchain-manifest", "graph/toolchain.json",
        "--out", stamp
      )
      for {
        _ <- write(root, rootFile, group("//app:g", "App", "App.bosatsu"))
        _ <- write(root, "graph/scope-map.json",
          """{"schema_version":1,"members":["App"],"dependencies":[],"imports":[],"bindings":[]}""")
        _ <- write(root, "graph/dependencies.json", """{"schema_version":1,"libraries":[]}""")
        _ <- write(root, "graph/toolchain.json", toolchain)
        valid <- runner.run(root, mixed)
        _ = success(valid)
        _ = assertEquals(valid.stdout, "")
        _ = assertEquals(valid.stderr, "")
        written <- read(root, stamp)
        _ = assertEquals(written, "ok")
        _ <- write(root, stamp, "previous")
        partial <- runner.run(root, mixed.filterNot(_ == "--toolchain-manifest").filterNot(_ == "graph/toolchain.json"))
        _ = failure(partial)
        _ = assert(partial.stderr.contains("mixed validation requires"), partial.stderr)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, "previous")
      } yield ()
    }
  }

  test("mixed validation requires source exposure metadata only when following that provider") {
    withRunner { (root, runner) =>
      val emptySha = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
      val toolchain =
        s"""{"schema_version":1,"compiler":{"uri":"unused","sha256":"$emptySha"},"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptySha"},"c_runtime":{"uri":"unused","sha256":"$emptySha"}}"""
      val scopeMap =
        """{"schema_version":1,"members":["App","Wrapper"],"dependencies":[{"from":{"kind":"member","member":"App"},"package":"Wrapper","to":{"kind":"member","member":"Wrapper"}}],"imports":[],"bindings":[{"referrer":{"kind":"member","member":"App"},"package":"Wrapper","provider":{"kind":"member","member":"Wrapper"},"interface_path":null}]}"""
      val command = List(
        "validate-graph",
        "--root", rootFile,
        "--scope-map", "graph/scope-map.json",
        "--dependency-context", "graph/dependencies.json",
        "--toolchain-manifest", "graph/toolchain.json",
        "--out", stamp
      )
      for {
        _ <- write(root, rootFile, groupWithMembers(
          "//app:g", List(("App", "App.bosatsu", None), ("Wrapper", "Wrapper.bosatsu", None))
        ))
        _ <- write(root, "graph/scope-map.json", scopeMap)
        _ <- write(root, "graph/dependencies.json", "{\"schema_version\":1,\"libraries\":[]}")
        _ <- write(root, "graph/toolchain.json", toolchain)
        missing <- runner.run(root, command)
        _ = failure(missing)
        _ = assert(
          missing.stderr.contains("missing public exposure metadata for source provider Wrapper"),
          missing.stderr
        )
        _ <- write(root, rootFile, groupWithMembers(
          "//app:g", List(("App", "App.bosatsu", Some(Nil)), ("Wrapper", "Wrapper.bosatsu", Some(Nil)))
        ))
        emptyExposures <- runner.run(root, command)
        _ = success(emptyExposures)
        _ = assertEquals(emptyExposures.stdout, "")
        _ = assertEquals(emptyExposures.stderr, "")
      } yield ()
    }
  }

  test("mixed graph validation follows external local signature exposures") {
    withRunner { (root, runner) =>
      val app = PackageName.parts("App")
      val facade = PackageName.parts("Local/Facade")
      val types = PackageName.parts("Local/Types")
      val localScope = (name: PackageName) => Json.JObject(List(
        "kind" -> Json.JString("local-signature"),
        "package" -> Json.JString(name.asString)
      ))
      val appScope = Json.JObject(List(
        "kind" -> Json.JString("member"), "member" -> Json.JString(app.asString)
      ))
      val tokenName = TypeName(Identifier.Constructor("Token"))
      val token = DefinedType(types, tokenName, Nil, Nil)
      val typesIface: Package.Interface = Package(types, Nil, List(
        ExportedName.TypeName(tokenName.ident, Referant.DefinedT(token))
      ), ())
      val facadeIface: Package.Interface = Package(facade, Nil, List(
        ExportedName.Binding(Name("token"), Referant.Value(Type.const(types, tokenName)))
      ), ())
      def scopeMap(includeExposure: Boolean): String = {
        val bindings = Vector(
          Json.JObject(List(
            "referrer" -> appScope,
            "package" -> Json.JString(facade.asString),
            "provider" -> localScope(facade),
            "interface_path" -> Json.JString("facade.sig")
          ))
        ) ++ Option.when(includeExposure)(Json.JObject(List(
          "referrer" -> localScope(facade),
          "package" -> Json.JString(types.asString),
          "provider" -> localScope(types),
          "interface_path" -> Json.JString("types.sig")
        )))
        Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "members" -> Json.JArray(Vector(Json.JString(app.asString))),
          "dependencies" -> Json.JArray(Vector(Json.JObject(List(
            "from" -> appScope,
            "package" -> Json.JString(facade.asString),
            "to" -> localScope(facade)
          )))),
          "imports" -> Json.JArray(Vector.empty),
          "bindings" -> Json.JArray(bindings)
        )).render
      }
      val emptySha = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
      val toolchain =
        s"""{"schema_version":1,"compiler":{"uri":"unused","sha256":"$emptySha"},"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$emptySha"},"c_runtime":{"uri":"unused","sha256":"$emptySha"}}"""
      val command = List(
        "validate-graph", "--root", rootFile,
        "--scope-map", "graph/scope-map.json",
        "--dependency-context", "graph/dependencies.json",
        "--toolchain-manifest", "graph/toolchain.json",
        "--direct-interface", "facade.sig", "--direct-interface", "types.sig",
        "--out", stamp
      )
      for {
        _ <- write(root, rootFile, groupWithMembers("//app:g", List(("App", "App.bosatsu", None))))
        _ <- write(root, "graph/dependencies.json", "{\"schema_version\":1,\"libraries\":[]}")
        _ <- write(root, "graph/toolchain.json", toolchain)
        facadeBundle <- IO.fromTry(PackageSignatures.encode(List(facadeIface)))
        typesBundle <- IO.fromTry(PackageSignatures.encode(List(typesIface)))
        _ <- IOPlatformIO.writeInterfaceBundle(facadeBundle, root.resolve("facade.sig"))
        _ <- IOPlatformIO.writeInterfaceBundle(typesBundle, root.resolve("types.sig"))
        _ <- write(root, "graph/scope-map.json", scopeMap(includeExposure = true))
        complete <- runner.run(root, command)
        _ = success(complete)
        _ <- write(root, "graph/scope-map.json", scopeMap(includeExposure = false))
        missing <- runner.run(root, command)
        _ = failure(missing)
        _ = assert(missing.stderr.contains("missing interface binding for Local/Types"), missing.stderr)
      } yield ()
    }
  }

  test("mixed validation authenticates imported roots and ignores their private versions for visibility") {
    withRunner { (root, runner) =>
      val parserPath = s"$inspectionFixtureRoot/parser-v1.0.0.bosatsu_lib"
      val inspection = List(
        "inspect-library",
        "--input", parserPath,
        "--expected-name", "parser",
        "--expected-version", "1.0.0",
        "--dependency-context", s"$inspectionFixtureRoot/parser-context.json",
        "--sig-out", "graph/parser.sig",
        "--exposures-out", "graph/parser-index.json"
      )
      val selectedCoreHash = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
      val selectedToolchain =
        s"""{"schema_version":1,"compiler":{"uri":"unused","sha256":"$selectedCoreHash"},"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$selectedCoreHash"},"c_runtime":{"uri":"unused","sha256":"$selectedCoreHash"}}"""
      val identity = """{"name":"parser","version":"1.0.0"}"""
      val scopeMap =
        s"""{"schema_version":1,"members":["App"],"dependencies":[{"from":{"kind":"member","member":"App"},"package":"Parser","to":{"kind":"library","identity":$identity}}],"imports":[{"identity":$identity,"signature_path":"graph/parser.sig","public_index_path":"graph/parser-index.json","label":"parser"}],"bindings":[{"referrer":{"kind":"member","member":"App"},"package":"Parser","provider":{"kind":"library","identity":$identity},"interface_path":"graph/parser.sig"}]}"""
      val emptyBindingsScopeMap = Json.parserFile.parseAll(scopeMap).toOption.get match {
        case obj: Json.JObject =>
          Json.JObject(obj.items.filterNot(_._1 == "bindings") :+
            ("bindings" -> Json.JArray(Vector.empty))).render
        case other => fail(s"expected scope map object, found $other")
      }
      val command = List(
        "validate-graph",
        "--root", rootFile,
        "--scope-map", "graph/scope-map.json",
        "--dependency-context", "graph/dependencies.json",
        "--toolchain-manifest", "graph/toolchain.json",
        "--direct-interface", "graph/parser.sig",
        "--out", stamp
      )
      for {
        _ <- copyInspectionFixtures(root)
        _ <- writeImportedParserInputs(root)
        bytes <- IO.blocking(Files.readAllBytes(root.resolve(parserPath)))
        inspected <- runner.run(root, inspection ++ hashes(bytes).flatMap(hash => List("--expected-hash", hash)))
        _ = success(inspected)
        signatureBytes <- IO.blocking(Files.readAllBytes(root.resolve("graph/parser.sig")))
        inspectedIndex <- read(root, "graph/parser-index.json")
        _ <- IO.blocking(Files.write(root.resolve(parserPath), bytes))
        _ <- write(root, rootFile, group("//app:g", "App", "App.bosatsu"))
        _ <- write(root, "graph/scope-map.json", scopeMap)
        _ <- write(root, "graph/toolchain.json", selectedToolchain)
        validated <- runner.run(root, command)
        _ = success(validated)
        _ = assertEquals(validated.stdout, "")
        _ = assertEquals(validated.stderr, "")
        stampText <- read(root, stamp)
        _ = assertEquals(stampText, "ok")
        _ <- write(root, stamp, "previous")
        _ <- write(root, "graph/scope-map.json", emptyBindingsScopeMap)
        missingBinding <- runner.run(root, command)
        _ = failure(missingBinding)
        _ = assert(
          missingBinding.stderr.contains("missing interface binding for Parser"),
          missingBinding.stderr
        )
        missingBindingStamp <- read(root, stamp)
        _ = assertEquals(missingBindingStamp, "previous")
        _ <- write(root, "graph/scope-map.json", scopeMap)
        _ <- write(root, stamp, "ok")
        _ <- writeBytes(root, "graph/parser.sig", proto.Interfaces().toByteArray)
        substitutedSignature <- runner.run(root, command)
        _ = failure(substitutedSignature)
        _ = assert(substitutedSignature.stderr.contains("does not match parser 1.0.0"), substitutedSignature.stderr)
        _ <- writeBytes(root, "graph/parser.sig", signatureBytes)
        actualIndexPackages = Json.parserFile.parseAll(inspectedIndex).toOption.get match {
          case index: Json.JObject => index.toMap.get("packages") match {
              case Some(Json.JArray(packages)) => packages
              case other => fail(s"expected inspected package array, found $other")
            }
          case other => fail(s"expected inspected index object, found $other")
        }
        forgedIndex = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "packages" -> Json.JArray(actualIndexPackages :+ Json.JObject(List(
            "library" -> Json.JObject(List(
              "name" -> Json.JString("forged"),
              "version" -> Json.JString("1.0.0")
            )),
            "package" -> Json.JString("Forged/Owner"),
            "owner" -> Json.JObject(List(
              "name" -> Json.JString("forged"),
              "version" -> Json.JString("1.0.0")
            )),
            "exposes" -> Json.JArray(Vector.empty)
          )))
        ))
        _ <- write(root, "graph/parser-index.json", forgedIndex.render)
        forgedOwnership <- runner.run(root, command)
        _ = failure(forgedOwnership)
        _ = assert(
          forgedOwnership.stderr.contains("scope-map public index does not match the inspected library index"),
          forgedOwnership.stderr
        )
        _ <- write(root, "graph/parser-index.json", inspectedIndex)
        _ <- writeBytes(root, parserPath, bytes :+ 0.toByte)
        substitutedArtifact <- runner.run(root, command)
        _ = failure(substitutedArtifact)
        _ = assert(substitutedArtifact.stderr.contains("pin mismatch"), substitutedArtifact.stderr)
        _ <- writeBytes(root, parserPath, bytes)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, "ok")
      } yield ()
    }
  }

  test("mixed validation rejects an incompatible core reached through a private edge") {
    withRunner { (root, runner) =>
      val parserPath = s"$inspectionFixtureRoot/parser-v1.0.0.bosatsu_lib"
      val corePath = "graph/bosatsu-v2.0.0.bosatsu_lib"
      val versionTwo = Version.parser.parseAll("2.0.0").toOption.get
      val coreBytes = proto.Library(
        name = "bosatsu",
        descriptor = Some(proto.LibDescriptor(version = Some(versionTwo.toProto)))
      ).toByteArray
      val coreDependency = proto.LibDependency(
        name = "bosatsu",
        desc = Some(proto.LibDescriptor(
          version = Some(versionTwo.toProto),
          hashes = List(s"blake3:${Algo.hashBytes[Algo.Blake3](coreBytes).hex}")
        ))
      )
      val command = List(
        "validate-graph",
        "--root", rootFile,
        "--scope-map", "graph/scope-map.json",
        "--dependency-context", "graph/dependencies.json",
        "--toolchain-manifest", "graph/toolchain.json",
        "--direct-interface", "graph/parser.sig",
        "--out", stamp
      )
      for {
        _ <- copyInspectionFixtures(root)
        originalParser <- IO.blocking(Files.readAllBytes(root.resolve(parserPath)))
        parsedParser = proto.Library.parseFrom(originalParser)
        parser = parsedParser.copy(
          privateDependencies = parsedParser.privateDependencies :+ coreDependency
        )
        parserBytes = parser.toByteArray
        _ <- IO.blocking {
          Files.createDirectories(root.resolve(corePath).getParent)
          Files.write(root.resolve(parserPath), parserBytes)
          Files.write(root.resolve(corePath), coreBytes)
          ()
        }
        dependencyEntries <- libraryEntries(root, s"$inspectionFixtureRoot/parser-context.json")
        coreDeclaration = Json.JObject(List(
          "name" -> Json.JString("bosatsu"),
          "version" -> Json.JString("2.0.0"),
          "path" -> Json.JString(corePath),
          "hashes" -> Json.JArray(hashes(coreBytes).map(Json.JString(_)).toVector)
        ))
        inspectionContext = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "libraries" -> Json.JArray(dependencyEntries :+ coreDeclaration)
        ))
        _ <- write(root, "graph/inspection-dependencies.json", inspectionContext.render)
        rootDeclaration = Json.JObject(List(
          "name" -> Json.JString("parser"),
          "version" -> Json.JString("1.0.0"),
          "path" -> Json.JString(parserPath),
          "hashes" -> Json.JArray(hashes(parserBytes).map(Json.JString(_)).toVector)
        ))
        allContext = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "libraries" -> Json.JArray(Vector(rootDeclaration) ++ dependencyEntries :+ coreDeclaration)
        ))
        _ <- write(root, "graph/dependencies.json", allContext.render)
        inspectArgs = List(
          "inspect-library",
          "--input", parserPath,
          "--expected-name", "parser",
          "--expected-version", "1.0.0",
          "--dependency-context", "graph/inspection-dependencies.json",
          "--sig-out", "graph/parser.sig",
          "--exposures-out", "graph/parser-index.json"
        ) ++ hashes(parserBytes).flatMap(hash => List("--expected-hash", hash))
        inspected <- runner.run(root, inspectArgs)
        _ = success(inspected)
        identity = """{"name":"parser","version":"1.0.0"}"""
        scopeMap =
          s"""{"schema_version":1,"members":["App"],"dependencies":[{"from":{"kind":"member","member":"App"},"package":"Parser","to":{"kind":"library","identity":$identity}}],"imports":[{"identity":$identity,"signature_path":"graph/parser.sig","public_index_path":"graph/parser-index.json","label":"parser"}],"bindings":[{"referrer":{"kind":"member","member":"App"},"package":"Parser","provider":{"kind":"library","identity":$identity},"interface_path":"graph/parser.sig"}]}"""
        _ <- write(root, "graph/scope-map.json", scopeMap)
        selectedCoreHash = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
        selectedToolchain =
          s"""{"schema_version":1,"compiler":{"uri":"unused","sha256":"$selectedCoreHash"},"core":{"library_name":"bosatsu","version":"1.2.3","uri":"unused","sha256":"$selectedCoreHash"},"c_runtime":{"uri":"unused","sha256":"$selectedCoreHash"}}"""
        _ <- write(root, "graph/toolchain.json", selectedToolchain)
        _ <- write(root, rootFile, group("//app:g", "App", "App.bosatsu"))
        _ <- write(root, stamp, "previous")
        invalid <- runner.run(root, command)
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains("toolchain core identity mismatch"), invalid.stderr)
        _ = assert(invalid.stderr.contains("parser 1.0.0 -> bosatsu 2.0.0"), invalid.stderr)
        previous <- read(root, stamp)
        _ = assertEquals(previous, "previous")
      } yield ()
    }
  }

  test("local and complete actions work from declared manifests alone") {
    withRunner { (root, runner) =>
      val app = group("//app:g", "App/Main", "//app:Main.bosatsu", "@repo//lib:g" -> depFile)
      val dep = group("@repo//lib:g", "Lib/Api", "@repo//lib:Api.bosatsu", "//leaf:g" -> leafFile)
      for {
        _ <- write(root, rootFile, app)
        _ <- write(root, depFile, dep)
        local <- validate(runner, root, List(depFile), local = true)
        _ = success(local)
        _ = assertEquals(local.stdout, "")
        _ = assertEquals(local.stderr, "")
        text <- read(root, stamp)
        _ = assertEquals(text.trim, "ok")
        _ <- IO.blocking(Files.setLastModifiedTime(root.resolve(stamp), FileTime.fromMillis(1000000000000L)))
        before = Files.getLastModifiedTime(root.resolve(stamp))
        // No source or implementation file exists. The leaf's node file is
        // added only for the complete composition check.
        _ <- write(root, leafFile, group("//leaf:g", "Lib/Leaf", "//leaf:Leaf.bosatsu"))
        full <- validate(runner, root, List(leafFile, depFile, depFile, rootFile), local = false)
        _ = success(full)
        _ = assertEquals(Files.getLastModifiedTime(root.resolve(stamp)), before)
        // A different valid topology also preserves the stamp and its mtime.
        _ <- write(root, depFile, group("@repo//lib:g", "Lib/New", "@repo//lib:New.bosatsu"))
        changed <- validate(runner, root, List(depFile), local = false)
        _ = success(changed)
        _ = assertEquals(Files.getLastModifiedTime(root.resolve(stamp)), before)
      } yield ()
    }
  }

  test("the local action can exclude a direct dependency's outgoing edges from its inputs") {
    withRunner { (root, runner) =>
      val membersFile = "gen/lib.members.json"
      val localRoot = "gen/app.local.json"
      def members(pkg: String) =
        s"""{"schema_version":1,"kind":"group-members","label":"//:lib","members":[{"package":"$pkg","source":"src/Lib.bosatsu"}]}"""
      val localArgs = List("validate-graph", "--local", "--root", localRoot, "--node", membersFile, "--out", stamp)
      for {
        _ <- write(root, localRoot, group("//:app", "App", "src/App.bosatsu", "//:lib" -> membersFile))
        _ <- write(root, rootFile, group("//:app", "App", "src/App.bosatsu", "//:lib" -> depFile))
        _ <- write(root, membersFile, members("Lib"))
        // A sandboxed local action has no access to lib's topology manifest.
        local <- runner.run(root, localArgs)
        _ = success(local)
        _ <- write(root, depFile, group("//:lib", "Lib", "src/Lib.bosatsu", "//:new" -> leafFile))
        _ <- write(root, leafFile, group("//:new", "New", "src/New.bosatsu"))
        full <- validate(runner, root, List(depFile, leafFile), local = false)
        _ = success(full)
        _ <- write(root, depFile, group("//:lib", "Lib", "src/Lib.bosatsu", "//:missing" -> "other.node"))
        badFull <- validate(runner, root, List(depFile), local = false)
        _ = failure(badFull)
        _ = assert(badFull.stderr.contains("//:app -> //:lib -> //:missing"), badFull.stderr)
        sameLocal <- runner.run(root, localArgs)
        _ = success(sameLocal)
        _ <- write(root, membersFile, members("App"))
        collision <- runner.run(root, localArgs)
        _ = failure(collision)
        _ = assert(collision.stderr.contains("duplicate package App"), collision.stderr)
        // A versioned library must never be interpreted in the loose group
        // namespace, even with the same file path, label and member list.
        _ <- write(root, membersFile, members("Lib").replace("group-members", "library"))
        library <- runner.run(root, localArgs)
        _ = failure(library)
        _ = assert(library.stderr.contains("unsupported graph node kind 'library'"), library.stderr)
      } yield ()
    }
  }

  test("a full scan requires topology even when a declared dependency has valid membership") {
    withRunner { (root, runner) =>
      val summary = """{"schema_version":1,"kind":"group-members","label":"//:lib","members":[{"package":"Lib","source":"Lib.bosatsu"}]}"""
      for {
        _ <- write(root, rootFile, group("//:app", "App", "App.bosatsu", "//:lib" -> depFile))
        _ <- write(root, depFile, summary)
        full <- validate(runner, root, List(depFile), local = false)
        _ = failure(full)
        _ = assert(full.stderr.contains("group-members"), full.stderr)
        _ = assert(!Files.exists(root.resolve(stamp)))
        _ <- write(root, rootFile, summary)
        rootOnly <- validate(runner, root, Nil, local = true)
        _ = failure(rootOnly)
        _ = assert(!Files.exists(root.resolve(stamp)))
      } yield ()
    }
  }

  test("a full check names both routes to a conflicting private member") {
    withRunner { (root, runner) =>
      val left = "out/left.json"
      val right = "out/right.json"
      val a = "out/a.json"
      val b = "out/b.json"
      val all = List(b, a, right, left)
      for {
        _ <- write(root, rootFile, group("//:app", "App", "App.bosatsu", "//:left" -> left, "//:right" -> right))
        _ <- write(root, left, group("//:left", "Left", "Left.bosatsu", "//:a" -> a))
        _ <- write(root, right, group("//:right", "Right", "Right.bosatsu", "//:b" -> b))
        _ <- write(root, a, group("//:a", "Util/Private", "first/Util.bosatsu"))
        _ <- write(root, b, group("//:b", "Util/Private", "second/Util.bosatsu"))
        local <- validate(runner, root, List(left, right), local = true)
        _ = success(local)
        original <- read(root, stamp)
        bad <- validate(runner, root, all, local = false)
        _ = failure(bad)
        _ = assert(bad.stderr.contains("//:app -> //:left -> //:a (first/Util.bosatsu, Util/Private)"), bad.stderr)
        _ = assert(bad.stderr.contains("//:app -> //:right -> //:b (second/Util.bosatsu, Util/Private)"), bad.stderr)
        reordered <- validate(runner, root, all.reverse, local = false)
        _ = assertEquals(reordered, bad)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, original)
        _ <- write(root, b, group("//:b", "Other/Private", "second/Util.bosatsu"))
        recovered <- validate(runner, root, all, local = false)
        _ = success(recovered)
      } yield ()
    }
  }

  test("the schema rejects mistaken node kinds and malformed member or edge identities") {
    withRunner { (root, runner) =>
      val base = """{"schema_version":1,"kind":"group","label":"//:g","members":[{"source":"s","package":"Good"}],"deps":[]}"""
      val invalid = List(
        base.replace("\"schema_version\":1", "\"schema_version\":2") -> "unsupported schema version 2",
        base.replace("\"schema_version\":1,", "") -> "schema_version",
        base.replace("\"group\"", "\"library\"") -> "unsupported graph node kind 'library'",
        base.replace("\"//:g\"", "\"\"") -> "nonempty string",
        base.replace("\"Good\"", "\"bad/name\"") -> "package",
        base.replace("\"s\"", "\"\"") -> "source",
        base.replace("{\"source\":\"s\",\"package\":\"Good\"}", "") -> "at least one group member",
        base.replace("\"source\":\"s\"", "\"source\":\"s\",\"source\":\"other\"") -> "repeated JSON fields: source",
        base.replace("\"schema_version\":1", "\"schema_version\":2,\"schema_version\":1") -> "repeated JSON fields: schema_version",
        base.replace("\"deps\":[]", "\"deps\":[{\"label\":\"//:missing\"}]") -> "manifest",
        base.replace("\"deps\":[]", "\"deps\":[{\"label\":true,\"manifest\":\"path\"}]") -> "label",
        base.dropRight(1) -> "invalid JSON"
      )
      for {
        _ <- write(root, stamp, "prior success")
        _ <- invalid.traverse_ { case (input, reason) =>
          for {
            _ <- write(root, rootFile, input)
            result <- validate(runner, root, Nil, local = false)
            _ = failure(result)
            _ = assert(result.stderr.contains(Path.of(rootFile).toString) && result.stderr.contains(reason), result.stderr)
            previous <- read(root, stamp)
            _ = assertEquals(previous, "prior success")
          } yield ()
        }
        _ <- write(root, rootFile, base)
        ok <- validate(runner, root, Nil, local = false)
        _ = success(ok)
      } yield ()
    }
  }

  test("a request cannot discover new node inputs or read surplus undeclared subtrees") {
    withRunner { (root, runner) =>
      val app = group("//:app", "App", "App.bosatsu", "//:dep" -> depFile)
      for {
        _ <- write(root, rootFile, app)
        _ <- write(root, depFile, "deliberately invalid JSON")
        undeclared <- validate(runner, root, Nil, local = false)
        _ = failure(undeclared)
        _ = assert(undeclared.stderr.contains("missing declared --node"), undeclared.stderr)
        _ = assert(undeclared.stderr.contains("//:app -> //:dep"), undeclared.stderr)
        declared <- validate(runner, root, List(depFile), local = false)
        _ = failure(declared)
        _ = assert(declared.stderr.contains("invalid JSON") && declared.stderr.contains(Path.of(depFile).toString), declared.stderr)
        _ <- write(root, depFile, group("//:dep", "Dep", "Dep.bosatsu"))
        extra <- validate(runner, root, List(depFile, "unreadable-extra"), local = false)
        _ = failure(extra)
        _ = assert(extra.stderr.contains("unreachable node manifests: unreadable-extra"), extra.stderr)
        _ = assert(!Files.exists(root.resolve(stamp)))
      } yield ()
    }
  }

  test("ill-formed UTF-8 cannot turn a different group label into the expected label") {
    withRunner { (root, runner) =>
      val label = "//:lib\ufffd"
      val marker = group("//:lib~", "Lib", "src/Lib.bosatsu")
      val corrupt = marker.getBytes(StandardCharsets.UTF_8)
      corrupt(marker.indexOf('~')) = 0xff.toByte
      for {
        _ <- write(root, rootFile, group("//:app", "App", "src/App.bosatsu", label -> depFile))
        _ <- write(root, depFile, group(label, "Lib", "src/Lib.bosatsu"))
        valid <- validate(runner, root, List(depFile), local = false)
        _ = success(valid)
        _ <- IO.blocking(Files.write(root.resolve(depFile), corrupt))
        _ <- write(root, stamp, "previous")
        invalid <- validate(runner, root, List(depFile), local = false)
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains(Path.of(depFile).toString) && invalid.stderr.contains("UTF-8"), invalid.stderr)
        preserved <- read(root, stamp)
        _ = assertEquals(preserved, "previous")
      } yield ()
    }
  }
}
