package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.effect.IO
import cats.syntax.all._
import dev.bosatsu.Identifier.{Constructor, Name}
import dev.bosatsu.hashing.Algo
import dev.bosatsu.rankn.{DefinedType, Type, TypeAlias}
import dev.bosatsu.tool.{CliException, LibraryInspection, PackageBindings, ToolchainManifest}
import java.nio.file.{Files, Path, Paths}

class CompilePackagesCommandTest extends FileCommandFixtures {
  private def compile(
      runner: FileCommandRunner,
      root: Path,
      out: String,
      sources: List[String],
      deps: List[String]
  ): IO[FileCommandRunner.Result] =
    runner.run(
      root,
      List("compile-packages", "--packages-out", s"$out.pkg", "--sig-out", s"$out.sig") :::
        sources.flatMap(s => List("--src", s"$s.bosatsu")) :::
        deps.flatMap(d => List("--direct-interface", s"$d.sig"))
    )

  private def bytes(root: Path, name: String): List[Byte] =
    Files.readAllBytes(root.resolve(name)).toList

  private def scope(kind: String, member: String): Json = Json.JObject(List(
    "kind" -> Json.JString(kind),
    "member" -> Json.JString(member)
  ))

  private def libraryScope(name: String, version: String): Json = Json.JObject(List(
    "kind" -> Json.JString("library"),
    "identity" -> Json.JObject(List(
      "name" -> Json.JString(name), "version" -> Json.JString(version)
    ))
  ))

  private def localSignatureScope(packageName: PackageName): Json = Json.JObject(List(
    "kind" -> Json.JString("local-signature"),
    "package" -> Json.JString(packageName.asString)
  ))

  private def emptyToolchain: String = {
    val digest = Algo.hashBytes[Algo.Sha256](Array.emptyByteArray).hex
    Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "compiler" -> Json.JObject(List(
        "uri" -> Json.JString("unused"), "sha256" -> Json.JString(digest)
      )),
      "core" -> Json.JObject(List(
        "library_name" -> Json.JString("bosatsu"),
        "version" -> Json.JString("1.2.3"),
        "uri" -> Json.JString("unused"),
        "sha256" -> Json.JString(digest)
      )),
      "c_runtime" -> Json.JObject(List(
        "uri" -> Json.JString("unused"), "sha256" -> Json.JString(digest)
      )),
      "core_sources" -> Json.JNull
    )).render
  }

  private def emptyDependencyContext: String = Json.JObject(List(
    "schema_version" -> Json.JNumberStr("1"),
    "libraries" -> Json.JArray(Vector.empty)
  )).render

  private def writeLocalScopeInputs(root: Path): IO[Unit] = {
    val client = PackageName.parts("Client")
    val provider = PackageName.parts("Provider")
    val dependencies = Json.JArray(Vector(Json.JObject(List(
      "from" -> scope("member", client.asString),
      "package" -> Json.JString(provider.asString),
      "to" -> scope("member", provider.asString)
    ))))
    val bindings = Json.JArray(Vector(Json.JObject(List(
      "referrer" -> scope("member", client.asString),
      "package" -> Json.JString(provider.asString),
      "provider" -> scope("member", provider.asString)
    ))))
    val scopeMap = Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "members" -> Json.JArray(Vector(Json.JString(client.asString), Json.JString(provider.asString))),
      "dependencies" -> dependencies,
      "imports" -> Json.JArray(Vector.empty),
      "bindings" -> bindings
    )).render
    val publicIndex = Json.JObject(List(
      "schema_version" -> Json.JNumberStr("1"),
      "packages" -> Json.JArray(Vector.empty)
    )).render
    write(root, "scope.json", scopeMap) *>
      write(root, "scope-index.json", publicIndex) *>
      write(root, "dependencies.json", emptyDependencyContext) *>
      write(root, "toolchain.json", emptyToolchain)
  }

  private def scopedArgs(out: String): List[String] =
    List(
      "compile-packages", "--src", "Client.bosatsu", "--src", "Provider.bosatsu",
      "--packages-out", s"$out.pkg", "--sig-out", s"$out.sig",
      "--scope-map", "scope.json", "--dependency-context", "dependencies.json",
      "--toolchain-manifest", "toolchain.json"
    )

  private def hashes(bytes: Array[Byte]): List[String] = List(
    s"blake3:${Algo.hashBytes[Algo.Blake3](bytes).hex}",
    s"sha256:${Algo.hashBytes[Algo.Sha256](bytes).hex}"
  )

  private def inspectSharedLibrary(
      root: Path,
      runner: FileCommandRunner
  ): IO[List[String]] = inspectSharedLibraryVersion(root, runner, "1.0.0", "shared")

  private def inspectSharedLibraryVersion(
      root: Path,
      runner: FileCommandRunner,
      version: String,
      prefix: String
  ): IO[List[String]] = {
    val fixture = Paths.get(s"test_workspace/inspect_library/shared-v$version.bosatsu_lib")
    for {
      bytes <- IO.blocking(Files.readAllBytes(fixture))
      _ <- IO.blocking(Files.write(root.resolve(s"$prefix.lib"), bytes))
      _ <- write(root, "inspect-context.json", emptyDependencyContext)
      signature = root.resolve(s"$prefix.sig").toString
      index = root.resolve(s"$prefix-index.json").toString
      args = List(
        "inspect-library", "--input", root.resolve(s"$prefix.lib").toString,
        "--expected-name", "shared", "--expected-version", version,
        "--dependency-context", "inspect-context.json",
        "--sig-out", signature, "--exposures-out", index
      ) ::: hashes(bytes).flatMap(hash => List("--expected-hash", hash))
      inspected <- runner.run(root, args)
      _ = success(inspected)
    } yield hashes(bytes)
  }

  test("batch outputs contain only the member packages and preserve private test discovery") {
    withRunner { (root, runner) =>
      val base = """package Base
                   |export answer
                   |answer = 42
                   |suite = Assertion(answer matches 42, "private suite")
                   |""".stripMargin
      val local = """package Local
                    |from Base import answer
                    |export value
                    |value = answer
                    |""".stripMargin
      val client = """package Client
                     |from Local import value
                     |export main
                     |main = value
                     |""".stripMargin
      for {
        _ <- write(root, "Base.bosatsu", base)
        _ <- write(root, "Local.bosatsu", local)
        _ <- write(root, "Client.bosatsu", client)
        first <- compile(runner, root, "group", List("Local", "Base"), Nil)
        _ = success(first)
        originalSig = bytes(root, "group.sig")
        originalImpl = bytes(root, "group.pkg")
        packs <- IOPlatformIO.readPackages(List(root.resolve("group.pkg")))
        _ = assertEquals(packs.map(_.name.asString), List("Base", "Local"))
        selected = Package.testEntry(packs.head).toOption.flatten.map(_.bindable.sourceCodeRepr)
        _ = assertEquals(selected, Some("suite"))
        sig <- IOPlatformIO.readInterfaces(List(root.resolve("group.sig")))
        publicNames = sig.map(_.exports.map(_.name.sourceCodeRepr))
        _ = assertEquals(publicNames, List(List("answer"), List("value")))
        reordered <- compile(runner, root, "group", List("Base", "Local"), Nil)
        _ = success(reordered)
        _ = assertEquals(bytes(root, "group.sig"), originalSig)
        _ = assertEquals(bytes(root, "group.pkg"), originalImpl)
        _ <- write(root, "Base.bosatsu", base.replace("private suite", "edited suite"))
        edited <- compile(runner, root, "group", List("Base", "Local"), Nil)
        _ = success(edited)
        _ = assertEquals(bytes(root, "group.sig"), originalSig)
        _ = assertNotEquals(bytes(root, "group.pkg"), originalImpl)
        _ <- IO.blocking(Files.delete(root.resolve("group.pkg")))
        dependent <- compile(runner, root, "client", List("Client"), List("group"))
        _ = success(dependent)
        _ <- write(root, "Base.bosatsu", base.replace("42", "True"))
        publicEdit <- compile(runner, root, "group", List("Base", "Local"), Nil)
        _ = success(publicEdit)
        _ = assertNotEquals(bytes(root, "group.sig"), originalSig)
      } yield ()
    }
  }

  test("unscoped compilation rejects outputs that alias each other or an input") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Base.bosatsu", "package Base\nexport answer\nanswer = 42\n")
        _ <- write(root, "group.sig", "previous signature")
        sameOutputs <- runner.run(root, List(
          "compile-packages", "--src", "Base.bosatsu",
          "--packages-out", "group.sig", "--sig-out", "group.sig"
        ))
        _ = failure(sameOutputs)
        _ = assert(sameOutputs.stderr.contains("multiple outputs refer to the same file"), sameOutputs.stderr)
        overwritesSource <- runner.run(root, List(
          "compile-packages", "--src", "Base.bosatsu",
          "--packages-out", "Base.bosatsu", "--sig-out", "group.sig"
        ))
        _ = failure(overwritesSource)
        _ = assert(overwritesSource.stderr.contains("refers to runtime input"), overwritesSource.stderr)
        preservedSource <- read(root, "Base.bosatsu")
        _ = assertEquals(preservedSource, "package Base\nexport answer\nanswer = 42\n")
        preservedSig <- read(root, "group.sig")
        _ = assertEquals(preservedSig, "previous signature")
      } yield ()
    }
  }

  test("scoped compilation publishes source-owner bindings and an explicit empty member context") {
    withRunner { (root, runner) =>
      val client = "package Client\nfrom Provider import value\nexport main\nmain = value\n"
      val provider = "package Provider\nexport value\nvalue = 42\n"
      for {
        _ <- write(root, "Client.bosatsu", client)
        _ <- write(root, "Provider.bosatsu", provider)
        _ <- writeLocalScopeInputs(root)
        result <- runner.run(root, scopedArgs("scoped"))
        _ = success(result)
        bundle <- IOPlatformIO.read[proto.Packages](root.resolve("scoped.pkg"))
        metadata = bundle.buildRequirements.get
        _ = assertEquals(metadata.scopedVersion, 1)
        _ = assertEquals(metadata.interfaces, Seq.empty)
        _ = assertEquals(metadata.members.map(_.memberName), Seq("Client", "Provider"))
        parsed <- IO.fromTry(PackageSignatures.parseBuildRequirements(metadata))
        _ = parsed match {
          case PackageSignatures.ParsedBuildRequirements.Scoped(members) =>
            val clientRequirements = members.find(_.member == PackageName.parts("Client")).get
            assertEquals(clientRequirements.bindings.map(_.owner.scope),
              List(PackageBindings.Scope.Member(PackageName.parts("Provider"))))
            assertEquals(clientRequirements.interfaces, Nil)
            val providerRequirements = members.find(_.member == PackageName.parts("Provider")).get
            assertEquals(providerRequirements.bindings, Nil)
          case other => fail(s"expected scoped requirements, found $other")
        }
        partial <- runner.run(root, scopedArgs("partial").take(10) ::: List("scope.json"))
        _ = failure(partial)
        _ = assert(partial.stderr.contains("requires --scope-map"), partial.stderr)
        _ <- write(root, "Extra.bosatsu", "package Extra\nexport value\nvalue = 1\n")
        extra <- compile(runner, root, "extra", List("Extra"), Nil)
        _ = success(extra)
        unused <- runner.run(root, scopedArgs("unused") :::
          List("--direct-interface", "extra.sig", "--error-unused-interfaces"))
        _ = failure(unused)
        _ = assert(unused.stderr.contains("unused direct interfaces: extra.sig"), unused.stderr)
        _ <- IO.blocking(Files.createDirectory(root.resolve("recover.sig")))
        _ <- write(root, "recover.pkg", "previous implementation")
        failedPublish <- runner.run(root, scopedArgs("recover"))
        _ = failure(failedPublish)
        previous <- read(root, "recover.pkg")
        _ = assertEquals(previous, "previous implementation")
      } yield ()
    }
  }

  test("scoped compilation authenticates imported bytes before replacing outputs") {
    withRunner { (root, runner) =>
      val digest = "0" * 64
      val identity = Json.JObject(List(
        "name" -> Json.JString("pinned"), "version" -> Json.JString("1.0.0")
      ))
      val publicIndex = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"), "packages" -> Json.JArray(Vector.empty)
      )).render
      val imports = Json.JArray(Vector(Json.JObject(List(
        "identity" -> identity,
        "signature_path" -> Json.JString("pinned.sig"),
        "public_index_path" -> Json.JString("pinned-index.json")
      ))))
      val scopeMap = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"),
        "members" -> Json.JArray(Vector(Json.JString("Client"))),
        "dependencies" -> Json.JArray(Vector.empty),
        "imports" -> imports,
        "bindings" -> Json.JArray(Vector.empty)
      )).render
      val dependencyContext = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"),
        "libraries" -> Json.JArray(Vector(Json.JObject(List(
          "name" -> Json.JString("pinned"),
          "version" -> Json.JString("1.0.0"),
          "path" -> Json.JString("pinned.lib"),
          "hashes" -> Json.JArray(Vector(Json.JString(s"blake3:$digest")))
        ))))
      )).render
      for {
        _ <- write(root, "Client.bosatsu", "package Client\nexport main\nmain = 1\n")
        _ <- write(root, "scope.json", scopeMap)
        _ <- write(root, "pinned-index.json", publicIndex)
        _ <- write(root, "dependencies.json", dependencyContext)
        _ <- write(root, "toolchain.json", emptyToolchain)
        _ <- write(root, "pinned.lib", "not a pinned library")
        _ <- IOPlatformIO.writeInterfaceBundle(proto.Interfaces(), root.resolve("pinned.sig"))
        _ <- write(root, "protected.pkg", "previous implementation")
        _ <- write(root, "protected.sig", "previous signature")
        args = List(
          "compile-packages", "--src", "Client.bosatsu", "--direct-interface", "pinned.sig",
          "--packages-out", "protected.pkg", "--sig-out", "protected.sig",
          "--scope-map", "scope.json", "--dependency-context", "dependencies.json",
          "--toolchain-manifest", "toolchain.json"
        )
        failed <- runner.run(root, args)
        _ = failure(failed)
        _ = assert(failed.stderr.contains("blake3 pin mismatch"), failed.stderr)
        previousImpl <- read(root, "protected.pkg")
        previousSig <- read(root, "protected.sig")
        _ = assertEquals(previousImpl, "previous implementation")
        _ = assertEquals(previousSig, "previous signature")
      } yield ()
    }
  }

  test("scoped compilation selects an authenticated imported owner and records its interface digest") {
    withRunner { (root, runner) =>
      val library = "shared"
      val version = "1.0.0"
      val identity = Json.JObject(List(
        "name" -> Json.JString(library), "version" -> Json.JString(version)
      ))
      val member = PackageName.parts("Client")
      val packageName = PackageName.parts("Shared/Api")
      for {
        hashes <- inspectSharedLibrary(root, runner)
        _ <- write(root, "Client.bosatsu",
          "package Client\nfrom Shared/Api import pick\nexport main\nmain = pick(1)\n")
        dependency = Json.JObject(List(
          "from" -> scope("member", member.asString),
          "package" -> Json.JString(packageName.asString),
          "to" -> libraryScope(library, version)
        ))
        binding = Json.JObject(List(
          "referrer" -> scope("member", member.asString),
          "package" -> Json.JString(packageName.asString),
          "provider" -> libraryScope(library, version),
          "interface_path" -> Json.JString("shared.sig")
        ))
        scopeMap = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "members" -> Json.JArray(Vector(Json.JString(member.asString))),
          "dependencies" -> Json.JArray(Vector(dependency)),
          "imports" -> Json.JArray(Vector(Json.JObject(List(
            "identity" -> identity,
            "signature_path" -> Json.JString("shared.sig"),
            "public_index_path" -> Json.JString("shared-index.json"),
            "label" -> Json.JString("//shared:api")
          )))),
          "bindings" -> Json.JArray(Vector(binding))
        )).render
        libraryRecord = Json.JObject(List(
          "name" -> Json.JString(library),
          "version" -> Json.JString(version),
          "path" -> Json.JString("shared.lib"),
          "hashes" -> Json.JArray(hashes.map(Json.JString(_)).toVector)
        ))
        dependencyContext = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "libraries" -> Json.JArray(Vector(libraryRecord))
        )).render
        _ <- write(root, "scope.json", scopeMap)
        _ <- write(root, "dependencies.json", dependencyContext)
        _ <- write(root, "toolchain.json", emptyToolchain)
        result <- runner.run(root, List(
          "compile-packages", "--src", "Client.bosatsu", "--direct-interface", "shared.sig",
          "--error-unused-interfaces", "--packages-out", "scoped.pkg", "--sig-out", "scoped.sig",
          "--scope-map", "scope.json", "--dependency-context", "dependencies.json",
          "--toolchain-manifest", "toolchain.json"
        ))
        _ = success(result)
        bundle <- IOPlatformIO.read[proto.Packages](root.resolve("scoped.pkg"))
        metadata = bundle.buildRequirements.get
        parsed <- IO.fromTry(PackageSignatures.parseBuildRequirements(metadata))
        sharedInterfaces <- IOPlatformIO.readInterfaces(List(root.resolve("shared.sig")))
        expected <- IO.fromTry(PackageSignatures.requirement(
          sharedInterfaces.find(_.name == packageName).get
        ))
        _ = parsed match {
          case PackageSignatures.ParsedBuildRequirements.Scoped(List(requirement)) =>
            assertEquals(requirement.member, member)
            assertEquals(requirement.bindings.map(_.packageName), List(packageName))
            requirement.bindings.head.owner.scope match {
              case PackageBindings.Scope.Library((name, selectedVersion)) =>
                assertEquals(name.name, library)
                assertEquals(selectedVersion.render, version)
              case other => fail(s"expected library owner, found $other")
            }
            assertEquals(requirement.interfaces, List(
              PackageSignatures.InterfaceDigest(packageName, expected.interfaceBlake3)
            ))
          case other => fail(s"expected one scoped member, found $other")
        }
      } yield ()
    }
  }

  test("scoped command bundles decode same-named providers in their recorded member contexts") {
    withRunner { (root, runner) =>
      val api = PackageName.parts("Shared/Api")
      val one = PackageName.parts("Client/One")
      val two = PackageName.parts("Client/Two")
      val versionOne = "1.0.0"
      val versionTwo = "2.0.0"
      def dependency(from: PackageName, version: String): Json = Json.JObject(List(
        "from" -> scope("member", from.asString),
        "package" -> Json.JString(api.asString),
        "to" -> libraryScope("shared", version)
      ))
      def binding(from: PackageName, version: String, signature: String): Json = Json.JObject(List(
        "referrer" -> scope("member", from.asString),
        "package" -> Json.JString(api.asString),
        "provider" -> libraryScope("shared", version),
        "interface_path" -> Json.JString(signature)
      ))
      def imported(version: String, signature: String, index: String): Json = Json.JObject(List(
        "identity" -> Json.JObject(List(
          "name" -> Json.JString("shared"), "version" -> Json.JString(version)
        )),
        "signature_path" -> Json.JString(signature),
        "public_index_path" -> Json.JString(index)
      ))
      for {
        hashesOne <- inspectSharedLibraryVersion(root, runner, versionOne, "shared-v1")
        hashesTwo <- inspectSharedLibraryVersion(root, runner, versionTwo, "shared-v2")
        signatureOne = root.resolve("shared-v1.sig").toString
        signatureTwo = root.resolve("shared-v2.sig").toString
        indexOne = root.resolve("shared-v1-index.json").toString
        indexTwo = root.resolve("shared-v2-index.json").toString
        artifactOne = root.resolve("shared-v1.lib").toString
        artifactTwo = root.resolve("shared-v2.lib").toString
        scopeJson = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "members" -> Json.JArray(Vector(Json.JString(one.asString), Json.JString(two.asString))),
          "dependencies" -> Json.JArray(Vector(
            dependency(one, versionOne), dependency(two, versionTwo)
          )),
          "imports" -> Json.JArray(Vector(
            imported(versionOne, signatureOne, indexOne), imported(versionTwo, signatureTwo, indexTwo)
          )),
          "bindings" -> Json.JArray(Vector(
            binding(one, versionOne, signatureOne), binding(two, versionTwo, signatureTwo)
          ))
        )).render
        dependencyContext = Json.JObject(List(
          "schema_version" -> Json.JNumberStr("1"),
          "libraries" -> Json.JArray(Vector(
            Json.JObject(List(
              "name" -> Json.JString("shared"), "version" -> Json.JString(versionOne),
              "path" -> Json.JString(artifactOne),
              "hashes" -> Json.JArray(hashesOne.map(Json.JString(_)).toVector)
            )),
            Json.JObject(List(
              "name" -> Json.JString("shared"), "version" -> Json.JString(versionTwo),
              "path" -> Json.JString(artifactTwo),
              "hashes" -> Json.JArray(hashesTwo.map(Json.JString(_)).toVector)
            ))
          ))
        )).render
        _ <- write(root, "scope.json", scopeJson)
        _ <- write(root, "dependencies.json", dependencyContext)
        _ <- write(root, "toolchain.json", emptyToolchain)
        _ <- write(root, "Client-One.bosatsu",
          "package Client/One\nfrom Shared/Api import pick\nexport main\nmain = pick(1)\n")
        _ <- write(root, "Client-Two.bosatsu",
          "package Client/Two\nfrom Shared/Api import pick\nexport main\nmain = pick(True)\n")
        args = List(
          "compile-packages", "--src", "Client-One.bosatsu", "--src", "Client-Two.bosatsu",
          "--direct-interface", signatureOne, "--direct-interface", signatureTwo,
          "--scope-map", "scope.json", "--dependency-context", "dependencies.json",
          "--toolchain-manifest", "toolchain.json", "--packages-out", "scoped.pkg",
          "--sig-out", "scoped.sig"
        )
        compiled <- runner.run(root, args)
        _ = success(compiled)
        scopeMap <- PackageBindings.read(
          IOPlatformIO,
          root.resolve("scope.json"),
          List(root.resolve("shared-v1.sig"), root.resolve("shared-v2.sig"))
        )
        context <- LibraryInspection.readDependencyContext(IOPlatformIO, root.resolve("dependencies.json"))
        toolchain <- ToolchainManifest.read(IOPlatformIO, root.resolve("toolchain.json"))
        importedInputs <- LibraryInspection.loadImportedInputs(
          IOPlatformIO, scopeMap, context, toolchain
        )(using IOPlatformIO.canPromiseF)
        sourceDependencies = List(
          PackageBindings.ScopeDependency(
            PackageBindings.Scope.Member(one), api,
            PackageBindings.Scope.Library(dev.bosatsu.library.Name("shared") ->
              dev.bosatsu.library.Version.parser.parseAll(versionOne).toOption.get)
          ),
          PackageBindings.ScopeDependency(
            PackageBindings.Scope.Member(two), api,
            PackageBindings.Scope.Library(dev.bosatsu.library.Name("shared") ->
              dev.bosatsu.library.Version.parser.parseAll(versionTwo).toOption.get)
          )
        )
        checked <- IO.fromEither(PackageBindings.validateAgainstInspection(
          scopeMap, importedInputs.publicIndex, sourceDependencies,
      memberExposures = Map.empty,
      localInterfaces = Nil
        ).leftMap(CliException.Basic(_)))
        ownerInterfaces = importedInputs.signatureBundles.toList.flatMap {
          case (identity, signatureBundle) =>
            ProtoConverter.interfacesFromProto(signatureBundle).get.map { iface =>
              PackageBindings.PackageOwner(PackageBindings.Scope.Library(identity), iface.name) -> iface
            }
        }.toMap
        decoded <- IOPlatformIO.readPackagesScoped(root.resolve("scoped.pkg"), checked, ownerInterfaces)
        _ = assertEquals(decoded.map(_.pack.name), List(one, two))
        _ = assert(decoded.forall(_.context.size == 1))
      } yield ()
    }
  }

  test("scoped compilation carries external local signatures and exposure owners through decoding") {
    withRunner { (root, runner) =>
      val client = PackageName.parts("Local/Client")
      val facade = PackageName.parts("Local/Facade")
      val types = PackageName.parts("Local/Types")
      val tokenName = TypeName(Constructor("Token"))
      val token = DefinedType(types, tokenName, Nil, Nil)
      val typesIface: Package.Interface = Package(types, Nil, List(
        ExportedName.TypeName(tokenName.ident, Referant.DefinedT(token))
      ), ())
      val facadeIface: Package.Interface = Package(facade, Nil, List(
        ExportedName.Binding(
          Name("token"), Referant.Value(Type.const(types, tokenName))
        )
      ), ())
      val clientScope = scope("member", client.asString)
      val facadeScope = localSignatureScope(facade)
      val typesScope = localSignatureScope(types)
      val scopeMap = Json.JObject(List(
        "schema_version" -> Json.JNumberStr("1"),
        "members" -> Json.JArray(Vector(Json.JString(client.asString))),
        "dependencies" -> Json.JArray(Vector(
          Json.JObject(List(
            "from" -> clientScope,
            "package" -> Json.JString(facade.asString),
            "to" -> facadeScope
          )),
          Json.JObject(List(
            "from" -> clientScope,
            "package" -> Json.JString(types.asString),
            "to" -> typesScope
          ))
        )),
        "imports" -> Json.JArray(Vector.empty),
        "bindings" -> Json.JArray(Vector(
          Json.JObject(List(
            "referrer" -> clientScope,
            "package" -> Json.JString(facade.asString),
            "provider" -> facadeScope,
            "interface_path" -> Json.JString("facade.sig")
          )),
          Json.JObject(List(
            "referrer" -> facadeScope,
            "package" -> Json.JString(types.asString),
            "provider" -> typesScope,
            "interface_path" -> Json.JString("types.sig")
          )),
          Json.JObject(List(
            "referrer" -> clientScope,
            "package" -> Json.JString(types.asString),
            "provider" -> typesScope,
            "interface_path" -> Json.JString("types.sig")
          ))
        ))
      )).render

      for {
        _ <- write(root, "Client.bosatsu",
          "package Local/Client\nfrom Local/Facade import token\nexport value\nexposes Local/Types\nvalue = token\n")
        facadeBundle <- IO.fromTry(PackageSignatures.encode(List(facadeIface)))
        typesBundle <- IO.fromTry(PackageSignatures.encode(List(typesIface)))
        _ <- IOPlatformIO.writeInterfaceBundle(facadeBundle, root.resolve("facade.sig"))
        _ <- IOPlatformIO.writeInterfaceBundle(typesBundle, root.resolve("types.sig"))
        _ <- write(root, "scope.json", scopeMap)
        _ <- write(root, "dependencies.json", emptyDependencyContext)
        _ <- write(root, "toolchain.json", emptyToolchain)
        args = List(
          "compile-packages", "--src", "Client.bosatsu",
          "--direct-interface", "facade.sig", "--direct-interface", "types.sig",
          "--scope-map", "scope.json", "--dependency-context", "dependencies.json",
          "--toolchain-manifest", "toolchain.json", "--error-unused-interfaces",
          "--packages-out", "scoped.pkg", "--sig-out", "scoped.sig"
        )
        compiled <- runner.run(root, args)
        _ = success(compiled)
        bundle <- IOPlatformIO.read[proto.Packages](root.resolve("scoped.pkg"))
        requirements <- IO.fromTry(PackageSignatures.parseBuildRequirements(bundle.buildRequirements.get))
        _ = requirements match {
          case PackageSignatures.ParsedBuildRequirements.Scoped(List(member)) =>
            assertEquals(member.member, client)
            assertEquals(member.bindings.map(_.referrer), List(
              PackageBindings.Scope.Member(client), PackageBindings.Scope.Member(client),
              PackageBindings.Scope.Local(facade)
            ))
            assertEquals(member.bindings.map(_.owner.scope), List(
              PackageBindings.Scope.Local(facade), PackageBindings.Scope.Local(types),
              PackageBindings.Scope.Local(types)
            ))
            assertEquals(member.interfaces.map(_.packageName), List(facade, types).sorted)
          case other => fail(s"expected external local requirements, found $other")
        }
        storedTypesBundle <- IOPlatformIO.readInterfaceBundle(root.resolve("types.sig"))
        checkedMap <- PackageBindings.read(
          IOPlatformIO,
          root.resolve("scope.json"),
          List(Paths.get("facade.sig"), Paths.get("types.sig"))
        )
        localBundles = List(
          PackageSignatures.Input(Paths.get("facade.sig"), None) -> facadeBundle,
          PackageSignatures.Input(Paths.get("types.sig"), None) -> storedTypesBundle
        )
        localRoots <- IO.fromTry(PackageSignatures.localInterfaces(localBundles)(using IOPlatformIO.showPath))
        checked <- IO.fromEither(PackageBindings.validateAgainstInspection(
          checkedMap,
          PackageBindings.indexFromPackages(Nil, identities = Nil),
          List(PackageBindings.ScopeDependency(
            PackageBindings.Scope.Member(client), facade, PackageBindings.Scope.Local(facade)
          ), PackageBindings.ScopeDependency(
            PackageBindings.Scope.Member(client), types, PackageBindings.Scope.Local(types)
          )),
          Map(client -> Some(List(types))),
          localRoots
        ).leftMap(CliException.Basic(_)))
        decodedInterfaces <- IOPlatformIO.readInterfaces(List(
          root.resolve("facade.sig"), root.resolve("types.sig")
        ))
        ownerInterfaces = decodedInterfaces.map(iface =>
          PackageBindings.PackageOwner(PackageBindings.Scope.Local(iface.name), iface.name) -> iface
        ).toMap
        decoded <- IOPlatformIO.readPackagesScoped(root.resolve("scoped.pkg"), checked, ownerInterfaces)
        _ = assertEquals(decoded.map(_.pack.name), List(client))
        _ = assertEquals(decoded.head.context.map(_.owner.scope), List(
          PackageBindings.Scope.Local(facade), PackageBindings.Scope.Local(types),
          PackageBindings.Scope.Local(types)
        ))
        missingContext = ProtoConverter.packagesFromProtoScoped(
          bundle, checked, ownerInterfaces - PackageBindings.PackageOwner(
            PackageBindings.Scope.Local(types), types
          )
        )
        _ = assert(missingContext.failed.get.getMessage.contains("missing interface context"))
        changedTokenAlias = TypeAlias(types, tokenName, Nil, Type.IntType)
        changedTypesIface = (Package(types, Nil, List(
          ExportedName.TypeName(tokenName.ident, Referant.TypeAliasT(changedTokenAlias))
        ), ()): Package.Interface)
        switchedSignature = ProtoConverter.packagesFromProtoScoped(
          bundle,
          checked,
          ownerInterfaces.updated(
            PackageBindings.PackageOwner(PackageBindings.Scope.Local(types), types),
            changedTypesIface
          )
        )
        _ = assert(switchedSignature.failed.get.getMessage.contains("stale public interface"))
        originalRequirements = bundle.buildRequirements.get
        exposedBindingOmitted = originalRequirements.copy(members =
          originalRequirements.members.map { member =>
            if (member.memberName == client.asString)
              member.copy(
                bindings = member.bindings.filterNot(_.packageName == types.asString),
                interfaces = member.interfaces.filterNot(_.packageName == types.asString)
              )
            else member
          }
        )
        incomplete = ProtoConverter.packagesFromProtoScoped(
          bundle.copy(buildRequirements = Some(exposedBindingOmitted)), checked, ownerInterfaces
        )
        _ = assert(incomplete.failed.get.getMessage.contains("missing selected exposure binding"))
        _ <- IOPlatformIO.writeInterfaceBundle(proto.Interfaces(Nil), root.resolve("unused.sig"))
        unused <- runner.run(root, args ::: List("--direct-interface", "unused.sig"))
        _ = failure(unused)
        _ = assert(unused.stderr.contains("unused direct interfaces: unused.sig"), unused.stderr)
      } yield ()
    }
  }

  test("a wide direct signature selects only the reachable roots before decoding") {
    withRunner { (root, runner) =>
      val y = """package Y
                |export Token(), token
                |struct Token(number: Int)
                |token = Token(1)
                |""".stripMargin
      val sibling = """package Sibling
                      |from Y import Token, token
                      |export shared
                      |exposes Y
                      |def copy_token(Token(n): Token) -> Token: Token(n)
                      |shared = copy_token(token)
                      |""".stripMargin
      def client(dep: String, value: String, exposes: String): String =
        s"package Client\nfrom $dep import $value\nexport main\n${exposes}main = [$value]\n"
      for {
        _ <- write(root, "Y.bosatsu", y)
        _ <- write(root, "X.bosatsu", "package X\nexport x\nx = 1\n")
        _ <- write(root, "Sibling.bosatsu", sibling)
        types <- compile(runner, root, "y", List("Y"), Nil)
        _ = success(types)
        group <- compile(runner, root, "wide", List("X", "Sibling"), List("y"))
        _ = success(group)
        bundle <- IOPlatformIO.readInterfaceBundle(root.resolve("wide.sig"))
        index = bundle.interfaces.toList.map { i =>
          (ProtoConverter.iname(i), i.signatureDependencies.map(_.exposes.toList))
        }
        _ = assertEquals(index, List(("Sibling", Some(List("Y"))), ("X", Some(Nil))))
        _ <- write(root, "Client.bosatsu", client("X", "x", ""))
        x <- compile(runner, root, "client", List("Client"), List("wide"))
        _ = success(x)
        previous = bytes(root, "client.sig")
        // The absence of an explicit source exposure here does not erase the
        // import's transitive requirement: diagnose it before inference.
        _ <- write(root, "Client.bosatsu", client("Sibling", "shared", ""))
        missing <- compile(runner, root, "client", List("Client"), List("wide"))
        _ = failure(missing)
        _ = assert(missing.stderr.contains("Client -> Sibling -> Y"), missing.stderr)
        _ = assertEquals(bytes(root, "client.sig"), previous)
        _ <- write(root, "Client.bosatsu", client("Sibling", "shared", "exposes Y\n"))
        both <- compile(runner, root, "client", List("Client"), List("y", "wide"))
        _ = success(both)
        fromSources <- compile(runner, root, "all", List("Client", "Sibling", "Y"), Nil)
        _ = success(fromSources)
        // Corrupt the sibling's semantic tables. An X-only action does not
        // decode them, even though they share a physical input file.
        broken = bundle.copy(interfaces = bundle.interfaces.map { i =>
          if (ProtoConverter.iname(i) == "Sibling") i.copy(types = Nil) else i
        })
        _ <- IOPlatformIO.writeInterfaceBundle(broken, root.resolve("broken.sig"))
        _ <- write(root, "Client.bosatsu", client("X", "x", ""))
        unrelated <- compile(runner, root, "client", List("Client"), List("broken"))
        _ = success(unrelated)
        _ <- write(root, "Client.bosatsu", client("Sibling", "shared", "exposes Y\n"))
        relevant <- compile(runner, root, "client", List("Client"), List("broken", "y"))
        _ = failure(relevant)
        _ = assert(relevant.stderr.contains("broken.sig"), relevant.stderr)
      } yield ()
    }
  }

  test("opaque field edits keep the implementation dependency out of the signature") {
    withRunner { (root, runner) =>
      val hidden = "package Hidden\nexport Secret\nstruct Secret(n: Int)\n"
      val box = """package Box
                  |from Hidden import Secret
                  |export Box, size
                  |struct Box[a](value: a, secret: Secret)
                  |def size(Box(value, _): Box[Int]) -> Int: value
                  |""".stripMargin
      for {
        _ <- write(root, "Hidden.bosatsu", hidden)
        _ <- write(root, "Box.bosatsu", box)
        dep <- compile(runner, root, "hidden", List("Hidden"), Nil)
        _ = success(dep)
        opaque <- compile(runner, root, "box", List("Box"), List("hidden"))
        _ = success(opaque)
        first = bytes(root, "box.sig")
        encoded <- IOPlatformIO.readInterfaceBundle(root.resolve("box.sig"))
        entry = encoded.interfaces.head
        _ = assertEquals(entry.getSignatureDependencies.exposes.toList, Nil)
        _ = assert(!entry.strings.contains("Hidden"))
        _ = assertEquals(entry.definedTypes.head.constructors.size, 0)
        _ = assertEquals(entry.definedTypes.head.typeParams.size, 1)
        edited = box.replace("Box, size", "size, Box")
          .replace("secret: Secret", "private_field: Option[Secret]")
        _ <- write(root, "Box.bosatsu", edited)
        changed <- compile(runner, root, "box", List("Box"), List("hidden"))
        _ = success(changed)
        _ = assertEquals(bytes(root, "box.sig"), first)
        _ <- write(root, "Use.bosatsu",
          "package Use\nfrom Box import size\nexport run\nexposes Box\nrun = size\n")
        use <- compile(runner, root, "use", List("Use"), List("box"))
        _ = success(use)
        // The full package still supplies its private representation at link time.
        full <- IOPlatformIO.read[proto.Packages](root.resolve("box.pkg"))
        _ = assert(full.packages.head.definedTypes.exists(_.constructors.nonEmpty))
        pair <- IOPlatformIO.readInterfacesAndPackages(
          List(root.resolve("box.sig")), List(root.resolve("box.pkg"), root.resolve("hidden.pkg"))
        )
        _ = assert(pair._1.head.exports.flatMap(_.tag.definedType).forall(_.isOpaque))
      } yield ()
    }
  }
  test("a warm compiler cache tracks an exposed provider and ignores unrelated interfaces") {
    withRunner { (root, runner) =>
      val y = PackageName.parts("Y")
      val s = PackageName.parts("Sibling")
      val typeName = TypeName(Constructor("Payload"))
      def owner(rhs: Type): Package.Interface = {
        val alias = TypeAlias(y, typeName, Nil, rhs)
        Package(y, Nil, List(ExportedName.TypeName(typeName.ident, Referant.TypeAliasT(alias))), ())
      }
      val sibling: Package.Interface = Package(s, Nil,
        List(ExportedName.Binding(Name("shared"), Referant.Value(Type.const(y, typeName)))), ())
      def save(name: String, iface: Package.Interface): IO[Unit] =
        IO.fromTry(PackageSignatures.encode(List(iface)))
          .flatMap(IOPlatformIO.writeInterfaceBundle(_, root.resolve(name)))
      def keys: IO[List[String]] = IO.blocking(listPaths(root.resolve("cache/keys")))
      val args = List("check", "--input", "Client.bosatsu", "--cache_dir", "cache",
        "--interface", "s.sig", "--interface", "y.sig", "--interface", "other.sig")
      val other = PackageName.parts("Other")
      def unrelated(t: Type): Package.Interface =
        Package(other, Nil, List(ExportedName.Binding(Name("v"), Referant.Value(t))), ())
      for {
        _ <- write(root, "Client.bosatsu",
          "package Client\nfrom Sibling import shared\nmain = add(shared, 1)\n")
        _ <- save("s.sig", sibling)
        _ <- save("y.sig", owner(Type.IntType))
        _ <- save("other.sig", unrelated(Type.IntType))
        first <- runner.run(root, args)
        _ = success(first)
        firstKeys <- keys
        _ <- save("other.sig", unrelated(Type.StrType))
        warm <- runner.run(root, args)
        _ = success(warm)
        warmKeys <- keys
        _ = assertEquals(warmKeys, firstKeys)
        _ <- save("y.sig", owner(Type.StrType))
        invalid <- runner.run(root, args)
        _ = failure(invalid)
        _ = assert(invalid.stderr.contains("Client.bosatsu"), invalid.stderr)
      } yield ()
    }
  }

  test("compiled imports restore their public view and older packages retain the full provider") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Owner.bosatsu", "package Owner\nexport value, Hidden\nstruct Hidden(n: Int)\nvalue = 1\n")
        _ <- write(root, "Use.bosatsu", "package Use\nfrom Owner import value\nmain = value\n")
        original <- runner.run(root, List("check", "--input", "Owner.bosatsu", "--interface_out", "owner.sig"))
        _ = success(original)
        compiled <- compile(runner, root, "use", List("Use"), List("owner"))
        _ = success(compiled)
        bundle <- IOPlatformIO.readInterfaceBundle(root.resolve("owner.sig"))
        full <- IOPlatformIO.read[proto.Packages](root.resolve("use.pkg"))
        pack = full.packages.head
        _ = assert(pack.imports.forall(_.canonicalInterface))
        legacy = pack.withImports(pack.imports.map(_.withCanonicalInterface(false)))
        decoded <- IO.fromTry(ProtoConverter.packagesFromProto(bundle.interfaces, List(pack)))
        old <- IO.fromTry(ProtoConverter.packagesFromProto(bundle.interfaces, List(legacy)))
        owner <- IO.fromTry(ProtoConverter.interfaceFromProto(bundle.interfaces.head))
        name = owner.name
        _ = assertEquals(decoded._2.head.imports.find(_.pack.name == name).map(_.pack),
          Some(PackageSignatures.canonical(owner)))
        _ = assertEquals(old._2.head.imports.find(_.pack.name == name).map(_.pack), Some(owner))
        _ = assertNotEquals(owner, PackageSignatures.canonical(owner))
      } yield ()
    }
  }

  test("a cache hit cannot discard strict diagnostics") {
    withRunner { (root, runner) =>
      val args = List("check", "--input", "Bad.bosatsu", "--cache_dir", "cache")
      for {
        _ <- write(root, "Bad.bosatsu", "package Bad\nexport main\nunused = 42\nmain = 1\n")
        cold <- runner.run(root, args)
        warm <- runner.run(root, args)
        _ = failure(cold)
        _ = assert(cold.stderr.contains("unused value"), cold.stderr)
        _ = assertEquals(warm, cold)
      } yield ()
    }
  }

  test("legacy embedded aliases cannot override the owner on a warm or cold check") {
    withRunner { (root, runner) =>
      val owner = PackageName.parts("AOwner")
      val payload = TypeName(Constructor("Payload"))
      val client = "package Client\nfrom ZFacade import item\nmain = add(item, 1)\n"
      // Historical writers used a local table pointer to a foreign definition.
      // Keep that wire shape independent of the current interface encoder.
      def save(rhs: Type, facade: Boolean): IO[Unit] = {
        val alias = TypeAlias(owner, payload, Nil, rhs)
        val typ = ExportedName.TypeName(payload.ident, Referant.TypeAliasT(alias))
        val value = ExportedName.Binding(Name("item"), Referant.Value(Type.const(owner, payload)))
        val exports = if (facade) List(typ, value) else List(typ)
        val iface: Package.Interface = Package(owner, Nil, exports, ())
        IO.fromTry(ProtoConverter.interfaceToProto(iface)).flatMap { encoded =>
          val entry = if (facade) encoded.copy(strings = encoded.strings :+ "ZFacade",
            packageName = encoded.strings.size + 1) else encoded
          IOPlatformIO.writeInterfaceBundle(proto.Interfaces(List(entry)),
            root.resolve(if (facade) "facade.sig" else "owner.sig"))
        }
      }
      def check(cache: String) = runner.run(root, List("check", "--input", "Client.bosatsu",
        "--cache_dir", cache, "--interface", "facade.sig", "--interface", "owner.sig"))
      for {
        _ <- write(root, "Client.bosatsu", client)
        _ <- save(Type.IntType, facade = true)
        _ <- save(Type.IntType, facade = false)
        initial <- check("warm")
        _ = success(initial)
        keys <- IO.blocking(listPaths(root.resolve("warm/keys")))
        _ <- save(Type.StrType, facade = true)
        same <- check("warm")
        fresh <- check("fresh-valid")
        _ = success(same)
        _ = success(fresh)
        _ = assertEquals(listPaths(root.resolve("warm/keys")), keys)
        _ <- save(Type.StrType, facade = false)
        _ <- save(Type.IntType, facade = true)
        warm <- check("warm")
        cold <- check("fresh-invalid")
        _ = failure(warm)
        _ = assertEquals(warm, cold)
        _ = assert(warm.stderr.contains("found: String"), warm.stderr)
        // A symbolic re-export can also bind to a source in this invocation.
        // Use the same cache across owner edits and retain the old embedded RHS.
        localArgs = List("check", "--input", "Client.bosatsu", "--input", "AOwner.bosatsu",
          "--cache_dir", "local", "--interface", "facade.sig")
        local = "package AOwner\nexport Payload\ntype Payload = "
        _ <- write(root, "AOwner.bosatsu", local + "Int\n")
        goodLocal <- runner.run(root, localArgs)
        _ = success(goodLocal)
        _ <- write(root, "AOwner.bosatsu", local + "String\n")
        badLocal <- runner.run(root, localArgs)
        freshLocal <- runner.run(root, localArgs.map(s => if (s == "local") "fresh-local" else s))
        _ = failure(badLocal)
        _ = assertEquals(badLocal, freshLocal)
      } yield ()
    }
  }

  test("legacy type re-exports retain the same definition with an interface or implementation owner") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Owner.bosatsu", "package Owner\nexport Token\nstruct Token(n: Int)\n")
        built <- runner.run(root, List("check", "--input", "Owner.bosatsu", "--output", "owner.pkg",
          "--interface_out", "owner.sig"))
        _ = success(built)
        ifaces <- IOPlatformIO.readInterfaces(List(root.resolve("owner.sig")))
        facade = ifaces.head.copy(name = PackageName.parts("Facade"))
        wire <- IO.fromTry(ProtoConverter.interfacesToProto(List(facade)))
        _ <- IOPlatformIO.writeInterfaceBundle(wire, root.resolve("facade.sig"))
        fromSig <- IOPlatformIO.readInterfaces(List(root.resolve("facade.sig"), root.resolve("owner.sig")))
        fromPack <- IOPlatformIO.readInterfacesAndPackages(List(root.resolve("facade.sig")), List(root.resolve("owner.pkg")))
        _ = assertEquals(fromPack._1, fromSig.filter(_.name == facade.name))
      } yield ()
    }
  }

  test("an implementation cannot supply a private type to a foreign signature") {
    withRunner { (root, runner) =>
      val owner = PackageName.parts("Owner")
      for {
        _ <- write(root, "Owner.bosatsu",
          "package Owner\nexport number\nstruct Secret(n: Int)\ndef get(Secret(n)): n\nnumber = get(Secret(1))\n")
        built <- compile(runner, root, "owner", List("Owner"), Nil)
        _ = success(built)
        packs <- IOPlatformIO.readPackages(List(root.resolve("owner.pkg")))
        secret = packs.head.types.getType(owner, TypeName(Constructor("Secret"))).get
        facade: Package.Interface = Package(PackageName.parts("Facade"), Nil,
          List(ExportedName.TypeName(secret.name.ident, Referant.DefinedT(secret))), ())
        wire <- IO.fromTry(PackageSignatures.encode(List(facade)))
        _ <- IOPlatformIO.writeInterfaceBundle(wire, root.resolve("facade.sig"))
        result <- IOPlatformIO.readInterfacesAndPackages(
          List(root.resolve("facade.sig")), List(root.resolve("owner.pkg"))).attempt
        _ = assert(result.isLeft, "private Owner::Secret escaped through Facade")
      } yield ()
    }
  }

  test("library re-exports resolve foreign aliases against the supplied source or signature") {
    withRunner { (root, runner) =>
      val owner = PackageName.parts("Owner")
      val payload = TypeName(Constructor("Payload"))
      val facade: Package.Interface = Package(PackageName.parts("Facade"), Nil, List(
        ExportedName.TypeName(payload.ident, Referant.TypeAliasT(TypeAlias(owner, payload, Nil, Type.IntType))),
        ExportedName.Binding(Name("item"), Referant.Value(Type.const(owner, payload)))
      ), ())
      val check = List("check", "--input", "Client.bosatsu", "--pub_dep", "facade.bosatsu_lib")
      for {
        raw <- IO.fromTry(ProtoConverter.interfaceToProto(facade))
        lib = proto.Library(name = "facade", exportedIfaces = List(raw),
          descriptor = Some(proto.LibDescriptor(version = Some(proto.Version(minor = 1L)))))
        _ <- IOPlatformIO.writeBytes(root.resolve("facade.bosatsu_lib"), Bytes.fromMessage(lib))
        _ <- write(root, "Client.bosatsu", "package Client\nfrom Facade import item\nmain = add(item, 1)\n")
        _ <- write(root, "Owner.bosatsu", "package Owner\nexport Payload\ntype Payload = Int\n")
        ownerGroup <- compile(runner, root, "owner", List("Owner"), Nil)
        _ = success(ownerGroup)
        fromSignature <- runner.run(root, check ::: List("--interface", "owner.sig"))
        _ = success(fromSignature)
        sourceArgs = check ::: List("--input", "Owner.bosatsu", "--cache_dir", "cache")
        good <- runner.run(root, sourceArgs)
        _ = success(good)
        _ <- write(root, "Owner.bosatsu", "package Owner\nexport Payload\ntype Payload = String\n")
        bad <- runner.run(root, sourceArgs)
        _ = failure(bad)
        _ = assert(bad.stderr.contains("found: String"), bad.stderr)
      } yield ()
    }
  }

  test("an old value signature cannot export an unapplied type constructor after its owner changes kind") {
    withRunner { (root, runner) =>
      for {
        _ <- write(root, "Y.bosatsu", "package Y\nexport Token, token\nstruct Token(n: Int)\ntoken = Token(1)\n")
        _ <- write(root, "X.bosatsu", "package X\nfrom Y import token\nexport value\nexposes Y\nvalue = token\n")
        _ <- write(root, "Client.bosatsu", "package Client\nfrom X import value\nexport main\nexposes Y\nmain = value\n")
        y <- compile(runner, root, "y", List("Y"), Nil)
        _ = success(y)
        x <- compile(runner, root, "x", List("X"), List("y"))
        _ = success(x)
        good <- compile(runner, root, "client", List("Client"), List("x", "y"))
        _ = success(good)
        signature = bytes(root, "client.sig")
        check = (cache: String) => runner.run(root, List("check", "--input", "Y.bosatsu",
          "--input", "Client.bosatsu", "--interface", "x.sig", "--cache_dir", cache))
        cached <- check("warm")
        _ = success(cached)
        _ <- write(root, "Y.bosatsu", "package Y\nexport Token\nstruct Token[a](n: a)\n")
        bad <- compile(runner, root, "client", List("Client", "Y"), List("x"))
        _ = failure(bad)
        _ = assert(bad.stderr.contains("X::value") && bad.stderr.contains("kind"), bad.stderr)
        _ = assertEquals(bytes(root, "client.sig"), signature)
        warm <- check("warm")
        cold <- check("cold")
        _ = failure(warm)
        _ = assertEquals(warm, cold)
      } yield ()
    }
  }

  test("a private change to an exposed legacy owner leaves the consumer cache key stable") {
    withRunner { (root, runner) =>
      val owner = "package Y\nexport Token, token\nstruct Token(number: Int)\ntoken = Token(1)\n"
      val emit = List("check", "--input", "Y.bosatsu", "--interface_out", "y.sig")
      val check = List("check", "--input", "Client.bosatsu", "--cache_dir", "cache",
        "--interface", "mid.sig", "--interface", "y.sig", "--output", "client.pkg")
      for {
        _ <- write(root, "Y.bosatsu", owner)
        _ <- write(root, "Mid.bosatsu",
          "package Mid\nfrom Y import token\nexport shared\nexposes Y\nshared = token\n")
        _ <- write(root, "Client.bosatsu", "package Client\nfrom Mid import shared\nmain = [shared]\n")
        first <- runner.run(root, emit)
        _ = success(first)
        original = bytes(root, "y.sig")
        middle <- compile(runner, root, "mid", List("Mid"), List("y"))
        _ = success(middle)
        initial <- runner.run(root, check)
        _ = success(initial)
        keys = listPaths(root.resolve("cache/keys"))
        artifact = bytes(root, "client.pkg")
        _ <- write(root, "Y.bosatsu", owner.replace("number:", "private_field:"))
        edited <- runner.run(root, emit)
        _ = success(edited)
        _ = assertNotEquals(bytes(root, "y.sig"), original)
        warm <- runner.run(root, check)
        _ = success(warm)
        _ = assertEquals(listPaths(root.resolve("cache/keys")), keys)
        _ = assertEquals(bytes(root, "client.pkg"), artifact)
        cold <- runner.run(root, check.map(s => if (s == "cache") "cold" else s))
        _ = success(cold)
        _ = assertEquals(bytes(root, "client.pkg"), artifact)
      } yield ()
    }
  }

  test("an external exposure resolves to the actual source owner in the group") {
    withRunner { (root, runner) =>
      val client = "package Client\nfrom X import shared\nexport main\nexposes Y\nmain = [shared]\n"
      for {
        _ <- write(root, "Y.bosatsu",
          "package Y\nexport Token, token\nstruct Token(number: Int)\ntoken = Token(1)\n")
        _ <- write(root, "X.bosatsu",
          "package X\nfrom Y import token\nexport shared\nexposes Y\nshared = token\n")
        _ <- write(root, "Client.bosatsu", client)
        owner <- compile(runner, root, "y", List("Y"), Nil)
        _ = success(owner)
        facade <- compile(runner, root, "x", List("X"), List("y"))
        _ = success(facade)
        external <- compile(runner, root, "external", List("Client"), List("x", "y"))
        _ = success(external)
        mixed <- compile(runner, root, "mixed", List("Client", "Y"), List("x"))
        _ = success(mixed)
        expected <- IOPlatformIO.readInterfaceBundle(root.resolve("external.sig"))
        actual <- IOPlatformIO.readInterfaceBundle(root.resolve("mixed.sig"))
        _ = assertEquals(actual.interfaces.filter(i => ProtoConverter.iname(i) == "Client"), expected.interfaces)
        checkArgs = List("check", "--input", "Client.bosatsu", "--input", "Y.bosatsu",
          "--interface", "x.sig", "--cache_dir", "warm")
        checked <- runner.run(root, checkArgs)
        _ = success(checked)
        x <- IOPlatformIO.readInterfaceBundle(root.resolve("x.sig"))
        library = proto.Library(name = "facade", exportedIfaces = x.interfaces,
          descriptor = Some(proto.LibDescriptor(version = Some(proto.Version(minor = 1L)))))
        _ <- IOPlatformIO.writeBytes(root.resolve("x.bosatsu_lib"), Bytes.fromMessage(library))
        libArgs = List("check", "--input", "Client.bosatsu", "--input", "Y.bosatsu",
          "--pub_dep", "x.bosatsu_lib", "--cache_dir", "lib-cache")
        fromLibrary <- runner.run(root, libArgs)
        _ = success(fromLibrary)
        _ <- write(root, "Y.bosatsu", "package Y\nexport other\nother = 1\n")
        _ <- write(root, "Client.bosatsu", client.replace("[shared]", "shared"))
        bad <- compile(runner, root, "mixed", List("Y", "Client"), List("x"))
        _ = failure(bad)
        _ = assert(bad.stderr.contains("missing exported type") && bad.stderr.contains("Token"), bad.stderr)
        invalidWarm <- runner.run(root, checkArgs)
        invalidCold <- runner.run(root, checkArgs.map(a => if (a == "warm") "cold" else a))
        _ = failure(invalidWarm)
        _ = assertEquals(invalidWarm, invalidCold)
        invalidLib <- runner.run(root, libArgs)
        _ = failure(invalidLib)
        _ = assert(invalidLib.stderr.contains("missing exported type"), invalidLib.stderr)
        preserved <- IOPlatformIO.readInterfaceBundle(root.resolve("mixed.sig"))
        _ = assertEquals(preserved, actual)
      } yield ()
    }
  }

  test("implementation roundtrip preserves the order of private ProgTest and Test entries") {
    withRunner { (root, runner) =>
      val prog = """package Bosatsu/Prog
                   |export ProgTest()
                   |struct ProgTest(check: Test)
                   |""".stripMargin
      val app = """package App
                  |from Bosatsu/Prog import ProgTest
                  |prog_suite = ProgTest(Assertion(True, "private prog test"))
                  |late_suite = Assertion(True, "private plain test")
                  |""".stripMargin
      for {
        _ <- write(root, "Prog.bosatsu", prog)
        _ <- write(root, "App.bosatsu", app)
        result <- compile(runner, root, "tests", List("App", "Prog"), Nil)
        _ = success(result)
        members <- IOPlatformIO.readPackages(List(root.resolve("tests.pkg")))
        app = members.find(_.name == PackageName.parts("App")).getOrElse(fail("expected App"))
        _ = Package.testEntry(app) match {
          case Left(Package.TestDiscoveryError.PlainTestAfterProgTest(_, prog, late)) =>
            assertEquals(prog.sourceCodeRepr, "prog_suite")
            assertEquals(late.toList.map(_.sourceCodeRepr), List("late_suite"))
          case result => fail(s"lost test discovery order: $result")
        }
      } yield ()
    }
  }

  test("external declarations compile with the implicit Predef and require no native implementation") {
    withRunner { (root, runner) =>
      val service = """package Native
                      |export length
                      |external def length(input: String) -> Int
                      |""".stripMargin
      val use = """package Use
                  |from Native import length
                  |export answer
                  |answer = length("hello")
                  |""".stripMargin
      for {
        _ <- write(root, "Native.bosatsu", service)
        _ <- write(root, "Use.bosatsu", use)
        native <- compile(runner, root, "native", List("Native"), Nil)
        _ = success(native)
        members <- IOPlatformIO.readPackages(List(root.resolve("native.pkg")))
        _ = assertEquals(members.map(_.externalDefs.map(_.sourceCodeRepr)), List(List("length")))
        _ <- IO.blocking(Files.delete(root.resolve("native.pkg")))
        consumer <- compile(runner, root, "use", List("Use"), List("native"))
        _ = success(consumer)
        first = bytes(root, "use.sig")
        _ <- write(root, "Use.bosatsu", "package Bosatsu/Predef\nexport x\nx = 1\n")
        rejected <- compile(runner, root, "use", List("Use"), Nil)
        _ = failure(rejected)
        _ = assert(rejected.stderr.contains("Predef is implicit"), rejected.stderr)
        _ = assertEquals(bytes(root, "use.sig"), first)
      } yield ()
    }
  }

}
