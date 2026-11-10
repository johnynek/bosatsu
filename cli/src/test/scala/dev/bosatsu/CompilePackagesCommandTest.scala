package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.effect.IO
import dev.bosatsu.Identifier.{Constructor, Name}
import dev.bosatsu.rankn.{Type, TypeAlias}
import java.nio.file.{Files, Path}

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
        _ <- IOPlatformIO.writeBytes(root.resolve("facade.bosatsu_lib"), lib.toByteArray)
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
        _ <- IOPlatformIO.writeBytes(root.resolve("x.bosatsu_lib"), library.toByteArray)
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
