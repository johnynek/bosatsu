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
