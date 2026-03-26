package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{
  CompileOptions,
  Json,
  Kind,
  LocationMap,
  Package,
  PackageMap,
  PackageName,
  Par,
  Referant
}
import dev.bosatsu.IorMethods.IorExtension
import scala.concurrent.duration.DurationInt

class LibConfigTest extends munit.FunSuite {
  override val munitTimeout = 90.seconds

  private def iface(pn: PackageName): Package.Interface =
    Package[Nothing, Nothing, Referant[Kind.Arg], Unit](pn, Nil, Nil, ())

  private def lib(
      name: String,
      v: Version,
      exports: List[PackageName],
      publicDeps: List[proto.LibDependency]
  ): DecodedLibrary[Algo.Blake3] = {
    val protoLib = proto.Library(
      name = name,
      descriptor = Some(proto.LibDescriptor(version = Some(v.toProto))),
      publicDependencies = publicDeps
    )

    DecodedLibrary(
      Name(name),
      v,
      HashValue[Algo.Blake3]("00"),
      protoLib,
      exports.map(iface),
      PackageMap.empty
    )
  }

  private def typeCheckOne(src: String): PackageMap.Compiled =
    Par.noParallelism {
      val pack = dev.bosatsu.Parser.unsafeParse(Package.parser, src)
      val nel =
        cats.data.NonEmptyList.one((("test", LocationMap(src)), pack))
      PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
    }

  private def typeCheckAll(srcs: List[String]): PackageMap.Compiled =
    Par.noParallelism {
      val parsed = cats.data.NonEmptyList.fromList(srcs.zipWithIndex.map { case (src, idx) =>
        ((idx.toString, LocationMap(src)), dev.bosatsu.Parser.unsafeParse(Package.parser, src))
      }).getOrElse(fail("expected at least one source"))

      PackageMap
        .typeCheckParsed(parsed, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
    }

  private def decodeConfig(jsonStr: String): LibConfig = {
    val json = Json.parserFile.parseAll(jsonStr) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse config json: $err")
    }

    Json.Reader[LibConfig].read(Json.Path.Root, json) match {
      case Right(value)         => value
      case Left((msg, got, jp)) =>
        fail(show"failed to decode config json: $msg, got=$got at $jp")
    }
  }
  test("duplicate package reports dependency paths") {
    val v = Version(1, 0, 0)

    val depA = Library.dep("a", v)
    val depB = Library.dep("b", v)
    val depC = Library.dep("c", v)
    val depD = Library.dep("d", v)

    val dupPack = PackageName.parts("Bosatsu", "Dup")
    val aPack = PackageName.parts("Bosatsu", "A")
    val bPack = PackageName.parts("Bosatsu", "B")

    val libC = lib("c", v, dupPack :: Nil, Nil)
    val libD = lib("d", v, dupPack :: Nil, Nil)
    val libA = lib("a", v, aPack :: Nil, depC :: Nil)
    val libB = lib("b", v, bPack :: Nil, depD :: Nil)

    val conf = LibConfig(
      name = Name("root"),
      repoUri = "repo",
      nextVersion = v,
      previous = None,
      exportedPackages = Nil,
      allPackages = Nil,
      publicDeps = depA :: Nil,
      privateDeps = depB :: Nil,
      defaultMain = None
    )

    val res = conf.validatePacks(
      previous = None,
      packs = Nil,
      deps = Nil,
      publicDepClosureLibs = List(libA, libB, libC, libD),
      prevPublicDepLibs = Nil
    )

    val dup = res match {
      case cats.data.Validated.Invalid(nec) =>
        nec.toList
          .collectFirst { case e: LibConfig.Error.DuplicatePackage =>
            e
          }
          .getOrElse(fail("missing DuplicatePackage error"))
      case cats.data.Validated.Valid(_) =>
        fail("expected DuplicatePackage error")
    }

    def showRef(ref: LibConfig.LibRef): String =
      s"${ref.name.name}:${ref.version}"

    val paths =
      dup.libs.toList.map(lp => lp.path.toList.map(showRef)).toSet

    val root = s"root:${v}"
    val expected = Set(
      List(root, s"a:${v}", s"c:${v}"),
      List(root, s"b:${v}", s"d:${v}")
    )

    assertEquals(paths, expected)
  }

  test("extra packages ignores Predef") {
    val pm = typeCheckOne(
      """package My/Hello
        |
        |x = 1
        |""".stripMargin
    )

    val predefPack = pm.toMap(PackageName.PredefName)
    val helloPack = pm.toMap(PackageName.parts("My", "Hello"))

    val conf = LibConfig(
      name = Name("root"),
      repoUri = "repo",
      nextVersion = Version(0, 0, 1),
      previous = None,
      exportedPackages = Nil,
      allPackages = Nil,
      publicDeps = Nil,
      privateDeps = Nil,
      defaultMain = None
    )

    val res = conf.validatePacks(
      previous = None,
      packs = List(predefPack, helloPack),
      deps = Nil,
      publicDepClosureLibs = Nil,
      prevPublicDepLibs = Nil
    )

    val extra = res match {
      case cats.data.Validated.Invalid(nec) =>
        nec.toList
          .collectFirst { case e: LibConfig.Error.ExtraPackages => e }
          .getOrElse(fail("missing ExtraPackages error"))
      case cats.data.Validated.Valid(_) =>
        fail("expected ExtraPackages error")
    }

    val names = extra.nel.toList.map(_.name)
    assertEquals(names, List(helloPack.name))
  }

  test("init defaults all_packages to .*") {
    val conf = LibConfig.init(Name("root"), "repo", Version(0, 0, 1))
    assertEquals(conf.allPackages.map(_.asString), List(".*"))
  }

  test("lib config defaults doc_base_url to none") {
    val conf = LibConfig.init(Name("root"), "repo", Version(0, 0, 1))
    val encoded = Json.Writer.write(conf).render
    assert(!encoded.contains("doc_base_url"), encoded)
    val decoded = decodeConfig(encoded)
    assertEquals(decoded.docBaseUrl, None)
  }

  test("lib config doc_base_url json round-trips") {
    val conf = LibConfig
      .init(Name("root"), "repo", Version(0, 0, 1))
      .copy(docBaseUrl = Some("https://docs.example.com/root/"))

    val encoded = Json.Writer.write(conf).render
    assert(encoded.contains("doc_base_url"), encoded)
    val decoded = decodeConfig(encoded)
    assertEquals(decoded.docBaseUrl, conf.docBaseUrl)
  }

  test("assemble writes doc_base_url into library metadata") {
    val pm = typeCheckOne(
      """package My/Hello
        |
        |export x,
        |
        |x = 1
        |""".stripMargin
    )
    val pack = pm.toMap(PackageName.parts("My", "Hello"))

    val conf = LibConfig(
      name = Name("root"),
      repoUri = "repo",
      nextVersion = Version(0, 0, 1),
      previous = None,
      exportedPackages = List(LibConfig.PackageFilter.Name(pack.name)),
      allPackages = List(LibConfig.PackageFilter.Name(pack.name)),
      publicDeps = Nil,
      privateDeps = Nil,
      defaultMain = None,
      docBaseUrl = Some("https://docs.example.com/root")
    )

    val assembled = conf.unvalidatedAssemble(
      previous = None,
      vcsIdent = "deadbeef",
      packs = List(pack),
      unusedTrans = Nil
    ) match {
      case Right(value) => value
      case Left(err)    => fail(show"failed to assemble library: ${err.toString}")
    }

    assertEquals(assembled.docBaseUrl, conf.docBaseUrl)
  }

  test("validatePacks rejects exposed private same-library packages") {
    val compiled = typeCheckAll(
      List(
        """package Same/Base
          |export Shared()
          |
          |struct Shared
          |""".stripMargin,
        """package Same/User
          |from Same/Base import Shared
          |export wrap
          |exposes Same/Base
          |
          |def wrap(value: Shared) -> Shared:
          |  value
          |""".stripMargin
      )
    )

    val basePack = compiled.toMap(PackageName.parts("Same", "Base"))
    val userPack = compiled.toMap(PackageName.parts("Same", "User"))

    val conf = LibConfig(
      name = Name("root"),
      repoUri = "repo",
      nextVersion = Version(0, 0, 1),
      previous = None,
      exportedPackages = List(LibConfig.PackageFilter.Name(userPack.name)),
      allPackages = List(
        LibConfig.PackageFilter.Name(basePack.name),
        LibConfig.PackageFilter.Name(userPack.name)
      ),
      publicDeps = Nil,
      privateDeps = Nil,
      defaultMain = None
    )

    val res = conf.validatePacks(
      previous = None,
      packs = List(basePack, userPack),
      deps = Nil,
      publicDepClosureLibs = Nil,
      prevPublicDepLibs = Nil
    )

    val err =
      res match {
        case cats.data.Validated.Invalid(nec) =>
          nec.toList
            .collectFirst { case e: LibConfig.Error.IllegalVisibleDep => e }
            .getOrElse(fail("missing IllegalVisibleDep error"))
        case cats.data.Validated.Valid(_) =>
          fail("expected IllegalVisibleDep error")
      }

    assertEquals(err.invalid, basePack.name)
  }
}
