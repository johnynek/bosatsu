package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.syntax.all._
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.{Kind, Package, PackageMap, PackageName, Referant}

class LibConfigTest extends munit.FunSuite {

  private def dep(name: String, v: Version): proto.LibDependency =
    proto.LibDependency(
      name = name,
      desc = Some(proto.LibDescriptor(version = Some(v.toProto)))
    )

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

  test("duplicate package reports dependency paths") {
    val v = Version(1, 0, 0)

    val depA = dep("a", v)
    val depB = dep("b", v)
    val depC = dep("c", v)
    val depD = dep("d", v)

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
}
