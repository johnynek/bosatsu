package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.Validated
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.PackageMap

class DecodedLibraryTest extends munit.FunSuite {

  private def lib(
      name: String,
      v: Version,
      publicDeps: List[proto.LibDependency],
      unusedTransitive: List[proto.LibDependency] = Nil
  ): DecodedLibrary[Algo.Blake3] = {
    val protoLib = proto.Library(
      name = name,
      descriptor = Some(proto.LibDescriptor(version = Some(v.toProto))),
      publicDependencies = publicDeps,
      unusedTransitivePublicDependencies = unusedTransitive
    )
    DecodedLibrary(
      Name(name),
      v,
      HashValue[Algo.Blake3]("00"),
      protoLib,
      Nil,
      PackageMap.empty
    )
  }

  test("publicDepClosure includes transitive dependencies") {
    val v = Version(1, 0, 0)
    val libC = lib("c", v, Nil)
    val libB = lib("b", v, Library.dep("c", v) :: Nil)
    val libA = lib("a", v, Library.dep("b", v) :: Nil)
    val depMap = Map(
      (libA.name, libA.version) -> libA,
      (libB.name, libB.version) -> libB,
      (libC.name, libC.version) -> libC
    )

    val res = DecodedLibrary.publicDepClosure(libA :: Nil, depMap)
    val names = res.toOption match {
      case Some(libs) => libs.map(_.name.name).toSet
      case None       => fail("expected successful transitive closure")
    }
    assertEquals(names, Set("a", "b", "c"))
  }

  test("publicDepClosure reports missing version errors") {
    val v = Version(1, 0, 0)
    val bad = lib("a", v, Library.dep("b", Option.empty[Version]) :: Nil)
    val depMap = Map((bad.name, bad.version) -> bad)

    DecodedLibrary.publicDepClosure(bad :: Nil, depMap) match {
      case Validated.Invalid(nec) =>
        val hasMissingVersion = nec.toNonEmptyList.toList.exists {
          case DecodedLibrary.DepClosureError.MissingVersion(depLib) =>
            depLib.name.name == "a"
          case _ =>
            false
        }
        assert(hasMissingVersion)
      case Validated.Valid(_) =>
        fail("expected MissingVersion error")
    }
  }

  test("publicDepClosure reports missing transitive dependencies") {
    val v = Version(1, 0, 0)
    val libA = lib("a", v, Library.dep("b", v) :: Nil)
    val depMap = Map((libA.name, libA.version) -> libA)

    DecodedLibrary.publicDepClosure(libA :: Nil, depMap) match {
      case Validated.Invalid(nec) =>
        val hasMissingTransitive = nec.toNonEmptyList.toList.exists {
          case DecodedLibrary.DepClosureError.MissingTransitiveDep(name, ver) =>
            (name.name == "b") && (ver.render == v.render)
          case _ =>
            false
        }
        assert(hasMissingTransitive)
      case Validated.Valid(_) =>
        fail("expected MissingTransitiveDep error")
    }
  }
}
