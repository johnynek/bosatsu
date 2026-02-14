package dev.bosatsu.codegen.clang

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.hashing.{Algo, HashValue}
import dev.bosatsu.library.{
  DecodedLibrary,
  DecodedLibraryWithDeps,
  Name,
  Version
}
import dev.bosatsu.{LocationMap, Package, PackageMap, PackageName, Par, Parser}
import scala.collection.immutable.SortedMap

class ClangGenLibraryDepsTest extends munit.FunSuite {

  private def typeCheck(
      src: String,
      ifaces: List[Package.Interface]
  ): PackageMap.Inferred = {
    val pack = Parser.unsafeParse(Package.parser(None), src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, ifaces, "<predef>")
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          identity
        )
    }
  }

  test("clang gen resolves globals across library deps") {
    val predefIface = Package.interfaceOf(PackageMap.predefCompiled)

    val depSrc =
      """package Dep/Lib
        |
        |export plusOne
        |
        |def plusOne(x: Int) -> Int: x
        |""".stripMargin

    val depPm = typeCheck(depSrc, predefIface :: Nil)
    val depPack = depPm.toMap(PackageName.parts("Dep", "Lib"))
    val depIface = Package.interfaceOf(depPack)

    val rootSrc =
      """package Root/Main
        |
        |from Dep/Lib import plusOne
        |
        |tests = TestSuite("dep",
        |  [Assertion(plusOne(1) matches 1, "ok")])
        |""".stripMargin

    val rootPm = typeCheck(rootSrc, predefIface :: depIface :: Nil)

    val depVersion = Version(1, 0, 0)
    val rootVersion = Version(1, 0, 0)

    val depLib = DecodedLibrary[Algo.Blake3](
      name = Name("dep"),
      version = depVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "dep",
        descriptor =
          Some(proto.LibDescriptor(version = Some(depVersion.toProto)))
      ),
      interfaces = depIface :: Nil,
      implementations = depPm
    )

    val rootLib = DecodedLibrary[Algo.Blake3](
      name = Name("root"),
      version = rootVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "root",
        descriptor = Some(
          proto.LibDescriptor(version = Some(rootVersion.toProto))
        )
      ),
      interfaces = Nil,
      implementations = rootPm
    )

    val depDl = DecodedLibraryWithDeps(depLib, SortedMap.empty)
    val rootDl =
      DecodedLibraryWithDeps(
        rootLib,
        SortedMap(depDl.nameVersion -> depDl)
      )

    Par.withEC {
      val values = rootDl.lib.implementations.testValues.toList.sorted
      ClangGen(rootDl).renderTests(values) match {
        case Right(_)  => ()
        case Left(err) =>
          fail(err.display.render(80))
      }
    }
  }
}
