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
import dev.bosatsu.{
  CompileOptions,
  LocationMap,
  Package,
  PackageMap,
  PackageName,
  Par,
  Parser
}
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
        .typeCheckParsed(nel, ifaces, "<predef>", CompileOptions.Default)
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
      val values = rootDl.lib.implementations.testEntries.toList.collect {
        case (pn, Right(entry)) => (pn, entry)
      }.sortBy(_._1)
      ClangGen(rootDl).renderTests(values) match {
        case Right(doc) =>
          val rendered = doc.render(120)
          assert(rendered.contains("bsts_test_argv_has_quiet"), rendered)
          assert(rendered.contains("bsts_test_run"), rendered)
          assert(rendered.contains("quiet"), rendered)
        case Left(err) =>
          fail(err.display.render(80))
      }
    }
  }

  test("clang gen emits bsts_test_run_prog for ProgTest entries") {
    val predefIface = Package.interfaceOf(PackageMap.predefCompiled)

    val progSrc =
      """package Bosatsu/Prog
        |
        |export Prog(), ProgTest(), pure
        |
        |enum Prog[e, a]:
        |  Pure(get: a)
        |
        |def pure[a](a: a) -> forall e. Prog[e, a]:
        |  Pure(a)
        |
        |struct ProgTest(test_fn: List[String] -> Prog[String, Test])
        |""".stripMargin

    val progPm = typeCheck(progSrc, predefIface :: Nil)
    val progPack = progPm.toMap(PackageName.parts("Bosatsu", "Prog"))
    val progIface = Package.interfaceOf(progPack)

    val rootSrc =
      """package Root/Main
        |
        |from Bosatsu/Prog import ProgTest, pure
        |
        |tests = ProgTest(_ -> pure(Assertion(True, "ok")))
        |""".stripMargin

    val rootPm = typeCheck(rootSrc, predefIface :: progIface :: Nil)

    val progVersion = Version(1, 0, 0)
    val rootVersion = Version(1, 0, 0)

    val progLib = DecodedLibrary[Algo.Blake3](
      name = Name("prog"),
      version = progVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "prog",
        descriptor =
          Some(proto.LibDescriptor(version = Some(progVersion.toProto)))
      ),
      interfaces = progIface :: Nil,
      implementations = progPm
    )

    val rootLib = DecodedLibrary[Algo.Blake3](
      name = Name("root"),
      version = rootVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "root",
        descriptor =
          Some(proto.LibDescriptor(version = Some(rootVersion.toProto)))
      ),
      interfaces = Nil,
      implementations = rootPm
    )

    val progDl = DecodedLibraryWithDeps(progLib, SortedMap.empty)
    val rootDl =
      DecodedLibraryWithDeps(
        rootLib,
        SortedMap(progDl.nameVersion -> progDl)
      )

    Par.withEC {
      val values = rootDl.lib.implementations.testEntries.toList.collect {
        case (pn, Right(entry)) => (pn, entry)
      }.sortBy(_._1)
      ClangGen(rootDl).renderTests(values) match {
        case Right(doc) =>
          val rendered = doc.render(120)
          assert(rendered.contains("bsts_test_run_prog"), rendered)
        case Left(err) =>
          fail(err.display.render(80))
      }
    }
  }

  test("clang gen includes Bosatsu/Prog observe external when referenced") {
    val predefIface = Package.interfaceOf(PackageMap.predefCompiled)

    val progSrc =
      """package Bosatsu/Prog
        |
        |export Prog(), ProgTest(), pure, observe, await
        |
        |external struct Prog[e: +*, a: +*]
        |
        |external def pure[a](a: a) -> forall e. Prog[e, a]
        |external def flat_map[e, a, b](prog: Prog[e, a], fn: a -> Prog[e, b]) -> Prog[e, b]
        |external def observe[a](a: a) -> forall e. Prog[e, Unit]
        |
        |def await(p, fn): p.flat_map(fn)
        |
        |struct ProgTest(test_fn: List[String] -> Prog[String, Test])
        |""".stripMargin

    val progPm = typeCheck(progSrc, predefIface :: Nil)
    val progPack = progPm.toMap(PackageName.parts("Bosatsu", "Prog"))
    val progIface = Package.interfaceOf(progPack)

    val rootSrc =
      """package Root/Main
        |
        |from Bosatsu/Prog import ProgTest, pure, observe, await
        |
        |tests = ProgTest(_ ->
        |  observe("probe").await(_ -> pure(Assertion(True, "ok")))
        |)
        |""".stripMargin

    val rootPm = typeCheck(rootSrc, predefIface :: progIface :: Nil)

    val progVersion = Version(1, 0, 0)
    val rootVersion = Version(1, 0, 0)

    val progLib = DecodedLibrary[Algo.Blake3](
      name = Name("prog"),
      version = progVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "prog",
        descriptor =
          Some(proto.LibDescriptor(version = Some(progVersion.toProto)))
      ),
      interfaces = progIface :: Nil,
      implementations = progPm
    )

    val rootLib = DecodedLibrary[Algo.Blake3](
      name = Name("root"),
      version = rootVersion,
      hashValue = HashValue[Algo.Blake3]("00"),
      protoLib = proto.Library(
        name = "root",
        descriptor =
          Some(proto.LibDescriptor(version = Some(rootVersion.toProto)))
      ),
      interfaces = Nil,
      implementations = rootPm
    )

    val progDl = DecodedLibraryWithDeps(progLib, SortedMap.empty)
    val rootDl =
      DecodedLibraryWithDeps(
        rootLib,
        SortedMap(progDl.nameVersion -> progDl)
      )

    Par.withEC {
      val values = rootDl.lib.implementations.testEntries.toList.collect {
        case (pn, Right(entry)) => (pn, entry)
      }.sortBy(_._1)
      ClangGen(rootDl).renderTests(values) match {
        case Right(doc) =>
          val rendered = doc.render(120)
          assert(rendered.contains("___bsts_g_Bosatsu_l_Prog_l_observe"), rendered)
        case Left(err) =>
          fail(err.display.render(80))
      }
    }
  }
}
