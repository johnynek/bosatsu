package dev.bosatsu.library

import _root_.bosatsu.{TypedAst => proto}
import cats.data.{NonEmptyList, Validated}
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.hashing.{Algo, Hashed}
import dev.bosatsu.{
  CompileOptions,
  LocationMap,
  MemoryMain,
  Package,
  PackageMap,
  PackageName,
  Par,
  Parser,
  ProtoConverter
}

class ReservedNamespaceTest extends munit.FunSuite {
  private type ErrorOr[A] = Either[Throwable, A]
  private type Lib = Hashed[Algo.Blake3, proto.Library]

  private val platform = MemoryMain[ErrorOr].platformIO
  import platform.{canPromiseF, moduleIOMonad}

  private def compile(src: String): Package.Compiled = {
    val pack = Parser.unsafeParse(Package.parser, src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    val pm = Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(errs => fail(errs.toList.mkString("\n")), identity)
    }
    pm.toMap(pack.name)
  }

  private val version = Version(1, 0, 0)

  private def library(name: String, packSrc: String, deps: List[Lib]): Lib = {
    val pack = compile(packSrc)
    val raw = proto.Library(
      name = name,
      descriptor = Some(proto.LibDescriptor(version = Some(version.toProto))),
      exportedIfaces =
        List(ProtoConverter.interfaceToProto(Package.interfaceOf(pack)).get),
      internalPackages = List(
        ProtoConverter.packageToProto(pack).get,
        // Every built library embeds the compiler's Predef.
        ProtoConverter.packageToProto(PackageMap.predefCompiled).get
      ),
      publicDependencies = deps.map(DecodedLibrary.toDep(_, version))
    )
    Hashed.viaBytes[Algo.Blake3, proto.Library](raw)(_.toByteArray)
  }

  private val anythingSrc = "package Bosatsu/Anything\nexport x\nx = 1\n"
  private val anything = PackageName.parts("Bosatsu", "Anything")

  test("only the core library may contain Bosatsu/ packages besides Predef") {
    val clean = library("clean", "package Clean/Lib\nexport x\nx = 1\n", Nil)
    assertEquals(LibConfig.validateReservedPackages(clean.arg), Validated.unit)

    val core = library("core_alpha", anythingSrc, Nil)
    assertEquals(LibConfig.validateReservedPackages(core.arg), Validated.unit)

    val bad = library("bad", anythingSrc, Nil)
    assertEquals(
      LibConfig.validateReservedPackages(bad.arg),
      Validated.invalidNec(
        LibConfig.Error.ReservedPackageInDependency(bad.arg, NonEmptyList.one(anything))
      )
    )
  }

  private def decodeAllError(root: Lib, all: List[Lib]): String = {
    val byName = all.map(lib => lib.arg.name -> lib).toMap
    val load: proto.LibDependency => MemoryMain.StateF[ErrorOr][Lib] =
      dep => moduleIOMonad.pure(byName(dep.name))
    DecodedLibraryWithDeps
      .decodeAll(root)(load)
      .run(MemoryMain.State.empty) match {
      case Right(_)  => fail("expected the dependency graph to be rejected")
      case Left(err) => Option(err.getMessage).getOrElse(err.toString)
    }
  }

  test("decoding a library graph rejects a non-core library with Bosatsu/ packages") {
    val bad = library("bad", anythingSrc, Nil)
    val mid = library("mid", "package Mid/Lib\nexport x\nx = 1\n", bad :: Nil)
    val app = library("app", "package App/Main\nexport x\nx = 1\n", mid :: Nil)
    val expected = "dependency bad 1.0.0 contains Bosatsu/Anything"

    // The root itself.
    val rootMsg = decodeAllError(bad, bad :: Nil)
    assert(rootMsg.contains(expected), rootMsg)
    // A transitive dependency, before it is decoded.
    val transitiveMsg = decodeAllError(app, app :: mid :: bad :: Nil)
    assert(transitiveMsg.contains(expected), transitiveMsg)
  }
}
