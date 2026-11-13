package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.codegen.CompilationSource
import dev.bosatsu.hashing.Algo
import dev.bosatsu.library.{DecodedLibrary, DecodedLibraryWithDeps, Name, Version}
import dev.bosatsu.rankn.Type
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

import Int64Intrinsics.{Intrinsic, Violation}

class Int64IntrinsicsTest extends munit.FunSuite {
  private val coreInt64Src =
    Predef.loadFileInCompile("test_workspace/Int64.bosatsu")

  private val clientName = PackageName.parts("Client")
  private val clientSrc =
    """package Client
      |
      |from Bosatsu/Num/Int64 import Int64, eq_Int64, cmp_Int64
      |
      |export same, before
      |exposes Bosatsu/Num/Int64
      |
      |def same(a: Int64, b: Int64) -> Int: 1 if eq_Int64(a, b) else 0
      |
      |def before(a: Int64, b: Int64) -> Bool: cmp_Int64(a, b) matches LT
      |""".stripMargin

  private def typeCheckResult(
      srcs: List[String],
      ifaces: List[Package.Interface]
  ) = {
    val parsed = srcs.zipWithIndex.map { case (src, idx) =>
      ((s"test$idx", LocationMap(src)), Parser.unsafeParse(Package.parser, src))
    }
    Par.noParallelism {
      PackageMap.typeCheckParsed(
        NonEmptyList.fromListUnsafe(parsed),
        ifaces,
        "<predef>",
        CompileOptions.Default
      )
    }
  }

  private def typeCheck(
      srcs: List[String],
      ifaces: List[Package.Interface]
  ): PackageMap.Compiled =
    typeCheckResult(srcs, ifaces).strictToValidated.fold(
      errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
      identity
    )

  private def typeCheckErrors(src: String): List[PackageError] =
    typeCheckResult(src :: Nil, Nil).strictToValidated.fold(
      _.toList,
      pm => fail(s"expected type check failure, got: ${pm.toMap.keySet}")
    )

  // True if any node of `expr` satisfies `onExpr` or `onBool`.
  private def exists[A](expr: Matchless.Expr[A])(
      onExpr: Matchless.Expr[A] => Boolean,
      onBool: Matchless.BoolExpr[A] => Boolean
  ): Boolean = {
    def loop(e: Matchless.Expr[A]): Boolean =
      onExpr(e) || (e match {
        case Matchless.Lambda(captures, _, _, body) =>
          captures.exists(loop) || loop(body)
        case Matchless.WhileExpr(cond, effectExpr, _) =>
          loopBool(cond) || loop(effectExpr)
        case Matchless.App(fn, args) =>
          loop(fn) || args.exists(loop)
        case Matchless.Let(_, value, in) =>
          loop(value) || loop(in)
        case Matchless.LetMut(_, in) =>
          loop(in)
        case Matchless.If(cond, thenExpr, elseExpr) =>
          loopBool(cond) || loop(thenExpr) || loop(elseExpr)
        case Matchless.SwitchVariant(on, _, cases, default) =>
          loop(on) || cases.exists { case (_, branch) => loop(branch) } ||
            default.exists(loop)
        case Matchless.Always(cond, thenExpr) =>
          loopBool(cond) || loop(thenExpr)
        case Matchless.PrevNat(of) =>
          loop(of)
        case Matchless.GetEnumElement(arg, _, _, _) =>
          loop(arg)
        case Matchless.GetStructElement(arg, _, _) =>
          loop(arg)
        case _ =>
          false
      })

    def loopBool(b: Matchless.BoolExpr[A]): Boolean =
      onBool(b) || (b match {
        case Matchless.CompareLit(arg, _, _) =>
          loop(arg)
        case Matchless.CompareInt(left, _, right) =>
          loop(left) || loop(right)
        case Matchless.CompareInt64(left, _, right) =>
          loop(left) || loop(right)
        case Matchless.CompareFloat64(left, _, right) =>
          loop(left) || loop(right)
        case Matchless.EqualsNat(arg, _) =>
          loop(arg)
        case Matchless.And(left, right) =>
          loopBool(left) || loopBool(right)
        case Matchless.CheckVariant(arg, _, _, _) =>
          loop(arg)
        case Matchless.CheckVariantSet(arg, _, _, _) =>
          loop(arg)
        case Matchless.SetMut(_, value) =>
          loop(value)
        case Matchless.LetBool(_, value, in) =>
          loop(value) || loopBool(in)
        case Matchless.LetMutBool(_, in) =>
          loopBool(in)
        case Matchless.TrueConst =>
          false
      })

    loop(expr)
  }

  private def assertBuiltinCompares[K](
      lets: List[(Identifier.Bindable, Matchless.Expr[K])]
  ): Unit = {
    val byName = lets.toMap
    List("same" -> Intrinsic.EqInt64, "before" -> Intrinsic.CmpInt64).foreach {
      case (fn, intrinsic) =>
        val expr = byName.getOrElse(
          Identifier.Name(fn),
          fail(s"missing $fn in ${lets.map(_._1)}")
        )
        assert(
          exists(expr)(
            _ => false,
            {
              case Matchless.CompareInt64(_, _, _) => true
              case _                               => false
            }
          ),
          s"$fn: $expr"
        )
        assert(
          !exists(expr)(
            {
              case Matchless.Global(_, pack, name) =>
                (pack == Int64Intrinsics.packageName) && (name == intrinsic.name)
              case _ => false
            },
            _ => false
          ),
          s"$fn: $expr"
        )
    }
  }

  test("eq_Int64 and cmp_Int64 lower to the builtin compare through a package map") {
    val pm = typeCheck(coreInt64Src :: clientSrc :: Nil, Nil)
    val compiled = Par.withEC {
      CompilationSource
        .namespace[PackageMap.Typed[Any], Unit](PackageMap.toAnyTyped(pm))
        .compiled
    }
    assertBuiltinCompares(compiled(())(clientName))
  }

  test("eq_Int64 and cmp_Int64 lower to the builtin compare through a decoded library") {
    type Library = DecodedLibraryWithDeps[Algo.Blake3]
    val predef = Package.interfaceOf(PackageMap.predefCompiled)

    // Round trip through the library encoding so the core Int64 also passes
    // the decode-time check.
    def library(
        name: String,
        source: String,
        pack: PackageName,
        deps: List[Library]
    ): Library = {
      val ifaces = predef :: deps.flatMap(_.lib.interfaces)
      val compiled = typeCheck(source :: Nil, ifaces).toMap(pack)
      val version = Version(1, 0, 0)
      val raw = proto.Library(
        name = name,
        descriptor = Some(proto.LibDescriptor(version = Some(version.toProto))),
        exportedIfaces =
          List(ProtoConverter.interfaceToProto(Package.interfaceOf(compiled)).get),
        internalPackages = List(ProtoConverter.packageToProto(compiled).get),
        privateDependencies = deps.map(_.lib.toDep)
      )
      val bytes = raw.toByteArray
      val (exports, packages) =
        ProtoConverter
          .packagesFromProto(raw.exportedIfaces, raw.internalPackages, ifaces)
          .get
      DecodedLibraryWithDeps(
        DecodedLibrary[Algo.Blake3](
          Name(name),
          version,
          Algo.hashBytes[Algo.Blake3](bytes),
          raw,
          exports,
          PackageMap.fromIterable(packages)
        ),
        SortedMap.from(deps.map(d => d.nameVersion -> d))
      )
    }

    val core = library("core_alpha", coreInt64Src, Int64Intrinsics.packageName, Nil)
    val app = library("app", clientSrc, clientName, core :: Nil)
    val compiled = Par.withEC {
      CompilationSource
        .namespace[Library, (Name, Version)](app)
        .compiled
    }
    assertBuiltinCompares(compiled(app.nameVersion)(clientName))
  }

  private def checkSource(src: String): Option[Int64Intrinsics.Mismatch] = {
    // Build the typed package directly so the checker sees what customs sees.
    val pack = Parser.unsafeParse(Package.parser, src)
    val nel = NonEmptyList.one((("test", LocationMap(src)), pack))
    val typed = Par.noParallelism {
      PackageMap
        .typeCheckParsed(nel, Nil, "<predef>", CompileOptions.Default)
        .left
    }
    typed.toList.flatMap(_.toList).collect {
      case PackageError.Int64IntrinsicMismatch(m) => m
    } match {
      case Nil      => None
      case m :: Nil => Some(m)
      case many     => fail(s"expected one mismatch, got: $many")
    }
  }

  test("the core Int64 source passes the intrinsic check") {
    assertEquals(checkSource(coreInt64Src), None)
    val pm = typeCheck(coreInt64Src :: Nil, Nil)
    val pack = pm.toMap(Int64Intrinsics.packageName)
    assertEquals(Int64Intrinsics.checkPackage(pack), None)
    assertEquals(Int64Intrinsics.checkInterface(Package.interfaceOf(pack)), None)
  }

  test("other packages are never checked") {
    val pm = typeCheck(
      "package Fake/Int64\nexport eq_Int64\nexternal def eq_Int64(a: Int, b: Int) -> Int\n" :: Nil,
      Nil
    )
    val pack = pm.toMap(PackageName.parts("Fake", "Int64"))
    assertEquals(Int64Intrinsics.checkPackage(pack), None)
    assertEquals(Int64Intrinsics.checkInterface(Package.interfaceOf(pack)), None)
  }

  test("a source Bosatsu/Num/Int64 must match the core declarations") {
    def violations(src: String): List[Violation] =
      checkSource(src).fold(List.empty[Violation])(_.violations.toList)

    val header = "package Bosatsu/Num/Int64\n"
    assertEquals(
      violations(header + "export Int64\nstruct Int64(value: Int)\n"),
      Violation.Int64NotExternal :: Nil
    )
    assertEquals(
      violations(header + "export Int64\nexternal type Int64[a]\n"),
      Violation.Int64NotExternal :: Nil
    )
    assertEquals(
      violations(header + "export Int64\ntype Int64 = Int\n"),
      Violation.Int64NotExternal :: Nil
    )
    val letEq = checkSource(
      header + "export Int64, eq_Int64\nexternal type Int64\n" +
        "def eq_Int64(a: Int64, b: Int64) -> Bool: True\n"
    ).getOrElse(fail("expected a mismatch"))
    assertEquals(letEq.violations.toList, Violation.NotExternalDef(Intrinsic.EqInt64) :: Nil)
    assert(
      letEq.message.contains(
        "eq_Int64 must be declared `external def` with type (Int64, Int64) -> Bool"
      ),
      letEq.message
    )
    val wrongType = Type.Fun(Type.IntType, Type.IntType)
    assertEquals(
      violations(
        header + "export Int64, int_low_bits_to_Int64\nexternal type Int64\n" +
          "external def int_low_bits_to_Int64(i: Int) -> Int\n"
      ).map {
        case Violation.WrongType(i, found) => (i, found.sameAs(wrongType))
        case other                         => fail(s"unexpected: $other")
      },
      (Intrinsic.IntLowBitsToInt64, true) :: Nil
    )
    // Names that are not intrinsics are unconstrained.
    assertEquals(
      violations(
        header + "export Int64, add_Int64\nexternal type Int64\n" +
          "external def add_Int64(a: Int64, b: Int) -> Int\n"
      ),
      Nil
    )
  }

  test("the source diagnostic names the package, binding and expected type") {
    val errs = typeCheckErrors(
      "package Bosatsu/Num/Int64\nexport Int64, cmp_Int64\nexternal type Int64\n" +
        "external def cmp_Int64(a: Int64, b: Int64) -> Bool\n"
    )
    val mismatch = errs.collect { case e: PackageError.Int64IntrinsicMismatch => e }
    assertEquals(mismatch.length, 1, errs)
    assert(!PackageError.isPostponable(mismatch.head))
    val msg = mismatch.head.message(Map.empty, LocationMap.Colorize.None)
    assert(msg.contains("package Bosatsu/Num/Int64"), msg)
    assert(
      msg.contains("cmp_Int64 must have type (Int64, Int64) -> Comparison, found (Int64, Int64) -> Bool"),
      msg
    )
  }

  test("a local Bool is rendered so it differs from the Predef Bool") {
    val mismatch = checkSource(
      "package Bosatsu/Num/Int64\nexport Int64, Bool(), eq_Int64\nexternal type Int64\n" +
        "enum Bool: F, T\nexternal def eq_Int64(a: Int64, b: Int64) -> Bool\n"
    ).getOrElse(fail("expected a mismatch"))
    assert(
      mismatch.message.contains(
        "eq_Int64 must have type (Int64, Int64) -> Bosatsu/Predef::Bool, found (Int64, Int64) -> Bool"
      ),
      mismatch.message
    )
  }

  // Compile `src` under another package name, then rename it in the encoded
  // artifact, as an older compiler could have produced.
  private def renamedToInt64(src: String): (proto.Interface, proto.Package) = {
    val fakeName = "Fake/Num/Int64"
    val pack = typeCheck(
      src.replace("package Bosatsu/Num/Int64", s"package $fakeName") :: Nil,
      Nil
    ).toMap(PackageName.parse(fakeName).get)
    def rename(strings: Seq[String]): Seq[String] =
      strings.map(s => if (s == fakeName) Int64Intrinsics.packageName.asString else s)
    val iface = ProtoConverter.interfaceToProto(Package.interfaceOf(pack)).get
    val impl = ProtoConverter.packageToProto(pack).get
    (iface.withStrings(rename(iface.strings)), impl.withStrings(rename(impl.strings)))
  }

  private def assertDecodeRejects[A](result: Try[A], expected: String): Unit =
    result match {
      case Success(value) => fail(s"expected decode failure, got: $value")
      case Failure(err) =>
        val msg = Option(err.getMessage).getOrElse(err.toString)
        assert(msg.contains("Bosatsu/Num/Int64 must match the core library"), msg)
        assert(msg.contains(expected), msg)
    }

  test("decoding a Bosatsu/Num/Int64 package or interface that differs from core fails") {
    val (iface, impl) = renamedToInt64(
      "package Bosatsu/Num/Int64\nexport Int64, eq_Int64\nexternal type Int64\n" +
        "external def eq_Int64(a: Int64, b: Int64) -> Int\n"
    )
    val expected = "eq_Int64 must have type (Int64, Int64) -> Bool, found (Int64, Int64) -> Int"
    assertDecodeRejects(ProtoConverter.packagesFromProto(Nil, impl :: Nil), expected)
    assertDecodeRejects(ProtoConverter.interfaceFromProto(iface), expected)
  }

  test("decoding an interface whose Int64 is not an external type fails") {
    List(
      "export Int64()\nstruct Int64(value: Int)\n",
      "export Int64\nexternal type Int64[a]\n",
      "export Int64\ntype Int64 = Int\n"
    ).foreach { body =>
      val (iface, _) = renamedToInt64("package Bosatsu/Num/Int64\n" + body)
      assertDecodeRejects(
        ProtoConverter.interfaceFromProto(iface),
        "Int64 must be declared `external type Int64`"
      )
    }
  }

  test("decoding the core Int64 package and interface succeeds") {
    val pack = typeCheck(coreInt64Src :: Nil, Nil).toMap(Int64Intrinsics.packageName)
    val iface = ProtoConverter.interfaceToProto(Package.interfaceOf(pack)).get
    val impl = ProtoConverter.packageToProto(pack).get
    assert(ProtoConverter.packagesFromProto(Nil, impl :: Nil).isSuccess)
    assert(ProtoConverter.interfaceFromProto(iface).isSuccess)
  }
}
