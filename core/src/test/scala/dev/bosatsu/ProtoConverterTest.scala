package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eq
import cats.data.NonEmptyList
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DefinedType, Type}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.util.{Failure, Success, Try}
import cats.implicits._

import Identifier.Constructor

class ProtoConverterTest extends munit.ScalaCheckSuite with ParTest {
  private given Eq[Package.Interface] =
    // Safe: Package.Interface is immutable and uses structural equals.
    Eq.fromUniversalEquals
  private given Eq[TypedExpr[Region]] =
    // Safe: TypedExpr[Region] is immutable and uses structural equals.
    Eq.fromUniversalEquals
  private given Eq[Package.Compiled] =
    // Safe: Package.Compiled is immutable and uses structural equals.
    Eq.fromUniversalEquals
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 100 else 5
    )

  private val genRegion: Gen[Region] =
    for {
      start <- Gen.choose(0, 200)
      length <- Gen.choose(0, 50)
    } yield Region(start, start + length)

  private val genSmallInt: Gen[Int] =
    Gen.choose(0, 20)

  private val genCompiledPackageSource: Gen[String] = {
    val genLiteralPackage =
      genSmallInt.map { value =>
        s"""package Proto/RoundTrip
           |
           |export main
           |
           |main = $value
           |""".stripMargin
      }

    val genIdentityPackage =
      genSmallInt.map { value =>
        s"""package Proto/RoundTrip
           |
           |export main
           |
           |def id(x: Int) -> Int:
           |  x
           |
           |main = id($value)
           |""".stripMargin
      }

    val genMatchPackage =
      for {
        someValue <- genSmallInt
        noneValue <- genSmallInt
      } yield
        s"""package Proto/RoundTrip
           |
           |export main
           |
           |def select(opt: Option[Int]) -> Int:
           |  match opt:
           |    case Some(v): v
           |    case None: $noneValue
           |
           |main = select(Some($someValue))
           |""".stripMargin

    Gen.frequency(
      (2, genLiteralPackage),
      (2, genIdentityPackage),
      (3, genMatchPackage)
    )
  }

  def law[A: Eq, B](a: A, fn: A => Try[B], gn: B => Try[A]) = {
    val maybeProto = fn(a)
    assert(maybeProto.isSuccess, maybeProto.toString)
    val proto = maybeProto.get

    val orig = gn(proto) match {
      case Success(o)   => o
      case Failure(err) =>
        err.printStackTrace
        fail(s"expected to deserialize: $err")
        sys.error(s"could not deserialize: $err")
    }

    lazy val diffIdx =
      a.toString
        .zip(orig.toString)
        .zipWithIndex
        .dropWhile { case ((a, b), _) => a == b }
        .headOption
        .map(_._2)
        .getOrElse(0)

    val context = 1000
    assert(
      Eq[A].eqv(a, orig),
      s"${a.toString.drop(diffIdx - context / 2).take(context)} != ${orig.toString.drop(diffIdx - context / 2).take(context)}"
    )
    // assert(Eq[A].eqv(a, orig), s"$a\n\n!=\n\n$orig")
  }

  def tabLaw[A: Eq, B](
      f: A => ProtoConverter.Tab[B]
  )(g: (ProtoConverter.SerState, B) => ProtoConverter.DTab[A]) = { (a: A) =>
    ProtoConverter.runTab(f(a)) match {
      case Success((ss, b)) =>
        val ds = ProtoConverter.DecodeState.init(ss.strings.inOrder)
        g(ss, b).run(ds) match {
          case Success(finalA) =>
            assert(Eq[A].eqv(a, finalA), s"$a\n\nnot equalto\n\n$finalA")
          case Failure(err) =>
            fail(s"on decode $b (from $a), got: ${err.toString}")
        }
      case Failure(err) =>
        fail(s"on encode $a, got: ${err.toString}")
    }
  }

  test("we can roundtrip types through proto") {
    val testFn = tabLaw(ProtoConverter.typeToProto(_: Type)) { (ss, idx) =>
      ProtoConverter.buildTypes(ss.types.inOrder).map(_(idx - 1))
    }

    forAll(rankn.NTypeGen.genDepth03)(testFn)
  }

  test("we can roundtrip patterns through proto") {
    val testFn = tabLaw(
      ProtoConverter.patternToProto(
        _: Pattern[(PackageName, Constructor), Type]
      )
    ) { (ss, idx) =>
      for {
        tps <- ProtoConverter.buildTypes(ss.types.inOrder)
        pats = ProtoConverter.buildPatterns(ss.patterns.inOrder).map(_(idx - 1))
        res <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
      } yield res
    }

    forAll(Generators.genCompiledPattern(5))(testFn)
  }

  test("we can roundtrip TypedExpr through proto") {
    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          expr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- expr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    forAll(
      Generators.genTypedExpr(genRegion, 4, rankn.NTypeGen.genDepth03)
    )(testFn)
  }

  test(
    "TypedExpr proto roundtrip keeps synthetic and backticked underscore names distinct"
  ) {
    val intType = rankn.Type.IntType
    val synthetic = Identifier.synthetic("x")
    val backticked = Identifier.Backticked("_x")
    val literal = TypedExpr.Literal(Lit.fromInt(1), intType, Region(1, 2))
    val expr = TypedExpr.Let(
      synthetic,
      literal,
      TypedExpr.Let(
        backticked,
        TypedExpr.Local(synthetic, intType, Region(2, 3)),
        TypedExpr.Local(backticked, intType, Region(3, 4)),
        RecursionKind.NonRecursive,
        Region(4, 5)
      ),
      RecursionKind.NonRecursive,
      Region(5, 6)
    )

    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          decodedExpr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- decodedExpr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    testFn(expr)
  }

  test("we can roundtrip Loop/Recur TypedExpr through proto") {
    val intType = rankn.Type.IntType
    val x = Identifier.Name("x")
    val xExpr = TypedExpr.Local(x, intType, Region(1, 2))
    val loopExpr = TypedExpr.Loop(
      NonEmptyList.one((x, TypedExpr.Literal(Lit.fromInt(1), intType, Region(2, 3)))),
      TypedExpr.Match(
        xExpr,
        NonEmptyList.of(
          TypedExpr.Branch(
            Pattern.Literal(Lit.fromInt(0)),
            None,
            xExpr
          )(using Region(3, 4)),
          TypedExpr.Branch(
            Pattern.WildCard,
            None,
            TypedExpr.Recur(NonEmptyList.one(xExpr), intType, Region(4, 5))
          )(using Region(5, 6))
        ),
        Region(6, 7)
      ),
      Region(7, 8)
    )

    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          expr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- expr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    testFn(loopExpr)
  }

  test("we can roundtrip Match kind through proto") {
    val intType = rankn.Type.IntType
    val x = Identifier.Name("x")
    val expr = TypedExpr.Match(
      TypedExpr.Local(x, intType, Region(1, 2)),
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.WildCard,
          None,
          TypedExpr.Literal(Lit.fromInt(0), intType, Region(2, 3))
        )(using Region(3, 4))
      ),
      Region(4, 5),
      Declaration.MatchKind.Recur
    )

    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          decoded = ProtoConverter.buildExprs(ss.expressions.inOrder).map(_(idx - 1))
          res <- decoded.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    testFn(expr)
  }

  test("we can roundtrip guarded match branches through proto") {
    val intType = rankn.Type.IntType
    val boolType = rankn.Type.BoolType
    val x = Identifier.Name("x")
    val xExpr = TypedExpr.Local(x, intType, Region(1, 2))
    val eqInt = TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("eq_Int"),
      rankn.Type.Fun(NonEmptyList.of(intType, intType), boolType),
      Region(2, 3)
    )
    val guardExpr = TypedExpr.App(
      eqInt,
      NonEmptyList.of(
        xExpr,
        TypedExpr.Literal(Lit.fromInt(1), intType, Region(3, 4))
      ),
      boolType,
      Region(4, 5)
    )
    val guardedMatch = TypedExpr.Match(
      xExpr,
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.WildCard,
          Some(guardExpr),
          TypedExpr.Literal(Lit.fromInt(7), intType, Region(5, 6))
        )(using Region(6, 7))
      ),
      Region(7, 8)
    )

    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          expr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- expr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    testFn(guardedMatch)
  }

  test("we can roundtrip match branch pattern regions through proto") {
    val intType = rankn.Type.IntType
    val x = Identifier.Name("x")
    val xExpr = TypedExpr.Local(x, intType, Region(1, 2))
    val matchExpr = TypedExpr.Match(
      xExpr,
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.Literal(Lit.fromInt(1)),
          None,
          TypedExpr.Literal(Lit.fromInt(7), intType, Region(2, 3))
        )(using Region(10, 11)),
        TypedExpr.Branch(
          Pattern.WildCard,
          None,
          TypedExpr.Literal(Lit.fromInt(9), intType, Region(3, 4))
        )(using Region(11, 12))
      ),
      Region(4, 5)
    )

    val testFn = tabLaw(ProtoConverter.typedExprToProto(_: TypedExpr[Region])) {
      (ss, idx) =>
        for {
          tps <- ProtoConverter.buildTypes(ss.types.inOrder)
          pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
          patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
          expr = ProtoConverter
            .buildExprs(ss.expressions.inOrder)
            .map(_(idx - 1))
          res <- expr.local[ProtoConverter.DecodeState](
            _.withTypes(tps).withPatterns(patTab)
          )
        } yield res
    }

    testFn(matchExpr)
  }

  test("we can roundtrip interface through proto") {
    forAll(Generators.interfaceGen) { iface =>
      law(
        iface,
        ProtoConverter.interfaceToProto,
        ProtoConverter.interfaceFromProto
      )
    }
  }

  val sortedEq: Eq[List[Package.Interface]] =
    new Eq[List[Package.Interface]] {
      def eqv(l: List[Package.Interface], r: List[Package.Interface]) =
        // we are only sorting the left because we expect the right
        // to come out sorted
        l.sortBy(_.name.asString) === r
    }

  test("we can roundtrip interfaces through proto") {
    forAll(Generators.smallDistinctByList(Generators.interfaceGen)(_.name)) {
      ifaces =>
        law(
          ifaces,
          ProtoConverter.interfacesToProto[List],
          ProtoConverter.interfacesFromProto
        )(using sortedEq)
    }
  }

  test("we can roundtrip interfaces from full packages through proto") {
    forAll(Generators.genPackage(genRegion, 10)) { packMap =>
      val ifaces = packMap.iterator.map { case (_, p) =>
        Package.interfaceOf(p)
      }.toList
      law(
        ifaces,
        ProtoConverter.interfacesToProto[List],
        ProtoConverter.interfacesFromProto
      )(using sortedEq)
    }
  }

  test("test some hand written packages") {
    def ser(p: List[Package.Compiled]): Try[List[proto.Package]] =
      p.traverse(ProtoConverter.packageToProto)
    def deser(ps: List[proto.Package]): Try[List[Package.Compiled]] =
      ProtoConverter.packagesFromProto(Nil, ps).map { case (_, p) =>
        p.sortBy(_.name)
      }

    TestUtils.testInferred(
      List(
        """package Foo

export bar

bar = 1
"""
      ),
      "Foo",
      (packs, _) =>
        law(
          packs.toMap.values.toList.sortBy(_.name),
          ser,
          deser
        )
    )
  }

  test("we can roundtrip packages through proto") {
    forAll(Generators.genPackage(genRegion, 10)) { packMap =>
      def ser(p: List[Package.Compiled]): Try[List[proto.Package]] =
        p.traverse(ProtoConverter.packageToProto)
      def deser(ps: List[proto.Package]): Try[List[Package.Compiled]] =
        ProtoConverter.packagesFromProto(Nil, ps).map { case (_, p) =>
          p.sortBy(_.name)
        }

      val packList = packMap.toList.sortBy(_._1).map(_._2)
      law(packList, ser, deser)
    }
  }

  private def compilePackage(source: String): Package.Compiled = {
    val parsed = Parser.unsafeParse(Package.parser, source)
    val withPath =
      NonEmptyList.one((("proto-roundtrip", LocationMap(source)), parsed))

    Par.noParallelism {
      PackageMap
        .typeCheckParsed(withPath, Nil, "<predef>", CompileOptions.Default)
        .strictToValidated
        .fold(
          errs => fail(errs.toList.mkString("typecheck failed: ", "\n", "")),
          _.toMap.getOrElse(parsed.name, fail(s"missing package ${parsed.name}"))
        )
    }
  }

  @annotation.tailrec
  private def stripWrappers(
      expr: TypedExpr[Region]
  ): TypedExpr[Region] =
    expr match {
      case TypedExpr.Generic(_, in)       => stripWrappers(in)
      case TypedExpr.Annotation(in, _, _) => stripWrappers(in)
      case _                              => expr
    }

  private def roundTrip(pack: Package.Compiled): Package.Compiled =
    ProtoConverter.packageToProto(pack) match {
      case Failure(err) => fail(s"failed to encode package: $err")
      case Success(protoPack) =>
        ProtoConverter.packagesFromProto(Nil, protoPack :: Nil) match {
          case Failure(err) =>
            fail(s"failed to decode package: $err")
          case Success((_, decoded :: Nil)) =>
            decoded
          case Success((_, other)) =>
            fail(s"expected one package after roundtrip, got ${other.map(_.name)}")
        }
    }

  property("compiled package roundtrip law holds for generated sources") {
    forAll(genCompiledPackageSource) { source =>
      val compiled = compilePackage(source)
      assertEquals(roundTrip(compiled), compiled)
    }
  }

  test("compiled package roundtrip preserves typed-expression regions") {
    val source =
      """package Proto/Regions
        |
        |export main
        |
        |def id(x: Int) -> Int:
        |  x
        |
        |main = id(1)
        |""".stripMargin

    val compiled = compilePackage(source)
    assertEquals(roundTrip(compiled), compiled)
  }

  test("compiled package roundtrip preserves branch pattern regions") {
    val source =
      """package Proto/MatchRegions
        |
        |export select
        |
        |def select(opt: Option[Int]) -> Int:
        |  match opt:
        |    case Some(v): v
        |    case None: 0
        |""".stripMargin

    val compiled = compilePackage(source)
    val roundTripped = roundTrip(compiled)

    def branchesOf(pack: Package.Compiled): NonEmptyList[TypedExpr.Branch[Region]] = {
      val selectExpr = pack.lets.collectFirst {
        case (Identifier.Name("select"), _, te) => te
      }.getOrElse(fail(s"missing select in ${pack.name}"))

      stripWrappers(selectExpr) match {
        case TypedExpr.AnnotatedLambda(_, body, _) =>
          stripWrappers(body) match {
            case TypedExpr.Match(_, branches, _) => branches
            case other                           =>
              fail(s"expected select body to be a match expression, got $other")
          }
        case other =>
          fail(s"expected select to be a lambda, got $other")
      }
    }

    val originalBranches = branchesOf(compiled)
    val decodedBranches = branchesOf(roundTripped)

    assert(originalBranches.exists(b => !b.patternRegion.eqv(Region.empty)))
    assertEquals(decodedBranches.map(_.patternRegion), originalBranches.map(_.patternRegion))
  }

  test("packagesFromProto accepts interface/package name overlap") {
    forAll(Generators.genPackage(genRegion, 5)) { packMap =>
      val packs = packMap.values.toList
      val ifaces = packs.map(Package.interfaceOf(_))
      val res = for {
        ifacesProto <- ifaces.traverse(ProtoConverter.interfaceToProto)
        packsProto <- packs.traverse(ProtoConverter.packageToProto)
        decoded <- ProtoConverter.packagesFromProto(ifacesProto, packsProto)
      } yield decoded

      assert(res.isSuccess, res.toString)
    }
  }

  test(
    "packagesFromProto can decode package with external dependency interfaces"
  ) {
    TestUtils.testInferred(
      List(
        """package Other/Dep

export dep_value

dep_value = 42
""",
        """package Main

from Other/Dep import dep_value

export main

main = dep_value
"""
      ),
      "Main",
      (packs, _) => {
        val typedPacks = packs.toMap.values.toList
          .filterNot(_.name == PackageName.PredefName)

        val depPack = typedPacks.find(_.name.asString == "Other/Dep")
        val mainPack = typedPacks.find(_.name.asString == "Main")
        assert(depPack.nonEmpty, typedPacks.map(_.name.asString).toString)
        assert(mainPack.nonEmpty, typedPacks.map(_.name.asString).toString)

        val res = for {
          protoMain <- ProtoConverter.packageToProto(mainPack.get)
        } yield {
          val failWithoutDeps =
            ProtoConverter.packagesFromProto(Nil, protoMain :: Nil)
          val successWithDeps = ProtoConverter.packagesFromProto(
            Nil,
            protoMain :: Nil,
            Package.interfaceOf(depPack.get) :: Nil
          )
          (failWithoutDeps, successWithDeps)
        }

        assert(res.isSuccess, res.toString)
        val (failWithoutDeps, successWithDeps) = res.get
        assert(failWithoutDeps.isFailure, failWithoutDeps.toString)
        assert(successWithDeps.isSuccess, successWithDeps.toString)
        assert(successWithDeps.get._2.nonEmpty)
      }
    )
  }

  private def interfaceWithConstructorDefault(
      defaultBinding: Option[Identifier.Bindable],
      defaultType: Option[Type] = None
  ): Package.Interface = {
    val pack = PackageName.parts("Proto", "Defaults")
    val ctor = Identifier.Constructor("Rec")
    val dt = DefinedType[Kind.Arg](
      packageName = pack,
      name = TypeName(ctor),
      annotatedTypeParams = Nil,
      constructors = List(
        ConstructorFn[Kind.Arg](
          name = ctor,
          args = List(
            ConstructorParam(
              name = Identifier.Name("a"),
              tpe = Type.IntType,
              defaultBinding = defaultBinding,
              defaultType = defaultType
            )
          )
        )
      )
    )
    val cfn = dt.constructors.head
    Package[Nothing, Nothing, Referant[Kind.Arg], Unit](
      pack,
      Nil,
      ExportedName.Constructor(ctor, Referant.Constructor(dt, cfn)) :: Nil,
      ()
    )
  }

  private def firstConstructorDefault(
      iface: Package.Interface
  ): Option[Identifier.Bindable] =
    iface.exports.collectFirst {
      case ExportedName.Constructor(_, Referant.Constructor(_, cf))
          if cf.args.nonEmpty =>
        cf.args.head.defaultBinding
    }.flatten

  private def firstConstructorDefaultType(
      iface: Package.Interface
  ): Option[Type] =
    iface.exports.collectFirst {
      case ExportedName.Constructor(_, Referant.Constructor(_, cf))
          if cf.args.nonEmpty =>
        cf.args.head.defaultType
    }.flatten

  test("interface proto preserves constructor default bindings") {
    val expectedDefault = Some(Identifier.Name("default_value"))
    val iface = interfaceWithConstructorDefault(expectedDefault)
    val protoIface = ProtoConverter.interfaceToProto(iface) match {
      case Success(p)   => p
      case Failure(err) => fail(s"failed to encode interface: $err")
    }

    val encodedDefaultIdx =
      protoIface.definedTypes.head.constructors.head.params.head.defaultBindingName
    assert(encodedDefaultIdx > 0, s"expected non-zero proto default index")

    val decoded = ProtoConverter.interfaceFromProto(protoIface) match {
      case Success(i)   => i
      case Failure(err) => fail(s"failed to decode interface: $err")
    }
    assertEquals(firstConstructorDefault(decoded), expectedDefault)
  }

  test("interface proto decodes missing constructor default field as None") {
    val iface = interfaceWithConstructorDefault(Some(Identifier.Name("default")))
    val protoIface = ProtoConverter.interfaceToProto(iface) match {
      case Success(p)   => p
      case Failure(err) => fail(s"failed to encode interface: $err")
    }

    val legacyProto = protoIface.copy(
      definedTypes = protoIface.definedTypes.map { dt =>
        dt.copy(
          constructors = dt.constructors.map { cf =>
            cf.copy(params = cf.params.map(_.copy(defaultBindingName = 0)))
          }
        )
      }
    )

    val decodedLegacy = ProtoConverter.interfaceFromProto(legacyProto) match {
      case Success(i)   => i
      case Failure(err) => fail(s"failed to decode legacy interface: $err")
    }
    assertEquals(firstConstructorDefault(decodedLegacy), None)
  }

  test("interface proto preserves constructor default helper types") {
    val expectedType = Some(
      Type.forAll(
        List((Type.Var.Bound("a"), Kind.Type)),
        Type.TyApply(Type.OptionType, Type.TyVar(Type.Var.Bound("a")))
      )
    )
    val iface = interfaceWithConstructorDefault(
      Some(Identifier.Name("default_value")),
      expectedType
    )
    val protoIface = ProtoConverter.interfaceToProto(iface) match {
      case Success(p)   => p
      case Failure(err) => fail(s"failed to encode interface: $err")
    }

    val encodedDefaultTypeIdx =
      protoIface.definedTypes.head.constructors.head.params.head.defaultTypeOf
    assert(encodedDefaultTypeIdx > 0, s"expected non-zero proto default type index")

    val decoded = ProtoConverter.interfaceFromProto(protoIface) match {
      case Success(i)   => i
      case Failure(err) => fail(s"failed to decode interface: $err")
    }
    assertEquals(firstConstructorDefaultType(decoded), expectedType)
  }

  test("interface proto decodes missing constructor default type field as None") {
    val iface = interfaceWithConstructorDefault(
      Some(Identifier.Name("default")),
      Some(Type.IntType)
    )
    val protoIface = ProtoConverter.interfaceToProto(iface) match {
      case Success(p)   => p
      case Failure(err) => fail(s"failed to encode interface: $err")
    }

    val legacyProto = protoIface.copy(
      definedTypes = protoIface.definedTypes.map { dt =>
        dt.copy(
          constructors = dt.constructors.map { cf =>
            cf.copy(params = cf.params.map(_.copy(defaultTypeOf = 0)))
          }
        )
      }
    )

    val decodedLegacy = ProtoConverter.interfaceFromProto(legacyProto) match {
      case Success(i)   => i
      case Failure(err) => fail(s"failed to decode legacy interface: $err")
    }
    assertEquals(firstConstructorDefaultType(decodedLegacy), None)
  }

}
