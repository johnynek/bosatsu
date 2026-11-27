package dev.bosatsu

import _root_.bosatsu.{TypedAst => proto}
import cats.Eq
import cats.data.{Ior, NonEmptyList}
import dev.bosatsu.IorMethods.IorExtension
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DefinedType, Type, TypeAlias}
import dev.bosatsu.library.{LibraryIdentity, Name as LibraryName, Version}
import dev.bosatsu.tool.PackageBindings
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.{Failure, Success, Try}
import cats.implicits._

import Identifier.{Constructor, Name}

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
      Declaration.MatchKind.Recur,
      TypedExpr.Local(x, intType, Region(1, 2)),
      NonEmptyList.one(
        TypedExpr.Branch(
          Pattern.WildCard,
          None,
          TypedExpr.Literal(Lit.fromInt(0), intType, Region(2, 3))
        )(using Region(3, 4))
      ),
      Region(4, 5)
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

  test("TypedExpr proto encodes BoolGuard on legacy guardExpr field") {
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

    val encoded = ProtoConverter.runTab(
      ProtoConverter.typedExprToProto(guardedMatch)
    ).map { case (ss, idx) =>
      ss.expressions.inOrder(idx - 1)
    }

    encoded match {
      case Success(rootExprProto) =>
        rootExprProto.value match {
          case proto.TypedExpr.Value.MatchExpr(matchProto) =>
            val branch = matchProto.branches.head
            assertNotEquals(branch.guardExpr, 0)
            assertEquals(branch.matchGuard, None)
          case other =>
            fail(s"expected encoded match expr, got $other")
        }
      case Failure(err) =>
        fail(s"expected encoding to succeed, got $err")
    }
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

  test("interface proto preserves exported type aliases") {
    val compiled = compilePackage(
      """package Proto/AliasIface
        |
        |export Box, Foo, mkFoo
        |
        |struct Box(item)
        |
        |type Foo = Box[Int]
        |
        |mkFoo: Foo = Box(1)
        |""".stripMargin
    )
    val iface = Package.interfaceOf(compiled)
    law(iface, ProtoConverter.interfaceToProto, ProtoConverter.interfaceFromProto)
  }

  test("compiled package roundtrip preserves local type aliases") {
    val compiled = compilePackage(
      """package Proto/AliasPack
        |
        |export main
        |
        |struct Box(item)
        |
        |type Foo = Box[Int]
        |
        |def unwrap(box: Foo) -> Int:
        |  match box:
        |    case Box(v): v
        |
        |foo: Foo = Box(1)
        |
        |main = unwrap(foo)
        |""".stripMargin
    )

    assertEquals(roundTrip(compiled), compiled)
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

  test("TypedExpr proto roundtrip preserves MatchGuard branches") {
    val intType = rankn.Type.IntType
    val boolType = rankn.Type.BoolType
    val pairPack = PackageName.parts("Proto", "MatchGuard")
    val pairCtor = Constructor("Pair")
    val pairType =
      Type.TyConst(Type.Const.Defined(pairPack, TypeName(pairCtor)))
    val value = Identifier.Name("value")
    val even = Identifier.Name("even")
    val valueExpr = TypedExpr.Local(value, pairType, Region(1, 2))
    val eqInt = TypedExpr.Global(
      PackageName.PredefName,
      Identifier.Name("eq_Int"),
      Type.Fun(NonEmptyList.of(intType, intType), boolType),
      Region(2, 3)
    )
    val innerGuardExpr = TypedExpr.App(
      eqInt,
      NonEmptyList.of(
        TypedExpr.Local(even, intType, Region(3, 4)),
        TypedExpr.Literal(Lit.fromInt(4), intType, Region(4, 5))
      ),
      boolType,
      Region(5, 6)
    )
    val guardPattern =
      Pattern.PositionalStruct(
        (pairPack, pairCtor),
        Pattern.Var(even) :: Pattern.WildCard :: Nil
      )
    val expr = TypedExpr.Match(
      valueExpr,
      NonEmptyList.of(
        TypedExpr.Branch.fromGuardNode(
          Pattern.WildCard,
          Some(
            TypedExpr.MatchGuard(valueExpr, guardPattern, Some(innerGuardExpr))(using
              Region(6, 7)
            )
          ),
          TypedExpr.Local(even, intType, Region(7, 8))
        )(using Region(8, 9)),
        TypedExpr.Branch(
          Pattern.WildCard,
          None,
          TypedExpr.Literal(Lit.fromInt(0), intType, Region(9, 10))
        )(using Region(10, 11))
      ),
      Region(11, 12)
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

  test("TypedExpr proto decode rejects branches with both guard encodings set") {
    val intType = rankn.Type.IntType
    val value = Identifier.Name("value")
    val expr = TypedExpr.Match(
      TypedExpr.Local(value, intType, Region(1, 2)),
      NonEmptyList.of(
        TypedExpr.Branch(
          Pattern.WildCard,
          None,
          TypedExpr.Literal(Lit.fromInt(0), intType, Region(2, 3))
        )(using Region(3, 4))
      ),
      Region(4, 5)
    )

    val decoded = ProtoConverter.runTab(ProtoConverter.typedExprToProto(expr)).flatMap {
      case (ss, idx) =>
        val decodedExpr = for {
          rootExprProto <- ss.expressions.inOrder.lift(idx - 1) match {
            case Some(exprProto) => Success(exprProto)
            case None =>
              Failure(new Exception(s"missing root expression at index $idx"))
          }
          matchProto <- rootExprProto.value match {
            case proto.TypedExpr.Value.MatchExpr(m) => Success(m)
            case other =>
              Failure(new Exception(s"expected encoded match expr, got $other"))
          }
          mutatedRoot = rootExprProto.copy(
            value = proto.TypedExpr.Value.MatchExpr(
              matchProto.copy(
                branches = matchProto.branches.updated(
                  0,
                  matchProto.branches.head.copy(
                    guardExpr = 1,
                    matchGuard = Some(proto.MatchGuard())
                  )
                )
              )
            )
          )
          mutatedExprs = ss.expressions.inOrder.updated(idx - 1, mutatedRoot)
          ds = ProtoConverter.DecodeState.init(ss.strings.inOrder)
          res <- {
            for {
              tps <- ProtoConverter.buildTypes(ss.types.inOrder)
              pats = ProtoConverter.buildPatterns(ss.patterns.inOrder)
              patTab <- pats.local[ProtoConverter.DecodeState](_.withTypes(tps))
              res <- ProtoConverter
                .buildExprs(mutatedExprs)
                .map(_(idx - 1))
                .local[ProtoConverter.DecodeState](
                  _.withTypes(tps).withPatterns(patTab)
                )
            } yield res
          }.run(ds)
        } yield res

        decodedExpr
    }

    decoded match {
      case Failure(err) =>
        assert(err.getMessage.contains("both guardExpr and matchGuard set"))
      case Success(decodedValue) =>
        fail(s"expected invalid guard decode failure, got $decodedValue")
    }
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

  private def compilePackageWithInterfaces(
      source: String,
      interfaces: List[Package.Interface]
  ): Package.Compiled = {
    val parsed = Parser.unsafeParse(Package.parser, source)
    val result = Par.noParallelism {
      PackageMap.typeCheckParsed(
        NonEmptyList.one((("scoped", LocationMap(source)), parsed)),
        interfaces,
        "<predef>",
        CompileOptions.Default
      )
    }
    val compiled = result match {
      case Ior.Left(errors) => fail(errors.toList.mkString("typecheck failed: ", ", ", ""))
      case Ior.Right(packages) => packages
      case Ior.Both(_, packages) => packages
    }
    compiled.toMap(parsed.name)
  }

  private def checkedScopedBindings(
      members: List[PackageSignatures.MemberRequirements],
      publicPackages: List[PackageBindings.PublicPackage]
  ): PackageBindings.CheckedScopeMap[String] = {
    import PackageBindings.BindingKey
    val memberNames = members.map(_.member)
    val checkedBindings = SortedMap.from(
      members.flatMap(_.bindings).map { binding =>
        val key = BindingKey(binding.referrer, binding.packageName)
        key -> PackageBindings.CheckedBinding[String](
          key,
          binding.owner,
          None,
          List(binding.referrer, binding.owner.scope)
        )
      }
    )(using PackageBindings.bindingKeyOrdering)
    val index = PackageBindings.indexFromPackages(publicPackages, identities = Nil)
    val identitiesOrdering = Ordering.Tuple2(using LibraryName.orderingName, Version.versionOrdering)
    val scopeMap = PackageBindings.ScopeMap(
      members = SortedSet.from(memberNames),
      dependencies = Nil,
      imports = SortedMap.empty[LibraryIdentity, PackageBindings.ImportedScope[String]](
        using identitiesOrdering
      ),
      publicIndex = index,
      directInterfaces = SortedSet.empty[String],
      bindings = SortedMap.empty[BindingKey, PackageBindings.BindingRequest[String]](
        using PackageBindings.bindingKeyOrdering
      )
    )
    PackageBindings.CheckedScopeMap(scopeMap, index, checkedBindings, localInterfaces = Nil)
  }

  test("scoped bundle decoding keeps incompatible same-named providers per member") {
    import PackageBindings.{BindingKey, PackageOwner, Scope}

    val libraryOne = LibraryName("shared") -> Version.parser.parseAll("1.0.0").toOption.get
    val libraryTwo = LibraryName("shared") -> Version.parser.parseAll("2.0.0").toOption.get
    val ownerOne = PackageOwner(Scope.Library(libraryOne), PackageName.parts("Shared/Api"))
    val ownerTwo = PackageOwner(Scope.Library(libraryTwo), PackageName.parts("Shared/Api"))
    val apiOne = Package.interfaceOf(compilePackageWithInterfaces(
      "package Shared/Api\nexport pick\ndef pick(value: Int) -> Int: value\n",
      Nil
    ))
    val apiTwo = Package.interfaceOf(compilePackageWithInterfaces(
      "package Shared/Api\nexport pick\ndef pick(value: Bool) -> Bool: value\n",
      Nil
    ))
    val oneName = PackageName.parts("Decoder/One")
    val twoName = PackageName.parts("Decoder/Two")
    val apiName = PackageName.parts("Shared/Api")
    val one = compilePackageWithInterfaces(
      "package Decoder/One\nfrom Shared/Api import pick\nexport main\nmain = pick(1)\n",
      List(apiOne)
    )
    val two = compilePackageWithInterfaces(
      "package Decoder/Two\nfrom Shared/Api import pick\nexport main\nmain = pick(True)\n",
      List(apiTwo)
    )
    def makeMemberRequirements(member: PackageName, owner: PackageOwner, iface: Package.Interface) = {
      val encoded = PackageSignatures.requirement(iface).get
      PackageSignatures.MemberRequirements(
        member,
        List(PackageSignatures.RequirementBinding(
          Scope.Member(member), apiName, owner.scope
        )),
        List(PackageSignatures.InterfaceDigest(apiName, encoded.interfaceBlake3))
      )
    }
    val memberRequirements = List(
      makeMemberRequirements(oneName, ownerOne, apiOne),
      makeMemberRequirements(twoName, ownerTwo, apiTwo)
    )
    val buildRequirements = PackageSignatures.scopedRequirementsToProto(memberRequirements)
    val bundle = proto.Packages(
      packages = List(ProtoConverter.packageToProto(one).get, ProtoConverter.packageToProto(two).get),
      buildRequirements = Some(buildRequirements)
    )
    val checked = checkedScopedBindings(memberRequirements, List(
      PackageBindings.PublicPackage(libraryOne, apiName, SortedMap.empty),
      PackageBindings.PublicPackage(libraryTwo, apiName, SortedMap.empty)
    ))
    val supplied = Map(ownerOne -> apiOne, ownerTwo -> apiTwo)

    val decoded = ProtoConverter.packagesFromProtoScoped(bundle, checked, supplied).get
    assertEquals(decoded.map(_.pack.name), List(oneName, twoName))
    assert(decoded.forall(_.context.size == 1))
    val missingContext = ProtoConverter.packagesFromProtoScoped(
      bundle, checked, Map(ownerOne -> apiOne)
    )
    assert(missingContext.failed.get.getMessage.contains("missing interface context"))
    def importedPick(member: PackageName): Type =
      decoded.find(_.pack.name == member).get.pack.imports
        .find(_.pack.name == apiName).get.pack.exports
        .find(_.name == Name("pick")).get.tag match {
          case Referant.Value(tpe) => tpe
          case other => fail(s"expected pick value type, found $other")
        }
    assertEquals(importedPick(oneName), apiOne.exports.find(_.name == Name("pick")).get.tag match {
      case Referant.Value(tpe) => tpe
      case other => fail(s"expected provider value type, found $other")
    })
    assertEquals(importedPick(twoName), apiTwo.exports.find(_.name == Name("pick")).get.tag match {
      case Referant.Value(tpe) => tpe
      case other => fail(s"expected provider value type, found $other")
    })

    val firstBinding = BindingKey(Scope.Member(oneName), apiName)
    val switched = checked.copy(bindings = checked.bindings.updated(
      firstBinding,
      checked.bindings(firstBinding).copy(owner = ownerTwo)
    ))
    val equalSignatureContexts = supplied.updated(ownerTwo, apiOne)
    val rebound = ProtoConverter.packagesFromProtoScoped(bundle, switched, equalSignatureContexts)
    assert(rebound.failed.get.getMessage.contains("provider changed"))

    val stale = buildRequirements.copy(members = buildRequirements.members.map { record =>
      if (record.memberName == oneName.asString)
        record.copy(interfaces = record.interfaces.map(_.copy(interfaceBlake3 = "0" * 64)))
      else record
    })
    assert(ProtoConverter.packagesFromProtoScoped(
      bundle.copy(buildRequirements = Some(stale)), checked, supplied
    ).failed.get.getMessage.contains("stale public interface"))

    val missingRecord = buildRequirements.copy(members = buildRequirements.members.filterNot(
      _.memberName == twoName.asString
    ))
    assert(ProtoConverter.packagesFromProtoScoped(
      bundle.copy(buildRequirements = Some(missingRecord)), checked, supplied
    ).failed.get.getMessage.contains("members do not match"))
  }

  test("scoped decoding keeps source owners and rejects omitted exposure contexts") {
    import PackageBindings.{BindingKey, PackageOwner, Scope}


    val providerName = PackageName.parts("Source/Provider")
    val clientName = PackageName.parts("Source/Client")
    val provider = compilePackageWithInterfaces(
      "package Source/Provider\nexport value\nvalue = 1\n", Nil
    )
    val providerIface = Package.interfaceOf(provider)
    val otherProviderIface = Package.interfaceOf(compilePackageWithInterfaces(
      "package Source/Provider\nexport value\nvalue = True\n", Nil
    ))
    val client = compilePackageWithInterfaces(
      "package Source/Client\nfrom Source/Provider import value\nexport main\nmain = value\n",
      List(providerIface)
    )
    val sourceOwner = PackageOwner(Scope.Member(providerName), providerName)
    val sourceRequirements = List(
      PackageSignatures.MemberRequirements(providerName, Nil, Nil),
      PackageSignatures.MemberRequirements(
        clientName,
        List(PackageSignatures.RequirementBinding(
          Scope.Member(clientName), providerName, sourceOwner.scope
        )),
        Nil
      )
    )
    val sourceChecked = checkedScopedBindings(sourceRequirements, publicPackages = Nil)
    val libraryIdentity = LibraryName("private") -> Version.parser.parseAll("1.0.0").toOption.get
    val lookalikeOwner = PackageOwner(Scope.Library(libraryIdentity), providerName)
    val sourceBundle = proto.Packages(
      packages = List(ProtoConverter.packageToProto(client).get, ProtoConverter.packageToProto(provider).get),
      buildRequirements = Some(PackageSignatures.scopedRequirementsToProto(sourceRequirements))
    )
    val sourceDecoded = ProtoConverter.packagesFromProtoScoped(
      sourceBundle,
      sourceChecked,
      Map(lookalikeOwner -> otherProviderIface)
    ).get
    val decodedClient = sourceDecoded.find(_.pack.name == clientName).get.pack
    val decodedSourceValue = decodedClient.imports.find(_.pack.name == providerName).get.pack.exports
      .find(_.name == Name("value")).get.tag match {
        case Referant.Value(tpe) => tpe
        case other => fail(s"expected source value type, found $other")
      }
    val expectedSourceValue = providerIface.exports.find(_.name == Name("value")).get.tag match {
      case Referant.Value(tpe) => tpe
      case other => fail(s"expected provider value type, found $other")
    }
    assertEquals(decodedSourceValue, expectedSourceValue)
    val clientSourceKey = BindingKey(Scope.Member(clientName), providerName)
    val switchedSourceOwner = sourceChecked.copy(bindings = sourceChecked.bindings.updated(
      clientSourceKey,
      sourceChecked.bindings(clientSourceKey).copy(owner = lookalikeOwner)
    ))
    assert(ProtoConverter.packagesFromProtoScoped(sourceBundle, switchedSourceOwner,
      Map(lookalikeOwner -> otherProviderIface)).failed.get.getMessage.contains("provider changed"))

    val leafName = PackageName.parts("Exposure/Leaf")
    val facadeName = PackageName.parts("Exposure/Facade")
    val exposureClientName = PackageName.parts("Exposure/Client")
    val payload = TypeName(Constructor("Payload"))
    val leafAlias = TypeAlias(leafName, payload, Nil, Type.IntType)
    val leafIface: Package.Interface = Package(leafName, Nil, List(
      ExportedName.TypeName(payload.ident, Referant.TypeAliasT(leafAlias))
    ), ())
    val facadeIface: Package.Interface = Package(facadeName, Nil, List(
      ExportedName.Binding(Name("value"), Referant.Value(Type.const(leafName, payload)))
    ), ())
    assertEquals(facadeIface.exposedDepPackages, List(leafName))
    val exposureClient = compilePackageWithInterfaces(
      "package Exposure/Client\nfrom Exposure/Facade import value\nexport main\nmain = value\n",
      List(facadeIface, leafIface)
    )
    val facadeOwner = PackageOwner(Scope.Library(libraryIdentity), facadeName)
    val leafOwner = PackageOwner(Scope.Library(libraryIdentity), leafName)
    val fullRequirements = PackageSignatures.MemberRequirements(
      exposureClientName,
      List(
        PackageSignatures.RequirementBinding(Scope.Member(exposureClientName), facadeName, facadeOwner.scope),
        PackageSignatures.RequirementBinding(Scope.Library(libraryIdentity), leafName, leafOwner.scope)
      ),
      List(
        PackageSignatures.InterfaceDigest(facadeName,
          PackageSignatures.requirement(facadeIface).get.interfaceBlake3),
        PackageSignatures.InterfaceDigest(leafName,
          PackageSignatures.requirement(leafIface).get.interfaceBlake3)
      )
    )
    val exposureChecked = checkedScopedBindings(List(fullRequirements), List(
      PackageBindings.PublicPackage(libraryIdentity, facadeName, SortedMap(
        leafName -> PackageBindings.PublicProvider.Library(libraryIdentity, List(libraryIdentity))
      )),
      PackageBindings.PublicPackage(libraryIdentity, leafName, SortedMap.empty)
    ))
    val exposureProviders = Map(facadeOwner -> facadeIface, leafOwner -> leafIface)
    val exposureBundle = proto.Packages(
      List(ProtoConverter.packageToProto(exposureClient).get),
      Some(PackageSignatures.scopedRequirementsToProto(List(fullRequirements)))
    )
    val decodedExposure = ProtoConverter.packagesFromProtoScoped(
      exposureBundle, exposureChecked, exposureProviders
    )
    assert(decodedExposure.isSuccess, decodedExposure.failed.toOption.map(_.getMessage).getOrElse(""))

    val omittedLeaf = fullRequirements.copy(
      bindings = fullRequirements.bindings.filterNot(_.packageName == leafName),
      interfaces = fullRequirements.interfaces.filterNot(_.packageName == leafName)
    )
    val omittedBundle = exposureBundle.copy(
      buildRequirements = Some(PackageSignatures.scopedRequirementsToProto(List(omittedLeaf)))
    )
    assert(ProtoConverter.packagesFromProtoScoped(
      omittedBundle, exposureChecked, exposureProviders
    ).isFailure)
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
