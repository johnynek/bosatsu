package dev.bosatsu.tool

import cats.data.NonEmptyList
import dev.bosatsu.{
  CompileOptions,
  Generators,
  ImportMap,
  Json,
  LocationMap,
  Matchless,
  MatchlessFromTypedExpr,
  Package,
  PackageMap,
  Par,
  Pattern,
  Parser,
  Platform
}
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DefinedType, Type, TypeEnv}
import dev.bosatsu.{
  ExportedName,
  Identifier,
  Kind,
  PackageName,
  Program,
  RecursionKind,
  Referant,
  TypeName,
  TypedExpr
}
import dev.bosatsu.edn.Edn
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class ShowEdnRoundTripTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 40 else 4
    )

  private def interfaceWithDefaultType(
      defaultBinding: Option[Identifier.Bindable],
      defaultType: Option[Type]
  ): Package.Interface = {
    val pack = PackageName.parts("ShowEdn", "Defaults")
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

  private def firstConstructorParam(
      iface: Package.Interface
  ): ConstructorParam =
    iface.exports.collectFirst {
      case ExportedName.Constructor(_, Referant.Constructor(_, cf))
          if cf.args.nonEmpty =>
        cf.args.head
    }.getOrElse(fail("expected constructor with at least one field"))

  private def normalized(pack: Package.Typed[Unit]): Package.Typed[Unit] =
    ShowEdn.normalizeForRoundTrip(pack) match {
      case Right(value) => value
      case Left(err)    => fail(err)
    }

  private def sampleMatchlessShowValue(): Output.ShowValue.Matchless = {
    val source =
      """package ShowEdn/Sample
        |
        |export main
        |
        |main = 42
        |""".stripMargin
    val parsed = Parser.unsafeParse(Package.parser, source)
    val checked = Par.noParallelism {
      PackageMap.typeCheckParsed(
        NonEmptyList.one((("sample", LocationMap(source)), parsed)),
        Nil,
        "<predef>",
        CompileOptions.Default
      )
    }
    val pm = checked.toOption.getOrElse(fail(s"typecheck failed: $checked"))
    val packs = pm.toMap.values.toList.map(Package.typedFunctor.void)
    val compiled = Par.withEC {
      MatchlessFromTypedExpr.compile(
        (),
        pm,
        Matchless.LocalPassOptions.Default
      )
    }
    val request =
      ShowSupport.Request(
        selection = ShowSelection.Request(Nil, Nil, Nil),
        ir = Output.ShowIr.Matchless,
        compileOptions = CompileOptions.Default,
        matchlessPassOptions = Matchless.PassOptions.Default,
        packageNamesOnly = false
      )

    ShowSupport.matchlessShowValue(packs, request, pack =>
      compiled.getOrElse(pack.name, Nil)
    )
  }

  private def collectGlobalPackageAtoms(edn: Edn): List[Edn] =
    edn match {
      case Edn.EList(Edn.ESymbol("global") :: packEdn :: _ :: Nil) =>
        packEdn :: Nil
      case Edn.EList(items) =>
        items.flatMap(collectGlobalPackageAtoms)
      case Edn.EVector(items) =>
        items.flatMap(collectGlobalPackageAtoms)
      case Edn.EMap(items) =>
        items.flatMap { case (key, value) =>
          collectGlobalPackageAtoms(key) ::: collectGlobalPackageAtoms(value)
        }
      case _ =>
        Nil
    }

  private def collectGlobalPackageJsonAtoms(json: Json): List[Json] =
    json match {
      case Json.JArray(Vector(Json.JString("global"), packJson, _)) =>
        packJson :: Nil
      case Json.JArray(items) =>
        items.toList.flatMap(collectGlobalPackageJsonAtoms)
      case Json.JObject(fields) =>
        fields.flatMap { case (_, value) =>
          collectGlobalPackageJsonAtoms(value)
        }
      case _ =>
        Nil
    }

  private def collectPatternConstructorAtoms(edn: Edn): List[Edn] =
    edn match {
      case Edn.EList(Edn.ESymbol("pstruct") :: constructorEdn :: _ :: Nil) =>
        constructorEdn :: Nil
      case Edn.EList(items) =>
        items.flatMap(collectPatternConstructorAtoms)
      case Edn.EVector(items) =>
        items.flatMap(collectPatternConstructorAtoms)
      case Edn.EMap(items) =>
        items.flatMap { case (key, value) =>
          collectPatternConstructorAtoms(key) ::: collectPatternConstructorAtoms(value)
        }
      case _ =>
        Nil
    }

  private def collectPatternConstructorJsonAtoms(json: Json): List[Json] =
    json match {
      case Json.JArray(Vector(Json.JString("pstruct"), constructorJson, _)) =>
        constructorJson :: Nil
      case Json.JArray(items) =>
        items.toList.flatMap(collectPatternConstructorJsonAtoms)
      case Json.JObject(fields) =>
        fields.flatMap { case (_, value) =>
          collectPatternConstructorJsonAtoms(value)
        }
      case _ =>
        Nil
    }

  private def showValueWithMixedDepthGlobalPackages(): Output.ShowValue.Matchless =
    Output.ShowValue.Matchless(
      packages =
        Output.ShowValue.MatchlessPackage(
          name = PackageName.parts("ShowEdn", "Sample"),
          imports = Nil,
          exportedValues = Nil,
          externals = Nil,
          defs = List(
            Identifier.Name("main") ->
              Matchless.App(
                Matchless.Global(
                  (),
                  PackageName.parts("Bosatsu", "Prog"),
                  Identifier.Name("await")
                ),
                NonEmptyList.one(
                  Matchless.Global(
                    (),
                    PackageName.parts("Zafu", "Tool", "Cat"),
                    Identifier.Name("emit_stderr_line")
                  )
                )
              )
          )
        ) :: Nil,
      typedPasses = CompileOptions.Default.enabledTypedPasses,
      matchlessPasses = Matchless.PassOptions.Default.enabledPasses,
      packageNamesOnly = false
    )

  private def showValueWithMixedDepthPatternPackages(): Output.ShowValue.Typed = {
    val shallowPattern =
      Pattern.PositionalStruct(
        (PackageName.parts("Foo"), Identifier.Constructor("Box")),
        Pattern.Var(Identifier.Name("x")) :: Nil
      )
    val deepPattern =
      Pattern.PositionalStruct(
        (PackageName.parts("Foo", "Bar"), Identifier.Constructor("Box")),
        Pattern.Var(Identifier.Name("y")) :: Nil
      )
    val matchExpr =
      TypedExpr.Match(
        TypedExpr.Local(Identifier.Name("value"), Type.IntType, ()),
        NonEmptyList.of(
          TypedExpr.Branch(
            shallowPattern,
            None,
            TypedExpr.Local(Identifier.Name("x"), Type.IntType, ())
          ),
          TypedExpr.Branch(
            deepPattern,
            None,
            TypedExpr.Local(Identifier.Name("y"), Type.IntType, ())
          )
        ),
        ()
      )
    val prog =
      Program(
        types = TypeEnv.empty,
        lets =
          List(
            (
              Identifier.Name("main"),
              RecursionKind.NonRecursive,
              matchExpr
            )
          ),
        externalDefs = Nil,
        from = ()
      )
    val pack: Package.Typed[Unit] =
      Package(
        name = PackageName.parts("ShowEdn", "TypedPatterns"),
        imports = Nil,
        exports = Nil,
        program = (
          prog,
          ImportMap.empty[Package.Interface, NonEmptyList[Referant[Kind.Arg]]]
        )
      )

    Output.ShowValue.Typed(
      packages = List(pack),
      interfaces = Nil,
      typedPasses = CompileOptions.Default.enabledTypedPasses,
      packageNamesOnly = false
    )
  }

  test("package codec render/parse round trips normalized typed packages") {
    forAll(Generators.genPackage(Gen.const(()), 8)) { packMap =>
      packMap.values.foreach { pack0 =>
        val pack = normalized(Package.typedFunctor.void(pack0))
        val rendered = ShowEdn.packageCodec.render(pack, 120)
        val parsed = ShowEdn.packageCodec.parse(rendered) match {
          case Right(value) => value
          case Left(err)    =>
            fail(s"failed to parse encoded package ${pack.name.asString}: $err")
        }
        assertEquals(parsed, pack)
      }
    }
  }

  test("edn/json conversion round trips losslessly") {
    forAll(Generators.genEdn) { edn =>
      val json = ShowEdn.ednToJson(edn)
      val decoded = ShowEdn.jsonToEdn(json) match {
        case Right(value) => value
        case Left(err)    => fail(s"failed to decode generated json ${json.render}: $err")
      }
      assertEquals(decoded, edn)
    }
  }

  test("showDoc output is parseable EDN") {
    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList.map(Package.typedFunctor.void)
      val ifaces = packs.map(Package.interfaceOf)
      val rendered = ShowEdn.showDoc(packs, ifaces, packageNamesOnly = false).render(120)
      Edn.parseAll(rendered) match {
        case Right(_)  => ()
        case Left(err) => fail(s"failed to parse showDoc output as EDN: $err")
      }
    }
  }

  test("showJson output is parseable JSON with a tagged top-level form") {
    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList.map(Package.typedFunctor.void)
      val ifaces = packs.map(Package.interfaceOf)
      val rendered = ShowEdn.showJson(packs, ifaces, packageNamesOnly = false).render
      Json.parserFile.parseAll(rendered) match {
        case Right(Json.JObject(fields)) =>
          assertEquals(fields.toMap.get("$form"), Some(Json.JString("show")))
        case Right(other) =>
          fail(s"expected show json object, found: $other")
        case Left(err) =>
          fail(s"failed to parse showJson output as JSON: $err")
      }
    }
  }

  test("showDoc omits :externals for packages without external defs") {
    import Edn._

    def hasExternalsKey(packEdn: Edn): Boolean =
      packEdn match {
        case EList(ESymbol("package") :: args) =>
          args.grouped(2).exists {
            case EKeyword("externals") :: _ :: Nil => true
            case _                                 => false
          }
        case other =>
          fail(s"expected package form, found: ${Edn.toDoc(other).render(120)}")
      }

    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList.map(Package.typedFunctor.void)
      packs.foreach { pack =>
        val normalized = ShowEdn.normalizeForRoundTrip(pack) match {
          case Right(value) => value
          case Left(err)    => fail(err)
        }

        val rendered =
          ShowEdn.showDoc(List(normalized), Nil, packageNamesOnly = false).render(120)
        val showEdn = Edn.parseAll(rendered) match {
          case Right(value) => value
          case Left(err)    => fail(s"failed parsing showDoc EDN: $err")
        }

        val packageEdn = showEdn match {
          case EList(
                ESymbol("show") :: EKeyword("ir") :: _ :: EKeyword(
                  "typed-passes"
                ) :: _ :: EKeyword("interfaces") :: _ :: EKeyword(
                  "packages"
                ) :: EVector(packages) :: Nil
              ) =>
            packages.headOption.getOrElse(fail("missing package in show output"))
          case other =>
            fail(s"unexpected show output: ${Edn.toDoc(other).render(120)}")
        }

        if (normalized.program._1.externalDefs.isEmpty) {
          assert(!hasExternalsKey(packageEdn), rendered)
        }
      }
    }
  }

  test("interface codec round trips constructor defaults with default types") {
    val expectedDefault = Some(Identifier.Name("default_value"))
    val expectedType = Some(
      Type.forAll(
        List((Type.Var.Bound("a"), Kind.Type)),
        Type.TyApply(Type.OptionType, Type.TyVar(Type.Var.Bound("a")))
      )
    )
    val iface = interfaceWithDefaultType(expectedDefault, expectedType)
    val rendered = ShowEdn.interfaceCodec.render(iface, 120)
    val parsed = ShowEdn.interfaceCodec.parse(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse encoded interface: $err")
    }
    val param = firstConstructorParam(parsed)
    assertEquals(param.defaultBinding, expectedDefault)
      assertEquals(param.defaultType, expectedType)
  }

  test("matchless showDoc output is parseable EDN without typed-only sections") {
    import Edn._

    val show = sampleMatchlessShowValue()
    val rendered = ShowEdn.showDoc(show).render(120)
    val parsed = Edn.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse Matchless showDoc output: $err")
    }

    parsed match {
      case EList(
            ESymbol("show") :: EKeyword("ir") :: ESymbol("matchless") :: EKeyword(
              "typed-passes"
            ) :: _ :: EKeyword("matchless-passes") :: _ :: EKeyword(
              "packages"
            ) :: EVector(packages) :: Nil
          ) =>
        val packageEdn = packages.headOption.getOrElse(fail("missing package"))
        val packageFields = packageEdn match {
          case EList(ESymbol("package") :: _ :: _ :: args) =>
            args.grouped(2).collect { case EKeyword(k) :: value :: Nil =>
              k -> value
            }.toMap
          case other =>
            fail(s"unexpected package form: ${Edn.toDoc(other).render(120)}")
        }
        assert(!packageFields.contains("types"))
      case other =>
        fail(s"unexpected Matchless show output: ${Edn.toDoc(other).render(120)}")
    }
  }

  test("matchless showDoc renders package names consistently in globals") {
    val show = showValueWithMixedDepthGlobalPackages()

    val rendered = ShowEdn.showDoc(show).render(120)
    val parsed = Edn.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse Matchless showDoc output: $err")
    }
    val packageAtoms = collectGlobalPackageAtoms(parsed)

    assertEquals(
      packageAtoms.map {
        case Edn.ESymbol(value) => value
        case Edn.EString(value) => value
        case other              => fail(s"unexpected package atom in global: $other")
      },
      List("Bosatsu/Prog", "Zafu/Tool/Cat")
    )
    assertEquals(
      packageAtoms.map {
        case _: Edn.ESymbol => "symbol"
        case _: Edn.EString => "string"
        case other          => fail(s"unexpected package atom in global: $other")
      }.distinct,
      List("string")
    )
  }

  test("matchless showJson renders package names consistently in globals") {
    val rendered = ShowEdn.showJson(showValueWithMixedDepthGlobalPackages()).render

    val parsed = Json.parserFile.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse Matchless showJson output as JSON: $err")
    }
    val packageAtoms = collectGlobalPackageJsonAtoms(parsed)
    assertEquals(
      packageAtoms,
      List(
        Json.JObject(List("$str" -> Json.JString("Bosatsu/Prog"))),
        Json.JObject(List("$str" -> Json.JString("Zafu/Tool/Cat")))
      )
    )

    val decoded = ShowEdn.jsonToEdn(parsed) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to decode Matchless showJson output back to EDN: $err")
    }
    val decodedPackageAtoms = collectGlobalPackageAtoms(decoded)

    assertEquals(
      decodedPackageAtoms.map {
        case Edn.EString(value) => value
        case Edn.ESymbol(value) => value
        case other              => fail(s"unexpected package atom in global: $other")
      },
      List("Bosatsu/Prog", "Zafu/Tool/Cat")
    )
    assertEquals(
      decodedPackageAtoms.map {
        case _: Edn.EString => "string"
        case _: Edn.ESymbol => "symbol"
        case other          => fail(s"unexpected package atom in global: $other")
      }.distinct,
      List("string")
    )
  }

  test("typed showDoc renders constructor pattern package paths consistently") {
    val rendered = ShowEdn.showDoc(showValueWithMixedDepthPatternPackages()).render(120)
    val parsed = Edn.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse typed showDoc output: $err")
    }
    val constructorAtoms = collectPatternConstructorAtoms(parsed)

    assertEquals(
      constructorAtoms.map {
        case Edn.EString(value) => value
        case Edn.ESymbol(value) => value
        case other              => fail(s"unexpected constructor atom in pattern: $other")
      },
      List("Foo/Box", "Foo/Bar/Box")
    )
    assertEquals(
      constructorAtoms.map {
        case _: Edn.EString => "string"
        case _: Edn.ESymbol => "symbol"
        case other          => fail(s"unexpected constructor atom in pattern: $other")
      }.distinct,
      List("string")
    )
  }

  test("typed showJson renders constructor pattern package paths consistently") {
    val rendered = ShowEdn.showJson(showValueWithMixedDepthPatternPackages()).render
    val parsed = Json.parserFile.parseAll(rendered) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to parse typed showJson output as JSON: $err")
    }
    val constructorAtoms = collectPatternConstructorJsonAtoms(parsed)
    assertEquals(
      constructorAtoms,
      List(
        Json.JObject(List("$str" -> Json.JString("Foo/Box"))),
        Json.JObject(List("$str" -> Json.JString("Foo/Bar/Box")))
      )
    )

    val decoded = ShowEdn.jsonToEdn(parsed) match {
      case Right(value) => value
      case Left(err)    => fail(s"failed to decode typed showJson output back to EDN: $err")
    }
    val decodedConstructorAtoms = collectPatternConstructorAtoms(decoded)

    assertEquals(
      decodedConstructorAtoms.map {
        case Edn.EString(value) => value
        case Edn.ESymbol(value) => value
        case other              => fail(s"unexpected constructor atom in pattern: $other")
      },
      List("Foo/Box", "Foo/Bar/Box")
    )
    assertEquals(
      decodedConstructorAtoms.map {
        case _: Edn.EString => "string"
        case _: Edn.ESymbol => "symbol"
        case other          => fail(s"unexpected constructor atom in pattern: $other")
      }.distinct,
      List("string")
    )
  }

  test("matchless showJson output is parseable JSON without interfaces") {
    val rendered = ShowEdn.showJson(sampleMatchlessShowValue()).render
    Json.parserFile.parseAll(rendered) match {
      case Right(Json.JObject(fields)) =>
        val byKey = fields.toMap
        assertEquals(byKey.get("$form"), Some(Json.JString("show")))
        assert(!byKey.contains("interfaces"))
      case Right(other) =>
        fail(s"expected show json object, found: $other")
      case Left(err) =>
        fail(s"failed to parse Matchless showJson output as JSON: $err")
    }
  }
}
