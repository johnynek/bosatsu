package dev.bosatsu.tool

import dev.bosatsu.{Generators, Package, Platform}
import dev.bosatsu.rankn.{ConstructorFn, ConstructorParam, DefinedType, Type}
import dev.bosatsu.{ExportedName, Identifier, Kind, PackageName, Referant, TypeName}
import dev.bosatsu.edn.Edn
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class ShowEdnRoundTripTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 100 else 20
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

  test("showDoc output is parseable EDN") {
    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList.map(Package.typedFunctor.void)
      val ifaces = packs.map(Package.interfaceOf)
      val rendered = ShowEdn.showDoc(packs, ifaces).render(120)
      Edn.parseAll(rendered) match {
        case Right(_)  => ()
        case Left(err) => fail(s"failed to parse showDoc output as EDN: $err")
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

        val rendered = ShowEdn.showDoc(List(normalized), Nil).render(120)
        val showEdn = Edn.parseAll(rendered) match {
          case Right(value) => value
          case Left(err)    => fail(s"failed parsing showDoc EDN: $err")
        }

        val packageEdn = showEdn match {
          case EList(
                ESymbol("show") :: EKeyword("interfaces") :: _ :: EKeyword(
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
}
