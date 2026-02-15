package dev.bosatsu.tool

import dev.bosatsu.{Generators, Package, Platform}
import dev.bosatsu.edn.Edn
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class ShowEdnRoundTripTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(
      if (Platform.isScalaJvm) 100 else 20
    )

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
}
