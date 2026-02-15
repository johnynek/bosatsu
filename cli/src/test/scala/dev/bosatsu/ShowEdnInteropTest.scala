package dev.bosatsu

import dev.bosatsu.tool.ShowEdn
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import us.bpsm.edn.parser.{Parser => EdnParser, Parsers}

class ShowEdnInteropTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(100)

  test("edn-java parser accepts showDoc output") {
    forAll(Generators.genPackage(Gen.const(()), 5)) { packMap =>
      val packs = packMap.values.toList.map(Package.typedFunctor.void)
      val ifaces = packs.map(Package.interfaceOf)
      val rendered = ShowEdn.showDoc(packs, ifaces).render(120)

      val parser = Parsers.newParser(Parsers.defaultConfiguration())
      val parseable = Parsers.newParseable(rendered)
      val parsed: Object = parser.nextValue(parseable)
      val tail: Object = parser.nextValue(parseable)
      assert(!(parsed eq EdnParser.END_OF_INPUT), rendered)
      assert(tail eq EdnParser.END_OF_INPUT)
    }
  }
}
