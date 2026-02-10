package dev.bosatsu.codegen.python

import dev.bosatsu.Generators.bindIdentGen
import dev.bosatsu.{Par, TestUtils}
import dev.bosatsu.codegen.CompilationSource
import org.scalacheck.Prop.forAll

class PythonGenTest extends munit.ScalaCheckSuite {
  override def scalaCheckTestParameters =
    // PropertyCheckConfiguration(minSuccessful = 50000)
    super.scalaCheckTestParameters.withMinSuccessfulTests(5000)
  // PropertyCheckConfiguration(minSuccessful = 500)

  val PythonName = "[_A-Za-z][_A-Za-z0-9]*".r.pattern

  test("all escapes are valid python identifiers") {
    forAll(bindIdentGen) { b =>
      val str = PythonGen.escape(b).name

      assert(
        PythonName.matcher(str).matches(),
        s"escaped: ${b.sourceCodeRepr} to $str"
      )
    }
  }

  test("we can parse an example externals file") {
    val extstr = """
      { IO: { foo: bar.baz, quux: quux.quux_impl }, Bop: { foo: collections.queue } }
    """
    assertEquals(
      PythonGen.externalParser.parseAll(extstr).map(_ => ()),
      Right(())
    )
  }

  test("we can parse an example evals file") {
    val str = """
      {
        IO::IO: IOPy.run_io,
        Build/Foo::Bar: BuildImpl.run_build,
      }
    """
    assertEquals(
      PythonGen.evaluatorParser.parseAll(str).map(_ => ()),
      Right(())
    )
  }

  test("top-level lets around lambda avoid closure tuples") {
    val src =
      """|enum Nat:
         |  Zero
         |  Succ(pred: Nat)
         |
         |def count_chars(s):
         |  def loop(s, acc: Nat):
         |    recur s:
         |      case "": acc
         |      case "$.{_}${tail}": loop(tail, Succ(acc))
         |
         |  loop(s, Zero)
         |""".stripMargin

    TestUtils.checkPackageMap(src) { pm =>
      Par.withEC {
        val rendered = PythonGen.renderSource(pm, Map.empty, Map.empty)
        val doc = rendered(())(TestUtils.testPackage)._2
        val code = doc.render(120)

        val expected =
          "\n\n" +
            """def count_chars(___bs0):
    ___a4 = ___bs0
    ___a6 = 0
    ___a1 = 1
    ___t1 = True
    while ___t1:
        if ___a4 == "":
            ___a1 = 0
            ___a2 = ___a6
        else:
            ___a4 = ___a4[1:]
            ___a6 = ___a6 + 1
        ___t1 = ___a1 == 1
    return ___a2"""
        assertEquals(code, expected)
      }
    }
  }
}
