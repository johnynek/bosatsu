package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.Generators.bindIdentGen
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
    assert(PythonGen.externalParser.parseAll(extstr).map(_ => ()) == Right(()))
  }

  test("we can parse an example evals file") {
    val str = """
      {
        IO::IO: IOPy.run_io,
        Build/Foo::Bar: BuildImpl.run_build,
      }
    """
    assert(PythonGen.evaluatorParser.parseAll(str).map(_ => ()) == Right(()))
  }
}
