package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.Generators.bindIdentGen
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class PythonGenTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 500)

  test("PythonGen.escape round trips") {
    forAll(bindIdentGen) { b =>
      val ident = PythonGen.escape(b)
      PythonGen.unescape(ident) match {
        case Some(b1) => assert(b1 == b)
        case None => assert(false, s"$b => $ident could not round trip")
      }
    }

  }

  val PythonName = "[_A-Za-z][_A-Za-z0-9]*".r.pattern

  test("all escapes are valid python identifiers") {
    forAll(bindIdentGen) { b =>
      val str = PythonGen.escape(b).name

      assert(PythonName.matcher(str).matches(), s"escaped: ${b.sourceCodeRepr} to $str")
    }
  }
}
