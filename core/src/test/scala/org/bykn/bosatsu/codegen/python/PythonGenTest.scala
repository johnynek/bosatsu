package org.bykn.bosatsu.codegen.python

import org.bykn.bosatsu.Identifier.{Bindable, unsafeBindable}
import org.bykn.bosatsu.Generators.bindIdentGen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.scalatest.FunSuite

class PythonGenTest extends FunSuite {
  implicit val generatorDrivenConfig =
    //PropertyCheckConfiguration(minSuccessful = 50000)
    PropertyCheckConfiguration(minSuccessful = 5000)
    //PropertyCheckConfiguration(minSuccessful = 500)

  test("PythonGen.escape round trips") {

    def law(b: Bindable) = {
      val ident = PythonGen.escape(b)
      PythonGen.unescape(ident) match {
        case Some(b1) => assert(b1.asString == b.asString)
        case None => assert(false, s"$b => $ident could not round trip")
      }
    }

    forAll(bindIdentGen)(law(_))

    val examples: List[Bindable] =
      List(
        "`12 =_=`",
        "`N`").map(unsafeBindable)

    examples.foreach(law(_))

  }

  val PythonName = "[_A-Za-z][_A-Za-z0-9]*".r.pattern

  test("all escapes are valid python identifiers") {
    forAll(bindIdentGen) { b =>
      val str = PythonGen.escape(b).name

      assert(PythonName.matcher(str).matches(), s"escaped: ${b.sourceCodeRepr} to $str")
    }
  }

  test("if unescape works, escape would round trip") {
    forAll { (s: String) =>
      if (Code.python2Name.matcher(s).matches) {
        val ident = Code.Ident(s)
        PythonGen.unescape(ident) match {
          case Some(b) =>
            assert(PythonGen.escape(b) == ident)
          case None =>
            ()
        }
      }
    }
  }
}
