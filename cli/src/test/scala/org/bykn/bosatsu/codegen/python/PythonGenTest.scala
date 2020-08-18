package org.bykn.bosatsu.codegen.python

import cats.Show
import cats.data.NonEmptyList
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.{Paths, Files}
import org.bykn.bosatsu.{PackageMap, MatchlessFromTypedExpr, Parser, Package, LocationMap, PackageName}
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks.{ forAll, PropertyCheckConfiguration }
import org.python.util.PythonInterpreter
import org.python.core.{PyInteger, PyFunction}

import org.bykn.bosatsu.DirectEC.directEC

class PythonGenTest extends FunSuite {

  implicit val generatorDrivenConfig =
    // these tests are slow
    PropertyCheckConfiguration(minSuccessful = 500)

  implicit val showStr: Show[String] = Show.show[String](identity)

  def compileFile(path: String): PackageMap.Typed[Any] = {
    val str = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")

    val pack = Parser.unsafeParse(Package.parser(None), str)
    val packNEL = NonEmptyList((("", LocationMap(str)), pack), Nil)
    PackageMap.typeCheckParsed(packNEL, Nil, "").right.get
  }

  def isfromString(s: String): InputStream =
    new ByteArrayInputStream(s.getBytes("UTF-8"))

  val intr = new PythonInterpreter()

  test("we can compile Nat.bosatsu") {
    val natPathBosatu: String = "test_workspace/Nat.bosatsu"

    val bosatsuPM = compileFile(natPathBosatu)
    val matchless = MatchlessFromTypedExpr.compile(bosatsuPM)

    val packMap = PythonGen.renderAll(matchless, Map.empty)
    val natDoc = packMap(PackageName.parts("Bosatsu", "Nat"))._2

    intr.execfile(isfromString(natDoc.renderTrim(80)), "nat.py")

  }

  test("to_Nat works like identity") {

    val fn = intr.get("to_Nat", classOf[PyFunction])

    forAll(Gen.choose(-100, 100)) { (i: Int) =>
      // this is O(N) computation, it shouldn't get too big
      val arg = new PyInteger(i)
      val res = fn.__call__(arg)
      if (i <= 0) {
        assert(res == new PyInteger(0))
      }
      else {
        assert(fn.__call__(arg) == arg)
      }
    }
  }

  test("mult works like multiplication") {

    val fn = intr.get("mult", classOf[PyFunction])

    forAll(Gen.choose(0, 100), Gen.choose(0, 100)) { (i1, i2) =>
      val m1 = fn.__call__(new PyInteger(i1)).asInstanceOf[PyFunction]
      assert(m1.__call__(new PyInteger(i2)) == new PyInteger(i1 * i2))
    }
  }

}
