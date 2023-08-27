package org.bykn.bosatsu.codegen.python

import cats.Show
import cats.data.NonEmptyList
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.{Paths, Files}
import java.util.concurrent.Semaphore
import org.bykn.bosatsu.{
  PackageMap,
  MatchlessFromTypedExpr,
  Parser,
  Package,
  LocationMap,
  PackageName
}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.{
  forAll,
  PropertyCheckConfiguration
}
import org.python.util.PythonInterpreter
import org.python.core.{PyInteger, PyFunction, PyObject, PyTuple}

import org.bykn.bosatsu.DirectEC.directEC
import org.scalatest.funsuite.AnyFunSuite

// Jython seems to have some thread safety issues
object JythonBarrier {
  private val sem = new Semaphore(1)

  def run[A](a: => A): A = {
    sem.acquire()
    try a
    finally {
      sem.release()
    }
  }
}

class PythonGenTest extends AnyFunSuite {

  implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    // these tests are slow
    PropertyCheckConfiguration(minSuccessful = 500)

  implicit val showStr: Show[String] = Show.show[String](identity)

  // for some reason, these need to be defs or Jython will NPE
  def zero = new PyInteger(0)
  def one = new PyInteger(1)

  @annotation.tailrec
  final def foreachList(lst: PyObject)(fn: PyObject => Unit): Unit = {
    val tup = lst.asInstanceOf[PyTuple]
    val ary = tup.getArray()
    if (ary(0) == zero) () // empty list
    else {
      fn(ary(1))
      foreachList(ary(2))(fn)
    }
  }

  // enum Test:
  //   Assertion(value: Bool, message: String)
  //   TestSuite(name: String, tests: List[Test])
  def checkTest(testValue: PyObject, prefix: String): Unit = {
    val tup = testValue.asInstanceOf[PyTuple]
    tup.getArray()(0) match {
      case x if x == zero =>
        // True == one in our encoding
        assert(
          tup.getArray()(1) == one,
          prefix + "/" + tup.getArray()(2).toString
        )
        ()
      case x if x == one =>
        val suite = tup.getArray()(1).toString
        foreachList(tup.getArray()(2)) { t =>
          checkTest(t, prefix + "/" + suite); ()
        }
      case other =>
        assert(
          false,
          s"expected a Test to have 0 or 1 in first tuple entry: $tup, $other"
        )
        ()
    }
  }

  def compileFile(path: String, rest: String*): PackageMap.Typed[Any] = {
    def toS(s: String): String =
      new String(Files.readAllBytes(Paths.get(s)), "UTF-8")

    val packNEL =
      NonEmptyList(path, rest.toList)
        .map { s =>
          val str = toS(s)
          val pack = Parser.unsafeParse(Package.parser(None), str)
          (("", LocationMap(str)), pack)
        }

    val res = PackageMap.typeCheckParsed(packNEL, Nil, "")
    res.left match {
      case Some(err) => sys.error(err.toString)
      case None      => ()
    }

    res.right.get
  }

  def isfromString(s: String): InputStream =
    new ByteArrayInputStream(s.getBytes("UTF-8"))

  val intr = JythonBarrier.run(new PythonInterpreter())

  test("we can compile Nat.bosatsu") {
    val natPathBosatu: String = "test_workspace/Nat.bosatsu"

    val bosatsuPM = compileFile(natPathBosatu)
    val matchless = MatchlessFromTypedExpr.compile(bosatsuPM)

    val packMap =
      PythonGen.renderAll(matchless, Map.empty, Map.empty, Map.empty)
    val natDoc = packMap(PackageName.parts("Bosatsu", "Nat"))._2

    JythonBarrier.run {
      intr.execfile(isfromString(natDoc.renderTrim(80)), "nat.py")
      checkTest(intr.get("tests"), "Nat.bosatsu")
    }
  }

  test("to_Nat works like identity") {
    JythonBarrier.run {
      val fn = intr.get("to_Nat", classOf[PyFunction])

      forAll(Gen.choose(-100, 100)) { (i: Int) =>
        // this is O(N) computation, it shouldn't get too big
        val arg = new PyInteger(i)
        val res = fn.__call__(arg)
        if (i <= 0) {
          assert(res == new PyInteger(0))
        } else {
          assert(fn.__call__(arg) == arg)
        }
      }
    }
  }

  test("mult works like multiplication") {

    JythonBarrier.run {
      val fn = intr.get("mult", classOf[PyFunction])

      forAll(Gen.choose(0, 100), Gen.choose(0, 100)) { (i1, i2) =>
        val m1 = fn.__call__(new PyInteger(i1)).asInstanceOf[PyFunction]
        assert(m1.__call__(new PyInteger(i2)) == new PyInteger(i1 * i2))
      }
    }
  }

  JythonBarrier.run(intr.close())

  def runBoTests(path: String, pn: PackageName, testName: String) =
    JythonBarrier.run {
      val intr = new PythonInterpreter()

      val bosatsuPM = compileFile(path)
      val matchless = MatchlessFromTypedExpr.compile(bosatsuPM)

      val packMap =
        PythonGen.renderAll(matchless, Map.empty, Map.empty, Map.empty)
      val doc = packMap(pn)._2

      intr.execfile(isfromString(doc.renderTrim(80)), "test.py")
      checkTest(intr.get(testName), pn.asString)

      intr.close()
    }

  test("we can compile StrConcatExample") {
    runBoTests(
      "test_workspace/StrConcatExample.bosatsu",
      PackageName.parts("StrConcatExample"),
      "test"
    )
  }

  test("test some list pattern matches") {
    runBoTests(
      "test_workspace/ListPat.bosatsu",
      PackageName.parts("ListPat"),
      "tests"
    )
  }

  test("test euler6") {
    runBoTests(
      "test_workspace/euler6.bosatsu",
      PackageName.parts("Euler", "P6"),
      "tests"
    )
  }

  test("test PredefTests") {
    runBoTests(
      "test_workspace/PredefTests.bosatsu",
      PackageName.parts("PredefTests"),
      "test"
    )
  }
}
