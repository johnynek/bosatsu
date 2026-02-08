package dev.bosatsu.codegen.python

import cats.{Eq, Show}
import cats.syntax.all._
import java.io.{ByteArrayInputStream, InputStream}
import java.util.concurrent.Semaphore
import dev.bosatsu.{Lit, PackageName, Par, TestUtils}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.python.util.PythonInterpreter
import org.python.core.{PyInteger, PyFunction, PyObject, PyTuple}

import TestUtils.compileFile

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

class PythonGenTest extends munit.ScalaCheckSuite {

  override def scalaCheckTestParameters =
    // these tests are slow
    super.scalaCheckTestParameters.withMinSuccessfulTests(500)

  implicit val showStr: Show[String] = Show.show[String](identity)

  // for some reason, these need to be defs or Jython will NPE
  def zero = new PyInteger(0)
  def one = new PyInteger(1)

  @annotation.tailrec
  final def foreachList(lst: PyObject)(fn: PyObject => Unit): Unit = {
    val tup = lst.asInstanceOf[PyTuple]
    val ary = tup.getArray()
    given Eq[PyObject] =
      // Safe: Jython's PyObject equality is well-defined for these tests.
      Eq.fromUniversalEquals
    if (ary(0) === zero) () // empty list
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
    given Eq[PyObject] =
      // Safe: Jython's PyObject equality is well-defined for these tests.
      Eq.fromUniversalEquals
    tup.getArray()(0) match {
      case x if x === zero =>
        // True == one in our encoding
        assertEquals(
          tup.getArray()(1),
          one,
          prefix + "/" + tup.getArray()(2).toString
        )
        ()
      case x if x === one =>
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

  def isfromString(s: String): InputStream =
    new ByteArrayInputStream(s.getBytes("UTF-8"))

  val intr = JythonBarrier.run(new PythonInterpreter())

  test("we can compile Nat.bosatsu") {
    val natPathBosatu: String = "test_workspace/Nat.bosatsu"

    val packMap =
      Par.noParallelism {
        val bosatsuPM = compileFile(natPathBosatu)
        PythonGen.renderSource(bosatsuPM, Map.empty, Map.empty)
      }
    val natDoc = packMap(())(PackageName.parts("Bosatsu", "Nat"))._2
    val natStr = natDoc.renderTrim(80)

    JythonBarrier.run {
      try {
        intr.execfile(isfromString(natStr), "nat.py")
        checkTest(intr.get("tests"), "Nat.bosatsu")
      } catch {
        case t: Throwable =>
          System.err.println("=" * 80)
          System.err.println("couldn't compile nat.py")
          System.err.println("=" * 80)
          System.err.println(natStr)
          System.err.println("=" * 80)
          throw t
      }
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
          assertEquals(res, new PyInteger(0))
        } else {
          assertEquals(fn.__call__(arg), arg)
        }
      }
    }
  }

  test("mult works like multiplication") {

    JythonBarrier.run {
      val fn = intr.get("mult", classOf[PyFunction])

      forAll(Gen.choose(0, 100), Gen.choose(0, 100)) { (i1, i2) =>
        val m1 = fn.__call__(new PyInteger(i1), new PyInteger(i2))
        assertEquals(m1, new PyInteger(i1 * i2))
      }
    }
  }

  test("float literal parsing matches python on hard cases") {
    val cases = List(
      "0.0",
      "-0.0",
      "1.0",
      "-1.0",
      ".5",
      "5.",
      "1e309",
      "-1e309",
      "1e-309",
      "5e-324",
      "2.2250738585072014e-308",
      "2.2250738585072011e-308",
      "1.7976931348623157e308",
      "1.7976931348623159e308",
      "3.141592653589793",
      "2.718281828459045",
      "1.0000000000000002",
      "9007199254740992.0",
      "9007199254740993.0",
      "4.9406564584124654e-324",
      "2.4703282292062327e-324",
      "2.2250738585072014e-308",
      "2.225073858507201e-308",
      "+6.02214076e23",
      "-6.02214076e23",
      "9.999999999999999e22",
      "7.2057594037927933e16",
      "1.1754943508222875e-38",
      "1.401298464324817e-45",
      "3.4028234663852886e38"
    )

    JythonBarrier.run {
      val py = new PythonInterpreter()
      try {
        cases.foreach { litStr =>
          val parsed = Lit.float64Parser.parseAll(litStr)
          parsed match {
            case Left(err) =>
              fail(s"failed to parse '$litStr' in Bosatsu: $err")
            case Right(f)  =>
              val pyBits = java.lang.Double.doubleToRawLongBits(
                py.eval(s"float('$litStr')").asDouble()
              )
              assertEquals(
                f.toRawLongBits,
                pyBits,
                s"mismatch for literal: $litStr"
              )
          }
        }
      } finally {
        py.close()
      }
    }
  }

  test("bosatsu parser accepts .NaN literal") {
    Lit.float64Parser.parseAll(".NaN") match {
      case Right(f) =>
        assert(java.lang.Double.isNaN(f.toDouble))
      case Left(err) =>
        fail(s"failed to parse .NaN: $err")
    }
  }

  JythonBarrier.run(intr.close())

  def runBoTests(
      path: String,
      pn: PackageName,
      testName: String,
      extraPaths: String*
  ) =
    JythonBarrier.run {
      val intr = new PythonInterpreter()

      val packMap =
        Par.noParallelism {
          val bosatsuPM = compileFile(path, extraPaths*)
          PythonGen.renderSource(bosatsuPM, Map.empty, Map.empty)
        }
      val doc = packMap(())(pn)._2

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
      "test",
      "test_workspace/Float64.bosatsu"
    )
  }

}
