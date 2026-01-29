package dev.bosatsu

import munit.FunSuite
import Value._
import java.math.BigInteger

class IOTest extends FunSuite {

  // =========================================================================
  // IO Effect Construction
  // =========================================================================

  test("IO.Pure creates a pure effect") {
    val effect = IO.Pure(VInt(BigInteger.valueOf(42)))
    effect match {
      case IO.Pure(VInt(bi)) => assertEquals(bi.intValue, 42)
      case _ => fail("Expected Pure effect")
    }
  }

  test("IO.Capture stores name and value") {
    val effect = IO.Capture("myVar", VInt(BigInteger.valueOf(100)), None)
    effect match {
      case IO.Capture(name, VInt(bi), formula) =>
        assertEquals(name, "myVar")
        assertEquals(bi.intValue, 100)
        assertEquals(formula, None)
      case _ => fail("Expected Capture effect")
    }
  }

  test("IO.Capture stores formula when provided") {
    val effect = IO.Capture("result", VInt(BigInteger.valueOf(50)), Some("x + y"))
    effect match {
      case IO.Capture(name, _, formula) =>
        assertEquals(name, "result")
        assertEquals(formula, Some("x + y"))
      case _ => fail("Expected Capture effect with formula")
    }
  }

  test("IO.RandomInt stores min and max") {
    val effect = IO.RandomInt(1, 100)
    effect match {
      case IO.RandomInt(min, max) =>
        assertEquals(min, 1)
        assertEquals(max, 100)
      case _ => fail("Expected RandomInt effect")
    }
  }

  test("IO.Trace creates a trace effect") {
    val effect = IO.Trace()
    assert(effect.isInstanceOf[IO.Trace])
  }

  test("IO.FlatMap chains effects") {
    val pure = IO.Pure(VInt(BigInteger.valueOf(10)))
    val flatMapped = IO.FlatMap(pure, { v => IO.Pure(v) })
    flatMapped match {
      case IO.FlatMap(io, _) =>
        assert(io.isInstanceOf[IO.Pure[?]])
      case _ => fail("Expected FlatMap effect")
    }
  }

  // =========================================================================
  // runWithProvenance - Pure values
  // =========================================================================

  test("runWithProvenance with Pure returns the value") {
    val effect = IO.Pure(VInt(BigInteger.valueOf(42)))
    val (result, trace) = IO.runWithProvenance(effect)
    result match {
      case VInt(bi) => assertEquals(bi.intValue, 42)
      case _ => fail("Expected VInt result")
    }
    assertEquals(trace.nodes.size, 0) // Pure doesn't create nodes
  }

  test("runWithProvenance with Pure String value") {
    val effect = IO.Pure(Str("hello"))
    val (result, _) = IO.runWithProvenance(effect)
    result match {
      case Str(s) => assertEquals(s, "hello")
      case _ => fail("Expected Str result")
    }
  }

  // =========================================================================
  // runWithProvenance - Capture
  // =========================================================================

  test("runWithProvenance with Capture creates provenance node") {
    val effect = IO.Capture("x", VInt(BigInteger.valueOf(100)), None)
    val (result, trace) = IO.runWithProvenance(effect)

    result match {
      case VInt(bi) => assertEquals(bi.intValue, 100)
      case _ => fail("Expected VInt result")
    }

    assertEquals(trace.nodes.size, 1)
    val node = trace.nodes(trace.root)
    assertEquals(node.name, "x")
    assertEquals(node.valueStr, "100")
    assertEquals(node.formula, "x") // No formula provided, defaults to name
  }

  test("runWithProvenance with Capture uses formula when provided") {
    val effect = IO.Capture("result", VInt(BigInteger.valueOf(15)), Some("x + y"))
    val (_, trace) = IO.runWithProvenance(effect)

    val node = trace.nodes(trace.root)
    assertEquals(node.formula, "x + y")
  }

  test("runWithProvenance captures different value types") {
    // String value
    val strEffect = IO.Capture("name", Str("Alice"), None)
    val (strResult, strTrace) = IO.runWithProvenance(strEffect)
    strResult match {
      case Str(s) => assertEquals(s, "Alice")
      case _ => fail("Expected Str result")
    }
    assertEquals(strTrace.nodes(strTrace.root).valueStr, "\"Alice\"")

    // Boolean True
    val trueEffect = IO.Capture("flag", True, None)
    val (_, trueTrace) = IO.runWithProvenance(trueEffect)
    assertEquals(trueTrace.nodes(trueTrace.root).valueStr, "True")

    // Boolean False
    val falseEffect = IO.Capture("flag", False, None)
    val (_, falseTrace) = IO.runWithProvenance(falseEffect)
    assertEquals(falseTrace.nodes(falseTrace.root).valueStr, "False")

    // Double value
    val doubleEffect = IO.Capture("rate", ExternalValue(java.lang.Double.valueOf(3.14)), None)
    val (_, doubleTrace) = IO.runWithProvenance(doubleEffect)
    assertEquals(doubleTrace.nodes(doubleTrace.root).valueStr, "3.14")
  }

  // =========================================================================
  // runWithProvenance - FlatMap
  // =========================================================================

  test("runWithProvenance with FlatMap chains computations") {
    val first = IO.Capture("a", VInt(BigInteger.valueOf(10)), None)
    val chained = IO.FlatMap[Value, Value](first, { _ =>
      IO.Capture("b", VInt(BigInteger.valueOf(20)), None)
    })

    val (result, trace) = IO.runWithProvenance(chained)
    result match {
      case VInt(bi) => assertEquals(bi.intValue, 20)
      case _ => fail("Expected VInt result")
    }

    assertEquals(trace.nodes.size, 2)
    assert(trace.nodes.values.exists(_.name == "a"))
    assert(trace.nodes.values.exists(_.name == "b"))
  }

  test("runWithProvenance tracks dependencies through FlatMap") {
    val first = IO.Capture("input", VInt(BigInteger.valueOf(5)), None)
    val chained = IO.FlatMap[Value, Value](first, { v =>
      val doubled = v match {
        case VInt(bi) => VInt(bi.multiply(BigInteger.valueOf(2)))
        case _ => v
      }
      IO.Capture("output", doubled, Some("input * 2"))
    })

    val (result, trace) = IO.runWithProvenance(chained)
    result match {
      case VInt(bi) => assertEquals(bi.intValue, 10)
      case _ => fail("Expected VInt result")
    }

    // The output node should have the input node as a dependency
    val outputNode = trace.nodes(trace.root)
    assertEquals(outputNode.name, "output")
    assertEquals(outputNode.formula, "input * 2")
    assert(outputNode.dependencies.nonEmpty)
  }

  // =========================================================================
  // runWithProvenance - RandomInt
  // =========================================================================

  test("runWithProvenance with RandomInt produces value in range") {
    val rng = new scala.util.Random(42) // Fixed seed for reproducibility
    val effect = IO.RandomInt(1, 10)
    val (result, _) = IO.runWithProvenance(effect, rng)

    result match {
      case i: Int =>
        assert(i >= 1 && i <= 10, s"Expected value in [1, 10], got $i")
      case _ => fail("Expected Int result")
    }
  }

  test("runWithProvenance with RandomInt respects bounds") {
    val rng = new scala.util.Random(123)
    val effect = IO.RandomInt(5, 5) // min == max
    val (result, _) = IO.runWithProvenance(effect, rng)

    result match {
      case i: Int => assertEquals(i, 5)
      case _ => fail("Expected Int result")
    }
  }

  // =========================================================================
  // runWithProvenance - Trace
  // =========================================================================

  test("runWithProvenance with Trace returns current trace as string") {
    // First capture some values, then trace
    val captureA = IO.Capture("a", VInt(BigInteger.valueOf(1)), None)
    val captureB = IO.FlatMap[Value, Value](captureA, { _ =>
      IO.Capture("b", VInt(BigInteger.valueOf(2)), None)
    })
    val withTrace = IO.FlatMap[Value, String](captureB, { _ =>
      IO.Trace()
    })

    val (result, _) = IO.runWithProvenance(withTrace)
    result match {
      case s: String =>
        assert(s.contains("a=1"), s"Expected trace to contain a=1, got: $s")
        assert(s.contains("b=2"), s"Expected trace to contain b=2, got: $s")
      case _ => fail("Expected String result from Trace")
    }
  }

  // =========================================================================
  // ProvenanceNode and ProvenanceTrace
  // =========================================================================

  test("ProvenanceNode stores all fields correctly") {
    val node = IO.ProvenanceNode(
      id = 1L,
      name = "testVar",
      valueStr = "42",
      dependencies = List(0L),
      formula = "x + y"
    )
    assertEquals(node.id, 1L)
    assertEquals(node.name, "testVar")
    assertEquals(node.valueStr, "42")
    assertEquals(node.dependencies, List(0L))
    assertEquals(node.formula, "x + y")
  }

  test("ProvenanceTrace stores nodes and root") {
    val node = IO.ProvenanceNode(1L, "x", "10", Nil, "x")
    val trace = IO.ProvenanceTrace(Map(1L -> node), 1L)
    assertEquals(trace.root, 1L)
    assertEquals(trace.nodes.size, 1)
    assertEquals(trace.nodes(1L), node)
  }

  // =========================================================================
  // JVM Externals
  // =========================================================================

  test("IO.jvmExternals is not empty") {
    assert(!IO.jvmExternals.equals(Externals.empty))
  }

  test("IO.packageName is correct") {
    assertEquals(IO.packageName.asString, "Bosatsu/IO")
  }

  test("IO.ioString contains IO type definitions") {
    assert(IO.ioString.contains("external struct IO"))
    assert(IO.ioString.contains("pure"))
    assert(IO.ioString.contains("flatMap"))
  }

  // =========================================================================
  // Value conversion in runWithProvenance
  // =========================================================================

  test("runWithProvenance converts Int to VInt in FlatMap") {
    val intEffect: IO.IOEffect[Int] = IO.RandomInt(1, 1) // Always returns 1
    val chained = IO.FlatMap[Int, Value](intEffect, { v =>
      // v should be converted to VInt
      IO.Pure(v)
    })

    val (result, _) = IO.runWithProvenance(chained, new scala.util.Random(0))
    result match {
      case VInt(bi) => assertEquals(bi.intValue, 1)
      case _ => fail(s"Expected VInt, got: $result")
    }
  }

  test("runWithProvenance handles ProductValue in valueToString") {
    val product = ProductValue(Array(VInt(BigInteger.valueOf(1)), VInt(BigInteger.valueOf(2))))
    val effect = IO.Capture("tuple", product, None)
    val (_, trace) = IO.runWithProvenance(effect)

    val valueStr = trace.nodes(trace.root).valueStr
    assert(valueStr.contains("1"), s"Expected valueStr to contain 1, got: $valueStr")
    assert(valueStr.contains("2"), s"Expected valueStr to contain 2, got: $valueStr")
  }
}
