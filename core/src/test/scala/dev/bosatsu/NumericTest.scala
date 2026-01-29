package dev.bosatsu

import munit.FunSuite
import Value._

class NumericTest extends FunSuite {

  private def wrap(d: Double): Value =
    ExternalValue(java.lang.Double.valueOf(d))

  // =========================================================================
  // Basic arithmetic operations
  // =========================================================================

  test("NumericImpl.add adds two doubles") {
    val result = NumericImpl.add(wrap(3.5), wrap(2.5))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 6.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.sub subtracts two doubles") {
    val result = NumericImpl.sub(wrap(5.0), wrap(2.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 3.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.times multiplies two doubles") {
    val result = NumericImpl.times(wrap(3.0), wrap(4.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 12.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.div divides two doubles") {
    val result = NumericImpl.div(wrap(10.0), wrap(4.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 2.5)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.div by zero produces Infinity") {
    val result = NumericImpl.div(wrap(1.0), wrap(0.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assert(d.doubleValue.isInfinite, "1.0 / 0.0 should be Infinity")
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.div 0/0 produces NaN") {
    val result = NumericImpl.div(wrap(0.0), wrap(0.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assert(d.doubleValue.isNaN, "0.0 / 0.0 should be NaN")
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  // =========================================================================
  // Conversion operations
  // =========================================================================

  test("NumericImpl.fromInt converts Int to Double") {
    val result = NumericImpl.fromInt(VInt(java.math.BigInteger.valueOf(42)))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 42.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.toInt converts Double to Int (truncates)") {
    val result = NumericImpl.toInt(wrap(3.9))
    result match {
      case VInt(bi) =>
        assertEquals(bi.intValue, 3)
      case _ => fail("Expected VInt")
    }
  }

  test("NumericImpl.toInt converts negative Double to Int") {
    val result = NumericImpl.toInt(wrap(-3.9))
    result match {
      case VInt(bi) =>
        assertEquals(bi.intValue, -3)
      case _ => fail("Expected VInt")
    }
  }

  // =========================================================================
  // Comparison operations
  // =========================================================================

  test("NumericImpl.cmp returns LT for less than") {
    val result = NumericImpl.cmp(wrap(1.0), wrap(2.0))
    assertEquals(result, Comparison.LT)
  }

  test("NumericImpl.cmp returns GT for greater than") {
    val result = NumericImpl.cmp(wrap(3.0), wrap(2.0))
    assertEquals(result, Comparison.GT)
  }

  test("NumericImpl.cmp returns EQ for equal") {
    val result = NumericImpl.cmp(wrap(2.0), wrap(2.0))
    assertEquals(result, Comparison.EQ)
  }

  test("NumericImpl.cmp with NaN returns GT") {
    val result = NumericImpl.cmp(wrap(Double.NaN), wrap(1.0))
    assertEquals(result, Comparison.GT)
  }

  test("NumericImpl.cmp second arg NaN returns GT") {
    val result = NumericImpl.cmp(wrap(1.0), wrap(Double.NaN))
    assertEquals(result, Comparison.GT)
  }

  test("NumericImpl.eq returns True for equal doubles") {
    val result = NumericImpl.eq(wrap(2.5), wrap(2.5))
    assertEquals(result, True)
  }

  test("NumericImpl.eq returns False for unequal doubles") {
    val result = NumericImpl.eq(wrap(2.5), wrap(3.5))
    assertEquals(result, False)
  }

  // =========================================================================
  // Unary operations
  // =========================================================================

  test("NumericImpl.neg negates a double") {
    val result = NumericImpl.neg(wrap(5.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, -5.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.neg of negative is positive") {
    val result = NumericImpl.neg(wrap(-3.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 3.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.abs of positive is unchanged") {
    val result = NumericImpl.abs(wrap(5.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 5.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  test("NumericImpl.abs of negative is positive") {
    val result = NumericImpl.abs(wrap(-5.0))
    result match {
      case ExternalValue(d: java.lang.Double) =>
        assertEquals(d.doubleValue, 5.0)
      case _ => fail("Expected ExternalValue with Double")
    }
  }

  // =========================================================================
  // Error handling
  // =========================================================================

  test("NumericImpl operations throw on invalid types") {
    intercept[RuntimeException] {
      NumericImpl.add(VInt(java.math.BigInteger.valueOf(1)), wrap(2.0))
    }
  }

  test("NumericImpl.fromInt throws on non-Int") {
    intercept[RuntimeException] {
      NumericImpl.fromInt(wrap(1.0))
    }
  }

  // =========================================================================
  // Numeric module constants
  // =========================================================================

  test("Numeric.packageName is correct") {
    assertEquals(Numeric.packageName.asString, "Bosatsu/Numeric")
  }

  test("Numeric.numericString contains Double definition") {
    assert(Numeric.numericString.contains("external struct Double"))
  }

  test("Numeric.jvmExternals is not empty") {
    // jvmExternals adds 10 functions
    assert(!Numeric.jvmExternals.equals(Externals.empty))
  }
}
