package dev.bosatsu

class RequireTest extends munit.FunSuite {

  test("require does not evaluate message when requirement is true") {
    var evaluated = false
    def message: String = {
      evaluated = true
      "unexpected"
    }

    Require(true, message)
    assertEquals(evaluated, false)
  }

  test("require without message throws the standard message") {
    val err = intercept[IllegalArgumentException] {
      Require(false)
    }
    assertEquals(err.getMessage, "requirement failed")
  }

  test("require with message throws the standard message prefix") {
    val err = intercept[IllegalArgumentException] {
      Require(false, 123)
    }
    assertEquals(err.getMessage, "requirement failed: 123")
  }

  test("require with string literal keeps the standard message") {
    val err = intercept[IllegalArgumentException] {
      Require(false, "literal")
    }
    assertEquals(err.getMessage, "requirement failed: literal")
  }
}
