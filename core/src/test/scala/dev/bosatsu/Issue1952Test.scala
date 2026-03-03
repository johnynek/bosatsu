package dev.bosatsu

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class Issue1952Test extends munit.FunSuite with ParTest {
  import TestUtils._

  private val charPack = new String(
    Files.readAllBytes(Paths.get("test_workspace/Char.bosatsu")),
    StandardCharsets.UTF_8
  )
  private val reproPack = """
package Repro/Issue1952

from Bosatsu/Char import max_Char, char_to_Int, int_to_Char

def max_char_round_trip() -> Bool:
  match int_to_Char(1114111):
    case Some(c):
      eq_Int(char_to_Int(c), char_to_Int(max_Char))
    case None:
      False

tests = TestSuite("Issue1952", [
  Assertion(eq_Int(char_to_Int(max_Char), 1114111), "max_Char codepoint"),
  Assertion(max_char_round_trip(), "max_Char round trip"),
])
"""

  test("issue 1952: Bosatsu/Char exports max_Char") {
    runBosatsuTest(
      List(charPack, reproPack),
      "Repro/Issue1952",
      2
    )
  }
}
