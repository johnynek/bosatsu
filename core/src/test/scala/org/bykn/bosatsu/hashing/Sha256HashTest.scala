package org.bykn.bosatsu.hashing

class Sha256HashTest extends munit.FunSuite {
  test("test some examples") {
    /* 
    oscar@patricks-air bosatsu % echo "foo" | shasum -a 256
b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c  -
     */
    assertEquals(
      Sha256Hash.sha256HashHex("foo\n".getBytes("utf-8")),
      "b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c")

    /* 
    oscar@patricks-air bosatsu % echo "bosatsu is functional programming" | shasum -a 256
f4efabb86f391fc1466235ffe2120550de16274d4c73a344709f6e823ce01600 -
     */
    assertEquals(
      Sha256Hash.sha256HashHex("bosatsu is functional programming\n".getBytes("utf-8")),
      "f4efabb86f391fc1466235ffe2120550de16274d4c73a344709f6e823ce01600")
  }
}