package dev.bosatsu

class BosatsuIntRuntimeTest extends munit.FunSuite {
  import TestUtils.runBosatsuTest

  test("Bosatsu Int tests exercise optimized runtime special cases") {
    val src =
      """package Bosatsu/Int/RuntimeCases
        |
        |big_two_word = 9223372036854775808
        |big_i128 = 24197857203266734864793317670504947440
        |big16_a = 3121748550315992231381597229793166305748598142666039144377117414666569249672850396177607455324670297544800622066501193227570027641603482357320981
        |big16_b = 1560874275157996115690798614896583152874299071332485575429578479812743765454501468157864837024250329533840100553036171343824973763816896977594545
        |smallres_and_left = 226854911280625642296618575572344526456
        |smallres_and_right = 113427455640312821148309287790314520575
        |smallres_xor_left = 249540402403406329026559260228226143864
        |smallres_xor_right = 249540402403406329026559260230516793072
        |pow2_65 = shift_left_Int(1, 65)
        |neg_pow2_65 = sub(0, pow2_65)
        |pow2_100 = shift_left_Int(1, 100)
        |shift_small_source = add(pow2_100, 305419896)
        |neg_big16_a = sub(0, big16_a)
        |not_big16_a = not_Int(big16_a)
        |big_decimal = 12345678901234567890123456789012345678901234567890123456789012345678901234567
        |big_decimal_neg = sub(0, big_decimal)
        |
        |def eq_Int_runtime(left, right): cmp_Int(left, right) matches EQ
        |def eq_String_runtime(left, right): cmp_String(left, right) matches EQ
        |def eq_Opt_Int_runtime(opt, expected):
        |  if opt matches Some(v):
        |    eq_Int_runtime(v, expected)
        |  else:
        |    False
        |
        |tests = TestSuite("int runtime special cases", [
        |  Assertion(cmp_Int(2305843009213693952, 2305843009213693951) matches GT, "63-bit immediate compare"),
        |  Assertion(add(2305843009213693952, 2305843009213693952) matches 4611686018427387904, "63-bit immediate add at old boundary"),
        |  Assertion(add(big_two_word, 7) matches 9223372036854775815, "2-word bigint plus small"),
        |  Assertion(sub(big_two_word, 5) matches 9223372036854775803, "2-word bigint minus small"),
        |  Assertion(sub(5, big_two_word) matches -9223372036854775803, "small minus 2-word bigint"),
        |  Assertion(not_Int(big_two_word) matches -9223372036854775809, "bigint not"),
        |  Assertion(mul(big_two_word, big_two_word) matches 85070591730234615865843651857942052864, "bigint multiply"),
        |  Assertion(and_Int(smallres_and_left, smallres_and_right) matches 305419896, "bitwise and canonicalizes to small"),
        |  Assertion(xor_Int(smallres_xor_left, smallres_xor_right) matches 2290649224, "bitwise xor canonicalizes to small"),
        |  Assertion(and_Int(big16_a, 0) matches 0, "and identity zero"),
        |  Assertion(eq_Int_runtime(and_Int(big16_a, -1), big16_a), "and identity -1"),
        |  Assertion(eq_Int_runtime(or_Int(big16_a, 0), big16_a), "or identity zero"),
        |  Assertion(or_Int(big16_a, -1) matches -1, "or identity -1"),
        |  Assertion(eq_Int_runtime(xor_Int(big16_a, 0), big16_a), "xor identity zero"),
        |  Assertion(eq_Int_runtime(xor_Int(big16_a, -1), not_big16_a), "xor identity -1"),
        |  Assertion(and_Int(big16_a, big16_b) matches 39471121, "wide positive and"),
        |  Assertion(or_Int(big16_a, big16_b) matches 4682622825473988347072395844689749458622897213998524719806695894479313015127351864335472292348920627078640722619537364571395001405420379295444405, "wide positive or"),
        |  Assertion(xor_Int(big16_a, big16_b) matches 4682622825473988347072395844689749458622897213998524719806695894479313015127351864335472292348920627078640722619537364571395001405420379255973284, "wide positive xor"),
        |  Assertion(shift_left_Int(305419896, 1) matches 610839792, "small shift left"),
        |  Assertion(shift_right_Int(305419896, 1) matches 152709948, "small shift right"),
        |  Assertion(shift_right_Int(shift_small_source, 64) matches 68719476736, "big shift right to small"),
        |  Assertion(shift_right_Int(shift_small_source, 96) matches 16, "big shift right canonicalizes to small"),
        |  Assertion(shift_right_Int(neg_big16_a, 5) matches -97554642197374757230674913431036447054643691958313723261784919208330289052276574880550232978895946798275019439578162288361563363800108823666281, "negative shift right"),
        |  Assertion(div(big_i128, 32) matches 756183037602085464524791177203279607, "div by small power of two"),
        |  Assertion(mod_Int(big_i128, 32) matches 16, "mod by small power of two"),
        |  Assertion(div(big_i128, -32) matches -756183037602085464524791177203279608, "div by negative small power of two"),
        |  Assertion(mod_Int(big_i128, -32) matches -16, "mod by negative small power of two"),
        |  Assertion(div(big_i128, pow2_65) matches 655884233731895160, "div by big power of two"),
        |  Assertion(mod_Int(big_i128, pow2_65) matches 1311768467463790320, "mod by big power of two"),
        |  Assertion(div(big_i128, neg_pow2_65) matches -655884233731895161, "div by negative big power of two"),
        |  Assertion(mod_Int(big_i128, neg_pow2_65) matches -35581719679955312912, "mod by negative big power of two"),
        |  Assertion(gcd_Int(big_i128, pow2_65) matches 16, "gcd via power-of-two divmod"),
        |  Assertion(eq_String_runtime(int_to_String(big_decimal), "12345678901234567890123456789012345678901234567890123456789012345678901234567"), "big int to string"),
        |  Assertion(eq_Opt_Int_runtime(string_to_Int("12345678901234567890123456789012345678901234567890123456789012345678901234567"), big_decimal), "big string to int"),
        |  Assertion(eq_Opt_Int_runtime(string_to_Int("-12345678901234567890123456789012345678901234567890123456789012345678901234567"), big_decimal_neg), "negative big string to int"),
        |  Assertion(string_to_Int("") matches None, "empty string parse failure"),
        |  Assertion(string_to_Int("-") matches None, "dash parse failure"),
        |  Assertion(string_to_Int("12x3") matches None, "junk parse failure")
        |])
        |""".stripMargin

    runBosatsuTest(List(src), "Bosatsu/Int/RuntimeCases", 38)
  }
}
