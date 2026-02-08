#include "bosatsu_runtime.h"
#include "bosatsu_ext_Bosatsu_l_Float_l_Float64.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gc.h"

void assert(_Bool cond, char* message) {
  if (!cond) {
    printf("%s\n", message);
    exit(1);
  }
}

void assert_string_equals(BValue got, const char* expected, const char* message) {
  BValue exp = bsts_string_from_utf8_bytes_static(strlen(expected), (char*)expected);
  if (bsts_string_cmp(got, exp) != 0) {
    printf("%s\nexpected: %s\ngot: ", message, expected);
    bsts_string_println(got);
    exit(1);
  }
}

void assert_int_string(BValue v, const char* expected, const char* message) {
  BValue s = bsts_integer_to_string(v);
  assert_string_equals(s, expected, message);
}

void assert_option_int(BValue opt, const char* expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(%s)\n", message, expected);
    exit(1);
  }
  BValue v = get_enum_index(opt, 0);
  assert_int_string(v, expected, message);
}

void assert_option_none(BValue opt, const char* message) {
  if (get_variant(opt) != 0) {
    printf("%s\nexpected: None\n", message);
    exit(1);
  }
}

void assert_u64_equals(uint64_t got, uint64_t expected, const char* message) {
  if (got != expected) {
    printf("%s\nexpected: %llu\ngot: %llu\n", message, (unsigned long long)expected, (unsigned long long)got);
    exit(1);
  }
}

void assert_option_float_bits(BValue opt, uint64_t expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(float64)\ngot: None\n", message);
    exit(1);
  }
  BValue v = get_enum_index(opt, 0);
  assert_u64_equals(bsts_float64_to_bits(v), expected, message);
}

void test_runtime_enum_struct() {
  BValue s1 = alloc_struct2(alloc_enum0(0), alloc_enum0(1));
  assert(get_variant(get_struct_index(s1, 0)) == 0, "index0 == alloc_enum0");
  assert(get_variant(get_struct_index(s1, 1)) == 1, "index0 == alloc_enum0(1)");
}

void test_integer() {
  assert_int_string(bsts_integer_from_int(0), "0", "to_string 0");
  assert_int_string(bsts_integer_from_int(-1), "-1", "to_string -1");
  assert_int_string(bsts_integer_from_int(123456), "123456", "to_string 123456");

  BValue i32_pos = bsts_integer_from_int(305419896); // 0x12345678
  BValue i32_neg = bsts_integer_from_int(-305419896);
  BValue i32_max = bsts_integer_from_int(INT32_MAX);
  BValue i32_min = bsts_integer_from_int(INT32_MIN);

  uint32_t i32_words[1] = { 0x12345678 };
  BValue i32_pos_big = bsts_integer_from_words_copy(1, 1, i32_words);
  BValue i32_neg_big = bsts_integer_from_words_copy(0, 1, i32_words);

  uint32_t i64_words[2] = { 0x9abcdef0, 0x12345678 };
  BValue i64_pos = bsts_integer_from_words_copy(1, 2, i64_words);
  BValue i64_neg = bsts_integer_from_words_copy(0, 2, i64_words);

  uint32_t i128_words[4] = { 0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678 };
  BValue i128_pos = bsts_integer_from_words_copy(1, 4, i128_words);
  BValue i128_neg = bsts_integer_from_words_copy(0, 4, i128_words);

  uint32_t neg_small_words[1] = { 123 };
  BValue neg_small_big = bsts_integer_from_words_copy(0, 1, neg_small_words);

  assert_int_string(i32_pos, "305419896", "i32_pos to_string");
  assert_int_string(i32_neg, "-305419896", "i32_neg to_string");
  assert_int_string(i32_max, "2147483647", "i32_max to_string");
  assert_int_string(i32_min, "-2147483648", "i32_min to_string");
  assert_int_string(i64_pos, "1311768467463790320", "i64_pos to_string");
  assert_int_string(i64_neg, "-1311768467463790320", "i64_neg to_string");
  assert_int_string(i128_pos, "24197857203266734864793317670504947440", "i128_pos to_string");
  assert_int_string(i128_neg, "-24197857203266734864793317670504947440", "i128_neg to_string");

  assert(bsts_integer_equals(i32_pos, i32_pos), "i32_pos equals self");
  assert(bsts_integer_equals(i32_pos, i32_pos_big), "i32_pos equals big");
  assert(bsts_integer_equals(i32_neg, i32_neg_big), "i32_neg equals big");
  assert(!bsts_integer_equals(i32_pos, i32_neg), "i32_pos not equal i32_neg");
  assert(bsts_integer_equals(i128_pos, i128_pos), "i128_pos equals self");
  assert(!bsts_integer_equals(i64_pos, i64_neg), "i64_pos not equal i64_neg");

  assert_int_string(bsts_integer_negate(i32_pos), "-305419896", "negate i32_pos");
  assert_int_string(bsts_integer_negate(i32_neg), "305419896", "negate i32_neg");
  assert_int_string(bsts_integer_negate(i64_pos), "-1311768467463790320", "negate i64_pos");
  assert_int_string(bsts_integer_negate(i64_neg), "1311768467463790320", "negate i64_neg");
  assert_int_string(bsts_integer_negate(i128_pos), "-24197857203266734864793317670504947440", "negate i128_pos");
  assert_int_string(bsts_integer_negate(i128_neg), "24197857203266734864793317670504947440", "negate i128_neg");
  assert_int_string(bsts_integer_negate(i32_min), "2147483648", "negate i32_min");

  assert(bsts_integer_to_int32(i32_pos) == 305419896, "to_int32 i32_pos");
  assert(bsts_integer_to_int32(i32_neg) == -305419896, "to_int32 i32_neg");
  assert(bsts_integer_to_int32(i32_pos_big) == 305419896, "to_int32 i32_pos_big");
  assert(bsts_integer_to_int32(neg_small_big) == -123, "to_int32 neg_small_big");
  assert(bsts_integer_to_int32(i64_pos) == -1698898192, "to_int32 i64_pos trunc");
  assert(bsts_integer_to_int32(i64_neg) == INT32_MIN, "to_int32 i64_neg sentinel");

  struct IntBinCase { const char* name; BValue a; BValue b; const char* expected; };
  struct IntBinCase add_cases[] = {
    { "add i32_pos i32_neg", i32_pos, i32_neg, "0" },
    { "add i64_pos i32_pos", i64_pos, i32_pos, "1311768467769210216" },
    { "add i128_pos i64_neg", i128_pos, i64_neg, "24197857203266734863481549203041157120" },
    { "add i128_neg i32_pos", i128_neg, i32_pos, "-24197857203266734864793317670199527544" },
  };
  for (size_t i = 0; i < sizeof(add_cases) / sizeof(add_cases[0]); i++) {
    assert_int_string(bsts_integer_add(add_cases[i].a, add_cases[i].b), add_cases[i].expected, add_cases[i].name);
  }

  struct IntBinCase mul_cases[] = {
    { "mul i32_pos i32_neg", i32_pos, i32_neg, "-93281312872650816" },
    { "mul i64_pos i32_pos", i64_pos, i32_pos, "400640188908870223300206720" },
    { "mul i128_pos i32_neg", i128_pos, i32_neg, "-7390507030444577022664749144420583314610266240" },
    { "mul i64_neg i64_neg", i64_neg, i64_neg, "1720736512232301123366780340925702400" },
  };
  for (size_t i = 0; i < sizeof(mul_cases) / sizeof(mul_cases[0]); i++) {
    assert_int_string(bsts_integer_times(mul_cases[i].a, mul_cases[i].b), mul_cases[i].expected, mul_cases[i].name);
  }

  struct IntBinCase and_cases[] = {
    { "and i32_pos i32_neg", i32_pos, i32_neg, "8" },
    { "and i64_pos i64_neg", i64_pos, i64_neg, "16" },
    { "and i128_pos i64_pos", i128_pos, i64_pos, "1311768467463790320" },
  };
  for (size_t i = 0; i < sizeof(and_cases) / sizeof(and_cases[0]); i++) {
    assert_int_string(bsts_integer_and(and_cases[i].a, and_cases[i].b), and_cases[i].expected, and_cases[i].name);
  }

  struct IntBinCase or_cases[] = {
    { "or i32_pos i32_neg", i32_pos, i32_neg, "-8" },
    { "or i64_pos i64_neg", i64_pos, i64_neg, "-16" },
    { "or i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734864793317670504947440" },
  };
  for (size_t i = 0; i < sizeof(or_cases) / sizeof(or_cases[0]); i++) {
    assert_int_string(bsts_integer_or(or_cases[i].a, or_cases[i].b), or_cases[i].expected, or_cases[i].name);
  }

  struct IntBinCase xor_cases[] = {
    { "xor i32_pos i32_neg", i32_pos, i32_neg, "-16" },
    { "xor i64_pos i64_neg", i64_pos, i64_neg, "-32" },
    { "xor i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734863481549203041157120" },
  };
  for (size_t i = 0; i < sizeof(xor_cases) / sizeof(xor_cases[0]); i++) {
    assert_int_string(bsts_integer_xor(xor_cases[i].a, xor_cases[i].b), xor_cases[i].expected, xor_cases[i].name);
  }

  struct IntShiftCase { const char* name; BValue value; int shift; const char* expected; };
  struct IntShiftCase shift_cases[] = {
    { "shift i32_pos << 5", i32_pos, 5, "9773436672" },
    { "shift i32_neg >> 5", i32_neg, -5, "-9544372" },
    { "shift i64_pos << 17", i64_pos, 17, "171936116567413924823040" },
    { "shift i64_neg >> 17", i64_neg, -17, "-10007999171935" },
    { "shift i128_pos << 33", i128_pos, 33, "207858010642617301217980562388315306121997844480" },
    { "shift i128_neg >> 33", i128_neg, -33, "-2817001333840509744453397309" },
  };
  for (size_t i = 0; i < sizeof(shift_cases) / sizeof(shift_cases[0]); i++) {
    BValue shift = bsts_integer_from_int(shift_cases[i].shift);
    assert_int_string(bsts_integer_shift_left(shift_cases[i].value, shift), shift_cases[i].expected, shift_cases[i].name);
  }

  struct IntCmpCase { const char* name; BValue a; BValue b; int expected; };
  struct IntCmpCase cmp_cases[] = {
    { "cmp i32_neg i32_pos", i32_neg, i32_pos, -1 },
    { "cmp i64_pos i32_pos", i64_pos, i32_pos, 1 },
    { "cmp i64_neg i32_neg", i64_neg, i32_neg, -1 },
    { "cmp i128_pos i128_pos", i128_pos, i128_pos, 0 },
    { "cmp i128_neg i64_neg", i128_neg, i64_neg, -1 },
  };
  for (size_t i = 0; i < sizeof(cmp_cases) / sizeof(cmp_cases[0]); i++) {
    int cmp = bsts_integer_cmp(cmp_cases[i].a, cmp_cases[i].b);
    if ((cmp_cases[i].expected < 0 && cmp >= 0) ||
        (cmp_cases[i].expected == 0 && cmp != 0) ||
        (cmp_cases[i].expected > 0 && cmp <= 0)) {
      printf("cmp failed: %s got %d\n", cmp_cases[i].name, cmp);
      exit(1);
    }
  }

  struct IntDivModCase { const char* name; BValue a; BValue b; const char* div; const char* mod; };
  struct IntDivModCase div_cases[] = {
    { "divmod i32_pos 7", i32_pos, bsts_integer_from_int(7), "43631413", "5" },
    { "divmod i32_neg 7", i32_neg, bsts_integer_from_int(7), "-43631414", "2" },
    { "divmod i64_pos -12345", i64_pos, bsts_integer_from_int(-12345), "-106259090114524", "-8460" },
    { "divmod i128_neg i64_pos", i128_neg, i64_pos, "-18446744073709551617", "0" },
    { "divmod i64_pos 0", i64_pos, bsts_integer_from_int(0), "0", "1311768467463790320" },
  };
  for (size_t i = 0; i < sizeof(div_cases) / sizeof(div_cases[0]); i++) {
    BValue dm = bsts_integer_div_mod(div_cases[i].a, div_cases[i].b);
    BValue div = get_struct_index(dm, 0);
    BValue mod = get_struct_index(dm, 1);
    assert_int_string(div, div_cases[i].div, div_cases[i].name);
    assert_int_string(mod, div_cases[i].mod, div_cases[i].name);
  }

  struct StrToIntCase { const char* name; const char* text; const char* expected; };
  struct StrToIntCase str_cases[] = {
    { "stoi i32_pos", "305419896", "305419896" },
    { "stoi i32_neg", "-305419896", "-305419896" },
    { "stoi i32_max", "2147483647", "2147483647" },
    { "stoi i32_min", "-2147483648", "-2147483648" },
    { "stoi i64_pos", "1311768467463790320", "1311768467463790320" },
    { "stoi i64_neg", "-1311768467463790320", "-1311768467463790320" },
    { "stoi i128_pos", "24197857203266734864793317670504947440", "24197857203266734864793317670504947440" },
    { "stoi i128_neg", "-24197857203266734864793317670504947440", "-24197857203266734864793317670504947440" },
  };
  for (size_t i = 0; i < sizeof(str_cases) / sizeof(str_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(str_cases[i].text), (char*)str_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_int(opt, str_cases[i].expected, str_cases[i].name);
  }

  struct StrToIntCase none_cases[] = {
    { "stoi empty", "", NULL },
    { "stoi dash", "-", NULL },
    { "stoi junk", "12x3", NULL },
  };
  for (size_t i = 0; i < sizeof(none_cases) / sizeof(none_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(none_cases[i].text), (char*)none_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_none(opt, none_cases[i].name);
  }
}

void test_runtime_strings() {

  char* hello = "hello1";

  BValue v1 = bsts_string_from_utf8_bytes_copy(5, "hello");
  // we can ignore trailing byte string on hello, by taking the front
  BValue v2 = bsts_string_from_utf8_bytes_static(5, hello);
  assert(bsts_string_equals(v1, v2), "v1 == v2");
  assert(bsts_string_equals(v1, v1), "v1 == v1");
  assert(bsts_string_equals(v2, v2), "v2 == v2");
  //codepoint tests
  assert(bsts_string_code_point_bytes(v1, 0) == 1, "code_point_bytes(v[0]) == 1");
  assert(bsts_string_char_at(v1, 0) == bsts_char_from_code_point(104), "char_at(v, 0) == 104");
  assert(bsts_string_char_at(v1, 1) == bsts_char_from_code_point(101), "char_at(v, 1) == 101");
  BValue v3 = bsts_string_from_utf8_bytes_static(4, "\x00F0\x009F\x0098\x008A");
  assert(bsts_string_char_at(v3, 0) == bsts_char_from_code_point(0x1F60A), "smiley check char");
  assert(bsts_string_code_point_bytes(v3, 0) == 4, "smiley length");

  BValue v1tail = bsts_string_substring_tail(v1, 1);
  BValue v2tail = bsts_string_substring_tail(v2, 1);
  BValue tail_expected = bsts_string_from_utf8_bytes_static(4, "ello");
  assert(bsts_string_equals(v1tail, v2tail), "v1tail == v2tail");
  assert(bsts_string_equals(v1tail, tail_expected), "v1tail == expected");

  {
    BValue hello_world1 = bsts_string_from_utf8_bytes_static(11, "hello world");
    BValue hello1 = bsts_string_from_utf8_bytes_static(5, "world");
    int find1 = bsts_string_find(hello_world1, hello1, 0);
    assert(find1 == 6, "find1");
    int find2 = bsts_string_find(hello_world1, hello1, 1);
    assert(find2 == 6, "find2");
    int find3 = bsts_string_find(hello_world1, hello1, 7);
    assert(find3 == -1, "find3");
  }

}

void test_float64() {
  uint64_t bits_cases[] = {
    UINT64_C(0x0000000000000000),
    UINT64_C(0x8000000000000000),
    UINT64_C(0x3ff0000000000000),
    UINT64_C(0xbff0000000000000),
    UINT64_C(0x7ff0000000000000),
    UINT64_C(0xfff0000000000000),
    UINT64_C(0x7ff8000000000001),
    UINT64_C(0x7ff8000000000002),
    UINT64_C(0x0000000000000001),
    UINT64_C(0x7fefffffffffffff)
  };
  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue v = bsts_float64_from_bits(bits_cases[i]);
    assert_u64_equals(bsts_float64_to_bits(v), bits_cases[i], "float bits roundtrip");
  }

  BValue neg_zero = bsts_float64_from_bits(UINT64_C(0x8000000000000000));
  BValue pos_zero = bsts_float64_from_bits(UINT64_C(0x0000000000000000));
  BValue nan1 = bsts_float64_from_bits(UINT64_C(0x7ff8000000000001));
  BValue nan2 = bsts_float64_from_bits(UINT64_C(0x7ff8000000000002));
  BValue one = bsts_float64_from_double(1.0);
  BValue two = bsts_float64_from_double(2.0);

  assert(bsts_float64_cmp_total(nan1, one) < 0, "nan sorts before non-nan");
  assert(bsts_float64_cmp_total(one, nan1) > 0, "non-nan sorts after nan");
  assert(bsts_float64_cmp_total(nan1, nan2) == 0, "all nan compare equal");
  assert(bsts_float64_cmp_total(neg_zero, pos_zero) == 0, "-0.0 compares equal to 0.0");
  assert(bsts_float64_cmp_total(one, two) < 0, "1.0 < 2.0");
  assert(bsts_float64_equals(neg_zero, pos_zero) == 1, "float equality treats signed zeros as equal");
  assert(bsts_float64_equals(nan1, nan2) == 1, "float equality matches all nan values");

  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue f = bsts_float64_from_bits(bits_cases[i]);
    BValue as_int = ___bsts_g_Bosatsu_l_Float_l_Float64_l_float64__bits__to__Int(f);
    BValue round = ___bsts_g_Bosatsu_l_Float_l_Float64_l_int__bits__to__Float64(as_int);
    assert_u64_equals(bsts_float64_to_bits(round), bits_cases[i], "float64 <-> int bits roundtrip");
  }

  BValue minus_one = bsts_integer_from_int(-1);
  BValue minus_one_float = ___bsts_g_Bosatsu_l_Float_l_Float64_l_int__bits__to__Float64(minus_one);
  assert_u64_equals(bsts_float64_to_bits(minus_one_float), UINT64_C(0xffffffffffffffff), "int_bits uses low 64 two's complement");

  {
    BValue inf_str = bsts_string_from_utf8_bytes_static(3, "\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(inf_str);
    assert_option_float_bits(parsed, UINT64_C(0x7ff0000000000000), "parse +infinity");
  }
  {
    BValue ninf_str = bsts_string_from_utf8_bytes_static(4, "-\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(ninf_str);
    assert_option_float_bits(parsed, UINT64_C(0xfff0000000000000), "parse -infinity");
  }
  {
    uint64_t nan_bits = UINT64_C(0x7ff80000000000ab);
    BValue nanv = bsts_float64_from_bits(nan_bits);
    BValue nan_str = ___bsts_g_Bosatsu_l_Float_l_Float64_l_float64__to__String(nanv);
    BValue parsed = ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(nan_str);
    assert_option_float_bits(parsed, nan_bits, "parse NaN payload");
  }
  {
    BValue nan_lit = bsts_string_from_utf8_bytes_static(4, ".NaN");
    BValue parsed = ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(nan_lit);
    assert(get_variant(parsed) == 1, "parse .NaN returns Some");
    assert(isnan(bsts_float64_to_double(get_enum_index(parsed, 0))), "parse .NaN produces NaN");
  }
  {
    BValue bad = bsts_string_from_utf8_bytes_static(6, "nope42");
    BValue parsed = ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(bad);
    assert(get_variant(parsed) == 0, "invalid float string returns None");
  }
}

int main(int argc, char** argv) {

  GC_init();
  test_runtime_enum_struct();
  test_runtime_strings();
  test_integer();
  test_float64();
  printf("success\n");
  return 0;
}
