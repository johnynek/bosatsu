#include "bosatsu_runtime.h"
#include "bosatsu_array_internal.h"
#include <limits.h>
#include "bosatsu_ext_Bosatsu_l_Collection_l_Array.h"
#include "bosatsu_ext_Bosatsu_l_Predef.h"
#include "bosatsu_ext_Bosatsu_l_Num_l_Float64.h"
#include "bosatsu_ext_Bosatsu_l_Num_l_Int64.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#if !defined(_WIN32)
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif
#include "gc.h"

void assert(_Bool cond, char* message) {
  if (!cond) {
    printf("%s\n", message);
    exit(1);
  }
}

void assert_string_equals(BValue got, const char* expected, const char* message) {
  BValue exp = bsts_string_from_utf8_bytes_static(strlen(expected), expected);
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

void assert_string_bytes(BValue got, const char* expected, size_t expected_len, const char* message) {
  BSTS_String_View view = bsts_string_view_ref(&got);
  size_t got_len = view.len;
  const char* got_bytes = view.bytes;
  if (got_len != expected_len || (expected_len > 0 && memcmp(got_bytes, expected, expected_len) != 0)) {
    printf("%s\nexpected len: %zu\ngot len: %zu\n", message, expected_len, got_len);
    exit(1);
  }
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

void assert_int64_bits(BValue value, uint64_t expected, const char* message) {
  assert_u64_equals(bsts_int64_to_bits(value), expected, message);
}

void assert_option_int64_bits(BValue opt, uint64_t expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(Int64)\n", message);
    exit(1);
  }
  assert_int64_bits(get_enum_index(opt, 0), expected, message);
}

void assert_is_small_int(BValue value, const char* message) {
  if ((value & (BValue)0x1) != (BValue)0x1) {
    printf("%s\nexpected small-int immediate\n", message);
    exit(1);
  }
}

void assert_is_big_int(BValue value, const char* message) {
  if ((value & (BValue)0x1) != (BValue)0x0) {
    printf("%s\nexpected heap-backed integer\n", message);
    exit(1);
  }
}

static BValue prog_assoc_pure_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_pure(arg);
}

static BValue prog_assoc_raise_fn(BValue arg) {
  return ___bsts_g_Bosatsu_l_Prog_l_raise__error(arg);
}

#if !defined(_WIN32)
typedef void (*VoidFn)(void);

static void assert_child_aborts(VoidFn fn, const char* message) {
  pid_t pid = fork();
  if (pid < 0) {
    perror("fork failed");
    exit(1);
  }
  if (pid == 0) {
    fn();
    _exit(0);
  }

  int status = 0;
  if (waitpid(pid, &status, 0) < 0) {
    perror("waitpid failed");
    exit(1);
  }

  if (!(WIFSIGNALED(status) && WTERMSIG(status) == SIGABRT)) {
    printf("%s\n", message);
    exit(1);
  }
}

static void call_string_copy_too_large() {
  (void)bsts_string_from_utf8_bytes_copy(BSTS_STRING_INLINE16_FLAG, "x");
}

static void call_string_static_too_large() {
  (void)bsts_string_from_utf8_bytes_static(BSTS_STRING_INLINE16_FLAG, "x");
}

static void call_string_mut_too_large() {
  (void)bsts_string_mut(BSTS_STRING_INLINE16_FLAG);
}

#if defined(BSTS_RUNTIME_DEBUG_CHECKS)
static BValue closure_zero_abort_fn(BValue *slots, BValue arg) {
  (void)slots;
  return arg;
}

static void call_alloc_closure_zero() {
  (void)alloc_closure1(0, NULL, closure_zero_abort_fn);
}
#endif
#endif

void assert_option_float_bits(BValue opt, uint64_t expected, const char* message) {
  if (get_variant(opt) != 1) {
    printf("%s\nexpected: Some(float64)\ngot: None\n", message);
    exit(1);
  }
  BValue v = get_enum_index(opt, 0);
  assert_u64_equals(bsts_float64_to_bits(v), expected, message);
}

static BSTS_Array* test_array_unbox(BValue array) {
  return BSTS_PTR(BSTS_Array, array);
}

static BValue test_array_from_values(size_t len, const BValue* values) {
  BValue* data = NULL;
  if (len > 0) {
    data = (BValue*)GC_malloc(sizeof(BValue) * len);
    if (data == NULL) {
      perror("GC_malloc failure in test_array_from_values data");
      exit(1);
    }
    memcpy(data, values, sizeof(BValue) * len);
  }

  BSTS_Array* arr = (BSTS_Array*)GC_malloc(sizeof(BSTS_Array));
  if (arr == NULL) {
    perror("GC_malloc failure in test_array_from_values array");
    exit(1);
  }

  arr->data = data;
  arr->offset = 0;
  arr->len = (int)len;
  return BSTS_VALUE_FROM_PTR(arr);
}

static void assert_int_array_equals(BValue array, const int* expected, size_t expected_len, const char* message) {
  BSTS_Array* arr = test_array_unbox(array);
  if ((size_t)arr->len != expected_len) {
    printf("%s\nexpected len: %zu\ngot len: %d\n", message, expected_len, arr->len);
    exit(1);
  }

  for (size_t idx = 0; idx < expected_len; idx++) {
    BValue got = arr->data[arr->offset + (int)idx];
    if (bsts_integer_cmp(got, bsts_integer_from_int(expected[idx])) != 0) {
      printf("%s\nmismatch at index %zu\n", message, idx);
      exit(1);
    }
  }
}

static void assert_int64_array_bits(BValue array, const uint64_t* expected, size_t expected_len, const char* message) {
  BSTS_Array* arr = test_array_unbox(array);
  if ((size_t)arr->len != expected_len) {
    printf("%s\nexpected len: %zu\ngot len: %d\n", message, expected_len, arr->len);
    exit(1);
  }

  for (size_t idx = 0; idx < expected_len; idx++) {
    assert_u64_equals(
        bsts_int64_to_bits(arr->data[arr->offset + (int)idx]),
        expected[idx],
        message);
  }
}

static BValue array_identity_i64_fn(BValue arg) {
  return arg;
}

static BValue array_echo_i64_fn(BValue arg) {
  return arg;
}

static BValue array_fold_index_sum_fn(BValue acc, BValue item, BValue idx) {
  int64_t idx_i64 = (int64_t)bsts_int64_to_bits(idx);
  return bsts_integer_add(
      acc,
      bsts_integer_add(item, bsts_integer_from_int64(idx_i64)));
}

static BValue array_map_index_sum_fn(BValue item, BValue idx) {
  int64_t idx_i64 = (int64_t)bsts_int64_to_bits(idx);
  return bsts_integer_add(item, bsts_integer_from_int64(idx_i64));
}

static BValue array_zip_add_fn(BValue left, BValue right) {
  return bsts_integer_add(left, right);
}

static BValue array_zip_accum_add_fn(BValue acc, BValue left, BValue right) {
  return bsts_integer_add(acc, bsts_integer_add(left, right));
}

static BValue array_float_mul_fn(BValue left, BValue right) {
  return bsts_float64_from_double(
      bsts_float64_to_double(left) * bsts_float64_to_double(right));
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
  BValue i61_max = bsts_integer_from_int64((INT64_C(1) << 61) - 1);
  BValue i61_min = bsts_integer_from_int64(-(INT64_C(1) << 61));
  BValue i62_max = bsts_integer_from_int64((INT64_C(1) << 62) - 1);
  BValue i62_min = bsts_integer_from_int64(-(INT64_C(1) << 62));

  uint32_t i61_max_mag_words[2] = { 0xffffffff, 0x1fffffff };
  uint32_t i61_min_mag_words[2] = { 0x00000000, 0x20000000 };
  uint32_t i61_under_mag_words[2] = { 0x00000001, 0x20000000 };
  BValue i61_max_from_words = bsts_integer_from_words_copy(1, 2, i61_max_mag_words);
  BValue i61_min_from_words = bsts_integer_from_words_copy(0, 2, i61_min_mag_words);
  BValue i61_over = bsts_integer_from_words_copy(1, 2, i61_min_mag_words);
  BValue i61_under = bsts_integer_from_words_copy(0, 2, i61_under_mag_words);
  uint32_t i62_max_mag_words[2] = { 0xffffffff, 0x3fffffff };
  uint32_t i62_min_mag_words[2] = { 0x00000000, 0x40000000 };
  uint32_t i62_under_mag_words[2] = { 0x00000001, 0x40000000 };
  BValue i62_max_from_words = bsts_integer_from_words_copy(1, 2, i62_max_mag_words);
  BValue i62_min_from_words = bsts_integer_from_words_copy(0, 2, i62_min_mag_words);
  BValue i62_over = bsts_integer_from_words_copy(1, 2, i62_min_mag_words);
  BValue i62_under = bsts_integer_from_words_copy(0, 2, i62_under_mag_words);
  BValue s62_pos = bsts_integer_from_int64((INT64_C(1) << 40) + 1234);
  BValue s62_neg = bsts_integer_from_int64(-((INT64_C(1) << 40) + 1234));
  BValue pow40 = bsts_integer_from_int64(INT64_C(1) << 40);
  BValue pow2_32 = bsts_integer_from_int(32);
  BValue pow2_neg_32 = bsts_integer_from_int(-32);

  uint32_t i128_words[4] = { 0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678 };
  BValue i128_pos = bsts_integer_from_words_copy(1, 4, i128_words);
  BValue i128_neg = bsts_integer_from_words_copy(0, 4, i128_words);
  uint32_t and_small_left_words[4] = { 0x12345678, 0x00000000, 0xaaaaaaaa, 0xaaaaaaaa };
  uint32_t and_small_right_words[4] = { 0xffffffff, 0x00000000, 0x55555555, 0x55555555 };
  uint32_t xor_small_left_words[4] = { 0x12345678, 0x00000000, 0xaaaaaaaa, 0xbbbbbbbb };
  uint32_t xor_small_right_words[4] = { 0x9abcdef0, 0x00000000, 0xaaaaaaaa, 0xbbbbbbbb };
  BValue and_small_left = bsts_integer_from_words_copy(1, 4, and_small_left_words);
  BValue and_small_right = bsts_integer_from_words_copy(1, 4, and_small_right_words);
  BValue xor_small_left = bsts_integer_from_words_copy(1, 4, xor_small_left_words);
  BValue xor_small_right = bsts_integer_from_words_copy(1, 4, xor_small_right_words);

  uint32_t neg_small_words[1] = { 123 };
  BValue neg_small_big = bsts_integer_from_words_copy(0, 1, neg_small_words);

  assert_int_string(i32_pos, "305419896", "i32_pos to_string");
  assert_int_string(i32_neg, "-305419896", "i32_neg to_string");
  assert_int_string(i32_max, "2147483647", "i32_max to_string");
  assert_int_string(i32_min, "-2147483648", "i32_min to_string");
  assert_int_string(i64_pos, "1311768467463790320", "i64_pos to_string");
  assert_int_string(i64_neg, "-1311768467463790320", "i64_neg to_string");
  assert_int_string(i61_max, "2305843009213693951", "i61_max to_string");
  assert_int_string(i61_min, "-2305843009213693952", "i61_min to_string");
  assert_int_string(i62_max, "4611686018427387903", "i62_max to_string");
  assert_int_string(i62_min, "-4611686018427387904", "i62_min to_string");
  assert_int_string(i61_over, "2305843009213693952", "i61_over to_string");
  assert_int_string(i61_under, "-2305843009213693953", "i61_under to_string");
  assert_int_string(i62_over, "4611686018427387904", "i62_over to_string");
  assert_int_string(i62_under, "-4611686018427387905", "i62_under to_string");
  assert_int_string(s62_pos, "1099511629010", "s62_pos to_string");
  assert_int_string(s62_neg, "-1099511629010", "s62_neg to_string");
  assert_int_string(i128_pos, "24197857203266734864793317670504947440", "i128_pos to_string");
  assert_int_string(i128_neg, "-24197857203266734864793317670504947440", "i128_neg to_string");

  assert_is_small_int(i64_pos, "i64_pos should be immediate");
  assert_is_small_int(i64_neg, "i64_neg should be immediate");
  assert_is_small_int(i61_max, "i61_max should be immediate");
  assert_is_small_int(i61_min, "i61_min should be immediate");
  assert_is_small_int(i62_max, "i62_max should be immediate");
  assert_is_small_int(i62_min, "i62_min should be immediate");
  assert_is_small_int(i61_max_from_words, "i61_max_from_words should canonicalize to immediate");
  assert_is_small_int(i61_min_from_words, "i61_min_from_words should canonicalize to immediate");
  assert_is_small_int(i62_max_from_words, "i62_max_from_words should canonicalize to immediate");
  assert_is_small_int(i62_min_from_words, "i62_min_from_words should canonicalize to immediate");
  assert_is_small_int(i61_over, "i61_over should now be immediate");
  assert_is_small_int(i61_under, "i61_under should now be immediate");
  assert_is_small_int(s62_pos, "s62_pos should be immediate");
  assert_is_small_int(s62_neg, "s62_neg should be immediate");
  assert_is_big_int(i62_over, "i62_over should spill to big-int");
  assert_is_big_int(i62_under, "i62_under should spill to big-int");

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

  BValue add_i61_over = bsts_integer_add(i61_max, bsts_integer_from_int(1));
  BValue sub_i61_under = bsts_integer_add(i61_min, bsts_integer_from_int(-1));
  BValue neg_i61_min = bsts_integer_negate(i61_min);
  assert_int_string(add_i61_over, "2305843009213693952", "add i61_max 1");
  assert_int_string(sub_i61_under, "-2305843009213693953", "sub i61_min 1");
  assert_int_string(neg_i61_min, "2305843009213693952", "negate i61_min");
  assert_is_big_int(add_i61_over, "add i61_max 1 should spill to big-int");
  assert_is_big_int(sub_i61_under, "sub i61_min 1 should spill to big-int");
  assert_is_big_int(neg_i61_min, "negate i61_min should spill to big-int");

  assert(bsts_integer_to_int32(i32_pos) == 305419896, "to_int32 i32_pos");
  assert(bsts_integer_to_int32(i32_neg) == -305419896, "to_int32 i32_neg");
  assert(bsts_integer_to_int32(i32_pos_big) == 305419896, "to_int32 i32_pos_big");
  assert(bsts_integer_to_int32(neg_small_big) == -123, "to_int32 neg_small_big");
  assert(bsts_integer_to_int32(i64_pos) == -1698898192, "to_int32 i64_pos trunc");
  assert(bsts_integer_to_int32(i64_neg) == INT32_MIN, "to_int32 i64_neg sentinel");
  assert(bsts_integer_to_int32(i61_max) == -1, "to_int32 i61_max trunc");
  assert(bsts_integer_to_int32(i61_min) == INT32_MIN, "to_int32 i61_min sentinel");

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
    { "mul i128_pos 32", i128_pos, pow2_32, "774331430504535515673386165456158318080" },
    { "mul i128_pos -32", i128_pos, pow2_neg_32, "-774331430504535515673386165456158318080" },
    { "mul i64_neg i64_neg", i64_neg, i64_neg, "1720736512232301123366780340925702400" },
  };
  for (size_t i = 0; i < sizeof(mul_cases) / sizeof(mul_cases[0]); i++) {
    assert_int_string(bsts_integer_times(mul_cases[i].a, mul_cases[i].b), mul_cases[i].expected, mul_cases[i].name);
  }

  BValue mul_small_small = bsts_integer_times(
      bsts_integer_from_int64(INT64_C(1) << 30),
      bsts_integer_from_int64(INT64_C(1) << 20));
  BValue mul_small_over = bsts_integer_times(
      bsts_integer_from_int64(INT64_C(1) << 31),
      bsts_integer_from_int64(INT64_C(1) << 31));
  assert_int_string(mul_small_small, "1125899906842624", "mul small range stays immediate");
  assert_int_string(mul_small_over, "4611686018427387904", "mul small overflow spills");
  assert_is_small_int(mul_small_small, "mul small range should stay immediate");
  assert_is_big_int(mul_small_over, "mul small overflow should spill to big-int");

  struct IntBinCase and_cases[] = {
    { "and i32_pos i32_neg", i32_pos, i32_neg, "8" },
    { "and i64_pos i64_neg", i64_pos, i64_neg, "16" },
    { "and i128_pos i64_pos", i128_pos, i64_pos, "1311768467463790320" },
    { "and s62_pos s62_neg", s62_pos, s62_neg, "2" },
  };
  for (size_t i = 0; i < sizeof(and_cases) / sizeof(and_cases[0]); i++) {
    assert_int_string(bsts_integer_and(and_cases[i].a, and_cases[i].b), and_cases[i].expected, and_cases[i].name);
  }

  struct IntBinCase or_cases[] = {
    { "or i32_pos i32_neg", i32_pos, i32_neg, "-8" },
    { "or i64_pos i64_neg", i64_pos, i64_neg, "-16" },
    { "or i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734864793317670504947440" },
    { "or s62_pos s62_neg", s62_pos, s62_neg, "-2" },
  };
  for (size_t i = 0; i < sizeof(or_cases) / sizeof(or_cases[0]); i++) {
    assert_int_string(bsts_integer_or(or_cases[i].a, or_cases[i].b), or_cases[i].expected, or_cases[i].name);
  }

  struct IntBinCase xor_cases[] = {
    { "xor i32_pos i32_neg", i32_pos, i32_neg, "-16" },
    { "xor i64_pos i64_neg", i64_pos, i64_neg, "-32" },
    { "xor i128_pos i64_pos", i128_pos, i64_pos, "24197857203266734863481549203041157120" },
    { "xor s62_pos s62_neg", s62_pos, s62_neg, "-4" },
  };
  for (size_t i = 0; i < sizeof(xor_cases) / sizeof(xor_cases[0]); i++) {
    assert_int_string(bsts_integer_xor(xor_cases[i].a, xor_cases[i].b), xor_cases[i].expected, xor_cases[i].name);
  }

  BValue and_small_result = bsts_integer_and(and_small_left, and_small_right);
  BValue xor_small_result = bsts_integer_xor(xor_small_left, xor_small_right);
  assert_int_string(and_small_result, "305419896", "and big big small result");
  assert_int_string(xor_small_result, "2290649224", "xor big big small result");
  assert_is_small_int(and_small_result, "and big big small should canonicalize to immediate");
  assert_is_small_int(xor_small_result, "xor big big small should canonicalize to immediate");

  struct IntShiftCase { const char* name; BValue value; int shift; const char* expected; };
  struct IntShiftCase shift_cases[] = {
    { "shift i32_pos << 5", i32_pos, 5, "9773436672" },
    { "shift i32_neg << 1", i32_neg, 1, "-610839792" },
    { "shift i32_pos >> 1", i32_pos, -1, "152709948" },
    { "shift i32_neg >> 5", i32_neg, -5, "-9544372" },
    { "shift i64_pos << 17", i64_pos, 17, "171936116567413924823040" },
    { "shift pow40 >> 32", pow40, -32, "256" },
    { "shift i64_neg >> 17", i64_neg, -17, "-10007999171935" },
    { "shift pow40 << 5", pow40, 5, "35184372088832" },
    { "shift pow40 << 30", pow40, 30, "1180591620717411303424" },
    { "shift i128_pos << 33", i128_pos, 33, "207858010642617301217980562388315306121997844480" },
    { "shift i128_pos >> 33", i128_pos, -33, "2817001333840509744453397308" },
    { "shift i128_neg >> 33", i128_neg, -33, "-2817001333840509744453397309" },
  };
  for (size_t i = 0; i < sizeof(shift_cases) / sizeof(shift_cases[0]); i++) {
    BValue shift = bsts_integer_from_int(shift_cases[i].shift);
    assert_int_string(bsts_integer_shift_left(shift_cases[i].value, shift), shift_cases[i].expected, shift_cases[i].name);
  }

  BValue shift_twos_small = bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(40));
  BValue shift_small_left1 = bsts_integer_shift_left(i32_pos, bsts_integer_from_int(1));
  BValue shift_small_right1 = bsts_integer_shift_left(i32_pos, bsts_integer_from_int(-1));
  BValue shift_big_right64_small = bsts_integer_shift_left(i128_pos, bsts_integer_from_int(-64));
  BValue shift_big_right96_small = bsts_integer_shift_left(i128_pos, bsts_integer_from_int(-96));
  BValue shift_twos_big = bsts_integer_shift_left(pow40, bsts_integer_from_int(30));
  assert_int_string(shift_twos_small, "1099511627776", "shift twos small canonicalization");
  assert_int_string(shift_small_left1, "610839792", "shift small left1 canonicalization");
  assert_int_string(shift_small_right1, "152709948", "shift small right1 canonicalization");
  assert_int_string(shift_big_right64_small, "1311768467463790320", "shift big right64 canonicalization");
  assert_int_string(shift_big_right96_small, "305419896", "shift big right96 canonicalization");
  assert_is_small_int(shift_twos_small, "shift twos small should stay immediate");
  assert_is_small_int(shift_small_left1, "shift small left1 should stay immediate");
  assert_is_small_int(shift_small_right1, "shift small right1 should stay immediate");
  assert_is_small_int(shift_big_right64_small, "shift big right64 should canonicalize to immediate");
  assert_is_small_int(shift_big_right96_small, "shift big right96 should canonicalize to immediate");
  assert_is_big_int(shift_twos_big, "shift pow40 << 30 should spill to big-int");

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
    { "divmod i61_max 7", i61_max, bsts_integer_from_int(7), "329406144173384850", "1" },
    { "divmod i61_min 7", i61_min, bsts_integer_from_int(7), "-329406144173384851", "5" },
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
    { "stoi i61_max", "2305843009213693951", "2305843009213693951" },
    { "stoi i61_min", "-2305843009213693952", "-2305843009213693952" },
    { "stoi i61_over", "2305843009213693952", "2305843009213693952" },
    { "stoi i61_under", "-2305843009213693953", "-2305843009213693953" },
    { "stoi i128_pos", "24197857203266734864793317670504947440", "24197857203266734864793317670504947440" },
    { "stoi i128_neg", "-24197857203266734864793317670504947440", "-24197857203266734864793317670504947440" },
  };
  for (size_t i = 0; i < sizeof(str_cases) / sizeof(str_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(str_cases[i].text), str_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_int(opt, str_cases[i].expected, str_cases[i].name);
  }

  struct StrToIntCase none_cases[] = {
    { "stoi empty", "", NULL },
    { "stoi dash", "-", NULL },
    { "stoi junk", "12x3", NULL },
  };
  for (size_t i = 0; i < sizeof(none_cases) / sizeof(none_cases[0]); i++) {
    BValue s = bsts_string_from_utf8_bytes_static(strlen(none_cases[i].text), none_cases[i].text);
    BValue opt = bsts_string_to_integer(s);
    assert_option_none(opt, none_cases[i].name);
  }
}

void test_runtime_strings() {

  char* hello = "hello1";

#if !defined(_WIN32)
  assert_child_aborts(call_string_copy_too_large, "oversized copy string should abort");
  assert_child_aborts(call_string_static_too_large, "oversized static string should abort");
  assert_child_aborts(call_string_mut_too_large, "oversized mutable string should abort");
#endif

  {
    BValue ch_ascii = bsts_char_from_code_point('A');
    BValue ch_null = bsts_char_from_code_point(0);
    BValue ch_smile = bsts_char_from_code_point(0x1F60A);
    assert(bsts_char_code_point_from_value(ch_ascii) == 'A', "char ascii roundtrip");
    assert(bsts_char_code_point_from_value(ch_null) == 0, "char NUL roundtrip");
    assert(bsts_char_code_point_from_value(ch_smile) == 0x1F60A, "char 4-byte roundtrip");
  }

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

  {
    BValue empty_a = bsts_string_from_utf8_bytes_static(0, NULL);
    BValue empty_b = bsts_string_from_utf8_bytes_static(0, "");
    assert(bsts_string_equals(empty_a, empty_b), "empty small strings compare equal");
    assert(bsts_string_utf8_len_ref(&empty_a) == 0, "empty small string has len 0");
    free_on_close(empty_a);
  }

  {
    BValue s7_static = bsts_string_from_utf8_bytes_static(7, "abcdefg");
    BValue s7_copy = bsts_string_from_utf8_bytes_copy(7, "abcdefg");
    BValue s8_static = bsts_string_from_utf8_bytes_static(8, "abcdefgh");
    assert(bsts_string_equals(s7_static, s7_copy), "small static/copy equals");
    assert(bsts_string_cmp(s7_static, s7_copy) == 0, "small static/copy cmp");
    assert(bsts_string_cmp(s7_static, s8_static) < 0, "small cmp against heap string");
    assert_string_bytes(s7_static, "abcdefg", 7, "small bytes roundtrip");
    assert_string_bytes(s8_static, "abcdefgh", 8, "heap bytes roundtrip");
  }

  {
    BValue barbara = bsts_string_from_utf8_bytes_static(7, "BARBARA");
    BValue linda = bsts_string_from_utf8_bytes_static(5, "LINDA");
    BValue a = bsts_string_from_utf8_bytes_static(1, "A");
    BValue c = bsts_string_from_utf8_bytes_static(1, "C");
    assert(bsts_string_cmp(barbara, linda) == -1, "cmp BARBARA < LINDA normalized");
    assert(bsts_string_cmp(linda, barbara) == 1, "cmp LINDA > BARBARA normalized");
    assert(bsts_string_cmp(a, c) == -1, "cmp A < C normalized");
    assert(bsts_string_cmp(c, a) == 1, "cmp C > A normalized");
  }

  {
    BValue u_e000 = bsts_char_from_code_point(0xE000);
    BValue u_ffff = bsts_char_from_code_point(0xFFFF);
    BValue u_10000 = bsts_char_from_code_point(0x10000);
    BValue a_u_e000 = bsts_string_from_utf8_bytes_static(4, "a\xEE\x80\x80");
    BValue a_u_10000 = bsts_string_from_utf8_bytes_static(5, "a\xF0\x90\x80\x80");

    assert(bsts_string_cmp(u_e000, u_10000) == -1, "cmp U+E000 < U+10000");
    assert(bsts_string_cmp(u_ffff, u_10000) == -1, "cmp U+FFFF < U+10000");
    assert(bsts_string_cmp(a_u_e000, a_u_10000) == -1, "cmp a+U+E000 < a+U+10000");
    assert(___bsts_g_Bosatsu_l_Predef_l_cmp__Char(u_e000, u_10000) == alloc_enum0(0), "cmp_Char U+E000 < U+10000");
    assert(___bsts_g_Bosatsu_l_Predef_l_eq__Char(u_e000, u_e000) == alloc_enum0(1), "eq_Char same");
    assert(___bsts_g_Bosatsu_l_Predef_l_eq__Char(u_e000, u_10000) == alloc_enum0(0), "eq_Char different");
  }

  {
    BValue long_s = bsts_string_from_utf8_bytes_static(10, "0123456789");
    BValue mid = bsts_string_substring(long_s, 3, 7);
    BValue tail = bsts_string_substring_tail(long_s, 8);
    assert_string_bytes(mid, "3456", 4, "substring long->small");
    assert_string_bytes(tail, "89", 2, "substring_tail long->small");
    assert(bsts_string_find(long_s, mid, 0) == 3, "find long with small needle");
    assert(bsts_string_rfind(long_s, bsts_string_from_utf8_bytes_static(2, "89"), 9) == 8, "rfind long with small needle");
  }

  {
    BValue small_hay = bsts_string_from_utf8_bytes_static(7, "abcbcba");
    BValue small_need = bsts_string_from_utf8_bytes_static(2, "bc");
    assert(bsts_string_find(small_hay, small_need, 0) == 1, "find in small haystack");
    assert(bsts_string_rfind(small_hay, small_need, 6) == 3, "rfind in small haystack");
    assert(bsts_string_char_at(small_hay, 6) == bsts_char_from_code_point('a'), "char_at small haystack");
  }

  {
    BValue a = bsts_string_from_utf8_bytes_static(3, "foo");
    BValue b = bsts_string_from_utf8_bytes_static(3, "bar");
    const char* a_bytes = bsts_string_utf8_bytes_ref(&a);
    char a_copy[3];
    memcpy(a_copy, a_bytes, 3);
    const char* b_bytes = bsts_string_utf8_bytes_ref(&b);
    assert(memcmp(a_copy, "foo", 3) == 0, "first small bytes survive second lookup");
    assert(memcmp(b_bytes, "bar", 3) == 0, "second small bytes are correct");
  }

  {
    BValue tiny = bsts_string_from_utf8_bytes_static(5, "abcde");
    BSTS_String_View view = bsts_string_view_ref(&tiny);
    const char* expected = ((const char*)(const void*)&tiny) + 1;
    assert(view.len == 5, "tiny view length");
    assert(view.bytes == expected, "tiny view points inside BValue storage");
    assert(view.bytes[5] == '\0', "tiny view is NUL terminated");
  }

  {
    BValue inline16 = bsts_string_from_utf8_bytes_copy(16, "abcdefghijklmnop");
    BSTS_String* inline16_str = BSTS_PTR(BSTS_String, inline16);
    assert((inline16_str->len_meta & BSTS_STRING_INLINE16_FLAG) != 0, "len16 string is inline");
    assert_string_bytes(inline16, "abcdefghijklmnop", 16, "len16 bytes roundtrip");

    BValue large = bsts_string_from_utf8_bytes_copy(17, "abcdefghijklmnopq");
    BSTS_String* large_str = BSTS_PTR(BSTS_String, large);
    assert((large_str->len_meta & BSTS_STRING_INLINE16_FLAG) == 0, "len17 string is external");
    assert_string_bytes(large, "abcdefghijklmnopq", 17, "len17 bytes roundtrip");
  }

  {
    static const BSTS_String static_lit = BSTS_STATIC_STRING_INIT(18, "abcdefghijklmnopqr");
    BValue lit = BSTS_VALUE_FROM_PTR(&static_lit);
    const char* lit_bytes = bsts_string_utf8_bytes_ref(&lit);
    assert_string_bytes(lit, "abcdefghijklmnopqr", 18, "boxed static literal bytes");
    assert(lit_bytes == static_lit.payload.ext.bytes, "boxed static literal is zero-copy");
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
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(neg_zero, pos_zero)) == 1,
      "predef eq_Float64 treats signed zeros as equal");
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(nan1, nan2)) == 1,
      "predef eq_Float64 treats all nan values as equal");
  assert(get_variant_value(___bsts_g_Bosatsu_l_Predef_l_eq__Float64(one, two)) == 0,
      "predef eq_Float64 is false for different finite values");

  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue f = bsts_float64_from_bits(bits_cases[i]);
    BValue as_int = ___bsts_g_Bosatsu_l_Num_l_Float64_l_float64__bits__to__Int(f);
    BValue round = ___bsts_g_Bosatsu_l_Num_l_Float64_l_int__bits__to__Float64(as_int);
    assert_u64_equals(bsts_float64_to_bits(round), bits_cases[i], "float64 <-> int bits roundtrip");
  }

  BValue minus_one = bsts_integer_from_int(-1);
  BValue minus_one_float = ___bsts_g_Bosatsu_l_Num_l_Float64_l_int__bits__to__Float64(minus_one);
  assert_u64_equals(bsts_float64_to_bits(minus_one_float), UINT64_C(0xffffffffffffffff), "int_bits uses low 64 two's complement");

  {
    BValue inf_str = bsts_string_from_utf8_bytes_static(3, "\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(inf_str);
    assert_option_float_bits(parsed, UINT64_C(0x7ff0000000000000), "parse +infinity");
  }
  {
    BValue ninf_str = bsts_string_from_utf8_bytes_static(4, "-\xE2\x88\x9E");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(ninf_str);
    assert_option_float_bits(parsed, UINT64_C(0xfff0000000000000), "parse -infinity");
  }
  {
    uint64_t nan_bits = UINT64_C(0x7ff80000000000ab);
    BValue nanv = bsts_float64_from_bits(nan_bits);
    BValue nan_str = ___bsts_g_Bosatsu_l_Num_l_Float64_l_float64__to__String(nanv);
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(nan_str);
    assert_option_float_bits(parsed, nan_bits, "parse NaN payload");
  }
  {
    BValue nan_lit = bsts_string_from_utf8_bytes_static(4, ".NaN");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(nan_lit);
    assert(get_variant(parsed) == 1, "parse .NaN returns Some");
    assert(isnan(bsts_float64_to_double(get_enum_index(parsed, 0))), "parse .NaN produces NaN");
  }
  {
    BValue bad = bsts_string_from_utf8_bytes_static(6, "nope42");
    BValue parsed = ___bsts_g_Bosatsu_l_Num_l_Float64_l_string__to__Float64(bad);
    assert(get_variant(parsed) == 0, "invalid float string returns None");
  }
}

void test_int64() {
  uint64_t bits_cases[] = {
    UINT64_C(0x0000000000000000),
    UINT64_C(0x0000000000000001),
    UINT64_C(0xffffffffffffffff),
    UINT64_C(0x7fffffffffffffff),
    UINT64_C(0x8000000000000000),
    UINT64_C(0x1234567890abcdef)
  };

  for (size_t i = 0; i < sizeof(bits_cases) / sizeof(bits_cases[0]); i++) {
    BValue v = bsts_int64_from_bits(bits_cases[i]);
    assert_u64_equals(bsts_int64_to_bits(v), bits_cases[i], "int64 bits roundtrip");
  }

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_min__i64(),
      UINT64_C(0x8000000000000000),
      "min_i64 bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_max__i64(),
      UINT64_C(0x7fffffffffffffff),
      "max_i64 bits");

  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(bsts_integer_from_int64(INT64_MIN)),
      UINT64_C(0x8000000000000000),
      "safe int64 min conversion");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(bsts_integer_from_int64(INT64_MAX)),
      UINT64_C(0x7fffffffffffffff),
      "safe int64 max conversion");
  {
    BValue too_big = bsts_integer_add(
        bsts_integer_from_int64(INT64_MAX),
        bsts_integer_from_int(1));
    BValue too_small = bsts_integer_add(
        bsts_integer_from_int64(INT64_MIN),
        bsts_integer_from_int(-1));
    assert_option_none(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(too_big),
        "safe int64 rejects max + 1");
    assert_option_none(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(too_small),
        "safe int64 rejects min - 1");
  }

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__low__bits__to__Int64(bsts_integer_from_int(-1)),
      UINT64_C(0xffffffffffffffff),
      "low bits conversion keeps two's complement");
  assert_int_string(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Int(bsts_int64_from_bits(UINT64_C(0xffffffffffffffff))),
      "-1",
      "int64_to_Int decodes signed payload");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_add__Int64(
          bsts_int64_from_int64(INT64_MAX),
          bsts_int64_from_int64(1)),
      UINT64_C(0x8000000000000000),
      "add wraps to min_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_sub__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(1)),
      UINT64_C(0x7fffffffffffffff),
      "sub wraps to max_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mul__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(-1)),
      UINT64_C(0x8000000000000000),
      "mul wraps min_i64 * -1");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(-5),
          bsts_int64_from_int64(3)),
      UINT64_C(0xfffffffffffffffe),
      "division uses floor semantics");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(INT64_MIN),
          bsts_int64_from_int64(-1)),
      UINT64_C(0x8000000000000000),
      "division overflow wraps to min_i64");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(
          bsts_int64_from_int64(1234),
          bsts_int64_from_int64(0)),
      UINT64_C(0x0000000000000000),
      "division by zero returns zero");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mod__Int64(
          bsts_int64_from_int64(5),
          bsts_int64_from_int64(-3)),
      UINT64_C(0xffffffffffffffff),
      "mod_Int64 keeps the divisor sign");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_mod__Int64(
          bsts_int64_from_int64(1234),
          bsts_int64_from_int64(0)),
      UINT64_C(0x00000000000004d2),
      "mod_Int64 by zero returns the left value");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_and__Int64(
          bsts_int64_from_bits(UINT64_C(0xffff0000ffff0000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0x0f0f00000f0f0000),
      "bitwise and keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_or__Int64(
          bsts_int64_from_bits(UINT64_C(0xf0f00000f0f00000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0xffff0f0fffff0f0f),
      "bitwise or keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_xor__Int64(
          bsts_int64_from_bits(UINT64_C(0xffff0000ffff0000)),
          bsts_int64_from_bits(UINT64_C(0x0f0f0f0f0f0f0f0f))),
      UINT64_C(0xf0f00f0ff0f00f0f),
      "bitwise xor keeps low bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_not__Int64(bsts_int64_from_int64(0)),
      UINT64_C(0xffffffffffffffff),
      "bitwise not flips all bits");

  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(63)),
      UINT64_C(0x8000000000000000),
      "left shift keeps low 64 bits");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(64)),
      UINT64_C(0x0000000000000000),
      "left shift by >= 64 clears the value");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-1)),
      UINT64_C(0x0000000000000000),
      "negative left shift becomes arithmetic right shift");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
          bsts_int64_from_int64(-1),
          bsts_integer_from_int(100)),
      UINT64_C(0xffffffffffffffff),
      "right shift keeps sign for large positive counts");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-63)),
      UINT64_C(0x8000000000000000),
      "negative right shift becomes wrapped left shift");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
          bsts_int64_from_bits(UINT64_C(0xffffffffffffffff)),
          bsts_integer_from_int(1)),
      UINT64_C(0x7fffffffffffffff),
      "unsigned right shift clears the sign bit");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
          bsts_int64_from_int64(1),
          bsts_integer_from_int(-63)),
      UINT64_C(0x8000000000000000),
      "negative unsigned right shift becomes wrapped left shift");
  {
    BValue huge_shift =
        bsts_integer_shift_left(bsts_integer_from_int(1), bsts_integer_from_int(70));
    BValue huge_neg_shift = bsts_integer_negate(huge_shift);
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
            bsts_int64_from_int64(1),
            huge_shift),
        UINT64_C(0x0000000000000000),
        "boxed huge left shift clears the value");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(
            bsts_int64_from_int64(-1),
            huge_neg_shift),
        UINT64_C(0xffffffffffffffff),
        "boxed huge negative left shift sign-fills");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(
            bsts_int64_from_int64(-1),
            huge_shift),
        UINT64_C(0xffffffffffffffff),
        "boxed huge right shift keeps the sign bit");
    assert_int64_bits(
        ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(
            bsts_int64_from_bits(UINT64_C(0xffffffffffffffff)),
            huge_shift),
        UINT64_C(0x0000000000000000),
        "boxed huge unsigned right shift clears the value");
  }
  assert_int_string(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_popcount__Int64(
          bsts_int64_from_bits(UINT64_C(0xffffffffffffffff))),
      "64",
      "popcount_Int64 counts raw bits");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_eq__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(7))) == 1,
      "eq_Int64 true");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_eq__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(8))) == 0,
      "eq_Int64 false");

  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_cmp__Int64(
              bsts_int64_from_int64(-1),
              bsts_int64_from_int64(1))) == 0,
      "cmp_Int64 sorts signed values");
  assert(
      get_variant(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_cmp__Int64(
              bsts_int64_from_int64(7),
              bsts_int64_from_int64(7))) == 1,
      "cmp_Int64 eq");

  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(1.5)),
      UINT64_C(0x0000000000000002),
      "float64_to_Int64 rounds ties to even");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(0.5))),
      UINT64_C(0x0000000000000000),
      "round_ties_even rounds +0.5 to +0.0");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(-0.5))),
      UINT64_C(0x8000000000000000),
      "round_ties_even rounds -0.5 to -0.0");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(2.5))),
      UINT64_C(0x4000000000000000),
      "round_ties_even keeps even integer ties");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(-1.5))),
      UINT64_C(0xc000000000000000),
      "round_ties_even handles negative half steps");
  assert_u64_equals(
      bsts_float64_to_bits(bsts_float64_from_double(bsts_round_ties_even(4503599627370496.0))),
      UINT64_C(0x4330000000000000),
      "round_ties_even leaves large integral values unchanged");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(2.5)),
      UINT64_C(0x0000000000000002),
      "float64_to_Int64 keeps even ties");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(-0.5)),
      UINT64_C(0x0000000000000000),
      "float64_to_Int64 handles negative ties");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(NAN)),
      "float64_to_Int64 rejects NaN");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(INFINITY)),
      "float64_to_Int64 rejects infinity");
  assert_option_none(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(ldexp(1.0, 63))),
      "float64_to_Int64 rejects 2^63");
  assert_option_int64_bits(
      ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(
          bsts_float64_from_double(-ldexp(1.0, 63))),
      UINT64_C(0x8000000000000000),
      "float64_to_Int64 accepts -2^63");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Float64(
              bsts_int64_from_int64(INT64_MIN))) == -ldexp(1.0, 63),
      "int64_to_Float64 matches Int conversion semantics");
}

void test_array_int64() {
  BValue tabulate_fn = alloc_boxed_pure_fn1(array_identity_i64_fn);
  BValue default_fn = alloc_boxed_pure_fn1(array_echo_i64_fn);
  BValue fold_index_fn = alloc_boxed_pure_fn3(array_fold_index_sum_fn);
  BValue map_index_fn = alloc_boxed_pure_fn2(array_map_index_sum_fn);
  BValue zip_add_fn = alloc_boxed_pure_fn2(array_zip_add_fn);
  BValue zip_accum_add_fn = alloc_boxed_pure_fn3(array_zip_accum_add_fn);
  BValue float_mul_fn = alloc_boxed_pure_fn2(array_float_mul_fn);

  BValue tabulated =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64(3),
          tabulate_fn);
  uint64_t tabulated_bits[] = { 0, 1, 2 };
  assert_int64_array_bits(
      tabulated,
      tabulated_bits,
      3,
      "tabulate_Array uses visible Int64 indices");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_size__Array(tabulated),
      3,
      "size_Array returns Int64");

  BValue tabulated_empty =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64(-1),
          tabulate_fn);
  assert(test_array_unbox(tabulated_empty)->len == 0, "tabulate_Array rejects negative sizes");

  BValue tabulated_oversized =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_tabulate__Array(
          bsts_int64_from_int64((int64_t)INT_MAX + 1),
          tabulate_fn);
  assert(test_array_unbox(tabulated_oversized)->len == 0, "tabulate_Array rejects oversized sizes");

  const BValue ints[] = {
    bsts_integer_from_int(0),
    bsts_integer_from_int(1),
    bsts_integer_from_int(2),
    bsts_integer_from_int(3),
    bsts_integer_from_int(4),
  };
  BValue base_ints = test_array_from_values(5, ints);
  BValue sliced =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_slice__Array(
          base_ints,
          bsts_int64_from_int64(2),
          bsts_int64_from_int64(5));

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__or__Array(
          sliced,
          bsts_int64_from_int64(0),
          default_fn),
      "2",
      "get_or_Array uses slice-relative visible indices");
  assert_int64_bits(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_get__or__Array(
          sliced,
          bsts_int64_from_int64(9),
          default_fn),
      9,
      "get_or_Array forwards the original miss index");

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_foldl__with__index__Array(
          sliced,
          bsts_integer_from_int(0),
          fold_index_fn),
      "12",
      "foldl_with_index_Array uses visible slice indices");

  BValue mapped =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_map__with__index__Array(
          sliced,
          map_index_fn);
  const int mapped_expected[] = { 2, 4, 6 };
  assert_int_array_equals(mapped, mapped_expected, 3, "map_with_index_Array uses visible indices");

  const BValue right_ints[] = {
    bsts_integer_from_int(10),
    bsts_integer_from_int(11),
    bsts_integer_from_int(12),
  };
  BValue right_prefix = test_array_from_values(3, right_ints);
  BValue zipped =
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__map__Array(
          base_ints,
          right_prefix,
          zip_add_fn);
  const int zipped_expected[] = { 10, 12, 14 };
  assert_int_array_equals(zipped, zipped_expected, 3, "zip_map_Array truncates to the shorter input");

  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__foldl__Array(
          base_ints,
          right_prefix,
          bsts_integer_from_int(0),
          zip_accum_add_fn),
      "36",
      "zip_foldl_Array truncates to the shorter input");
  assert_int_string(
      ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__foldl__Array(
          test_array_from_values(0, NULL),
          base_ints,
          bsts_integer_from_int(99),
          zip_accum_add_fn),
      "99",
      "zip_foldl_Array keeps the initial accumulator on empty prefixes");

  const BValue neg_zero_items[] = { bsts_float64_from_double(-0.0) };
  BValue neg_zero_array = test_array_from_values(1, neg_zero_items);
  assert_u64_equals(
      bsts_float64_to_bits(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumf__Array(neg_zero_array)),
      UINT64_C(0x8000000000000000),
      "sumf_Array preserves negative zero");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumf__Array(
              test_array_from_values(0, NULL))) == 0.0,
      "sumf_Array returns 0.0 on empty arrays");
  assert(
      bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_sumsqf__Array(
              test_array_from_values(0, NULL))) == 0.0,
      "sumsqf_Array returns 0.0 on empty arrays");

  const BValue left_float_items[] = { bsts_float64_from_double(INFINITY) };
  const BValue right_float_items[] = {
    bsts_float64_from_double(1.0),
    bsts_float64_from_double(NAN),
  };
  BValue left_float_array = test_array_from_values(1, left_float_items);
  BValue right_float_array = test_array_from_values(2, right_float_items);

  assert(
      isinf(bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_dotf__Array(
              left_float_array,
              right_float_array))),
      "dotf_Array truncates before reading past the shorter input");
  assert(
      isinf(bsts_float64_to_double(
          ___bsts_g_Bosatsu_l_Collection_l_Array_l_zip__sumf__Array(
              left_float_array,
              right_float_array,
              float_mul_fn))),
      "zip_sumf_Array truncates before reading past the shorter input");
}

void test_prog_assoc() {
#if !defined(_WIN32) && defined(BSTS_RUNTIME_DEBUG_CHECKS)
  assert_child_aborts(call_alloc_closure_zero, "zero-capture closures must use alloc_boxed_pure_fn");
#endif

  BValue pure_fn = alloc_boxed_pure_fn1(prog_assoc_pure_fn);
  BValue raise_fn = alloc_boxed_pure_fn1(prog_assoc_raise_fn);

  BValue flat_base = ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(1));
  BValue flat_assoc = ___bsts_g_Bosatsu_l_Prog_l_flat__map(
      ___bsts_g_Bosatsu_l_Prog_l_flat__map(flat_base, pure_fn),
      pure_fn);
  assert(get_variant(flat_assoc) == 2, "flat_map assoc keeps flat_map tag");
  assert(get_enum_index(flat_assoc, 0) == flat_base, "flat_map assoc keeps left-most program");

  BValue flat_arg = bsts_integer_from_int(9);
  BValue flat_composed = call_fn1(get_enum_index(flat_assoc, 1), flat_arg);
  assert(get_variant(flat_composed) == 2, "flat_map assoc composes continuation into flat_map");
  assert(get_enum_index(flat_composed, 1) == pure_fn, "flat_map assoc keeps outer continuation");
  BValue flat_left = get_enum_index(flat_composed, 0);
  assert(get_variant(flat_left) == 0, "flat_map composed left branch is pure");
  assert(get_enum_index(flat_left, 0) == flat_arg, "flat_map composed pure keeps argument");

  BValue recover_base = ___bsts_g_Bosatsu_l_Prog_l_raise__error(
      bsts_string_from_utf8_bytes_static(4, "boom"));
  BValue recover_assoc = ___bsts_g_Bosatsu_l_Prog_l_recover(
      ___bsts_g_Bosatsu_l_Prog_l_recover(recover_base, raise_fn),
      raise_fn);
  assert(get_variant(recover_assoc) == 3, "recover assoc keeps recover tag");
  assert(get_enum_index(recover_assoc, 0) == recover_base, "recover assoc keeps left-most program");

  BValue recover_arg = bsts_string_from_utf8_bytes_static(1, "e");
  BValue recover_composed = call_fn1(get_enum_index(recover_assoc, 1), recover_arg);
  assert(get_variant(recover_composed) == 3, "recover assoc composes handler into recover");
  assert(get_enum_index(recover_composed, 1) == raise_fn, "recover assoc keeps outer handler");
  BValue recover_left = get_enum_index(recover_composed, 0);
  assert(get_variant(recover_left) == 1, "recover composed left branch is raise");
  assert(get_enum_index(recover_left, 0) == recover_arg, "recover composed raise keeps error");
}

int main(int argc, char** argv) {

  GC_init();
  test_runtime_enum_struct();
  test_runtime_strings();
  test_integer();
  test_float64();
  test_int64();
  test_array_int64();
  test_prog_assoc();
  printf("success\n");
  return 0;
}
