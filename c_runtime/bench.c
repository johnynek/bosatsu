#include "bosatsu_runtime.h"
#include "bosatsu_ext_Bosatsu_l_Predef.h"
#include "gc.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static volatile BValue sink;

static uint64_t now_ns() {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ((uint64_t)ts.tv_sec * 1000000000ull) + (uint64_t)ts.tv_nsec;
}

static void print_result(const char* name, uint64_t ns, uint64_t ops) {
  double ns_per_op = ops ? (double)ns / (double)ops : 0.0;
  printf("%-24s  total_ns=%llu  ops=%llu  ns/op=%.2f\n",
         name,
         (unsigned long long)ns,
         (unsigned long long)ops,
         ns_per_op);
}

static void bench_add_big_small(const char* name, size_t iters, BValue big, BValue small_pos, BValue small_neg) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = big;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_add(acc, small_pos);
    acc = bsts_integer_add(acc, small_neg);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters * 2ull);
}

static void bench_add_small_small(const char* name, size_t iters, BValue left, BValue right) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = left;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_add(left, right);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_add(const char* name, size_t iters, BValue left, BValue right) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = left;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_add(left, right);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_sub(const char* name, size_t iters, BValue left, BValue right) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = left;
  for (size_t i = 0; i < iters; i++) {
    acc = ___bsts_g_Bosatsu_l_Predef_l_sub(left, right);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_mul(const char* name, size_t iters, BValue left, BValue right) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = left;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_times(left, right);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_mul_big_small(const char* name, size_t iters, BValue big, BValue small) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = big;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_times(big, small);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_not(const char* name, size_t iters, BValue value) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = value;
  for (size_t i = 0; i < iters; i++) {
    acc = ___bsts_g_Bosatsu_l_Predef_l_not__Int(value);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_cmp_small_small(const char* name, size_t iters, BValue left, BValue right) {
  int acc = 0;
  GC_gcollect();
  uint64_t start = now_ns();
  for (size_t i = 0; i < iters; i++) {
    acc += bsts_integer_cmp(left, right);
  }
  sink = bsts_integer_from_int(acc);
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_bitwise(const char* name, size_t iters, BValue left, BValue right, int op) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = left;
  for (size_t i = 0; i < iters; i++) {
    switch (op) {
      case '&':
        acc = bsts_integer_and(left, right);
        break;
      case '|':
        acc = bsts_integer_or(left, right);
        break;
      case '^':
        acc = bsts_integer_xor(left, right);
        break;
      default:
        break;
    }
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_shift(const char* name, size_t iters, BValue value, BValue shift) {
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = value;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_integer_shift_left(value, shift);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_string_static_ctor(const char* name, size_t iters, const char* text) {
  size_t len = strlen(text);
  GC_gcollect();
  uint64_t start = now_ns();
  BValue acc = 0;
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_string_from_utf8_bytes_static(len, text);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_string_equals(const char* name, size_t iters, BValue left, BValue right) {
  int acc = 0;
  GC_gcollect();
  uint64_t start = now_ns();
  for (size_t i = 0; i < iters; i++) {
    acc += bsts_string_equals(left, right);
  }
  sink = bsts_integer_from_int(acc);
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_string_cmp(const char* name, size_t iters, BValue left, BValue right) {
  int acc = 0;
  GC_gcollect();
  uint64_t start = now_ns();
  for (size_t i = 0; i < iters; i++) {
    acc += bsts_string_cmp(left, right);
  }
  sink = bsts_integer_from_int(acc);
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_string_find(const char* name, size_t iters, BValue haystack, BValue needle, int start_idx) {
  int acc = 0;
  GC_gcollect();
  uint64_t start = now_ns();
  for (size_t i = 0; i < iters; i++) {
    acc += bsts_string_find(haystack, needle, start_idx);
  }
  sink = bsts_integer_from_int(acc);
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

static void bench_string_char_at(const char* name, size_t iters, BValue value, int offset) {
  BValue acc = 0;
  GC_gcollect();
  uint64_t start = now_ns();
  for (size_t i = 0; i < iters; i++) {
    acc = bsts_string_char_at(value, offset);
  }
  sink = acc;
  uint64_t end = now_ns();
  print_result(name, end - start, (uint64_t)iters);
}

int main(int argc, char** argv) {
  GC_init();

  size_t iters = 200000;
  if (argc > 1) {
    iters = (size_t)strtoull(argv[1], NULL, 10);
    if (iters == 0) iters = 200000;
  }

  uint32_t big_words[4] = { 0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678 };
  uint32_t big_words_alt[4] = { 0x76543210, 0x0fedcba9, 0x89abcdef, 0x13579bdf };
  uint32_t big2_words[2] = { 0x12345678, 0x40000001 };
  BValue big_pos = bsts_integer_from_words_copy(1, 4, big_words);
  BValue big_pos_alt = bsts_integer_from_words_copy(1, 4, big_words_alt);
  BValue big_neg = bsts_integer_negate(big_pos);
  BValue big2_pos = bsts_integer_from_words_copy(1, 2, big2_words);
  BValue big2_neg = bsts_integer_negate(big2_pos);

  BValue small_pos = bsts_integer_from_int(1);
  BValue small_neg = bsts_integer_from_int(-1);
  BValue small_pow2 = bsts_integer_from_int(32);
  BValue small_pow2_neg = bsts_integer_from_int(-32);
  BValue small_10 = bsts_integer_from_int(10);
  BValue small_mask = bsts_integer_from_int(0x12345678);
  BValue small62_pos = bsts_integer_from_int64((INT64_C(1) << 40) + 12345);
  BValue small62_neg = bsts_integer_from_int64(-((INT64_C(1) << 40) + 12345));
  BValue small62_mask = bsts_integer_from_int64((INT64_C(1) << 40) + 0x12345678);
  BValue small62_other = bsts_integer_from_int64((INT64_C(1) << 39) + 0x55555555);
  BValue small62_other_neg = bsts_integer_from_int64(-((INT64_C(1) << 39) + 0x13579bdf));
  BValue small63_pos = bsts_integer_from_int64((INT64_C(1) << 61) + 12345);
  BValue small63_neg = bsts_integer_from_int64(-((INT64_C(1) << 61) + 12345));
  BValue small63_mask = bsts_integer_from_int64((INT64_C(1) << 61) + 0x12345678);
  BValue small63_other = bsts_integer_from_int64((INT64_C(1) << 60) + 0x55555555);
  BValue shift_left_small = bsts_integer_from_int(1);
  BValue shift_left = bsts_integer_from_int(5);
  BValue shift_left_word = bsts_integer_from_int(32);
  BValue shift_left_large = bsts_integer_from_int(40);
  BValue shift_right_small = bsts_integer_from_int(-1);
  BValue shift_right = bsts_integer_from_int(-5);
  BValue shift_right_word = bsts_integer_from_int(-32);

  BValue s_small_1 = bsts_string_from_utf8_bytes_static(5, "hello");
  BValue s_small_2 = bsts_string_from_utf8_bytes_static(5, "hello");
  BValue s_small_diff = bsts_string_from_utf8_bytes_static(5, "jello");
  BValue s_small_hay = bsts_string_from_utf8_bytes_static(5, "abcba");
  BValue s_small_need = bsts_string_from_utf8_bytes_static(2, "bc");
  BValue s_heap_1 = bsts_string_from_utf8_bytes_static(24, "abcdefghijklmnopqrstuvwx");
  BValue s_heap_2 = bsts_string_from_utf8_bytes_static(24, "abcdefghijklmnopqrstuvwx");

  printf("iters=%zu\n", iters);
  bench_add_small_small("add_small_small", iters, small_pos, small_neg);
  bench_add_small_small("add_small_small62", iters, small62_pos, small62_neg);
  bench_add_small_small("add_small_small63", iters, small63_pos, small63_neg);
  bench_sub("sub_small_small", iters, small_pos, small_neg);
  bench_sub("sub_small_small62", iters, small62_pos, small62_other);
  bench_sub("sub_small_small63", iters, small63_pos, small63_other);
  bench_sub("sub_big2_small63", iters, big2_pos, small63_pos);
  bench_sub("sub_small63_big2", iters, small63_pos, big2_pos);
  bench_sub("sub_big_small62", iters, big_pos, small62_pos);
  bench_sub("sub_small_big", iters, small62_pos, big_pos);
  bench_cmp_small_small("cmp_small_small", iters, small_pos, small_neg);
  bench_cmp_small_small("cmp_small_small62", iters, small62_pos, small62_other);
  bench_cmp_small_small("cmp_small_small62_neg", iters, small62_neg, small62_other_neg);
  bench_cmp_small_small("cmp_small_small63", iters, small63_pos, small63_other);
  bench_bitwise("and_small_small62", iters, small62_pos, small62_mask, '&');
  bench_bitwise("or_small_small62", iters, small62_pos, small62_other, '|');
  bench_bitwise("xor_small_small62", iters, small62_pos, small62_other, '^');
  bench_bitwise("and_small_small63", iters, small63_pos, small63_mask, '&');
  bench_not("not_small_small62", iters, small62_pos);
  bench_not("not_small_small63", iters, small63_pos);
  bench_not("not_big_pos", iters, big_pos);
  bench_not("not_big_neg", iters, big_neg);
  bench_add("add_big_big_pos", iters, big_pos, big_pos_alt);
  bench_add("add_big_small63_direct", iters, big_pos, small63_pos);
  bench_add("add_big2_small63_pos", iters, big2_pos, small63_pos);
  bench_add("add_big2_small63_neg", iters, big2_neg, small63_pos);
  bench_add_big_small("add_big_small_pos", iters, big_pos, small_pos, small_neg);
  bench_add_big_small("add_big_small_neg", iters, big_neg, small_pos, small_neg);
  bench_add_big_small("add_big_small62_pos", iters, big_pos, small62_pos, small62_neg);
  bench_add_big_small("add_big_small62_neg", iters, big_neg, small62_pos, small62_neg);
  bench_mul("mul_big_big_pos", iters, big_pos, big_pos_alt);
  bench_mul("mul_big_small63_direct", iters, big_pos, small63_pos);
  bench_mul("mul_small63_pow2", iters, small63_pos, small_pow2);
  bench_mul("mul_big_pow2_pos", iters, big_pos, small_pow2);
  bench_mul("mul_big_pow2_neg", iters, big_pos, small_pow2_neg);
  bench_mul("mul_big2_pow2_pos", iters, big2_pos, small_pow2);
  bench_mul("mul_big2_pow2_neg", iters, big2_neg, small_pow2);
  bench_mul("mul_big2_small10", iters, big2_pos, small_10);
  bench_mul("mul_big2_small63_pos", iters, big2_pos, small63_pos);
  bench_mul("mul_big2_small63_neg", iters, big2_neg, small63_pos);
  bench_mul_big_small("mul_big_small_pos", iters, big_pos, small_10);
  bench_mul_big_small("mul_big_small_neg", iters, big_neg, small_10);
  bench_mul_big_small("mul_big_small62_pos", iters, big_pos, small62_pos);
  bench_mul_big_small("mul_big_small62_neg", iters, big_neg, small62_pos);
  bench_bitwise("and_big_big_pos", iters, big_pos, big_pos_alt, '&');
  bench_bitwise("or_big_big_pos", iters, big_pos, big_pos_alt, '|');
  bench_bitwise("xor_big_big_pos", iters, big_pos, big_pos_alt, '^');
  bench_bitwise("and_neg_mixed", iters, big_neg, small_mask, '&');
  bench_bitwise("or_neg_mixed", iters, big_neg, small_mask, '|');
  bench_bitwise("xor_neg_mixed", iters, big_neg, small_mask, '^');
  bench_bitwise("and_neg_mixed62", iters, big_neg, small62_mask, '&');
  bench_bitwise("or_neg_mixed62", iters, big_neg, small62_mask, '|');
  bench_bitwise("xor_neg_mixed62", iters, big_neg, small62_mask, '^');
  bench_shift("shift_small_pos_left1", iters, small_pos, shift_left_small);
  bench_shift("shift_small_pos_right1", iters, small_pos, shift_right_small);
  bench_shift("shift_small63_left5", iters, small63_pos, shift_left);
  bench_shift("shift_small63_right5", iters, small63_pos, shift_right);
  bench_shift("shift_small63_left40", iters, small63_pos, shift_left_large);
  bench_shift("shift_big_pos_left", iters, big_pos, shift_left);
  bench_shift("shift_big_pos_right", iters, big_pos, shift_right);
  bench_shift("shift_big_pos_left32", iters, big_pos, shift_left_word);
  bench_shift("shift_big_pos_right32", iters, big_pos, shift_right_word);
  bench_shift("shift_neg_left", iters, big_neg, shift_left);
  bench_shift("shift_neg_right", iters, big_neg, shift_right);
  bench_string_static_ctor("str_ctor_static_heap", iters, "abcdefghijklmnopqrstuvwx");
  bench_string_static_ctor("str_ctor_static_medium", iters, "abcdefghijklmnop");
  bench_string_static_ctor("str_ctor_static_small", iters, "hello");
  bench_string_equals("str_equals_small_eq", iters, s_small_1, s_small_2);
  bench_string_equals("str_equals_small_neq", iters, s_small_1, s_small_diff);
  bench_string_equals("str_equals_heap_eq", iters, s_heap_1, s_heap_2);
  bench_string_cmp("str_cmp_small", iters, s_small_1, s_small_diff);
  bench_string_find("str_find_small", iters, s_small_hay, s_small_need, 0);
  bench_string_char_at("str_char_at_small", iters, s_small_hay, 4);

  return 0;
}
