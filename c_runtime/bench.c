#include "bosatsu_runtime.h"
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
  BValue big_pos = bsts_integer_from_words_copy(1, 4, big_words);
  BValue big_neg = bsts_integer_negate(big_pos);

  BValue small_pos = bsts_integer_from_int(1);
  BValue small_neg = bsts_integer_from_int(-1);
  BValue small_10 = bsts_integer_from_int(10);
  BValue small_mask = bsts_integer_from_int(0x12345678);
  BValue shift_left = bsts_integer_from_int(5);
  BValue shift_right = bsts_integer_from_int(-5);

  BValue s_small_1 = bsts_string_from_utf8_bytes_static(5, "hello");
  BValue s_small_2 = bsts_string_from_utf8_bytes_static(5, "hello");
  BValue s_small_diff = bsts_string_from_utf8_bytes_static(5, "jello");
  BValue s_small_hay = bsts_string_from_utf8_bytes_static(5, "abcba");
  BValue s_small_need = bsts_string_from_utf8_bytes_static(2, "bc");
  BValue s_heap_1 = bsts_string_from_utf8_bytes_static(24, "abcdefghijklmnopqrstuvwx");
  BValue s_heap_2 = bsts_string_from_utf8_bytes_static(24, "abcdefghijklmnopqrstuvwx");

  printf("iters=%zu\n", iters);
  bench_add_big_small("add_big_small_pos", iters, big_pos, small_pos, small_neg);
  bench_add_big_small("add_big_small_neg", iters, big_neg, small_pos, small_neg);
  bench_mul_big_small("mul_big_small_pos", iters, big_pos, small_10);
  bench_mul_big_small("mul_big_small_neg", iters, big_neg, small_10);
  bench_bitwise("and_neg_mixed", iters, big_neg, small_mask, '&');
  bench_bitwise("or_neg_mixed", iters, big_neg, small_mask, '|');
  bench_bitwise("xor_neg_mixed", iters, big_neg, small_mask, '^');
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
