#include "bosatsu_runtime.h"
#include <stdlib.h>
#include <stdio.h>

void assert(_Bool cond, char* message) {
  if (!cond) {
    printf("%s\n", message);
    exit(1);
  }
}

int main(int argc, char** argv) {
  BValue s1 = alloc_struct2(alloc_enum0(0), alloc_enum0(1));
  assert(get_variant(get_struct_index(s1, 0)) == 0, "index0 == alloc_enum0");
  assert(get_variant(get_struct_index(s1, 1)) == 1, "index0 == alloc_enum0(1)");
  release_value(s1);

  char* hello = "hello1";

  BValue v1 = bsts_string_from_utf8_bytes_copy(5, "hello");
  // we can ignore trailing byte string on hello, by taking the front
  BValue v2 = bsts_string_from_utf8_bytes_static(5, hello);
  assert(bsts_string_equals(v1, v2), "v1 == v2");
  assert(bsts_string_equals(v1, v1), "v1 == v1");
  assert(bsts_string_equals(v2, v2), "v2 == v2");
  //codepoint tests
  assert(bsts_string_code_point_bytes(v1, 0) == 1, "code_point_bytes(v[0]) == 1");
  assert(bsts_string_char_at(v1, 0) == BSTS_TO_CHAR(104), "char_at(v, 0) == 104");
  assert(bsts_string_char_at(v1, 1) == BSTS_TO_CHAR(101), "char_at(v, 1) == 101");
  BValue v3 = bsts_string_from_utf8_bytes_static(4, "\x00F0\x009F\x0098\x008A");
  assert(bsts_string_char_at(v3, 0) == BSTS_TO_CHAR(0x1F60A), "smiley check char");
  assert(bsts_string_code_point_bytes(v3, 0) == 4, "smiley length");

  BValue v1tail = bsts_string_substring_tail(v1, 1);
  BValue v2tail = bsts_string_substring_tail(v2, 1);
  BValue tail_expected = bsts_string_from_utf8_bytes_static(4, "ello");
  assert(bsts_string_equals(v1tail, v2tail), "v1tail == v2tail");
  assert(bsts_string_equals(v1tail, tail_expected), "v1tail == expected");

  release_value(v1tail);
  release_value(v2tail);
  release_value(tail_expected);
  release_value(v3);

  printf("success\n");
  return 0;
}