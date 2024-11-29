#include "bosatsu_runtime.h"
#include <stdlib.h>
#include <stdio.h>

void assert(_Bool cond, char* message) {
  if (!cond) {
    printf("%s\n", message);
    exit(1);
  }
}

void test_runtime_enum_struct() {
  BValue s1 = alloc_struct2(alloc_enum0(0), alloc_enum0(1));
  assert(get_variant(get_struct_index(s1, 0)) == 0, "index0 == alloc_enum0");
  assert(get_variant(get_struct_index(s1, 1)) == 1, "index0 == alloc_enum0(1)");
  release_value(s1);
}

void test_integer() {
  {
    BValue s1 = bsts_integer_to_string(bsts_integer_from_int(0));
    BValue expects1 = bsts_string_from_utf8_bytes_static(1, "0");
    if (bsts_string_cmp(s1, expects1) != 0) {
      printf("0 didn't match, got: ");
      bsts_string_println(s1);
      exit(1);
    }
    release_value(s1);
    release_value(expects1);
  }

  {
    BValue s1 = bsts_integer_to_string(bsts_integer_from_int(-1));
    BValue expects1 = bsts_string_from_utf8_bytes_static(2, "-1");
    if (bsts_string_cmp(s1, expects1) != 0) {
      printf("-1 didn't match, got: ");
      bsts_string_println(s1);
      exit(1);
    }
    release_value(s1);
    release_value(expects1);
  }

  {

    BValue s1 = bsts_integer_to_string(bsts_integer_from_int(123456));
    BValue expects1 = bsts_string_from_utf8_bytes_static(6, "123456");
    if (bsts_string_cmp(s1, expects1) != 0) {
      printf("123456 didn't match, got: ");
      bsts_string_println(s1);
      exit(1);
    }
    release_value(s1);
    release_value(expects1);
  }

  {
    BValue s0 = bsts_integer_from_int(0);
    BValue s1 = bsts_integer_from_int(1);
    BValue s2 = bsts_integer_from_int(2);
    assert(bsts_integer_cmp(s1, s2) < 0, "1 < 2");
    assert(bsts_integer_cmp(s0, s2) < 0, "0 < 2");
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

  release_value(v1tail);
  release_value(v2tail);
  release_value(tail_expected);
  release_value(v3);

  {
    BValue hello_world1 = bsts_string_from_utf8_bytes_static(11, "hello world");
    BValue hello1 = bsts_string_from_utf8_bytes_static(5, "world");
    int find1 = bsts_string_find(hello_world1, hello1, 0);
    assert(find1 == 6, "find1");
    int find2 = bsts_string_find(hello_world1, hello1, 1);
    assert(find2 == 6, "find2");
    int find3 = bsts_string_find(hello_world1, hello1, 7);
    assert(find3 == -1, "find3");
    release_value(hello_world1);
    release_value(hello1);
  }

}

int main(int argc, char** argv) {

  test_runtime_enum_struct();
  test_runtime_strings();
  test_integer();
  printf("success\n");
  return 0;
}