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
  release_value(v1);
  release_value(v2);

  printf("success\n");
  return 0;
}