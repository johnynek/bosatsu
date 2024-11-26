#include "bosatsu_runtime.h"
#include <stdio.h>

BValue ___bsts_g_Bosatsu_l_Predef_l_add(BValue a, BValue b) {
  BValue result = bsts_integer_add(a, b);
  release_value(a);
  release_value(b);
  return result;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_and__Int(BValue a, BValue b) {
  BValue result = bsts_integer_and(a, b);
  release_value(a);
  release_value(b);
  return result;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_char__to__String(BValue a);

BValue ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(BValue a, BValue b) {
  int result = bsts_integer_cmp(a, b);
  release_value(a);
  release_value(b);
  // -1, 0, 1, but we map to 0, 1, 2 which are the adt tags for LT, EQ, GT
  return alloc_enum0(result + 1);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_concat__String(BValue a);

BValue ___bsts_g_Bosatsu_l_Predef_l_div(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_eq__Int(BValue a, BValue b) {
  BValue res = bsts_integer_equals(a, b) ? alloc_enum0(1) : alloc_enum0(0);
  release_value(a);
  release_value(b);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_gcd__Int(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_int__loop(BValue a, BValue b, BValue c);

BValue ___bsts_g_Bosatsu_l_Predef_l_int__to__String(BValue a);

BValue ___bsts_g_Bosatsu_l_Predef_l_mod__Int(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_not__Int(BValue a);

BValue ___bsts_g_Bosatsu_l_Predef_l_or__Int(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_partition__String(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_rpartition__String(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__left__Int(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__right__Int(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_string__Order__fn(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_sub(BValue a, BValue b) {
  return ___bsts_g_Bosatsu_l_Predef_l_add(a, bsts_integer_negate(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_times(BValue a, BValue b);

BValue ___bsts_g_Bosatsu_l_Predef_l_trace(BValue a, BValue b) {
  char* bytes = bsts_string_utf8_bytes(a);
  size_t len = bsts_string_utf8_len(a);
  // TODO: if this string is somehow too big for an int this may fail
  printf("%.*s\n", (int)len, bytes);
  release_value(a);

  return b;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_xor__Int(BValue a, BValue b);