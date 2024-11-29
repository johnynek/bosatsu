#include "bosatsu_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

BValue ___bsts_g_Bosatsu_l_Predef_l_char__to__String(BValue a) {
  int codepoint = bsts_char_code_point_from_value(a);
  char bytes[4];
  int len = bsts_string_code_point_to_utf8(codepoint, bytes);
  return bsts_string_from_utf8_bytes_owned(len, bytes);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(BValue a, BValue b) {
  int result = bsts_integer_cmp(a, b);
  release_value(a);
  release_value(b);
  // -1, 0, 1, but we map to 0, 1, 2 which are the adt tags for LT, EQ, GT
  return alloc_enum0(result + 1);
}

// a is a List[String]
BValue ___bsts_g_Bosatsu_l_Predef_l_concat__String(BValue a) {
  BValue amut = a;
  BValue res;
  ENUM_TAG v = get_variant(amut);
  if (v == 0) {
    // this is the empty list
    res = bsts_string_from_utf8_bytes_static(0, NULL);
    goto done;
  }
  // otherwise we have at least one
  size_t total_len = 0;
  size_t count = 0;
  while (v != 0) {
    BValue str = get_enum_index(amut, 0);
    amut = get_enum_index(amut, 1);
    v = get_variant(amut);
    total_len += bsts_string_utf8_len(str);
    count++;
  }
  // now we know the total length and count
  if (count == 1) {
    // this is List(s), just increment the ref count of s and return
    res = clone_value(get_enum_index(a, 0));
  }
  else {
    // we allocate some bytes and copy
    char* bytes = malloc(sizeof(char) * total_len);
    char* current_pos = bytes;
    // reset to go through the list
    amut = a;
    v = get_variant(amut);
    while (v != 0) {
      BValue str = get_enum_index(amut, 0);
      size_t str_len = bsts_string_utf8_len(str);
      char* str_bytes = bsts_string_utf8_bytes(str);
      memcpy(current_pos, str_bytes, str_len);
      current_pos += str_len;
      amut = get_enum_index(amut, 1);
      v = get_variant(amut);
    }
    res = bsts_string_from_utf8_bytes_owned(total_len, bytes);
  }

  done:
  release_value(a);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_div(BValue a, BValue b) {
  // TODO this is non-trivial, just return something wrong
  release_value(b);
  return a;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_eq__Int(BValue a, BValue b) {
  BValue res = bsts_integer_equals(a, b) ? alloc_enum0(1) : alloc_enum0(0);
  release_value(a);
  release_value(b);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_gcd__Int(BValue a, BValue b) {
  // TODO this is non-trivial, just return something wrong
  release_value(b);
  return a;
}

/*
this loops until the returned Int is <= 0 or the returned Int is >= intValue
external def int_loop(intValue: Int, state: a, fn: (Int, a) -> (Int, a)) -> a
*/
BValue ___bsts_g_Bosatsu_l_Predef_l_int__loop(BValue i, BValue a, BValue fn) {
  // def int_loop(i, a, fn):
  //   cont = (0 < i)
  //   res = a
  //   _i = i
  //   _a = a
  //   while cont:
  //     res = fn(_i, _a)
  //     tmp_i = res[0]
  //     _a = res[1][0]
  //     cont = (0 < tmp_i) and (tmp_i < _i)
  //     _i = tmp_i
  //   return _a
  BValue zero = bsts_integer_from_int(0);
  int cont = bsts_integer_cmp(zero, i) < 0;
  BValue _i = i;
  BValue _a = a;
  while (cont) {
    // we have to keep a ref to _i to compare below
    BValue i_clone = clone_value(_i);
    // _i and _a are consumed here, so
    BValue res = call_fn2(fn, _i, _a);
    BValue tmp_i = clone_value(get_struct_index(res, 0));
    _a = clone_value(get_struct_index(res, 1));
    release_value(res);
    // we have to be strictly decreasing _i but > 0
    cont = (bsts_integer_cmp(zero, tmp_i) < 0) && (bsts_integer_cmp(tmp_i, i_clone) < 0);
    release_value(i_clone);
    _i = tmp_i;
  }
  // all the rest of the values are references
  release_value(fn);
  release_value(_i);
  return _a;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_int__to__String(BValue a) {
  BValue str = bsts_integer_to_string(a);
  release_value(a);
  return str;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_mod__Int(BValue a, BValue b) {
  // TODO this is non-trivial, just return something wrong
  release_value(b);
  return a;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_not__Int(BValue a) {
  // ~x = (-1 - x)
  return bsts_integer_negate(bsts_integer_add(a, bsts_integer_from_int(1)));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_or__Int(BValue a, BValue b) {
  BValue result = bsts_integer_or(a, b);
  release_value(a);
  release_value(b);
  return result;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_partition__String(BValue a, BValue b) {
  size_t blen = bsts_string_utf8_len(b);
  BValue res;
  if (blen == 0) {
    // the result has to give proper substrings, so here we return None
    res = alloc_enum0(0);
    goto done;
  }
  int offset = bsts_string_find(a, b, 0);
  if (offset < 0) {
    // return None
    res = alloc_enum0(0);
    goto done;
  }
  // we return substrings
  // Some((x, y)) with x = a[0:offset], y = a[offset + b.len():]
  BValue x = bsts_string_substring(a, 0, offset);
  BValue y = bsts_string_substring_tail(a, offset + blen);
  res = alloc_enum2(1, x, y);

  done:
  release_value(a);
  release_value(b);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_rpartition__String(BValue a, BValue b) {
  size_t blen = bsts_string_utf8_len(b);
  BValue res;
  if (blen == 0) {
    // the result has to give proper substrings, so here we return None
    res = alloc_enum0(0);
    goto done;
  }
  size_t alen = bsts_string_utf8_len(a);
  int offset = bsts_string_rfind(a, b, alen - 1);
  if (offset < 0) {
    // return None
    res = alloc_enum0(0);
    goto done;
  }
  // we return substrings
  // Some((x, y)) with x = a[0:offset], y = a[offset + b.len():]
  BValue x = bsts_string_substring(a, 0, offset);
  BValue y = bsts_string_substring_tail(a, offset + blen);
  res = alloc_enum2(1, x, y);

  done:
  release_value(a);
  release_value(b);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__left__Int(BValue a, BValue b) {
  BValue res = bsts_integer_shift_left(a, b);
  release_value(a);
  release_value(b);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__right__Int(BValue a, BValue b) {
  BValue negb = bsts_integer_negate(b);
  BValue res = bsts_integer_shift_left(a, negb);
  release_value(a);
  release_value(negb);
  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_string__Order__fn(BValue a, BValue b) {
  int result = bsts_string_cmp(a, b);
  release_value(a);
  release_value(b);
  // -1, 0, 1, but we map to 0, 1, 2 which are the adt tags for LT, EQ, GT
  return alloc_enum0(result + 1);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_sub(BValue a, BValue b) {
  return ___bsts_g_Bosatsu_l_Predef_l_add(a, bsts_integer_negate(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_times(BValue a, BValue b) {
  BValue result = bsts_integer_times(a, b);
  release_value(a);
  release_value(b);
  return result;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_trace(BValue a, BValue b) {
  bsts_string_println(a);
  release_value(a);
  return b;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_xor__Int(BValue a, BValue b) {
  BValue result = bsts_integer_xor(a, b);
  release_value(a);
  release_value(b);
  return result;
}