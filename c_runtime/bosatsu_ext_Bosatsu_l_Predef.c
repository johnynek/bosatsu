#include "bosatsu_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

BValue ___bsts_g_Bosatsu_l_Predef_l_add(BValue a, BValue b) {
  return bsts_integer_add(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_addf(BValue a, BValue b) {
  return bsts_float64_from_double(bsts_float64_to_double(a) + bsts_float64_to_double(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_and__Int(BValue a, BValue b) {
  return bsts_integer_and(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_char__to__String(BValue a) {
  int codepoint = bsts_char_code_point_from_value(a);
  char bytes[4];
  int len = bsts_string_code_point_to_utf8(codepoint, bytes);
  return bsts_string_from_utf8_bytes_copy(len, bytes);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_char__to__Int(BValue a) {
  int codepoint = bsts_char_code_point_from_value(a);
  return bsts_integer_from_int(codepoint);
}

// a is a List[Char]
BValue ___bsts_g_Bosatsu_l_Predef_l_char__List__to__String(BValue a) {
  BValue amut = a;
  ENUM_TAG v = get_variant(amut);
  if (v == 0) {
    return bsts_string_from_utf8_bytes_static(0, NULL);
  }

  size_t total_len = 0;
  while (v != 0) {
    BValue ch = get_enum_index(amut, 0);
    int codepoint = bsts_char_code_point_from_value(ch);
    char bytes[4];
    int char_len = bsts_string_code_point_to_utf8(codepoint, bytes);
    if (char_len <= 0) {
      // invalid code points should be impossible for Char values
      return bsts_string_from_utf8_bytes_static(0, NULL);
    }
    total_len += (size_t)char_len;
    amut = get_enum_index(amut, 1);
    v = get_variant(amut);
  }

  BValue res = bsts_string_mut(total_len);
  char* out = bsts_string_utf8_bytes(res);
  amut = a;
  v = get_variant(amut);

  while (v != 0) {
    BValue ch = get_enum_index(amut, 0);
    int codepoint = bsts_char_code_point_from_value(ch);
    char bytes[4];
    int char_len = bsts_string_code_point_to_utf8(codepoint, bytes);
    memcpy(out, bytes, (size_t)char_len);
    out += char_len;
    amut = get_enum_index(amut, 1);
    v = get_variant(amut);
  }

  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(BValue a, BValue b) {
  int result = bsts_integer_cmp(a, b);
  // -1, 0, 1, but we map to 0, 1, 2 which are the adt tags for LT, EQ, GT
  return alloc_enum0(result + 1);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_cmp__Float64(BValue a, BValue b) {
  int result = bsts_float64_cmp_total(a, b);
  return alloc_enum0(result + 1);
}

// a is a List[String]
BValue ___bsts_g_Bosatsu_l_Predef_l_concat__String(BValue a) {
  BValue amut = a;
  BValue res;
  ENUM_TAG v = get_variant(amut);
  if (v == 0) {
    // this is the empty list
    return bsts_string_from_utf8_bytes_static(0, NULL);
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
    res = get_enum_index(a, 0);
  }
  else {
    // we allocate some bytes and copy
    res = bsts_string_mut(total_len);
    char* bytes = bsts_string_utf8_bytes(res);
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
  }

  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_div(BValue a, BValue b) {
  BValue divmod = bsts_integer_div_mod(a, b);
  return get_struct_index(divmod, 0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_divf(BValue a, BValue b) {
  return bsts_float64_from_double(bsts_float64_to_double(a) / bsts_float64_to_double(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_eq__Int(BValue a, BValue b) {
  return bsts_integer_equals(a, b) ? alloc_enum0(1) : alloc_enum0(0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_gcd__Int(BValue a, BValue b) {
  BValue zero = bsts_integer_from_int(0);
  while(!bsts_integer_equals(b, zero)) {
    BValue tb = b;

    BValue divmod = bsts_integer_div_mod(a, b);
    b = get_struct_index(divmod, 1);
    a = tb;
  }

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
    BValue i_clone = _i;
    // _i and _a are consumed here, so
    BValue res = call_fn2(fn, _i, _a);
    BValue tmp_i = get_struct_index(res, 0);
    _a = get_struct_index(res, 1);
    // we have to be strictly decreasing _i but > 0
    cont = (bsts_integer_cmp(zero, tmp_i) < 0) && (bsts_integer_cmp(tmp_i, i_clone) < 0);
    _i = tmp_i;
  }
  // all the rest of the values are references
  return _a;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_int__to__Char(BValue a) {
  BValue zero = bsts_integer_from_int(0);
  BValue max_cp = bsts_integer_from_int(0x10FFFF);
  if ((bsts_integer_cmp(a, zero) < 0) || (bsts_integer_cmp(a, max_cp) > 0)) {
    return alloc_enum0(0);
  }

  BValue surrogate_start = bsts_integer_from_int(0xD800);
  BValue surrogate_end = bsts_integer_from_int(0xDFFF);
  if ((bsts_integer_cmp(a, surrogate_start) >= 0) && (bsts_integer_cmp(a, surrogate_end) <= 0)) {
    return alloc_enum0(0);
  }

  int32_t codepoint = bsts_integer_to_int32(a);
  return alloc_enum1(1, bsts_char_from_code_point(codepoint));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_int__to__String(BValue a) {
  return bsts_integer_to_string(a);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_mod__Int(BValue a, BValue b) {
  BValue divmod = bsts_integer_div_mod(a, b);
  return get_struct_index(divmod, 1);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_not__Int(BValue a) {
  // ~x = (-1 - x)
  return bsts_integer_negate(bsts_integer_add(a, bsts_integer_from_int(1)));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_or__Int(BValue a, BValue b) {
  return bsts_integer_or(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_partition__String(BValue a, BValue b) {
  size_t blen = bsts_string_utf8_len(b);
  BValue res;
  if (blen == 0) {
    // the result has to give proper substrings, so here we return None
    return alloc_enum0(0);
  }
  int offset = bsts_string_find(a, b, 0);
  if (offset < 0) {
    // return None
    return alloc_enum0(0);
  }
  // we return substrings
  // Some((x, y)) with x = a[0:offset], y = a[offset + b.len():]
  BValue x = bsts_string_substring(a, 0, offset);
  BValue y = bsts_string_substring_tail(a, offset + blen);
  res = alloc_enum1(1, alloc_struct2(x, y));

  return res;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_rpartition__String(BValue a, BValue b) {
  size_t blen = bsts_string_utf8_len(b);
  if (blen == 0) {
    // the result has to give proper substrings, so here we return None
    return alloc_enum0(0);
  }
  size_t alen = bsts_string_utf8_len(a);
  int offset = bsts_string_rfind(a, b, alen - 1);
  if (offset < 0) {
    // return None
    return alloc_enum0(0);
  }
  // we return substrings
  // Some((x, y)) with x = a[0:offset], y = a[offset + b.len():]
  BValue x = bsts_string_substring(a, 0, offset);
  BValue y = bsts_string_substring_tail(a, offset + blen);
  return alloc_enum1(1, alloc_struct2(x, y));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__left__Int(BValue a, BValue b) {
  return bsts_integer_shift_left(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_shift__right__Int(BValue a, BValue b) {
  BValue negb = bsts_integer_negate(b);
  return bsts_integer_shift_left(a, negb);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_cmp__String(BValue a, BValue b) {
  int result = bsts_string_cmp(a, b);
  // -1, 0, 1, but we map to 0, 1, 2 which are the adt tags for LT, EQ, GT
  return alloc_enum0(result + 1);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_string__to__Int(BValue a) {
  return bsts_string_to_integer(a);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_sub(BValue a, BValue b) {
  return ___bsts_g_Bosatsu_l_Predef_l_add(a, bsts_integer_negate(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_subf(BValue a, BValue b) {
  return bsts_float64_from_double(bsts_float64_to_double(a) - bsts_float64_to_double(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_mul(BValue a, BValue b) {
  return bsts_integer_times(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_times(BValue a, BValue b) {
  // compatibility alias for previously exported predef name
  return ___bsts_g_Bosatsu_l_Predef_l_mul(a, b);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_timesf(BValue a, BValue b) {
  return bsts_float64_from_double(bsts_float64_to_double(a) * bsts_float64_to_double(b));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_trace(BValue a, BValue b) {
  bsts_string_println(a);
  return b;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_xor__Int(BValue a, BValue b) {
  return bsts_integer_xor(a, b);
}
