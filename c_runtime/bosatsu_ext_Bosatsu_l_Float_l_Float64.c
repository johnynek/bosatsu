#include "bosatsu_runtime.h"
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static BValue bsts_boxf(double d) {
  return bsts_float64_from_double(d);
}

static double bsts_unboxf(BValue v) {
  return bsts_float64_to_double(v);
}

static BValue bsts_bool(_Bool b) {
  return alloc_enum0(b ? 1 : 0);
}

static BValue bsts_none(void) {
  return alloc_enum0(0);
}

static BValue bsts_some(BValue value) {
  return alloc_enum1(1, value);
}

static BValue bsts_uint64_to_int(uint64_t bits) {
  uint32_t words[2] = {
    (uint32_t)(bits & UINT32_C(0xffffffff)),
    (uint32_t)((bits >> 32) & UINT32_C(0xffffffff))
  };

  if (words[1] == 0 && words[0] <= INT32_MAX) {
    return bsts_integer_from_int((int32_t)words[0]);
  }
  return bsts_integer_from_words_copy(1, words[1] == 0 ? 1 : 2, words);
}

static uint64_t bsts_int_to_low_uint64(BValue int_value) {
  uint32_t mask_words[2] = { UINT32_C(0xffffffff), UINT32_C(0xffffffff) };
  uint32_t div_words[2] = { UINT32_C(0x00000000), UINT32_C(0x00000001) };
  BValue mask = bsts_integer_from_words_copy(1, 2, mask_words);
  BValue two32 = bsts_integer_from_words_copy(1, 2, div_words);
  BValue low64 = bsts_integer_and(int_value, mask);
  BValue divmod = bsts_integer_div_mod(low64, two32);
  BValue hi = get_struct_index(divmod, 0);
  BValue lo = get_struct_index(divmod, 1);
  uint32_t hi32 = (uint32_t)bsts_integer_to_int32(hi);
  uint32_t lo32 = (uint32_t)bsts_integer_to_int32(lo);
  return (((uint64_t)hi32) << 32) | ((uint64_t)lo32);
}

static int hex_digit_value(unsigned char c) {
  if (c >= '0' && c <= '9') return (int)(c - '0');
  if (c >= 'a' && c <= 'f') return 10 + (int)(c - 'a');
  if (c >= 'A' && c <= 'F') return 10 + (int)(c - 'A');
  return -1;
}

static _Bool parse_hex_64(const char* bytes, size_t len, uint64_t* out) {
  if (len != 16) return 0;
  uint64_t acc = 0;
  for (size_t i = 0; i < len; i++) {
    int hex = hex_digit_value((unsigned char)bytes[i]);
    if (hex < 0) return 0;
    acc = (acc << 4) | (uint64_t)hex;
  }
  *out = acc;
  return 1;
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_abs(BValue a) {
  return bsts_boxf(fabs(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_acos(BValue a) {
  return bsts_boxf(acos(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_asin(BValue a) {
  return bsts_boxf(asin(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_atan(BValue a) {
  return bsts_boxf(atan(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_atan2(BValue a, BValue b) {
  return bsts_boxf(atan2(bsts_unboxf(a), bsts_unboxf(b)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_ceil(BValue a) {
  return bsts_boxf(ceil(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_copy__sign(BValue a, BValue b) {
  return bsts_boxf(copysign(bsts_unboxf(a), bsts_unboxf(b)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_cos(BValue a) {
  return bsts_boxf(cos(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_cosh(BValue a) {
  return bsts_boxf(cosh(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_exp(BValue a) {
  return bsts_boxf(exp(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_floor(BValue a) {
  return bsts_boxf(floor(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_hypot(BValue a, BValue b) {
  return bsts_boxf(hypot(bsts_unboxf(a), bsts_unboxf(b)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_is__infinite(BValue a) {
  return bsts_bool(isinf(bsts_unboxf(a)) != 0);
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_is__nan(BValue a) {
  return bsts_bool(isnan(bsts_unboxf(a)) != 0);
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_float64__to__String(BValue a) {
  double d = bsts_unboxf(a);
  if (isinf(d)) {
    return signbit(d) ? bsts_string_from_utf8_bytes_static(4, "-\xE2\x88\x9E")
                      : bsts_string_from_utf8_bytes_static(3, "\xE2\x88\x9E");
  }

  if (isnan(d)) {
    char nan_buf[32];
    uint64_t bits = bsts_float64_to_bits(a);
    int nan_len = snprintf(nan_buf, sizeof(nan_buf), "NaN:0x%016llx", (unsigned long long)bits);
    if (nan_len <= 0) {
      return bsts_string_from_utf8_bytes_static(0, NULL);
    }
    return bsts_string_from_utf8_bytes_copy((size_t)nan_len, nan_buf);
  }

  char buf[32];
  int len = snprintf(buf, sizeof(buf), "%.17g", d);
  if (len <= 0) {
    return bsts_string_from_utf8_bytes_static(0, NULL);
  }

  if (strchr(buf, '.') == NULL && strchr(buf, 'e') == NULL && strchr(buf, 'E') == NULL) {
    if (len + 2 < (int)sizeof(buf)) {
      buf[len] = '.';
      buf[len + 1] = '0';
      buf[len + 2] = '\0';
      len += 2;
    }
  }

  return bsts_string_from_utf8_bytes_copy((size_t)len, buf);
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_string__to__Float64(BValue a) {
  size_t len = bsts_string_utf8_len(a);
  char* bytes = bsts_string_utf8_bytes(a);

  char* cleaned = (char*)malloc(len + 1);
  if (cleaned == NULL) {
    perror("failed to malloc in string_to_Float64");
    abort();
  }

  size_t out_idx = 0;
  for (size_t i = 0; i < len; i++) {
    if (bytes[i] != '_') {
      cleaned[out_idx] = bytes[i];
      out_idx++;
    }
  }
  cleaned[out_idx] = '\0';

  if (out_idx == 3 && memcmp(cleaned, "\xE2\x88\x9E", 3) == 0) {
    free(cleaned);
    return bsts_some(bsts_boxf(INFINITY));
  }
  if (out_idx == 4 && cleaned[0] == '+' && memcmp(cleaned + 1, "\xE2\x88\x9E", 3) == 0) {
    free(cleaned);
    return bsts_some(bsts_boxf(INFINITY));
  }
  if (out_idx == 4 && cleaned[0] == '-' && memcmp(cleaned + 1, "\xE2\x88\x9E", 3) == 0) {
    free(cleaned);
    return bsts_some(bsts_boxf(-INFINITY));
  }
  if ((out_idx == 8 && memcmp(cleaned, "Infinity", 8) == 0) ||
      (out_idx == 9 && memcmp(cleaned, "+Infinity", 9) == 0)) {
    free(cleaned);
    return bsts_some(bsts_boxf(INFINITY));
  }
  if (out_idx == 9 && memcmp(cleaned, "-Infinity", 9) == 0) {
    free(cleaned);
    return bsts_some(bsts_boxf(-INFINITY));
  }
  if ((out_idx == 3 && memcmp(cleaned, "NaN", 3) == 0) ||
      (out_idx == 4 && memcmp(cleaned, ".NaN", 4) == 0)) {
    free(cleaned);
    return bsts_some(bsts_boxf(NAN));
  }
  if (out_idx == 22 && memcmp(cleaned, "NaN:0x", 6) == 0) {
    uint64_t nan_bits = 0;
    if (parse_hex_64(cleaned + 6, 16, &nan_bits)) {
      free(cleaned);
      return bsts_some(bsts_float64_from_bits(nan_bits));
    }
    free(cleaned);
    return bsts_none();
  }

  char* endptr = NULL;
  double parsed = strtod(cleaned, &endptr);
  _Bool ok = (out_idx > 0) && (endptr != cleaned) && (endptr != NULL) && (*endptr == '\0');
  free(cleaned);

  if (!ok) return bsts_none();
  return bsts_some(bsts_boxf(parsed));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_int__bits__to__Float64(BValue a) {
  return bsts_float64_from_bits(bsts_int_to_low_uint64(a));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_float64__bits__to__Int(BValue a) {
  return bsts_uint64_to_int(bsts_float64_to_bits(a));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_log(BValue a) {
  return bsts_boxf(log(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_log10(BValue a) {
  return bsts_boxf(log10(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_pow(BValue a, BValue b) {
  return bsts_boxf(pow(bsts_unboxf(a), bsts_unboxf(b)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_sin(BValue a) {
  return bsts_boxf(sin(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_sinh(BValue a) {
  return bsts_boxf(sinh(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_sqrt(BValue a) {
  return bsts_boxf(sqrt(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_tan(BValue a) {
  return bsts_boxf(tan(bsts_unboxf(a)));
}

BValue ___bsts_g_Bosatsu_l_Float_l_Float64_l_tanh(BValue a) {
  return bsts_boxf(tanh(bsts_unboxf(a)));
}
