#include "bosatsu_runtime.h"
#include <limits.h>
#include <math.h>

static BValue bsts_none(void) {
  return alloc_enum0(0);
}

static BValue bsts_bool(_Bool b) {
  if (b) return alloc_enum0(1);
  return alloc_enum0(0);
}

static BValue bsts_some(BValue value) {
  return alloc_enum1(1, value);
}

static BValue bsts_cmp_from_int(int cmp) {
  if (cmp < 0) return alloc_enum0(0);
  if (cmp > 0) return alloc_enum0(2);
  return alloc_enum0(1);
}

static BValue bsts_i64_min_bound_cons() {
  return bsts_integer_from_int64(INT64_MIN);
}

static BValue bsts_i64_max_bound_cons() {
  return bsts_integer_from_int64(INT64_MAX);
}

static _Atomic BValue bsts_i64_min_bound_value = BSTS_BVALUE_NULL;
static _Atomic BValue bsts_i64_max_bound_value = BSTS_BVALUE_NULL;

static BValue bsts_i64_min_bound() {
  return read_or_build(&bsts_i64_min_bound_value, bsts_i64_min_bound_cons);
}

static BValue bsts_i64_max_bound() {
  return read_or_build(&bsts_i64_max_bound_value, bsts_i64_max_bound_cons);
}

static uint64_t bsts_int64_arithmetic_shift_right_bits(uint64_t bits, uint32_t shift) {
  if (shift == 0U) return bits;
  if (shift >= 64U) {
    return (bits & UINT64_C(0x8000000000000000)) ? UINT64_MAX : UINT64_C(0);
  }

  uint64_t logical = bits >> shift;
  if ((bits & UINT64_C(0x8000000000000000)) == 0U) {
    return logical;
  }

  uint64_t fill = UINT64_MAX << (64U - shift);
  return logical | fill;
}

static uint64_t bsts_int64_logical_shift_right_bits(uint64_t bits, uint32_t shift) {
  if (shift == 0U) return bits;
  if (shift >= 64U) return UINT64_C(0);
  return bits >> shift;
}

static uint32_t bsts_int64_popcount_bits(uint64_t bits) {
#if defined(__clang__) || defined(__GNUC__)
  return (uint32_t)__builtin_popcountll(bits);
#else
  uint32_t total = 0U;
  while (bits != 0U) {
    bits &= (bits - UINT64_C(1));
    total += 1U;
  }
  return total;
#endif
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_min__i64() {
  return bsts_int64_from_int64(INT64_MIN);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_max__i64() {
  return bsts_int64_from_int64(INT64_MAX);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__to__Int64(BValue a) {
  if (bsts_integer_cmp(a, bsts_i64_min_bound()) < 0) return bsts_none();
  if (bsts_integer_cmp(a, bsts_i64_max_bound()) > 0) return bsts_none();
  return bsts_some(bsts_int64_from_bits(bsts_integer_to_low_uint64(a)));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_int__low__bits__to__Int64(BValue a) {
  return bsts_int64_from_bits(bsts_integer_to_low_uint64(a));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Int(BValue a) {
  return bsts_integer_from_int64(bsts_int64_to_int64(a));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_int64__to__Float64(BValue a) {
  return bsts_float64_from_double((double)bsts_int64_to_int64(a));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_float64__to__Int64(BValue a) {
  double value = bsts_float64_to_double(a);
  if (isnan(value) || isinf(value)) {
    return bsts_none();
  }

  double rounded = bsts_round_ties_even(value);
  if (!((rounded >= -0x1p63) && (rounded < 0x1p63))) {
    return bsts_none();
  }

  return bsts_some(bsts_int64_from_int64((int64_t)rounded));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_add__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) + bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_sub__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) - bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_mul__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) * bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_div__Int64(BValue a, BValue b) {
  int64_t lhs = bsts_int64_to_int64(a);
  int64_t rhs = bsts_int64_to_int64(b);
  if (rhs == 0) return bsts_int64_from_int64(0);
  if ((lhs == INT64_MIN) && (rhs == -1)) {
    return bsts_int64_from_int64(INT64_MIN);
  }

  int64_t quot = lhs / rhs;
  int64_t rem = lhs % rhs;
  if ((rem != 0) && ((lhs < 0) != (rhs < 0))) {
    quot -= 1;
  }
  return bsts_int64_from_int64(quot);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_mod__Int64(BValue a, BValue b) {
  int64_t lhs = bsts_int64_to_int64(a);
  int64_t rhs = bsts_int64_to_int64(b);
  if (rhs == 0) return bsts_int64_from_int64(lhs);
  if ((lhs == INT64_MIN) && (rhs == -1)) {
    return bsts_int64_from_int64(0);
  }

  int64_t rem = lhs % rhs;
  if ((rem != 0) && ((lhs < 0) != (rhs < 0))) {
    rem += rhs;
  }
  return bsts_int64_from_int64(rem);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_and__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) & bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_or__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) | bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_xor__Int64(BValue a, BValue b) {
  return bsts_int64_from_bits(bsts_int64_to_bits(a) ^ bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_not__Int64(BValue a) {
  return bsts_int64_from_bits(~bsts_int64_to_bits(a));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__left__Int64(BValue a, BValue b) {
  if (bsts_integer_is_zero(b)) return a;
  int shift_vs_zero = bsts_integer_cmp(b, bsts_integer_from_int(0));

  uint64_t bits = bsts_int64_to_bits(a);
  BValue shift = (shift_vs_zero > 0) ? b : bsts_integer_negate(b);
  if (bsts_integer_cmp(shift, bsts_integer_from_int(64)) >= 0) {
    if (shift_vs_zero > 0) return bsts_int64_from_int64(0);
    return bsts_int64_from_bits(
        (bits & UINT64_C(0x8000000000000000)) ? UINT64_MAX : UINT64_C(0));
  }

  uint32_t shift_count = (uint32_t)bsts_integer_to_int32(shift);
  if (shift_vs_zero > 0) {
    return bsts_int64_from_bits(bits << shift_count);
  }
  return bsts_int64_from_bits(
      bsts_int64_arithmetic_shift_right_bits(bits, shift_count));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__Int64(BValue a, BValue b) {
  if (bsts_integer_is_zero(b)) return a;
  int shift_vs_zero = bsts_integer_cmp(b, bsts_integer_from_int(0));

  uint64_t bits = bsts_int64_to_bits(a);
  BValue shift = (shift_vs_zero > 0) ? b : bsts_integer_negate(b);
  if (bsts_integer_cmp(shift, bsts_integer_from_int(64)) >= 0) {
    if (shift_vs_zero > 0) {
      return bsts_int64_from_bits(
          (bits & UINT64_C(0x8000000000000000)) ? UINT64_MAX : UINT64_C(0));
    }
    return bsts_int64_from_int64(0);
  }

  uint32_t shift_count = (uint32_t)bsts_integer_to_int32(shift);
  if (shift_vs_zero > 0) {
    return bsts_int64_from_bits(
        bsts_int64_arithmetic_shift_right_bits(bits, shift_count));
  }
  return bsts_int64_from_bits(bits << shift_count);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_shift__right__unsigned__Int64(BValue a, BValue b) {
  if (bsts_integer_is_zero(b)) return a;
  int shift_vs_zero = bsts_integer_cmp(b, bsts_integer_from_int(0));

  uint64_t bits = bsts_int64_to_bits(a);
  BValue shift = (shift_vs_zero > 0) ? b : bsts_integer_negate(b);
  if (bsts_integer_cmp(shift, bsts_integer_from_int(64)) >= 0) {
    return bsts_int64_from_int64(0);
  }

  uint32_t shift_count = (uint32_t)bsts_integer_to_int32(shift);
  if (shift_vs_zero > 0) {
    return bsts_int64_from_bits(
        bsts_int64_logical_shift_right_bits(bits, shift_count));
  }
  return bsts_int64_from_bits(bits << shift_count);
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_popcount__Int64(BValue a) {
  return bsts_integer_from_int((int)bsts_int64_popcount_bits(bsts_int64_to_bits(a)));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_eq__Int64(BValue a, BValue b) {
  return bsts_bool(bsts_int64_to_bits(a) == bsts_int64_to_bits(b));
}

BValue ___bsts_g_Bosatsu_l_Num_l_Int64_l_cmp__Int64(BValue a, BValue b) {
  int64_t lhs = bsts_int64_to_int64(a);
  int64_t rhs = bsts_int64_to_int64(b);
  if (lhs < rhs) return bsts_cmp_from_int(-1);
  if (lhs > rhs) return bsts_cmp_from_int(1);
  return bsts_cmp_from_int(0);
}
