#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

typedef uintptr_t BValue;
typedef uint32_t ENUM_TAG;
#include "bosatsu_decls_generated.h"

#define BSTS_BVALUE_NULL ((BValue)0)

static inline BValue bsts_bvalue_from_ptr(const void* ptr) {
  return (BValue)(uintptr_t)ptr;
}

static inline void* bsts_bvalue_to_ptr(BValue value) {
  return (void*)(uintptr_t)value;
}

static inline const void* bsts_bvalue_to_const_ptr(BValue value) {
  return (const void*)(uintptr_t)value;
}

#define BSTS_VALUE_FROM_PTR(ptr) bsts_bvalue_from_ptr((const void*)(ptr))
#define BSTS_PTR(type, value) ((type*)bsts_bvalue_to_ptr((value)))
#define BSTS_CONST_PTR(type, value) ((const type*)bsts_bvalue_to_const_ptr((value)))

#define BSTS_STRING_INLINE16_FLAG (((size_t)1) << 63)
#define BSTS_STRING_MAX_LEN ((size_t)(BSTS_STRING_INLINE16_FLAG - 1))

#define BSTS_SMALL_INT_PAYLOAD_BITS 63
#define BSTS_SMALL_INT_MIN (-(INT64_C(1) << 62))
#define BSTS_SMALL_INT_MAX ((INT64_C(1) << 62) - 1)

typedef struct BSTS_String_View {
  size_t len;
  const char* bytes;
} BSTS_String_View;

typedef struct BSTS_String {
  size_t len_meta;
  union {
    struct {
      size_t offset;
      const char* bytes;
    } ext;
    unsigned char inl[16];
  } payload;
} BSTS_String;

// Compile-time initializer for static external string objects.
#define BSTS_STATIC_STRING_INIT(len_value, bytes_value) \
  { (size_t)(len_value), { .ext = { (size_t)0, (bytes_value) } } }

// Nat values are encoded in integers
// TODO: move these to functions implemented in bosatsu_runtime.c
#define BSTS_NAT_0 ((BValue)((uintptr_t)0x1))
#define BSTS_NAT_SUCC(n) ((BValue)((n) + (BValue)((uintptr_t)2)))
#define BSTS_NAT_PREV(n) ((BValue)((n) - (BValue)((uintptr_t)2)))
#define BSTS_NAT_IS_0(n) ((n) == BSTS_NAT_0)
#define BSTS_NAT_GT_0(n) ((n) != BSTS_NAT_0)

// Finalizer for non-GC payloads stored via alloc_external.
typedef void (*BSTS_FreeFn)(void*);
// A function which constructs a BValue
typedef BValue (*BConstruct)();

// delta may be negative or positive
void bsts_increment_value(BValue value, int delta);

// (&BValue, int) -> &BValue
BValue get_struct_index(BValue v, int idx);

// &BValue -> Tag
ENUM_TAG get_variant(BValue v);
// This is only safe if all enums have zero args
ENUM_TAG get_variant_value(BValue v);
// (&BValue, int) -> &BValue
BValue get_enum_index(BValue v, int idx);

// This one is not auto generated because it can always be fit into the BValue directly.
// Keep it header-only so call sites can expand this to a value equivalent to
// TO_PURE_VALUE(tag). NOTE: keep this encoding in sync with TO_PURE_VALUE in
// bosatsu_runtime.c.
static inline BValue alloc_enum0(ENUM_TAG tag) {
  return (BValue)((((uintptr_t)tag) << 2) | ((uintptr_t)0x1));
}

BValue bsts_string_from_utf8_bytes_copy(size_t len, const char* bytes);
// This is dangerous, it should not be mutated after returned 
BValue bsts_string_mut(size_t len);
BValue bsts_string_from_utf8_bytes_static(size_t len, const char* bytes);
BValue bsts_string_from_utf8_bytes_static_null_term(const char* bytes);
/*
 * write the codepoint into bytes, which must be >= 4 in length
 * and return the number of bytes written
 */
int bsts_string_code_point_to_utf8(int codepoint, char* bytes);
// (&String, &String) -> Bool
_Bool bsts_string_equals(BValue left, BValue right);
// (&String, &String) -> int in {-1, 0, 1}
int bsts_string_cmp(BValue left, BValue right);
// &String -> int (length in bytes)
size_t bsts_string_utf8_len_ref(const BValue* str);
// Return a view into string bytes. For tiny inline strings the bytes pointer is
// into the provided BValue storage and is valid while that storage is alive.
BSTS_String_View bsts_string_view_ref(const BValue* str);
const char* bsts_string_utf8_bytes_ref(const BValue* str);
// Mutable bytes for owned, non-tiny, non-sliced strings; aborts otherwise.
char* bsts_string_utf8_bytes_mut(BValue str);
int bsts_utf8_code_point_bytes(const char* utf8data, int offset, int len);

// How many bytes is the codepoint at this offset, 1, 2, 3, 4, or -1 on error
// (&String, int) -> int
int bsts_string_code_point_bytes(BValue, int offset);

// (&String, int) -> char
BValue bsts_string_char_at(BValue, int);

// (&string, int, int) -> string
BValue bsts_string_substring(BValue, int start, int end);

// (&String, int) -> String
BValue bsts_string_substring_tail(BValue, int byte_offset);

// return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
// (&string, string, int) -> int
int bsts_string_find(BValue haystack, BValue needle, int start);
/*
 * search from right to left.
 * return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
 * (&string, string, int) -> int
 */
int bsts_string_rfind(BValue haystack, BValue needle, int start);
// &String -> Unit
void bsts_string_print(BValue v);
// &String -> Unit
void bsts_string_println(BValue v);

BValue bsts_unit_value();

BValue bsts_char_from_code_point(int codepoint);
int bsts_char_code_point_from_value(BValue ch);

BValue bsts_integer_from_int(int32_t small_int);
// Construct from 64-bit values, using immediate representation when possible.
BValue bsts_integer_from_int64(int64_t value);
BValue bsts_integer_from_uint64(uint64_t value);
int32_t bsts_integer_to_int32(BValue bint);
BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words);
// Integer-specific helper: callers must already know the value is an Integer.
static inline _Bool bsts_integer_is_small(BValue value) {
  return (value & (BValue)((uintptr_t)0x1)) == (BValue)((uintptr_t)0x1);
}
// Integer-specific helper: callers must already know the value is a small Integer.
static inline int64_t bsts_integer_small_value(BValue value) {
  return ((int64_t)value) >> 1;
}
int bsts_integer_cmp_zero(BValue v);
_Bool bsts_integer_is_zero(BValue v);
_Bool bsts_integer_equals(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_add(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_sub(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_times(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_or(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_xor(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_and(BValue l, BValue r);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_shift_left(BValue l, BValue r);
// &Integer -> Integer
BValue bsts_integer_popcount(BValue v);
// (&Integer, &Integer) -> int
int bsts_integer_cmp(BValue l, BValue r);
// return the negative of this
// Integer -> Integer
BValue bsts_integer_negate(BValue v);
// Integer -> Integer
BValue bsts_integer_not(BValue v);
// &Integer -> String
BValue bsts_integer_to_string(BValue v);
// String -> Option[Integer]
BValue bsts_string_to_integer(BValue v);
// (&Integer, &Integer) -> (Integer, Integer)
// div_mod(l, r) == (d, m) <=> l = r * d + m
BValue bsts_integer_div_mod(BValue l, BValue r);
// Integer -> uint64, taking low 64 bits in two's complement for negatives.
uint64_t bsts_integer_to_low_uint64(BValue bint);
// Integer -> Float64 using round-to-nearest, ties-to-even semantics.
double bsts_integer_to_double(BValue bint);
// Integral finite Float64 -> Integer.
BValue bsts_integral_float64_to_integer(double d);
// Round finite Float64 to the nearest integer with ties to even.
double bsts_round_ties_even(double d);

// Int64 values are stored by packing signed two's-complement bits into the
// BValue word.
static inline BValue bsts_int64_from_bits(uint64_t bits) {
  return (BValue)bits;
}
static inline uint64_t bsts_int64_to_bits(BValue v) {
  return (uint64_t)v;
}
// Reinterpret bits between signed/unsigned 64-bit forms. These are not
// numeric conversions; keep exact two's-complement payload bits.
static inline BValue bsts_int64_from_int64(int64_t value) {
  union {
    int64_t i;
    uint64_t u;
  } conv;
  conv.i = value;
  return bsts_int64_from_bits(conv.u);
}
static inline int64_t bsts_int64_to_int64(BValue v) {
  union {
    int64_t i;
    uint64_t u;
  } conv;
  conv.u = bsts_int64_to_bits(v);
  return conv.i;
}

// Float64 values are stored by packing IEEE754 bits into the BValue word.
#define BSTS_FLOAT64_SIGN_MASK UINT64_C(0x8000000000000000)
#define BSTS_FLOAT64_EXP_MASK UINT64_C(0x7ff0000000000000)
#define BSTS_FLOAT64_FRAC_MASK UINT64_C(0x000fffffffffffff)

static inline BValue bsts_float64_from_bits(uint64_t bits) {
  return (BValue)bits;
}
static inline uint64_t bsts_float64_to_bits(BValue v) {
  return (uint64_t)v;
}
// Reinterpret IEEE754 bits between double and BValue storage. Do not replace
// these with casts such as (double)v or (uint64_t)d, which perform numeric
// conversion instead of preserving the exact bit pattern.
static inline BValue bsts_float64_from_double(double d) {
  union {
    double d;
    uint64_t u;
  } conv;
  conv.d = d;
  return bsts_float64_from_bits(conv.u);
}
static inline double bsts_float64_to_double(BValue v) {
  union {
    double d;
    uint64_t u;
  } conv;
  conv.u = bsts_float64_to_bits(v);
  return conv.d;
}
static inline _Bool bsts_float64_is_nan_bits(uint64_t bits) {
  return ((bits & BSTS_FLOAT64_EXP_MASK) == BSTS_FLOAT64_EXP_MASK) &&
      ((bits & BSTS_FLOAT64_FRAC_MASK) != 0);
}
static inline _Bool bsts_float64_is_zero_bits(uint64_t bits) {
  return (bits & ~BSTS_FLOAT64_SIGN_MASK) == 0;
}
static inline uint64_t bsts_float64_order_key(uint64_t bits) {
  return (bits & BSTS_FLOAT64_SIGN_MASK) ? ~bits : (bits ^ BSTS_FLOAT64_SIGN_MASK);
}
static inline _Bool bsts_float64_equals(BValue left, BValue right) {
  if (left == right) return 1;

  uint64_t lbits = bsts_float64_to_bits(left);
  uint64_t rbits = bsts_float64_to_bits(right);
  if (bsts_float64_is_zero_bits(lbits) && bsts_float64_is_zero_bits(rbits)) {
    return 1;
  }
  return bsts_float64_is_nan_bits(lbits) && bsts_float64_is_nan_bits(rbits);
}
static inline int bsts_float64_cmp_total(BValue left, BValue right) {
  if (left == right) return 0;

  uint64_t lbits = bsts_float64_to_bits(left);
  uint64_t rbits = bsts_float64_to_bits(right);
  _Bool l_nan = bsts_float64_is_nan_bits(lbits);
  _Bool r_nan = bsts_float64_is_nan_bits(rbits);
  if (l_nan) return r_nan ? 0 : -1;
  if (r_nan) return 1;

  if (bsts_float64_is_zero_bits(lbits) && bsts_float64_is_zero_bits(rbits)) {
    return 0;
  }

  uint64_t lkey = bsts_float64_order_key(lbits);
  uint64_t rkey = bsts_float64_order_key(rbits);
  if (lkey < rkey) return -1;
  return 1;
}

// Wrap non-GC (e.g. malloc-backed) payload data with a finalizer callback.
// Contract: free_fn is expected to be non-NULL for this representation.
// For GC-managed payloads with no finalizer, use BSTS_VALUE_FROM_PTR/BSTS_PTR directly.
BValue alloc_external(void* eval, BSTS_FreeFn free_fn);
// Unwrap payloads created by alloc_external.
void* get_external(BValue v);

// Given the slots variable return the closure fn value
BValue bsts_closure_from_slots(BValue*);

// should be called in main before accessing any BValue top level functions
void init_statics();

// should be called immediately before returning.
void free_statics();

// This should only be called immediately on allocated values
// on top level allocations that are made
void free_on_close(BValue v);

BValue read_or_build(_Atomic BValue* v, BConstruct cons);

typedef struct BSTS_Test_Result {
  char* package_name;
  int passes;
  int fails;
  uint64_t elapsed_nanos;
} BSTS_Test_Result;

// This is the constructor to get a Test value for the given package name
// and print to stdout
_Bool bsts_test_argv_has_quiet(int argc, char** argv);
BSTS_Test_Result bsts_test_run(
    char* package_name,
    BConstruct test_value,
    _Bool quiet
);
BSTS_Test_Result bsts_test_run_prog(
    char* package_name,
    BConstruct test_value,
    _Bool quiet
);
int bsts_test_result_print_summary(int count, BSTS_Test_Result* results);

#endif
