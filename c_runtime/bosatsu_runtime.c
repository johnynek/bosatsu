#include "bosatsu_runtime.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <time.h>
#include "gc.h"

#include <assert.h>

_Static_assert(sizeof(void*) == 8, "Bosatsu runtime currently requires 64-bit pointers");
_Static_assert(sizeof(uintptr_t) == 8, "Bosatsu runtime assumes 64-bit uintptr_t");
_Static_assert(sizeof(size_t) == 8, "Bosatsu runtime assumes 64-bit size_t");
_Static_assert(sizeof(BSTS_String) == 24, "BSTS_String should remain 24 bytes on 64-bit platforms");
_Static_assert(BSTS_SMALL_INT_PAYLOAD_BITS == 63, "small-int payload bits must remain 63");

/*
There are a few kinds of values:

1. generic pure values: enum0 values, characters, small strings that can fit into 63 bits.
2. pointers to gc'ed values

to distinguish these cases we allocate pointers such that they are aligned to at least 4 byte
boundaries:
  a. ends with 01: pure value
  b. ends with 00: gc pointer.

Integer-specific helpers additionally treat any odd BValue as a small Int. This is
safe because integer operations are type-directed.

when it comes to functions there are two types, PureFn and closures. We have to box
  pointers to them, but when we know we have a global PureFn we can directly call it.
  if we have a static boxed value, it ends in 1, else 0.

Nat-like values are represented by positive integers encoded as PURE_VALUE such that
NAT(x) = (x << 2) | 1, since we don't have enough time to increment through 2^{62} values
this is a safe encoding.

Char values are stored as tiny UTF-8 strings (1..4 bytes) in type-directed contexts.

String values are either pointers to BSTS_String objects or tiny strings packed
directly into the BValue word in type-directed contexts.

Integer values are either odd immediates (signed values packed into 63 payload bits),
or gced big integers
*/
#define TAG_MASK ((uintptr_t)0x3)
#define PURE_VALUE_TAG ((uintptr_t)0x1)
#define POINTER_TAG ((uintptr_t)0x0)

#define TINY_STRING_TAG PURE_VALUE_TAG
#define TINY_STRING_LEN_SHIFT ((uintptr_t)2)
#define TINY_STRING_LEN_MASK ((uintptr_t)0x7)
#define TINY_STRING_MAX_LEN ((size_t)5)
#define MEDIUM_STRING_MAX_LEN ((size_t)16)
#define BSTS_STRING_LEN_LIMIT BSTS_STRING_INLINE16_FLAG

static inline _Bool bsts_is_pure_value(BValue value) {
  return (value & TAG_MASK) == PURE_VALUE_TAG;
}

static inline _Bool bsts_is_tiny_string(BValue value) {
  return (value & TAG_MASK) == TINY_STRING_TAG;
}

static inline BValue bsts_to_pure_value(uintptr_t value) {
  return (BValue)((value << 2) | PURE_VALUE_TAG);
}

static inline uintptr_t bsts_pure_value(BValue value) {
  return value >> 2;
}

#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#define BSTS_IS_LITTLE_ENDIAN 1
#elif defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#define BSTS_IS_LITTLE_ENDIAN 0
#elif defined(_WIN32)
#define BSTS_IS_LITTLE_ENDIAN 1
#else
#error "Unsupported byte order for Bosatsu tiny string layout"
#endif

static inline size_t bsts_tiny_string_len(BValue value) {
  return (size_t)((value >> TINY_STRING_LEN_SHIFT) & TINY_STRING_LEN_MASK);
}

static inline uintptr_t bsts_tiny_string_shift(size_t idx) {
#if BSTS_IS_LITTLE_ENDIAN
  return (uintptr_t)(8 * (idx + 1));
#else
  return (uintptr_t)(48 - (8 * idx));
#endif
}

static inline BValue bsts_tiny_string_from_bytes(size_t len, const char* bytes) {
  uintptr_t packed = TINY_STRING_TAG | ((uintptr_t)len << TINY_STRING_LEN_SHIFT);
  for (size_t i = 0; i < len; i++) {
    packed |= ((uintptr_t)(unsigned char)bytes[i]) << bsts_tiny_string_shift(i);
  }
  return (BValue)packed;
}

static inline const char* bsts_tiny_string_data_ptr(const BValue* value_ref) {
  return ((const char*)(const void*)value_ref) + 1;
}

static inline unsigned char bsts_tiny_string_u8_at(BValue value, size_t idx) {
  return (unsigned char)((value >> bsts_tiny_string_shift(idx)) & 0xFF);
}

static inline void bsts_abort_string_len_too_large(size_t len, const char* fn_name) {
  if (len >= BSTS_STRING_LEN_LIMIT) {
    fprintf(stderr, "%s: string length %zu exceeds supported max (%zu)\n",
            fn_name,
            len,
            (size_t)(BSTS_STRING_LEN_LIMIT - 1));
    abort();
  }
}

// Compatibility aliases used by generated helpers.
#define IS_PURE_VALUE(value) bsts_is_pure_value((value))
#define TO_PURE_VALUE(value) bsts_to_pure_value((uintptr_t)(value))
#define PURE_VALUE(value) bsts_pure_value((value))
#define IS_POINTER(value) ((((value) & TAG_MASK)) == POINTER_TAG)

#define DEFINE_BSTS_OBJ(name, fields) \
    struct name { \
      fields \
    }; \
    typedef struct name name

#define DEFINE_BSTS_ENUM(name, fields) DEFINE_BSTS_OBJ(name, ENUM_TAG tag; int32_t pad; fields)

DEFINE_BSTS_OBJ(BSTS_OBJ,);

BValue bsts_unit_value() {
  return (BValue)PURE_VALUE_TAG;
}

BValue bsts_char_from_code_point(int codepoint) {
  char utf8[4];
  size_t len;
  // Char construction sites are type-directed: code points are valid Unicode
  // scalar values by construction.
  if (codepoint <= 0x7F) {
    utf8[0] = (char)codepoint;
    len = 1;
  } else if (codepoint <= 0x7FF) {
    utf8[0] = (char)(0xC0 | ((codepoint >> 6) & 0x1F));
    utf8[1] = (char)(0x80 | (codepoint & 0x3F));
    len = 2;
  } else if (codepoint <= 0xFFFF) {
    utf8[0] = (char)(0xE0 | ((codepoint >> 12) & 0x0F));
    utf8[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
    utf8[2] = (char)(0x80 | (codepoint & 0x3F));
    len = 3;
  } else {
    utf8[0] = (char)(0xF0 | ((codepoint >> 18) & 0x07));
    utf8[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
    utf8[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
    utf8[3] = (char)(0x80 | (codepoint & 0x3F));
    len = 4;
  }
  return bsts_tiny_string_from_bytes(len, utf8);
}

int bsts_char_code_point_from_value(BValue ch) {
  // Char values are represented as tiny UTF-8 strings (len 1..4) by the
  // typed frontend and runtime constructors. We decode directly without
  // re-validating UTF-8 structure on each read.
  size_t len = bsts_tiny_string_len(ch);
  unsigned char b0 = bsts_tiny_string_u8_at(ch, 0);
  switch (len) {
    case 1:
      return (int)b0;
    case 2: {
      unsigned char b1 = bsts_tiny_string_u8_at(ch, 1);
      return (int)(((b0 & 0x1F) << 6) | (b1 & 0x3F));
    }
    case 3: {
      unsigned char b1 = bsts_tiny_string_u8_at(ch, 1);
      unsigned char b2 = bsts_tiny_string_u8_at(ch, 2);
      return (int)(((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F));
    }
    case 4: {
      unsigned char b1 = bsts_tiny_string_u8_at(ch, 1);
      unsigned char b2 = bsts_tiny_string_u8_at(ch, 2);
      unsigned char b3 = bsts_tiny_string_u8_at(ch, 3);
      return (int)(((b0 & 0x07) << 18) |
                   ((b1 & 0x3F) << 12) |
                   ((b2 & 0x3F) << 6) |
                   (b3 & 0x3F));
    }
    default:
      // Unreachable under the type-system/runtime representation invariant.
      return 0;
  }
}

// Round finite Float64 to the nearest integer using IEEE-754 ties-to-even.
// This matches the reference semantics used elsewhere in Bosatsu:
//   * Scala/JVM: java.lang.Math.rint
//   * Python backend/tests: round(x) on binary floats
// Callers screen out NaN and infinities before reaching this helper.
double bsts_round_ties_even(double d) {
  uint64_t bits = bsts_float64_to_bits(bsts_float64_from_double(d));
  uint64_t abs_bits = bits & ~BSTS_FLOAT64_SIGN_MASK;

  // All finite Float64 values with magnitude >= 2^52 are already integral.
  if (abs_bits >= UINT64_C(0x4330000000000000)) {
    return d;
  }

  // Preserve the sign of zero for magnitudes that round to 0.
  if (abs_bits < UINT64_C(0x3fe0000000000000)) {
    return bsts_float64_to_double(
        bsts_float64_from_bits(bits & BSTS_FLOAT64_SIGN_MASK));
  }

  // Handle [0.5, 1.0) separately so exact 0.5 ties round to signed zero.
  if (abs_bits < UINT64_C(0x3ff0000000000000)) {
    if (abs_bits == UINT64_C(0x3fe0000000000000)) {
      return bsts_float64_to_double(
          bsts_float64_from_bits(bits & BSTS_FLOAT64_SIGN_MASK));
    }
    return (bits & BSTS_FLOAT64_SIGN_MASK) ? -1.0 : 1.0;
  }

  int exponent = (int)((abs_bits & BSTS_FLOAT64_EXP_MASK) >> 52) - 1023;
  int fractional_bits = 52 - exponent;
  uint64_t significand = (UINT64_C(1) << 52) | (abs_bits & BSTS_FLOAT64_FRAC_MASK);
  uint64_t fractional_mask = (UINT64_C(1) << fractional_bits) - 1;
  uint64_t fractional = significand & fractional_mask;

  if (fractional == 0) {
    return d;
  }

  uint64_t integer = significand >> fractional_bits;
  uint64_t half = UINT64_C(1) << (fractional_bits - 1);
  if ((fractional > half) || ((fractional == half) && (integer & UINT64_C(1)))) {
    integer += 1;
  }

  double rounded = (double)integer;
  return (bits & BSTS_FLOAT64_SIGN_MASK) ? -rounded : rounded;
}

// Closures:
DEFINE_BSTS_OBJ(Closure1Data, BClosure1 fn; size_t slot_len;);

size_t closure_data_size(size_t slot_len) {
  return sizeof(Closure1Data) + slot_len * sizeof(BValue);
}
BValue* closure_data_of(Closure1Data* s) {
  return (BValue*)(s + 1);
}

// Given the slots variable return the closure fn value
BValue bsts_closure_from_slots(BValue* slots) {
  uintptr_t s = (uintptr_t)slots;
  uintptr_t pointer_to_closure = s - sizeof(Closure1Data);
  return (BValue)pointer_to_closure;
}

// ENUM0 can always be encoded into a BValue, but we define it to
// be able to get sizeof() to skip the header
DEFINE_BSTS_ENUM(Enum0,);

#include "bosatsu_generated.h"

DEFINE_BSTS_OBJ(BSTS_External, void* external;);
DEFINE_BSTS_OBJ(BSTS_Integer, size_t len; _Bool sign; uint32_t words[];);

typedef struct {
    size_t len;
    _Bool sign;
    uint32_t* words;
} BSTS_Int_Operand;

// Helper macros and functions
#define IS_SMALL(v) (((v) & (BValue)((uintptr_t)0x1)) == (BValue)((uintptr_t)0x1))
#define GET_BIG_INT(v) BSTS_PTR(BSTS_Integer, (v))

static inline int64_t bsts_small_int_decode(BValue value) {
  return ((int64_t)value) >> 1;
}

static inline _Bool bsts_small_int_fits_int64(int64_t value) {
  return (value >= BSTS_SMALL_INT_MIN) && (value <= BSTS_SMALL_INT_MAX);
}

static inline BValue bsts_small_int_from_int64_unchecked(int64_t value) {
  return (BValue)((((uint64_t)value) << 1) | PURE_VALUE_TAG);
}

static inline BValue bsts_small_int_from_int64_maybe(int64_t value) {
  if (!bsts_small_int_fits_int64(value)) return BSTS_BVALUE_NULL;
  return bsts_small_int_from_int64_unchecked(value);
}

static inline uint64_t bsts_abs_i64(int64_t value) {
  if (value >= 0) return (uint64_t)value;
  uint64_t abs_value = (uint64_t)(-(value + 1));
  return abs_value + UINT64_C(1);
}

static inline size_t bsts_u64_to_words(uint64_t magnitude, uint32_t* words) {
  words[0] = (uint32_t)(magnitude & UINT32_C(0xffffffff));
  words[1] = (uint32_t)((magnitude >> 32) & UINT32_C(0xffffffff));
  return (words[1] == 0U) ? 1U : 2U;
}

static inline size_t bsts_small_twos_word_len(int64_t value) {
  return (bsts_abs_i64(value) >> 32) == 0U ? 1U : 2U;
}

#if defined(__clang__) || defined(__GNUC__)
static inline uint32_t bsts_popcount_u32(uint32_t value) {
  return (uint32_t)__builtin_popcount(value);
}

static inline uint32_t bsts_popcount_u64(uint64_t value) {
  return (uint32_t)__builtin_popcountll(value);
}
#else
static inline uint32_t bsts_popcount_u32(uint32_t value) {
  uint32_t count = 0U;
  while (value != 0U) {
    value &= (value - 1U);
    count += 1U;
  }
  return count;
}

static inline uint32_t bsts_popcount_u64(uint64_t value) {
  uint32_t count = 0U;
  while (value != 0U) {
    value &= (value - 1U);
    count += 1U;
  }
  return count;
}
#endif

#define GET_SMALL_INT(v) bsts_small_int_decode((v))

void bsts_load_op_from_small(int64_t value, uint32_t* words, BSTS_Int_Operand* op) {
    op->sign = value < 0;
    op->words = words;
    op->len = bsts_u64_to_words(bsts_abs_i64(value), words);
}
 
void bsts_integer_load_op(BValue v, uint32_t* buffer, BSTS_Int_Operand* operand) {
    // Convert l to BSTS_Int_Operand
    if (IS_SMALL(v)) {
        bsts_load_op_from_small(GET_SMALL_INT(v), buffer, operand);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(v);
        operand->sign = l_big->sign;
        operand->len = l_big->len;
        operand->words = l_big->words;
    }
}

// A general structure for a reference counted memory block
// it is always allocated with len BValue array immediately after
typedef struct _Node {
  BSTS_OBJ* value;
  struct _Node* next;
} Node;

typedef struct Stack {
  Node* head;
} Stack;

static _Atomic Stack statics;

static void push(BSTS_OBJ* static_value) {
  // TODO what if this malloc fails
  Node* node = GC_malloc_uncollectable(sizeof(Node));
  node->value = static_value;

  Stack current;
  Stack next;
  do {
    current = atomic_load(&statics);
    node->next = current.head; //step 2
    next.head = node; //local change of head
  } while(!atomic_compare_exchange_weak(&statics, &current, next));
}

void free_on_close(BValue v) {
  if (IS_POINTER(v) && v != BSTS_BVALUE_NULL && GC_base(bsts_bvalue_to_ptr(v)) != NULL) {
    push(BSTS_PTR(BSTS_OBJ, v));
  }
}

// Returns NULl when there is nothing to pop
static BSTS_OBJ* pop() {
  Stack next;
  Stack current;
  do {
    current = atomic_load(&statics);
    if (current.head == NULL) {
      return NULL;
    }
    next.head = current.head->next; //set the head to the next node
  } while(!atomic_compare_exchange_weak(&statics, &current, next));
  BSTS_OBJ* rc = current.head->value;
  GC_free(current.head);
  return rc;
}

void init_statics() {
  Stack empty;
  empty.head = NULL;
  atomic_store(&statics, empty);
}

BValue get_struct_index(BValue v, int idx) {
  uintptr_t rc = v;
  BValue* ptr = (BValue*)(rc + sizeof(BSTS_OBJ) + idx * sizeof(BValue));
  return *ptr;
}

ENUM_TAG get_variant(BValue v) {
  if (IS_PURE_VALUE(v)) {
    return (ENUM_TAG)PURE_VALUE(v);
  }
  else {
    Enum0* real_v = BSTS_PTR(Enum0, v);
    return real_v->tag;
  }
}

ENUM_TAG get_variant_value(BValue v) {
  return (ENUM_TAG)PURE_VALUE(v);
}

BValue get_enum_index(BValue v, int idx) {
  uintptr_t rc = v;
  BValue* ptr = (BValue*)(rc + sizeof(Enum0) + idx * sizeof(BValue));
  return *ptr;
}

// Externals:
// alloc_external/get_external are for non-GC payloads that require explicit
// finalization. GC-managed payloads should be represented directly as BValue
// pointers and cast with BSTS_PTR/BSTS_VALUE_FROM_PTR.
void free_external(void* ex, void* data) {
  BSTS_FreeFn ex_free = (BSTS_FreeFn)data;
#if defined(BSTS_RUNTIME_DEBUG_CHECKS)
  assert(ex_free != NULL);
#endif
  ex_free(ex);
}

BValue alloc_external(void* data, BSTS_FreeFn free) {
    BSTS_External* ext = GC_malloc(sizeof(BSTS_External));
    ext->external = data;
#if defined(BSTS_RUNTIME_DEBUG_CHECKS)
    assert(free != NULL);
#endif
    GC_register_finalizer(ext, free_external, free, NULL, NULL);
    return BSTS_VALUE_FROM_PTR(ext);
}

void* get_external(BValue v) {
  // Externals can be static also, top level external values
  BSTS_External* ext = BSTS_PTR(BSTS_External, v);
  return ext->external;
}

static char bsts_empty_string_bytes[1] = {0};

static inline size_t bsts_string_len_of(const BSTS_String* str) {
  return str->len_meta & (BSTS_STRING_INLINE16_FLAG - 1);
}

static inline _Bool bsts_string_is_inline16(const BSTS_String* str) {
  return (str->len_meta & BSTS_STRING_INLINE16_FLAG) != 0;
}

static inline void bsts_string_set_len_meta(BSTS_String* str, size_t len, _Bool is_inline16) {
  if (len >= BSTS_STRING_LEN_LIMIT) {
    bsts_abort_string_len_too_large(len, "bsts_string_set_len_meta");
  }
  str->len_meta = len | (is_inline16 ? BSTS_STRING_INLINE16_FLAG : (size_t)0);
}

static inline const char* bsts_string_data_ptr(const BSTS_String* str) {
  size_t len = bsts_string_len_of(str);
  if (len == 0) return bsts_empty_string_bytes;
  if (bsts_string_is_inline16(str)) {
    return (const char*)str->payload.inl;
  }
  return str->payload.ext.bytes + str->payload.ext.offset;
}

static inline char* bsts_string_mut_data_ptr(BSTS_String* str) {
  size_t len = bsts_string_len_of(str);
  if (len == 0) return bsts_empty_string_bytes;
  if (bsts_string_is_inline16(str)) {
    return (char*)str->payload.inl;
  }
  return (char*)(str->payload.ext.bytes + str->payload.ext.offset);
}

BSTS_String_View bsts_string_view_ref(const BValue* value_ref) {
  BValue value = *value_ref;
  if (bsts_is_tiny_string(value)) {
    BSTS_String_View view;
    view.len = bsts_tiny_string_len(value);
    view.bytes = bsts_tiny_string_data_ptr(value_ref);
    return view;
  }
  const BSTS_String* str = BSTS_CONST_PTR(BSTS_String, value);
  BSTS_String_View view;
  view.len = bsts_string_len_of(str);
  view.bytes = bsts_string_data_ptr(str);
  return view;
}

size_t bsts_string_utf8_len_ref(const BValue* str) {
  return bsts_string_view_ref(str).len;
}

const char* bsts_string_utf8_bytes_ref(const BValue* str) {
  return bsts_string_view_ref(str).bytes;
}

char* bsts_string_utf8_bytes_mut(BValue value) {
  if (bsts_is_tiny_string(value)) {
    fprintf(stderr, "attempted mutable access to tiny inline string\n");
    abort();
  }
  BSTS_String* str = BSTS_PTR(BSTS_String, value);
  if (bsts_string_is_inline16(str)) {
    return (char*)str->payload.inl;
  }
  if (bsts_string_len_of(str) == 0) return bsts_empty_string_bytes;
  if (str->payload.ext.offset != 0) {
    fprintf(stderr, "attempted mutable access to sliced string bytes\n");
    abort();
  }
  if (GC_base((void*)str->payload.ext.bytes) == NULL) {
    fprintf(stderr, "attempted mutable access to non-owned string bytes\n");
    abort();
  }
  return (char*)str->payload.ext.bytes;
}

static inline BValue bsts_string_inline16_from_bytes_copy(size_t len, const char* bytes) {
  if (len > 0 && bytes == NULL) {
    fprintf(stderr, "bsts_string_inline16_from_bytes_copy: non-zero length with NULL bytes\n");
    abort();
  }
  BSTS_String* str = GC_malloc(sizeof(BSTS_String));
  if (str == NULL) {
    perror("failed to GC_malloc in bsts_string_inline16_from_bytes_copy");
    abort();
  }
  bsts_string_set_len_meta(str, len, 1);
  if (len > 0) {
    memcpy(str->payload.inl, bytes, len);
  }
  if (len < MEDIUM_STRING_MAX_LEN) {
    str->payload.inl[len] = 0;
  }
  return BSTS_VALUE_FROM_PTR(str);
}

BValue bsts_string_mut(size_t len) {
  bsts_abort_string_len_too_large(len, "bsts_string_mut");
  if (len <= MEDIUM_STRING_MAX_LEN) {
    BSTS_String* str = GC_malloc(sizeof(BSTS_String));
    if (str == NULL) {
      perror("failed to GC_malloc in bsts_string_mut");
      abort();
    }
    bsts_string_set_len_meta(str, len, 1);
    if (len < MEDIUM_STRING_MAX_LEN) {
      str->payload.inl[len] = 0;
    }
    return BSTS_VALUE_FROM_PTR(str);
  }

  BSTS_String* str = GC_malloc(sizeof(BSTS_String));
  if (str == NULL) {
      perror("failed to GC_malloc in bsts_string_mut");
      abort();
  }
  bsts_string_set_len_meta(str, len, 0);
  str->payload.ext.offset = 0;
  char* bytes = GC_malloc_atomic(sizeof(char) * len);
  if (bytes == NULL) {
      perror("failed to GC_malloc in bsts_string_mut");
      abort();
  }
  str->payload.ext.bytes = bytes;
  return BSTS_VALUE_FROM_PTR(str);
}

// this copies the bytes in, it does not take ownership
BValue bsts_string_from_utf8_bytes_copy(size_t len, const char* bytes) {
  bsts_abort_string_len_too_large(len, "bsts_string_from_utf8_bytes_copy");
  if (len > 0 && bytes == NULL) {
    fprintf(stderr, "bsts_string_from_utf8_bytes_copy: non-zero length with NULL bytes\n");
    abort();
  }
  if (len <= TINY_STRING_MAX_LEN) {
    if (len == 0) return bsts_tiny_string_from_bytes(0, "");
    return bsts_tiny_string_from_bytes(len, bytes);
  }
  if (len <= MEDIUM_STRING_MAX_LEN) {
    return bsts_string_inline16_from_bytes_copy(len, bytes);
  }
  BValue result = bsts_string_mut(len);
  if (len > 0) {
    memcpy(bsts_string_utf8_bytes_mut(result), bytes, len);
  }
  return result;
}


BValue bsts_string_from_utf8_bytes_static(size_t len, const char* bytes) {
  bsts_abort_string_len_too_large(len, "bsts_string_from_utf8_bytes_static");
  if (len > 0 && bytes == NULL) {
    fprintf(stderr, "bsts_string_from_utf8_bytes_static: non-zero length with NULL bytes\n");
    abort();
  }
  if (len <= TINY_STRING_MAX_LEN) {
    if (len == 0) return bsts_tiny_string_from_bytes(0, "");
    return bsts_tiny_string_from_bytes(len, bytes);
  }
  if (len <= MEDIUM_STRING_MAX_LEN) {
    return bsts_string_inline16_from_bytes_copy(len, bytes);
  }
  BSTS_String* str = GC_malloc_atomic(sizeof(BSTS_String));
  if (str == NULL) {
    perror("failed to GC_malloc_atomic in bsts_string_from_utf8_bytes_static");
    abort();
  }
  bsts_string_set_len_meta(str, len, 0);
  str->payload.ext.offset = 0;
  if (len == 0 && bytes == NULL) {
    str->payload.ext.bytes = bsts_empty_string_bytes;
  }
  else {
    str->payload.ext.bytes = bytes;
  }
  return BSTS_VALUE_FROM_PTR(str);
}

BValue bsts_string_from_utf8_bytes_static_null_term(const char* bytes) {
  return bsts_string_from_utf8_bytes_static(strlen(bytes), bytes);
}

int bsts_string_code_point_to_utf8(int code_point, char* output) {
    // Validate the code point
    if (code_point < 0 || code_point > 0x10FFFF ||
        (code_point >= 0xD800 && code_point <= 0xDFFF)) {
        // Invalid code point
        return -1;
    }

    if (code_point <= 0x7F) {
        // 1-byte sequence (ASCII)
        output[0] = (char)code_point;
        return 1;
    } else if (code_point <= 0x7FF) {
        // 2-byte sequence
        output[0] = (char)(0xC0 | ((code_point >> 6) & 0x1F));
        output[1] = (char)(0x80 | (code_point & 0x3F));
        return 2;
    } else if (code_point <= 0xFFFF) {
        // 3-byte sequence
        output[0] = (char)(0xE0 | ((code_point >> 12) & 0x0F));
        output[1] = (char)(0x80 | ((code_point >> 6) & 0x3F));
        output[2] = (char)(0x80 | (code_point & 0x3F));
        return 3;
    } else if (code_point <= 0x10FFFF) {
        // 4-byte sequence
        output[0] = (char)(0xF0 | ((code_point >> 18) & 0x07));
        output[1] = (char)(0x80 | ((code_point >> 12) & 0x3F));
        output[2] = (char)(0x80 | ((code_point >> 6) & 0x3F));
        output[3] = (char)(0x80 | (code_point & 0x3F));
        return 4;
    }

    // Should not reach here
    return -1;
}

_Bool bsts_string_equals(BValue left, BValue right) {
  if (left == right) {
    return 1;
  }
  BSTS_String_View lview = bsts_string_view_ref(&left);
  BSTS_String_View rview = bsts_string_view_ref(&right);
  if (lview.len != rview.len) return 0;
  if (lview.len == 0) return 1;
  return memcmp(lview.bytes, rview.bytes, lview.len) == 0;
}

int bsts_string_cmp(BValue left, BValue right) {
  if (left == right) {
    return 0;
  }
  BSTS_String_View lview = bsts_string_view_ref(&left);
  BSTS_String_View rview = bsts_string_view_ref(&right);

  size_t llen = lview.len;
  size_t rlen = rview.len;
  size_t min_len = (llen <= rlen) ? llen : rlen;
  // All Bosatsu strings are valid UTF-8. Lexicographic comparison of valid UTF-8
  // byte sequences matches lexicographic comparison of their Unicode scalar values.
  int cmp = memcmp(lview.bytes, rview.bytes, min_len);
  if (cmp < 0) return -1;
  if (cmp > 0) return 1;
  return (llen < rlen) ? -1 : ((llen > rlen) ? 1 : 0);
}

int bsts_utf8_code_point_bytes(const char* utf8data, int offset, int len) {
    if (utf8data == NULL || offset < 0 || offset >= len) {
        // Invalid input
        return -1;
    }

    // cast to an unsigned char for the math below
    const unsigned char *s = (const unsigned char*)(utf8data + offset);
    unsigned char c = s[0];
    int remaining = len - offset;
    int bytes = -1;

    if (c <= 0x7F) {
        // 1-byte sequence (ASCII)
        bytes = 1;
    } else if ((c & 0xE0) == 0xC0) {
        // 2-byte sequence
        if (remaining < 2 || (s[1] & 0xC0) != 0x80) {
            // Invalid continuation byte
            bytes = -1;
        }
        else {
          bytes = 2;
        }
    } else if ((c & 0xF0) == 0xE0) {
        // 3-byte sequence
        if (remaining < 3 || (s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80) {
            // Invalid continuation bytes
            bytes = -1;
        }
        else {
          bytes = 3;
        }
    } else if ((c & 0xF8) == 0xF0) {
        // 4-byte sequence
        if (remaining < 4 || (s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80 || (s[3] & 0xC0) != 0x80) {
            // Invalid continuation bytes
            bytes = -1;
        }
        else {
          bytes = 4;
        }
    } else {
        // Invalid UTF-8 leading byte
        bytes = -1;
    }

    // Return the code point value
    return bytes;
}

// Return the UTF-8 code point width at `offset`: 1, 2, 3, 4, or -1 for an
// invalid byte offset/lead byte. String values are assumed valid UTF-8 by the
// typed frontend, so we don't re-validate continuation bytes here.
int bsts_string_code_point_bytes(BValue value, int offset) {
    BSTS_String_View view = bsts_string_view_ref(&value);
    if (offset < 0 || offset >= (int)view.len) return -1;
    const unsigned char c = (const unsigned char)view.bytes[offset];
    if (c <= 0x7F) return 1;
    if ((c & 0xE0) == 0xC0) return 2;
    if ((c & 0xF0) == 0xE0) return 3;
    if ((c & 0xF8) == 0xF0) return 4;
    return -1;
}

/**
 * return char at the given offset.
 * String inputs are assumed valid UTF-8 from the type-directed pipeline, so
 * this decodes directly from the leading byte class.
 */
BValue bsts_string_char_at(BValue value, int offset) {
    BSTS_String_View view = bsts_string_view_ref(&value);
    if (offset < 0 || offset >= (int)view.len) {
        // Invalid input
        return 0;
    }

    // cast to an unsigned char for the math below
    const char* bytes = view.bytes;
    const unsigned char *s = (const unsigned char*)(bytes + offset);
    unsigned char c = s[0];
    int remaining = (int)view.len - offset;
    uint32_t code_point = 0;

    if (c <= 0x7F) {
        // 1-byte sequence (ASCII)
        code_point = c;
    } else if ((c & 0xE0) == 0xC0) {
        // 2-byte sequence
        if (remaining < 2) return 0;
        code_point = ((c & 0x1F) << 6) | (s[1] & 0x3F);
    } else if ((c & 0xF0) == 0xE0) {
        // 3-byte sequence
        if (remaining < 3) return 0;
        code_point = ((c & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F);
    } else if ((c & 0xF8) == 0xF0) {
        // 4-byte sequence
        if (remaining < 4) return 0;
        code_point = ((c & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F);
    } else {
        // Invalid UTF-8 leading byte
        return 0;
    }

    // Return the code point value
    return bsts_char_from_code_point(code_point);
}

// (&string, int, int) -> string
BValue bsts_string_substring(BValue value, int start, int end) {
  BSTS_String_View view = bsts_string_view_ref(&value);
  size_t len = view.len;
  if (start < 0 || end < start || (size_t)end > len) {
    // this is invalid
    return 0;
  }
  size_t new_len = end - start;
  if (new_len == 0) {
    // empty string, should probably be a constant
    return bsts_string_from_utf8_bytes_static(0, NULL);
  }
  else if (new_len <= MEDIUM_STRING_MAX_LEN) {
    return bsts_string_from_utf8_bytes_copy(new_len, view.bytes + start);
  }
  else {
    if (bsts_is_tiny_string(value)) {
      return bsts_string_from_utf8_bytes_copy(new_len, view.bytes + start);
    }
    const BSTS_String* str = BSTS_CONST_PTR(BSTS_String, value);
    if (bsts_string_is_inline16(str)) {
      return bsts_string_from_utf8_bytes_copy(new_len, view.bytes + start);
    }
    // Keep the base pointer and track the offset to avoid copying.
    BSTS_String* res = GC_malloc(sizeof(BSTS_String));
    if (res == NULL) {
      perror("failed to GC_malloc in bsts_string_substring");
      abort();
    }
    bsts_string_set_len_meta(res, new_len, 0);
    res->payload.ext.offset = str->payload.ext.offset + (size_t)start;
    res->payload.ext.bytes = str->payload.ext.bytes;
    return BSTS_VALUE_FROM_PTR(res);
  }
}

// (String, int) -> String
BValue bsts_string_substring_tail(BValue value, int byte_offset) {
  return bsts_string_substring(value, byte_offset, (int)bsts_string_utf8_len_ref(&value));
}

int bsts_string_find(BValue haystack, BValue needle, int start) {
    if (bsts_is_tiny_string(haystack) && bsts_is_tiny_string(needle)) {
      size_t haystack_len = bsts_tiny_string_len(haystack);
      size_t needle_len = bsts_tiny_string_len(needle);
      if (needle_len == 0) {
        return (start <= (int)haystack_len) ? start : -1;
      }
      if (haystack_len < needle_len) return -1;
      if (start < 0 || start > (int)(haystack_len - needle_len)) return -1;
      for (size_t i = (size_t)start; i <= haystack_len - needle_len; i++) {
        size_t j = 0;
        for (; j < needle_len; j++) {
          if (bsts_tiny_string_u8_at(haystack, i + j) != bsts_tiny_string_u8_at(needle, j)) break;
        }
        if (j == needle_len) return (int)i;
      }
      return -1;
    }

    BSTS_String_View haystack_view = bsts_string_view_ref(&haystack);
    BSTS_String_View needle_view = bsts_string_view_ref(&needle);

    size_t haystack_len = haystack_view.len;
    size_t needle_len = needle_view.len;
    const char* haystack_bytes = haystack_view.bytes;
    const char* needle_bytes = needle_view.bytes;
    if (needle_len == 0) {
        // Empty needle matches at start
        return (start <= (int)haystack_len) ? start : -1;
    }

    if (haystack_len < needle_len) return -1;
    if (start < 0 || start > (int)(haystack_len - needle_len)) {
        // Start position is out of bounds
        return -1;
    }


    // The maximum valid start index is haystack_len - needle_len
    for (size_t i = (size_t)start; i <= haystack_len - needle_len; i++) {
        if (haystack_bytes[i] == needle_bytes[0]) {
            // Potential match found, check the rest of the needle
            size_t j;
            for (j = 1; j < needle_len; j++) {
                if (haystack_bytes[i + j] != needle_bytes[j]) {
                    break;
                }
            }
            if (j == needle_len) {
                // Full match found
                return (int)i;
            }
        }
    }

    // No match found
    return -1;
}

int bsts_string_rfind(BValue haystack, BValue needle, int start) {
    if (bsts_is_tiny_string(haystack) && bsts_is_tiny_string(needle)) {
      size_t hlen = bsts_tiny_string_len(haystack);
      size_t nlen = bsts_tiny_string_len(needle);
      if (nlen == 0) {
        if (hlen == 0) return 0;
        if (start < 0) start = (int)(hlen - 1);
        if (start >= (int)hlen) return -1;
        return start;
      }
      if (hlen < nlen) return -1;
      size_t max_pos = hlen - nlen;
      size_t i = (start < 0) ? max_pos : (start > (int)max_pos ? max_pos : (size_t)start);
      for (;; ) {
        size_t j = 0;
        for (; j < nlen; j++) {
          if (bsts_tiny_string_u8_at(haystack, i + j) != bsts_tiny_string_u8_at(needle, j)) break;
        }
        if (j == nlen) return (int)i;
        if (i == 0) break;
        i--;
      }
      return -1;
    }

    BSTS_String_View h = bsts_string_view_ref(&haystack);
    BSTS_String_View n = bsts_string_view_ref(&needle);

    size_t hlen = h.len;
    size_t nlen = n.len;
    const char* hbytes = h.bytes;
    const char* nbytes = n.bytes;

    if (nlen == 0) {
        if (hlen == 0) return 0;
        if (start < 0) start = (int)(hlen - 1);
        if (start >= (int)hlen) return -1;
        return start;
    }

    if (hlen < nlen) return -1;

    // Clamp start to last possible position where needle fits
    size_t max_pos = hlen - nlen;
    size_t i = (start < 0) ? max_pos
                           : (start > (int)max_pos ? max_pos : (size_t)start);

    for (;; ) {
        if (hbytes[i] == nbytes[0]) {
            size_t j = 1;
            for (; j < nlen; j++) {
                if (hbytes[i + j] != nbytes[j]) break;
            }
            if (j == nlen) return (int)i;
        }
        if (i == 0) break;
        i--;
    }
    return -1;
}

void bsts_string_println(BValue v) {
  BSTS_String_View view = bsts_string_view_ref(&v);
  // TODO: if this string is somehow too big for an int this may fail
  printf("%.*s\n", (int)view.len, view.bytes);
}

void bsts_string_print(BValue v) {
  BSTS_String_View view = bsts_string_view_ref(&v);
  // TODO: if this string is somehow too big for an int this may fail
  printf("%.*s", (int)view.len, view.bytes);
}

BValue bsts_integer_from_int(int32_t small_int) {
    return bsts_small_int_from_int64_unchecked((int64_t)small_int);
}

int32_t bsts_integer_to_int32(BValue bint) {
    if (IS_SMALL(bint)) {
        int64_t small = GET_SMALL_INT(bint);
        if (small < INT32_MIN) {
            return INT32_MIN;
        }
        return (int32_t)small;
    }

    BSTS_Integer* bi = GET_BIG_INT(bint);
    if (bi->len == 0) return 0;

    uint32_t low = bi->words[0];
    if (bi->sign == 0) { // positive
        return (int32_t)low;  // truncation
    } else {
        uint64_t mag = low;
        if (bi->len > 1 || mag > (uint64_t)INT32_MAX + 1) {
            // out of range
            return INT32_MIN;

        }
        int64_t val = -(int64_t)mag;
        return (int32_t)val;
    }
}


BSTS_Integer* bsts_integer_alloc(size_t size) {
    size_t bytes = sizeof(BSTS_Integer) + size * sizeof(uint32_t);
    BSTS_Integer* integer = (BSTS_Integer*)GC_malloc_atomic(bytes);
    if (integer == NULL) {
        perror("failed to alloc BSTS_Integer");
        abort();
    }
    integer->len = size;
    integer->sign = 0;
    return integer; // Low bit is 0 since it's a pointer
}

static BValue bsts_maybe_small_int_words(_Bool is_pos, size_t size, const uint32_t* words) {
    if (size == 0) {
      return bsts_small_int_from_int64_unchecked(0);
    }
    while ((size > 1) && (words[size - 1] == 0U)) {
      size--;
    }
    if (size > 2) {
      return BSTS_BVALUE_NULL;
    }

    uint64_t magnitude = (uint64_t)words[0];
    if (size == 2) {
      uint32_t high = words[1];
      // Any high bits above payload bit 62 cannot fit in small-int form.
      if (high > UINT32_C(0x40000000)) {
        return BSTS_BVALUE_NULL;
      }
      magnitude |= ((uint64_t)high << 32);
    }

    if (magnitude == 0U) {
      return bsts_small_int_from_int64_unchecked(0);
    }

    if (is_pos) {
      if (magnitude > (uint64_t)BSTS_SMALL_INT_MAX) {
        return BSTS_BVALUE_NULL;
      }
      return bsts_small_int_from_int64_unchecked((int64_t)magnitude);
    }

    uint64_t min_mag = UINT64_C(1) << 62;
    if (magnitude > min_mag) {
      return BSTS_BVALUE_NULL;
    }
    if (magnitude == min_mag) {
      return bsts_small_int_from_int64_unchecked(BSTS_SMALL_INT_MIN);
    }
    return bsts_small_int_from_int64_unchecked(-(int64_t)magnitude);
}

BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words) {
    // remove any leading 0 words
    while ((size > 1) && (words[size - 1] == 0)) {
      size--;
    }
    {
      BValue maybe = bsts_maybe_small_int_words(is_pos, size, words);
      if (maybe) return maybe;
    }
    BSTS_Integer* integer = bsts_integer_alloc(size);
    integer->sign = !is_pos; // sign: 0 for positive, 1 for negative
    memcpy(integer->words, words, size * sizeof(uint32_t));
    return BSTS_VALUE_FROM_PTR(integer);
}

static BValue bsts_integer_finish_allocated_words(_Bool is_pos, size_t size, BSTS_Integer* integer) {
    while ((size > 1) && (integer->words[size - 1] == 0U)) {
      size--;
    }
    {
      BValue maybe = bsts_maybe_small_int_words(is_pos, size, integer->words);
      if (maybe) return maybe;
    }
    integer->len = size;
    integer->sign = !is_pos;
    return BSTS_VALUE_FROM_PTR(integer);
}

BValue bsts_integer_from_int64(int64_t result) {
  BValue maybe = bsts_small_int_from_int64_maybe(result);
  if (maybe != BSTS_BVALUE_NULL) {
      return maybe;
  }
  uint32_t words[2];
  size_t size = bsts_u64_to_words(bsts_abs_i64(result), words);
  return bsts_integer_from_words_copy(result >= 0, size, words);
}

BValue bsts_integer_from_uint64(uint64_t result) {
  if (result <= (uint64_t)BSTS_SMALL_INT_MAX) {
      return bsts_small_int_from_int64_unchecked((int64_t)result);
  }
  uint32_t words[2];
  size_t size = bsts_u64_to_words(result, words);
  return bsts_integer_from_words_copy(1, size, words);
}

uint64_t bsts_integer_to_low_uint64(BValue bint) {
  if (IS_SMALL(bint)) {
    return (uint64_t)(int64_t)GET_SMALL_INT(bint);
  }

  BSTS_Integer* bi = GET_BIG_INT(bint);
  uint64_t low = 0;
  if (bi->len > 0) low |= (uint64_t)bi->words[0];
  if (bi->len > 1) low |= ((uint64_t)bi->words[1]) << 32;
  if (!bi->sign) return low;
  return (~low) + UINT64_C(1);
}

static unsigned bsts_u32_msb_index(uint32_t word) {
#if defined(__clang__) || defined(__GNUC__)
  return (unsigned)(31U - (unsigned)__builtin_clz(word));
#else
  unsigned idx = 0;
  while (word >>= 1) {
    idx++;
  }
  return idx;
#endif
}

static inline _Bool bsts_bigint_get_bit(const BSTS_Integer* bi, size_t bit_index) {
  size_t word_index = bit_index >> 5;
  if (word_index >= bi->len) return 0;
  return (bi->words[word_index] >> (bit_index & 31U)) & 1U;
}

static _Bool bsts_bigint_any_lower_bits(const BSTS_Integer* bi, size_t bit_count) {
  size_t full_words = bit_count >> 5;
  size_t limit = (full_words < bi->len) ? full_words : bi->len;
  for (size_t i = 0; i < limit; i++) {
    if (bi->words[i] != 0) return 1;
  }

  size_t rem_bits = bit_count & 31U;
  if (rem_bits == 0 || full_words >= bi->len) return 0;
  uint32_t mask = (UINT32_C(1) << rem_bits) - 1U;
  return (bi->words[full_words] & mask) != 0U;
}

double bsts_integer_to_double(BValue bint) {
  if (IS_SMALL(bint)) {
    return (double)GET_SMALL_INT(bint);
  }

  BSTS_Integer* bi = GET_BIG_INT(bint);
  size_t len = bi->len;
  while (len > 0 && bi->words[len - 1] == 0) {
    len--;
  }
  if (len == 0) return 0.0;

  uint32_t top_word = bi->words[len - 1];
  size_t exponent = (len - 1) * 32U + (size_t)bsts_u32_msb_index(top_word);

  if (exponent > 1023U) {
    return bi->sign ? -INFINITY : INFINITY;
  }

  if (exponent <= 52U) {
    uint64_t mag = 0;
    for (size_t i = len; i > 0; i--) {
      mag = (mag << 32) | (uint64_t)bi->words[i - 1];
    }
    double d = (double)mag;
    return bi->sign ? -d : d;
  }

  size_t shift = exponent - 52U;
  uint64_t top53 = 0;
  for (size_t bit = 0; bit < 53U; bit++) {
    if (bsts_bigint_get_bit(bi, shift + bit)) {
      top53 |= UINT64_C(1) << bit;
    }
  }

  if (shift > 0U) {
    _Bool half = bsts_bigint_get_bit(bi, shift - 1U);
    _Bool sticky = bsts_bigint_any_lower_bits(bi, shift - 1U);
    if (half && (sticky || (top53 & UINT64_C(1)) != 0U)) {
      top53 += UINT64_C(1);
      if (top53 == (UINT64_C(1) << 53)) {
        top53 >>= 1;
        exponent += 1U;
        if (exponent > 1023U) {
          return bi->sign ? -INFINITY : INFINITY;
        }
      }
    }
  }

  double d = ldexp((double)top53, (int)exponent - 52);
  return bi->sign ? -d : d;
}

BValue bsts_integral_float64_to_integer(double d) {
  if (d == 0.0) {
    return bsts_integer_from_int(0);
  }

  union {
    double d;
    uint64_t bits;
  } conv;
  conv.d = d;

  _Bool is_negative = (conv.bits >> 63) != 0;
  uint64_t exponent_bits = (conv.bits >> 52) & UINT64_C(0x7ff);
  uint64_t fraction = conv.bits & UINT64_C(0x000fffffffffffff);

  // Caller passes finite integral values; keep behavior safe for unexpected input.
  if (exponent_bits == UINT64_C(0x7ff)) {
    return bsts_integer_from_int(0);
  }
  if (exponent_bits == 0) {
    return bsts_integer_from_int(0);
  }

  int exponent = (int)exponent_bits - 1023;
  if (exponent < 0) {
    return bsts_integer_from_int(0);
  }

  uint64_t significand = (UINT64_C(1) << 52) | fraction;
  if (exponent <= 52) {
    uint64_t magnitude = significand >> (52 - exponent);
    uint32_t words[2] = {
      (uint32_t)(magnitude & UINT32_C(0xffffffff)),
      (uint32_t)(magnitude >> 32)
    };
    size_t size = (words[1] == 0) ? 1 : 2;
    return bsts_integer_from_words_copy(!is_negative, size, words);
  }

  size_t shift = (size_t)(exponent - 52);
  size_t word_shift = shift >> 5;
  unsigned bit_shift = (unsigned)(shift & 31U);

  // Max finite exponent is 1023, so this comfortably fits.
  uint32_t words[35] = {0};
  uint32_t low = (uint32_t)(significand & UINT32_C(0xffffffff));
  uint32_t high = (uint32_t)(significand >> 32);
  size_t size = word_shift + 3;

  if (bit_shift == 0U) {
    words[word_shift] = low;
    words[word_shift + 1] = high;
  } else {
    words[word_shift] = low << bit_shift;
    words[word_shift + 1] =
        (low >> (32U - bit_shift)) | (high << bit_shift);
    words[word_shift + 2] = high >> (32U - bit_shift);
  }

  while (size > 1 && words[size - 1] == 0U) {
    size--;
  }
  return bsts_integer_from_words_copy(!is_negative, size, words);
}

// Function to check equality between two BValues
_Bool bsts_integer_equals(BValue left, BValue right) {
    if (left == right) { return 1; }

    _Bool l_is_small = IS_SMALL(left);
    _Bool r_is_small = IS_SMALL(right);

    if (l_is_small && r_is_small) {
        // Both are small integers, but they aren't equal
        return 0;
    } else if (!l_is_small && !r_is_small) {
        // Both are BSTS_Integer pointers
        BSTS_Integer* l_int = GET_BIG_INT(left);
        BSTS_Integer* r_int = GET_BIG_INT(right);

        // Compare sign
        if (l_int->sign != r_int->sign)
            return 0;
        // Compare length
        if (l_int->len != r_int->len)
            return 0;
        // Compare words
        for (size_t i = 0; i < l_int->len; ++i) {
            if (l_int->words[i] != r_int->words[i])
                return 0;
        }
        return 1; // All equal
    } else {
        // One is small integer, one is BSTS_Integer*
        // Ensure left is the small integer
        if (!l_is_small) {
            BValue temp = left;
            left = right;
            right = temp;
            _Bool temp_is_small = l_is_small;
            l_is_small = r_is_small;
            r_is_small = temp_is_small;
        }

        // Extract small integer value
        int64_t small_int_value = GET_SMALL_INT(left);
        BSTS_Integer* big_int = GET_BIG_INT(right);

        // Check sign
        _Bool big_int_sign = big_int->sign; // 0 for positive, 1 for negative
        _Bool small_int_sign = (small_int_value < 0) ? 1 : 0;
        if (big_int_sign != small_int_sign) {
            return 0; // Different signs
        }
        if (big_int->len == 0) {
          return small_int_value == 0;
        }

        uint32_t small_words[2];
        BSTS_Int_Operand small_operand;
        bsts_load_op_from_small(small_int_value, small_words, &small_operand);

        if (big_int->len != small_operand.len) {
          return 0;
        }
        for (size_t i = 0; i < big_int->len; i++) {
          if (big_int->words[i] != small_operand.words[i]) {
            return 0;
          }
        }
        return 1;
    }
}

int compare_abs(size_t len_a, uint32_t* words_a, size_t len_b, uint32_t* words_b) {
    if (len_a > len_b) {
        return 1;
    } else if (len_a < len_b) {
        return -1;
    } else {
        // Same length, compare from most significant word
        for (size_t i = len_a; i > 0; i--) {
            uint32_t word_a = words_a[i - 1];
            uint32_t word_b = words_b[i - 1];
            if (word_a > word_b) {
                return 1;
            } else if (word_a < word_b) {
                return -1;
            }
        }
        return 0;
    }
}

#if !defined(__SIZEOF_INT128__)
static _Bool bsts_mul_i64_overflow(int64_t left, int64_t right, int64_t* out) {
#if defined(__has_builtin)
#if __has_builtin(__builtin_mul_overflow)
    return __builtin_mul_overflow(left, right, out);
#endif
#endif
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_mul_overflow(left, right, out);
#else
    if ((left == 0) || (right == 0)) {
      *out = 0;
      return 0;
    }
    if (left == -1) {
      if (right == INT64_MIN) return 1;
      *out = -right;
      return 0;
    }
    if (right == -1) {
      if (left == INT64_MIN) return 1;
      *out = -left;
      return 0;
    }

    if (left > 0) {
      if (right > 0) {
        if (left > (INT64_MAX / right)) return 1;
      } else {
        if (right < (INT64_MIN / left)) return 1;
      }
    } else {
      if (right > 0) {
        if (left < (INT64_MIN / right)) return 1;
      } else {
        if (left < (INT64_MAX / right)) return 1;
      }
    }
    *out = left * right;
    return 0;
#endif
}
#endif

static BValue bsts_integer_add_loaded(BSTS_Int_Operand left_operand, BSTS_Int_Operand right_operand) {
    BValue result = BSTS_BVALUE_NULL;
    if (left_operand.sign == right_operand.sign) {
        _Bool result_sign = left_operand.sign;
        size_t max_len = (left_operand.len > right_operand.len) ? left_operand.len : right_operand.len;
        BSTS_Integer* result_integer = bsts_integer_alloc(max_len + 1);
        uint32_t* result_words = result_integer->words;

        uint64_t carry = 0;
        size_t i = 0;
        for (; i < max_len; i++) {
            uint64_t left_word = (i < left_operand.len) ? left_operand.words[i] : 0;
            uint64_t right_word = (i < right_operand.len) ? right_operand.words[i] : 0;
            uint64_t sum = left_word + right_word + carry;
            result_words[i] = (uint32_t)(sum & UINT32_C(0xffffffff));
            carry = sum >> 32;
        }
        if (carry) {
            result_words[i++] = (uint32_t)carry;
        }
        size_t result_len = i;

        while (result_len > 1 && result_words[result_len - 1] == 0) {
            result_len--;
        }

        result = bsts_integer_finish_allocated_words(!result_sign, result_len, result_integer);
    } else {
        int cmp = compare_abs(left_operand.len, left_operand.words, right_operand.len, right_operand.words);
        if (cmp == 0) {
            result = bsts_integer_from_int(0);
        } else {
            BSTS_Int_Operand* larger;
            BSTS_Int_Operand* smaller;
            _Bool result_sign;
            if (cmp > 0) {
                larger = &left_operand;
                smaller = &right_operand;
                result_sign = left_operand.sign;
            } else {
                larger = &right_operand;
                smaller = &left_operand;
                result_sign = right_operand.sign;
            }

            size_t result_len = larger->len;
            BSTS_Integer* result_integer = bsts_integer_alloc(result_len);
            uint32_t* result_words = result_integer->words;

            int64_t borrow = 0;
            for (size_t i = 0; i < result_len; i++) {
                int64_t large_word = (int64_t)larger->words[i];
                int64_t small_word = (i < smaller->len) ? (int64_t)smaller->words[i] : 0;
                int64_t diff = large_word - small_word - borrow;
                if (diff < 0) {
                    diff += ((int64_t)1 << 32);
                    borrow = 1;
                } else {
                    borrow = 0;
                }
                result_words[i] = (uint32_t)(diff & UINT32_C(0xffffffff));
            }

            while (result_len > 1 && result_words[result_len - 1] == 0) {
                result_len--;
            }

            result = bsts_integer_finish_allocated_words(!result_sign, result_len, result_integer);
        }
    }

    return result;
}

BValue bsts_integer_add(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (l_is_small && r_is_small) {
        int64_t l_int = GET_SMALL_INT(l);
        int64_t r_int = GET_SMALL_INT(r);
        return bsts_integer_from_int64(l_int + r_int);
    } else if (l == (BValue)PURE_VALUE_TAG) {
        // sub(x, y) is encoded as add(x, negate(y)), and in Bosatsu code
        // -y is encoded as 0 - y. We should have a negate in Predef, but don't currently.
        return r;
    } else if (r == (BValue)PURE_VALUE_TAG) {
        return l;
    } else {
        // At least one operand is a big integer

        uint32_t left_temp[2];
        uint32_t right_temp[2];
        BSTS_Int_Operand left_operand;
        BSTS_Int_Operand right_operand;

        bsts_integer_load_op(l, left_temp, &left_operand);
        bsts_integer_load_op(r, right_temp, &right_operand);
        return bsts_integer_add_loaded(left_operand, right_operand);
    }
}

BValue bsts_integer_sub(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (r == (BValue)PURE_VALUE_TAG) {
        return l;
    }
    if (l_is_small && r_is_small) {
        int64_t l_int = GET_SMALL_INT(l);
        int64_t r_int = GET_SMALL_INT(r);
        return bsts_integer_from_int64(l_int - r_int);
    }
    if (l == (BValue)PURE_VALUE_TAG) {
        return bsts_integer_negate(r);
    }
    if (r_is_small) {
        return bsts_integer_add(l, bsts_integer_from_int64(-GET_SMALL_INT(r)));
    }

    uint32_t left_temp[2];
    uint32_t right_temp[2];
    BSTS_Int_Operand left_operand;
    BSTS_Int_Operand right_operand;

    bsts_integer_load_op(l, left_temp, &left_operand);
    bsts_integer_load_op(r, right_temp, &right_operand);
    right_operand.sign = !right_operand.sign;
    return bsts_integer_add_loaded(left_operand, right_operand);
}

// Function to negate a BValue
BValue bsts_integer_negate(BValue v) {
    if (IS_SMALL(v)) {
        int64_t small = GET_SMALL_INT(v);
        return bsts_integer_from_int64(-small);
    } else {
        // Negate big integer
        BSTS_Integer* integer = GET_BIG_INT(v);
        // Check if the integer is zero
        _Bool is_zero = 1;
        for (size_t i = 0; i < integer->len; ++i) {
            if (integer->words[i] != 0) {
                is_zero = 0;
                break;
            }
        }
        if (is_zero) {
            // Zero remains zero when negated
            return bsts_integer_from_int(0);
        }
        // Create a new big integer with flipped sign

        // recall the sign is (-1)^sign, so to negate, pos = sign
        _Bool pos = integer->sign;
        return bsts_integer_from_words_copy(pos, integer->len, integer->words);
    }
}

BValue bsts_integer_not(BValue v) {
    if (IS_SMALL(v)) {
        return bsts_integer_from_int64(~GET_SMALL_INT(v));
    }
    return bsts_integer_sub(bsts_integer_from_int(-1), v);
}

// Helper f;unction to divide big integer by 10
uint32_t bigint_divide_by_10(uint32_t* words, size_t len, uint32_t* quotient_words, size_t* quotient_len_ptr) {
    uint64_t remainder = 0;
    for (size_t i = len; i > 0; i--) {
        uint64_t dividend = (remainder << 32) | words[i - 1];
        uint32_t quotient = (uint32_t)(dividend / 10);
        remainder = dividend % 10;
        quotient_words[i - 1] = quotient;
    }

    // Remove leading zeros
    size_t quotient_len = len;
    while (quotient_len > 0 && quotient_words[quotient_len - 1] == 0) {
        quotient_len--;
    }

    if (quotient_len_ptr) {
        *quotient_len_ptr = quotient_len;
    }

    return (uint32_t)remainder;
}

// &Integer -> String
BValue bsts_integer_to_string(BValue v) {
    if (IS_SMALL(v)) {
        int64_t value = GET_SMALL_INT(v);

        // Convert small integer to string
        char buffer[32]; // Enough for 64-bit integer
        int length = snprintf(buffer, sizeof(buffer), "%lld", (long long)value);

        if (length < 0) {
            // snprintf error
            perror("snprintf error in bsts_integer_to_string");
            abort();
        }

        return bsts_string_from_utf8_bytes_copy(length, buffer);
    } else {
        // Big integer
        BSTS_Integer* bigint = GET_BIG_INT(v);

        // Check for zero
        int is_zero = 1;
        for (size_t i = 0; i < bigint->len; i++) {
            if (bigint->words[i] != 0) {
                is_zero = 0;
                break;
            }
        }
        if (is_zero) {
            // Return "0"
            return bsts_string_from_utf8_bytes_static(1, "0");
        }

        // Estimate the maximum number of digits
        size_t bits = bigint->len * 32;
        size_t max_digits = (size_t)(bits * 0.30103) + 2; // +1 for sign, +1 for safety

        // Allocate array for digits
        char* digits = (char*)malloc(max_digits);
        if (digits == NULL) {
            // Memory allocation error
            perror("failed to malloc digits in bsts_integer_to_string");
            abort();
        }

        size_t digit_count = 0;

        // Make a copy of the bigint words
        size_t len = bigint->len;
        uint32_t* words_copy = (uint32_t*)malloc(len * sizeof(uint32_t));
        if (words_copy == NULL) {
            // Memory allocation error
            free(digits);
            perror("failed to malloc words_copy in bsts_integer_to_string");
            abort();
        }
        memcpy(words_copy, bigint->words, len * sizeof(uint32_t));

        uint32_t* quotient_words = (uint32_t*)malloc(len * sizeof(uint32_t));
        if (quotient_words == NULL) {
            // Memory allocation error
            free(digits);
            free(words_copy);
            perror("failed to malloc quotient_words in bsts_integer_to_string");
            abort();
        }

        // Handle sign
        _Bool sign = bigint->sign;

        // Repeatedly divide words_copy by 10
        while (len > 0) {
            size_t quotient_len = 0;
            uint32_t remainder = bigint_divide_by_10(words_copy, len, quotient_words, &quotient_len);

            // Store the remainder as a digit
            digits[digit_count++] = '0' + (char)remainder;

            // Prepare for next iteration
            len = quotient_len;
            uint32_t* temp = words_copy;
            words_copy = quotient_words;
            quotient_words = temp;
        }

        // Free the last quotient_words
        free(quotient_words);

        // If negative, add '-' sign
        if (sign) {
            digits[digit_count++] = '-';
        }

        // Now, reverse the digits to get the correct order
        BSTS_String* res = BSTS_PTR(BSTS_String, bsts_string_mut(digit_count));
        char* out = bsts_string_mut_data_ptr(res);

        // reverse the data
        for (size_t i = 0; i < digit_count; i++) {
            out[i] = digits[digit_count - i - 1];
        }

        // Free temporary allocations
        free(digits);
        free(words_copy);

        return BSTS_VALUE_FROM_PTR(res);
    }
}

// String -> Option[Integer]
BValue bsts_string_to_integer(BValue v) {
  BSTS_String_View view = bsts_string_view_ref(&v);
  size_t slen = view.len;
  const char* bytes = view.bytes;
  if (slen == 0) return alloc_enum0(0);

  size_t pos = 0;
  _Bool sign = 0;
  if (bytes[pos] == '-') {
    sign = 1;
    pos++;
    if (slen == 1) return alloc_enum0(0);
  }
  // at least 1 character

  int64_t acc = 0;
  _Bool use_big_acc = 0;
  BValue bacc = BSTS_BVALUE_NULL;
  BValue ten = bsts_integer_from_int(10);
  while(pos < slen) {
    int32_t digit = (int32_t)(bytes[pos] - '0');
    if ((digit < 0) || (9 < digit)) return alloc_enum0(0);

    if (!use_big_acc) {
      // Keep an int64 accumulator until the next decimal step would overflow.
      if (acc > (INT64_MAX - digit) / 10) {
        use_big_acc = 1;
        bacc = bsts_integer_from_int64(acc);
      } else {
        acc = acc * 10 + digit;
        pos++;
        continue;
      }
    }
    bacc = bsts_integer_add(bsts_integer_times(bacc, ten), bsts_integer_from_int(digit));
    pos++;
  }

  if (!use_big_acc) {
    return alloc_enum1(1, bsts_integer_from_int64(sign ? -acc : acc));
  }
  return alloc_enum1(1, sign ? bsts_integer_negate(bacc) : bacc);
}

// Function to convert sign-magnitude to two's complement representation
void sign_magnitude_to_twos_complement(_Bool sign, size_t len, uint32_t* words, uint32_t* result_words, size_t result_len) {
    memcpy(result_words, words, len * sizeof(uint32_t));
    for (size_t i = len; i < result_len; i++) {
        result_words[i] = 0;
    }
    if (sign == 1) {
        // Negative number
        // Invert all bits
        for (size_t i = 0; i < result_len; i++) {
            result_words[i] = ~result_words[i];
        }
        // Add 1
        uint64_t carry = 1;
        for (size_t i = 0; i < result_len; i++) {
            uint64_t sum = (uint64_t)result_words[i] + carry;
            result_words[i] = (uint32_t)(sum & 0xFFFFFFFF);
            carry = sum >> 32;
            if (carry == 0) {
                break;
            }
        }
    }
}

// Function to convert two's complement to sign-magnitude representation
void twos_complement_to_sign_magnitude(size_t len, uint32_t* words, _Bool* sign, size_t* result_len, uint32_t* result_words) {
    // Determine sign from the most significant bit
    uint32_t msb = words[len - 1];
    if (msb & 0x80000000) {
        // Negative number
        *sign = 1;
        // Take two's complement to get magnitude
        uint32_t* temp_words = (uint32_t*)malloc(len * sizeof(uint32_t));
        if (temp_words == NULL) {
            *result_len = 0;
            return;
        }
        for (size_t i = 0; i < len; i++) {
            temp_words[i] = ~words[i];
        }
        // Add 1
        uint64_t carry = 1;
        for (size_t i = 0; i < len; i++) {
            uint64_t sum = (uint64_t)temp_words[i] + carry;
            temp_words[i] = (uint32_t)(sum & 0xFFFFFFFF);
            carry = sum >> 32;
            if (carry == 0) {
                break;
            }
        }
        // Remove leading zeros
        size_t mag_len = len;
        while (mag_len > 1 && temp_words[mag_len - 1] == 0) {
            mag_len--;
        }
        memcpy(result_words, temp_words, mag_len * sizeof(uint32_t));
        *result_len = mag_len;
        free(temp_words);
    } else {
        // Positive number
        *sign = 0;
        size_t mag_len = len;
        while (mag_len > 1 && words[mag_len - 1] == 0) {
            mag_len--;
        }
        memcpy(result_words, words, mag_len * sizeof(uint32_t));
        *result_len = mag_len;
    }
}

void bsts_integer_small_to_twos(int64_t value, uint32_t* target, size_t max_len) {
  uint64_t bits = (uint64_t)value;
  if (max_len > 0) {
    target[0] = (uint32_t)(bits & UINT32_C(0xffffffff));
  }
  if (max_len > 1) {
    target[1] = (uint32_t)((bits >> 32) & UINT32_C(0xffffffff));
  }
  uint32_t fill = (value < 0) ? UINT32_C(0xffffffff) : UINT32_C(0x00000000);
  for (size_t i = 2; i < max_len; i++) {
    target[i] = fill;
  }
}

BValue bsts_integer_from_twos(size_t max_len, uint32_t* result_twos) {
    // Convert result from two's complement to sign-magnitude
    _Bool result_sign;
    size_t result_len = max_len;
    BSTS_Integer* result = bsts_integer_alloc(max_len);
    twos_complement_to_sign_magnitude(max_len, result_twos, &result_sign, &result_len, result->words);
    free(result_twos);

    // Canonicalize to immediate if the normalized magnitude fits 63 bits.
    BValue maybe = bsts_maybe_small_int_words(!result_sign, result_len, result->words);
    if (maybe) {
      return maybe;
    }
    result->len = result_len;
    result->sign = result_sign;
    return BSTS_VALUE_FROM_PTR(result);
}

// Function to perform bitwise AND on two BValues
BValue bsts_integer_and(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (l_is_small && r_is_small) {
      int64_t lv = GET_SMALL_INT(l);
      int64_t rv = GET_SMALL_INT(r);
      return bsts_integer_from_int64(lv & rv);
    }
    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(l)) : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(r)) : (GET_BIG_INT(r)->len + 1);
    size_t max_len = (l_len > r_len) ? l_len : r_len;

    // Ensure at least one word
    if (max_len == 0) {
        max_len = 1;
    }

    // Allocate arrays for two's complement representations
    uint32_t* l_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    uint32_t* r_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    if (l_twos == NULL || r_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to calloc l_twos or r_twos in bsts_integer_and");
        abort();
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(l), l_twos, max_len);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(r), r_twos, max_len);
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise AND
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to malloc result_twos in bsts_integer_and");
        abort();
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] & r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    // Convert result from two's complement to sign-magnitude
    return bsts_integer_from_twos(max_len, result_twos);
}

// Function to multiply two BValues
BValue bsts_integer_times(BValue left, BValue right) {
    _Bool left_is_small = IS_SMALL(left);
    _Bool right_is_small = IS_SMALL(right);

    if (left_is_small && right_is_small) {
        int64_t l_int = GET_SMALL_INT(left);
        int64_t r_int = GET_SMALL_INT(right);
#if defined(__SIZEOF_INT128__)
        __int128 wide = ((__int128)l_int) * ((__int128)r_int);
        if ((wide >= (__int128)BSTS_SMALL_INT_MIN) && (wide <= (__int128)BSTS_SMALL_INT_MAX)) {
            return bsts_small_int_from_int64_unchecked((int64_t)wide);
        }
        unsigned __int128 mag = (wide < 0) ? (unsigned __int128)(-wide) : (unsigned __int128)wide;
        uint32_t words[4] = { 0, 0, 0, 0 };
        size_t size = 0;
        while (mag != 0) {
            words[size++] = (uint32_t)(mag & UINT32_C(0xffffffff));
            mag >>= 32;
        }
        if (size == 0) {
            words[0] = 0;
            size = 1;
        }
        return bsts_integer_from_words_copy(wide >= 0, size, words);
#else
        int64_t result = 0;
        if (!bsts_mul_i64_overflow(l_int, r_int, &result)) {
            return bsts_integer_from_int64(result);
        }

        uint32_t left_words[2];
        uint32_t right_words[2];
        size_t left_len = bsts_u64_to_words(bsts_abs_i64(l_int), left_words);
        size_t right_len = bsts_u64_to_words(bsts_abs_i64(r_int), right_words);
        uint32_t result_words[4] = { 0, 0, 0, 0 };
        size_t result_len = left_len + right_len;
        for (size_t i = 0; i < left_len; i++) {
            uint64_t carry = 0;
            uint64_t a = left_words[i];
            for (size_t j = 0; j < right_len; j++) {
                uint64_t b = right_words[j];
                uint64_t sum = (uint64_t)result_words[i + j] + a * b + carry;
                result_words[i + j] = (uint32_t)(sum & UINT32_C(0xffffffff));
                carry = sum >> 32;
            }
            result_words[i + right_len] += (uint32_t)carry;
        }
        while ((result_len > 1) && (result_words[result_len - 1] == 0U)) {
            result_len--;
        }
        return bsts_integer_from_words_copy((l_int < 0) == (r_int < 0), result_len, result_words);
#endif
    }

    if (left_is_small) {
      int64_t lv = GET_SMALL_INT(left);
      if (lv == 0) return bsts_integer_from_int(0);
      if (lv == 1) return right;
      if (lv == -1) return bsts_integer_negate(right);
    }
    if (right_is_small) {
      int64_t rv = GET_SMALL_INT(right);
      if (rv == 0) return bsts_integer_from_int(0);
      if (rv == 1) return left;
      if (rv == -1) return bsts_integer_negate(left);
    }

    // At least one operand is big integer.
    uint32_t left_temp[2];
    uint32_t right_temp[2];
    BSTS_Int_Operand l_operand;
    BSTS_Int_Operand r_operand;

    bsts_integer_load_op(left, left_temp, &l_operand);
    bsts_integer_load_op(right, right_temp, &r_operand);

    size_t result_len = l_operand.len + r_operand.len;
    BSTS_Integer* result_integer = bsts_integer_alloc(result_len);
    uint32_t* result_words = result_integer->words;
    memset(result_words, 0, result_len * sizeof(uint32_t));

    for (size_t i = 0; i < l_operand.len; i++) {
        uint64_t carry = 0;
        uint64_t a = l_operand.words[i];
        for (size_t j = 0; j < r_operand.len; j++) {
            uint64_t b = r_operand.words[j];
            uint64_t sum = (uint64_t)result_words[i + j] + a * b + carry;
            result_words[i + j] = (uint32_t)(sum & UINT32_C(0xffffffff));
            carry = sum >> 32;
        }
        result_words[i + r_operand.len] += (uint32_t)carry;
    }

    _Bool result_sign = !(l_operand.sign == r_operand.sign);
    while (result_len > 1 && result_words[result_len - 1] == 0) {
        result_len--;
    }

    return bsts_integer_finish_allocated_words(!result_sign, result_len, result_integer);
}

// Function to perform bitwise OR on two BValues
BValue bsts_integer_or(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);
    if (l_is_small && r_is_small) {
      int64_t lv = GET_SMALL_INT(l);
      int64_t rv = GET_SMALL_INT(r);
      return bsts_integer_from_int64(lv | rv);
    }

    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(l)) : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(r)) : (GET_BIG_INT(r)->len + 1);
    size_t max_len = (l_len > r_len) ? l_len : r_len;

    // Ensure at least one word
    if (max_len == 0) {
        max_len = 1;
    }

    // Allocate arrays for two's complement representations
    uint32_t* l_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    uint32_t* r_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    if (l_twos == NULL || r_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to calloc l_twos or r_twos in bsts_integer_or");
        abort();
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(l), l_twos, max_len);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(r), r_twos, max_len);
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise OR
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to malloc result_twos in bsts_integer_or");
        abort();
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] | r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    return bsts_integer_from_twos(max_len, result_twos);
}

// Function to perform bitwise XOR on two BValues
BValue bsts_integer_xor(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);
    if (l_is_small && r_is_small) {
      int64_t lv = GET_SMALL_INT(l);
      int64_t rv = GET_SMALL_INT(r);
      return bsts_integer_from_int64(lv ^ rv);
    }

    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(l)) : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(r)) : (GET_BIG_INT(r)->len + 1);
    size_t max_len = (l_len > r_len) ? l_len : r_len;

    // Ensure at least one word
    if (max_len == 0) {
        max_len = 1;
    }

    // Allocate arrays for two's complement representations
    uint32_t* l_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    uint32_t* r_twos = (uint32_t*)calloc(max_len, sizeof(uint32_t));
    if (l_twos == NULL || r_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to calloc l_twos or r_twos in bsts_integer_xor");
        abort();
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(l), l_twos, max_len);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(r), r_twos, max_len);
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise XOR
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        perror("failed to malloc result_twos in bsts_integer_xor");
        abort();
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] ^ r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    // Convert result from two's complement to sign-magnitude
    return bsts_integer_from_twos(max_len, result_twos);
}

// Function to compare two BValues
// (&Integer, &Integer) -> Integer
int bsts_integer_cmp(BValue l, BValue r) {
    if (l == r) {
      // this is either same pointer or same packed value
      return 0;
    }
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (l_is_small && r_is_small) {
        // Both are small integers, but not equal (we already compared)
        int64_t l_int = GET_SMALL_INT(l);
        int64_t r_int = GET_SMALL_INT(r);
        return (l_int < r_int) ? -1 : 1;
    } else if (!l_is_small && !r_is_small) {
        // Both are big integers
        BSTS_Integer* l_big = GET_BIG_INT(l);
        BSTS_Integer* r_big = GET_BIG_INT(r);

        // Compare signs
        if (l_big->sign != r_big->sign) {
            // If signs differ, the positive one is greater
            return l_big->sign ? -1 : 1;
        }

        // Signs are the same, compare magnitudes
        if (l_big->len != r_big->len) {
            if (l_big->len > r_big->len)
                return l_big->sign ? -1 : 1;
            else
                return l_big->sign ? 1 : -1;
        } else {
            // Same length, compare words from most significant to least significant
            for (size_t i = l_big->len; i > 0; i--) {
                uint32_t l_word = l_big->words[i - 1];
                uint32_t r_word = r_big->words[i - 1];
                if (l_word != r_word) {
                    if (l_word > r_word)
                        return l_big->sign ? -1 : 1;
                    else
                        return l_big->sign ? 1 : -1;
                }
            }
            // All words are equal
            return 0;
        }
    } else {
        // One is small, one is big
        // Ensure 'l' is the big integer
        if (l_is_small) {
            // Negate the result
            int rev_cmp = bsts_integer_cmp(r, l);
            return -rev_cmp;
        }

        // Now 'l' is big, 'r' is small
        int64_t r_int = GET_SMALL_INT(r);
        BSTS_Integer* l_big = GET_BIG_INT(l);

        // Compare signs
        _Bool l_sign = l_big->sign;
        _Bool r_sign = (r_int < 0) ? 1 : 0;

        if (l_sign != r_sign) {
            return l_sign ? -1 : 1;
        }
        uint32_t buffer[2];
        BSTS_Int_Operand operand;
        bsts_load_op_from_small(r_int, buffer, &operand);

        if (l_big->len != operand.len) {
            if (l_big->len > operand.len)
                return l_sign ? -1 : 1;
            else
                return l_sign ? 1 : -1;
        } else {
            // Lengths are equal, compare words
            // Compare words from most significant to least significant
            for (size_t i = l_big->len; i > 0; i--) {
                uint32_t l_word = l_big->words[i - 1];
                uint32_t r_word = operand.words[i - 1];
                if (l_word != r_word) {
                    if (l_word > r_word)
                        return l_sign ? -1 : 1;
                    else
                        return l_sign ? 1 : -1;
                }
            }
            // All words are equal
            return 0;
        }
    }
}

// &Integer -> bool
_Bool bsts_integer_lt_zero(BValue v) {
  if (IS_SMALL(v)) {
    return GET_SMALL_INT(v) < 0;
  }
  else {
    BSTS_Integer* vint = GET_BIG_INT(v);
    return vint->sign;
  }
}

static BValue bsts_integer_shift_twos(BValue l, int64_t shift_amount) {
    _Bool l_is_small = IS_SMALL(l);
    size_t l_len = l_is_small ? bsts_small_twos_word_len(GET_SMALL_INT(l)) : (GET_BIG_INT(l)->len + 1);
    uint32_t* l_twos = (uint32_t*)calloc(l_len, sizeof(uint32_t));
    if (l_twos == NULL) {
        perror("failed to calloc l_twos in bsts_integer_shift_left");
        abort();
    }
    // Convert left operand to two's complement
    if (l_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(l), l_twos, l_len);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, l_len);
    }

    // Determine direction of shift
    _Bool shift_left = shift_amount > 0;
    uint64_t shift_abs_u64 = shift_left ? (uint64_t)shift_amount : bsts_abs_i64(shift_amount);

    // Perform shifting on operand.words
    if (shift_left) {
        // Left shift
        uint64_t word_shift_u64 = shift_abs_u64 / 32U;
        size_t bit_shift = (size_t)(shift_abs_u64 % 32U);

        if (word_shift_u64 > (uint64_t)(SIZE_MAX - l_len - 1U)) {
            free(l_twos);
            perror("shift amount too large in bsts_integer_shift_left");
            abort();
        }
        size_t word_shift = (size_t)word_shift_u64;

        size_t new_len = l_len + word_shift + 1; // +1 for possible carry
        if (new_len > (SIZE_MAX / sizeof(uint32_t))) {
            free(l_twos);
            perror("shift allocation too large in bsts_integer_shift_left");
            abort();
        }
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            free(l_twos);
            perror("failed to calloc new_words in bsts_integer_shift_left");
            abort();
        }

        // Shift bits
        uint64_t carry = 0;

        for (size_t i = 0; i < l_len; i++) {
            uint64_t shifted = ((uint64_t)l_twos[i] << bit_shift) | carry;
            new_words[i + word_shift] = (uint32_t)(shifted & 0xFFFFFFFF);
            carry = shifted >> 32;
        }
        // make sure the top bits are negative
        uint32_t high_bits =
            bsts_integer_lt_zero(l)
                ? ((UINT32_C(0xffffffff) >> bit_shift) << bit_shift)
                : UINT32_C(0x00000000);
        new_words[l_len + word_shift] = ((uint32_t)carry) | high_bits;

        free(l_twos);
        return bsts_integer_from_twos(new_len, new_words);
    } else {
        // Right shift
        uint64_t word_shift_u64 = shift_abs_u64 / 32U;
        size_t bit_shift = (size_t)(shift_abs_u64 % 32U);

        if (word_shift_u64 >= l_len) {
            // All bits are shifted out
            if (bsts_integer_lt_zero(l)) {
                // Negative number, result is -1
                free(l_twos);
                return bsts_integer_from_int(-1);
            } else {
                // Positive number, result is 0
                free(l_twos);
                return bsts_integer_from_int(0);
            }
        }

        size_t word_shift = (size_t)word_shift_u64;
        size_t new_len = l_len - word_shift;
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            free(l_twos);
            perror("failed to calloc new_words in bsts_integer_shift_left");
            abort();
        }

        _Bool operand_sign = bsts_integer_lt_zero(l);
        uint32_t sign_extension =
            operand_sign ? UINT32_C(0xffffffff) : UINT32_C(0x00000000);

        for (size_t i = 0; i < new_len; i++) {
            uint64_t high = (i + word_shift + 1 < l_len) ? l_twos[i + word_shift + 1] : sign_extension;
            uint64_t low = l_twos[i + word_shift];
            uint64_t combined = (high << 32) | low;
            new_words[i] = (uint32_t)((combined >> bit_shift) & 0xFFFFFFFF);
        }

        free(l_twos);
        return bsts_integer_from_twos(new_len, new_words);
    }
}

// Function to shift a BValue left or right
BValue bsts_integer_shift_left(BValue l, BValue r) {
    // Check if r is a small integer
    if (!IS_SMALL(r)) {
        // r is not a small integer, return NULL
        // TODO: it could be a small value encoded as a big number
        // or it could be negative which we could handle, since it may
        // be just resulting in zero
        perror("non-small left shift");
        abort();
    }

    // Get the shift amount
    int64_t shift_amount = GET_SMALL_INT(r);

    // If shift_amount is zero, return l as is
    if (shift_amount == 0) {
        return l;
    }
    return bsts_integer_shift_twos(l, shift_amount);
}

// Number of bits differing from the sign bit in two's complement representation.
BValue bsts_integer_popcount(BValue v) {
    uint64_t total = 0U;
    if (IS_SMALL(v)) {
      int64_t small = GET_SMALL_INT(v);
      if (small >= 0) {
        total = bsts_popcount_u64((uint64_t)small);
      } else {
        total = bsts_popcount_u64((uint64_t)(~small));
      }
      return bsts_integer_from_uint64(total);
    }

    BSTS_Integer* integer = GET_BIG_INT(v);
    if (!integer->sign) {
      for (size_t i = 0; i < integer->len; i++) {
        total += (uint64_t)bsts_popcount_u32(integer->words[i]);
      }
    } else {
      // For negative values x, popcount(x) = popcount(~x) = popcount((-x) - 1).
      _Bool borrow = 1;
      for (size_t i = 0; i < integer->len; i++) {
        uint32_t word = integer->words[i];
        uint32_t adjusted;
        if (borrow) {
          if (word == 0U) {
            adjusted = UINT32_C(0xffffffff);
          } else {
            adjusted = word - 1U;
            borrow = 0;
          }
        } else {
          adjusted = word;
        }
        total += (uint64_t)bsts_popcount_u32(adjusted);
      }
    }
    return bsts_integer_from_uint64(total);
}

typedef struct {
  BValue div;
  BValue mod;
} BSTS_Int_Div_Mod;

// returns left - prod * right
BValue bsts_integer_diff_prod(BSTS_Int_Operand left, uint64_t prod, BSTS_Int_Operand right) {
    // TODO: we could this in one step and allocate one time
    BValue li = bsts_integer_from_words_copy(1, left.len, left.words);
    BValue ri = bsts_integer_from_words_copy(1, right.len, right.words);
    BValue p = bsts_integer_from_uint64(prod);

    BValue v1 = bsts_integer_negate(bsts_integer_times(p, ri));
    BValue result = bsts_integer_add(li, v1);
    return result;
}

BSTS_Int_Div_Mod bsts_integer_search_div_mod(BSTS_Int_Operand left, BSTS_Int_Operand right, uint64_t low, uint64_t high) {
    while (1) {
      uint64_t mid = (high >> 1) + (low >> 1);
      if ((high & low) & 1) {
        // both lowest bits are 0
        mid = mid + 1;
      }
      BValue mod = bsts_integer_diff_prod(left, mid, right);
      //println(s"with l = $l, r = $r search($low, $high) gives mid = $mid, c = $c")
      if (bsts_integer_lt_zero(mod)) {
        // mid is too big
        high = mid;
      }
      else {
        BSTS_Int_Operand mod_op;
        uint32_t mod_buffer[2];
        bsts_integer_load_op(mod, mod_buffer, &mod_op);
        int cmp_mod_r = compare_abs(mod_op.len, mod_op.words, right.len, right.words);
        if (cmp_mod_r >= 0) {
          // mid is too small
          low = mid;
        }
        else {
          // 0 < mod < r
          // the div result fits in 2 words
          BValue div = bsts_integer_from_uint64(mid);
          BSTS_Int_Div_Mod result = { div, mod };
          return result;
        }
      }
    }

    // this is unreachable
    BSTS_Int_Div_Mod result = { 0, 0 };
    return result;
}

// (&Integer, &Integer) -> (Integer, Integer)
// required invariants: 1 < r_op < l_op < (r_op << 32)
BSTS_Int_Div_Mod bsts_integer_divmod_top(BSTS_Int_Operand l_op, BSTS_Int_Operand r_op) {
    // we keep the top most word of n and divide
    // since we know that r > 0, r_op.len > 0
    uint64_t right_top = (uint64_t)r_op.words[r_op.len - 1];
    uint64_t llow = (uint64_t)(l_op.words[r_op.len - 1]); 
    uint64_t lhigh = (uint64_t)((l_op.len > r_op.len) ? l_op.words[r_op.len] : 0);
    uint64_t left_top = (lhigh << 32) | llow;
    // this division can be done with unsigned longs
    uint64_t div_guess = left_top / right_top;
    BValue mod = bsts_integer_diff_prod(l_op, div_guess, r_op);
    if (bsts_integer_lt_zero(mod)) {
      // divGuess is too big, we know right_top + 1 can't overflow
      return bsts_integer_search_div_mod(l_op, r_op, left_top / (right_top + 1), div_guess);
    }
    else {
      BSTS_Int_Operand mod_op;
      uint32_t mod_buffer[2];
      bsts_integer_load_op(mod, mod_buffer, &mod_op);
      int cmp_mod_r = compare_abs(mod_op.len, mod_op.words, r_op.len, r_op.words);
      if (cmp_mod_r > 0) {
        // divGuess is too small
        uint64_t upper;
        if (left_top < UINT64_MAX) {
          upper = (left_top + 1) / right_top;
        }
        else {
          // left_top + 1 will overflow, so instead we do
          // (left_top >> 1) + 1 / (right_top >> 1)
          // we know that right_top > 1, so the denominator is never zero
          upper = ((left_top >> 1) + 1) / (right_top >> 1);
        }
        return bsts_integer_search_div_mod(l_op, r_op, div_guess, upper);
      }
      else {
        // it's good
        BValue div = bsts_integer_from_uint64(div_guess);
        BSTS_Int_Div_Mod result = { div, mod };
        return result;
      }
    }
}

// (&Integer, &Integer) -> (Integer, Integer)
BSTS_Int_Div_Mod bsts_integer_divmod_pos(BSTS_Int_Operand l_op, BSTS_Int_Operand r_op) {
  // this is shift right compare
  int cmp_l1_r = compare_abs(l_op.len - 1, l_op.words + 1, r_op.len, r_op.words);
  if (cmp_l1_r >= 0) {
    // we can use the recursive algo:
    BSTS_Int_Operand l1 = { l_op.len - 1, l_op.sign, l_op.words + 1 };
    BSTS_Int_Div_Mod div_mod_1 = bsts_integer_divmod_pos(l1, r_op);
    // we know that d1 >= 1, because l1 >= r
    // l = l1 * b + l0
    // l1 = d1 * r + m1
    // l = (d1 * r + m1) * b + l0
    // l = d1 * r * b + (m1 * b + l0)
    // l0 is the low 32 bits of l
    // we know that m1 * b + l0 < l
    BValue d1 = div_mod_1.div;
    BValue m1 = div_mod_1.mod;
    // this must be a big integer if it isn't zero
    BValue m1_shift = bsts_integer_shift_left(m1, bsts_integer_from_int(32));
    // next_left = (m1 << 32) | l0
    uint32_t next_left_temp[2];
    BSTS_Int_Operand next_left_operand;
    bsts_integer_load_op(m1_shift, next_left_temp, &next_left_operand);
    // we own m1_shift so we can modify the words
    next_left_operand.words[0] = l_op.words[0];

    BSTS_Int_Div_Mod div_mod_next = bsts_integer_divmod_pos(next_left_operand, r_op);
    // we are done
    // m1 * b + l0 == md1 * r + mm1
    // l = d1 * r * b + md1 * r + mm1 = (d1 * b + md1) * r + mm1
    // TODO this is next_div += (d1 << 32), and we own d1 and next_div
    // to we could reuse the memory if we are careful
    BValue div_shift = bsts_integer_shift_left(d1, bsts_integer_from_int(32));
    BValue div = bsts_integer_add(div_shift, div_mod_next.div);
    // we are done with div_shift and div_mod_next.div now
    div_mod_next.div = div;
    // but we don't need to do this
    // div_mod_next.mod = div_mod_next.mod;
    return div_mod_next;
  }
  else {
    int cmp_l_r = compare_abs(l_op.len, l_op.words, r_op.len, r_op.words);
    if (cmp_l_r > 0) {
      // l > r but l < (r << 32)
      return bsts_integer_divmod_top(l_op, r_op);
    }
    else if (cmp_l_r < 0) {
      // l < r
      BSTS_Int_Div_Mod res = {
        bsts_integer_from_int(0),
        bsts_integer_from_words_copy(!l_op.sign, l_op.len, l_op.words)
      };
      return res;
    }
    else {
      // l == n
      BSTS_Int_Div_Mod res = { bsts_integer_from_int(1), bsts_integer_from_int(0) };
      return res;
    }
  }
}

_Bool bsts_integer_is_zero(BValue v) {
  return bsts_integer_cmp_zero(v) == 0;
}

int bsts_integer_cmp_zero(BValue v) {
  if (IS_SMALL(v)) {
    int64_t small = GET_SMALL_INT(v);
    return (small > 0) - (small < 0);
  }

  // Canonical integers never heap-allocate zero or any small-magnitude value.
  BSTS_Integer* integer = GET_BIG_INT(v);
  return integer->sign ? -1 : 1;
}

// (&Integer, &Integer) -> (Integer, Integer)
// div_mod(l, r) == (d, m) <=> l = r * d + m
BValue bsts_integer_div_mod(BValue l, BValue r) {
    _Bool r_is_small = IS_SMALL(r);
    if (r_is_small) {
      int64_t rs = GET_SMALL_INT(r);
      if (rs == 0) {
        // we define division by zero as (0, l)
        return alloc_struct2(bsts_integer_from_int(0), l);
      }
      if (rs == 1) {
        return alloc_struct2(l, bsts_integer_from_int(0));
      }
      if (rs == -1) {
        return alloc_struct2(
          bsts_integer_negate(l),
          bsts_integer_from_int(0));
      }
      if (IS_SMALL(l)) {
        // normal integer division works
        int64_t ls = GET_SMALL_INT(l);
        // C rounds to 0, but bosatsu and python round to -inf
        int64_t div = ls / rs;
        int64_t mod = ls % rs;
        _Bool lpos = ls >= 0;
        _Bool rpos = rs >= 0;
        if ((mod != 0) & (lpos ^ rpos)) {
          // need to repair, mod != 0 & at least one is negative
          // div * r + mod == l, but we may need to shift r towards zero
          // when we shift div = (div - 1) so mod = mod + r
          div = div - 1;
          mod = mod + rs;
        }
        return alloc_struct2(bsts_integer_from_int64(div), bsts_integer_from_int64(mod));
      }
    }
    if (bsts_integer_is_zero(r)) {
        // we define division by zero as (0, l)
        return alloc_struct2(bsts_integer_from_int(0), l);
    }
    // TODO: we could handle the special case of r = 2^n with bit shifting

    // the general case is below
    uint32_t left_temp[2];
    uint32_t right_temp[2];
    BSTS_Int_Operand left_operand;
    BSTS_Int_Operand right_operand;

    // Prepare left operand
    bsts_integer_load_op(l, left_temp, &left_operand);
    // keep positive
    _Bool left_neg = left_operand.sign;
    left_operand.sign = 0;
    // Prepare right operand
    bsts_integer_load_op(r, right_temp, &right_operand);
    // keep positive
    _Bool right_neg = right_operand.sign;
    right_operand.sign = 0;
    BSTS_Int_Div_Mod divmod = bsts_integer_divmod_pos(left_operand, right_operand);
    // now we need to 
    BValue div = divmod.div;
    BValue mod = divmod.mod;
    if (!bsts_integer_is_zero(mod)) {
      if (!left_neg) {
        if (!right_neg) {
          // l = d r + m
        }
        else {
          // l = dr + m
          // l = (-d)(-r) + m - r + r
          // l = -(d + 1) (-r) + (m + (-r))
          BValue div1 = bsts_integer_negate(bsts_integer_add(div, bsts_integer_from_int(1))); 
          div = div1;
          BValue mod1 = bsts_integer_add(mod, r);
          mod = mod1;
        }
      }
      else {
        if (!right_neg) {
          // -l = (-d - 1) r + (r - m)
          BValue div1 = bsts_integer_negate(bsts_integer_add(div, bsts_integer_from_int(1))); 
          div = div1;
          BValue neg_mod = bsts_integer_negate(mod);
          BValue mod1 = bsts_integer_add(r, neg_mod);
          mod = mod1;
        }
        else {
          //  l = d * r + m
          // -l = d (-r) + -m
          mod = bsts_integer_negate(mod);
        }
      }
    }
    else {
      // mod == 0
      if (right_neg ^ left_neg) {
        div = bsts_integer_negate(div);
      }
    }
    return alloc_struct2(div, mod);
}

void free_statics() {
  BSTS_OBJ* rc;
  do {
    rc = pop();
    if (rc == NULL) return;
  } while(1);
}

/**
 * This may build twice in concurrency situations but recall Bosatsu is a statically typed
 * and pure language, so running a constructor twice is never a problem, it just creates
 * some extra garbage to collect.
 */
BValue read_or_build(_Atomic BValue* target, BConstruct cons) {
    BValue result = atomic_load(target);
    if (result == BSTS_BVALUE_NULL) {
        result = cons();
        BValue expected = BSTS_BVALUE_NULL;
        do {
            if (atomic_compare_exchange_weak(target, &expected, result)) {
                free_on_close(result);
                break;
            } else {
                expected = atomic_load(target);
                if (expected != BSTS_BVALUE_NULL) {
                    result = expected;
                    break;
                }
            }
        } while (1);
    }
    return result;
}

/*
typedef struct BSTS_Test_Result {
  char* package_name;
  int passes;
  int fails;
  uint64_t elapsed_nanos;
} BSTS_Test_Result;

enum Test:
  Assertion(value: Bool, message: String)
  TestSuite(name: String, tests: List[Test])

  test_value() returns a Test
*/
typedef struct BSTS_PassFail {
  int passes;
  int fails;
} BSTS_PassFail;

static uint64_t bsts_monotonic_nanos() {
#if defined(CLOCK_MONOTONIC)
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    return UINT64_C(0);
  }
  if (ts.tv_sec < 0 || ts.tv_nsec < 0) {
    return UINT64_C(0);
  }
  return ((uint64_t)ts.tv_sec * UINT64_C(1000000000)) + (uint64_t)ts.tv_nsec;
#else
  return UINT64_C(0);
#endif
}

static uint64_t bsts_elapsed_nanos_since(uint64_t start_nanos) {
  uint64_t end_nanos = bsts_monotonic_nanos();
  if (start_nanos == UINT64_C(0) || end_nanos == UINT64_C(0) || end_nanos < start_nanos) {
    return UINT64_C(0);
  }
  return end_nanos - start_nanos;
}

static double bsts_nanos_to_seconds(uint64_t elapsed_nanos) {
  return (double)elapsed_nanos / 1000000000.0;
}

void print_indent(int indent) {
  for(int z = 0; z < indent; z++) {
    printf(" ");
  }
}

static void bsts_print_test_summary(int indent, int passes, int fails) {
  print_indent(indent);
  if (fails == 0) {
    printf("passed: \033[32m%i\033[0m\n", passes);
  }
  else {
    printf("passed: \033[32m%i\033[0m, failed: \033[31m%i\033[0m\n", passes, fails);
  }
}

_Bool bsts_test_argv_has_quiet(int argc, char** argv) {
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--quiet") == 0) {
      return 1;
    }
  }
  return 0;
}

BSTS_PassFail bsts_check_test(BValue v, int indent) {
  int passes = 0;
  int fails = 0;
  if (get_variant(v) == 0) {
    _Bool success = get_variant(get_enum_index(v, 0));
    if (!success) {
      BValue message = get_enum_index(v, 1);
      print_indent(indent);
      // red failure
      printf("\033[31mfailure: ");
      bsts_string_println(message);
      printf("\033[0m");
      ++fails;
    }
    else {
      ++passes;
    }
  }
  else {
    // must be the (Suite, List[Test])
    BValue suite_name = get_enum_index(v, 0);
    BValue suite_tests = get_enum_index(v, 1);
    print_indent(indent);
    bsts_string_print(suite_name);
    printf(":\n");
    // loop through all the children
    int next_indent = indent + 4;
    int this_fails = 0;
    int this_passes = 0;
    while(get_variant(suite_tests) != 0) {
      BValue t1 = get_enum_index(suite_tests, 0);
      suite_tests = get_enum_index(suite_tests, 1);
      BSTS_PassFail tests = bsts_check_test(t1, next_indent);
      this_passes += tests.passes;
      this_fails += tests.fails;
    }
    bsts_print_test_summary(next_indent, this_passes, this_fails);
    passes += this_passes;
    fails += this_fails;
  }

  BSTS_PassFail res = { passes, fails };
  return res;
}

static BSTS_PassFail bsts_count_test(BValue v) {
  int passes = 0;
  int fails = 0;
  if (get_variant(v) == 0) {
    _Bool success = get_variant(get_enum_index(v, 0));
    if (success) {
      ++passes;
    }
    else {
      ++fails;
    }
  }
  else {
    BValue suite_tests = get_enum_index(v, 1);
    while(get_variant(suite_tests) != 0) {
      BValue t1 = get_enum_index(suite_tests, 0);
      suite_tests = get_enum_index(suite_tests, 1);
      BSTS_PassFail tests = bsts_count_test(t1);
      passes += tests.passes;
      fails += tests.fails;
    }
  }

  BSTS_PassFail res = { passes, fails };
  return res;
}

static void bsts_print_test_failures(BValue v, int indent) {
  if (get_variant(v) == 0) {
    _Bool success = get_variant(get_enum_index(v, 0));
    if (!success) {
      BValue message = get_enum_index(v, 1);
      print_indent(indent);
      printf("\033[31mfailure: ");
      bsts_string_println(message);
      printf("\033[0m");
    }
    return;
  }

  BValue suite_name = get_enum_index(v, 0);
  BValue suite_tests = get_enum_index(v, 1);
  _Bool wrote_header = 0;
  int next_indent = indent + 4;
  while(get_variant(suite_tests) != 0) {
    BValue t1 = get_enum_index(suite_tests, 0);
    suite_tests = get_enum_index(suite_tests, 1);
    BSTS_PassFail tests = bsts_count_test(t1);
    if (tests.fails > 0) {
      if (!wrote_header) {
        print_indent(indent);
        bsts_string_print(suite_name);
        printf(":\n");
        wrote_header = 1;
      }
      bsts_print_test_failures(t1, next_indent);
    }
  }
}

static BSTS_Test_Result bsts_test_run_value(
    char* package_name,
    BValue res,
    _Bool quiet,
    uint64_t elapsed_nanos
) {
  printf("%s: %.3fs\n", package_name, bsts_nanos_to_seconds(elapsed_nanos));

  BSTS_PassFail this_test;
  if (quiet) {
    this_test = bsts_count_test(res);
    if (this_test.fails > 0) {
      bsts_print_test_failures(res, 4);
    }
  }
  else {
    this_test = bsts_check_test(res, 4);
    if (get_variant(res) == 0) {
      bsts_print_test_summary(4, this_test.passes, this_test.fails);
    }
  }

  BSTS_Test_Result test_res = {
    package_name,
    this_test.passes,
    this_test.fails,
    elapsed_nanos
  };
  return test_res;
}

BSTS_Test_Result bsts_test_run(
    char* package_name,
    BConstruct test_value,
    _Bool quiet
) {
  uint64_t start_nanos = bsts_monotonic_nanos();
  BValue res = test_value();
  uint64_t elapsed_nanos = bsts_elapsed_nanos_since(start_nanos);
  return bsts_test_run_value(package_name, res, quiet, elapsed_nanos);
}

BSTS_Test_Result bsts_test_run_prog(
    char* package_name,
    BConstruct test_value,
    _Bool quiet
) {
  uint64_t start_nanos = bsts_monotonic_nanos();
  BSTS_Prog_Test_Result prog_result = bsts_Bosatsu_Prog_run_test(test_value());
  uint64_t elapsed_nanos = bsts_elapsed_nanos_since(start_nanos);
  if (prog_result.is_error) {
    BSTS_PassFail failed = { 0, 1 };
    printf("%s: %.3fs\n", package_name, bsts_nanos_to_seconds(elapsed_nanos));
    print_indent(4);
    printf("\033[31mfailure: ProgTest raised an uncaught error\033[0m\n");
    if (!quiet) {
      bsts_print_test_summary(4, failed.passes, failed.fails);
    }

    BSTS_Test_Result test_res = {
      package_name,
      failed.passes,
      failed.fails,
      elapsed_nanos
    };
    return test_res;
  }

  return bsts_test_run_value(
      package_name,
      prog_result.value,
      quiet,
      elapsed_nanos
  );
}

int bsts_test_result_print_summary(int count, BSTS_Test_Result* results) {
  int total_fails = 0;
  int total_passes = 0;
  uint64_t total_elapsed_nanos = UINT64_C(0);
  for (int i = 0; i < count; i++) {
    total_fails += results[i].fails;
    total_passes += results[i].passes;
    if (UINT64_MAX - total_elapsed_nanos < results[i].elapsed_nanos) {
      total_elapsed_nanos = UINT64_MAX;
    } else {
      total_elapsed_nanos += results[i].elapsed_nanos;
    }
  }

  if (total_fails > 0) {
    printf("\n\npackages with failures:\n");
    for (int i = 0; i < count; i++) {
      if (results[i].fails > 0) {
        printf("\t%s\n", results[i].package_name);
      }
    }
    printf("\n");
  }

  const char* fail_color = total_fails > 0 ? "\033[31m" : "";
  const char* fail_reset = total_fails > 0 ? "\033[0m" : "";
  int total_tests = total_passes + total_fails;
  const char* test_word = (total_tests == 1) ? "test" : "tests";
  printf(
      "\n%i %s, \033[32m%i passed\033[0m %s%i failed%s in %.3fs\n",
      total_tests,
      test_word,
      total_passes,
      fail_color,
      total_fails,
      fail_reset,
      bsts_nanos_to_seconds(total_elapsed_nanos)
  );
  return (total_fails > 0);
}
