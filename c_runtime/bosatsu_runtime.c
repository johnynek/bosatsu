#include "bosatsu_runtime.h"

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include "gc.h"

#include <assert.h>

_Static_assert(sizeof(void*) == 8, "Bosatsu runtime currently requires 64-bit pointers");
_Static_assert(sizeof(uintptr_t) == 8, "Bosatsu runtime assumes 64-bit uintptr_t");

/*
There are a few kinds of values:

1. pure values: small ints, characters, small strings that can fit into 63 bits.
2. pointers to gc'ed values

to distinguish these cases we allocate pointers such that they are aligned to at least 4 byte
boundaries:
  a. ends with 01: pure value
  b. ends with 00: gc pointer.

when it comes to functions there are two types, PureFn and closures. We have to box
  pointers to them, but when we know we have a global PureFn we can directly call it.
  if we have a static boxed value, it ends in 1, else 0.

Nat-like values are represented by positive integers encoded as PURE_VALUE such that
NAT(x) = (x << 2) | 1, since we don't have enough time to increment through 2^{62} values
this is a safe encoding.

Char values are stored as unicode code points with a trailing 1.

String values encodings, string values are like ref-counted structs with
a length, a base pointer, and an offset into the utf-8 bytes. We could also potentially optimize
short strings by packing them literally into 63 bits with a length.

Integer values are either pure values (signed values packed into 63 bits),
or gced big integers
*/
#define TAG_MASK ((uintptr_t)0x3)
#define PURE_VALUE_TAG ((uintptr_t)0x1)
#define POINTER_TAG ((uintptr_t)0x0)

static inline _Bool bsts_is_pure_value(BValue value) {
  return (value & TAG_MASK) == PURE_VALUE_TAG;
}

static inline BValue bsts_to_pure_value(uintptr_t value) {
  return (BValue)((value << 2) | PURE_VALUE_TAG);
}

static inline uintptr_t bsts_pure_value(BValue value) {
  return value >> 2;
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
  return TO_PURE_VALUE((uintptr_t)codepoint);
}

int bsts_char_code_point_from_value(BValue ch) {
  return (int)PURE_VALUE(ch);
}

BValue bsts_float64_from_bits(uint64_t bits) {
  return (BValue)bits;
}

uint64_t bsts_float64_to_bits(BValue v) {
  return (uint64_t)v;
}

BValue bsts_float64_from_double(double d) {
  union {
    double d;
    uint64_t u;
  } conv;
  conv.d = d;
  return bsts_float64_from_bits(conv.u);
}

double bsts_float64_to_double(BValue v) {
  union {
    double d;
    uint64_t u;
  } conv;
  conv.u = bsts_float64_to_bits(v);
  return conv.d;
}

_Bool bsts_float64_equals(BValue left, BValue right) {
  double l = bsts_float64_to_double(left);
  double r = bsts_float64_to_double(right);
  if (isnan(l) && isnan(r)) return 1;
  return l == r;
}

int bsts_float64_cmp_total(BValue left, BValue right) {
  double l = bsts_float64_to_double(left);
  double r = bsts_float64_to_double(right);
  _Bool l_nan = isnan(l);
  _Bool r_nan = isnan(r);
  if (l_nan) {
    if (r_nan) {
      return 0;
    }
    return -1;
  }
  if (r_nan) {
    return 1;
  }
  if (l < r) return -1;
  if (l > r) return 1;
  return 0;
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

DEFINE_BSTS_OBJ(External, void* external;);
DEFINE_BSTS_OBJ(BSTS_String, size_t len; size_t offset; char* bytes;);
DEFINE_BSTS_OBJ(BSTS_Integer, size_t len; _Bool sign; uint32_t words[];);

typedef struct {
    size_t len;
    _Bool sign;
    uint32_t* words;
} BSTS_Int_Operand;

// Helper macros and functions
#define IS_SMALL(v) IS_PURE_VALUE(v)
#define GET_SMALL_INT(v) (int32_t)(PURE_VALUE(v))
#define GET_BIG_INT(v) BSTS_PTR(BSTS_Integer, (v))

void bsts_load_op_from_small(int32_t value, uint32_t* words, BSTS_Int_Operand* op) {
    op->sign = value < 0;
    op->words = words;
    int64_t bigger = (int64_t)value;
    int64_t abs_bigger = (bigger < 0 ? -bigger : bigger);
    uint32_t low = (uint32_t)(abs_bigger & 0xFFFFFFFF);
    uint32_t high = (uint32_t)((abs_bigger >> 32) & 0xFFFFFFFF);
    if (high == 0) {
      words[0] = low;
      op->len = 1;
    }
    else {
      // TODO: this branch should be unreachable, remove it
      words[0] = low;
      words[1] = high;
      op->len = 2;
    }
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
  if (v != BSTS_BVALUE_NULL && GC_base(bsts_bvalue_to_ptr(v)) != NULL) {
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
void free_external(void* ex, void* data) {
  FreeFn ex_free = (FreeFn)data;
  ex_free(ex);
}

BValue alloc_external(void* data, FreeFn free) {
    External* ext = GC_malloc(sizeof(External));
    ext->external = data;
    GC_register_finalizer(ext, free_external, free, NULL, NULL);
    return BSTS_VALUE_FROM_PTR(ext);
}

void* get_external(BValue v) {
  // Externals can be static also, top level external values
  External* ext = BSTS_PTR(External, v);
  return ext->external;
}

static char bsts_empty_string_bytes[1] = {0};

static inline char* bsts_string_data_ptr(BSTS_String* str) {
  if (str->len == 0) return bsts_empty_string_bytes;
  return str->bytes + str->offset;
}

BValue bsts_string_mut(size_t len) {
  BSTS_String* str = GC_malloc(sizeof(BSTS_String));
  if (str == NULL) {
      perror("failed to GC_malloc in bsts_string_mut");
      abort();
  }
  str->len = len;
  str->offset = 0;
  if (len == 0) {
    str->bytes = bsts_empty_string_bytes;
    return BSTS_VALUE_FROM_PTR(str);
  }
  char* bytes = GC_malloc_atomic(sizeof(char) * len);
  if (bytes == NULL) {
      perror("failed to GC_malloc in bsts_string_mut");
      abort();
  }
  str->bytes = bytes;
  return BSTS_VALUE_FROM_PTR(str);
}

// this copies the bytes in, it does not take ownership
BValue bsts_string_from_utf8_bytes_copy(size_t len, char* bytes) {
  BSTS_String* str = BSTS_PTR(BSTS_String, bsts_string_mut(len));
  if (len > 0) {
    memcpy(str->bytes, bytes, len);
  }
  return BSTS_VALUE_FROM_PTR(str);
}


BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes) {
  BSTS_String* str = GC_malloc_atomic(sizeof(BSTS_String));
  str->len = len;
  str->offset = 0;
  if (len == 0 && bytes == NULL) {
    str->bytes = bsts_empty_string_bytes;
  }
  else {
    str->bytes = bytes;
  }
  return BSTS_VALUE_FROM_PTR(str);
}

BValue bsts_string_from_utf8_bytes_static_null_term(char* bytes) {
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

#define GET_STRING(v) BSTS_PTR(BSTS_String, (v))

_Bool bsts_string_equals(BValue left, BValue right) {
  if (left == right) {
    return 1;
  }

  BSTS_String* lstr = GET_STRING(left);
  BSTS_String* rstr = GET_STRING(right);

  size_t llen = lstr->len;
  if (llen == rstr->len) {
    char* lbytes = bsts_string_data_ptr(lstr);
    char* rbytes = bsts_string_data_ptr(rstr);
    return (strncmp(
      lbytes,
      rbytes,
      llen) == 0);
  }
  else {
    return 0;
  }
}

int bsts_string_cmp(BValue left, BValue right) {
  if (left == right) {
    return 0;
  }

  BSTS_String* lstr = GET_STRING(left);
  BSTS_String* rstr = GET_STRING(right);

  size_t llen = lstr->len;
  size_t rlen = rstr->len;
  size_t min_len = (llen <= rlen) ? llen : rlen;
  char* lbytes = bsts_string_data_ptr(lstr);
  char* rbytes = bsts_string_data_ptr(rstr);
  int cmp = strncmp(lbytes, rbytes, min_len);

  if (cmp == 0) {
    return (llen < rlen) ? -1 : ((llen > rlen) ? 1 : 0);
  }
  else {
    return cmp;
  }
}

size_t bsts_string_utf8_len(BValue str) {
  BSTS_String* strptr = GET_STRING(str);
  return strptr->len;
}

char* bsts_string_utf8_bytes(BValue str) {
  BSTS_String* strptr = GET_STRING(str);
  return bsts_string_data_ptr(strptr);
}

int bsts_utf8_code_point_bytes(const char* utf8data, int offset, int len) {
    if (utf8data == NULL || offset < 0 || offset >= len) {
        // Invalid input
        return -1;
    }

    // cast to an unsigned char for the math below
    unsigned char *s = (unsigned char*)(utf8data + offset);
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

/**
 * return the number of bytes at this position, 1, 2, 3, 4 or -1 on error
 * TODO: the runtime maybe should assume everything is safe, which the
 * compiler should have guaranteed, so doing error checks here is probably
 * wasteful once we debug the compiler.
 */
int bsts_string_code_point_bytes(BValue value, int offset) {
    BSTS_String* str = GET_STRING(value);
    if (str == NULL) {
        return -1;
    }
    char* bytes = bsts_string_data_ptr(str);
    return bsts_utf8_code_point_bytes(bytes, offset, str->len);
}

/**
 * return char at the given offset
 * TODO: the runtime maybe should assume everything is safe, which the
 * compiler should have guaranteed, so doing error checks here is probably
 * wasteful once we debug the compiler.
 */
BValue bsts_string_char_at(BValue value, int offset) {
    BSTS_String* str = GET_STRING(value);
    if (str == NULL || offset < 0 || offset >= str->len) {
        // Invalid input
        return 0;
    }

    // cast to an unsigned char for the math below
    char* bytes = bsts_string_data_ptr(str);
    unsigned char *s = (unsigned char*)(bytes + offset);
    unsigned char c = s[0];
    int remaining = str->len - offset;
    uint32_t code_point = 0;

    if (c <= 0x7F) {
        // 1-byte sequence (ASCII)
        code_point = c;
    } else if ((c & 0xE0) == 0xC0) {
        // 2-byte sequence
        if (remaining < 2 || (s[1] & 0xC0) != 0x80) {
            // Invalid continuation byte
            return 0;
        }
        code_point = ((c & 0x1F) << 6) | (s[1] & 0x3F);
    } else if ((c & 0xF0) == 0xE0) {
        // 3-byte sequence
        if (remaining < 3 || (s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80) {
            // Invalid continuation bytes
            return 0;
        }
        code_point = ((c & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F);
    } else if ((c & 0xF8) == 0xF0) {
        // 4-byte sequence
        if (remaining < 4 || (s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80 || (s[3] & 0xC0) != 0x80) {
            // Invalid continuation bytes
            return 0;
        }
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
  BSTS_String* str = GET_STRING(value);
  size_t len = str->len;
  if (start < 0 || end < start || (size_t)end > len) {
    // this is invalid
    return 0;
  }
  size_t new_len = end - start;
  if (new_len == 0) {
    // empty string, should probably be a constant
    return bsts_string_from_utf8_bytes_static(0, "");
  }
  else {
    // Keep the base pointer and track the offset to avoid copying.
    BSTS_String* res = GC_malloc(sizeof(BSTS_String));
    if (res == NULL) {
      perror("failed to GC_malloc in bsts_string_substring");
      abort();
    }
    res->len = new_len;
    res->offset = str->offset + (size_t)start;
    res->bytes = str->bytes;
    return BSTS_VALUE_FROM_PTR(res);
  }
}

// (String, int) -> String
BValue bsts_string_substring_tail(BValue value, int byte_offset) {
  return bsts_string_substring(value, byte_offset, (int)bsts_string_utf8_len(value));
}

int bsts_string_find(BValue haystack, BValue needle, int start) {
    BSTS_String* haystack_str = GET_STRING(haystack);
    BSTS_String* needle_str = GET_STRING(needle);

    size_t haystack_len = haystack_str->len;
    size_t needle_len = needle_str->len;
    char* haystack_bytes = bsts_string_data_ptr(haystack_str);
    char* needle_bytes = bsts_string_data_ptr(needle_str);
    if (needle_len == 0) {
        // Empty needle matches at start
        return (start <= (int)haystack_len) ? start : -1;
    }

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
    BSTS_String* h = GET_STRING(haystack);
    BSTS_String* n = GET_STRING(needle);

    size_t hlen = h->len;
    size_t nlen = n->len;
    char* hbytes = bsts_string_data_ptr(h);
    char* nbytes = bsts_string_data_ptr(n);

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
  char* bytes = bsts_string_utf8_bytes(v);
  size_t len = bsts_string_utf8_len(v);
  // TODO: if this string is somehow too big for an int this may fail
  printf("%.*s\n", (int)len, bytes);
}

void bsts_string_print(BValue v) {
  char* bytes = bsts_string_utf8_bytes(v);
  size_t len = bsts_string_utf8_len(v);
  // TODO: if this string is somehow too big for an int this may fail
  printf("%.*s", (int)len, bytes);
}

BValue bsts_integer_from_int(int32_t small_int) {
    return TO_PURE_VALUE(small_int);
}

int32_t bsts_integer_to_int32(BValue bint) {
    if (IS_SMALL(bint)) {
        return GET_SMALL_INT(bint);
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

BValue bsts_maybe_small_int(_Bool pos, uint32_t small_result);

BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words) {
    // remove any leading 0 words
    while ((size > 1) && (words[size - 1] == 0)) {
      size--;
    }
    if (size == 1) {
      BValue maybe = bsts_maybe_small_int(is_pos, words[0]);
      if (maybe) return maybe;
    }
    BSTS_Integer* integer = bsts_integer_alloc(size);
    integer->sign = !is_pos; // sign: 0 for positive, 1 for negative
    memcpy(integer->words, words, size * sizeof(uint32_t));
    return BSTS_VALUE_FROM_PTR(integer);
}

BValue bsts_integer_from_int64(int64_t result) {
  // Check if result fits in small integer
  if ((INT32_MIN <= result) && (result <= INT32_MAX)) {
      return bsts_integer_from_int((int32_t)result);
  } else {
      // Promote to big integer
      _Bool is_positive = result >= 0;
      uint64_t abs_result = is_positive ? result : -result;
      uint32_t low = (uint32_t)(abs_result & 0xFFFFFFFF);
      uint32_t high = (uint32_t)((abs_result >> 32) & 0xFFFFFFFF);
      if (high == 0) {
        BSTS_Integer* result = bsts_integer_alloc(1);
        result->sign = !is_positive;
        result->words[0] = low;
        return BSTS_VALUE_FROM_PTR(result);
      }
      else {
        BSTS_Integer* result = bsts_integer_alloc(2);
        result->sign = !is_positive;
        result->words[0] = low;
        result->words[1] = high;
        return BSTS_VALUE_FROM_PTR(result);
      }
  }
}

BValue bsts_integer_from_uint64(uint64_t result) {
  // Check if result fits in small integer
  if (result <= INT32_MAX) {
      return bsts_integer_from_int((int32_t)result);
  } else {
      // Promote to big integer
      uint32_t low = (uint32_t)(result & 0xFFFFFFFF);
      uint32_t high = (uint32_t)((result >> 32) & 0xFFFFFFFF);
      if (high == 0) {
        BSTS_Integer* result = bsts_integer_alloc(1);
        result->sign = 0;
        result->words[0] = low;
        return BSTS_VALUE_FROM_PTR(result);
      }
      else {
        BSTS_Integer* result = bsts_integer_alloc(2);
        result->sign = 0;
        result->words[0] = low;
        result->words[1] = high;
        return BSTS_VALUE_FROM_PTR(result);
      }
  }
}

BValue bsts_maybe_small_int(_Bool pos, uint32_t small_result) {
  if (!pos) {
    if (small_result <= 0x80000000) {
      // this fits in int32_t
      if (small_result == 0x80000000) {
        return bsts_integer_from_int(INT32_MIN);
      }
      else {
        return bsts_integer_from_int(-((int32_t)small_result));
      }
    }
    else {
      return BSTS_BVALUE_NULL;
    }
  }
  else if (small_result <= INT32_MAX) {
    // it is a small positive
    return bsts_integer_from_int(((int32_t)small_result));
  }
  else {
    // positive number > INT32_MAX
    return BSTS_BVALUE_NULL;
  }
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
        int32_t small_int_value = GET_SMALL_INT(left);
        BSTS_Integer* big_int = GET_BIG_INT(right);

        // Check sign
        _Bool big_int_sign = big_int->sign; // 0 for positive, 1 for negative
        _Bool small_int_sign = (small_int_value < 0) ? 1 : 0;
        if (big_int_sign != small_int_sign) {
            return 0; // Different signs
        }
        // they are both positive
        if (big_int->len > 1) {
          // the big int is bigger
          return 0;
        }
        if (big_int->len == 0) {
          return small_int_value == 0;
        }
        // else len == 1
        int64_t small64 = (int64_t)small_int_value;
        int64_t big64 = big_int_sign ? -(int64_t)big_int->words[0] : (int64_t)big_int->words[0];

        return big64 == small64;
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

static inline uint64_t bsts_abs_i32(int32_t v) {
  return (v < 0) ? (uint64_t)(-(int64_t)v) : (uint64_t)v;
}

static BValue bsts_integer_add_big_small(BValue bigv, int32_t small) {
    if (small == 0) {
        return bigv;
    }
    BSTS_Integer* big = GET_BIG_INT(bigv);
    _Bool big_sign = big->sign;
    _Bool small_sign = (small < 0);
    uint64_t abs_small = bsts_abs_i32(small);

    if (big_sign == small_sign) {
        // same sign: add magnitudes
        size_t len = big->len;
        size_t result_len = len + 1;
        BSTS_Integer* result = bsts_integer_alloc(result_len);

        uint64_t sum = (uint64_t)big->words[0] + abs_small;
        result->words[0] = (uint32_t)(sum & 0xFFFFFFFF);
        uint64_t carry = sum >> 32;

        for (size_t i = 1; i < len; i++) {
            sum = (uint64_t)big->words[i] + carry;
            result->words[i] = (uint32_t)(sum & 0xFFFFFFFF);
            carry = sum >> 32;
        }
        if (carry) {
            result->words[len] = (uint32_t)carry;
        }

        size_t out_len = carry ? (len + 1) : len;
        while (out_len > 1 && result->words[out_len - 1] == 0) {
            out_len--;
        }
        if (out_len == 1) {
            BValue maybe = bsts_maybe_small_int(!big_sign, result->words[0]);
            if (maybe) return maybe;
        }
        result->len = out_len;
        result->sign = big_sign;
        return BSTS_VALUE_FROM_PTR(result);
    }
    else {
        // different signs: subtract magnitudes
        size_t len = big->len;
        if (len > 1) {
            // big magnitude is larger than abs_small
            BSTS_Integer* result = bsts_integer_alloc(len);
            uint64_t w0 = big->words[0];
            uint64_t diff = w0 - abs_small;
            uint32_t borrow = (w0 < abs_small) ? 1u : 0u;
            result->words[0] = (uint32_t)(diff & 0xFFFFFFFF);

            for (size_t i = 1; i < len; i++) {
                uint32_t w = big->words[i];
                if (borrow) {
                    if (w == 0) {
                        result->words[i] = 0xFFFFFFFFu;
                        borrow = 1;
                    } else {
                        result->words[i] = w - 1;
                        borrow = 0;
                    }
                } else {
                    result->words[i] = w;
                }
            }

            size_t out_len = len;
            while (out_len > 1 && result->words[out_len - 1] == 0) {
                out_len--;
            }
            if (out_len == 1) {
                BValue maybe = bsts_maybe_small_int(!big_sign, result->words[0]);
                if (maybe) return maybe;
            }
            result->len = out_len;
            result->sign = big_sign;
            return BSTS_VALUE_FROM_PTR(result);
        } else {
            uint32_t bw = big->words[0];
            if (bw == abs_small) {
                return bsts_integer_from_int(0);
            }
            if (bw > abs_small) {
                uint32_t diff = (uint32_t)(bw - abs_small);
                BValue maybe = bsts_maybe_small_int(!big_sign, diff);
                if (maybe) return maybe;
                BSTS_Integer* result = bsts_integer_alloc(1);
                result->words[0] = diff;
                result->len = 1;
                result->sign = big_sign;
                return BSTS_VALUE_FROM_PTR(result);
            } else {
                uint32_t diff = (uint32_t)(abs_small - bw);
                _Bool result_sign = small_sign;
                BValue maybe = bsts_maybe_small_int(!result_sign, diff);
                if (maybe) return maybe;
                BSTS_Integer* result = bsts_integer_alloc(1);
                result->words[0] = diff;
                result->len = 1;
                result->sign = result_sign;
                return BSTS_VALUE_FROM_PTR(result);
            }
        }
    }
}

static BValue bsts_integer_mul_big_small(BValue bigv, int32_t small) {
    if (small == 0) return bsts_integer_from_int(0);
    if (small == 1) return bigv;
    if (small == -1) return bsts_integer_negate(bigv);

    BSTS_Integer* big = GET_BIG_INT(bigv);
    size_t len = big->len;
    uint64_t abs_small = bsts_abs_i32(small);
    _Bool result_sign = big->sign ^ (small < 0);

    size_t result_len = len + 1;
    BSTS_Integer* result = bsts_integer_alloc(result_len);
    uint64_t carry = 0;
    for (size_t i = 0; i < len; i++) {
        uint64_t prod = (uint64_t)big->words[i] * abs_small + carry;
        result->words[i] = (uint32_t)(prod & 0xFFFFFFFF);
        carry = prod >> 32;
    }
    result->words[len] = (uint32_t)carry;

    size_t out_len = result_len;
    while (out_len > 1 && result->words[out_len - 1] == 0) {
        out_len--;
    }
    if (out_len == 1) {
        BValue maybe = bsts_maybe_small_int(!result_sign, result->words[0]);
        if (maybe) return maybe;
    }
    result->len = out_len;
    result->sign = result_sign;
    return BSTS_VALUE_FROM_PTR(result);
}

BValue bsts_integer_add(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    // Case 1: Both are small integers
    if (l_is_small && r_is_small) {
        int64_t l_int = (int64_t)GET_SMALL_INT(l);
        int64_t r_int = (int64_t)GET_SMALL_INT(r);
        return bsts_integer_from_int64(l_int + r_int);
    } else if (l_is_small && !r_is_small) {
        return bsts_integer_add_big_small(r, GET_SMALL_INT(l));
    } else if (!l_is_small && r_is_small) {
        return bsts_integer_add_big_small(l, GET_SMALL_INT(r));
    } else if (l == (BValue)PURE_VALUE_TAG) {
        // sub(x, y) is encoded as add(x, negate(y)), and in Bosatsu code
        // -y is encoded as 0 - y. We should have a negate in Predef, but don't currently.
        return r;
    } else {
        // At least one operand is a big integer

        uint32_t left_temp[2];
        uint32_t right_temp[2];
        BSTS_Int_Operand left_operand;
        BSTS_Int_Operand right_operand;

        // Prepare left operand
        bsts_integer_load_op(l, left_temp, &left_operand);
        // Prepare right operand
        bsts_integer_load_op(r, right_temp, &right_operand);

        BValue result = BSTS_BVALUE_NULL;
        if (left_operand.sign == right_operand.sign) {
            // Addition
            _Bool result_sign = left_operand.sign;
            size_t max_len = (left_operand.len > right_operand.len) ? left_operand.len : right_operand.len;
            uint32_t* result_words = (uint32_t*)calloc(max_len + 1, sizeof(uint32_t));
            if (result_words == NULL) {
                perror("failed to alloc result_words in bsts_integer_add");
                abort();
            }

            uint64_t carry = 0;
            size_t i = 0;
            for (; i < max_len; i++) {
                uint64_t left_word = (i < left_operand.len) ? left_operand.words[i] : 0;
                uint64_t right_word = (i < right_operand.len) ? right_operand.words[i] : 0;
                uint64_t sum = left_word + right_word + carry;
                result_words[i] = (uint32_t)(sum & 0xFFFFFFFF);
                carry = sum >> 32;
            }
            if (carry) {
                result_words[i++] = (uint32_t)carry;
            }
            size_t result_len = i;

            // Normalize result
            while (result_len > 1 && result_words[result_len - 1] == 0) {
                result_len--;
            }

            // Check for small integer representation
            if (result_len == 1) {
                BValue maybe_result = bsts_maybe_small_int(!result_sign, result_words[0]);
                if (maybe_result) {
                  result = maybe_result;
                  free(result_words);
                }
                else {
                  result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                  free(result_words);
                }
            } else {
                result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                free(result_words);
            }
        } else {
            // Subtraction
            // (-a) + b if |a| > |b|, then -(a - b)
            // a + (-b) if |a| > |b|, then (a - b)
            // a + (-b) if |a| < |b|, then -(b - a)
            // (-a) + b if |a| < |b|, then (b - a)
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
                uint32_t* result_words = (uint32_t*)calloc(result_len, sizeof(uint32_t));
                if (result_words == NULL) {
                    perror("failed to calloc result_words in bsts_integer_add");
                    abort();
                }

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
                    result_words[i] = (uint32_t)(diff & 0xFFFFFFFF);
                }

                // Normalize result
                while (result_len > 1 && result_words[result_len - 1] == 0) {
                    result_len--;
                }

                // Check for small integer representation
                if (result_len == 1) {
                    BValue maybe_result = bsts_maybe_small_int(!result_sign, result_words[0]);
                    if (maybe_result) {
                      result = maybe_result;
                      free(result_words);
                    }
                    else {
                      result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                      free(result_words);
                    }
                } else {
                    result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                    free(result_words);
                }
            }
        }

        return result;
    }
}

// Function to negate a BValue
BValue bsts_integer_negate(BValue v) {
    if (IS_SMALL(v)) {
        int32_t small = GET_SMALL_INT(v);
        if (small != INT32_MIN) {
            return bsts_integer_from_int(-small);
        } else {
            uint32_t words[1] = { 0x80000000 };
            return bsts_integer_from_words_copy(1, 1, words);
        }
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
        int value = GET_SMALL_INT(v);

        // Convert small integer to string
        char buffer[32]; // Enough for 64-bit integer
        int length = snprintf(buffer, sizeof(buffer), "%d", value);

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
        char* out = bsts_string_data_ptr(res);

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
  size_t slen = bsts_string_utf8_len(v);
  char* bytes = bsts_string_utf8_bytes(v);
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
  BValue bacc = 0;
  while(pos < slen) {
    int32_t digit = (int32_t)(bytes[pos] - '0');
    if ((digit < 0) || (9 < digit)) return alloc_enum0(0);
    if (pos >= 10) {
      if (pos == 10) {
        // we could be overflowing an int32_t at this point
        bacc = bsts_integer_from_int64(((int64_t)acc) * 10L);
      }
      else {
        bacc = bsts_integer_times(bacc, bsts_integer_from_int(10));
      }
      bacc = bsts_integer_add(bacc, bsts_integer_from_int(digit)); 
    }
    else {
      acc = acc * 10 + digit;
    }
    pos++;
  }
  if (slen < 11) {
    // acc should hold the number
    return alloc_enum1(1, bsts_integer_from_int64(sign ? -acc : acc));
  }
  else {
    return alloc_enum1(1, sign ? bsts_integer_negate(bacc) : bacc);
  }
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

void bsts_integer_small_to_twos(int32_t value, uint32_t* target, size_t max_len) {
  memcpy(target, &value, sizeof(int32_t));
  if (value < 0) {
    // fill with -1 all the rest
    for (size_t i = 1; i < max_len; i++) {
      target[i] = 0xFFFFFFFF;
    }
  }
}

BValue bsts_integer_from_twos(size_t max_len, uint32_t* result_twos) {
    // Convert result from two's complement to sign-magnitude
    _Bool result_sign;
    size_t result_len = max_len;
    BSTS_Integer* result = bsts_integer_alloc(max_len);
    twos_complement_to_sign_magnitude(max_len, result_twos, &result_sign, &result_len, result->words);
    free(result_twos);

    // Check if result can be represented as small integer
    if (result_len == 1) {
        // Attempt to pack into small integer
        BValue maybe = bsts_maybe_small_int(!result_sign, result->words[0]);
        if (maybe) {
          return maybe;
        }
    }
    result->len = result_len;
    result->sign = result_sign;
    return BSTS_VALUE_FROM_PTR(result);
}

// Function to perform bitwise AND on two BValues
BValue bsts_integer_and(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (l_is_small & r_is_small) {
      return bsts_integer_from_int(GET_SMALL_INT(l) & GET_SMALL_INT(r));
    }
    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? 1 : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? 1 : (GET_BIG_INT(r)->len + 1);
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

    if (left_is_small & right_is_small) {
        // Both are small integers
        int32_t l_int = GET_SMALL_INT(left);
        int32_t r_int = GET_SMALL_INT(right);
        // Multiply and check for overflow
        int64_t result = (int64_t)l_int * (int64_t)r_int;
        return bsts_integer_from_int64(result);
    } else if (left_is_small && !right_is_small) {
        return bsts_integer_mul_big_small(right, GET_SMALL_INT(left));
    } else if (!left_is_small && right_is_small) {
        return bsts_integer_mul_big_small(left, GET_SMALL_INT(right));
    } else {
        // At least one operand is big integer
        uint32_t left_temp[2];
        uint32_t right_temp[2];
        BSTS_Int_Operand l_operand;
        BSTS_Int_Operand r_operand;

        // Prepare left operand
        bsts_integer_load_op(left, left_temp, &l_operand);
        // Prepare right operand
        bsts_integer_load_op(right, right_temp, &r_operand);

        // Multiply operands
        size_t result_len = l_operand.len + r_operand.len;
        uint32_t* result_words = (uint32_t*)calloc(result_len, sizeof(uint32_t));
        if (result_words == NULL) {
            perror("failed to malloc result_words in bsts_integer_times");
            abort();
        }

        for (size_t i = 0; i < l_operand.len; i++) {
            uint64_t carry = 0;
            uint64_t a = l_operand.words[i];
            for (size_t j = 0; j < r_operand.len; j++) {
                uint64_t b = r_operand.words[j];
                uint64_t sum = (uint64_t)result_words[i + j] + a * b + carry;
                result_words[i + j] = (uint32_t)(sum & 0xFFFFFFFF);
                carry = sum >> 32;
            }
            result_words[i + r_operand.len] += (uint32_t)carry;
        }

        // Determine sign of result
        _Bool result_sign = !(l_operand.sign == r_operand.sign);

        // Normalize result
        while (result_len > 1 && result_words[result_len - 1] == 0) {
            result_len--;
        }

        // Check if result fits in small integer
        if (result_len == 1) {
            BValue maybe_res = bsts_maybe_small_int(!result_sign, result_words[0]);
            if (maybe_res) {
              free(result_words);
              return maybe_res;
            }
        }
        // if we make it here we have to fit into big
        BValue result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
        free(result_words);
        return result;
    }
}

// Function to perform bitwise OR on two BValues
BValue bsts_integer_or(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);
    if (l_is_small & r_is_small) {
      return bsts_integer_from_int(GET_SMALL_INT(l) | GET_SMALL_INT(r));
    }

    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? 1 : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? 1 : (GET_BIG_INT(r)->len + 1);
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
    if (l_is_small & r_is_small) {
      return bsts_integer_from_int(GET_SMALL_INT(l) ^ GET_SMALL_INT(r));
    }

    // Determine maximum length in words
    // we need to leave space for maybe 1 extra word if we have -MAX
    size_t l_len = l_is_small ? 1 : (GET_BIG_INT(l)->len + 1);
    size_t r_len = r_is_small ? 1 : (GET_BIG_INT(r)->len + 1);
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
        int32_t l_int = GET_SMALL_INT(l);
        int32_t r_int = GET_SMALL_INT(r);
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
        int32_t r_int = GET_SMALL_INT(r);
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

static BValue bsts_integer_shift_twos(BValue l, int32_t shift_amount) {
    _Bool l_is_small = IS_SMALL(l);
    size_t l_len = l_is_small ? 1 : (GET_BIG_INT(l)->len + 1);
    uint32_t* l_twos = (uint32_t*)calloc(l_len, sizeof(uint32_t));
    // Convert left operand to two's complement
    if (l_is_small) {
        bsts_integer_small_to_twos(GET_SMALL_INT(l), l_twos, l_len);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, l_len);
    }

    // Determine direction of shift
    _Bool shift_left = shift_amount > 0;
    intptr_t shift_abs = shift_left ? shift_amount : -shift_amount;

    // Perform shifting on operand.words
    if (shift_left) {
        // Left shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        size_t new_len = l_len + word_shift + 1; // +1 for possible carry
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
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
        uint32_t high_bits = bsts_integer_lt_zero(l) ? ((0xFFFFFFFF >> bit_shift) << bit_shift) : 0;
        new_words[l_len + word_shift] = ((uint32_t)carry) | high_bits;

        free(l_twos);
        return bsts_integer_from_twos(new_len, new_words);
    } else {
        // Right shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        if (word_shift >= l_len) {
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

        size_t new_len = l_len - word_shift;
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            perror("failed to calloc new_words in bsts_integer_shift_left");
            abort();
        }

        _Bool operand_sign = bsts_integer_lt_zero(l);
        uint32_t sign_extension = operand_sign ? 0xFFFFFFFF : 0x00000000;

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
    int32_t shift_amount = GET_SMALL_INT(r);

    // If shift_amount is zero, return l as is
    if (shift_amount == 0) {
        return l;
    }
    return bsts_integer_shift_twos(l, shift_amount);
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
  _Bool is_zero;
  if (IS_SMALL(v)) {
      // zero is encoded as just the pure value tag
      is_zero = (v == (BValue)PURE_VALUE_TAG);
  } else {
      BSTS_Integer* m_big = GET_BIG_INT(v);
      is_zero = 1;
      for (size_t i = 0; i < m_big->len; i++) {
          if (m_big->words[i] != 0) { is_zero = 0; break; }
      }
  }

  return is_zero;
}

// (&Integer, &Integer) -> (Integer, Integer)
// div_mod(l, r) == (d, m) <=> l = r * d + m
BValue bsts_integer_div_mod(BValue l, BValue r) {
    _Bool r_is_small = IS_SMALL(r);
    if (r_is_small) {
      int32_t rs = GET_SMALL_INT(r);
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
        int32_t ls = GET_SMALL_INT(l);
        // C rounds to 0, but bosatsu and python round to -inf
        int32_t div = ls / rs;
        int32_t mod = ls % rs;
        _Bool lpos = ls >= 0;
        _Bool rpos = rs >= 0;
        if ((mod != 0) & (lpos ^ rpos)) {
          // need to repair, mod != 0 & at least one is negative
          // div * r + mod == l, but we may need to shift r towards zero
          // when we shift div = (div - 1) so mod = mod + r
          div = div - 1;
          mod = mod + rs;
        }
        return alloc_struct2(bsts_integer_from_int(div), bsts_integer_from_int(mod));
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

BSTS_Test_Result bsts_test_run(char* package_name, BConstruct test_value) {
  BValue res = test_value();
  printf("%s:\n", package_name);
  BSTS_PassFail this_test = bsts_check_test(res, 4);
  if (get_variant(res) == 0) {
    bsts_print_test_summary(4, this_test.passes, this_test.fails);
  }
  BSTS_Test_Result test_res = { package_name, this_test.passes, this_test.fails };
  return test_res;
}

int bsts_test_result_print_summary(int count, BSTS_Test_Result* results) {
  int total_fails = 0;
  int total_passes = 0;
  for (int i = 0; i < count; i++) {
    total_fails += results[i].fails;
    total_passes += results[i].passes;
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

  if (total_fails == 0) {
    printf("\npassed: \033[32m%i\033[0m\n", total_passes);
  }
  else {
    printf("\npassed: \033[32m%i\033[0m, failed: \033[31m%i\033[0m\n", total_passes, total_fails);
  }
  return (total_fails > 0);
}
