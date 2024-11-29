#include "bosatsu_runtime.h"

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

/*
There are a few kinds of values:

1. pure values: small ints, characters, small strings that can fit into 63 bits.
2. pointers to referenced counted values
3. pointers to static values stack allocated at startup

to distinguish these cases we allocate pointers such that they are aligned to at least 4 byte
boundaries:
  a. ends with 01: pure value
  b. ends with 11: static pointer (allocated once and deleteds at the end of the world)
  c. ends with 00: refcount pointer.

when it comes to functions there are two types, PureFn and closures. We have to box
  pointers to them, but when we know we have a global PureFn we can directly call it.
  if we have a static boxed value, it ends in 1, else 0.

Nat-like values are represented by positive integers encoded as PURE_VALUE such that
NAT(x) = (x << 1) | 1, since we don't have enough time to increment through 2^{63} values
this is a safe encoding.

Char values are stored as unicode code points with a trailing 1.

String values encodings, string values are like ref-counted structs with
a length and char* holding the utf-8 bytes. We could also potentially optimize
short strings by packing them literally into 63 bits with a length.

Integer values are either pure values (signed values packed into 63 bits),
or ref-counted big integers

We need to know which case we are in because in generic context we need to know
how to clone values.
*/
#define TAG_MASK 0x3
#define PURE_VALUE_TAG 0x1
#define STATIC_VALUE_TAG 0x3
#define POINTER_TAG 0x0

// Utility macros to check the tag of a value
#define IS_PURE_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == PURE_VALUE_TAG)
#define TO_PURE_VALUE(v) ((BValue)((((uintptr_t)v) << 2) | PURE_VALUE_TAG))
#define PURE_VALUE(ptr) ((uintptr_t)(ptr) >> 2)
#define IS_STATIC_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == STATIC_VALUE_TAG)
#define IS_POINTER(ptr) (((uintptr_t)(ptr) & TAG_MASK) == POINTER_TAG)
#define TO_POINTER(ptr) ((uintptr_t)(ptr) & ~TAG_MASK)

#define DEFINE_RC_STRUCT(name, fields) \
    struct name { \
      atomic_int ref_count; \
      FreeFn free; \
      fields \
    }; \
    typedef struct name name

#define DEFINE_RC_ENUM(name, fields) DEFINE_RC_STRUCT(name, ENUM_TAG tag; fields)

DEFINE_RC_STRUCT(RefCounted,);

BValue bsts_unit_value() {
  return (BValue)PURE_VALUE_TAG;
}

BValue bsts_char_from_code_point(int codepoint) {
  return (BValue)PURE_VALUE(codepoint);
}

int bsts_char_code_point_from_value(BValue ch) {
  return (int)PURE_VALUE(ch);
}

// Closures:
DEFINE_RC_STRUCT(Closure1Data, BClosure1 fn; size_t slot_len;);

size_t closure_data_size(size_t slot_len) {
  return sizeof(Closure1Data) + slot_len * sizeof(BValue);
}
BValue* closure_data_of(Closure1Data* s) {
  return (BValue*)((uintptr_t)s + sizeof(Closure1Data));
}
void free_closure(Closure1Data* s) {
  size_t slots = s->slot_len;
  BValue* items = closure_data_of(s);
  while (slots > 0) {
    release_value(items);
    items = items + 1;
    slots = slots - 1;
  }
  free(s);
}

#include "bosatsu_generated.h"

// ENUM0 can always be encoded into a BValue, but we define it to
// be able to get sizeof() to skip the header
DEFINE_RC_ENUM(Enum0,);

DEFINE_RC_STRUCT(External, void* external; FreeFn ex_free;);
DEFINE_RC_STRUCT(BSTS_String, size_t len; char* bytes;);
DEFINE_RC_STRUCT(BSTS_Integer, size_t len; _Bool sign; uint32_t* words;);

typedef struct {
    size_t len;
    _Bool sign;
    uint32_t* words;
} BSTS_Int_Operand;

BSTS_Int_Operand* bsts_int_op_from_int(BSTS_Integer* i) {
  return (BSTS_Int_Operand*)(((uintptr_t)i) + sizeof(RefCounted));
}

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
      words[0] = low;
      words[1] = high;
      op->len = 2;
    }
}

// A general structure for a reference counted memory block
// it is always allocated with len BValue array immediately after
typedef struct _Node {
  RefCounted* value;
  struct _Node* next;
} Node;

typedef struct Stack {
  Node* head;
} Stack;

static _Atomic Stack statics;

// for now, we only push refcounted values on here while they are still valid
// then we | STATIC_VALUE_TAG to avoid bothering with ref-counting during operation
static void push(RefCounted* static_value) {
  // TODO what if this malloc fails
  Node* node = malloc(sizeof(Node));
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
  if (IS_POINTER(v)) {
    push(v);
  }
}

// Returns NULl when there is nothing to pop
static RefCounted* pop() {
  Stack next;
  Stack current;
  do {
    current = atomic_load(&statics);
    if (current.head == NULL) {
      return NULL;
    }
    next.head = current.head->next; //set the head to the next node
  } while(!atomic_compare_exchange_weak(&statics, &current, next));
  RefCounted* rc = current.head->value;
  free(current.head);
  return rc;
}

void init_statics() {
  Stack empty;
  empty.head = NULL;
  atomic_store(&statics, empty);
}

BValue get_struct_index(BValue v, int idx) {
  uintptr_t rc = TO_POINTER(v);
  BValue* ptr = (BValue*)(rc + sizeof(RefCounted) + idx * sizeof(BValue));
  return *ptr;
}

ENUM_TAG get_variant(BValue v) {
  if (IS_PURE_VALUE(v)) {
    return (ENUM_TAG)PURE_VALUE(v);
  }
  else {
    Enum0* real_v = (Enum0*)TO_POINTER(v);
    return real_v->tag;
  }
}

BValue get_enum_index(BValue v, int idx) {
  uintptr_t rc = TO_POINTER(v);
  BValue* ptr = (BValue*)(rc + sizeof(Enum0) + idx * sizeof(BValue));
  return *ptr;
}

BValue alloc_enum0(ENUM_TAG tag) {
  return TO_PURE_VALUE(tag);
}

// Externals:
void free_external(External* ex) {
  ex->ex_free(ex->external);
  free(ex);
}

void bsts_init_rc(RefCounted* rc, FreeFn free) {
    // this is safe to do because before initialization there can't be race on allocated values
    rc->ref_count = 1;
    rc->free = free;
}

BValue alloc_external(void* data, FreeFn free) {
    External* rc = malloc(sizeof(External));
    bsts_init_rc((RefCounted*)rc, free);
    rc->external = data;
    rc->ex_free = free;
    return (BValue)rc;
}

void* get_external(BValue v) {
  // Externals can be static also, top level external values
  External* rc = (External*)TO_POINTER(v);
  return rc->external;
}

void free_string(void* str) {
  BSTS_String* casted = (BSTS_String*)str;
  free(casted->bytes);
  free(str);
}

void free_static_string(void* str) {
  free(str);
}

// this copies the bytes in, it does not take ownership
BValue bsts_string_from_utf8_bytes_copy(size_t len, char* bytes) {
  BSTS_String* str = malloc(sizeof(BSTS_String));
  // TODO we could allocate just once and make sure this is the tail of BSTS_String
  char* bytes_copy = malloc(sizeof(char) * len);
  memcpy(bytes_copy, bytes, len);
  str->len = len;
  str->bytes = bytes_copy;
  bsts_init_rc((RefCounted*)str, free_string);

  return (BValue)str;
}

BValue bsts_string_from_utf8_bytes_owned(size_t len, char* bytes) {
  BSTS_String* str = malloc(sizeof(BSTS_String));
  str->len = len;
  str->bytes = bytes;
  bsts_init_rc((RefCounted*)str, free_string);

  return (BValue)str;
}

BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes) {
  BSTS_String* str = malloc(sizeof(BSTS_String));
  str->len = len;
  str->bytes = bytes;
  bsts_init_rc((RefCounted*)str, free_static_string);

  return (BValue)str;
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

#define GET_STRING(v) (BSTS_String*)(TO_POINTER(v))

_Bool bsts_string_equals(BValue left, BValue right) {
  if (left == right) {
    return 1;
  }

  BSTS_String* lstr = GET_STRING(left);
  BSTS_String* rstr = GET_STRING(right);

  size_t llen = lstr->len;
  if (llen == rstr->len) {
    return (strncmp(
      lstr->bytes,
      rstr->bytes,
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
  int cmp = strncmp(lstr->bytes, rstr->bytes, min_len);

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
  return strptr->bytes;
}

/**
 * return the number of bytes at this position, 1, 2, 3, 4 or -1 on error
 * TODO: the runtime maybe should assume everything is safe, which the
 * compiler should have guaranteed, so doing error checks here is probably
 * wasteful once we debug the compiler.
 */
int bsts_string_code_point_bytes(BValue value, int offset) {
    BSTS_String* str = GET_STRING(value);
    if (str == NULL || offset < 0 || offset >= str->len) {
        // Invalid input
        return -1;
    }

    // cast to an unsigned char for the math below
    unsigned char *s = (unsigned char*)(str->bytes + offset);
    unsigned char c = s[0];
    int remaining = str->len - offset;
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
    unsigned char *s = (unsigned char*)(str->bytes + offset);
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

_Bool bsts_rc_value_is_unique(RefCounted* value) {
  return atomic_load(&(value->ref_count)) == 1;
}

// (&string, int, int) -> string
BValue bsts_string_substring(BValue value, int start, int end) {
  BSTS_String* str = GET_STRING(value);
  size_t len = str->len;
  if (len < end || end <= start) {
    // this is invalid
    return 0;
  }
  size_t new_len = end - start;
  if (str->free == free_static_string) {
    if (new_len > 0) {
      return bsts_string_from_utf8_bytes_static(new_len, str->bytes + start);
    }
    else {
      // empty string, should probably be a constant
      return bsts_string_from_utf8_bytes_static(0, "");
    }
  }
  else {
    // ref-counted bytes
    // TODO: we could keep track of an offset into the string to optimize
    // this case when refcount == 1, which may matter for tail recursion
    // taking substrings....
    return bsts_string_from_utf8_bytes_copy(new_len, str->bytes + start);
  }
}

// this takes ownership since it can possibly reuse (if it is a static string, or count is 1)
// (String, int) -> String
BValue bsts_string_substring_tail(BValue value, int byte_offset) {
  BSTS_String* str = GET_STRING(value);
  return bsts_string_substring(str, byte_offset, str->len);
}

int bsts_string_find(BValue haystack, BValue needle, int start) {
    BSTS_String* haystack_str = GET_STRING(haystack);
    BSTS_String* needle_str = GET_STRING(needle);

    size_t haystack_len = haystack_str->len;
    size_t needle_len = needle_str->len;
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
        if (haystack_str->bytes[i] == needle_str->bytes[0]) {
            // Potential match found, check the rest of the needle
            size_t j;
            for (j = 1; j < needle_len; j++) {
                if (haystack_str->bytes[i + j] != needle_str->bytes[j]) {
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
    BSTS_String* haystack_str = GET_STRING(haystack);
    BSTS_String* needle_str = GET_STRING(needle);

    size_t haystack_len = haystack_str->len;
    size_t needle_len = needle_str->len;
    if (needle_len == 0) {
        // Empty needle matches at end
        if (haystack_len == 0) {
          return 0;
        }
        return (start < (int)haystack_len) ? start : -1;
    }

    if (start < 0 || start > (int)(haystack_len - needle_len)) {
        // Start position is out of bounds
        return -1;
    }


    // The maximum valid start index is haystack_len - needle_len
    for (size_t i = (size_t)start; i <= 0; i--) {
        if (haystack_str->bytes[i] == needle_str->bytes[0]) {
            // Potential match found, check the rest of the needle
            size_t j;
            for (j = 1; j < needle_len; j++) {
                if (haystack_str->bytes[i + j] != needle_str->bytes[j]) {
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

// Helper macros and functions
#define IS_SMALL(v) IS_PURE_VALUE(v)
#define GET_SMALL_INT(v) (int32_t)(PURE_VALUE(v))
#define GET_BIG_INT(v) ((BSTS_Integer*)(TO_POINTER(v)))

BValue bsts_integer_from_int(int32_t small_int) {
    return TO_PURE_VALUE(small_int);
}

void free_integer(void* integer) {
  BSTS_Integer* bint = GET_BIG_INT(integer);
  free(bint->words);
  free(integer);
}

BSTS_Integer* bsts_integer_alloc(size_t size) {
    // chatgpt authored this
    BSTS_Integer* integer = (BSTS_Integer*)malloc(sizeof(BSTS_Integer));
    if (integer == NULL) {
        // Handle allocation failure
        return NULL;
    }

    // TODO we could allocate just once and make sure this is the tail of BSTS_Integer
    integer->words = (uint32_t*)malloc(size * sizeof(uint32_t));
    if (integer->words == NULL) {
        // Handle allocation failure
        free(integer);
        return NULL;
    }
    integer->len = size;
    bsts_init_rc((RefCounted*)integer, free_integer);
    return integer; // Low bit is 0 since it's a pointer
}

BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words) {
    // chatgpt authored this
    // remove any leading 0 words
    while ((size > 1) && (words[size - 1] == 0)) {
      size--;
    }
    BSTS_Integer* integer = bsts_integer_alloc(size);
    if (integer == NULL) {
        // Handle allocation failure
        return NULL;
    }
    integer->sign = !is_pos; // sign: 0 for positive, 1 for negative
    memcpy(integer->words, words, size * sizeof(uint32_t));
    return (BValue)integer; // Low bit is 0 since it's a pointer
}

BValue bsts_integer_from_words_owned(_Bool is_pos, size_t size, uint32_t* words) {
    // TODO: use bsts_integer_alloc
    BSTS_Integer* integer = (BSTS_Integer*)malloc(sizeof(BSTS_Integer));
    if (integer == NULL) {
        // Handle allocation failure
        return NULL;
    }

    integer->sign = !is_pos; // sign: 0 for positive, 1 for negative
    // remove any leading 0 words
    while ((size > 1) && (words[size - 1] == 0)) {
      size--;
    }
    integer->len = size;
    integer->words = words;
    bsts_init_rc((RefCounted*)integer, free_integer);
    return (BValue)integer; // Low bit is 0 since it's a pointer
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
      // this can't fit in a small int
    }
  }
  else if (small_result <= INT32_MAX) {
    // it is a small positive
    return bsts_integer_from_int(((int32_t)small_result));
  }

  return NULL;
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
        return big_int->words[0] == (uint32_t)small_int_value;
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

BValue bsts_integer_add(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    // Case 1: Both are small integers
    if (l_is_small && r_is_small) {
        int64_t l_int = (int64_t)GET_SMALL_INT(l);
        int64_t r_int = (int64_t)GET_SMALL_INT(r);
        int64_t result = l_int + r_int;

        // Check for overflow
        if ((result < INT32_MIN) || (INT32_MAX < result)) {
            // Promote to big integer
            _Bool pos = result >= 0;
            int64_t abs_result = pos ? result : -result;
            uint32_t low = (uint32_t)(abs_result & 0xFFFFFFFF);
            uint32_t high = (uint32_t)((abs_result >> 32) & 0xFFFFFFFF);
            if (high == 0) {
              uint32_t words[1] = { low };
              return bsts_integer_from_words_copy(pos, 1, words);
            }
            else {
              uint32_t words[2] = { low, high };
              return bsts_integer_from_words_copy(pos, 2, words);
            }
        } else {
            // Result fits in small integer
            return bsts_integer_from_int((int32_t)result);
        }
    } else {
        // At least one operand is a big integer

        uint32_t left_temp[2];
        uint32_t right_temp[2];
        BSTS_Int_Operand left_operand;
        BSTS_Int_Operand right_operand;

        // Prepare left operand
        if (l_is_small) {
            int32_t l_int = GET_SMALL_INT(l);
            bsts_load_op_from_small(l_int, left_temp, &left_operand);
        } else {
            BSTS_Integer* l_big = GET_BIG_INT(l);
            left_operand.sign = l_big->sign;
            left_operand.len = l_big->len;
            left_operand.words = l_big->words;
        }

        // Prepare left operand
        if (r_is_small) {
            int32_t r_int = GET_SMALL_INT(r);
            bsts_load_op_from_small(r_int, right_temp, &right_operand);
        } else {
            BSTS_Integer* r_big = GET_BIG_INT(r);
            right_operand.sign = r_big->sign;
            right_operand.len = r_big->len;
            right_operand.words = r_big->words;
        }

        BValue result = NULL;
        if (left_operand.sign == right_operand.sign) {
            // Addition
            _Bool result_sign = left_operand.sign;
            size_t max_len = (left_operand.len > right_operand.len) ? left_operand.len : right_operand.len;
            uint32_t* result_words = (uint32_t*)calloc(max_len + 1, sizeof(uint32_t));
            if (result_words == NULL) {
                return NULL;
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
                  result = bsts_integer_from_words_owned(!result_sign, result_len, result_words);
                }
            } else {
                result = bsts_integer_from_words_owned(!result_sign, result_len, result_words);
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
                    return NULL;
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
                      result = bsts_integer_from_words_owned(!result_sign, result_len, result_words);
                    }
                } else {
                    result = bsts_integer_from_words_owned(!result_sign, result_len, result_words);
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
        if (bsts_rc_value_is_unique((RefCounted*)integer)) {
          // we can reuse the data
          _Bool sign = integer->sign;
          integer->sign = !sign;
          return v;
        }
        // Create a new big integer with flipped sign

        // recall the sign is (-1)^sign, so to negate, pos = sign
        _Bool pos = integer->sign;
        BValue result = bsts_integer_from_words_copy(pos, integer->len, integer->words);
        if (result == NULL) {
            return NULL;
        }
        release_value(v);
        return result;
    }
}

// Helper function to divide big integer by 10
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
            return NULL;
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
            return NULL; 
        }

        size_t digit_count = 0;

        // Make a copy of the bigint words
        size_t len = bigint->len;
        uint32_t* words_copy = (uint32_t*)malloc(len * sizeof(uint32_t));
        if (words_copy == NULL) {
            // Memory allocation error
            free(digits);
            return NULL;
        }
        memcpy(words_copy, bigint->words, len * sizeof(uint32_t));

        uint32_t* quotient_words = (uint32_t*)malloc(len * sizeof(uint32_t));
        if (quotient_words == NULL) {
            // Memory allocation error
            free(digits);
            free(words_copy);
            return NULL;
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
        char* data = (char*)malloc(digit_count);
        if (data == NULL) {
            // Memory allocation error
            free(digits);
            free(words_copy);
            return NULL;
        }

        // reverse the data
        for (size_t i = 0; i < digit_count; i++) {
            data[i] = digits[digit_count - i - 1];
        }

        // Free temporary allocations
        free(digits);
        free(words_copy);

        return bsts_string_from_utf8_bytes_owned(digit_count, data);
    }
}

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value) {
    if (IS_POINTER(value)) {
        // It's a pointer to a reference counted value
        RefCounted* original = (RefCounted *)value;
        // Increase reference count atomically using atomic_fetch_add
        atomic_fetch_add_explicit(&original->ref_count, 1, memory_order_seq_cst);
        return value;
    } else {
        // must be a pure or static value
        // else if (IS_PURE_VALUE(value) || IS_STATIC_VALUE(value)) {
        // Pure values and static values are immutable, return them directly
        return value;
    }
}

// Function to safely decrement the reference count and free memory if needed
static void release_ref_counted(RefCounted *block) {
    if (block == NULL) return;

    // Decrement the reference count atomically using atomic_fetch_sub
    if (atomic_fetch_sub_explicit(&block->ref_count, 1, memory_order_seq_cst) == 1) {
        // If reference count drops to 0, free the memory
        // TODO actually free
        //block->free(block);
    }
}

// Function to convert sign-magnitude to two's complement representation
void sign_magnitude_to_twos_complement(_Bool sign, size_t len, uint32_t* words, uint32_t* result_words, size_t result_len) {
    if (sign == 0) {
        // Positive number
        memcpy(result_words, words, len * sizeof(uint32_t));
        for (size_t i = len; i < result_len; i++) {
            result_words[i] = 0;
        }
    } else {
        // Negative number
        memcpy(result_words, words, len * sizeof(uint32_t));
        for (size_t i = len; i < result_len; i++) {
            result_words[i] = 0;
        }
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

// Function to perform bitwise AND on two BValues
BValue bsts_integer_and(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    // Determine maximum length in words
    size_t l_len = l_is_small ? 1 : GET_BIG_INT(l)->len;
    size_t r_len = r_is_small ? 1 : GET_BIG_INT(r)->len;
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
        return NULL;
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        int64_t l_int = (int64_t)GET_SMALL_INT(l);
        _Bool l_sign = (l_int < 0) ? 1 : 0;
        uint64_t l_abs = (u_int64_t)(l_sign ? -l_int : l_int);
        for (size_t i = 0; i < max_len && l_abs > 0; i++) {
            l_twos[i] = (uint32_t)(l_abs & 0xFFFFFFFF);
            l_abs >>= 32;
        }
        if (l_sign) {
            // Negative number: invert bits and add 1
            for (size_t i = 0; i < max_len; i++) {
                l_twos[i] = ~l_twos[i];
            }
            uint64_t carry = 1;
            for (size_t i = 0; i < max_len; i++) {
                uint64_t sum = (uint64_t)l_twos[i] + carry;
                l_twos[i] = (uint32_t)(sum & 0xFFFFFFFF);
                carry = sum >> 32;
                if (carry == 0) break;
            }
        }
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        int64_t r_int = (int64_t)GET_SMALL_INT(r);
        _Bool r_sign = (r_int < 0) ? 1 : 0;
        uint64_t r_abs = (uint64_t)(r_sign ? -r_int : r_int);
        for (size_t i = 0; i < max_len && r_abs > 0; i++) {
            r_twos[i] = (uint32_t)(r_abs & 0xFFFFFFFF);
            r_abs >>= 32;
        }
        if (r_sign) {
            // Negative number: invert bits and add 1
            for (size_t i = 0; i < max_len; i++) {
                r_twos[i] = ~r_twos[i];
            }
            uint64_t carry = 1;
            for (size_t i = 0; i < max_len; i++) {
                uint64_t sum = (uint64_t)r_twos[i] + carry;
                r_twos[i] = (uint32_t)(sum & 0xFFFFFFFF);
                carry = sum >> 32;
                if (carry == 0) break;
            }
        }
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise AND
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        return NULL;
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] & r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    // Convert result from two's complement to sign-magnitude
    _Bool result_sign;
    size_t result_len = max_len;
    uint32_t* result_words = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_words == NULL) {
        free(result_twos);
        return NULL;
    }

    twos_complement_to_sign_magnitude(max_len, result_twos, &result_sign, &result_len, result_words);

    free(result_twos);

    // Check if result can be represented as small integer
    if (result_len * 32 <= sizeof(intptr_t) * 8) {
        // Attempt to pack into small integer
        intptr_t result_int = 0;
        for (size_t i = 0; i < result_len; i++) {
            result_int |= ((intptr_t)result_words[i]) << (32 * i);
        }
        if (result_sign) {
            result_int = -result_int;
        }
        if (result_int <= (INTPTR_MAX >> 1) && result_int >= (INTPTR_MIN >> 1)) {
            BValue result = bsts_integer_from_int((int)result_int);
            free(result_words);
            return result;
        }
    }

    // Return result as big integer
    BValue result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
    free(result_words);
    return result;
}

// Function to multiply two BValues
BValue bsts_integer_times(BValue left, BValue right) {
    _Bool left_is_small = IS_SMALL(left);
    _Bool right_is_small = IS_SMALL(right);

    if (left_is_small && right_is_small) {
        // Both are small integers
        int32_t l_int = GET_SMALL_INT(left);
        int32_t r_int = GET_SMALL_INT(right);
        // Multiply and check for overflow
        int64_t result = (int64_t)l_int * (int64_t)r_int;
        // Check if result fits in small integer
        if ((INT32_MIN <= result) && (result <= INT32_MAX)) {
            return bsts_integer_from_int((int32_t)result);
        } else {
            // Promote to big integer
            _Bool is_positive = result >= 0;
            uint64_t abs_result = result >= 0 ? result : -result;
            uint32_t low = (u_int32_t)(abs_result & 0xFFFFFFFF);
            uint32_t high = (u_int32_t)((abs_result >> 32) & 0xFFFFFFFF);
            if (high == 0) {
              uint32_t words[1] = { low };
              return bsts_integer_from_words_copy(is_positive, 1, words);
            }
            else {
              uint32_t words[2] = { low, high };
              return bsts_integer_from_words_copy(is_positive, 2, words);
            }
        }
    } else {
        // At least one operand is big integer
        uint32_t left_temp[2];
        uint32_t right_temp[2];
        BSTS_Int_Operand l_operand;
        BSTS_Int_Operand r_operand;

        // Prepare left operand
        if (left_is_small) {
            int32_t l_int = GET_SMALL_INT(left);
            bsts_load_op_from_small(l_int, left_temp, &l_operand);
        } else {
            BSTS_Integer* l_big = GET_BIG_INT(left);
            l_operand.sign = l_big->sign;
            l_operand.len = l_big->len;
            l_operand.words = l_big->words;
        }

        // Prepare left operand
        if (right_is_small) {
            int32_t r_int = GET_SMALL_INT(right);
            bsts_load_op_from_small(r_int, right_temp, &r_operand);
        } else {
            BSTS_Integer* r_big = GET_BIG_INT(right);
            r_operand.sign = r_big->sign;
            r_operand.len = r_big->len;
            r_operand.words = r_big->words;
        }

        // Multiply operands
        size_t result_len = l_operand.len + r_operand.len;
        uint32_t* result_words = (uint32_t*)calloc(result_len, sizeof(uint32_t));
        if (result_words == NULL) {
            return NULL;
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
        BValue result = bsts_integer_from_words_owned(!result_sign, result_len, result_words);
        return result;
    }
}

// Function to perform bitwise OR on two BValues
BValue bsts_integer_or(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    // Determine maximum length in words
    size_t l_len = l_is_small ? sizeof(intptr_t) * 8 / 32 : GET_BIG_INT(l)->len;
    size_t r_len = r_is_small ? sizeof(intptr_t) * 8 / 32 : GET_BIG_INT(r)->len;
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
        return NULL;
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        intptr_t l_int = GET_SMALL_INT(l);
        memcpy(l_twos, &l_int, sizeof(intptr_t));
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        intptr_t r_int = GET_SMALL_INT(r);
        memcpy(r_twos, &r_int, sizeof(intptr_t));
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise OR
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        return NULL;
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] | r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    // Convert result from two's complement to sign-magnitude
    _Bool result_sign;
    size_t result_len = max_len;
    uint32_t* result_words = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_words == NULL) {
        free(result_twos);
        return NULL;
    }

    twos_complement_to_sign_magnitude(max_len, result_twos, &result_sign, &result_len, result_words);

    free(result_twos);

    // Check if result can be represented as small integer
    if (result_len * 32 <= sizeof(intptr_t) * 8) {
        // Attempt to pack into small integer
        intptr_t result_int = 0;
        for (size_t i = 0; i < result_len; i++) {
            result_int |= ((intptr_t)result_words[i]) << (32 * i);
        }
        if (result_sign) {
            result_int = -result_int;
        }
        if (result_int <= (INTPTR_MAX >> 1) && result_int >= (INTPTR_MIN >> 1)) {
            BValue result = bsts_integer_from_int((int)result_int);
            free(result_words);
            return result;
        }
    }

    // Return result as big integer
    BValue result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
    free(result_words);
    return result;
}

// Function to perform bitwise XOR on two BValues
BValue bsts_integer_xor(BValue l, BValue r) {
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    // Determine maximum length in words
    size_t l_len = l_is_small ? sizeof(intptr_t) * 8 / 32 : GET_BIG_INT(l)->len;
    size_t r_len = r_is_small ? sizeof(intptr_t) * 8 / 32 : GET_BIG_INT(r)->len;
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
        return NULL;
    }

    // Convert left operand to two's complement
    if (l_is_small) {
        intptr_t l_int = GET_SMALL_INT(l);
        memcpy(l_twos, &l_int, sizeof(intptr_t));
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        sign_magnitude_to_twos_complement(l_big->sign, l_big->len, l_big->words, l_twos, max_len);
    }

    // Convert right operand to two's complement
    if (r_is_small) {
        intptr_t r_int = GET_SMALL_INT(r);
        memcpy(r_twos, &r_int, sizeof(intptr_t));
    } else {
        BSTS_Integer* r_big = GET_BIG_INT(r);
        sign_magnitude_to_twos_complement(r_big->sign, r_big->len, r_big->words, r_twos, max_len);
    }

    // Perform bitwise XOR
    uint32_t* result_twos = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_twos == NULL) {
        free(l_twos);
        free(r_twos);
        return NULL;
    }
    for (size_t i = 0; i < max_len; i++) {
        result_twos[i] = l_twos[i] ^ r_twos[i];
    }

    free(l_twos);
    free(r_twos);

    // Convert result from two's complement to sign-magnitude
    _Bool result_sign;
    size_t result_len = max_len;
    uint32_t* result_words = (uint32_t*)malloc(max_len * sizeof(uint32_t));
    if (result_words == NULL) {
        free(result_twos);
        return NULL;
    }

    twos_complement_to_sign_magnitude(max_len, result_twos, &result_sign, &result_len, result_words);

    free(result_twos);

    // Check if result can be represented as small integer
    if (result_len * 32 <= sizeof(intptr_t) * 8) {
        // Attempt to pack into small integer
        intptr_t result_int = 0;
        for (size_t i = 0; i < result_len; i++) {
            result_int |= ((intptr_t)result_words[i]) << (32 * i);
        }
        if (result_sign) {
            result_int = -result_int;
        }
        if (result_int <= (INTPTR_MAX >> 1) && result_int >= (INTPTR_MIN >> 1)) {
            BValue result = bsts_integer_from_int((int)result_int);
            free(result_words);
            return result;
        }
    }

    // Return result as big integer
    BValue result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
    free(result_words);
    return result;
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

// Function to shift a BValue left or right
BValue bsts_integer_shift_left(BValue l, BValue r) {
    // Check if r is a small integer
    if (!IS_SMALL(r)) {
        // r is not a small integer, return NULL
        return NULL;
    }

    // Get the shift amount
    int32_t shift_amount = GET_SMALL_INT(r);

    // If shift_amount is zero, return l as is
    if (shift_amount == 0) {
        return l;
    }

    // Determine direction of shift
    _Bool shift_left = shift_amount > 0;
    intptr_t shift_abs = shift_left ? shift_amount : -shift_amount;

    // Prepare the operand (l)
    BSTS_Int_Operand operand;
    uint32_t buffer[2];

    // Convert l to BSTS_Int_Operand
    if (IS_SMALL(l)) {
        bsts_load_op_from_small(GET_SMALL_INT(l), buffer, &operand);
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        operand.sign = l_big->sign;
        operand.len = l_big->len;
        operand.words = l_big->words;
    }

    // Perform shifting on operand.words
    if (shift_left) {
        // Left shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        size_t new_len = operand.len + word_shift + 1; // +1 for possible carry
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            return NULL;
        }

        // Shift bits
        uint64_t carry = 0;
        for (size_t i = 0; i < operand.len; i++) {
            uint64_t shifted = ((uint64_t)operand.words[i] << bit_shift) | carry;
            new_words[i + word_shift] = (uint32_t)(shifted & 0xFFFFFFFF);
            carry = shifted >> 32;
        }
        if (carry != 0) {
            new_words[operand.len + word_shift] = (uint32_t)carry;
        }

        // Remove leading zeros
        size_t result_len = new_len;
        while (result_len > 1 && new_words[result_len - 1] == 0) {
            result_len--;
        }

        // Check if result fits in small integer
        if (result_len == 1) {
            BValue maybe = bsts_maybe_small_int(!operand.sign, new_words[0]);
            if (maybe) {
              free(new_words);
              return maybe;
            }
        }
        // Create new big integer
        BValue result = bsts_integer_from_words_owned(!operand.sign, result_len, new_words);
        if (result == NULL) {
            free(new_words);
            return NULL;
        }
        return (BValue)result;
    } else {
        // Right shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        if (word_shift >= operand.len) {
            // All bits are shifted out
            if (operand.sign) {
                // Negative number, result is -1
                return bsts_integer_from_int(-1);
            } else {
                // Positive number, result is 0
                return bsts_integer_from_int(0);
            }
        }

        size_t new_len = operand.len - word_shift;
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            return NULL;
        }

        uint32_t sign_extension = operand.sign ? 0xFFFFFFFF : 0x00000000;

        for (size_t i = 0; i < new_len; i++) {
            uint64_t high = (i + word_shift + 1 < operand.len) ? operand.words[i + word_shift + 1] : sign_extension;
            uint64_t low = operand.words[i + word_shift];
            uint64_t combined = (high << 32) | low;
            new_words[i] = (uint32_t)((combined >> bit_shift) & 0xFFFFFFFF);
        }

        // Remove leading redundant words
        size_t result_len = new_len;
        if (operand.sign) {
            while (result_len > 1 && new_words[result_len - 1] == 0xFFFFFFFF) {
                result_len--;
            }
        } else {
            while (result_len > 1 && new_words[result_len - 1] == 0) {
                result_len--;
            }
        }

        // Check if result fits in small integer
        if (result_len == 1) {
            BValue maybe = bsts_maybe_small_int(!operand.sign, new_words[0]);
            if (maybe) {
              free(new_words);
              return maybe;
            }
        }
        // Create new big integer
        BValue result = bsts_integer_from_words_owned(!operand.sign, result_len, new_words);
        if (result == NULL) {
            free(new_words);
            return NULL;
        }
        return result;
    }
}

void free_statics() {
  RefCounted* rc;
  do {
    rc = pop();
    if (rc == NULL) return;
    release_ref_counted(rc);
  } while(1);
}

void release_value(BValue value) {
    if (IS_POINTER(value)) {
        // It's a pointer to a reference counted value
        return release_ref_counted((RefCounted *)value);
    }
}

BValue make_static(BValue v) {
  if (IS_POINTER(v)) {
    return (BValue)(((uintptr_t)v) | STATIC_VALUE_TAG);
  }
  return v;
}

BValue read_or_build(_Atomic BValue* target, BConstruct cons) {
    BValue result = atomic_load(target);
    if (result == NULL) {
        result = cons();
        BValue static_version = make_static(result);
        BValue expected = NULL;
        do {
            if (atomic_compare_exchange_weak(target, &expected, static_version)) {
                free_on_close(result);
                break;
            } else {
                expected = atomic_load(target);
                if (expected != NULL) {
                    release_value(result);
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
    print_indent(next_indent);
    printf("passed: \033[32m%i\033[0m, failed: \033[31m%i\033[0m\n", this_passes, this_fails);
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

  printf("\npassed: \033[32m%i\033[0m, failed: \033[31m%i\033[0m\n", total_passes, total_fails);
  return (total_fails > 0);
}