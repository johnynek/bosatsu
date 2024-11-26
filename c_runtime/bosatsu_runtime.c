#include "bosatsu_runtime.h"

#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define DEFINE_RC_ENUM(name, fields) DEFINE_RC_STRUCT(name, ENUM_TAG tag; fields)

DEFINE_RC_STRUCT(RefCounted,);

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
  uintptr_t rc = (uintptr_t)v;
  BValue* ptr = (BValue*)(rc + sizeof(RefCounted));
  return ptr[idx];
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
  BValue* ptr = (BValue*)(rc + sizeof(Enum0));
  return ptr[idx];
}

BValue alloc_enum0(ENUM_TAG tag) {
  return (BValue)(((uintptr_t)tag << 1) | PURE_VALUE_TAG);
}

// Externals:
void free_external(External* ex) {
  ex->ex_free(ex->external);
  free(ex);
}

void bsts_init_rc(RefCounted* rc, FreeFn free) {
    atomic_init(&rc->ref_count, 1);
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
  char* bytes_copy = malloc(sizeof(char) * len);
  for(size_t i = 0; i < len; i++) {
    bytes_copy[i] = bytes[i];
  }
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

_Bool bsts_string_equals(BValue left, BValue right) {
  if (left == right) {
    return 1;
  }

  BSTS_String* lstr = (BSTS_String*)left;
  BSTS_String* rstr = (BSTS_String*)right;

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

  BSTS_String* lstr = (BSTS_String*)left;
  BSTS_String* rstr = (BSTS_String*)right;

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
  BSTS_String* strptr = (BSTS_String*)str;
  return strptr->len;
}

char* bsts_string_utf8_bytes(BValue str) {
  BSTS_String* strptr = (BSTS_String*)str;
  return strptr->bytes;
}

/**
 * return the number of bytes at this position, 1, 2, 3, 4 or -1 on error
 * TODO: the runtime maybe should assume everything is safe, which the
 * compiler should have guaranteed, so doing error checks here is probably
 * wasteful once we debug the compiler.
 */
int bsts_string_code_point_bytes(BValue value, int offset) {
    BSTS_String* str = (BSTS_String*)value;
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
    BSTS_String* str = (BSTS_String*)value;
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
    return BSTS_TO_CHAR((intptr_t)code_point);
}

_Bool bsts_rc_value_is_unique(RefCounted* value) {
  return atomic_load(&(value->ref_count)) == 1;
}

// (&string, int, int) -> string
BValue bsts_string_substring(BValue value, int start, int end) {
  BSTS_String* str = (BSTS_String*)value;
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
  BSTS_String* str = (BSTS_String*)value;
  return bsts_string_substring(str, byte_offset, str->len);
}

int bsts_string_find(BValue haystack, BValue needle, int start) {
    BSTS_String* haystack_str = (BSTS_String*)haystack;
    BSTS_String* needle_str = (BSTS_String*)needle;

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
    BSTS_String* haystack_str = (BSTS_String*)haystack;
    BSTS_String* needle_str = (BSTS_String*)needle;

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

// Helper macros and functions
#define IS_SMALL(v) (((uintptr_t)(v)) & 1)
#define GET_SMALL_INT(v) ((intptr_t)((uintptr_t)(v) >> 1))
#define GET_BIG_INT(v) ((BSTS_Integer*)(v))

BValue bsts_integer_from_int(int small_int) {
    // chatgpt
    uintptr_t value = (((uintptr_t)(intptr_t)small_int) << 1) | 1;
    return (BValue)value;
}

void free_integer(void* integer) {
  BSTS_Integer* bint = GET_BIG_INT(integer);
  free(bint->words);
  free(integer);
}

BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words) {
    // chatgpt authored this
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
    integer->words = (uint32_t*)malloc(size * sizeof(uint32_t));
    if (integer->words == NULL) {
        // Handle allocation failure
        free(integer);
        return NULL;
    }
    bsts_init_rc((RefCounted*)integer, free_integer);
    memcpy(integer->words, words, size * sizeof(uint32_t));
    return (BValue)integer; // Low bit is 0 since it's a pointer
}

// Function to check equality between two BValues
_Bool bsts_integer_equals(BValue left, BValue right) {
    if (left == right) { return 1; }

    uintptr_t lval = (uintptr_t)left;
    uintptr_t rval = (uintptr_t)right;

    _Bool l_is_small = lval & 1;
    _Bool r_is_small = rval & 1;

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
        intptr_t small_int_value = GET_SMALL_INT(left);
        BSTS_Integer* big_int = GET_BIG_INT(right);

        // Check sign
        _Bool big_int_sign = big_int->sign; // 0 for positive, 1 for negative
        _Bool small_int_sign = (small_int_value < 0) ? 1 : 0;
        if (big_int_sign != small_int_sign) {
            return 0; // Different signs
        }

        // Compare absolute values
        uintptr_t abs_small_int_value = (uintptr_t)(small_int_value < 0 ? -small_int_value : small_int_value);

        // Check if big_int can fit in uintptr_t
        size_t bits_in_uintptr_t = sizeof(uintptr_t) * 8;
        if (big_int->len * 32 > bits_in_uintptr_t) {
            return 0; // big_int is too large
        }

        // Reconstruct big integer value
        uintptr_t big_int_value = 0;
        for (size_t i = 0; i < big_int->len; ++i) {
            big_int_value |= ((uintptr_t)big_int->words[i]) << (32 * i);
        }

        // Compare values
        if (big_int_value != abs_small_int_value) {
            return 0;
        }

        return 1; // Values are equal
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
        intptr_t l_int = GET_SMALL_INT(l);
        intptr_t r_int = GET_SMALL_INT(r);
        intptr_t result = l_int + r_int;

        // Check for overflow
        if ((result > (INTPTR_MAX >> 1)) || (result < (INTPTR_MIN >> 1))) {
            // Promote to big integer
            _Bool is_positive = result >= 0;
            uintptr_t abs_result = (uintptr_t)(result >= 0 ? result : -result);

            size_t word_count = 0;
            uintptr_t temp = abs_result;
            while (temp > 0) {
                temp >>= 32;
                word_count++;
            }
            if (word_count == 0) {
                return bsts_integer_from_int(0);
            }
            uint32_t* words = (uint32_t*)malloc(word_count * sizeof(uint32_t));
            if (words == NULL) {
                return NULL;
            }
            temp = abs_result;
            for (size_t i = 0; i < word_count; i++) {
                words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
            BValue big_result = bsts_integer_from_words_copy(is_positive, word_count, words);
            free(words);
            return big_result;
        } else {
            // Result fits in small integer
            return bsts_integer_from_int((int)result);
        }
    } else {
        // At least one operand is a big integer
        typedef struct {
            _Bool sign;
            size_t len;
            uint32_t* words;
        } Operand;

        Operand left_operand;
        Operand right_operand;

        // Process left operand
        if (l_is_small) {
            intptr_t l_int = GET_SMALL_INT(l);
            left_operand.sign = l_int < 0;
            uintptr_t abs_l_int = (uintptr_t)(l_int < 0 ? -l_int : l_int);

            size_t l_word_count = (abs_l_int == 0) ? 1 : 0;
            uintptr_t temp = abs_l_int;
            while (temp > 0) {
                temp >>= 32;
                l_word_count++;
            }
            left_operand.len = l_word_count;
            left_operand.words = (uint32_t*)calloc(l_word_count, sizeof(uint32_t));
            if (left_operand.words == NULL) {
                return NULL;
            }
            temp = abs_l_int;
            for (size_t i = 0; i < l_word_count; i++) {
                left_operand.words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
        } else {
            BSTS_Integer* l_big = GET_BIG_INT(l);
            left_operand.sign = l_big->sign;
            left_operand.len = l_big->len;
            left_operand.words = (uint32_t*)malloc(l_big->len * sizeof(uint32_t));
            if (left_operand.words == NULL) {
                return NULL;
            }
            memcpy(left_operand.words, l_big->words, l_big->len * sizeof(uint32_t));
        }

        // Process right operand
        if (r_is_small) {
            intptr_t r_int = GET_SMALL_INT(r);
            right_operand.sign = r_int < 0;
            uintptr_t abs_r_int = (uintptr_t)(r_int < 0 ? -r_int : r_int);

            size_t r_word_count = (abs_r_int == 0) ? 1 : 0;
            uintptr_t temp = abs_r_int;
            while (temp > 0) {
                temp >>= 32;
                r_word_count++;
            }
            right_operand.len = r_word_count;
            right_operand.words = (uint32_t*)calloc(r_word_count, sizeof(uint32_t));
            if (right_operand.words == NULL) {
                free(left_operand.words);
                return NULL;
            }
            temp = abs_r_int;
            for (size_t i = 0; i < r_word_count; i++) {
                right_operand.words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
        } else {
            BSTS_Integer* r_big = GET_BIG_INT(r);
            right_operand.sign = r_big->sign;
            right_operand.len = r_big->len;
            right_operand.words = (uint32_t*)malloc(r_big->len * sizeof(uint32_t));
            if (right_operand.words == NULL) {
                free(left_operand.words);
                return NULL;
            }
            memcpy(right_operand.words, r_big->words, r_big->len * sizeof(uint32_t));
        }

        BValue result = NULL;
        if (left_operand.sign == right_operand.sign) {
            // Addition
            _Bool result_sign = left_operand.sign;
            size_t max_len = (left_operand.len > right_operand.len) ? left_operand.len : right_operand.len;
            uint32_t* result_words = (uint32_t*)calloc(max_len + 1, sizeof(uint32_t));
            if (result_words == NULL) {
                free(left_operand.words);
                free(right_operand.words);
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
                intptr_t small_int = (intptr_t)result_words[0];
                if (result_sign) {
                    small_int = -small_int;
                }
                result = bsts_integer_from_int((int)small_int);
                free(result_words);
            } else {
                result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                free(result_words);
            }
        } else {
            // Subtraction
            int cmp = compare_abs(left_operand.len, left_operand.words, right_operand.len, right_operand.words);
            if (cmp == 0) {
                result = bsts_integer_from_int(0);
            } else {
                Operand* larger;
                Operand* smaller;
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
                    free(left_operand.words);
                    free(right_operand.words);
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
                    intptr_t small_int = (intptr_t)result_words[0];
                    if (result_sign) {
                        small_int = -small_int;
                    }
                    result = bsts_integer_from_int((int)small_int);
                    free(result_words);
                } else {
                    result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
                    free(result_words);
                }
            }
        }

        free(left_operand.words);
        free(right_operand.words);

        return result;
    }
}

// Function to negate a BValue
BValue bsts_integer_negate(BValue v) {
    if (IS_SMALL(v)) {
        intptr_t small_int = GET_SMALL_INT(v);
        if (small_int != INTPTR_MIN) {
            intptr_t negated_int = -small_int;
            return bsts_integer_from_int((int)negated_int);
        } else {
            // Handle INT_MIN, which cannot be negated in two's complement
            uintmax_t abs_value = (uintmax_t)INTPTR_MAX + 1; // Absolute value of INTPTR_MIN
            // Determine the number of 32-bit words needed
            size_t num_words = 0;
            uintmax_t temp = abs_value;
            do {
                temp >>= 32;
                num_words++;
            } while (temp != 0);

            uint32_t* words = (uint32_t*)malloc(num_words * sizeof(uint32_t));
            if (words == NULL) {
                return NULL;
            }
            temp = abs_value;
            for (size_t i = 0; i < num_words; ++i) {
                words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
            // Create a big integer with positive sign
            BValue result = bsts_integer_from_words_copy(1, num_words, words);
            free(words);
            return result;
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
        BSTS_Integer* negated_integer = (BSTS_Integer*)malloc(sizeof(BSTS_Integer));
        if (negated_integer == NULL) {
            return NULL;
        }
        negated_integer->len = integer->len;
        negated_integer->sign = !integer->sign; // Flip the sign
        negated_integer->words = (uint32_t*)malloc(integer->len * sizeof(uint32_t));
        if (negated_integer->words == NULL) {
            free(negated_integer);
            return NULL;
        }
        memcpy(negated_integer->words, integer->words, integer->len * sizeof(uint32_t));
        release_value(v);
        return (BValue)negated_integer;
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
        intptr_t value = GET_SMALL_INT(v);

        // Convert small integer to string
        char buffer[32]; // Enough for 64-bit integer
        int length = snprintf(buffer, sizeof(buffer), "%ld", value);

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
        block->free(block);
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
        _Bool l_sign = (l_int < 0) ? 1 : 0;
        uintptr_t l_abs = (uintptr_t)(l_sign ? -l_int : l_int);
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
        intptr_t r_int = GET_SMALL_INT(r);
        _Bool r_sign = (r_int < 0) ? 1 : 0;
        uintptr_t r_abs = (uintptr_t)(r_sign ? -r_int : r_int);
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
        intptr_t l_int = GET_SMALL_INT(left);
        intptr_t r_int = GET_SMALL_INT(right);
        // Multiply and check for overflow
        __int128 result = (__int128)l_int * (__int128)r_int;
        // Check if result fits in small integer
        if (result >= (INTPTR_MIN >> 1) && result <= (INTPTR_MAX >> 1)) {
            return bsts_integer_from_int((int)result);
        } else {
            // Promote to big integer
            _Bool is_positive = result >= 0;
            __uint128_t abs_result = result >= 0 ? result : -result;
            // Convert abs_result to words
            size_t word_count = 0;
            __uint128_t temp = abs_result;
            while (temp > 0) {
                temp >>= 32;
                word_count++;
            }
            uint32_t* words = (uint32_t*)malloc(word_count * sizeof(uint32_t));
            if (words == NULL) {
                return NULL;
            }
            temp = abs_result;
            for (size_t i = 0; i < word_count; i++) {
                words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
            BValue big_result = bsts_integer_from_words_copy(is_positive, word_count, words);
            free(words);
            return big_result;
        }
    } else {
        // At least one operand is big integer
        typedef struct {
            _Bool sign;
            size_t len;
            uint32_t* words;
        } Operand;

        Operand l_operand;
        Operand r_operand;

        // Prepare left operand
        if (left_is_small) {
            intptr_t l_int = GET_SMALL_INT(left);
            l_operand.sign = l_int < 0;
            uintptr_t abs_l_int = (uintptr_t)(l_int < 0 ? -l_int : l_int);
            l_operand.len = 0;
            uintptr_t temp = abs_l_int;
            while (temp > 0) {
                temp >>= 32;
                l_operand.len++;
            }
            if (l_operand.len == 0) {
                // Zero
                return bsts_integer_from_int(0);
            }
            l_operand.words = (uint32_t*)malloc(l_operand.len * sizeof(uint32_t));
            if (l_operand.words == NULL) {
                return NULL;
            }
            temp = abs_l_int;
            for (size_t i = 0; i < l_operand.len; i++) {
                l_operand.words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
        } else {
            BSTS_Integer* l_big = GET_BIG_INT(left);
            l_operand.sign = l_big->sign;
            l_operand.len = l_big->len;
            l_operand.words = (uint32_t*)malloc(l_operand.len * sizeof(uint32_t));
            if (l_operand.words == NULL) {
                return NULL;
            }
            memcpy(l_operand.words, l_big->words, l_big->len * sizeof(uint32_t));
        }

        // Prepare right operand
        if (right_is_small) {
            intptr_t r_int = GET_SMALL_INT(right);
            r_operand.sign = r_int < 0;
            uintptr_t abs_r_int = (uintptr_t)(r_int < 0 ? -r_int : r_int);
            r_operand.len = 0;
            uintptr_t temp = abs_r_int;
            while (temp > 0) {
                temp >>= 32;
                r_operand.len++;
            }
            if (r_operand.len == 0) {
                // Zero
                free(l_operand.words);
                return bsts_integer_from_int(0);
            }
            r_operand.words = (uint32_t*)malloc(r_operand.len * sizeof(uint32_t));
            if (r_operand.words == NULL) {
                free(l_operand.words);
                return NULL;
            }
            temp = abs_r_int;
            for (size_t i = 0; i < r_operand.len; i++) {
                r_operand.words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
        } else {
            BSTS_Integer* r_big = GET_BIG_INT(right);
            r_operand.sign = r_big->sign;
            r_operand.len = r_big->len;
            r_operand.words = (uint32_t*)malloc(r_operand.len * sizeof(uint32_t));
            if (r_operand.words == NULL) {
                free(l_operand.words);
                return NULL;
            }
            memcpy(r_operand.words, r_big->words, r_big->len * sizeof(uint32_t));
        }

        // Multiply operands
        size_t result_len = l_operand.len + r_operand.len;
        uint32_t* result_words = (uint32_t*)calloc(result_len, sizeof(uint32_t));
        if (result_words == NULL) {
            free(l_operand.words);
            free(r_operand.words);
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
        _Bool result_sign = l_operand.sign != r_operand.sign;

        // Normalize result
        while (result_len > 1 && result_words[result_len - 1] == 0) {
            result_len--;
        }

        // Check if result fits in small integer
        if (result_len == 1) {
            intptr_t small_result = (intptr_t)result_words[0];
            if (result_sign) {
                small_result = -small_result;
            }
            free(result_words);
            free(l_operand.words);
            free(r_operand.words);
            return bsts_integer_from_int((int)small_result);
        } else {
            BValue result = bsts_integer_from_words_copy(!result_sign, result_len, result_words);
            free(result_words);
            free(l_operand.words);
            free(r_operand.words);
            return result;
        }
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
    _Bool l_is_small = IS_SMALL(l);
    _Bool r_is_small = IS_SMALL(r);

    if (l_is_small && r_is_small) {
        // Both are small integers
        intptr_t l_int = GET_SMALL_INT(l);
        intptr_t r_int = GET_SMALL_INT(r);
        if (l_int < r_int) return -1;
        else if (l_int > r_int) return 1;
        else return 0;
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
            // Swap 'l' and 'r'
            BValue temp = l;
            l = r;
            r = temp;
            _Bool temp_is_small = l_is_small;
            l_is_small = r_is_small;
            r_is_small = temp_is_small;

            // Negate the result
            int cmp = bsts_integer_cmp(l, r);
            return -cmp;
        }

        // Now 'l' is big, 'r' is small
        intptr_t r_int = GET_SMALL_INT(r);
        BSTS_Integer* l_big = GET_BIG_INT(l);

        // Compare signs
        _Bool l_sign = l_big->sign;
        _Bool r_sign = (r_int < 0) ? 1 : 0;

        if (l_sign != r_sign) {
            return l_sign ? -1 : 1;
        }

        // Signs are the same, compare magnitudes
        uintptr_t r_abs = (uintptr_t)(r_int < 0 ? -r_int : r_int);

        // Calculate number of words in 'r_abs'
        size_t r_abs_len = 0;
        uintptr_t temp_r_abs = r_abs;
        do {
            temp_r_abs >>= 32;
            r_abs_len++;
        } while (temp_r_abs > 0);

        if (l_big->len != r_abs_len) {
            if (l_big->len > r_abs_len)
                return l_sign ? -1 : 1;
            else
                return l_sign ? 1 : -1;
        } else {
            // Lengths are equal, compare words
            uint32_t r_words[r_abs_len];
            uintptr_t temp = r_abs;
            for (size_t i = 0; i < r_abs_len; i++) {
                r_words[i] = (uint32_t)(temp & 0xFFFFFFFF);
                temp >>= 32;
            }
            // Compare words from most significant to least significant
            for (size_t i = l_big->len; i > 0; i--) {
                uint32_t l_word = l_big->words[i - 1];
                uint32_t r_word = r_words[i - 1];
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
    intptr_t shift_amount = GET_SMALL_INT(r);

    // If shift_amount is zero, return l as is
    if (shift_amount == 0) {
        return l;
    }

    // Determine direction of shift
    _Bool shift_left = shift_amount > 0;
    intptr_t shift_abs = shift_left ? shift_amount : -shift_amount;

    // Prepare the operand (l)
    typedef struct {
        _Bool sign;
        size_t len;
        uint32_t* words;
    } Operand;

    Operand operand;

    // Convert l to Operand
    if (IS_SMALL(l)) {
        intptr_t l_int = GET_SMALL_INT(l);
        operand.sign = l_int < 0;
        uintptr_t abs_l_int = (uintptr_t)(l_int < 0 ? -l_int : l_int);

        // Convert abs_l_int to words
        size_t word_count = 0;
        uintptr_t temp = abs_l_int;
        do {
            temp >>= 32;
            word_count++;
        } while (temp != 0);

        operand.len = word_count;
        operand.words = (uint32_t*)calloc(word_count, sizeof(uint32_t));
        if (operand.words == NULL) {
            return NULL;
        }
        temp = abs_l_int;
        for (size_t i = 0; i < word_count; i++) {
            operand.words[i] = (uint32_t)(temp & 0xFFFFFFFF);
            temp >>= 32;
        }
    } else {
        BSTS_Integer* l_big = GET_BIG_INT(l);
        operand.sign = l_big->sign;
        operand.len = l_big->len;
        operand.words = (uint32_t*)malloc(operand.len * sizeof(uint32_t));
        if (operand.words == NULL) {
            return NULL;
        }
        memcpy(operand.words, l_big->words, operand.len * sizeof(uint32_t));
    }

    // Perform shifting on operand.words
    if (shift_left) {
        // Left shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        size_t new_len = operand.len + word_shift + 1; // +1 for possible carry
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            free(operand.words);
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
            intptr_t result_int = (intptr_t)new_words[0];
            if (operand.sign) {
                result_int = -result_int;
            }
            free(new_words);
            free(operand.words);
            return bsts_integer_from_int((int)result_int);
        } else {
            // Create new big integer
            BSTS_Integer* result = (BSTS_Integer*)malloc(sizeof(BSTS_Integer));
            if (result == NULL) {
                free(new_words);
                free(operand.words);
                return NULL;
            }
            result->len = result_len;
            result->sign = operand.sign;
            result->words = new_words;

            free(operand.words);
            return (BValue)result;
        }
    } else {
        // Right shift
        size_t word_shift = shift_abs / 32;
        size_t bit_shift = shift_abs % 32;

        if (word_shift >= operand.len) {
            // All bits are shifted out
            if (operand.sign) {
                // Negative number, result is -1
                free(operand.words);
                return bsts_integer_from_int(-1);
            } else {
                // Positive number, result is 0
                free(operand.words);
                return bsts_integer_from_int(0);
            }
        }

        size_t new_len = operand.len - word_shift;
        uint32_t* new_words = (uint32_t*)calloc(new_len, sizeof(uint32_t));
        if (new_words == NULL) {
            free(operand.words);
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
            intptr_t result_int = (intptr_t)new_words[0];
            if (operand.sign) {
                result_int = -result_int;
            }
            free(new_words);
            free(operand.words);
            return bsts_integer_from_int((int)result_int);
        } else {
            // Create new big integer
            BSTS_Integer* result = (BSTS_Integer*)malloc(sizeof(BSTS_Integer));
            if (result == NULL) {
                free(new_words);
                free(operand.words);
                return NULL;
            }
            result->len = result_len;
            result->sign = operand.sign;
            result->words = new_words;

            free(operand.words);
            return (BValue)result;
        }
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