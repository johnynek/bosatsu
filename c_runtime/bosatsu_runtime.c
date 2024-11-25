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

BValue alloc_external(void* data, FreeFn free) {
    External* rc = malloc(sizeof(External));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_external;
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
  atomic_init(&str->ref_count, 1);
  str->free = (FreeFn)free_string;

  return (BValue)str;
}

BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes) {
  BSTS_String* str = malloc(sizeof(BSTS_String));
  str->len = len;
  str->bytes = bytes;
  atomic_init(&str->ref_count, 1);
  str->free = (FreeFn)free_static_string;

  return (BValue)str;
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

size_t bsts_string_utf8_len(BValue str) {
  BSTS_String* strptr = (BSTS_String*)str;
  return strptr->len;
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

// (string, int, int) -> string
// this takes ownership since it can possibly reuse (if it is a static string, or count is 1)
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