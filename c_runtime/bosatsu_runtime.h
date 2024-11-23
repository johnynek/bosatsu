#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

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

when it comes to functions there are three types:
  a. top level pure function: ends with 1
  b. static closure (something that closes over static things, ideally we would optimize this away): ends with 10
  c. refcounted closure: ends with 00

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
#define IS_PURE_VALUE(ptr) (((uintptr_t)(ptr) & PURE_VALUE_TAG) == PURE_VALUE_TAG)
#define PURE_VALUE(ptr) ((uintptr_t)(ptr) >> 1)
#define IS_STATIC_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == STATIC_VALUE_TAG)
#define IS_POINTER(ptr) (((uintptr_t)(ptr) & TAG_MASK) == POINTER_TAG)
#define TO_POINTER(ptr) ((uintptr_t)(ptr) & ~TAG_MASK)
#define STATIC_PUREFN(ptr) (BValue*)((uintptr_t)(ptr) | PURE_VALUE_TAG)

#define DEFINE_RC_STRUCT(name, fields) \
    struct name { \
      atomic_int ref_count; \
      FreeFn free; \
      fields \
    }; \
    typedef struct name name

typedef void* BValue;
typedef uint32_t ENUM_TAG;
#include "bosatsu_decls_generated.h"

// Nat values are encoded in integers
#define BSTS_NAT_0 ((BValue)0x1)
#define BSTS_NAT_SUCC(n) ((BValue)((uintptr_t)(n) + 2))
#define BSTS_NAT_PREV(n) ((BValue)((uintptr_t)(n) - 2))
#define BSTS_NAT_IS_0(n) (((uintptr_t)(n)) == 0x1)
#define BSTS_NAT_GT_0(n) (((uintptr_t)(n)) != 0x1)

#define BSTS_TO_CHAR(x) (BValue)((x << 1) | 1)

// this is the free function to call on an external value
typedef void (*FreeFn)(void*);
// A function which constructs a BValue
typedef BValue (*BConstruct)();

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value);
void release_value(BValue value);

BValue get_struct_index(BValue v, int idx);

ENUM_TAG get_variant(BValue v);
BValue get_enum_index(BValue v, int idx);
// This one is not auto generated because it can always be fit into the BValue directly
BValue alloc_enum0(ENUM_TAG tag);

BValue bsts_string_from_utf8_bytes_copy(size_t len, char* bytes);
BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes);
_Bool bsts_string_equals(BValue left, BValue right);
// string -> int (lenght in bytes)
size_t bsts_string_utf8_len(BValue);

// (string, int) -> int
int bsts_string_code_point_bytes(BValue, int offset);

// (string, int) -> char
BValue bsts_string_char_at(BValue, int);

// (string, int) -> string
BValue bsts_string_substring_tail(BValue, int start);

// (string, int, int) -> string
BValue bsts_string_substring(BValue, int start, int end);

// return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
// (string, string, int) -> int
int bsts_string_find(BValue haystack, BValue needle, int start);

// basically python src.startswith(expected, _) but with utf8 byte offsets
// (string, int, string) -> _Bool
_Bool bsts_string_matches_at(BValue src, int start, BValue expected);

BValue bsts_integer_from_int(int small_int);
BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words);
_Bool bsts_integer_equals(BValue left, BValue right);

BValue alloc_external(void* eval, FreeFn free_fn);
void* get_external(BValue v);

// should be called in main before accessing any BValue top level functions
void init_statics();

// should be called immediately before returning.
void free_statics();

// This should only be called immediately on allocated values
// on top level allocations that are made
BValue make_static(BValue v);
void free_on_close(BValue v);

BValue read_or_build(_Atomic BValue* v, BConstruct cons);

typedef struct BSTS_Test_Result {
  char* package_name;
  int passes;
  int fails;
} BSTS_Test_Result;

// This is the constructor to get a Test value for the given package name
// and print to stdout
BSTS_Test_Result bsts_test_run(char* package_name, BConstruct test_value);
int bsts_test_result_print_summary(int count, BSTS_Test_Result* results);

#define CONSTRUCT(target, cons) (\
{\
    BValue result = atomic_load(target);\
    if (result == NULL) {\
        result = (cons)();\
        BValue static_version = make_static(result);\
        BValue expected = NULL;\
        do {\
            if (atomic_compare_exchange_weak(target, &expected, static_version)) {\
                free_on_close(result);\
                break;\
            } else {\
                expected = atomic_load(target);\
                if (expected != NULL) {\
                    release_value(result);\
                    result = expected;\
                    break;\
                }\
            }\
        } while (1);\
    }\
    result;\
})

#endif