#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

typedef void* BValue;
typedef uint32_t ENUM_TAG;
#include "bosatsu_decls_generated.h"

// Nat values are encoded in integers
// TODO: move these to functions implemented in bosatsu_runtime.c
#define BSTS_NAT_0 ((BValue)0x1)
#define BSTS_NAT_SUCC(n) ((BValue)((uintptr_t)(n) + 4))
#define BSTS_NAT_PREV(n) ((BValue)((uintptr_t)(n) - 4))
#define BSTS_NAT_IS_0(n) (((uintptr_t)(n)) == 0x1)
#define BSTS_NAT_GT_0(n) (((uintptr_t)(n)) != 0x1)

// this is the free function to call on an external value
typedef void (*FreeFn)(void*);
// A function which constructs a BValue
typedef BValue (*BConstruct)();

// delta may be negative or positive
void bsts_increment_value(BValue value, int delta);

// (&BValue, int) -> &BValue
BValue get_struct_index(BValue v, int idx);

// &BValue -> Tag
ENUM_TAG get_variant(BValue v);
// (&BValue, int) -> &BValue
BValue get_enum_index(BValue v, int idx);

// This one is not auto generated because it can always be fit into the BValue directly
BValue alloc_enum0(ENUM_TAG tag);

BValue bsts_string_from_utf8_bytes_copy(size_t len, char* bytes);
// This is dangerous, it should not be mutated after returned 
BValue bsts_string_mut(size_t len);
BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes);
/*
 * write the codepoint into bytes, which must be >= 4 in length
 * and return the number of bytes written
 */
int bsts_string_code_point_to_utf8(int codepoint, char* bytes);
// (&String, &String) -> Bool
_Bool bsts_string_equals(BValue left, BValue right);
// (&String, &String) -> int 
int bsts_string_cmp(BValue left, BValue right);
// &String -> int (length in bytes)
size_t bsts_string_utf8_len(BValue);
char* bsts_string_utf8_bytes(BValue);

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
void bsts_string_println(BValue v);

BValue bsts_unit_value();

BValue bsts_char_from_code_point(int codepoint);
int bsts_char_code_point_from_value(BValue ch);

BValue bsts_integer_from_int(int32_t small_int);
BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words);
_Bool bsts_integer_equals(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_add(BValue left, BValue right);
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
// (&Integer, &Integer) -> int
int bsts_integer_cmp(BValue l, BValue r);
// return the negative of this
// Integer -> Integer
BValue bsts_integer_negate(BValue v);
// &Integer -> String
BValue bsts_integer_to_string(BValue v);
// (&Integer, &Integer) -> (Integer, Integer)
// div_mod(l, r) == (d, m) <=> l = r * d + m
BValue bsts_integer_div_mod(BValue l, BValue r);

BValue alloc_external(void* eval, FreeFn free_fn);
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
} BSTS_Test_Result;

// This is the constructor to get a Test value for the given package name
// and print to stdout
BSTS_Test_Result bsts_test_run(char* package_name, BConstruct test_value);
int bsts_test_result_print_summary(int count, BSTS_Test_Result* results);

#endif