#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

/*
There are a few kinds of values:

1. pure values: small ints, characters, small strings that can fit into 62 bits.
2. pointers to referenced counted values
3. pointers to static values stack allocated at startup

to distinguish these cases we allocate pointers such that they are aligned to at least 4 byte
boundaries, so the least significant 2 bits should always be 0. Values that end with 00
correspond to case 2 above. Then case 01: corresponds to case 1, and case 11: coresponds to case 3.

We need to know which case we are in because in generic context we need to know
how to clone values.

When we point to a referenced counted value we have an atomic int and a union of three different possibilities:
1. a pointer to an array of values
2. a variant tag which is an int and a pointer to an array of values
3. a value and a pointer to a function, this represents a closure

*/

#include <stdint.h>
#include <stdatomic.h>
#include <stdlib.h>

typedef void* BValue;
// The first argument is the array of closure values
// the second is the array or arguments
typedef BValue (*BClosure1)(BValue*, BValue);
typedef BValue (*BClosure2)(BValue*, BValue, BValue);
typedef BValue (*BClosure3)(BValue*, BValue, BValue, BValue);
// this is a function that doesn't close over values
typedef BValue (*BPureFn1)(BValue);
typedef BValue (*BPureFn2)(BValue, BValue);
typedef BValue (*BPureFn3)(BValue, BValue, BValue);
// this is the free function to call on an external value
typedef void (*FreeFn)(void*);
// A function which constructs a BValue
typedef BValue (*BConstruct)();
typedef uint32_t ENUM_TAG;

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value);

void release_value(BValue value);

// this does not clone the args, so we own b1, b2
BValue alloc_struct2(BValue b1, BValue b2);

BValue alloc_enum0(ENUM_TAG variant);
BValue alloc_enum1(ENUM_TAG variant, BValue b1);
BValue alloc_enum2(ENUM_TAG variant, BValue b1, BValue b2);

// Helper function to allocate a RefCounted structure with a specified number of BValues
BValue alloc_closure(uint8_t len, BValue captures[], BClosure1 fn);

BValue alloc_external(void* eval, FreeFn free_fn);

void* get_external(BValue v);

BValue call_fn1(BValue fn, BValue arg0);
BValue call_fn2(BValue fn, BValue arg0, BValue arg1);

BValue get_index(BValue v, int idx);

ENUM_TAG get_variant(BValue v);

BValue value_from_pure_fn(BPureFn1 fn);

// should be called in main before accessing any BValue top level functions
void init_statics();

// should be called immediately before returning.
void free_statics();

// This should only be called immediately on allocated values
// on top level allocations that are made
BValue make_static(BValue v);
void free_on_close(BValue v);

#define DEFINE_RC_STRUCT(name, fields) \
    struct name { \
      atomic_int ref_count; \
      FreeFn free; \
      fields \
    }; \
    typedef struct name name

#define DEFINE_RC_ENUM(name, fields) \
    struct name { \
      atomic_int ref_count; \
      FreeFn free; \
      ENUM_TAG tag; \
      fields \
    }; \
    typedef struct name name

DEFINE_RC_STRUCT(RefCounted,);

DEFINE_RC_STRUCT(Struct1,BValue _0;);
void free_struct1(Struct1* s) {
  release_value(s->_0);
  free(s);
}
// we already own b
BValue alloc_struct1(BValue b0) {
    Struct1* rc = malloc(sizeof(Struct1));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_struct1;
    rc->_0 = b0;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct2,BValue _0; BValue _1;);
void free_struct2(Struct2* s) {
  release_value(s->_0);
  release_value(s->_1);
  free(s);
}
BValue alloc_struct2(BValue b0, BValue b1) {
    Struct2* rc = malloc(sizeof(Struct2));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_struct2;
    rc->_0 = b0;
    rc->_1 = b1;
    return (BValue)rc;
}
DEFINE_RC_STRUCT(Struct3,BValue _0; BValue _1; BValue _2;);
void free_struct3(Struct3* s) {
  release_value(s->_0);
  release_value(s->_1);
  release_value(s->_2);
  free(s);
}
BValue alloc_struct3(BValue b0, BValue b1, BValue b2) {
    Struct3* rc = malloc(sizeof(Struct3));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_struct3;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return (BValue)rc;
}
DEFINE_RC_STRUCT(Struct4,BValue _0; BValue _1; BValue _2; BValue _3;);
void free_struct4(Struct4* s) {
  release_value(s->_0);
  release_value(s->_1);
  release_value(s->_2);
  release_value(s->_3);
  free(s);
}
BValue alloc_struct4(BValue b0, BValue b1, BValue b2, BValue b3) {
    Struct3* rc = malloc(sizeof(Struct3));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_struct4;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_2 = b3;
    return (BValue)rc;
}
// ENUM0 can always be encoded into a BValue
DEFINE_RC_ENUM(Enum1,BValue _0;);
void free_enum1(Enum1* s) {
  release_value(s->_0);
  free(s);
}
BValue alloc_enum1(ENUM_TAG tag, BValue b0) {
    Enum1* rc = malloc(sizeof(Enum1));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_enum1;
    rc->tag = tag;
    rc->_0 = b0;
    return (BValue)rc;
}
DEFINE_RC_ENUM(Enum2,BValue _0; BValue _1;);
void free_enum2(Enum2* s) {
  release_value(s->_0);
  release_value(s->_1);
  free(s);
}
BValue alloc_enum2(ENUM_TAG tag, BValue b0, BValue b1) {
    Enum2* rc = malloc(sizeof(Enum2));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_enum2;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    return (BValue)rc;
}
DEFINE_RC_ENUM(Enum3,BValue _0; BValue _1; BValue _2;);
void free_enum3(Enum3* s) {
  release_value(s->_0);
  release_value(s->_1);
  release_value(s->_2);
  free(s);
}
BValue alloc_enum3(ENUM_TAG tag, BValue b0, BValue b1, BValue b2) {
    Enum3* rc = malloc(sizeof(Enum3));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_enum3;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return (BValue)rc;
}

// we allocate space for slot_len BValues at the end
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
BValue alloc_closure1(size_t size, BValue* data, BClosure1 fn) {
    Closure1Data* rc = malloc(closure_data_size(size));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of(rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}
DEFINE_RC_STRUCT(Closure2Data, BClosure2 fn; size_t slot_len;);
BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn) {
    Closure2Data* rc = malloc(closure_data_size(size));
    atomic_init(&rc->ref_count, 1);
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

DEFINE_RC_STRUCT(External, void* external; FreeFn ex_free;);
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