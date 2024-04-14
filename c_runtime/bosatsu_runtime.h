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

typedef void* BValue;
// The first argument is the array of closure values
// the second is the array or arguments
typedef BValue (*BClosure)(BValue*, BValue*);
// this is a function that doesn't close over values
typedef BValue (*BPureFn)(BValue*);
// this is the free function to call on an external value
typedef void (*FreeFn)(void*);

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value);

void release_value(BValue value);

// this does not clone the args, so we own b1, b2
BValue alloc_struct2(BValue b1, BValue b2);

BValue alloc_enum0(int variant);
BValue alloc_enum1(int variant, BValue b1);
BValue alloc_enum2(int variant, BValue b1, BValue b2);

// Helper function to allocate a RefCounted structure with a specified number of BValues
BValue alloc_closure(uint8_t len, BValue captures[], BClosure fn);

BValue alloc_external(void* eval, FreeFn free_fn);

void* get_external(BValue v);

BValue call_fn(BValue fn, BValue args[]);

BValue get_index(BValue v, int idx);

int get_variant(BValue v);

BValue value_from_pure_fn(BPureFn fn);

// should be called in main before accessing any BValue top level functions
void init_statics();

// should be called immediately before returning.
void free_statics();

// This should only be called immediately on allocated values
// on top level allocations that get cached using atomic_bool flags
BValue make_static(BValue v);

#endif