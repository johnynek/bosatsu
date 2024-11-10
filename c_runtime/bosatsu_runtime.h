#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdatomic.h>

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

// there is no struct1 at runtime
// this does not clone the args, so we own b1, b2
BValue alloc_struct2(BValue b1, BValue b2);
BValue alloc_struct3(BValue b1, BValue b2, BValue b3);
BValue alloc_struct4(BValue b1, BValue b2, BValue b3, BValue b4);

BValue get_struct_index(BValue v, int idx);

BValue alloc_enum0(ENUM_TAG variant);
BValue alloc_enum1(ENUM_TAG variant, BValue b1);
BValue alloc_enum2(ENUM_TAG variant, BValue b1, BValue b2);

ENUM_TAG get_variant(BValue v);
BValue get_enum_index(BValue v, int idx);

BValue alloc_closure1(size_t size, BValue* data, BClosure1 fn);
BValue call_fn1(BValue fn, BValue arg0);
BValue value_from_pure_fn1(BPureFn1 fn);

BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn);
BValue call_fn2(BValue fn, BValue arg0, BValue arg1);
BValue value_from_pure_fn2(BPureFn2 fn);

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