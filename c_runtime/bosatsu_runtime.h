#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

typedef void* BValue;
typedef uint32_t ENUM_TAG;
#include "bosatsu_decls_generated.h"

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