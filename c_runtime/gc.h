/*
 * Minimal gc.h stub for emscripten/WASM builds.
 *
 * Maps Boehm GC macros to standard malloc. This works for WASM where:
 * - Memory is managed by the runtime
 * - Programs typically run to completion and release all memory
 * - No true garbage collection is needed for short-lived demo programs
 *
 * For native builds, install libgc-dev and use the real gc.h.
 */

#ifndef BOSATSU_GC_STUB_H
#define BOSATSU_GC_STUB_H

#include <stdlib.h>
#include <string.h>

/* GC initialization - no-op for malloc-based stub */
#define GC_INIT() ((void)0)

/* GC_MALLOC: allocates zeroed memory (like calloc) */
#define GC_MALLOC(size) calloc(1, size)

/* GC_MALLOC_ATOMIC: for memory that won't contain pointers */
#define GC_MALLOC_ATOMIC(size) calloc(1, size)

/* GC_REALLOC: reallocate memory */
#define GC_REALLOC(ptr, size) realloc(ptr, size)

/* GC_FREE: optional explicit free (usually not needed with real GC) */
#define GC_FREE(ptr) free(ptr)

/* GC_strdup: duplicate a string */
static inline char* GC_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* copy = (char*)GC_MALLOC_ATOMIC(len);
    if (copy) memcpy(copy, s, len);
    return copy;
}

#endif /* BOSATSU_GC_STUB_H */
