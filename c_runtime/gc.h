/*
 * Minimal gc.h stub for emscripten/WASM builds.
 *
 * Maps Boehm GC functions to standard malloc. This works for WASM where:
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

/* ========== Uppercase macros (traditional Boehm GC API) ========== */

#define GC_INIT() ((void)0)
#define GC_MALLOC(size) calloc(1, size)
#define GC_MALLOC_ATOMIC(size) calloc(1, size)
#define GC_REALLOC(ptr, size) realloc(ptr, size)
#define GC_FREE(ptr) free(ptr)

/* ========== Lowercase functions (also used by Boehm GC) ========== */

/* GC initialization */
static inline void GC_init(void) { /* no-op */ }

/* Standard allocation (zeroed) */
static inline void* GC_malloc(size_t size) {
    return calloc(1, size);
}

/* Allocation for atomic data (no pointers inside) */
static inline void* GC_malloc_atomic(size_t size) {
    return calloc(1, size);
}

/* Allocation that won't be collected (until explicit free) */
static inline void* GC_malloc_uncollectable(size_t size) {
    return calloc(1, size);
}

/* Reallocation */
static inline void* GC_realloc(void* ptr, size_t size) {
    return realloc(ptr, size);
}

/* Explicit free (usually not needed with real GC) */
static inline void GC_free(void* ptr) {
    free(ptr);
}

/* String duplication */
static inline char* GC_strdup(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s) + 1;
    char* copy = (char*)calloc(1, len);
    if (copy) memcpy(copy, s, len);
    return copy;
}

/* ========== Finalizer stubs (no-op for WASM) ========== */

typedef void (*GC_finalization_proc)(void* obj, void* client_data);

static inline void GC_register_finalizer(
    void* obj,
    GC_finalization_proc fn,
    void* cd,
    GC_finalization_proc* ofn,
    void** ocd
) {
    /* No-op: finalizers not supported in stub */
    if (ofn) *ofn = NULL;
    if (ocd) *ocd = NULL;
}

/* ========== Heap info stubs ========== */

static inline size_t GC_get_heap_size(void) {
    return 0; /* Unknown in WASM */
}

#endif /* BOSATSU_GC_STUB_H */
