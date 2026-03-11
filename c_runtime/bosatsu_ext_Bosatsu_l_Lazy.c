#include "bosatsu_ext_Bosatsu_l_Lazy.h"

#include <gc.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  _Atomic _Bool forced;
  _Atomic BValue thunk;
  _Atomic BValue value;
} BSTS_Lazy;

static BSTS_Lazy* bsts_lazy_unbox(BValue lazy_value) {
  return (BSTS_Lazy*)get_external(lazy_value);
}

static BValue bsts_lazy_wrap(BValue thunk) {
  BSTS_Lazy* lazy = (BSTS_Lazy*)GC_malloc(sizeof(BSTS_Lazy));
  if (lazy == NULL) {
    perror("GC_malloc failure in bsts_lazy_wrap");
    abort();
  }

  atomic_init(&lazy->forced, 0);
  atomic_init(&lazy->thunk, thunk);
  atomic_init(&lazy->value, bsts_unit_value());
  return alloc_external(lazy, NULL);
}

BValue ___bsts_g_Bosatsu_l_Lazy_l_lazy(BValue fn) {
  return bsts_lazy_wrap(fn);
}

BValue ___bsts_g_Bosatsu_l_Lazy_l_get__Lazy(BValue lazy_value) {
  BSTS_Lazy* lazy = bsts_lazy_unbox(lazy_value);

  if (atomic_load_explicit(&lazy->forced, memory_order_acquire)) {
    return atomic_load_explicit(&lazy->value, memory_order_acquire);
  }

  BValue thunk = atomic_load_explicit(&lazy->thunk, memory_order_acquire);
  if (thunk == bsts_unit_value()) {
    return atomic_load_explicit(&lazy->value, memory_order_acquire);
  }

  BValue value = call_fn1(thunk, bsts_unit_value());

  if (!atomic_load_explicit(&lazy->forced, memory_order_acquire)) {
    atomic_store_explicit(&lazy->value, value, memory_order_relaxed);
    atomic_store_explicit(&lazy->forced, 1, memory_order_release);
    // Clearing the thunk reference lets the closure be reclaimed.
    atomic_store_explicit(&lazy->thunk, bsts_unit_value(), memory_order_release);
    return value;
  }

  return atomic_load_explicit(&lazy->value, memory_order_acquire);
}
