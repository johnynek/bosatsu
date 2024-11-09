#include "bosatsu_runtime.h"

#include <stdatomic.h>
#include <stdlib.h>

// Define masks based on the value tagging information provided.
#define TAG_MASK 0x3
#define PURE_VALUE_TAG 0x1
#define STATIC_VALUE_TAG 0x3
#define POINTER_TAG 0x0

// Utility macros to check the tag of a value
#define IS_PURE_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == PURE_VALUE_TAG)
#define IS_STATIC_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == STATIC_VALUE_TAG)
#define IS_POINTER(ptr) (((uintptr_t)(ptr) & TAG_MASK) == POINTER_TAG)

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

// we *always* put an array of values immediately after rc
BValue* values_of(RefCounted* rc) {
  return (BValue*)(rc + 1);
}

// Function to clone a referenced counted value
static RefCounted *clone_ref_counted(RefCounted *original) {
    if (original == NULL) return NULL;

    // Increase reference count atomically using atomic_fetch_add
    atomic_fetch_add_explicit(&original->ref_count, 1, memory_order_seq_cst);

    return original;
}

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value) {
    if (IS_POINTER(value)) {
        // It's a pointer to a reference counted value
        return clone_ref_counted((RefCounted *)value);
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

BValue value_from_pure_fn(BPureFn1 fn) {
  return (BValue)(((uintptr_t)fn) | STATIC_VALUE_TAG);
}

BValue call_fn1(BValue fn, BValue arg0) {
  if (IS_STATIC_VALUE(fn)) {
    BPureFn1 pfn = (BPureFn1)((uintptr_t)fn & ~TAG_MASK);
    return pfn(arg0);
  }
  else {
    // this must be a closure:
    Closure1Data* rc = (Closure1Data*)fn;
    BValue* data = closure_data_of(rc);
    return rc->fn(data, arg0);
  }
}

BValue call_fn2(BValue fn, BValue arg0, BValue arg1) {
  if (IS_STATIC_VALUE(fn)) {
    BPureFn2 pfn = (BPureFn2)((uintptr_t)fn & ~TAG_MASK);
    return pfn(arg0, arg1);
  }
  else {
    // this must be a closure:
    Closure2Data* rc = (Closure2Data*)fn;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return ((BClosure2)(rc->fn))(data, arg0, arg1);
  }
}

BValue get_index(BValue v, int idx) {
  RefCounted* rc = (RefCounted*)v;
  return values_of(rc)[idx];
}

void* get_external(BValue v) {
  External* rc = (External*)v;
  return rc->external;
}

ENUM_TAG get_variant(BValue v) {
  if (IS_PURE_VALUE(v)) {
    return (ENUM_TAG)((uintptr_t)v >> 2);
  }
  else {
    Enum1* real_v = (Enum1*)((uintptr_t) v & ~TAG_MASK);
    return real_v->tag;
  }
}

BValue make_static(BValue v) {
  if (IS_POINTER(v)) {
    return (BValue)(((uintptr_t)v) | STATIC_VALUE_TAG);
  }
  return v;
}

// Example static
BValue make_foo();
static _Atomic BValue __bvalue_foo = NULL;
// Add this to the main function to construct all
// the top level values before we start
BValue foo() {
  return CONSTRUCT(&__bvalue_foo, make_foo);
}