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
typedef struct RefCounted {
    atomic_int ref_count;
    uint8_t len;
    union {
        struct {
        } struct_value;

        struct {
            int tag;      // variant tag: TODO can probably limit enum variants
        } enum_value;

        struct {
            BClosure func;  // pointer to a static function, first argument is value
        } closure;
    } data;
} RefCounted;

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

// this is tag set in the length field of RefCounted when we have an external value
#define EX_VAL_TAG 255

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
        int ilen = (int)block->len;
        BValue* values = values_of(block);
        if (ilen != EX_VAL_TAG) {
          for(int i = 0; i < ilen; i++) {
            release_value(values[i]);
          }
        }
        else {
          // ilen == EX_VAL_TAG is a tag that we have an external value:
          void* eval = values[0];
          FreeFn free_fn = (FreeFn)values[1];
          free_fn(eval);
        }
        free(block);
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

// Helper function to allocate a RefCounted structure with a specified number of BValues
static RefCounted* alloc_ref_counted(int len, BValue values[]) {
    RefCounted* rc = malloc(sizeof(RefCounted) + sizeof(BValue) * len);
    if (!rc) {
      // we need to release the values now
      for (int i = 0; i < len; i++) {
          release_value(values[i]);
      }
      return NULL;
    }
    
    atomic_init(&rc->ref_count, 1);
    rc->len = len;
    BValue* rc_values = values_of(rc);
    for (int i = 0; i < len; i++) {
        rc_values[i] = values[i];
    }
    return rc;
}
// we never allocate a struct1, we just use the original type

// this does not clone the args, so we own b1, b2
BValue alloc_struct2(BValue b1, BValue b2) {
    BValue args[2] = {b1, b2};
    return alloc_ref_counted(2, args);
}

static RefCounted* alloc_ref_counted_enum(uint8_t len, int variant, BValue values[]) {
    RefCounted* rc = alloc_ref_counted(len, values);
    if (!rc) return NULL;
    rc->data.enum_value.tag = variant;
    return rc;
}

BValue alloc_enum0(int variant) {
    BValue args[0] = {};
    return alloc_ref_counted_enum(0, variant, args);
}

BValue alloc_enum1(int variant, BValue b1) {
    BValue args[1] = {b1};
    return alloc_ref_counted_enum(1, variant, args);
}

BValue alloc_enum2(int variant, BValue b1, BValue b2) {
    BValue args[2] = {b1, b2};
    return alloc_ref_counted_enum(2, variant, args);
}

// Helper function to allocate a RefCounted structure with a specified number of BValues
BValue alloc_closure(uint8_t len, BValue captures[], BClosure fn) {
    RefCounted* rc = alloc_ref_counted(len, captures);
    if (!rc) return NULL;
    rc->data.closure.func = fn;
    return rc;
}

BValue alloc_external(void* eval, FreeFn free_fn) {
    RefCounted* rc = malloc(sizeof(RefCounted) + sizeof(BValue) * 2);
    if (!rc) {
      // we need to release the values now
      free_fn(eval);
      return NULL;
    }
    
    atomic_init(&rc->ref_count, 1);
    rc->len = EX_VAL_TAG;
    BValue* values = values_of(rc);
    values[0] = eval;
    values[1] = free_fn;
    return rc;
}

BValue value_from_pure_fn(BPureFn fn) {
  return (BValue)(((uintptr_t)fn) | STATIC_VALUE_TAG);
}

BValue call_fn(BValue fn, BValue args[]) {
  if (IS_STATIC_VALUE(fn)) {
    BPureFn pfn = (BPureFn)fn;
    return pfn(args);
  }
  else {
    // this must be a closure:
    RefCounted* rc = (RefCounted*)fn;
    return rc->data.closure.func(values_of(rc), args);
  }
}

BValue get_index(BValue v, int idx) {
  RefCounted* rc = (RefCounted*)v;
  return values_of(rc)[idx];
}

void* get_external(BValue v) {
  RefCounted* rc = (RefCounted*)v;
  return values_of(rc)[0];
}

int get_variant(BValue v) {
  RefCounted* rc = (RefCounted*)v;
  return rc->data.enum_value.tag;
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
BValue foo() {
  return CONSTRUCT(&__bvalue_foo, make_foo);
}