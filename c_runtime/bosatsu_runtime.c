#include "bosatsu_runtime.h"

#include <stdatomic.h>
#include <stdlib.h>

/*
There are a few kinds of values:

1. pure values: small ints, characters, small strings that can fit into 63 bits.
2. pointers to referenced counted values
3. pointers to static values stack allocated at startup

to distinguish these cases we allocate pointers such that they are aligned to at least 4 byte
boundaries:
  a. ends with 1: pure value
  b. ends with 10: static pointer (allocated once and deleteds at the end of the world)
  c. ends with 00: refcount pointer.

We need to know which case we are in because in generic context we need to know
how to clone values.
*/
#define TAG_MASK 0x3
#define PURE_VALUE_TAG 0x1
#define STATIC_VALUE_TAG 0x3
#define POINTER_TAG 0x0

// Utility macros to check the tag of a value
#define IS_PURE_VALUE(ptr) (((uintptr_t)(ptr) & PURE_VALUE_TAG) == PURE_VALUE_TAG)
#define PURE_VALUE(ptr) ((uintptr_t)(ptr) >> 1)
#define IS_STATIC_VALUE(ptr) (((uintptr_t)(ptr) & TAG_MASK) == STATIC_VALUE_TAG)
#define IS_POINTER(ptr) (((uintptr_t)(ptr) & TAG_MASK) == POINTER_TAG)
#define TO_POINTER(ptr) ((uintptr_t)(ptr) & ~TAG_MASK)

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

DEFINE_RC_STRUCT(Struct2,BValue _0; BValue _1;);
DEFINE_RC_STRUCT(Struct3,BValue _0; BValue _1; BValue _2;);
DEFINE_RC_STRUCT(Struct4,BValue _0; BValue _1; BValue _2; BValue _3;);
// ENUM0 can always be encoded into a BValue, but we define it to
// be able to get sizeof() to skip the header
DEFINE_RC_ENUM(Enum0,);
DEFINE_RC_ENUM(Enum1,BValue _0;);
DEFINE_RC_ENUM(Enum2,BValue _0; BValue _1;);
DEFINE_RC_ENUM(Enum3,BValue _0; BValue _1; BValue _2;);

// we allocate space for slot_len BValues at the end
DEFINE_RC_STRUCT(Closure1Data, BClosure1 fn; size_t slot_len;);
DEFINE_RC_STRUCT(Closure2Data, BClosure2 fn; size_t slot_len;);

DEFINE_RC_STRUCT(External, void* external; FreeFn ex_free;);
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

BValue get_struct_index(BValue v, int idx) {
  uintptr_t rc = (uintptr_t)v;
  BValue* ptr = (BValue*)(rc + sizeof(RefCounted));
  return ptr[idx];
}

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

// Enum functions
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

ENUM_TAG get_variant(BValue v) {
  if (IS_PURE_VALUE(v)) {
    return (ENUM_TAG)PURE_VALUE(v);
  }
  else {
    Enum0* real_v = (Enum0*)TO_POINTER(v);
    return real_v->tag;
  }
}

BValue get_enum_index(BValue v, int idx) {
  uintptr_t rc = TO_POINTER(v);
  BValue* ptr = (BValue*)(rc + sizeof(Enum0));
  return ptr[idx];
}

// Closures:
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

BValue call_fn1(BValue fn, BValue arg0) {
  if (IS_STATIC_VALUE(fn)) {
    BPureFn1 pfn = (BPureFn1)TO_POINTER(fn);
    return pfn(arg0);
  }
  else {
    // this must be a closure:
    Closure1Data* rc = (Closure1Data*)fn;
    BValue* data = closure_data_of(rc);
    return rc->fn(data, arg0);
  }
}

BValue value_from_pure_fn1(BPureFn1 fn) {
  return (BValue)(((uintptr_t)fn) | STATIC_VALUE_TAG);
}

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

BValue call_fn2(BValue fn, BValue arg0, BValue arg1) {
  if (IS_STATIC_VALUE(fn)) {
    BPureFn2 pfn = (BPureFn2)TO_POINTER(fn);
    return pfn(arg0, arg1);
  }
  else {
    // this must be a closure:
    Closure2Data* rc = (Closure2Data*)fn;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return ((BClosure2)(rc->fn))(data, arg0, arg1);
  }
}
BValue value_from_pure_fn2(BPureFn2 fn) {
  return (BValue)(((uintptr_t)fn) | STATIC_VALUE_TAG);
}

// Externals:
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

void* get_external(BValue v) {
  // Externals can be static also, top level external values
  External* rc = (External*)TO_POINTER(v);
  return rc->external;
}

// Function to determine the type of the given value pointer and clone if necessary
BValue clone_value(BValue value) {
    if (IS_POINTER(value)) {
        // It's a pointer to a reference counted value
        RefCounted* original = (RefCounted *)value;
        // Increase reference count atomically using atomic_fetch_add
        atomic_fetch_add_explicit(&original->ref_count, 1, memory_order_seq_cst);
        return value;
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