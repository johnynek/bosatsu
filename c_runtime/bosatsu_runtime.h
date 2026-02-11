#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdint.h>
#include <stddef.h>

typedef uintptr_t BValue;
typedef uint32_t ENUM_TAG;
#include "bosatsu_decls_generated.h"

#define BSTS_BVALUE_NULL ((BValue)0)

static inline BValue bsts_bvalue_from_ptr(const void* ptr) {
  return (BValue)(uintptr_t)ptr;
}

static inline void* bsts_bvalue_to_ptr(BValue value) {
  return (void*)(uintptr_t)value;
}

static inline const void* bsts_bvalue_to_const_ptr(BValue value) {
  return (const void*)(uintptr_t)value;
}

#define BSTS_VALUE_FROM_PTR(ptr) bsts_bvalue_from_ptr((const void*)(ptr))
#define BSTS_PTR(type, value) ((type*)bsts_bvalue_to_ptr((value)))
#define BSTS_CONST_PTR(type, value) ((const type*)bsts_bvalue_to_const_ptr((value)))

/*
 * Stack allocation helpers for non-escaping values. These are intended for
 * constructor values that provably do not outlive the current function scope.
 *
 * IMPORTANT:
 * - These values MUST NOT be returned or stored in closures/statics.
 * - Struct payloads are contiguous BValue fields at offset 0.
 * - Enum payloads are prefixed by (ENUM_TAG tag, int32_t pad).
 */
#define BSTS_STACK_ALLOC_STRUCT_N(arity, ...) \
  bsts_bvalue_from_ptr((const void*)(&(struct { \
    BValue fields[(arity)]; \
  }){ .fields = { __VA_ARGS__ } }))

#define BSTS_STACK_ALLOC_ENUM_N(arity, tag_value, ...) \
  bsts_bvalue_from_ptr((const void*)(&(struct { \
    ENUM_TAG _tag; \
    int32_t _pad; \
    BValue fields[(arity)]; \
  }){ ._tag = (tag_value), ._pad = 0, .fields = { __VA_ARGS__ } }))

#define BSTS_STACK_ALLOC_STRUCT2(a0, a1) \
  BSTS_STACK_ALLOC_STRUCT_N(2, a0, a1)
#define BSTS_STACK_ALLOC_STRUCT3(a0, a1, a2) \
  BSTS_STACK_ALLOC_STRUCT_N(3, a0, a1, a2)
#define BSTS_STACK_ALLOC_STRUCT4(a0, a1, a2, a3) \
  BSTS_STACK_ALLOC_STRUCT_N(4, a0, a1, a2, a3)
#define BSTS_STACK_ALLOC_STRUCT5(a0, a1, a2, a3, a4) \
  BSTS_STACK_ALLOC_STRUCT_N(5, a0, a1, a2, a3, a4)
#define BSTS_STACK_ALLOC_STRUCT6(a0, a1, a2, a3, a4, a5) \
  BSTS_STACK_ALLOC_STRUCT_N(6, a0, a1, a2, a3, a4, a5)
#define BSTS_STACK_ALLOC_STRUCT7(a0, a1, a2, a3, a4, a5, a6) \
  BSTS_STACK_ALLOC_STRUCT_N(7, a0, a1, a2, a3, a4, a5, a6)
#define BSTS_STACK_ALLOC_STRUCT8(a0, a1, a2, a3, a4, a5, a6, a7) \
  BSTS_STACK_ALLOC_STRUCT_N(8, a0, a1, a2, a3, a4, a5, a6, a7)
#define BSTS_STACK_ALLOC_STRUCT9(a0, a1, a2, a3, a4, a5, a6, a7, a8) \
  BSTS_STACK_ALLOC_STRUCT_N(9, a0, a1, a2, a3, a4, a5, a6, a7, a8)
#define BSTS_STACK_ALLOC_STRUCT10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
  BSTS_STACK_ALLOC_STRUCT_N(10, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
#define BSTS_STACK_ALLOC_STRUCT11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
  BSTS_STACK_ALLOC_STRUCT_N(11, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
#define BSTS_STACK_ALLOC_STRUCT12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) \
  BSTS_STACK_ALLOC_STRUCT_N(12, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
#define BSTS_STACK_ALLOC_STRUCT13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) \
  BSTS_STACK_ALLOC_STRUCT_N(13, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
#define BSTS_STACK_ALLOC_STRUCT14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) \
  BSTS_STACK_ALLOC_STRUCT_N(14, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
#define BSTS_STACK_ALLOC_STRUCT15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) \
  BSTS_STACK_ALLOC_STRUCT_N(15, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
#define BSTS_STACK_ALLOC_STRUCT16(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) \
  BSTS_STACK_ALLOC_STRUCT_N(16, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
#define BSTS_STACK_ALLOC_STRUCT17(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) \
  BSTS_STACK_ALLOC_STRUCT_N(17, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
#define BSTS_STACK_ALLOC_STRUCT18(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) \
  BSTS_STACK_ALLOC_STRUCT_N(18, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
#define BSTS_STACK_ALLOC_STRUCT19(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) \
  BSTS_STACK_ALLOC_STRUCT_N(19, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
#define BSTS_STACK_ALLOC_STRUCT20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) \
  BSTS_STACK_ALLOC_STRUCT_N(20, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
#define BSTS_STACK_ALLOC_STRUCT21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) \
  BSTS_STACK_ALLOC_STRUCT_N(21, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
#define BSTS_STACK_ALLOC_STRUCT22(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) \
  BSTS_STACK_ALLOC_STRUCT_N(22, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
#define BSTS_STACK_ALLOC_STRUCT23(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) \
  BSTS_STACK_ALLOC_STRUCT_N(23, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
#define BSTS_STACK_ALLOC_STRUCT24(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23) \
  BSTS_STACK_ALLOC_STRUCT_N(24, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)
#define BSTS_STACK_ALLOC_STRUCT25(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) \
  BSTS_STACK_ALLOC_STRUCT_N(25, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24)
#define BSTS_STACK_ALLOC_STRUCT26(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) \
  BSTS_STACK_ALLOC_STRUCT_N(26, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25)
#define BSTS_STACK_ALLOC_STRUCT27(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26) \
  BSTS_STACK_ALLOC_STRUCT_N(27, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26)
#define BSTS_STACK_ALLOC_STRUCT28(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27) \
  BSTS_STACK_ALLOC_STRUCT_N(28, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27)
#define BSTS_STACK_ALLOC_STRUCT29(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28) \
  BSTS_STACK_ALLOC_STRUCT_N(29, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28)
#define BSTS_STACK_ALLOC_STRUCT30(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29) \
  BSTS_STACK_ALLOC_STRUCT_N(30, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29)
#define BSTS_STACK_ALLOC_STRUCT31(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30) \
  BSTS_STACK_ALLOC_STRUCT_N(31, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)
#define BSTS_STACK_ALLOC_STRUCT32(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) \
  BSTS_STACK_ALLOC_STRUCT_N(32, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)

#define BSTS_STACK_ALLOC_ENUM1(tag_value, a0) \
  BSTS_STACK_ALLOC_ENUM_N(1, tag_value, a0)
#define BSTS_STACK_ALLOC_ENUM2(tag_value, a0, a1) \
  BSTS_STACK_ALLOC_ENUM_N(2, tag_value, a0, a1)
#define BSTS_STACK_ALLOC_ENUM3(tag_value, a0, a1, a2) \
  BSTS_STACK_ALLOC_ENUM_N(3, tag_value, a0, a1, a2)
#define BSTS_STACK_ALLOC_ENUM4(tag_value, a0, a1, a2, a3) \
  BSTS_STACK_ALLOC_ENUM_N(4, tag_value, a0, a1, a2, a3)
#define BSTS_STACK_ALLOC_ENUM5(tag_value, a0, a1, a2, a3, a4) \
  BSTS_STACK_ALLOC_ENUM_N(5, tag_value, a0, a1, a2, a3, a4)
#define BSTS_STACK_ALLOC_ENUM6(tag_value, a0, a1, a2, a3, a4, a5) \
  BSTS_STACK_ALLOC_ENUM_N(6, tag_value, a0, a1, a2, a3, a4, a5)
#define BSTS_STACK_ALLOC_ENUM7(tag_value, a0, a1, a2, a3, a4, a5, a6) \
  BSTS_STACK_ALLOC_ENUM_N(7, tag_value, a0, a1, a2, a3, a4, a5, a6)
#define BSTS_STACK_ALLOC_ENUM8(tag_value, a0, a1, a2, a3, a4, a5, a6, a7) \
  BSTS_STACK_ALLOC_ENUM_N(8, tag_value, a0, a1, a2, a3, a4, a5, a6, a7)
#define BSTS_STACK_ALLOC_ENUM9(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8) \
  BSTS_STACK_ALLOC_ENUM_N(9, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8)
#define BSTS_STACK_ALLOC_ENUM10(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
  BSTS_STACK_ALLOC_ENUM_N(10, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
#define BSTS_STACK_ALLOC_ENUM11(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
  BSTS_STACK_ALLOC_ENUM_N(11, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
#define BSTS_STACK_ALLOC_ENUM12(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) \
  BSTS_STACK_ALLOC_ENUM_N(12, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
#define BSTS_STACK_ALLOC_ENUM13(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) \
  BSTS_STACK_ALLOC_ENUM_N(13, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
#define BSTS_STACK_ALLOC_ENUM14(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) \
  BSTS_STACK_ALLOC_ENUM_N(14, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
#define BSTS_STACK_ALLOC_ENUM15(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) \
  BSTS_STACK_ALLOC_ENUM_N(15, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
#define BSTS_STACK_ALLOC_ENUM16(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) \
  BSTS_STACK_ALLOC_ENUM_N(16, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
#define BSTS_STACK_ALLOC_ENUM17(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) \
  BSTS_STACK_ALLOC_ENUM_N(17, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
#define BSTS_STACK_ALLOC_ENUM18(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) \
  BSTS_STACK_ALLOC_ENUM_N(18, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
#define BSTS_STACK_ALLOC_ENUM19(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) \
  BSTS_STACK_ALLOC_ENUM_N(19, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
#define BSTS_STACK_ALLOC_ENUM20(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) \
  BSTS_STACK_ALLOC_ENUM_N(20, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
#define BSTS_STACK_ALLOC_ENUM21(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) \
  BSTS_STACK_ALLOC_ENUM_N(21, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
#define BSTS_STACK_ALLOC_ENUM22(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) \
  BSTS_STACK_ALLOC_ENUM_N(22, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
#define BSTS_STACK_ALLOC_ENUM23(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) \
  BSTS_STACK_ALLOC_ENUM_N(23, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
#define BSTS_STACK_ALLOC_ENUM24(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23) \
  BSTS_STACK_ALLOC_ENUM_N(24, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)
#define BSTS_STACK_ALLOC_ENUM25(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) \
  BSTS_STACK_ALLOC_ENUM_N(25, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24)
#define BSTS_STACK_ALLOC_ENUM26(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) \
  BSTS_STACK_ALLOC_ENUM_N(26, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25)
#define BSTS_STACK_ALLOC_ENUM27(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26) \
  BSTS_STACK_ALLOC_ENUM_N(27, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26)
#define BSTS_STACK_ALLOC_ENUM28(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27) \
  BSTS_STACK_ALLOC_ENUM_N(28, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27)
#define BSTS_STACK_ALLOC_ENUM29(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28) \
  BSTS_STACK_ALLOC_ENUM_N(29, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28)
#define BSTS_STACK_ALLOC_ENUM30(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29) \
  BSTS_STACK_ALLOC_ENUM_N(30, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29)
#define BSTS_STACK_ALLOC_ENUM31(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30) \
  BSTS_STACK_ALLOC_ENUM_N(31, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)
#define BSTS_STACK_ALLOC_ENUM32(tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) \
  BSTS_STACK_ALLOC_ENUM_N(32, tag_value, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)
// Nat values are encoded in integers
// TODO: move these to functions implemented in bosatsu_runtime.c
#define BSTS_NAT_0 ((BValue)((uintptr_t)0x1))
#define BSTS_NAT_SUCC(n) ((BValue)((n) + (BValue)((uintptr_t)4)))
#define BSTS_NAT_PREV(n) ((BValue)((n) - (BValue)((uintptr_t)4)))
#define BSTS_NAT_IS_0(n) ((n) == BSTS_NAT_0)
#define BSTS_NAT_GT_0(n) ((n) != BSTS_NAT_0)

// this is the free function to call on an external value
typedef void (*FreeFn)(void*);
// A function which constructs a BValue
typedef BValue (*BConstruct)();

// delta may be negative or positive
void bsts_increment_value(BValue value, int delta);

// (&BValue, int) -> &BValue
BValue get_struct_index(BValue v, int idx);

// &BValue -> Tag
ENUM_TAG get_variant(BValue v);
// This is only safe if all enums have zero args
ENUM_TAG get_variant_value(BValue v);
// (&BValue, int) -> &BValue
BValue get_enum_index(BValue v, int idx);

// This one is not auto generated because it can always be fit into the BValue directly.
// Keep it header-only so call sites can expand this to a value equivalent to
// TO_PURE_VALUE(tag). NOTE: keep this encoding in sync with TO_PURE_VALUE in
// bosatsu_runtime.c.
static inline BValue alloc_enum0(ENUM_TAG tag) {
  return (BValue)((((uintptr_t)tag) << 2) | ((uintptr_t)0x1));
}

BValue bsts_string_from_utf8_bytes_copy(size_t len, char* bytes);
// This is dangerous, it should not be mutated after returned 
BValue bsts_string_mut(size_t len);
BValue bsts_string_from_utf8_bytes_static(size_t len, char* bytes);
BValue bsts_string_from_utf8_bytes_static_null_term(char* bytes);
/*
 * write the codepoint into bytes, which must be >= 4 in length
 * and return the number of bytes written
 */
int bsts_string_code_point_to_utf8(int codepoint, char* bytes);
// (&String, &String) -> Bool
_Bool bsts_string_equals(BValue left, BValue right);
// (&String, &String) -> int 
int bsts_string_cmp(BValue left, BValue right);
// &String -> int (length in bytes)
size_t bsts_string_utf8_len(BValue);
char* bsts_string_utf8_bytes(BValue);
int bsts_utf8_code_point_bytes(const char* utf8data, int offset, int len);

// How many bytes is the codepoint at this offset, 1, 2, 3, 4, or -1 on error
// (&String, int) -> int
int bsts_string_code_point_bytes(BValue, int offset);

// (&String, int) -> char
BValue bsts_string_char_at(BValue, int);

// (&string, int, int) -> string
BValue bsts_string_substring(BValue, int start, int end);

// (&String, int) -> String
BValue bsts_string_substring_tail(BValue, int byte_offset);

// return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
// (&string, string, int) -> int
int bsts_string_find(BValue haystack, BValue needle, int start);
/*
 * search from right to left.
 * return -1 if the needle isn't in the haystack, else the offset >= byteOffset it was found
 * (&string, string, int) -> int
 */
int bsts_string_rfind(BValue haystack, BValue needle, int start);
// &String -> Unit
void bsts_string_print(BValue v);
// &String -> Unit
void bsts_string_println(BValue v);

BValue bsts_unit_value();

BValue bsts_char_from_code_point(int codepoint);
int bsts_char_code_point_from_value(BValue ch);

BValue bsts_integer_from_int(int32_t small_int);
int32_t bsts_integer_to_int32(BValue bint);
BValue bsts_integer_from_words_copy(_Bool is_pos, size_t size, uint32_t* words);
_Bool bsts_integer_equals(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_add(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_times(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_or(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_xor(BValue left, BValue right);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_and(BValue l, BValue r);
// (&Integer, &Integer) -> Integer
BValue bsts_integer_shift_left(BValue l, BValue r);
// (&Integer, &Integer) -> int
int bsts_integer_cmp(BValue l, BValue r);
// return the negative of this
// Integer -> Integer
BValue bsts_integer_negate(BValue v);
// &Integer -> String
BValue bsts_integer_to_string(BValue v);
// String -> Option[Integer]
BValue bsts_string_to_integer(BValue v);
// (&Integer, &Integer) -> (Integer, Integer)
// div_mod(l, r) == (d, m) <=> l = r * d + m
BValue bsts_integer_div_mod(BValue l, BValue r);

// Float64 values are stored by packing IEEE754 bits into the BValue word.
BValue bsts_float64_from_bits(uint64_t bits);
uint64_t bsts_float64_to_bits(BValue v);
BValue bsts_float64_from_double(double d);
double bsts_float64_to_double(BValue v);
_Bool bsts_float64_equals(BValue left, BValue right);
int bsts_float64_cmp_total(BValue left, BValue right);

BValue alloc_external(void* eval, FreeFn free_fn);
void* get_external(BValue v);

// Given the slots variable return the closure fn value
BValue bsts_closure_from_slots(BValue*);

// should be called in main before accessing any BValue top level functions
void init_statics();

// should be called immediately before returning.
void free_statics();

// This should only be called immediately on allocated values
// on top level allocations that are made
void free_on_close(BValue v);

BValue read_or_build(_Atomic BValue* v, BConstruct cons);

typedef struct BSTS_Test_Result {
  char* package_name;
  int passes;
  int fails;
} BSTS_Test_Result;

// This is the constructor to get a Test value for the given package name
// and print to stdout
BSTS_Test_Result bsts_test_run(char* package_name, BConstruct test_value);
int bsts_test_result_print_summary(int count, BSTS_Test_Result* results);

#endif
