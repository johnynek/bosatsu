#ifndef BOSATSU_RUNTIME_H
#define BOSATSU_RUNTIME_H

#include <stdatomic.h>

typedef void* BValue;
// The first argument is the array of closure values
// the second is the array or arguments
typedef BValue (*BClosure1)(BValue*,BValue);
typedef BValue (*BClosure2)(BValue*,BValue,BValue);
typedef BValue (*BClosure3)(BValue*,BValue,BValue,BValue);
typedef BValue (*BClosure4)(BValue*,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure5)(BValue*,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure6)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure7)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure8)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure9)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure10)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure11)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure12)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure13)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure14)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure15)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure16)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure17)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure18)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure19)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure20)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure21)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure22)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure23)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure24)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure25)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure26)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure27)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure28)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure29)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure30)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure31)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BClosure32)(BValue*,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);

// this is a function that doesn't close over values
typedef BValue (*BPureFn1)(BValue);
typedef BValue (*BPureFn2)(BValue,BValue);
typedef BValue (*BPureFn3)(BValue,BValue,BValue);
typedef BValue (*BPureFn4)(BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn5)(BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn6)(BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn7)(BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn8)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn9)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn10)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn11)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn12)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn13)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn14)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn15)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn16)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn17)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn18)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn19)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn20)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn21)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn22)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn23)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn24)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn25)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn26)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn27)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn28)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn29)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn30)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn31)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);
typedef BValue (*BPureFn32)(BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue,BValue);

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
BValue alloc_struct2(BValue arg0, BValue arg1);
BValue alloc_struct3(BValue arg0, BValue arg1, BValue arg2);
BValue alloc_struct4(BValue arg0, BValue arg1, BValue arg2, BValue arg3);
BValue alloc_struct5(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4);
BValue alloc_struct6(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5);
BValue alloc_struct7(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6);
BValue alloc_struct8(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7);
BValue alloc_struct9(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8);
BValue alloc_struct10(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9);
BValue alloc_struct11(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10);
BValue alloc_struct12(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11);
BValue alloc_struct13(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12);
BValue alloc_struct14(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13);
BValue alloc_struct15(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14);
BValue alloc_struct16(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15);
BValue alloc_struct17(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16);
BValue alloc_struct18(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17);
BValue alloc_struct19(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18);
BValue alloc_struct20(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19);
BValue alloc_struct21(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20);
BValue alloc_struct22(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21);
BValue alloc_struct23(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22);
BValue alloc_struct24(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23);
BValue alloc_struct25(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24);
BValue alloc_struct26(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25);
BValue alloc_struct27(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26);
BValue alloc_struct28(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27);
BValue alloc_struct29(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28);
BValue alloc_struct30(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29);
BValue alloc_struct31(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30);
BValue alloc_struct32(BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30, BValue arg31);

BValue get_struct_index(BValue v, int idx);

BValue alloc_enum0(ENUM_TAG variant);
BValue alloc_enum1(ENUM_TAG variant, BValue arg0);
BValue alloc_enum2(ENUM_TAG variant, BValue arg0, BValue arg1);
BValue alloc_enum3(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2);
BValue alloc_enum4(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3);
BValue alloc_enum5(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4);
BValue alloc_enum6(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5);
BValue alloc_enum7(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6);
BValue alloc_enum8(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7);
BValue alloc_enum9(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8);
BValue alloc_enum10(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9);
BValue alloc_enum11(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10);
BValue alloc_enum12(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11);
BValue alloc_enum13(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12);
BValue alloc_enum14(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13);
BValue alloc_enum15(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14);
BValue alloc_enum16(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15);
BValue alloc_enum17(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16);
BValue alloc_enum18(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17);
BValue alloc_enum19(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18);
BValue alloc_enum20(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19);
BValue alloc_enum21(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20);
BValue alloc_enum22(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21);
BValue alloc_enum23(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22);
BValue alloc_enum24(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23);
BValue alloc_enum25(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24);
BValue alloc_enum26(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25);
BValue alloc_enum27(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26);
BValue alloc_enum28(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27);
BValue alloc_enum29(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28);
BValue alloc_enum30(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29);
BValue alloc_enum31(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30);
BValue alloc_enum32(ENUM_TAG variant, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30, BValue arg31);

ENUM_TAG get_variant(BValue v);
BValue get_enum_index(BValue v, int idx);

BValue alloc_closure1(size_t size, BValue* data, BClosure1 fn);
BValue value_from_pure_fn1(BPureFn1 fn);
BValue call_fn1(BValue fn, BValue arg0);

BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn);
BValue value_from_pure_fn2(BPureFn2 fn);
BValue call_fn2(BValue fn, BValue arg0, BValue arg1);

BValue alloc_closure3(size_t size, BValue* data, BClosure3 fn);
BValue value_from_pure_fn3(BPureFn3 fn);
BValue call_fn3(BValue fn, BValue arg0, BValue arg1, BValue arg2);

BValue alloc_closure4(size_t size, BValue* data, BClosure4 fn);
BValue value_from_pure_fn4(BPureFn4 fn);
BValue call_fn4(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3);

BValue alloc_closure5(size_t size, BValue* data, BClosure5 fn);
BValue value_from_pure_fn5(BPureFn5 fn);
BValue call_fn5(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4);

BValue alloc_closure6(size_t size, BValue* data, BClosure6 fn);
BValue value_from_pure_fn6(BPureFn6 fn);
BValue call_fn6(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5);

BValue alloc_closure7(size_t size, BValue* data, BClosure7 fn);
BValue value_from_pure_fn7(BPureFn7 fn);
BValue call_fn7(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6);

BValue alloc_closure8(size_t size, BValue* data, BClosure8 fn);
BValue value_from_pure_fn8(BPureFn8 fn);
BValue call_fn8(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7);

BValue alloc_closure9(size_t size, BValue* data, BClosure9 fn);
BValue value_from_pure_fn9(BPureFn9 fn);
BValue call_fn9(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8);

BValue alloc_closure10(size_t size, BValue* data, BClosure10 fn);
BValue value_from_pure_fn10(BPureFn10 fn);
BValue call_fn10(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9);

BValue alloc_closure11(size_t size, BValue* data, BClosure11 fn);
BValue value_from_pure_fn11(BPureFn11 fn);
BValue call_fn11(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10);

BValue alloc_closure12(size_t size, BValue* data, BClosure12 fn);
BValue value_from_pure_fn12(BPureFn12 fn);
BValue call_fn12(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11);

BValue alloc_closure13(size_t size, BValue* data, BClosure13 fn);
BValue value_from_pure_fn13(BPureFn13 fn);
BValue call_fn13(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12);

BValue alloc_closure14(size_t size, BValue* data, BClosure14 fn);
BValue value_from_pure_fn14(BPureFn14 fn);
BValue call_fn14(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13);

BValue alloc_closure15(size_t size, BValue* data, BClosure15 fn);
BValue value_from_pure_fn15(BPureFn15 fn);
BValue call_fn15(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14);

BValue alloc_closure16(size_t size, BValue* data, BClosure16 fn);
BValue value_from_pure_fn16(BPureFn16 fn);
BValue call_fn16(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15);

BValue alloc_closure17(size_t size, BValue* data, BClosure17 fn);
BValue value_from_pure_fn17(BPureFn17 fn);
BValue call_fn17(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16);

BValue alloc_closure18(size_t size, BValue* data, BClosure18 fn);
BValue value_from_pure_fn18(BPureFn18 fn);
BValue call_fn18(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17);

BValue alloc_closure19(size_t size, BValue* data, BClosure19 fn);
BValue value_from_pure_fn19(BPureFn19 fn);
BValue call_fn19(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18);

BValue alloc_closure20(size_t size, BValue* data, BClosure20 fn);
BValue value_from_pure_fn20(BPureFn20 fn);
BValue call_fn20(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19);

BValue alloc_closure21(size_t size, BValue* data, BClosure21 fn);
BValue value_from_pure_fn21(BPureFn21 fn);
BValue call_fn21(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20);

BValue alloc_closure22(size_t size, BValue* data, BClosure22 fn);
BValue value_from_pure_fn22(BPureFn22 fn);
BValue call_fn22(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21);

BValue alloc_closure23(size_t size, BValue* data, BClosure23 fn);
BValue value_from_pure_fn23(BPureFn23 fn);
BValue call_fn23(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22);

BValue alloc_closure24(size_t size, BValue* data, BClosure24 fn);
BValue value_from_pure_fn24(BPureFn24 fn);
BValue call_fn24(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23);

BValue alloc_closure25(size_t size, BValue* data, BClosure25 fn);
BValue value_from_pure_fn25(BPureFn25 fn);
BValue call_fn25(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24);

BValue alloc_closure26(size_t size, BValue* data, BClosure26 fn);
BValue value_from_pure_fn26(BPureFn26 fn);
BValue call_fn26(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25);

BValue alloc_closure27(size_t size, BValue* data, BClosure27 fn);
BValue value_from_pure_fn27(BPureFn27 fn);
BValue call_fn27(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26);

BValue alloc_closure28(size_t size, BValue* data, BClosure28 fn);
BValue value_from_pure_fn28(BPureFn28 fn);
BValue call_fn28(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27);

BValue alloc_closure29(size_t size, BValue* data, BClosure29 fn);
BValue value_from_pure_fn29(BPureFn29 fn);
BValue call_fn29(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28);

BValue alloc_closure30(size_t size, BValue* data, BClosure30 fn);
BValue value_from_pure_fn30(BPureFn30 fn);
BValue call_fn30(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29);

BValue alloc_closure31(size_t size, BValue* data, BClosure31 fn);
BValue value_from_pure_fn31(BPureFn31 fn);
BValue call_fn31(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30);

BValue alloc_closure32(size_t size, BValue* data, BClosure32 fn);
BValue value_from_pure_fn32(BPureFn32 fn);
BValue call_fn32(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30, BValue arg31);

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