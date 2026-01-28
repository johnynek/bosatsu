#include "bosatsu_runtime.h"
#include "bosatsu_ext_Bosatsu_l_Predef.h"
#include "bosatsu_ext_Bosatsu_l_Prog.h"
#include <stdlib.h>
#include "gc.h"

BValue __bsts_t_lambda0(BValue __bsts_b_a0, BValue __bsts_b_b0) {
    return alloc_enum2(1, __bsts_b_a0, __bsts_b_b0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_build__List(BValue __bsts_b_fn0) {
    return call_fn2(__bsts_b_fn0, alloc_boxed_pure_fn2(__bsts_t_lambda0), alloc_enum0(0));
}

BValue __bsts_t_closure__loop1(BValue* __bstsi_slot, BValue __bsts_b_list0) {
    if (get_variant(__bsts_b_list0) == 0) {
        return __bstsi_slot[0];
    }
    else {
        BValue __bsts_b_h0 = get_enum_index(__bsts_b_list0, 0);
        BValue __bsts_b_t0 = get_enum_index(__bsts_b_list0, 1);
        return call_fn2(__bstsi_slot[1],
            __bsts_b_h0,
            __bsts_t_closure__loop1(__bstsi_slot, __bsts_b_t0));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldr__List(BValue __bsts_b_list0,
    BValue __bsts_b_fn0,
    BValue __bsts_b_acc0) {
    BValue __bsts_l_captures2[2] = { __bsts_b_acc0, __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure1(2, __bsts_l_captures2, __bsts_t_closure__loop1);
    return call_fn1(__bsts_b_loop0, __bsts_b_list0);
}

BValue __bsts_t_closure3(BValue* __bstsi_slot, BValue __bsts_b_lst0, BValue __bsts_b_item0) {
    BValue __bsts_a_0;
    BValue __bsts_a_1;
    BValue __bsts_a_3;
    BValue __bsts_a_5;
    __bsts_a_3 = __bsts_b_lst0;
    __bsts_a_5 = __bsts_b_item0;
    __bsts_a_0 = alloc_enum0(1);
    _Bool __bsts_l_cond4;
    __bsts_l_cond4 = get_variant_value(__bsts_a_0) == 1;
    while (__bsts_l_cond4) {
        if (get_variant(__bsts_a_3) == 0) {
            __bsts_a_0 = alloc_enum0(0);
            __bsts_a_1 = __bsts_a_5;
        }
        else {
            BValue __bsts_b_head0 = get_enum_index(__bsts_a_3, 0);
            BValue __bsts_b_tail0 = get_enum_index(__bsts_a_3, 1);
            BValue __bsts_a_2 = __bsts_b_tail0;
            BValue __bsts_a_4 = call_fn2(__bstsi_slot[0], __bsts_a_5, __bsts_b_head0);
            __bsts_a_3 = __bsts_a_2;
            __bsts_a_5 = __bsts_a_4;
        }
        __bsts_l_cond4 = get_variant_value(__bsts_a_0) == 1;
    }
    return __bsts_a_1;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_foldl__List(BValue __bsts_b_lst0,
    BValue __bsts_b_item0,
    BValue __bsts_b_fn0) {
    BValue __bsts_l_captures5[1] = { __bsts_b_fn0 };
    BValue __bsts_b_loop0 = alloc_closure2(1, __bsts_l_captures5, __bsts_t_closure3);
    return call_fn2(__bsts_b_loop0, __bsts_b_lst0, __bsts_b_item0);
}

BValue __bsts_t_lambda6(BValue __bsts_b_tail0, BValue __bsts_b_h0) {
    return alloc_enum2(1, __bsts_b_h0, __bsts_b_tail0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(BValue __bsts_b_front0, BValue __bsts_b_back0) {
    return ___bsts_g_Bosatsu_l_Predef_l_foldl__List(__bsts_b_front0,
        __bsts_b_back0,
        alloc_boxed_pure_fn2(__bsts_t_lambda6));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_reverse(BValue __bsts_b_as0) {
    return ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(__bsts_b_as0, alloc_enum0(0));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_concat(BValue __bsts_b_front0, BValue __bsts_b_back0) {
    return get_variant(__bsts_b_back0) == 0 ?
        __bsts_b_front0 :
        ___bsts_g_Bosatsu_l_Predef_l_reverse__concat(___bsts_g_Bosatsu_l_Predef_l_reverse__concat(__bsts_b_front0,
                alloc_enum0(0)),
            __bsts_b_back0);
}

BValue __bsts_t_closure7(BValue* __bstsi_slot, BValue __bsts_b_t0, BValue __bsts_b_a0) {
    return alloc_enum2(1, call_fn1(__bstsi_slot[0], __bsts_b_a0), __bsts_b_t0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_map__List(BValue __bsts_b_lst0, BValue __bsts_b_fn0) {
    BValue __bsts_l_captures8[1] = { __bsts_b_fn0 };
    return ___bsts_g_Bosatsu_l_Predef_l_reverse(___bsts_g_Bosatsu_l_Predef_l_foldl__List(__bsts_b_lst0,
            alloc_enum0(0),
            alloc_closure2(1, __bsts_l_captures8, __bsts_t_closure7)));
}

BValue __bsts_t_closure9(BValue* __bstsi_slot, BValue __bsts_b_t0, BValue __bsts_b_a0) {
    BValue __bsts_b_front0 = call_fn1(__bstsi_slot[0], __bsts_b_a0);
    return ___bsts_g_Bosatsu_l_Predef_l_foldl__List(__bsts_b_front0,
        __bsts_b_t0,
        alloc_boxed_pure_fn2(__bsts_t_lambda6));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_flat__map__List(BValue __bsts_b_lst0, BValue __bsts_b_fn0) {
    BValue __bsts_l_captures10[1] = { __bsts_b_fn0 };
    return ___bsts_g_Bosatsu_l_Predef_l_reverse(___bsts_g_Bosatsu_l_Predef_l_foldl__List(__bsts_b_lst0,
            alloc_enum0(0),
            alloc_closure2(1, __bsts_l_captures10, __bsts_t_closure9)));
}

BValue __bsts_t_closure11(BValue* __bstsi_slot, BValue __bsts_b_i0, BValue __bsts_b_acc0) {
    return alloc_struct2(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1), __bsts_b_i0),
        alloc_enum2(1, __bstsi_slot[0], __bsts_b_acc0));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_replicate__List(BValue __bsts_b_item0, BValue __bsts_b_cnt0) {
    BValue __bsts_l_captures12[1] = { __bsts_b_item0 };
    return ___bsts_g_Bosatsu_l_Predef_l_int__loop(__bsts_b_cnt0,
        alloc_enum0(0),
        alloc_closure2(1, __bsts_l_captures12, __bsts_t_closure11));
}

BValue __bsts_t_closure13(BValue* __bstsi_slot, BValue __bsts_b_x10, BValue __bsts_b_x20) {
    return call_fn1(call_fn1(__bstsi_slot[0], __bsts_b_x10), __bsts_b_x20);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_uncurry2(BValue __bsts_b_f0) {
    BValue __bsts_l_captures14[1] = { __bsts_b_f0 };
    return alloc_closure2(1, __bsts_l_captures14, __bsts_t_closure13);
}

BValue __bsts_t_closure15(BValue* __bstsi_slot,
    BValue __bsts_b_x10,
    BValue __bsts_b_x20,
    BValue __bsts_b_x30) {
    return call_fn1(call_fn1(call_fn1(__bstsi_slot[0], __bsts_b_x10), __bsts_b_x20), __bsts_b_x30);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_uncurry3(BValue __bsts_b_f0) {
    BValue __bsts_l_captures16[1] = { __bsts_b_f0 };
    return alloc_closure3(1, __bsts_l_captures16, __bsts_t_closure15);
}

BValue __bsts_t_lambda17(BValue __bsts_b_i0, BValue __bsts_b_tail0) {
    BValue __bsts_b_inext0 = ___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1),
        __bsts_b_i0);
    return alloc_struct2(__bsts_b_inext0, alloc_enum2(1, __bsts_b_inext0, __bsts_b_tail0));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_range(BValue __bsts_b_exclusiveUpper0) {
    return ___bsts_g_Bosatsu_l_Predef_l_int__loop(__bsts_b_exclusiveUpper0,
        alloc_enum0(0),
        alloc_boxed_pure_fn2(__bsts_t_lambda17));
}

BValue __bsts_t_closure18(BValue* __bstsi_slot, BValue __bsts_b_diff00, BValue __bsts_b_a0) {
    return alloc_struct2(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1),
            __bsts_b_diff00),
        call_fn2(__bstsi_slot[0],
            __bsts_b_a0,
            ___bsts_g_Bosatsu_l_Predef_l_sub(__bstsi_slot[1], __bsts_b_diff00)));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_range__fold(BValue __bsts_b_inclusiveLower0,
    BValue __bsts_b_exclusiveUpper0,
    BValue __bsts_b_init0,
    BValue __bsts_b_fn0) {
    BValue __bsts_b_diff0 = ___bsts_g_Bosatsu_l_Predef_l_sub(__bsts_b_exclusiveUpper0,
        __bsts_b_inclusiveLower0);
    BValue __bsts_l_captures19[2] = { __bsts_b_fn0, __bsts_b_exclusiveUpper0 };
    return ___bsts_g_Bosatsu_l_Predef_l_int__loop(__bsts_b_diff0,
        __bsts_b_init0,
        alloc_closure2(2, __bsts_l_captures19, __bsts_t_closure18));
}

static _Atomic BValue ___bsts_s_Bosatsu_l_Predef_l_string__Order = 0;

static BValue ___bsts_c_Bosatsu_l_Predef_l_string__Order() {
    return alloc_boxed_pure_fn2(___bsts_g_Bosatsu_l_Predef_l_cmp__String);
}

static BValue ___bsts_g_Bosatsu_l_Predef_l_string__Order() {
    return read_or_build(&___bsts_s_Bosatsu_l_Predef_l_string__Order,
        ___bsts_c_Bosatsu_l_Predef_l_string__Order);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_rotation(BValue __bsts_b_left0,
    BValue __bsts_b_right0,
    BValue __bsts_b_max__diff0) {
    BValue __bsts_a_6 = ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(___bsts_g_Bosatsu_l_Predef_l_sub(__bsts_b_left0,
            __bsts_b_right0),
        __bsts_b_max__diff0);
    BValue __bsts_a_9 = get_variant_value(__bsts_a_6) == 2 ? alloc_enum0(1) : alloc_enum0(0);
    if (get_variant_value(__bsts_a_9) == 1) {
        return alloc_enum0(2);
    }
    else {
        BValue __bsts_a_7 = ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(___bsts_g_Bosatsu_l_Predef_l_sub(__bsts_b_right0,
                __bsts_b_left0),
            __bsts_b_max__diff0);
        BValue __bsts_a_8 = get_variant_value(__bsts_a_7) == 2 ? alloc_enum0(1) : alloc_enum0(0);
        return get_variant_value(__bsts_a_8) == 1 ? alloc_enum0(0) : alloc_enum0(1);
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_branch(BValue __bsts_b_sz0,
    BValue __bsts_b_item0,
    BValue __bsts_b_left0,
    BValue __bsts_b_right0) {
    BValue __bsts_b_i0 = get_variant(__bsts_b_left0) == 0 ?
        bsts_integer_from_int(0) :
        get_enum_index(__bsts_b_left0, 1);
    BValue __bsts_b_j0 = get_variant(__bsts_b_right0) == 0 ?
        bsts_integer_from_int(0) :
        get_enum_index(__bsts_b_right0, 1);
    BValue __bsts_a_10 = ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(__bsts_b_i0, __bsts_b_j0);
    return alloc_enum5(1,
        __bsts_b_sz0,
        ___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(1),
            get_variant_value(__bsts_a_10) == 0 ? __bsts_b_j0 : __bsts_b_i0),
        __bsts_b_item0,
        __bsts_b_left0,
        __bsts_b_right0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_branch__s(BValue __bsts_b_item0,
    BValue __bsts_b_left0,
    BValue __bsts_b_right0) {
    return ___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(1),
            ___bsts_g_Bosatsu_l_Predef_l_add(get_variant(__bsts_b_left0) == 0 ?
                    bsts_integer_from_int(0) :
                    get_enum_index(__bsts_b_left0, 0),
                get_variant(__bsts_b_right0) == 0 ?
                    bsts_integer_from_int(0) :
                    get_enum_index(__bsts_b_right0, 0))),
        __bsts_b_item0,
        __bsts_b_left0,
        __bsts_b_right0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_balance(BValue __bsts_b_t0) {
    if (get_variant(__bsts_b_t0) == 0) {
        return alloc_enum0(0);
    }
    else {
        BValue __bsts_b_top__item0 = get_enum_index(__bsts_b_t0, 2);
        BValue __bsts_b_left0 = get_enum_index(__bsts_b_t0, 3);
        BValue __bsts_b_right0 = get_enum_index(__bsts_b_t0, 4);
        BValue __bsts_a_13 = ___bsts_g_Bosatsu_l_Predef_l_rotation(get_variant(__bsts_b_left0) == 0 ?
                bsts_integer_from_int(0) :
                get_enum_index(__bsts_b_left0, 1),
            get_variant(__bsts_b_right0) == 0 ?
                bsts_integer_from_int(0) :
                get_enum_index(__bsts_b_right0, 1),
            bsts_integer_from_int(1));
        if (get_variant_value(__bsts_a_13) == 1) {
            return __bsts_b_t0;
        }
        else if (get_variant_value(__bsts_a_13) == 2) {
            if (get_variant(__bsts_b_left0) == 0) {
                return __bsts_b_t0;
            }
            else {
                BValue __bsts_b_inner__item0 = get_enum_index(__bsts_b_left0, 2);
                BValue __bsts_b_left1 = get_enum_index(__bsts_b_left0, 3);
                BValue __bsts_b_left__right0 = get_enum_index(__bsts_b_left1, 4);
                BValue __bsts_a_11 = ___bsts_g_Bosatsu_l_Predef_l_rotation(get_variant(__bsts_b_left1) == 0 ?
                        bsts_integer_from_int(0) :
                        get_enum_index(__bsts_b_left1, 1),
                    get_variant(__bsts_b_left__right0) == 0 ?
                        bsts_integer_from_int(0) :
                        get_enum_index(__bsts_b_left__right0, 1),
                    bsts_integer_from_int(0));
                if (get_variant_value(__bsts_a_11) == 2) {
                    return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item0,
                        __bsts_b_left1,
                        ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                            __bsts_b_left__right0,
                            __bsts_b_right0));
                }
                else if (get_variant_value(__bsts_a_11) == 1) {
                    return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item0,
                        __bsts_b_left1,
                        ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                            __bsts_b_left__right0,
                            __bsts_b_right0));
                }
                else if (get_variant(__bsts_b_left__right0) == 0) {
                    return ___bsts_g_Bosatsu_l_Predef_l_trace(bsts_string_from_utf8_bytes_static(11,
                            "unreachable"),
                        __bsts_b_t0);
                }
                else {
                    BValue __bsts_b_lrv0 = get_enum_index(__bsts_b_left__right0, 2);
                    BValue __bsts_b_left__right__left0 = get_enum_index(__bsts_b_left__right0, 3);
                    BValue __bsts_b_left__right__right0 = get_enum_index(__bsts_b_left__right0, 4);
                    return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_lrv0,
                        ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item0,
                            __bsts_b_left1,
                            __bsts_b_left__right__left0),
                        ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                            __bsts_b_left__right__right0,
                            __bsts_b_right0));
                }
            }
        }
        else if (get_variant(__bsts_b_right0) == 0) {
            return __bsts_b_t0;
        }
        else {
            BValue __bsts_b_inner__item1 = get_enum_index(__bsts_b_right0, 2);
            BValue __bsts_b_right__left0 = get_enum_index(__bsts_b_right0, 3);
            BValue __bsts_b_right__right0 = get_enum_index(__bsts_b_right0, 4);
            BValue __bsts_a_12 = ___bsts_g_Bosatsu_l_Predef_l_rotation(get_variant(__bsts_b_right__left0) == 0 ?
                    bsts_integer_from_int(0) :
                    get_enum_index(__bsts_b_right__left0, 1),
                get_variant(__bsts_b_right__right0) == 0 ?
                    bsts_integer_from_int(0) :
                    get_enum_index(__bsts_b_right__right0, 1),
                bsts_integer_from_int(0));
            if (get_variant_value(__bsts_a_12) == 0) {
                return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item1,
                    ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                        __bsts_b_left0,
                        __bsts_b_right__left0),
                    __bsts_b_right__right0);
            }
            else if (get_variant_value(__bsts_a_12) == 1) {
                return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item1,
                    ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                        __bsts_b_left0,
                        __bsts_b_right__left0),
                    __bsts_b_right__right0);
            }
            else if (get_variant(__bsts_b_right__left0) == 0) {
                return ___bsts_g_Bosatsu_l_Predef_l_trace(bsts_string_from_utf8_bytes_static(11,
                        "unreachable"),
                    __bsts_b_t0);
            }
            else {
                BValue __bsts_b_right__left__key0 = get_enum_index(__bsts_b_right__left0, 2);
                BValue __bsts_b_right__left__right0 = get_enum_index(__bsts_b_right__left0, 3);
                BValue __bsts_b_right__left__left0 = get_enum_index(__bsts_b_right__left0, 4);
                return ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_right__left__key0,
                    ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_top__item0,
                        __bsts_b_left0,
                        __bsts_b_right__left__left0),
                    ___bsts_g_Bosatsu_l_Predef_l_branch__s(__bsts_b_inner__item1,
                        __bsts_b_right__left__right0,
                        __bsts_b_right__right0));
            }
        }
    }
}

BValue __bsts_t_closure__loop20(BValue* __bstsi_slot, BValue __bsts_b_tree0) {
    if (get_variant(__bsts_b_tree0) == 0) {
        return alloc_enum5(1,
            bsts_integer_from_int(1),
            bsts_integer_from_int(1),
            __bstsi_slot[1],
            alloc_enum0(0),
            alloc_enum0(0));
    }
    else {
        BValue __bsts_b_s0 = get_enum_index(__bsts_b_tree0, 0);
        BValue __bsts_b_h0 = get_enum_index(__bsts_b_tree0, 1);
        BValue __bsts_b_item00 = get_enum_index(__bsts_b_tree0, 2);
        BValue __bsts_b_left0 = get_enum_index(__bsts_b_tree0, 3);
        BValue __bsts_b_right0 = get_enum_index(__bsts_b_tree0, 4);
        BValue __bsts_a_14 = call_fn2(__bstsi_slot[0], __bstsi_slot[1], __bsts_b_item00);
        return get_variant_value(__bsts_a_14) == 1 ?
            alloc_enum5(1,
                __bsts_b_s0,
                __bsts_b_h0,
                __bstsi_slot[1],
                __bsts_b_left0,
                __bsts_b_right0) :
            (get_variant_value(__bsts_a_14) == 0 ?
                ___bsts_g_Bosatsu_l_Predef_l_balance(___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(1),
                            __bsts_b_s0),
                        __bsts_b_item00,
                        __bsts_t_closure__loop20(__bstsi_slot, __bsts_b_left0),
                        __bsts_b_right0)) :
                ___bsts_g_Bosatsu_l_Predef_l_balance(___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(1),
                            __bsts_b_s0),
                        __bsts_b_item00,
                        __bsts_b_left0,
                        __bsts_t_closure__loop20(__bstsi_slot, __bsts_b_right0))));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_add__item(BValue __bsts_b_ord0,
    BValue __bsts_b_tree0,
    BValue __bsts_b_item0) {
    BValue __bsts_b_fn0 = __bsts_b_ord0;
    BValue __bsts_l_captures21[2] = { __bsts_b_fn0, __bsts_b_item0 };
    BValue __bsts_b_loop0 = alloc_closure1(2, __bsts_l_captures21, __bsts_t_closure__loop20);
    return call_fn1(__bsts_b_loop0, __bsts_b_tree0);
}

BValue __bsts_t_closure22(BValue* __bstsi_slot, BValue __bsts_b_tree0) {
    BValue __bsts_a_16;
    BValue __bsts_a_17;
    BValue __bsts_a_19;
    __bsts_a_19 = __bsts_b_tree0;
    __bsts_a_16 = alloc_enum0(1);
    _Bool __bsts_l_cond23;
    __bsts_l_cond23 = get_variant_value(__bsts_a_16) == 1;
    while (__bsts_l_cond23) {
        if (get_variant(__bsts_a_19) == 0) {
            __bsts_a_16 = alloc_enum0(0);
            __bsts_a_17 = alloc_enum0(0);
        }
        else {
            BValue __bsts_b_key0 = get_enum_index(__bsts_a_19, 2);
            BValue __bsts_b_left0 = get_enum_index(__bsts_a_19, 3);
            BValue __bsts_b_right0 = get_enum_index(__bsts_a_19, 4);
            BValue __bsts_a_15 = call_fn2(__bstsi_slot[0], __bstsi_slot[1], __bsts_b_key0);
            if (get_variant_value(__bsts_a_15) == 1) {
                __bsts_a_16 = alloc_enum0(0);
                __bsts_a_17 = alloc_enum1(1, __bsts_b_key0);
            }
            else if (get_variant_value(__bsts_a_15) == 0) {
                BValue __bsts_a_18 = __bsts_b_left0;
                __bsts_a_19 = __bsts_a_18;
            }
            else {
                BValue __bsts_a_18 = __bsts_b_right0;
                __bsts_a_19 = __bsts_a_18;
            }
        }
        __bsts_l_cond23 = get_variant_value(__bsts_a_16) == 1;
    }
    return __bsts_a_17;
}

BValue ___bsts_g_Bosatsu_l_Predef_l_contains(BValue __bsts_b_ord0,
    BValue __bsts_b_tree0,
    BValue __bsts_b_item0) {
    BValue __bsts_b_fn0 = __bsts_b_ord0;
    BValue __bsts_l_captures24[2] = { __bsts_b_fn0, __bsts_b_item0 };
    BValue __bsts_b_loop0 = alloc_closure1(2, __bsts_l_captures24, __bsts_t_closure22);
    return call_fn1(__bsts_b_loop0, __bsts_b_tree0);
}

BValue __bsts_t_closure__loop25(BValue* __bstsi_slot, BValue __bsts_b_tree0) {
    if (get_variant(__bsts_b_tree0) == 0) {
        return alloc_enum0(0);
    }
    else {
        BValue __bsts_b_size0 = get_enum_index(__bsts_b_tree0, 0);
        BValue __bsts_b_key0 = get_enum_index(__bsts_b_tree0, 2);
        BValue __bsts_b_left0 = get_enum_index(__bsts_b_tree0, 3);
        BValue __bsts_b_right0 = get_enum_index(__bsts_b_tree0, 4);
        BValue __bsts_a_20 = call_fn2(__bstsi_slot[0], __bstsi_slot[1], __bsts_b_key0);
        return get_variant_value(__bsts_a_20) == 1 ?
            (get_variant(__bsts_b_right0) == 0 ?
                __bsts_b_left0 :
                ___bsts_g_Bosatsu_l_Predef_l_balance(___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1),
                            __bsts_b_size0),
                        __bsts_b_key0,
                        __bsts_b_left0,
                        __bsts_t_closure__loop25(__bstsi_slot, __bsts_b_right0)))) :
            (get_variant_value(__bsts_a_20) == 0 ?
                ___bsts_g_Bosatsu_l_Predef_l_balance(___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1),
                            __bsts_b_size0),
                        __bsts_b_key0,
                        __bsts_t_closure__loop25(__bstsi_slot, __bsts_b_left0),
                        __bsts_b_right0)) :
                ___bsts_g_Bosatsu_l_Predef_l_balance(___bsts_g_Bosatsu_l_Predef_l_branch(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1),
                            __bsts_b_size0),
                        __bsts_b_key0,
                        __bsts_b_left0,
                        __bsts_t_closure__loop25(__bstsi_slot, __bsts_b_right0))));
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_remove__item(BValue __bsts_b_ord0,
    BValue __bsts_b_tree0,
    BValue __bsts_b_item0) {
    BValue __bsts_b_fn0 = __bsts_b_ord0;
    BValue __bsts_l_captures26[2] = { __bsts_b_fn0, __bsts_b_item0 };
    BValue __bsts_b_loop0 = alloc_closure1(2, __bsts_l_captures26, __bsts_t_closure__loop25);
    return call_fn1(__bsts_b_loop0, __bsts_b_tree0);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_fold__right__Tree(BValue __bsts_b_t0,
    BValue __bsts_b_right__v0,
    BValue __bsts_b_fn0) {
    if (get_variant(__bsts_b_t0) == 0) {
        return __bsts_b_right__v0;
    }
    else {
        BValue __bsts_b_key0 = get_enum_index(__bsts_b_t0, 2);
        BValue __bsts_b_left0 = get_enum_index(__bsts_b_t0, 3);
        BValue __bsts_b_right0 = get_enum_index(__bsts_b_t0, 4);
        return ___bsts_g_Bosatsu_l_Predef_l_fold__right__Tree(__bsts_b_left0,
            call_fn2(__bsts_b_fn0,
                __bsts_b_key0,
                ___bsts_g_Bosatsu_l_Predef_l_fold__right__Tree(__bsts_b_right0,
                    __bsts_b_right__v0,
                    __bsts_b_fn0)),
            __bsts_b_fn0);
    }
}

BValue __bsts_t_closure27(BValue* __bstsi_slot, BValue __bsts_b_a0, BValue __bsts_b_b0) {
    BValue __bsts_b_k10 = get_struct_index(__bsts_b_a0, 0);
    BValue __bsts_b_k20 = get_struct_index(__bsts_b_b0, 0);
    return call_fn2(__bstsi_slot[0], __bsts_b_k10, __bsts_b_k20);
}

BValue ___bsts_g_Bosatsu_l_Predef_l_empty__Dict(BValue __bsts_b_comp0) {
    BValue __bsts_b_fn0 = __bsts_b_comp0;
    BValue __bsts_l_captures28[1] = { __bsts_b_fn0 };
    return alloc_struct2(alloc_closure2(1, __bsts_l_captures28, __bsts_t_closure27),
        alloc_enum0(0));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_add__key(BValue __bsts_b_dict0,
    BValue __bsts_b_key0,
    BValue __bsts_b_value0) {
    BValue __bsts_b_ord0 = get_struct_index(__bsts_b_dict0, 0);
    BValue __bsts_b_tree0 = get_struct_index(__bsts_b_dict0, 1);
    return alloc_struct2(__bsts_b_ord0,
        ___bsts_g_Bosatsu_l_Predef_l_add__item(__bsts_b_ord0,
            __bsts_b_tree0,
            alloc_struct2(__bsts_b_key0, __bsts_b_value0)));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_get__key(BValue __bsts_b_dict0, BValue __bsts_b_key0) {
    BValue __bsts_b_ord0 = get_struct_index(__bsts_b_dict0, 0);
    BValue __bsts_b_tree0 = get_struct_index(__bsts_b_dict0, 1);
    BValue __bsts_a_23;
    _Bool __bsts_l_cond30;
    if (get_variant(__bsts_b_tree0) == 1) {
        __bsts_a_23 = get_enum_index(__bsts_b_tree0, 2);
        __bsts_l_cond30 = 1;
    }
    else {
        __bsts_l_cond30 = 0;
    }
    if (__bsts_l_cond30) {
        BValue __bsts_b_v0 = get_struct_index(__bsts_a_23, 1);
        BValue __bsts_a_21 = ___bsts_g_Bosatsu_l_Predef_l_contains(__bsts_b_ord0,
            __bsts_b_tree0,
            alloc_struct2(__bsts_b_key0, __bsts_b_v0));
        BValue __bsts_a_22;
        _Bool __bsts_l_cond29;
        if (get_variant(__bsts_a_21) == 1) {
            __bsts_a_22 = get_enum_index(__bsts_a_21, 0);
            __bsts_l_cond29 = 1;
        }
        else {
            __bsts_l_cond29 = 0;
        }
        if (__bsts_l_cond29) {
            BValue __bsts_b_v1 = get_struct_index(__bsts_a_22, 1);
            return alloc_enum1(1, __bsts_b_v1);
        }
        else {
            return alloc_enum0(0);
        }
    }
    else {
        return alloc_enum0(0);
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_remove__key(BValue __bsts_b_dict0, BValue __bsts_b_key0) {
    BValue __bsts_b_ord0 = get_struct_index(__bsts_b_dict0, 0);
    BValue __bsts_b_tree0 = get_struct_index(__bsts_b_dict0, 1);
    BValue __bsts_a_24;
    _Bool __bsts_l_cond31;
    if (get_variant(__bsts_b_tree0) == 1) {
        __bsts_a_24 = get_enum_index(__bsts_b_tree0, 2);
        __bsts_l_cond31 = 1;
    }
    else {
        __bsts_l_cond31 = 0;
    }
    if (__bsts_l_cond31) {
        BValue __bsts_b_v0 = get_struct_index(__bsts_a_24, 1);
        return alloc_struct2(__bsts_b_ord0,
            ___bsts_g_Bosatsu_l_Predef_l_remove__item(__bsts_b_ord0,
                __bsts_b_tree0,
                alloc_struct2(__bsts_b_key0, __bsts_b_v0)));
    }
    else {
        return __bsts_b_dict0;
    }
}

BValue ___bsts_g_Bosatsu_l_Predef_l_items(BValue __bsts_b_dict0) {
    BValue __bsts_b_tree0 = get_struct_index(__bsts_b_dict0, 1);
    return ___bsts_g_Bosatsu_l_Predef_l_fold__right__Tree(__bsts_b_tree0,
        alloc_enum0(0),
        alloc_boxed_pure_fn2(__bsts_t_lambda0));
}

BValue ___bsts_g_Bosatsu_l_Predef_l_clear__Dict(BValue __bsts_b_dict0) {
    BValue __bsts_b_ord0 = get_struct_index(__bsts_b_dict0, 0);
    return alloc_struct2(__bsts_b_ord0, alloc_enum0(0));
}

BValue __bsts_t_closure32(BValue* __bstsi_slot, BValue __bsts_b_res0) {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(call_fn1(__bstsi_slot[0], __bsts_b_res0));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_map(BValue __bsts_b_prog0, BValue __bsts_b_fn0) {
    BValue __bsts_l_captures33[1] = { __bsts_b_fn0 };
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(__bsts_b_prog0,
        alloc_closure1(1, __bsts_l_captures33, __bsts_t_closure32));
}

BValue __bsts_t_closure34(BValue* __bstsi_slot, BValue __bsts_b_res0) {
    return ___bsts_g_Bosatsu_l_Prog_l_raise__error(call_fn1(__bstsi_slot[0], __bsts_b_res0));
}

BValue ___bsts_g_Bosatsu_l_Prog_l_map__err(BValue __bsts_b_prog0, BValue __bsts_b_fn0) {
    BValue __bsts_l_captures35[1] = { __bsts_b_fn0 };
    return ___bsts_g_Bosatsu_l_Prog_l_recover(__bsts_b_prog0,
        alloc_closure1(1, __bsts_l_captures35, __bsts_t_closure34));
}

BValue __bsts_t_closure36(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    return __bstsi_slot[0];
}

BValue ___bsts_g_Bosatsu_l_Prog_l_with__env(BValue __bsts_b_p0, BValue __bsts_b_env0) {
    BValue __bsts_l_captures37[1] = { __bsts_b_env0 };
    return ___bsts_g_Bosatsu_l_Prog_l_remap__env(__bsts_b_p0,
        alloc_closure1(1, __bsts_l_captures37, __bsts_t_closure36));
}

BValue __bsts_t_lambda38(BValue __bsts_b_a0) {
    return bsts_unit_value();
}

BValue ___bsts_g_Bosatsu_l_Prog_l_ignore__env(BValue __bsts_b_p0) {
    return ___bsts_g_Bosatsu_l_Prog_l_remap__env(__bsts_b_p0,
        alloc_boxed_pure_fn1(__bsts_t_lambda38));
}

BValue __bsts_t_closure39(BValue* __bstsi_slot, BValue __bsts_b_fn0) {
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(__bstsi_slot[0], __bsts_b_fn0);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_await(BValue __bsts_b_p0) {
    BValue __bsts_l_captures40[1] = { __bsts_b_p0 };
    return alloc_closure1(1, __bsts_l_captures40, __bsts_t_closure39);
}

BValue __bsts_t_closure41(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_apply__fix(__bsts_b_a0, __bstsi_slot[0]);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_recursive(BValue __bsts_b_fn0) {
    BValue __bsts_l_captures42[1] = { __bsts_b_fn0 };
    return alloc_closure1(1, __bsts_l_captures42, __bsts_t_closure41);
}

static _Atomic BValue ___bsts_s_Bosatsu_l_Prog_l_unit = 0;

static BValue ___bsts_c_Bosatsu_l_Prog_l_unit() {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_unit_value());
}

static BValue ___bsts_g_Bosatsu_l_Prog_l_unit() {
    return read_or_build(&___bsts_s_Bosatsu_l_Prog_l_unit, ___bsts_c_Bosatsu_l_Prog_l_unit);
}

BValue __bsts_t_closure45(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    return call_fn1(__bstsi_slot[0],
        ___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1), __bstsi_slot[1]));
}

BValue __bsts_t_closure44(BValue* __bstsi_slot, BValue __bsts_b_i0) {
    BValue __bsts_a_0 = ___bsts_g_Bosatsu_l_Predef_l_cmp__Int(__bsts_b_i0,
        bsts_integer_from_int(0));
    if (get_variant_value(__bsts_a_0) == 1) {
        return ___bsts_g_Bosatsu_l_Prog_l_println(bsts_string_from_utf8_bytes_static(5,
                "" "\xa" "done"));
    }
    else if (get_variant_value(__bsts_a_0) == 0) {
        return ___bsts_g_Bosatsu_l_Prog_l_println(bsts_string_from_utf8_bytes_static(5,
                "" "\xa" "done"));
    }
    else {
        BValue __bsts_l_captures46[2] = { __bstsi_slot[0], __bsts_b_i0 };
        return ___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_print(___bsts_g_Bosatsu_l_Predef_l_concat__String(alloc_enum2(1,
                        ___bsts_g_Bosatsu_l_Predef_l_int__to__String(__bsts_b_i0),
                        alloc_enum2(1,
                            bsts_string_from_utf8_bytes_static(2, ", "),
                            alloc_enum0(0))))),
            alloc_closure1(2, __bsts_l_captures46, __bsts_t_closure45));
    }
}

BValue __bsts_t_lambda43(BValue __bsts_b_loop0) {
    BValue __bsts_l_captures47[1] = { __bsts_b_loop0 };
    return alloc_closure1(1, __bsts_l_captures47, __bsts_t_closure44);
}

BValue ___bsts_g_Bosatsu_l_Prog_l_count__down(BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_apply__fix(__bsts_b_a0,
        alloc_boxed_pure_fn1(__bsts_t_lambda43));
}

BValue __bsts_t_lambda49(BValue __bsts_b_s0, BValue __bsts_b_item0) {
    return bsts_string_equals(__bsts_b_s0, bsts_string_from_utf8_bytes_static(0, "")) ?
        __bsts_b_item0 :
        ___bsts_g_Bosatsu_l_Predef_l_concat__String(alloc_enum2(1,
                __bsts_b_s0,
                alloc_enum2(1,
                    bsts_string_from_utf8_bytes_static(2, ", "),
                    alloc_enum2(1, __bsts_b_item0, alloc_enum0(0)))));
}

BValue __bsts_t_lambda51(BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_string_from_utf8_bytes_static(0, ""));
}

BValue __bsts_t_closure50(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(__bstsi_slot[0],
        alloc_boxed_pure_fn1(__bsts_t_lambda51));
}

BValue __bsts_t_lambda56(BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(0));
}

BValue __bsts_t_closure55(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_println(___bsts_g_Bosatsu_l_Predef_l_concat__String(alloc_enum2(1,
                    bsts_string_from_utf8_bytes_static(7, "args = "),
                    alloc_enum2(1, __bstsi_slot[0], alloc_enum0(0))))),
        alloc_boxed_pure_fn1(__bsts_t_lambda56));
}

BValue __bsts_t_closure54(BValue* __bstsi_slot, BValue __bsts_b_a0) {
    BValue __bsts_l_captures57[1] = { __bstsi_slot[0] };
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_count__down(bsts_integer_from_int(10)),
        alloc_closure1(1, __bsts_l_captures57, __bsts_t_closure55));
}

BValue __bsts_t_closure53(BValue* __bstsi_slot, BValue __bsts_b_stdin0) {
    BValue __bsts_l_captures58[1] = { __bstsi_slot[0] };
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_println(___bsts_g_Bosatsu_l_Predef_l_concat__String(alloc_enum2(1,
                    bsts_string_from_utf8_bytes_static(13, "found stdin: "),
                    alloc_enum2(1, __bsts_b_stdin0, alloc_enum0(0))))),
        alloc_closure1(1, __bsts_l_captures58, __bsts_t_closure54));
}

BValue __bsts_t_lambda48(BValue __bsts_b_args0) {
    BValue __bsts_b_arg__str0 = ___bsts_g_Bosatsu_l_Predef_l_foldl__List(__bsts_b_args0,
        bsts_string_from_utf8_bytes_static(0, ""),
        alloc_boxed_pure_fn2(__bsts_t_lambda49));
    BValue __bsts_b_prog0 = ___bsts_g_Bosatsu_l_Prog_l_println(bsts_string_from_utf8_bytes_static(21,
            "<failed to read stdin"));
    BValue __bsts_l_captures52[1] = { __bsts_b_prog0 };
    BValue __bsts_l_captures59[1] = { __bsts_b_arg__str0 };
    return ___bsts_g_Bosatsu_l_Prog_l_ignore__env(___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_recover(___bsts_g_Bosatsu_l_Prog_l_read__stdin__utf8__bytes(bsts_integer_from_int(1000)),
                alloc_closure1(1, __bsts_l_captures52, __bsts_t_closure50)),
            alloc_closure1(1, __bsts_l_captures59, __bsts_t_closure53)));
}

static _Atomic BValue ___bsts_s_Bosatsu_l_Prog_l_to__run = 0;

static BValue ___bsts_c_Bosatsu_l_Prog_l_to__run() {
    return ___bsts_g_Bosatsu_l_Prog_l_flat__map(___bsts_g_Bosatsu_l_Prog_l_read__env(),
        alloc_boxed_pure_fn1(__bsts_t_lambda48));
}

static BValue ___bsts_g_Bosatsu_l_Prog_l_to__run() {
    return read_or_build(&___bsts_s_Bosatsu_l_Prog_l_to__run, ___bsts_c_Bosatsu_l_Prog_l_to__run);
}

BValue __bsts_t_lambda60(BValue __bsts_b_i0, BValue __bsts_b_acc0) {
    BValue __bsts_b_a0 = get_struct_index(__bsts_b_acc0, 0);
    BValue __bsts_b_b0 = get_struct_index(__bsts_b_acc0, 1);
    return alloc_struct2(___bsts_g_Bosatsu_l_Predef_l_add(bsts_integer_from_int(-1), __bsts_b_i0),
        alloc_struct2(__bsts_b_b0, ___bsts_g_Bosatsu_l_Predef_l_add(__bsts_b_a0, __bsts_b_b0)));
}

BValue ___bsts_g_Demo_l_Fibonacci_l_fib(BValue __bsts_b_n0) {
    BValue __bsts_a_0 = ___bsts_g_Bosatsu_l_Predef_l_int__loop(__bsts_b_n0,
        alloc_struct2(bsts_integer_from_int(0), bsts_integer_from_int(1)),
        alloc_boxed_pure_fn2(__bsts_t_lambda60));
    return get_struct_index(__bsts_a_0, 0);
}

BValue __bsts_t_lambda61(BValue __bsts_b_a0) {
    return ___bsts_g_Bosatsu_l_Prog_l_pure(bsts_integer_from_int(0));
}

static _Atomic BValue ___bsts_s_Demo_l_Fibonacci_l_main = 0;

static BValue ___bsts_c_Demo_l_Fibonacci_l_main() {
    BValue __bsts_b_result0 = ___bsts_g_Demo_l_Fibonacci_l_fib(bsts_integer_from_int(20));
    return ___bsts_g_Bosatsu_l_Prog_l_ignore__env(call_fn1(___bsts_g_Bosatsu_l_Prog_l_await(___bsts_g_Bosatsu_l_Prog_l_ignore__env(___bsts_g_Bosatsu_l_Prog_l_println(___bsts_g_Bosatsu_l_Predef_l_concat__String(alloc_enum2(1,
                                bsts_string_from_utf8_bytes_static(10, "fib(20) = "),
                                alloc_enum2(1,
                                    ___bsts_g_Bosatsu_l_Predef_l_int__to__String(__bsts_b_result0),
                                    alloc_enum0(0))))))),
            alloc_boxed_pure_fn1(__bsts_t_lambda61)));
}

static BValue ___bsts_g_Demo_l_Fibonacci_l_main() {
    return read_or_build(&___bsts_s_Demo_l_Fibonacci_l_main, ___bsts_c_Demo_l_Fibonacci_l_main);
}

int main(int argc, char** argv) {
    GC_init();
    init_statics();
    atexit(free_statics);
    BValue main_value = ___bsts_g_Demo_l_Fibonacci_l_main();
    return bsts_Bosatsu_Prog_run_main(main_value, argc, argv);
}