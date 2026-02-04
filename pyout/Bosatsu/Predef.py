

def build_List(___bfn0):
    return ___bfn0(lambda ___t0, ___t1: (1, ___t0, ___t1), (0,))

def foldr_List(___blist0, ___bfn1, ___bacc0):
    def ___bloop0(___blist1):
        if ___blist1[0] == 0:
            return ___bacc0
        else:
            return ___bfn1(___blist1[1], ___bloop0(___blist1[2]))
    return ___bloop0(___blist0)

def foldl_List(___blst0, ___bitem0, ___bfn2):
    def ___bloop1(___blst1, ___bitem1):
        ___a3 = ___blst1
        ___a5 = ___bitem1
        ___a0 = 1
        ___t2 = ___a0 == 1
        while ___t2:
            if ___a3[0] == 0:
                ___a0 = 0
                ___a1 = ___a5
            else:
                ___bhead0 = ___a3[1]
                ___btail0 = ___a3[2]
                ___a2 = ___btail0
                ___a4 = ___bfn2(___a5, ___bhead0)
                ___a3 = ___a2
                ___a5 = ___a4
            ___t2 = ___a0 == 1
        return ___a1
    return ___bloop1(___blst0, ___bitem0)

def reverse_concat(___bfront0, ___bback0):
    return foldl_List(___bfront0, ___bback0, lambda ___btail1, ___bh1: (1, ___bh1, ___btail1))

def reverse(___bas0):
    return reverse_concat(___bas0, (0,))

def concat(___bfront1, ___bback1):
    return ___bfront1 if ___bback1[0] == 0 else reverse_concat(reverse_concat(___bfront1, (0,)),
        ___bback1)

def map_List(___blst2, ___bfn3):
    return reverse(foldl_List(___blst2, (0,), lambda ___bt1, ___ba0: (1, ___bfn3(___ba0), ___bt1)))

def flat_map_List(___blst3, ___bfn4):
    def ___t5(___bt2, ___ba1):
        return foldl_List(___bfn4(___ba1), ___bt2, lambda ___btail2, ___bh2: (1, ___bh2, ___btail2))
    return reverse(foldl_List(___blst3, (0,), ___t5))

def replicate_List(___bitem2, ___bcnt0):
    ___t8 = 0 < ___bcnt0
    ___t9 = (0,)
    ___t10 = ___bcnt0
    ___t11 = (0,)
    while ___t8:
        ___t12 = ___t10 + -1
        ___t11 = (1, ___bitem2, ___t11)
        ___t8 = (0 < ___t12) and (___t12 < ___t10)
        ___t10 = ___t12
    return ___t11

def uncurry2(___bf0):
    return lambda ___bx10, ___bx20: ___bf0(___bx10)(___bx20)

def uncurry3(___bf1):
    return lambda ___bx11, ___bx21, ___bx30: ___bf1(___bx11)(___bx21)(___bx30)

def range(___bexclusiveUpper0):
    def ___t15(___bi1, ___btail3):
        ___binext0 = ___bi1 + -1
        return (___binext0, (1, ___binext0, ___btail3))
    ___t16 = 0 < ___bexclusiveUpper0
    ___t17 = (0,)
    ___t18 = ___bexclusiveUpper0
    ___t19 = (0,)
    while ___t16:
        ___t17 = ___t15(___t18, ___t19)
        ___t20 = ___t17[0]
        ___t19 = ___t17[1]
        ___t16 = (0 < ___t20) and (___t20 < ___t18)
        ___t18 = ___t20
    return ___t19

def range_fold(___binclusiveLower0, ___bexclusiveUpper1, ___binit0, ___bfn5):
    ___bdiff0 = ___bexclusiveUpper1 - ___binclusiveLower0
    ___t22 = 0 < ___bdiff0
    ___t23 = ___binit0
    ___t24 = ___bdiff0
    ___t25 = ___binit0
    while ___t22:
        ___t26 = ___t24 + -1
        ___t25 = ___bfn5(___t25, ___bexclusiveUpper1 - ___t24)
        ___t22 = (0 < ___t26) and (___t26 < ___t24)
        ___t24 = ___t26
    return ___t25

string_Order = lambda ___t27, ___t28: 0 if ___t27 < ___t28 else 1 if ___t27 == ___t28 else 2

def rotation(___bleft0, ___bright0, ___bmax__diff0):
    ___a6 = 0 if (___bleft0 - ___bright0) < ___bmax__diff0 else 1 if (___bleft0 - ___bright0) == ___bmax__diff0 else 2
    ___a9 = ___a6 == 2
    if ___a9 == 1:
        return 2
    else:
        return ((0 if (___bright0 - ___bleft0) < ___bmax__diff0 else 1 if (___bright0 - ___bleft0) == ___bmax__diff0 else 2) == 2) != 1

def branch(___bsz0, ___bitem3, ___bleft1, ___bright1):
    ___bi2 = 0 if ___bleft1[0] == 0 else ___bleft1[2]
    ___bj0 = 0 if ___bright1[0] == 0 else ___bright1[2]
    return (1,
        ___bsz0,
        (___bj0 if (0 if ___bi2 < ___bj0 else 1 if ___bi2 == ___bj0 else 2) == 0 else ___bi2) + 1,
        ___bitem3,
        ___bleft1,
        ___bright1)

def branch_s(___bitem4, ___bleft2, ___bright2):
    return branch((0 if ___bleft2[0] == 0 else ___bleft2[1]) + (0 if ___bright2[0] == 0 else ___bright2[1]) + 1,
        ___bitem4,
        ___bleft2,
        ___bright2)

def balance(___bt3):
    if ___bt3[0] == 0:
        return (0,)
    else:
        ___btop__item0 = ___bt3[3]
        ___bleft3 = ___bt3[4]
        ___bright3 = ___bt3[5]
        ___a13 = rotation(0 if ___bleft3[0] == 0 else ___bleft3[2],
            0 if ___bright3[0] == 0 else ___bright3[2],
            1)
        if ___a13 == 1:
            return ___bt3
        elif ___a13 == 2:
            if ___bleft3[0] == 0:
                return ___bt3
            else:
                ___binner__item0 = ___bleft3[3]
                ___bleft4 = ___bleft3[4]
                ___bleft__right0 = ___bleft4[5]
                ___a11 = rotation(0 if ___bleft4[0] == 0 else ___bleft4[2],
                    0 if ___bleft__right0[0] == 0 else ___bleft__right0[2],
                    0)
                if ___a11 == 2:
                    return branch_s(___binner__item0,
                        ___bleft4,
                        branch_s(___btop__item0, ___bleft__right0, ___bright3))
                elif ___a11 == 1:
                    return branch_s(___binner__item0,
                        ___bleft4,
                        branch_s(___btop__item0, ___bleft__right0, ___bright3))
                elif ___bleft__right0[0] == 0:
                    print("unreachable", ___bt3)
                    return ___bt3
                else:
                    return branch_s(___bleft__right0[3],
                        branch_s(___binner__item0, ___bleft4, ___bleft__right0[4]),
                        branch_s(___btop__item0, ___bleft__right0[5], ___bright3))
        elif ___bright3[0] == 0:
            return ___bt3
        else:
            ___binner__item1 = ___bright3[3]
            ___bright__left0 = ___bright3[4]
            ___bright__right0 = ___bright3[5]
            ___a12 = rotation(0 if ___bright__left0[0] == 0 else ___bright__left0[2],
                0 if ___bright__right0[0] == 0 else ___bright__right0[2],
                0)
            if ___a12 == 0:
                return branch_s(___binner__item1,
                    branch_s(___btop__item0, ___bleft3, ___bright__left0),
                    ___bright__right0)
            elif ___a12 == 1:
                return branch_s(___binner__item1,
                    branch_s(___btop__item0, ___bleft3, ___bright__left0),
                    ___bright__right0)
            elif ___bright__left0[0] == 0:
                print("unreachable", ___bt3)
                return ___bt3
            else:
                return branch_s(___bright__left0[3],
                    branch_s(___btop__item0, ___bleft3, ___bright__left0[5]),
                    branch_s(___binner__item1, ___bright__left0[4], ___bright__right0))

def add_item(___bord0, ___btree0, ___bitem5):
    ___bfn6 = ___bord0
    def ___bloop2(___btree1):
        if ___btree1[0] == 0:
            return (1, 1, 1, ___bitem5, (0,), (0,))
        else:
            ___bs0 = ___btree1[1]
            ___bh3 = ___btree1[2]
            ___bitem00 = ___btree1[3]
            ___bleft5 = ___btree1[4]
            ___bright4 = ___btree1[5]
            ___a14 = ___bfn6(___bitem5, ___bitem00)
            return (1,
                ___bs0,
                ___bh3,
                ___bitem5,
                ___bleft5,
                ___bright4) if ___a14 == 1 else balance(branch(___bs0 + 1,
                    ___bitem00,
                    ___bloop2(___bleft5),
                    ___bright4)) if ___a14 == 0 else balance(branch(___bs0 + 1,
                    ___bitem00,
                    ___bleft5,
                    ___bloop2(___bright4)))
    return ___bloop2(___btree0)

def contains(___bord1, ___btree2, ___bitem6):
    ___bfn7 = ___bord1
    def ___bloop3(___btree3):
        ___a19 = ___btree3
        ___a16 = 1
        ___t29 = ___a16 == 1
        while ___t29:
            if ___a19[0] == 0:
                ___a16 = 0
                ___a17 = (0,)
            else:
                ___bkey0 = ___a19[3]
                ___bleft6 = ___a19[4]
                ___bright5 = ___a19[5]
                ___a15 = ___bfn7(___bitem6, ___bkey0)
                if ___a15 == 1:
                    ___a16 = 0
                    ___a17 = (1, ___bkey0)
                elif ___a15 == 0:
                    ___a18 = ___bleft6
                    ___a19 = ___a18
                else:
                    ___a18 = ___bright5
                    ___a19 = ___a18
            ___t29 = ___a16 == 1
        return ___a17
    return ___bloop3(___btree2)

def remove_item(___bord2, ___btree4, ___bitem7):
    ___bfn8 = ___bord2
    def ___bloop4(___btree5):
        if ___btree5[0] == 0:
            return (0,)
        else:
            ___bsize0 = ___btree5[1]
            ___bkey1 = ___btree5[3]
            ___bleft7 = ___btree5[4]
            ___bright6 = ___btree5[5]
            ___a20 = ___bfn8(___bitem7, ___bkey1)
            return (___bleft7 if ___bright6[0] == 0 else balance(branch(___bsize0 + -1,
                    ___bkey1,
                    ___bleft7,
                    ___bloop4(___bright6)))) if ___a20 == 1 else balance(branch(___bsize0 + -1,
                    ___bkey1,
                    ___bloop4(___bleft7),
                    ___bright6)) if ___a20 == 0 else balance(branch(___bsize0 + -1,
                    ___bkey1,
                    ___bleft7,
                    ___bloop4(___bright6)))
    return ___bloop4(___btree4)

def fold_right_Tree(___bt4, ___bright__v0, ___bfn9):
    if ___bt4[0] == 0:
        return ___bright__v0
    else:
        return fold_right_Tree(___bt4[4],
            ___bfn9(___bt4[3], fold_right_Tree(___bt4[5], ___bright__v0, ___bfn9)),
            ___bfn9)

def empty_Dict(___bcomp0):
    ___bfn10 = ___bcomp0
    def ___t30(___ba3, ___bb0):
        return ___bfn10(___ba3[0], ___bb0[0])
    return (___t30, (0,))

def add_key(___bdict0, ___bkey3, ___bvalue0):
    ___bord3 = ___bdict0[0]
    return (___bord3, add_item(___bord3, ___bdict0[1], (___bkey3, ___bvalue0)))

def get_key(___bdict1, ___bkey4):
    ___bord4 = ___bdict1[0]
    ___btree7 = ___bdict1[1]
    if ___btree7[0] == 1:
        ___a23 = ___btree7[3]
        ___bv0 = ___a23[1]
        ___a21 = contains(___bord4, ___btree7, (___bkey4, ___bv0))
        if ___a21[0] == 1:
            return (1, ___a21[1][1])
        else:
            return (0,)
    else:
        return (0,)

def remove_key(___bdict2, ___bkey5):
    ___bord5 = ___bdict2[0]
    ___btree8 = ___bdict2[1]
    if ___btree8[0] == 1:
        return (___bord5, remove_item(___bord5, ___btree8, (___bkey5, ___btree8[3][1])))
    else:
        return ___bdict2

def items(___bdict3):
    return fold_right_Tree(___bdict3[1], (0,), lambda ___t31, ___t32: (1, ___t31, ___t32))

def clear_Dict(___bdict4):
    return (___bdict4[0], (0,))