import Bosatsu.Bool as ___iBool1
import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest2

def any(___bas0):
    ___a2 = 0
    ___a4 = ___bas0
    ___t1 = ___a4[0] == 1
    while ___t1:
        ___a0 = ___a4
        if ___a0[0] == 1:
            ___a1 = ___a0[1]
            ___t0 = ___a1 == 1
        else:
            ___t0 = False
        if ___t0:
            ___a4 = (0,)
            ___a2 = 1
        else:
            ___a4 = ___a4[2]
        ___t1 = ___a4[0] == 1
    return ___a2 == 1

def for_all(___bxs0, ___bfn0):
    ___a9 = ___bxs0
    ___a11 = ___bfn0
    ___a6 = 1
    ___t2 = ___a6 == 1
    while ___t2:
        if ___a9[0] == 0:
            ___a6 = 0
            ___a7 = 1
        else:
            ___bhead0 = ___a9[1]
            ___btail0 = ___a9[2]
            ___a5 = ___a11(___bhead0)
            if ___a5 == 1:
                ___a8 = ___btail0
                ___a9 = ___a8
            else:
                ___a6 = 0
                ___a7 = 0
        ___t2 = ___a6 == 1
    return ___a7

def sum(___bas1):
    return ___iPredef0.foldl_List(___bas1, 0, lambda ___t3, ___t4: ___t3 + ___t4)

def exists(___bxs1, ___bfn1):
    ___a16 = ___bxs1
    ___a18 = ___bfn1
    ___a13 = 1
    ___t5 = ___a13 == 1
    while ___t5:
        if ___a16[0] == 0:
            ___a13 = 0
            ___a14 = 0
        else:
            ___bhead1 = ___a16[1]
            ___btail1 = ___a16[2]
            ___a12 = ___a18(___bhead1)
            if ___a12 == 1:
                ___a13 = 0
                ___a14 = 1
            else:
                ___a15 = ___btail1
                ___a16 = ___a15
        ___t5 = ___a13 == 1
    return ___a14

def uncons(___bxs2):
    if ___bxs2[0] == 0:
        return (0,)
    else:
        return (1, (___bxs2[1], ___bxs2[2]))

def head(___bxs3):
    if ___bxs3[0] == 0:
        return (0,)
    else:
        return (1, ___bxs3[1])

def eq_List(___bfn2):
    def ___t6(___ba0, ___bb0):
        if ___ba0[0] == 0:
            return ___bb0[0] == 0
        else:
            ___bah0 = ___ba0[1]
            ___bat0 = ___ba0[2]
            if ___bb0[0] == 0:
                return 0
            else:
                return eq_List(___bfn2)(___bat0, ___bb0[2]) if ___bfn2(___bah0,
                    ___bb0[1]) == 1 else 0
    return ___t6

def zip(___bleft0, ___bright0):
    if ___bleft0[0] == 0:
        return (0,)
    else:
        ___bah1 = ___bleft0[1]
        ___bat1 = ___bleft0[2]
        if ___bright0[0] == 0:
            return (0,)
        else:
            return (1, (___bah1, ___bright0[1]), zip(___bat1, ___bright0[2]))

def size1(___blist0, ___bacc0):
    ___a23 = ___blist0
    ___a25 = ___bacc0
    ___a20 = 1
    ___t7 = ___a20 == 1
    while ___t7:
        if ___a23[0] == 0:
            ___a20 = 0
            ___a21 = ___a25
        else:
            ___bt1 = ___a23[2]
            ___a22 = ___bt1
            ___a24 = ___a25 + 1
            ___a23 = ___a22
            ___a25 = ___a24
        ___t7 = ___a20 == 1
    return ___a21

def size(___blist1):
    return size1(___blist1, 0)

def sort(___bord0, ___blist2):
    ___bto__Fn0 = ___bord0
    def ___bloop0(___blist3, ___bsz0):
        if ___bsz0 == 0:
            return ___blist3
        else:
            ___a30 = ___bsz0 - 1
            ___bn0 = ___a30
            if ___blist3[0] == 0:
                return (0,)
            else:
                ___bh2 = ___blist3[1]
                ___bt2 = ___blist3[2]
                ___t9 = (___bto__Fn0, ___bh2)
                def ___t8(___bta0):
                    return (1, ___bta0, (0,)) if (___t9[0](___bta0, ___t9[1]) == 0) == 1 else (0,)
                ___blesser0 = ___iPredef0.flat_map_List(___bt2, ___t8)
                ___t11 = (___bto__Fn0, ___bh2)
                def ___t10(___bta1):
                    ___a28 = ___t11[0](___bta1, ___t11[1])
                    return (1, ___bta1, (0,)) if (1 if ___a28 == 2 else ___a28 == 1) == 1 else (0,)
                return ___iPredef0.concat(___bloop0(___blesser0, ___bn0),
                    (1, ___bh2, ___bloop0(___iPredef0.flat_map_List(___bt2, ___t10), ___bn0)))
    return ___bloop0(___blist2, size1(___blist2, 0))

___n_z__g__z_ = eq_List(lambda ___t12, ___t13: ___t12 == ___t13)

___a31 = (1, 1)
if ___a31[0] == 1:
    ___a32 = ___a31[1]
    ___t14 = ___a32 == 1
else:
    ___t14 = False
headTest = (0, ___t14, "head test")

___a33 = (1, (1, (1, 2, (1, 3, (0,)))))
if ___a33[0] == 1:
    ___a34 = ___a33[1]
    ___a36 = ___a34[1]
    ___a35 = ___a34[0]
    if ___a35 == 1:
        if ___a36[0] == 1:
            ___a37 = ___a36[1]
            ___a38 = ___a36[2]
            if ___a37 == 2:
                if ___a38[0] == 1:
                    ___a39 = ___a38[1]
                    ___a40 = ___a38[2]
                    ___t15 = (___a39 == 3) and (___a40[0] == 0)
                else:
                    ___t15 = False
            else:
                ___t15 = False
        else:
            ___t15 = False
    else:
        ___t15 = False
else:
    ___t15 = False
unconsTest = (0, ___t15, "uncons test")

___a41 = zip((1, 1, (1, 2, (0,))), (1, "1", (1, "2", (0,))))
if ___a41[0] == 1:
    ___a42 = ___a41[1]
    ___a45 = ___a41[2]
    ___a44 = ___a42[1]
    ___a43 = ___a42[0]
    if (___a43 == 1) and (___a44 == "1"):
        if ___a45[0] == 1:
            ___a46 = ___a45[1]
            ___a49 = ___a45[2]
            ___a48 = ___a46[1]
            ___a47 = ___a46[0]
            ___t16 = (___a47 == 2) and (___a48 == "2") and (___a49[0] == 0)
        else:
            ___t16 = False
    else:
        ___t16 = False
else:
    ___t16 = False
___a50 = zip((1, 1, (0,)), (1, "1", (1, "2", (0,))))
if ___a50[0] == 1:
    ___a51 = ___a50[1]
    ___a54 = ___a50[2]
    ___a53 = ___a51[1]
    ___a52 = ___a51[0]
    ___t17 = (___a52 == 1) and (___a53 == "1") and (___a54[0] == 0)
else:
    ___t17 = False
___a55 = zip((1, 1, (1, 2, (0,))), (1, "1", (0,)))
if ___a55[0] == 1:
    ___a56 = ___a55[1]
    ___a59 = ___a55[2]
    ___a58 = ___a56[1]
    ___a57 = ___a56[0]
    ___t18 = (___a57 == 1) and (___a58 == "1") and (___a59[0] == 0)
else:
    ___t18 = False
zipTest = (1,
    "zip tests",
    (1,
        (0, ___t16, "same size"),
        (1, (0, ___t17, "left smaller"), (1, (0, ___t18, "right smaller"), (0,)))))

___a60 = sort(lambda ___t19, ___t20: 0 if ___t19 < ___t20 else 1 if ___t19 == ___t20 else 2,
    (1, 3, (1, 1, (1, 2, (0,)))))
if ___a60[0] == 1:
    ___a61 = ___a60[1]
    ___a62 = ___a60[2]
    if ___a61 == 1:
        if ___a62[0] == 1:
            ___a63 = ___a62[1]
            ___a64 = ___a62[2]
            if ___a63 == 2:
                if ___a64[0] == 1:
                    ___a65 = ___a64[1]
                    ___a66 = ___a64[2]
                    ___t21 = (___a65 == 3) and (___a66[0] == 0)
                else:
                    ___t21 = False
            else:
                ___t21 = False
        else:
            ___t21 = False
    else:
        ___t21 = False
else:
    ___t21 = False
___a67 = sort(lambda ___t22, ___t23: 0 if ___t22 < ___t23 else 1 if ___t22 == ___t23 else 2,
    (1, 1, (1, 2, (1, 3, (0,)))))
if ___a67[0] == 1:
    ___a68 = ___a67[1]
    ___a69 = ___a67[2]
    if ___a68 == 1:
        if ___a69[0] == 1:
            ___a70 = ___a69[1]
            ___a71 = ___a69[2]
            if ___a70 == 2:
                if ___a71[0] == 1:
                    ___a72 = ___a71[1]
                    ___a73 = ___a71[2]
                    ___t24 = (___a72 == 3) and (___a73[0] == 0)
                else:
                    ___t24 = False
            else:
                ___t24 = False
        else:
            ___t24 = False
    else:
        ___t24 = False
else:
    ___t24 = False
___a74 = sort(lambda ___t25, ___t26: 0 if ___t25 < ___t26 else 1 if ___t25 == ___t26 else 2,
    (1, 2, (1, 3, (1, 1, (0,)))))
if ___a74[0] == 1:
    ___a75 = ___a74[1]
    ___a76 = ___a74[2]
    if ___a75 == 1:
        if ___a76[0] == 1:
            ___a77 = ___a76[1]
            ___a78 = ___a76[2]
            if ___a77 == 2:
                if ___a78[0] == 1:
                    ___a79 = ___a78[1]
                    ___a80 = ___a78[2]
                    ___t27 = (___a79 == 3) and (___a80[0] == 0)
                else:
                    ___t27 = False
            else:
                ___t27 = False
        else:
            ___t27 = False
    else:
        ___t27 = False
else:
    ___t27 = False
___a81 = sort(lambda ___t28, ___t29: 0 if ___t28 < ___t29 else 1 if ___t28 == ___t29 else 2,
    (1, 1, (1, 2, (1, 1, (0,)))))
if ___a81[0] == 1:
    ___a82 = ___a81[1]
    ___a83 = ___a81[2]
    if ___a82 == 1:
        if ___a83[0] == 1:
            ___a84 = ___a83[1]
            ___a85 = ___a83[2]
            if ___a84 == 1:
                if ___a85[0] == 1:
                    ___a86 = ___a85[1]
                    ___a87 = ___a85[2]
                    ___t30 = (___a86 == 2) and (___a87[0] == 0)
                else:
                    ___t30 = False
            else:
                ___t30 = False
        else:
            ___t30 = False
    else:
        ___t30 = False
else:
    ___t30 = False
sortTest = (1,
    "sort tests",
    (1,
        (0, ___t21, "3, 1, 2"),
        (1,
            (0, ___t24, "1, 2, 3"),
            (1, (0, ___t27, "2, 3, 1"), (1, (0, ___t30, "1, 2, 1"), (0,))))))

___a88 = "0"
___t31 = []
___t32 = (1, "int_to_String(0) == ", (1, "0", (0,)))
while ___t32[0] != 0:
    ___t31.append(___t32[1])
    ___t32 = ___t32[2]
___a89 = "1"
___t33 = []
___t34 = (1, "int_to_String(1) == ", (1, "1", (0,)))
while ___t34[0] != 0:
    ___t33.append(___t34[1])
    ___t34 = ___t34[2]
___a90 = "2"
___t35 = []
___t36 = (1, "int_to_String(2) == ", (1, "2", (0,)))
while ___t36[0] != 0:
    ___t35.append(___t36[1])
    ___t36 = ___t36[2]
stringTests = (1,
    "string tests",
    (1,
        (0, ___a88 == "0", "".join(___t31)),
        (1, (0, ___a89 == "1", "".join(___t33)), (1, (0, ___a90 == "2", "".join(___t35)), (0,)))))

___t38 = 0 < 5
___t39 = 0
___t40 = 5
___t41 = 0
while ___t38:
    ___t42 = ___t40 + -1
    ___t41 = ___t40 + ___t41
    ___t38 = (0 < ___t42) and (___t42 < ___t40)
    ___t40 = ___t42
il = ___t41

___t43 = []
___t44 = (1, "2 - 3 = ", (1, "-1", (0,)))
while ___t44[0] != 0:
    ___t43.append(___t44[1])
    ___t44 = ___t44[2]
___t45 = []
___t46 = (1, "int_loop test ", (1, il.__str__(), (0,)))
while ___t46[0] != 0:
    ___t45.append(___t46[1])
    ___t46 = ___t46[2]
___a91 = ___iPredef0.range(0)
___a92 = ___iPredef0.range(1)
if ___a92[0] == 1:
    ___a93 = ___a92[1]
    ___a94 = ___a92[2]
    ___t47 = (___a93 == 0) and (___a94[0] == 0)
else:
    ___t47 = False
___a95 = ___iPredef0.range(2)
if ___a95[0] == 1:
    ___a96 = ___a95[1]
    ___a97 = ___a95[2]
    if ___a96 == 0:
        if ___a97[0] == 1:
            ___a98 = ___a97[1]
            ___a99 = ___a97[2]
            ___t48 = (___a98 == 1) and (___a99[0] == 0)
        else:
            ___t48 = False
    else:
        ___t48 = False
else:
    ___t48 = False
def ___t49(___bi1):
    ___t50 = []
    ___t51 = (1, ___bi1.__str__(), (1, ", ", (0,)))
    while ___t51[0] != 0:
        ___t50.append(___t51[1])
        ___t51 = ___t51[2]
    return "".join(___t50)
___t52 = []
___t53 = ___iPredef0.map_List(___iPredef0.range(6), ___t49)
while ___t53[0] != 0:
    ___t52.append(___t53[1])
    ___t53 = ___t53[2]
___t54 = []
___t55 = (1, "range(6) == ", (1, "".join(___t52), (0,)))
while ___t55[0] != 0:
    ___t54.append(___t55[1])
    ___t55 = ___t55[2]
___a100 = ___iPredef0.foldl_List((0,), 0, lambda ___t56, ___t57: ___t56 + ___t57)
___a101 = ___iPredef0.foldl_List((1, 5, (0,)), 0, lambda ___t58, ___t59: ___t58 + ___t59)
___a102 = ___iPredef0.foldl_List((1, 5, (1, 11, (0,))), 0, lambda ___t60, ___t61: ___t60 + ___t61)
___a103 = ___iPredef0.foldl_List((1, 0, (1, 1, (1, 2, (1, 3, (1, 4, (1, 5, (0,))))))),
    0,
    lambda ___t62, ___t63: ___t62 + ___t63)
___a104 = (0,)
___a107 = 0
___a109 = ___a104
___t67 = ___a109[0] == 1
while ___t67:
    ___a105 = ___a109
    if ___a105[0] == 1:
        ___a106 = ___a105[1]
        ___t66 = ___a106 == 1
    else:
        ___t66 = False
    if ___t66:
        ___a109 = (0,)
        ___a107 = 1
    else:
        ___a109 = ___a109[2]
    ___t67 = ___a109[0] == 1
___a108 = ___a107
___a110 = ___a107 == 1
___a111 = (1, 1, (0,))
___a114 = 0
___a116 = ___a111
___t69 = ___a116[0] == 1
while ___t69:
    ___a112 = ___a116
    if ___a112[0] == 1:
        ___a113 = ___a112[1]
        ___t68 = ___a113 == 1
    else:
        ___t68 = False
    if ___t68:
        ___a116 = (0,)
        ___a114 = 1
    else:
        ___a116 = ___a116[2]
    ___t69 = ___a116[0] == 1
___a115 = ___a114
___a117 = ___a114 == 1
___a118 = (1, 0, (0,))
___a121 = 0
___a123 = ___a118
___t71 = ___a123[0] == 1
while ___t71:
    ___a119 = ___a123
    if ___a119[0] == 1:
        ___a120 = ___a119[1]
        ___t70 = ___a120 == 1
    else:
        ___t70 = False
    if ___t70:
        ___a123 = (0,)
        ___a121 = 1
    else:
        ___a123 = ___a123[2]
    ___t71 = ___a123[0] == 1
___a122 = ___a121
___a124 = ___a121 == 1
___a125 = (1, 0, (1, 0, (0,)))
___a128 = 0
___a130 = ___a125
___t73 = ___a130[0] == 1
while ___t73:
    ___a126 = ___a130
    if ___a126[0] == 1:
        ___a127 = ___a126[1]
        ___t72 = ___a127 == 1
    else:
        ___t72 = False
    if ___t72:
        ___a130 = (0,)
        ___a128 = 1
    else:
        ___a130 = ___a130[2]
    ___t73 = ___a130[0] == 1
___a129 = ___a128
___a131 = ___a128 == 1
___a132 = (1, 0, (1, 1, (0,)))
___a135 = 0
___a137 = ___a132
___t75 = ___a137[0] == 1
while ___t75:
    ___a133 = ___a137
    if ___a133[0] == 1:
        ___a134 = ___a133[1]
        ___t74 = ___a134 == 1
    else:
        ___t74 = False
    if ___t74:
        ___a137 = (0,)
        ___a135 = 1
    else:
        ___a137 = ___a137[2]
    ___t75 = ___a137[0] == 1
___a136 = ___a135
___a138 = ___a135 == 1
___a139 = (1, 1, (1, 0, (0,)))
___a142 = 0
___a144 = ___a139
___t77 = ___a144[0] == 1
while ___t77:
    ___a140 = ___a144
    if ___a140[0] == 1:
        ___a141 = ___a140[1]
        ___t76 = ___a141 == 1
    else:
        ___t76 = False
    if ___t76:
        ___a144 = (0,)
        ___a142 = 1
    else:
        ___a144 = ___a144[2]
    ___t77 = ___a144[0] == 1
___a143 = ___a142
___a145 = ___a142 == 1
___a146 = (1, 1, (1, 1, (0,)))
___a149 = 0
___a151 = ___a146
___t79 = ___a151[0] == 1
while ___t79:
    ___a147 = ___a151
    if ___a147[0] == 1:
        ___a148 = ___a147[1]
        ___t78 = ___a148 == 1
    else:
        ___t78 = False
    if ___t78:
        ___a151 = (0,)
        ___a149 = 1
    else:
        ___a151 = ___a151[2]
    ___t79 = ___a151[0] == 1
___a150 = ___a149
___a152 = ___a149 == 1
tests = (1,
    "List tests",
    (1,
        (0, 1, "".join(___t43)),
        (1,
            (0, il == 15, "".join(___t45)),
            (1,
                (0,
                    ___n_z__g__z_((1, 1, (1, 2, (1, 3, (0,)))), (1, 1, (1, 2, (1, 3, (0,))))),
                    "list [1, 2, 3]"),
                (1,
                    (0,
                        ___iBool1.___nnot(___n_z__g__z_((1, 1, (1, 2, (1, 3, (0,)))),
                                (1, 1, (1, 2, (0,))))),
                        "list [1, 2, 3] != [1, 2]"),
                    (1,
                        (0, ___a91[0] == 0, "range(0) matches []"),
                        (1,
                            (0, ___t47, "range(1) matches [0]"),
                            (1,
                                (0, ___t48, "range(2) matches [0, 1]"),
                                (1,
                                    (0,
                                        ___n_z__g__z_(___iPredef0.range(6),
                                            (1, 0, (1, 1, (1, 2, (1, 3, (1, 4, (1, 5, (0,)))))))),
                                        "".join(___t54)),
                                    (1,
                                        (0, ___a100 == 0, "sum([])"),
                                        (1,
                                            (0, ___a101 == 5, "sum([5])"),
                                            (1,
                                                (0, ___a102 == 16, "sum([5, 11])"),
                                                (1,
                                                    (0, ___a103 == 15, "sum([0, 1, 2, 3, 4, 5])"),
                                                    (1,
                                                        (0,
                                                            exists(___iPredef0.range(6),
                                                                lambda ___bv0: ___bv0 == 5),
                                                            "range(6) does have 5"),
                                                        (1,
                                                            (0,
                                                                ___iBool1.___nnot(exists(___iPredef0.range(6),
                                                                        lambda ___bv1: ___bv1 == 6)),
                                                                "range(6) does not have 6"),
                                                            (1,
                                                                stringTests,
                                                                (1,
                                                                    headTest,
                                                                    (1,
                                                                        unconsTest,
                                                                        (1,
                                                                            zipTest,
                                                                            (1,
                                                                                sortTest,
                                                                                (1,
                                                                                    (0,
                                                                                        ___a110 == 0,
                                                                                        "any([])"),
                                                                                    (1,
                                                                                        (0,
                                                                                            ___a117 == 1,
                                                                                            "any([True])"),
                                                                                        (1,
                                                                                            (0,
                                                                                                ___a124 == 0,
                                                                                                "any([False])"),
                                                                                            (1,
                                                                                                (0,
                                                                                                    ___a131 == 0,
                                                                                                    "any([False, False])"),
                                                                                                (1,
                                                                                                    (0,
                                                                                                        ___a138 == 1,
                                                                                                        "any([False, True])"),
                                                                                                    (1,
                                                                                                        (0,
                                                                                                            ___a145 == 1,
                                                                                                            "any([True, False])"),
                                                                                                        (1,
                                                                                                            (0,
                                                                                                                ___a152 == 1,
                                                                                                                "any([True, True])"),
                                                                                                            (0,))))))))))))))))))))))))))))

class BosatsuTests(___iunittest2.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t80 = value[2]
                while ___t80[0] != 0:
                    test_loop(___t80[1])
                    ___t80 = ___t80[2]
        test_loop(tests)