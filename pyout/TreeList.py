import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

def foldTree(___bt0, ___binit0, ___bfn0):
    def ___bloop0(___bt1, ___binit1):
        if ___bt1[0] == 0:
            return ___bfn0(___binit1, ___bt1[1])
        else:
            return ___bloop0(___bt1[4], ___bloop0(___bt1[3], ___bfn0(___binit1, ___bt1[2])))
    return ___bloop0(___bt0, ___binit0)

empty = (0,)

def cons(___bhead0, ___ba1):
    ___btail0 = ___ba1
    if ___btail0[0] == 0:
        return (1, (0, ___bhead0), (0,))
    else:
        if ___btail0[0] == 1:
            ___a1 = ___btail0[1]
            ___a2 = ___btail0[2]
            ___t4 = (___a1[0] == 0) and (___a2[0] == 0)
        else:
            ___t4 = False
        if ___t4:
            return (1, (0, ___bhead0), (1, ___a1, (0,)))
        else:
            if ___btail0[0] == 1:
                ___a3 = ___btail0[1]
                ___a4 = ___btail0[2]
                if ___a3[0] == 0:
                    if ___a4[0] == 1:
                        ___a5 = ___a4[1]
                        ___t3 = ___a5[0] == 1
                    else:
                        ___t3 = False
                else:
                    ___t3 = False
            else:
                ___t3 = False
            if ___t3:
                return (1, (0, ___bhead0), (1, ___a5, ___a4[2]))
            else:
                if ___btail0[0] == 1:
                    ___a6 = ___btail0[1]
                    ___a7 = ___btail0[2]
                    if ___a6[0] == 0:
                        if ___a7[0] == 1:
                            ___a8 = ___a7[1]
                            ___t2 = ___a8[0] == 0
                        else:
                            ___t2 = False
                    else:
                        ___t2 = False
                else:
                    ___t2 = False
                if ___t2:
                    return (1, (1, 3, ___bhead0, ___a6, ___a8), ___a7[2])
                else:
                    if ___btail0[0] == 1:
                        ___a9 = ___btail0[1]
                        ___a10 = ___btail0[2]
                        if ___a9[0] == 1:
                            if ___a10[0] == 1:
                                ___a11 = ___a10[1]
                                ___t1 = ___a11[0] == 0
                            else:
                                ___t1 = False
                        else:
                            ___t1 = False
                    else:
                        ___t1 = False
                    if ___t1:
                        return empty
                    else:
                        if ___btail0[0] == 1:
                            ___a12 = ___btail0[1]
                            ___a13 = ___btail0[2]
                            ___t0 = (___a12[0] == 1) and (___a13[0] == 0)
                        else:
                            ___t0 = False
                        if ___t0:
                            return (1, (0, ___bhead0), (1, ___a12, (0,)))
                        else:
                            ___a14 = ___btail0[1]
                            ___a15 = ___btail0[2]
                            ___a16 = ___a15[1]
                            ___bb11 = ___a14
                            ___bs12 = ___a14[1]
                            ___bb20 = ___a16
                            ___bs21 = ___a16[1]
                            return (1,
                                (1, ___bs12 + ___bs21 + 1, ___bhead0, ___bb11, ___bb20),
                                ___a15[2]) if (___bs12 == ___bs21) == 1 else (1,
                                (0, ___bhead0),
                                ___btail0)

def decons(___ba2):
    ___btrees0 = ___ba2
    if ___btrees0[0] == 0:
        return (0,)
    else:
        if ___btrees0[0] == 1:
            ___a17 = ___btrees0[1]
            ___t5 = ___a17[0] == 0
        else:
            ___t5 = False
        if ___t5:
            return (1, (___a17[1], ___btrees0[2]))
        else:
            ___a18 = ___btrees0[1]
            return (1, (___a18[2], (1, ___a18[3], (1, ___a18[4], ___btrees0[2]))))

def head(___btl0):
    ___a19 = decons(___btl0)
    if ___a19[0] == 1:
        return (1, ___a19[1][0])
    else:
        return (0,)

def get(___bb0, ___bidx0):
    ___btrees1 = ___bb0
    def ___t6(___bidx1, ___ba3):
        ___btrees2 = ___ba3[0]
        ___a27 = ___bidx1 == 1
        if ___a27 == 1:
            ___a21 = decons(___btrees2)
            if ___a21[0] == 1:
                ___a22 = ___a21[1]
                ___bh4 = ___a22[0]
                ___t7 = (1, ___bh4)
            else:
                ___t7 = (0,)
            return (0, ((0,), ___t7))
        elif ___btrees2[0] == 0:
            return (0, ((0,), (0,)))
        else:
            if ___btrees2[0] == 1:
                ___a25 = ___btrees2[1]
                ___t8 = ___a25[0] == 0
            else:
                ___t8 = False
            if ___t8:
                return (0, ((0,), (1, ___a25[1]))) if (___bidx1 == 1) == 1 else (___bidx1 + -1,
                    (___btrees2[2], (0,)))
            else:
                ___a26 = ___btrees2[1]
                ___bs0 = ___a26[1]
                ___bt10 = ___a26[3]
                ___bt20 = ___a26[4]
                ___brest1 = ___btrees2[2]
                ___a24 = 0 if ___bidx1 < ___bs0 else 1 if ___bidx1 == ___bs0 else 2
                return (___bidx1 + -1,
                    ((1, ___bt10, (1, ___bt20, ___brest1)),
                        (0,))) if ___a24 == 0 else (___bidx1 + -1,
                    ((1, ___bt10, (1, ___bt20, ___brest1)),
                        (0,))) if ___a24 == 1 else (___bidx1 - ___bs0, (___brest1, (0,)))
    ___t9 = 0 < (___bidx0 + 1)
    ___t10 = (___btrees1, (0,))
    ___t11 = ___bidx0 + 1
    ___t12 = (___btrees1, (0,))
    while ___t9:
        ___t10 = ___t6(___t11, ___t12)
        ___t13 = ___t10[0]
        ___t12 = ___t10[1]
        ___t9 = (0 < ___t13) and (___t13 < ___t11)
        ___t11 = ___t13
    return ___t12[1]

def fold(___ba4, ___binit2, ___bfn1):
    ___btrees3 = ___ba4
    def ___bloop1(___btrees4, ___binit3):
        ___a32 = ___btrees4
        ___a34 = ___binit3
        ___a29 = 1
        ___t14 = ___a29 == 1
        while ___t14:
            if ___a32[0] == 0:
                ___a29 = 0
                ___a30 = ___a34
            else:
                ___bh6 = ___a32[1]
                ___bt7 = ___a32[2]
                ___a31 = ___bt7
                ___a33 = foldTree(___bh6, ___a34, ___bfn1)
                ___a32 = ___a31
                ___a34 = ___a33
            ___t14 = ___a29 == 1
        return ___a30
    return ___bloop1(___btrees3, ___binit2)

def to_List(___blist0):
    return ___iPredef0.reverse(fold(___blist0, (0,), lambda ___bl1, ___bh7: (1, ___bh7, ___bl1)))

def eq_TreeList(___bfn2):
    def ___t16(___ba5, ___bb1):
        ___t18 = (___bfn2,)
        def ___t17(___ba6, ___bh8):
            ___bcurrent0 = ___ba6[0]
            ___bb2 = ___ba6[1]
            if ___bcurrent0 == 1:
                ___a36 = decons(___bb2)
                if ___a36[0] == 0:
                    return (0, empty)
                else:
                    ___a37 = ___a36[1]
                    return (1, ___a37[1]) if ___t18[0](___bh8, ___a37[0]) == 1 else (0, empty)
            else:
                return (0, empty)
        return fold(___ba5, (1, ___bb1), ___t17)[0]
    return ___t16

eq_ti = eq_TreeList(lambda ___t19, ___t20: ___t19 == ___t20)

tl12 = cons(2, cons(1, empty))

___blist1 = (1, 1, (1, 2, (1, 3, (1, 4, (0,)))))
list14 = ___iPredef0.foldl_List(___iPredef0.reverse(___blist1),
    empty,
    lambda ___blst0, ___bh9: cons(___bh9, ___blst0))

cons14 = cons(1, cons(2, cons(3, cons(4, empty))))

___a39 = (get(tl12, 0), (1, 2))
___a41 = ___a39[1]
___a40 = ___a39[0]
if (___a40[0] == 1) and (___a41[0] == 1):
    ___ba7 = ___a40[1]
    ___bb3 = ___a41[1]
    ___t22 = ___ba7 == ___bb3
else:
    ___a43 = ___a39[1]
    ___a42 = ___a39[0]
    ___t22 = (___a42[0] == 0) and (___a43[0] == 0)
___a44 = (get(tl12, 1), (1, 1))
___a46 = ___a44[1]
___a45 = ___a44[0]
if (___a45[0] == 1) and (___a46[0] == 1):
    ___ba8 = ___a45[1]
    ___bb4 = ___a46[1]
    ___t23 = ___ba8 == ___bb4
else:
    ___a48 = ___a44[1]
    ___a47 = ___a44[0]
    ___t23 = (___a47[0] == 0) and (___a48[0] == 0)
___a49 = (get(tl12, 2), (0,))
___a51 = ___a49[1]
___a50 = ___a49[0]
if (___a50[0] == 1) and (___a51[0] == 1):
    ___ba9 = ___a50[1]
    ___bb5 = ___a51[1]
    ___t24 = ___ba9 == ___bb5
else:
    ___a53 = ___a49[1]
    ___a52 = ___a49[0]
    ___t24 = (___a52[0] == 0) and (___a53[0] == 0)
___a54 = (get(list14, 0), (1, 1))
___a56 = ___a54[1]
___a55 = ___a54[0]
if (___a55[0] == 1) and (___a56[0] == 1):
    ___ba10 = ___a55[1]
    ___bb6 = ___a56[1]
    ___t25 = ___ba10 == ___bb6
else:
    ___a58 = ___a54[1]
    ___a57 = ___a54[0]
    ___t25 = (___a57[0] == 0) and (___a58[0] == 0)
___a59 = (get(list14, 1), (1, 2))
___a61 = ___a59[1]
___a60 = ___a59[0]
if (___a60[0] == 1) and (___a61[0] == 1):
    ___ba11 = ___a60[1]
    ___bb7 = ___a61[1]
    ___t26 = ___ba11 == ___bb7
else:
    ___a63 = ___a59[1]
    ___a62 = ___a59[0]
    ___t26 = (___a62[0] == 0) and (___a63[0] == 0)
___a64 = (get(list14, 2), (1, 3))
___a66 = ___a64[1]
___a65 = ___a64[0]
if (___a65[0] == 1) and (___a66[0] == 1):
    ___ba12 = ___a65[1]
    ___bb8 = ___a66[1]
    ___t27 = ___ba12 == ___bb8
else:
    ___a68 = ___a64[1]
    ___a67 = ___a64[0]
    ___t27 = (___a67[0] == 0) and (___a68[0] == 0)
___a69 = (get(list14, 3), (1, 4))
___a71 = ___a69[1]
___a70 = ___a69[0]
if (___a70[0] == 1) and (___a71[0] == 1):
    ___ba13 = ___a70[1]
    ___bb9 = ___a71[1]
    ___t28 = ___ba13 == ___bb9
else:
    ___a73 = ___a69[1]
    ___a72 = ___a69[0]
    ___t28 = (___a72[0] == 0) and (___a73[0] == 0)
tests = (1,
    "TreeList tests",
    (1,
        (0, ___t22, "get 0 == 2"),
        (1,
            (0, ___t23, "get 1 == 1"),
            (1,
                (0, ___t24, "get 2 == None"),
                (1,
                    (0, ___t25, "[1, 2, 3, 4] get 0"),
                    (1,
                        (0, ___t26, "[1, 2, 3, 4] get 1"),
                        (1,
                            (0, ___t27, "[1, 2, 3, 4] get 2"),
                            (1,
                                (0, ___t28, "[1, 2, 3, 4] get 3"),
                                (1,
                                    (0,
                                        fold(list14,
                                            0,
                                            lambda ___t29, ___t30: ___t29 + ___t30) == 10,
                                        "fold to 10"),
                                    (1,
                                        (0,
                                            eq_ti(list14, cons14),
                                            "fromList matches building by cons"),
                                        (0,)))))))))))

class BosatsuTests(___iunittest1.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t31 = value[2]
                while ___t31[0] != 0:
                    test_loop(___t31[1])
                    ___t31 = ___t31[2]
        test_loop(tests)