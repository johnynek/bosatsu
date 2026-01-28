import Bosatsu.Nat as ___iNat0
import Bosatsu.Predef as ___iPredef1
import unittest as ___iunittest2

def is_even(___bb0):
    return 1 if ___bb0[0] == 0 else ___bb0[0] == 2

def toInt(___bb1):
    if ___bb1[0] == 0:
        return 0
    elif ___bb1[0] == 1:
        ___bn0 = ___bb1[1]
        return toInt(___bn0) + toInt(___bn0) + 1
    else:
        ___bn1 = ___bb1[1]
        return toInt(___bn1) + toInt(___bn1) + 2

def toNat(___bb2):
    if ___bb2[0] == 0:
        return 0
    elif ___bb2[0] == 1:
        return ___iNat0.times2(toNat(___bb2[1])) + 1
    else:
        return ___iNat0.times2(toNat(___bb2[1])) + 2

def toBinNat(___bn4):
    def ___t0(___bn5, ___bfns0):
        ___a0 = ___bn5 & 1
        ___bis__even0 = ___a0 == 0
        ___a1 = (lambda ___t1: (2, ___t1),
            lambda ___bn6: ___bn6 + -1) if ___bis__even0 == 1 else (lambda ___t3: (1, ___t3),
            lambda ___bn7: ___bn7)
        return (___a1[1](___bn5 >> 1), (1, ___a1[0], ___bfns0))
    ___t5 = 0 < ___bn4
    ___t6 = (0,)
    ___t7 = ___bn4
    ___t8 = (0,)
    while ___t5:
        ___t6 = ___t0(___t7, ___t8)
        ___t9 = ___t6[0]
        ___t8 = ___t6[1]
        ___t5 = (0 < ___t9) and (___t9 < ___t7)
        ___t7 = ___t9
    return ___iPredef1.foldl_List(___t8, (0,), lambda ___bn8, ___bfn0: ___bfn0(___bn8))

def cmp_BinNat(___ba0, ___bb3):
    if ___ba0[0] == 1:
        ___ba10 = ___ba0[1]
        if ___bb3[0] == 1:
            return cmp_BinNat(___ba10, ___bb3[1])
        elif ___bb3[0] == 2:
            ___bb11 = ___bb3[1]
            ___a2 = cmp_BinNat(___ba10, ___bb11)
            return 0 if ___a2 == 0 else 0 if ___a2 == 1 else 2
        else:
            return 2
    elif ___ba0[0] == 2:
        ___ba11 = ___ba0[1]
        if ___bb3[0] == 2:
            return cmp_BinNat(___ba11, ___bb3[1])
        elif ___bb3[0] == 1:
            ___bb13 = ___bb3[1]
            ___a3 = cmp_BinNat(___ba11, ___bb13)
            return 2 if ___a3 == 2 else 2 if ___a3 == 1 else 0
        else:
            return 2
    else:
        return 0 if ___bb3[0] == 1 else ___bb3[0] != 2

def eq_BinNat(___ba1, ___bb4):
    ___a7 = ___ba1
    ___a9 = ___bb4
    ___a4 = 1
    ___t11 = ___a4 == 1
    while ___t11:
        if ___a7[0] == 0:
            ___a4 = 0
            ___a5 = ___a9[0] == 0
        elif ___a7[0] == 1:
            ___bn9 = ___a7[1]
            if ___a9[0] == 1:
                ___bm0 = ___a9[1]
                ___a6 = ___bn9
                ___a8 = ___bm0
                ___a7 = ___a6
                ___a9 = ___a8
            else:
                ___a4 = 0
                ___a5 = 0
        else:
            ___bn10 = ___a7[1]
            if ___a9[0] == 2:
                ___bm1 = ___a9[1]
                ___a6 = ___bn10
                ___a8 = ___bm1
                ___a7 = ___a6
                ___a9 = ___a8
            else:
                ___a4 = 0
                ___a5 = 0
        ___t11 = ___a4 == 1
    return ___a5

def next(___bb5):
    if ___bb5[0] == 1:
        return (2, ___bb5[1])
    elif ___bb5[0] == 2:
        return (1, next(___bb5[1]))
    else:
        return (1, (0,))

def prev(___bb6):
    if ___bb6[0] == 0:
        return (0,)
    else:
        if ___bb6[0] == 1:
            ___a10 = ___bb6[1]
            ___t12 = ___a10[0] == 0
        else:
            ___t12 = False
        if ___t12:
            return (0,)
        elif ___bb6[0] == 1:
            return (2, prev(___bb6[1]))
        else:
            return (1, ___bb6[1])

def add_BinNat(___bleft0, ___bright0):
    if ___bleft0[0] == 1:
        ___bodd0 = ___bleft0
        ___bleft1 = ___bleft0[1]
        if ___bright0[0] == 1:
            return (2, add_BinNat(___bleft1, ___bright0[1]))
        elif ___bright0[0] == 2:
            return (1, add_BinNat(___bleft1, next(___bright0[1])))
        else:
            return ___bodd0
    elif ___bleft0[0] == 2:
        ___beven0 = ___bleft0
        ___bleft2 = ___bleft0[1]
        if ___bright0[0] == 1:
            return (1, add_BinNat(___bleft2, next(___bright0[1])))
        elif ___bright0[0] == 2:
            return (2, add_BinNat(___bleft2, next(___bright0[1])))
        else:
            return ___beven0
    else:
        return ___bright0

def times2(___bb7):
    if ___bb7[0] == 1:
        return (2, times2(___bb7[1]))
    elif ___bb7[0] == 2:
        return (2, (1, ___bb7[1]))
    else:
        return (0,)

def doub_prev(___bb8):
    if ___bb8[0] == 1:
        return (1, (1, times2(___bb8[1])))
    elif ___bb8[0] == 2:
        return (1, (1, (1, ___bb8[1])))
    else:
        return (0,)

def sub_Option(___bleft3, ___bright5):
    if ___bleft3[0] == 0:
        return (1, (0,)) if ___bright5[0] == 0 else (0,)
    elif ___bleft3[0] == 1:
        ___bodd1 = ___bleft3
        ___bleft4 = ___bleft3[1]
        if ___bright5[0] == 0:
            return (1, ___bodd1)
        elif ___bright5[0] == 1:
            ___bright6 = ___bright5[1]
            ___a11 = sub_Option(___bleft4, ___bright6)
            if ___a11[0] == 1:
                return (1, times2(___a11[1]))
            else:
                return (0,)
        else:
            ___bright7 = ___bright5[1]
            ___a12 = sub_Option(___bleft4, ___bright7)
            if ___a12[0] == 1:
                return doub_prev(___a12[1])
            else:
                return (0,)
    else:
        ___beven1 = ___bleft3
        ___bleft5 = ___bleft3[1]
        if ___bright5[0] == 0:
            return (1, ___beven1)
        elif ___bright5[0] == 1:
            ___bright8 = ___bright5[1]
            ___a13 = sub_Option(___bleft5, ___bright8)
            if ___a13[0] == 1:
                return (1, (1, ___a13[1]))
            else:
                return (0,)
        else:
            ___bright9 = ___bright5[1]
            ___a14 = sub_Option(___bleft5, ___bright9)
            if ___a14[0] == 1:
                return (1, times2(___a14[1]))
            else:
                return (0,)

def sub_BinNat(___bleft6, ___bright10):
    ___a15 = sub_Option(___bleft6, ___bright10)
    return ___a15[1] if ___a15[0] == 1 else (0,)

def div2(___bb9):
    if ___bb9[0] == 0:
        return (0,)
    elif ___bb9[0] == 1:
        return ___bb9[1]
    else:
        return next(___bb9[1])

def times_BinNat(___bleft7, ___bright11):
    if ___bleft7[0] == 0:
        return (0,)
    elif ___bleft7[0] == 1:
        ___bleft8 = ___bleft7[1]
        if ___bright11[0] == 0:
            return (0,)
        else:
            return add_BinNat(times2(times_BinNat(___bleft8, ___bright11)), ___bright11)
    else:
        ___bleft9 = ___bleft7[1]
        if ___bright11[0] == 0:
            return (0,)
        else:
            return times2(add_BinNat(times_BinNat(___bleft9, ___bright11), ___bright11))

one = (1, (0,))

def divmod(___bnumerator0, ___bdivisor0):
    ___t13 = (___bdivisor0,)
    def ___bloop0(___bnumerator1):
        if ___bnumerator1[0] == 1:
            ___bn16 = ___bnumerator1[1]
            ___a20 = ___bloop0(___bn16)
            ___bd10 = ___a20[0]
            ___bm10 = ___a20[1]
            ___bm2 = (1, ___bm10)
            ___a19 = cmp_BinNat(___bm2, ___t13[0])
            if ___a19 == 0:
                return (times2(___bd10), ___bm2)
            else:
                ___a16 = sub_Option(___bm2, ___t13[0])
                ___bm20 = ___a16[1] if ___a16[0] == 1 else (0,)
                ___a18 = cmp_BinNat(___bm20, ___t13[0])
                if ___a18 == 0:
                    return ((1, ___bd10), ___bm20)
                elif ___a18 == 1:
                    return ((2, ___bd10), (0,))
                else:
                    ___a17 = sub_Option(___bm20, ___t13[0])
                    return ((2, ___bd10), ___a17[1] if ___a17[0] == 1 else (0,))
        elif ___bnumerator1[0] == 2:
            ___bn17 = ___bnumerator1[1]
            ___a25 = ___bloop0(___bn17)
            ___bd11 = ___a25[0]
            ___bm11 = ___a25[1]
            ___bm3 = (2, ___bm11)
            ___a24 = cmp_BinNat(___bm3, ___t13[0])
            if ___a24 == 0:
                return (times2(___bd11), ___bm3)
            else:
                ___a21 = sub_Option(___bm3, ___t13[0])
                ___bm21 = ___a21[1] if ___a21[0] == 1 else (0,)
                ___a23 = cmp_BinNat(___bm21, ___t13[0])
                if ___a23 == 0:
                    return ((1, ___bd11), ___bm21)
                elif ___a23 == 1:
                    return ((2, ___bd11), (0,))
                else:
                    ___a22 = sub_Option(___bm21, ___t13[0])
                    return ((2, ___bd11), ___a22[1] if ___a22[0] == 1 else (0,))
        else:
            return ((0,), (0,))
    if ___bdivisor0[0] == 1:
        ___a26 = ___bdivisor0[1]
        ___t14 = ___a26[0] == 0
    else:
        ___t14 = False
    return (___bnumerator0,
        (0,)) if ___t14 else ___bloop0(___bnumerator0) if ___bdivisor0[0] == 1 else ___bloop0(___bnumerator0) if ___bdivisor0[0] == 2 else ((0,),
        ___bnumerator0)

def exp(___bbase0, ___bpower0):
    if ___bpower0[0] == 0:
        return one
    elif ___bpower0[0] == 1:
        ___bn18 = ___bpower0[1]
        ___bbn0 = exp(___bbase0, ___bn18)
        return times_BinNat(times_BinNat(___bbn0, ___bbn0), ___bbase0)
    else:
        ___bn19 = ___bpower0[1]
        ___bbn10 = times_BinNat(exp(___bbase0, ___bn19), ___bbase0)
        return times_BinNat(___bbn10, ___bbn10)

def fold_left_BinNat(___bfn1, ___binit0, ___bcnt0):
    ___t15 = (___bfn1,)
    def ___bloop1(___binit1, ___bcnt1, ___bcnt__Nat0):
        ___a31 = ___binit1
        ___a33 = ___bcnt1
        ___a35 = ___bcnt__Nat0
        ___a28 = 1
        ___t16 = ___a28 == 1
        while ___t16:
            if ___a35 == 0:
                ___a28 = 0
                ___a29 = ___a31
            else:
                ___a27 = ___a35 - 1
                ___bprevNat0 = ___a27
                ___bcnt2 = prev(___a33)
                ___a30 = ___t15[0](___a31, ___bcnt2)
                ___a32 = ___bcnt2
                ___a34 = ___bprevNat0
                ___a31 = ___a30
                ___a33 = ___a32
                ___a35 = ___a34
            ___t16 = ___a28 == 1
        return ___a29
    return ___bloop1(___binit0, ___bcnt0, toNat(___bcnt0))

def fib(___bb10):
    def ___bloop2(___bn20, ___bcur0, ___bnext0):
        ___a40 = ___bn20
        ___a42 = ___bcur0
        ___a44 = ___bnext0
        ___a37 = 1
        ___t17 = ___a37 == 1
        while ___t17:
            if ___a40 == 0:
                ___a37 = 0
                ___a38 = ___a42
            else:
                ___a36 = ___a40 - 1
                ___bn21 = ___a36
                ___a39 = ___bn21
                ___a41 = ___a44
                ___a43 = add_BinNat(___a42, ___a44)
                ___a40 = ___a39
                ___a42 = ___a41
                ___a44 = ___a43
            ___t17 = ___a37 == 1
        return ___a38
    ___bone0 = (1, (0,))
    return ___bloop2(toNat(___bb10), ___bone0, ___bone0)

def next_law(___bi0, ___bmsg0):
    return (0, toInt(next(toBinNat(___bi0))) == (___bi0 + 1), ___bmsg0)

def times2_law(___bi1, ___bmsg1):
    return (0, toInt(times2(toBinNat(___bi1))) == (___bi1 + ___bi1), ___bmsg1)

two = next(one)

three = next(two)

four = next(three)

def ___t18(___ba2):
    ___bi2 = ___ba2[0]
    return (0, toInt(toBinNat(___bi2)) == ___bi2, ___ba2[1])
def ___t19(___ba3):
    return next_law(___ba3[0], ___ba3[1])
def ___t20(___ba4):
    return times2_law(___ba4[0], ___ba4[1])
___t22 = []
___t23 = (1, "fib(3) == 3 (got ", (1, toInt(fib(three)).__str__(), (1, ")", (0,))))
while ___t23[0] != 0:
    ___t22.append(___t23[1])
    ___t23 = ___t23[2]
___a45 = cmp_BinNat(toBinNat(54), toBinNat(54))
test = (1,
    "BinNat tests",
    (1,
        (0, toInt((0,)) == 0, "0.toBinNat"),
        (1,
            (0, toInt((1, (0,))) == 1, "1.toBinNat"),
            (1,
                (0, toInt((2, (0,))) == 2, "2.toBinNat"),
                (1,
                    (0, toInt((1, (1, (0,)))) == 3, "3.toBinNat"),
                    (1,
                        (0, toInt((2, (1, (0,)))) == 4, "4.toBinNat"),
                        (1,
                            (1,
                                "round trip laws",
                                ___iPredef1.map_List((1,
                                        (0, "roundtrip 0"),
                                        (1,
                                            (1, "roundtrip 1"),
                                            (1,
                                                (2, "roundtrip 2"),
                                                (1,
                                                    (3, "roundtrip 3"),
                                                    (1,
                                                        (4, "roundtrip 4"),
                                                        (1,
                                                            (5, "roundtrip 5"),
                                                            (1,
                                                                (6, "roundtrip 6"),
                                                                (1,
                                                                    (7, "roundtrip 7"),
                                                                    (1,
                                                                        (50, "roundtrip 50"),
                                                                        (1,
                                                                            (61, "roundtrip 61"),
                                                                            (1,
                                                                                (72,
                                                                                    "roundtrip 72"),
                                                                                (0,)))))))))))),
                                    ___t18)),
                            (1,
                                (1,
                                    "next law",
                                    ___iPredef1.map_List((1,
                                            (0, "0.next"),
                                            (1,
                                                (5, "5.next"),
                                                (1,
                                                    (10, "10.next"),
                                                    (1, (113, "113.next"), (0,))))),
                                        ___t19)),
                                (1,
                                    (0, toInt(prev(next(toBinNat(0)))) == 0, "0.next().prev == 0"),
                                    (1,
                                        (0,
                                            toInt(prev(next(toBinNat(5)))) == 5,
                                            "5.next().prev == 5"),
                                        (1,
                                            (0,
                                                toInt(prev(next(toBinNat(10)))) == 10,
                                                "10.next().prev == 10"),
                                            (1,
                                                (0,
                                                    toInt(add_BinNat(toBinNat(10),
                                                            toBinNat(11))) == 21,
                                                    "add_BinNat(10, 11) == 21"),
                                                (1,
                                                    (1,
                                                        "times2 law",
                                                        ___iPredef1.map_List((1,
                                                                (0, "0 * 2"),
                                                                (1,
                                                                    (1, "1 * 2"),
                                                                    (1,
                                                                        (2, "2 * 2"),
                                                                        (1,
                                                                            (5, "5 * 2"),
                                                                            (1,
                                                                                (10, "10 * 2"),
                                                                                (0,)))))),
                                                            ___t20)),
                                                    (1,
                                                        (0,
                                                            toInt(times_BinNat(toBinNat(10),
                                                                    toBinNat(11))) == 110,
                                                            "10*11 = 110"),
                                                        (1,
                                                            (0,
                                                                toInt(times_BinNat(toBinNat(0),
                                                                        toBinNat(11))) == 0,
                                                                "0*11 = 0"),
                                                            (1,
                                                                (0,
                                                                    toInt(fold_left_BinNat(lambda ___bn22, ___ba5: next(___bn22),
                                                                            (0,),
                                                                            toBinNat(10))) == 10,
                                                                    "1 + ... + 1 = 10"),
                                                                (1,
                                                                    (0,
                                                                        toInt(fold_left_BinNat(add_BinNat,
                                                                                (0,),
                                                                                toBinNat(4))) == 6,
                                                                        "1+2+3=6"),
                                                                    (1,
                                                                        (0,
                                                                            toInt(fib((0,))) == 1,
                                                                            "fib(0) == 1"),
                                                                        (1,
                                                                            (0,
                                                                                toInt(fib(one)) == 1,
                                                                                "fib(1) == 1"),
                                                                            (1,
                                                                                (0,
                                                                                    toInt(fib(two)) == 2,
                                                                                    "fib(2) == 2"),
                                                                                (1,
                                                                                    (0,
                                                                                        toInt(fib(three)) == 3,
                                                                                        "".join(___t22)),
                                                                                    (1,
                                                                                        (0,
                                                                                            toInt(fib(four)) == 5,
                                                                                            "fib(4) == 5"),
                                                                                        (1,
                                                                                            (0,
                                                                                                ___a45 == 1,
                                                                                                "54 == 54"),
                                                                                            (0,))))))))))))))))))))))))

class BosatsuTests(___iunittest2.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t24 = value[2]
                while ___t24[0] != 0:
                    test_loop(___t24[1])
                    ___t24 = ___t24[2]
        test_loop(test)