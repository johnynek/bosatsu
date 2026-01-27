import unittest as ___iunittest0

def cmp_Nat(___ba0, ___bb0):
    ___a5 = ___ba0
    ___a7 = ___bb0
    ___a2 = 1
    ___t0 = ___a2 == 1
    while ___t0:
        if ___a5 == 0:
            ___a2 = 0
            ___a3 = ___a7 == 0
        else:
            ___a1 = ___a5 - 1
            ___bn0 = ___a1
            if ___a7 == 0:
                ___a2 = 0
                ___a3 = 2
            else:
                ___a0 = ___a7 - 1
                ___bm0 = ___a0
                ___a4 = ___bn0
                ___a6 = ___bm0
                ___a5 = ___a4
                ___a7 = ___a6
        ___t0 = ___a2 == 1
    return ___a3

def times2(___bn1):
    def ___bloop0(___bn2, ___bacc0):
        ___a12 = ___bn2
        ___a14 = ___bacc0
        ___a9 = 1
        ___t1 = ___a9 == 1
        while ___t1:
            if ___a12 == 0:
                ___a9 = 0
                ___a10 = ___a14
            else:
                ___a8 = ___a12 - 1
                ___bprev0 = ___a8
                ___a11 = ___bprev0
                ___a13 = ___a14 + 2
                ___a12 = ___a11
                ___a14 = ___a13
            ___t1 = ___a9 == 1
        return ___a10
    return ___bloop0(___bn1, 0)

def add(___bn10, ___bn20):
    def ___bloop1(___bn11, ___bn21):
        ___a19 = ___bn11
        ___a21 = ___bn21
        ___a16 = 1
        ___t2 = ___a16 == 1
        while ___t2:
            if ___a19 == 0:
                ___a16 = 0
                ___a17 = ___a21
            else:
                ___a15 = ___a19 - 1
                ___bprev__n10 = ___a15
                ___a18 = ___bprev__n10
                ___a20 = ___a21 + 1
                ___a19 = ___a18
                ___a21 = ___a20
            ___t2 = ___a16 == 1
        return ___a17
    return ___bn10 if ___bn20 == 0 else ___bloop1(___bn10, ___bn20)

def sub_Nat(___bn12, ___bn22):
    ___a27 = ___bn12
    ___a29 = ___bn22
    ___a24 = 1
    ___t3 = ___a24 == 1
    while ___t3:
        if ___a29 == 0:
            ___a24 = 0
            ___a25 = ___a27
        else:
            ___a23 = ___a29 - 1
            ___bprev__n20 = ___a23
            if ___a27 == 0:
                ___a24 = 0
                ___a25 = 0
            else:
                ___a22 = ___a27 - 1
                ___bprev__n11 = ___a22
                ___a26 = ___bprev__n11
                ___a28 = ___bprev__n20
                ___a27 = ___a26
                ___a29 = ___a28
        ___t3 = ___a24 == 1
    return ___a25

def mult(___bn13, ___bn23):
    def ___bloop2(___bn14, ___bn24, ___bc0):
        ___a35 = ___bn14
        ___a37 = ___bn24
        ___a39 = ___bc0
        ___a32 = 1
        ___t4 = ___a32 == 1
        while ___t4:
            if ___a35 == 0:
                ___a32 = 0
                ___a33 = ___a39
            else:
                ___a31 = ___a35 - 1
                ___bn15 = ___a31
                if ___a37 == 0:
                    ___a32 = 0
                    ___a33 = ___a39
                else:
                    ___a30 = ___a37 - 1
                    ___bn25 = ___a30
                    ___a34 = ___bn15
                    ___a36 = ___bn25
                    ___a38 = add(add(___bn15, ___bn25), ___a39) + 1
                    ___a35 = ___a34
                    ___a37 = ___a36
                    ___a39 = ___a38
            ___t4 = ___a32 == 1
        return ___a33
    return ___bloop2(___bn23, ___bn13, 0) if cmp_Nat(___bn13, ___bn23) == 2 else ___bloop2(___bn13,
        ___bn23,
        0)

def is_even(___bn3):
    def ___bloop3(___bn4, ___bres0):
        ___a45 = ___bn4
        ___a47 = ___bres0
        ___a42 = 1
        ___t5 = ___a42 == 1
        while ___t5:
            if ___a45 == 0:
                ___a42 = 0
                ___a43 = ___a47
            else:
                ___a41 = ___a45 - 1
                ___bn5 = ___a41
                ___a44 = ___bn5
                ___a46 = ___a47 != 1
                ___a45 = ___a44
                ___a47 = ___a46
            ___t5 = ___a42 == 1
        return ___a43
    return ___bloop3(___bn3, 1)

def div2(___bn6):
    def ___bloop4(___bn7, ___bacc1, ___bis__even0):
        ___a52 = ___bn7
        ___a54 = ___bacc1
        ___a56 = ___bis__even0
        ___a49 = 1
        ___t6 = ___a49 == 1
        while ___t6:
            if ___a52 == 0:
                ___a49 = 0
                ___a50 = ___a54
            else:
                ___a48 = ___a52 - 1
                ___bn8 = ___a48
                if ___a56 == 1:
                    ___a51 = ___bn8
                    ___a53 = ___a54 + 1
                    ___a55 = 0
                    ___a52 = ___a51
                    ___a54 = ___a53
                    ___a56 = ___a55
                else:
                    ___a51 = ___bn8
                    ___a55 = 1
                    ___a52 = ___a51
                    ___a56 = ___a55
            ___t6 = ___a49 == 1
        return ___a50
    return ___bloop4(___bn6, 0, is_even(___bn6))

def divmod(___bnumerator0, ___bdivisor0):
    ___t7 = (___bdivisor0,)
    def ___bloop5(___bnumerator1, ___bd0, ___bm1):
        ___a62 = ___bnumerator1
        ___a64 = ___bd0
        ___a66 = ___bm1
        ___a59 = 1
        ___t8 = ___a59 == 1
        while ___t8:
            if ___a62 == 0:
                ___a59 = 0
                ___a60 = (___a64, ___a66)
            else:
                ___a58 = ___a62 - 1
                ___bn9 = ___a58
                ___bm10 = ___a66 + 1
                ___a57 = cmp_Nat(___bm10, ___t7[0])
                if ___a57 == 1:
                    ___a61 = ___bn9
                    ___a63 = ___a64 + 1
                    ___a65 = 0
                    ___a62 = ___a61
                    ___a64 = ___a63
                    ___a66 = ___a65
                else:
                    ___a61 = ___bn9
                    ___a65 = ___bm10
                    ___a62 = ___a61
                    ___a66 = ___a65
            ___t8 = ___a59 == 1
        return ___a60
    if ___bdivisor0 > 0:
        ___a67 = ___bdivisor0 - 1
        ___t9 = ___a67 == 0
    else:
        ___t9 = False
    return (___bnumerator0, 0) if ___t9 else ___bloop5(___bnumerator0,
        0,
        0) if ___bdivisor0 > 0 else (0, ___bnumerator0)

one = 1

def exp(___bbase0, ___bpower0):
    if ___bbase0 == 0:
        return one if ___bpower0 == 0 else 0
    else:
        if ___bbase0 > 0:
            ___a75 = ___bbase0 - 1
            ___t12 = ___a75 == 0
        else:
            ___t12 = False
        if ___t12:
            return one
        else:
            ___btwo__or__more0 = ___bbase0
            ___t10 = (___btwo__or__more0,)
            def ___bloop6(___bpower1, ___bacc2):
                ___a72 = ___bpower1
                ___a74 = ___bacc2
                ___a69 = 1
                ___t11 = ___a69 == 1
                while ___t11:
                    if ___a72 == 0:
                        ___a69 = 0
                        ___a70 = ___a74
                    else:
                        ___a68 = ___a72 - 1
                        ___bprev1 = ___a68
                        ___a71 = ___bprev1
                        ___a73 = mult(___a74, ___t10[0])
                        ___a72 = ___a71
                        ___a74 = ___a73
                    ___t11 = ___a69 == 1
                return ___a70
            return ___bloop6(___bpower0, one)

def to_Int(___bn10):
    def ___bloop7(___bacc3, ___bn11):
        ___a80 = ___bacc3
        ___a82 = ___bn11
        ___a77 = 1
        ___t13 = ___a77 == 1
        while ___t13:
            if ___a82 == 0:
                ___a77 = 0
                ___a78 = ___a80
            else:
                ___a76 = ___a82 - 1
                ___bn12 = ___a76
                ___a79 = ___a80 + 1
                ___a81 = ___bn12
                ___a80 = ___a79
                ___a82 = ___a81
            ___t13 = ___a77 == 1
        return ___a78
    return ___bloop7(0, ___bn10)

def to_Nat(___bi0):
    ___t15 = 0 < ___bi0
    ___t16 = 0
    ___t17 = ___bi0
    ___t18 = 0
    while ___t15:
        ___t19 = ___t17 + -1
        ___t18 = ___t18 + 1
        ___t15 = (0 < ___t19) and (___t19 < ___t17)
        ___t17 = ___t19
    return ___t18

n1 = 1

n2 = n1 + 1

n3 = n2 + 1

n4 = n3 + 1

n5 = n4 + 1

def addLaw(___bn16, ___bn26, ___blabel0):
    return (0, to_Int(add(___bn16, ___bn26)) == (to_Int(___bn16) + to_Int(___bn26)), ___blabel0)

def multLaw(___bn17, ___bn27, ___blabel1):
    return (0, to_Int(mult(___bn17, ___bn27)) == (to_Int(___bn17) * to_Int(___bn27)), ___blabel1)

def from_to_law(___bi2, ___bmessage0):
    ___t21 = 0 < ___bi2
    ___t22 = 0
    ___t23 = ___bi2
    ___t24 = 0
    while ___t21:
        ___t25 = ___t23 + -1
        ___t24 = ___t24 + 1
        ___t21 = (0 < ___t25) and (___t25 < ___t23)
        ___t23 = ___t25
    return (0, to_Int(___t24) == ___bi2, ___bmessage0)

___t27 = 0 < -1
___t28 = 0
___t29 = -1
___t30 = 0
while ___t27:
    ___t31 = ___t29 + -1
    ___t30 = ___t30 + 1
    ___t27 = (0 < ___t31) and (___t31 < ___t29)
    ___t29 = ___t31
___t33 = 0 < -42
___t34 = 0
___t35 = -42
___t36 = 0
while ___t33:
    ___t37 = ___t35 + -1
    ___t36 = ___t36 + 1
    ___t33 = (0 < ___t37) and (___t37 < ___t35)
    ___t35 = ___t37
from_to_suite = (1,
    "to_Nat/to_Int tests",
    (1,
        (0, to_Int(___t30) == 0, "-1 -> 0"),
        (1,
            (0, to_Int(___t36) == 0, "-42 -> 0"),
            (1,
                from_to_law(0, "0"),
                (1,
                    from_to_law(1, "1"),
                    (1, from_to_law(10, "10"), (1, from_to_law(42, "42"), (0,))))))))

___a83 = to_Int(exp(n2, n5))
___a84 = to_Int(div2(n1))
___a85 = to_Int(div2(n2))
___a86 = to_Int(div2(n3))
___a87 = to_Int(div2(n4))
tests = (1,
    "Nat tests",
    (1,
        addLaw(0, 0, "0 + 0"),
        (1,
            addLaw(0, n1, "0 + 1"),
            (1,
                addLaw(n1, 0, "1 + 0"),
                (1,
                    addLaw(n1, n2, "1 + 2"),
                    (1,
                        addLaw(n2, n1, "2 + 1"),
                        (1,
                            multLaw(0, 0, "0 * 0"),
                            (1,
                                multLaw(0, n1, "0 * 1"),
                                (1,
                                    multLaw(n1, 0, "1 * 0"),
                                    (1,
                                        multLaw(n1, n2, "1 * 2"),
                                        (1,
                                            multLaw(n2, n1, "2 * 1"),
                                            (1,
                                                from_to_suite,
                                                (1,
                                                    (0, ___a83 == 32, "exp(2, 5) == 32"),
                                                    (1,
                                                        (0, ___a84 == 0, "1 div2 == 0"),
                                                        (1,
                                                            (0, ___a85 == 1, "2 div2 == 1"),
                                                            (1,
                                                                (0, ___a86 == 1, "3 div2 == 1"),
                                                                (1,
                                                                    (0, ___a87 == 2, "4 div2 == 2"),
                                                                    (0,))))))))))))))))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t38 = value[2]
                while ___t38[0] != 0:
                    test_loop(___t38[1])
                    ___t38 = ___t38[2]
        test_loop(tests)