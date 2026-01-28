import Bosatsu.List as ___iList2
import Bosatsu.Nat as ___iNat0
import Bosatsu.Predef as ___iPredef1
import unittest as ___iunittest3

def max_opt(___bo10, ___bo20):
    if ___bo10[0] == 0:
        return ___bo20
    else:
        ___bs10 = ___bo10
        ___bv10 = ___bo10[1]
        if ___bo20[0] == 0:
            return ___bs10
        else:
            ___bs20 = ___bo20
            ___bv20 = ___bo20[1]
            return ___bs20 if ((0 if ___bv20 < ___bv10 else 1 if ___bv20 == ___bv10 else 2) == 2) == 1 else ___bs10

def max_of(___bn0, ___bfn0):
    ___t0 = (___bfn0,)
    def ___bloop0(___bnat0, ___bn1, ___bmax0):
        ___a6 = ___bnat0
        ___a8 = ___bn1
        ___a10 = ___bmax0
        ___a3 = 1
        ___t1 = ___a3 == 1
        while ___t1:
            if ___a6 == 0:
                ___a3 = 0
                ___a4 = max_opt(___a10, ___t0[0](___a8))
            else:
                ___a2 = ___a6 - 1
                ___bprev__nat0 = ___a2
                ___a5 = ___bprev__nat0
                ___a7 = ___a8 + -1
                ___a9 = max_opt(___a10, ___t0[0](___a8))
                ___a6 = ___a5
                ___a8 = ___a7
                ___a10 = ___a9
            ___t1 = ___a3 == 1
        return ___a4
    return ___bloop0(___iNat0.to_Nat(___bn0), ___bn0, (0,))

def first_of(___bn2, ___bfn1):
    ___t2 = (___bfn1,)
    def ___bloop1(___bnat1, ___bn3):
        ___a16 = ___bnat1
        ___a18 = ___bn3
        ___a13 = 1
        ___t3 = ___a13 == 1
        while ___t3:
            if ___a16 == 0:
                ___a13 = 0
                ___a14 = ___t2[0](___a18)
            else:
                ___a12 = ___a16 - 1
                ___bprev__nat1 = ___a12
                ___a11 = ___t2[0](___a18)
                if ___a11[0] == 0:
                    ___a15 = ___bprev__nat1
                    ___a17 = ___a18 + -1
                    ___a16 = ___a15
                    ___a18 = ___a17
                else:
                    ___a13 = 0
                    ___a14 = ___a11
            ___t3 = ___a13 == 1
        return ___a14
    return ___bloop1(___iNat0.to_Nat(___bn2), ___bn2)

def digit_list(___bn4):
    ___t5 = 0 < ___bn4
    ___t6 = (0,)
    ___t7 = ___bn4
    ___t8 = (0,)
    while ___t5:
        ___t9 = ___t7 // 10
        ___t8 = (1, ___t7 % 10, ___t8)
        ___t5 = (0 < ___t9) and (___t9 < ___t7)
        ___t7 = ___t9
    return ___iPredef1.reverse(___t8)

def product_palindrome(___bn10, ___bn20):
    ___bprod0 = ___bn10 * ___bn20
    ___bdigits0 = digit_list(___bprod0)
    return (1, ___bprod0) if ___iList2.eq_List(lambda ___t10, ___t11: ___t10 == ___t11)(___bdigits0,
        ___iPredef1.reverse(___bdigits0)) == 1 else (0,)

def ___t12(___bn11):
    ___t14 = (___bn11,)
    def ___t13(___bn21):
        return product_palindrome(___t14[0], ___bn21)
    return first_of(99, ___t13)
max_pal_opt = max_of(99, ___t12)

max_pal = max_pal_opt[1] if max_pal_opt[0] == 1 else 0

test = (0, max_pal == 9009, "maximum palindrome")

class BosatsuTests(___iunittest3.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t15 = value[2]
                while ___t15[0] != 0:
                    test_loop(___t15[1])
                    ___t15 = ___t15[2]
        test_loop(test)