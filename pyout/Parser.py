import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

empty = lambda ___ba0: (0,)

def parse(___bp0, ___bstr0):
    ___bfn0 = ___bp0
    ___a0 = ___bfn0(___bstr0)
    if ___a0[0] == 1:
        return (1, ___a0[1][1])
    else:
        return (0,)

def expect(___bstr1):
    def ___t1(___bs0):
        if ___bstr1:
            ___t2 = ___bs0.partition(___bstr1)
            ___a2 = (1, (___t2[0], ___t2[2])) if ___t2[1] else (0,)
        else:
            ___a2 = (0,)
        if ___a2[0] == 1:
            ___a3 = ___a2[1]
            ___a4 = ___a3[0]
            ___t3 = ___a4 == ""
        else:
            ___t3 = False
        if ___t3:
            return (1, (___a3[1], ()))
        else:
            return (0,)
    return ___t1

def map(___bp1, ___bfn1):
    ___bpfn0 = ___bp1
    def ___t4(___bs1):
        ___a5 = ___bpfn0(___bs1)
        if ___a5[0] == 1:
            ___a6 = ___a5[1]
            return (1, (___a6[0], ___bfn1(___a6[1])))
        else:
            return (0,)
    return ___t4

def flat_map(___bp2, ___bfn2):
    ___bfa0 = ___bp2
    def ___t5(___bs00):
        ___a8 = ___bfa0(___bs00)
        if ___a8[0] == 1:
            ___a9 = ___a8[1]
            return ___bfn2(___a9[1])(___a9[0])
        else:
            return (0,)
    return ___t5

def one_of(___bps0):
    if ___bps0[0] == 0:
        return empty
    else:
        if ___bps0[0] == 1:
            ___a12 = ___bps0[2]
            ___t7 = ___a12[0] == 0
        else:
            ___t7 = False
        if ___t7:
            return ___bps0[1]
        else:
            ___a13 = ___bps0[1]
            ___bheadFn0 = ___a13
            ___bprest0 = ___bps0[2]
            ___a11 = one_of(___bprest0)
            ___btailFn0 = ___a11
            def ___t6(___bs2):
                ___a10 = ___bheadFn0(___bs2)
                return ___btailFn0(___bs2) if ___a10[0] == 0 else ___a10
            return ___t6

def then_parse(___bpa0, ___bpb0):
    ___bfa1 = ___bpa0
    ___bfb1 = ___bpb0
    def ___t8(___bs01):
        ___a16 = ___bfa1(___bs01)
        if ___a16[0] == 1:
            ___a17 = ___a16[1]
            ___bs11 = ___a17[0]
            ___ba4 = ___a17[1]
            ___a14 = ___bfb1(___bs11)
            if ___a14[0] == 1:
                ___a15 = ___a14[1]
                return (1, (___a15[0], (___ba4, ___a15[1])))
            else:
                return (0,)
        else:
            return (0,)
    return ___t8

def expect_int(___bi0):
    ___bstr2 = ___bi0.__str__()
    def ___t9(___bs3):
        if ___bstr2:
            ___t10 = ___bs3.partition(___bstr2)
            ___a18 = (1, (___t10[0], ___t10[2])) if ___t10[1] else (0,)
        else:
            ___a18 = (0,)
        if ___a18[0] == 1:
            ___a19 = ___a18[1]
            ___a20 = ___a19[0]
            ___t11 = ___a20 == ""
        else:
            ___t11 = False
        if ___t11:
            return (1, (___a19[1], ()))
        else:
            return (0,)
    return map(___t9, lambda ___ba5: ___bi0)

digit = one_of(___iPredef0.map_List(___iPredef0.range(10), expect_int))

def recur_max(___bn0, ___bfn3, ___bin0):
    ___a26 = ___bn0
    ___a28 = ___bfn3
    ___a30 = ___bin0
    ___a23 = 1
    ___t13 = ___a23 == 1
    while ___t13:
        if ___a26 == 0:
            ___a23 = 0
            ___a24 = (0,)
        else:
            ___a22 = ___a26 - 1
            ___bprev0 = ___a22
            ___a21 = ___a28(___a30)
            if ___a21[0] == 0:
                ___bin10 = ___a21[1]
                ___a25 = ___bprev0
                ___a29 = ___bin10
                ___a26 = ___a25
                ___a30 = ___a29
            else:
                ___a23 = 0
                ___bout0 = ___a21[1]
                ___a24 = (1, ___bout0)
        ___t13 = ___a23 == 1
    return ___a24

def digits_n(___bat__most0):
    ___bfn4 = digit
    def ___t14(___bs4):
        ___bin1 = (___bs4, -1)
        ___t16 = 0 < ___bat__most0
        ___t17 = 0
        ___t18 = ___bat__most0
        ___t19 = 0
        while ___t16:
            ___t20 = ___t18 + -1
            ___t19 = ___t19 + 1
            ___t16 = (0 < ___t20) and (___t20 < ___t18)
            ___t18 = ___t20
        ___t22 = (___bfn4,)
        def ___t21(___bin2):
            ___bs02 = ___bin2[0]
            ___bacc00 = ___bin2[1]
            ___a33 = ___t22[0](___bs02)
            if ___a33[0] == 1:
                ___a34 = ___a33[1]
                ___bs12 = ___a34[0]
                ___bd0 = ___a34[1]
                ___a31 = ___bacc00 == -1
                ___bacc0 = 0 if ___a31 == 1 else ___bacc00
                return (0,
                    (___bs12,
                        ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bacc0 + ___bd0))
            else:
                return (1, (0,)) if (___bacc00 == -1) == 1 else (1, (1, (___bs02, ___bacc00)))
        ___a35 = recur_max(___t19 + 1, ___t21, ___bin1)
        if ___a35[0] == 1:
            ___a36 = ___a35[1]
            ___t23 = ___a36[0] == 1
        else:
            ___t23 = False
        if ___t23:
            return (1, ___a36[1])
        else:
            return (0,)
    return ___t14

___a37 = (1, ("foo", 1))
if ___a37[0] == 1:
    ___a38 = ___a37[1]
    ___ba7 = ___a38[1]
    ___a39 = (1, ___ba7)
else:
    ___a39 = (0,)
if ___a39[0] == 1:
    ___a40 = ___a39[1]
    ___t24 = ___a40 == 1
else:
    ___t24 = False
if "fo":
    ___t25 = "foo".partition("fo")
    ___a41 = (1, (___t25[0], ___t25[2])) if ___t25[1] else (0,)
else:
    ___a41 = (0,)
if ___a41[0] == 1:
    ___a42 = ___a41[1]
    ___a43 = ___a42[0]
    ___t26 = ___a43 == ""
else:
    ___t26 = False
if ___t26:
    ___brest3 = ___a42[1]
    ___a44 = (1, (___brest3, ()))
else:
    ___a44 = (0,)
if ___a44[0] == 1:
    ___a45 = ___a44[1]
    ___ba8 = ___a45[1]
    ___a46 = (1, ___ba8)
else:
    ___a46 = (0,)
def ___t27(___bs5):
    if "fo":
        ___t28 = ___bs5.partition("fo")
        ___a47 = (1, (___t28[0], ___t28[2])) if ___t28[1] else (0,)
    else:
        ___a47 = (0,)
    if ___a47[0] == 1:
        ___a48 = ___a47[1]
        ___a49 = ___a48[0]
        ___t29 = ___a49 == ""
    else:
        ___t29 = False
    if ___t29:
        return (1, (___a48[1], ()))
    else:
        return (0,)
def ___t30(___bs6):
    if "o":
        ___t31 = ___bs6.partition("o")
        ___a50 = (1, (___t31[0], ___t31[2])) if ___t31[1] else (0,)
    else:
        ___a50 = (0,)
    if ___a50[0] == 1:
        ___a51 = ___a50[1]
        ___a52 = ___a51[0]
        ___t32 = ___a52 == ""
    else:
        ___t32 = False
    if ___t32:
        return (1, (___a51[1], ()))
    else:
        return (0,)
___a55 = then_parse(___t27, ___t30)
___bfn5 = ___a55
___a53 = ___bfn5("foo")
if ___a53[0] == 1:
    ___a54 = ___a53[1]
    ___ba9 = ___a54[1]
    ___a56 = (1, ___ba9)
else:
    ___a56 = (0,)
___bfn6 = digit
___a57 = ___bfn6("2")
if ___a57[0] == 1:
    ___a58 = ___a57[1]
    ___ba10 = ___a58[1]
    ___a59 = (1, ___ba10)
else:
    ___a59 = (0,)
if ___a59[0] == 1:
    ___a60 = ___a59[1]
    ___t33 = ___a60 == 2
else:
    ___t33 = False
___bfn7 = digit
___a61 = ___bfn7("9")
if ___a61[0] == 1:
    ___a62 = ___a61[1]
    ___ba11 = ___a62[1]
    ___a63 = (1, ___ba11)
else:
    ___a63 = (0,)
if ___a63[0] == 1:
    ___a64 = ___a63[1]
    ___t34 = ___a64 == 9
else:
    ___t34 = False
___a67 = digits_n(10)
___bfn8 = ___a67
___a65 = ___bfn8("4242")
if ___a65[0] == 1:
    ___a66 = ___a65[1]
    ___ba12 = ___a66[1]
    ___a68 = (1, ___ba12)
else:
    ___a68 = (0,)
if ___a68[0] == 1:
    ___a69 = ___a68[1]
    ___t35 = ___a69 == 4242
else:
    ___t35 = False
___a72 = digits_n(10)
___bfn9 = ___a72
___a70 = ___bfn9("4242")
if ___a70[0] == 1:
    ___a71 = ___a70[1]
    ___ba13 = ___a71[1]
    ___a73 = (1, ___ba13)
else:
    ___a73 = (0,)
if ___a73[0] == 1:
    ___a74 = ___a73[1]
    ___t36 = ___a74 == 4242
else:
    ___t36 = False
___a77 = digits_n(3)
___bfn10 = ___a77
___a75 = ___bfn10("4242")
if ___a75[0] == 1:
    ___a76 = ___a75[1]
    ___ba14 = ___a76[1]
    ___a78 = (1, ___ba14)
else:
    ___a78 = (0,)
___a81 = digits_n(4)
___bfn11 = ___a81
___a79 = ___bfn11("4242")
if ___a79[0] == 1:
    ___a80 = ___a79[1]
    ___ba15 = ___a80[1]
    ___a82 = (1, ___ba15)
else:
    ___a82 = (0,)
if ___a82[0] == 1:
    ___a83 = ___a82[1]
    ___t37 = ___a83 == 4242
else:
    ___t37 = False
tests = (1,
    "Parser tests",
    (1,
        (0, ___t24, "pure"),
        (1,
            (0, ___a46[0] == 1, "expect(fo)"),
            (1,
                (0, ___a56[0] == 1, "expect(fo)"),
                (1,
                    (0, ___t33, "digit.parse(2)"),
                    (1,
                        (0, ___t34, "digit.parse(2)"),
                        (1,
                            (0, ___t35, "digits_n(10).parse(4242)"),
                            (1,
                                (0, ___t36, "digits_n(10).parse(4242)"),
                                (1,
                                    (0, ___a78[0] == 0, "digits_n(3).parse(4242)"),
                                    (1, (0, ___t37, "digits_n(4).parse(4242)"), (0,)))))))))))

class BosatsuTests(___iunittest1.TestCase):
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