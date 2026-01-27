import Bosatsu.BinInt as ___iBinInt3
import Bosatsu.BinNat as ___iBinNat2
import Bosatsu.Nat as ___iNat1
import Bosatsu.Predef as ___iPredef5
import Bosatsu.Properties as ___iProperties4
import Bosatsu.Rand as ___iRand0
import unittest as ___iunittest6

rand_Int = ___iRand0.from_pair(___iRand0.int_range(128), ___iRand0.geometric_Int)

rand_Nat = ___iRand0.map_Rand(rand_Int, ___iNat1.to_Nat)

rand_BinNat = ___iRand0.map_Rand(rand_Int, ___iBinNat2.toBinNat)

___bpos0 = ___iRand0.map_Rand(rand_Int, ___iBinInt3.int_to_BinInt)
rand_BinInt = ___iRand0.from_pair(___bpos0, ___iRand0.map_Rand(___bpos0, ___iBinInt3.___nnot))

def ___t0(___bc0):
    ___ba0 = ___bc0[0]
    ___bb0 = ___bc0[1]
    return (0,
        (((___ba0 // ___bb0 if ___bb0 else 0) * ___bb0) + (___ba0 % ___bb0 if ___bb0 else ___ba0)) == ___ba0,
        "check")
int_props = ___iProperties4.suite_Prop("Int props",
    (1,
        ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Int, rand_Int), "divmod law", ___t0),
        (0,)))

def cmp_Comparison(___bc10, ___bc20):
    return ___bc20 == 0 if ___bc10 == 0 else (2 if ___bc20 == 0 else ___bc20 == 1) if ___bc10 == 1 else 1 if ___bc20 == 2 else 2

def exp_Int(___bbase0, ___bpower0):
    ___t2 = (___bbase0,)
    def ___t1(___bp0, ___bacc0):
        return (___bp0 + -1, ___bacc0 * ___t2[0])
    ___t3 = 0 < ___bpower0
    ___t4 = 1
    ___t5 = ___bpower0
    ___t6 = 1
    while ___t3:
        ___t4 = ___t1(___t5, ___t6)
        ___t7 = ___t4[0]
        ___t6 = ___t4[1]
        ___t3 = (0 < ___t7) and (___t7 < ___t5)
        ___t5 = ___t7
    return ___t6

small_rand_Nat = ___iRand0.map_Rand(___iRand0.int_range(7), ___iNat1.to_Nat)

small_rand_BinNat = ___iRand0.map_Rand(___iRand0.int_range(7), ___iBinNat2.toBinNat)

def ___t8(___bn0):
    ___a2 = ___iNat1.is_even(___bn0)
    if ___a2 == 1:
        return (0,
            ___iNat1.cmp_Nat(___iNat1.times2(___iNat1.div2(___bn0)), ___bn0) == 1,
            "times2/div2")
    else:
        return (0,
            ___iNat1.cmp_Nat(___iNat1.times2(___iNat1.div2(___bn0)) + 1, ___bn0) == 1,
            "times2/div2")
def ___t9(___ba1):
    ___bn10 = ___ba1[0]
    ___bn20 = ___ba1[1]
    return (0,
        cmp_Comparison(___iNat1.cmp_Nat(___bn10, ___bn20),
            0 if ___iNat1.to_Int(___bn10) < ___iNat1.to_Int(___bn20) else 1 if ___iNat1.to_Int(___bn10) == ___iNat1.to_Int(___bn20) else 2) == 1,
        "cmp_Nat")
def ___t10(___ba2):
    ___bn11 = ___ba2[0]
    ___bn21 = ___ba2[1]
    return (0,
        ___iNat1.to_Int(___iNat1.add(___bn11,
                ___bn21)) == (___iNat1.to_Int(___bn11) + ___iNat1.to_Int(___bn21)),
        "add homomorphism")
def ___t11(___ba3):
    ___bn12 = ___ba3[0]
    ___bn22 = ___ba3[1]
    ___bi10 = ___iNat1.to_Int(___bn12)
    ___bi20 = ___iNat1.to_Int(___bn22)
    ___a5 = 0 if ___bi10 < ___bi20 else 1 if ___bi10 == ___bi20 else 2
    if ___a5 == 1:
        return (0,
            ___iNat1.to_Int(___iNat1.sub_Nat(___bn12, ___bn22)) == (___bi10 - ___bi20),
            "sub_Nat homomorphism")
    elif ___a5 == 2:
        return (0,
            ___iNat1.to_Int(___iNat1.sub_Nat(___bn12, ___bn22)) == (___bi10 - ___bi20),
            "sub_Nat homomorphism")
    else:
        return (0, ___iNat1.sub_Nat(___bn12, ___bn22) == 0, "sub to zero")
def ___t12(___ba4):
    ___bn13 = ___ba4[0]
    ___bn23 = ___ba4[1]
    return (0,
        ___iNat1.to_Int(___iNat1.mult(___bn13,
                ___bn23)) == (___iNat1.to_Int(___bn13) * ___iNat1.to_Int(___bn23)),
        "mult homomorphism")
def ___t13(___ba5):
    ___bn14 = ___ba5[0]
    ___bn24 = ___ba5[1]
    return (0,
        ___iNat1.to_Int(___iNat1.exp(___bn14, ___bn24)) == exp_Int(___iNat1.to_Int(___bn14),
            ___iNat1.to_Int(___bn24)),
        "exp homomorphism")
def ___t14(___bn1):
    return (0,
        ___iNat1.cmp_Nat(___iNat1.times2(___bn1), ___iNat1.mult(___bn1, 2)) == 1,
        "times2 == mult(2, _)")
def ___t15(___ba6):
    ___bn15 = ___ba6[0]
    ___bn25 = ___ba6[1]
    ___a10 = ___iNat1.divmod(___bn15, ___bn25)
    ___bdn0 = ___a10[0]
    ___bmn0 = ___a10[1]
    ___a7 = (___iNat1.to_Int(___bdn0) == (___iNat1.to_Int(___bn15) // ___iNat1.to_Int(___bn25) if ___iNat1.to_Int(___bn25) else 0),
        ___iNat1.to_Int(___bmn0) == (___iNat1.to_Int(___bn15) % ___iNat1.to_Int(___bn25) if ___iNat1.to_Int(___bn25) else ___iNat1.to_Int(___bn15)))
    return (0, (___a7[0] == 1) and (___a7[1] == 1), "div Nat")
nat_props = ___iProperties4.suite_Prop("Nat props",
    (1,
        ___iProperties4.forall_Prop(rand_Nat, "if is_even(n) then times2(div2(n)) == n", ___t8),
        (1,
            ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Nat, rand_Nat),
                "cmp_Nat matches cmp_Int",
                ___t9),
            (1,
                ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Nat, rand_Nat),
                    "add homomorphism",
                    ___t10),
                (1,
                    ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Nat, rand_Nat),
                        "sub_Nat homomorphism",
                        ___t11),
                    (1,
                        ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Nat, rand_Nat),
                            "mult homomorphism",
                            ___t12),
                        (1,
                            ___iProperties4.forall_Prop(___iRand0.prod_Rand(small_rand_Nat,
                                    small_rand_Nat),
                                "exp homomorphism",
                                ___t13),
                            (1,
                                ___iProperties4.forall_Prop(rand_Nat,
                                    "times2 == x -> mult(2, x)",
                                    ___t14),
                                (1,
                                    ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_Nat,
                                            rand_Nat),
                                        "divmod homomorphism",
                                        ___t15),
                                    (0,))))))))))

def ___t16(___bn2):
    ___a13 = ___iBinNat2.is_even(___bn2)
    if ___a13 == 1:
        ___bn16 = ___iBinNat2.times2(___iBinNat2.div2(___bn2))
        ___a11 = ___iBinNat2.cmp_BinNat(___bn16, ___bn2)
        ___t17 = []
        ___t18 = (1,
            "even, times2/div2: n = ",
            (1,
                ___iBinNat2.toInt(___bn2).__str__(),
                (1, ", n1 = ", (1, ___iBinNat2.toInt(___bn16).__str__(), (0,)))))
        while ___t18[0] != 0:
            ___t17.append(___t18[1])
            ___t18 = ___t18[2]
        return (0, ___a11 == 1, "".join(___t17))
    else:
        ___bn17 = ___iBinNat2.times2(___iBinNat2.div2(___bn2))
        ___a12 = ___iBinNat2.cmp_BinNat(___iBinNat2.next(___bn17), ___bn2)
        ___t19 = []
        ___t20 = (1,
            "times2/div2: n = ",
            (1,
                ___iBinNat2.toInt(___bn2).__str__(),
                (1, ", n1 = ", (1, ___iBinNat2.toInt(___bn17).__str__(), (0,)))))
        while ___t20[0] != 0:
            ___t19.append(___t20[1])
            ___t20 = ___t20[2]
        return (0, ___a12 == 1, "".join(___t19))
def ___t21(___ba7):
    ___bn18 = ___ba7[0]
    ___bn26 = ___ba7[1]
    return (0,
        cmp_Comparison(___iBinNat2.cmp_BinNat(___bn18, ___bn26),
            0 if ___iBinNat2.toInt(___bn18) < ___iBinNat2.toInt(___bn26) else 1 if ___iBinNat2.toInt(___bn18) == ___iBinNat2.toInt(___bn26) else 2) == 1,
        "cmp_BinNat")
def ___t22(___ba8):
    ___bn19 = ___ba8[0]
    ___bn27 = ___ba8[1]
    ___a15 = ___iBinNat2.cmp_BinNat(___bn19, ___bn27)
    ___a16 = (___a15 == 1, ___iBinNat2.eq_BinNat(___bn19, ___bn27))
    ___a18 = ___a16[1]
    ___a17 = ___a16[0]
    if (___a17 == 1) and (___a18 == 1):
        ___t23 = 1
    else:
        ___a20 = ___a16[1]
        ___a19 = ___a16[0]
        ___t23 = (___a19 == 0) and (___a20 == 0)
    return (0, ___t23, "cmp vs eq consistency")
def ___t24(___ba9):
    ___bn110 = ___ba9[0]
    ___bn28 = ___ba9[1]
    return (0,
        ___iBinNat2.toInt(___iBinNat2.add_BinNat(___bn110,
                ___bn28)) == (___iBinNat2.toInt(___bn110) + ___iBinNat2.toInt(___bn28)),
        "add homomorphism")
def ___t25(___ba10):
    ___bn111 = ___ba10[0]
    ___bn29 = ___ba10[1]
    ___bi11 = ___iBinNat2.toInt(___bn111)
    ___bi21 = ___iBinNat2.toInt(___bn29)
    ___a22 = 0 if ___bi11 < ___bi21 else 1 if ___bi11 == ___bi21 else 2
    if ___a22 == 1:
        return (0,
            ___iBinNat2.toInt(___iBinNat2.sub_BinNat(___bn111, ___bn29)) == (___bi11 - ___bi21),
            "sub_BinNat homomorphism")
    elif ___a22 == 2:
        return (0,
            ___iBinNat2.toInt(___iBinNat2.sub_BinNat(___bn111, ___bn29)) == (___bi11 - ___bi21),
            "sub_BinNat homomorphism")
    else:
        return (0, ___iBinNat2.sub_BinNat(___bn111, ___bn29)[0] == 0, "sub to zero")
def ___t26(___ba11):
    ___bn112 = ___ba11[0]
    ___bn210 = ___ba11[1]
    ___a25 = ___iBinNat2.sub_Option(___bn112, ___bn210)
    if ___a25[0] == 1:
        return (0,
            ___iBinNat2.cmp_BinNat(___a25[1], ___iBinNat2.sub_BinNat(___bn112, ___bn210)) == 1,
            "sub_BinNat same as sub_BinNat_Option when Some")
    else:
        return (0, ___iBinNat2.cmp_BinNat(___bn112, ___bn210) == 0, "otherwise n1 < n2")
def ___t27(___ba12):
    ___bn113 = ___ba12[0]
    ___bn211 = ___ba12[1]
    return (0,
        ___iBinNat2.toInt(___iBinNat2.times_BinNat(___bn113,
                ___bn211)) == (___iBinNat2.toInt(___bn113) * ___iBinNat2.toInt(___bn211)),
        "mult homomorphism")
def ___t28(___ba13):
    ___bn114 = ___ba13[0]
    ___bn212 = ___ba13[1]
    return (0,
        ___iBinNat2.toInt(___iBinNat2.exp(___bn114,
                ___bn212)) == exp_Int(___iBinNat2.toInt(___bn114), ___iBinNat2.toInt(___bn212)),
        "exp homomorphism")
def ___t29(___bn3):
    return (0,
        ___iBinNat2.cmp_BinNat(___iBinNat2.times2(___bn3),
            ___iBinNat2.times_BinNat(___bn3, (2, (0,)))) == 1,
        "times2 == mult(2, _)")
def ___t30(___ba14):
    ___bn115 = ___ba14[0]
    ___bn213 = ___ba14[1]
    ___a30 = ___iBinNat2.divmod(___bn115, ___bn213)
    ___bdn1 = ___a30[0]
    ___bmn1 = ___a30[1]
    ___a27 = (___iBinNat2.toInt(___bdn1) == (___iBinNat2.toInt(___bn115) // ___iBinNat2.toInt(___bn213) if ___iBinNat2.toInt(___bn213) else 0),
        ___iBinNat2.toInt(___bmn1) == (___iBinNat2.toInt(___bn115) % ___iBinNat2.toInt(___bn213) if ___iBinNat2.toInt(___bn213) else ___iBinNat2.toInt(___bn115)))
    return (0, (___a27[0] == 1) and (___a27[1] == 1), "div BinNat")
binnat_props = ___iProperties4.suite_Prop("BinNat props",
    (1,
        ___iProperties4.forall_Prop(rand_BinNat, "if is_even(n) then times2(div2(n)) == n", ___t16),
        (1,
            ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat, rand_BinNat),
                "cmp_BinNat matches cmp_Int",
                ___t21),
            (1,
                ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat, rand_BinNat),
                    "cmp_BinNat matches eq_BinNat",
                    ___t22),
                (1,
                    ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat, rand_BinNat),
                        "add homomorphism",
                        ___t24),
                    (1,
                        ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat, rand_BinNat),
                            "sub_BinNat homomorphism",
                            ___t25),
                        (1,
                            ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat,
                                    rand_BinNat),
                                "sub_BinNat_Option is None implies a < b",
                                ___t26),
                            (1,
                                ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat,
                                        rand_BinNat),
                                    "mult homomorphism",
                                    ___t27),
                                (1,
                                    ___iProperties4.forall_Prop(___iRand0.prod_Rand(small_rand_BinNat,
                                            small_rand_BinNat),
                                        "exp homomorphism",
                                        ___t28),
                                    (1,
                                        ___iProperties4.forall_Prop(rand_BinNat,
                                            "times2 == x -> mult(2, x)",
                                            ___t29),
                                        (1,
                                            ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinNat,
                                                    rand_BinNat),
                                                "divmod homomorphism",
                                                ___t30),
                                            (0,))))))))))))

def ___t31(___ba15):
    ___bn116 = ___ba15[0]
    ___bn214 = ___ba15[1]
    return (0,
        ___iBinInt3.binInt_to_Int(___iBinInt3.add(___bn116,
                ___bn214)) == (___iBinInt3.binInt_to_Int(___bn116) + ___iBinInt3.binInt_to_Int(___bn214)),
        "add BinInt")
def ___t32(___ba16):
    ___bn117 = ___ba16[0]
    ___bn215 = ___ba16[1]
    return (0,
        ___iBinInt3.binInt_to_Int(___iBinInt3.sub(___bn117,
                ___bn215)) == (___iBinInt3.binInt_to_Int(___bn117) - ___iBinInt3.binInt_to_Int(___bn215)),
        "sub BinInt")
def ___t34(___bx1):
    ___bxi0 = ___iBinInt3.binInt_to_Int(___bx1)
    return (0,
        ___iBinInt3.binInt_to_Int(___iBinInt3.add(___bx1,
                ___iBinInt3.binNat_to_BinInt(___iBinInt3.abs(___bx1)))) == (___bxi0 + ___bxi0),
        "x + |x| == 2|x|") if (0 if ___bxi0 < 0 else 1 if ___bxi0 == 0 else 2) == 2 else (0,
        ___iBinInt3.binInt_to_Int(___iBinInt3.add(___bx1,
                ___iBinInt3.binNat_to_BinInt(___iBinInt3.abs(___bx1)))) == 0,
        "x + |x| == 0 if x <= 0")
def ___t35(___bx2):
    ___bz0 = ___iBinInt3.add(___bx2, ___iBinInt3.___nnot(___bx2))
    ___bneg__10 = ___iBinInt3.int_to_BinInt(-1)
    ___a32 = (___iBinInt3.cmp(___bz0, ___bneg__10), ___iBinInt3.eq(___bz0, ___bneg__10))
    return (0, (___a32[0] == 1) and (___a32[1] == 1), "x + not(x) = -1")
binint_props = ___iProperties4.suite_Prop("BinInt props",
    (1,
        ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinInt, rand_BinInt),
            "add homomorphism",
            ___t31),
        (1,
            ___iProperties4.forall_Prop(___iRand0.prod_Rand(rand_BinInt, rand_BinInt),
                "sub homomorphism",
                ___t32),
            (1,
                ___iProperties4.forall_Prop(rand_BinInt,
                    "x + (-x) == 0",
                    lambda ___bx0: (0,
                        ___iBinInt3.binInt_to_Int(___iBinInt3.add(___bx0,
                                ___iBinInt3.negate(___bx0))) == 0,
                        "x + (-x) == 0")),
                (1,
                    ___iProperties4.forall_Prop(rand_BinInt, "x + |x| == 0 or 2x", ___t34),
                    (1,
                        ___iProperties4.forall_Prop(rand_BinInt,
                            "x + not(x) == x - x - 1 = -1",
                            ___t35),
                        (0,)))))))

all_props = (1, int_props, (1, nat_props, (1, binnat_props, (1, binint_props, (0,)))))

test = (1,
    "properties",
    ___iPredef5.map_List(all_props, lambda ___bp1: ___iProperties4.run_Prop(___bp1, 100, 123456)))

class BosatsuTests(___iunittest6.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t37 = value[2]
                while ___t37[0] != 0:
                    test_loop(___t37[1])
                    ___t37 = ___t37[2]
        test_loop(test)