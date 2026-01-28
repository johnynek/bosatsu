import Bosatsu.Predef as ___iPredef0
import Bosatsu.Rand as ___iRand1
import unittest as ___iunittest2

def forall_Prop(___brand0, ___bname0, ___bfn0):
    ___t1 = (___brand0, ___bname0, ___bfn0)
    def ___t0(___bcnt0):
        ___brands0 = ___iPredef0.replicate_List(___t1[0], ___bcnt0)
        ___bseq0 = ___iRand1.sequence_Rand(___brands0)
        ___t3 = (___t1[1], ___t1[2])
        def ___t2(___bas0):
            return (1, ___t3[0], ___iPredef0.map_List(___bas0, ___t3[1]))
        return ___iRand1.map_Rand(___bseq0, ___t2)
    return ___t0

def suite_Prop(___bname1, ___bprops0):
    ___t5 = (___bprops0, ___bname1)
    def ___t4(___bs0):
        ___t7 = (___bs0,)
        def ___t6(___ba0):
            return ___ba0(___t7[0])
        ___bs1 = ___iRand1.sequence_Rand(___iPredef0.map_List(___t5[0], ___t6))
        ___t9 = (___t5[1],)
        def ___t8(___bas1):
            return (1, ___t9[0], ___bas1)
        return ___iRand1.map_Rand(___bs1, ___t8)
    return ___t4

def run_Prop(___bprop0, ___btrials0, ___bseed0):
    return ___iRand1.run_Rand(___bprop0(___btrials0), ___bseed0)

signed64 = ___iRand1.map_Rand(___iRand1.int_range(18446744073709551616),
    lambda ___bi0: ___bi0 - 9223372036854775808)

def ___t11(___bi1):
    ___bistr0 = ___bi1.__str__()
    ___t12 = []
    ___t13 = (1, "~", (1, ___bistr0, (1, " == (-1 - ", (1, ___bistr0, (1, ")", (0,))))))
    while ___t13[0] != 0:
        ___t12.append(___t13[1])
        ___t13 = ___t13[2]
    return (0, True, "".join(___t12))
not_law = forall_Prop(signed64, "not_law", ___t11)

def ___t14(___ba1):
    ___bi2 = ___ba1[0]
    ___bk0 = ___ba1[1]
    ___bistr1 = ___bi2.__str__()
    ___bkstr0 = ___bk0.__str__()
    ___bresult0 = (___bi2 << ___bk0) >> ___bk0
    ___t15 = []
    ___t16 = (1,
        "(",
        (1,
            ___bistr1,
            (1,
                " << ",
                (1,
                    ___bkstr0,
                    (1,
                        ") >> ",
                        (1,
                            ___bkstr0,
                            (1,
                                " == ",
                                (1,
                                    ___bistr1,
                                    (1, ", got: ", (1, ___bresult0.__str__(), (0,)))))))))))
    while ___t16[0] != 0:
        ___t15.append(___t16[1])
        ___t16 = ___t16[2]
    return (0, ___bresult0 == ___bi2, "".join(___t15))
shift_unshift_law = forall_Prop(___iRand1.prod_Rand(signed64, ___iRand1.int_range(32)),
    "shift_unshift_law",
    ___t14)

def ___t17(___ba2):
    ___bx0 = ___ba2[0]
    ___by0 = ___ba2[1]
    ___bxs0 = ___bx0.__str__()
    ___bys0 = ___by0.__str__()
    ___a0 = 0 if (___bx0 & ___by0) < 0 else 1 if (___bx0 & ___by0) == 0 else 2
    ___a4 = 1 if ___a0 == 2 else ___a0 == 1
    if ___a4 == 1:
        ___a1 = 0 if ___bx0 < 0 else 1 if ___bx0 == 0 else 2
        ___a3 = 1 if ___a1 == 2 else ___a1 == 1
        if ___a3 == 1:
            ___t20 = 1
        else:
            ___a2 = 0 if ___by0 < 0 else 1 if ___by0 == 0 else 2
            ___t20 = 1 if ___a2 == 2 else ___a2 == 1
    else:
        ___t20 = 1
    ___t18 = []
    ___t19 = (1,
        ___bxs0,
        (1,
            " & ",
            (1,
                ___bys0,
                (1,
                    " is >= 0 implies ",
                    (1, ___bxs0, (1, " >= 0 or ", (1, ___bys0, (1, " >= 0", (0,)))))))))
    while ___t19[0] != 0:
        ___t18.append(___t19[1])
        ___t19 = ___t19[2]
    return (0, ___t20, "".join(___t18))
positive_and_law = forall_Prop(___iRand1.prod_Rand(signed64, signed64),
    "x & y is >= 0 implies x >= 0 or y >= 0",
    ___t17)

all_props = suite_Prop("integer props",
    (1, not_law, (1, shift_unshift_law, (1, positive_and_law, (0,)))))

___bfn3 = all_props
test = ___iRand1.run_Rand(___bfn3(100), 42)

class BosatsuTests(___iunittest2.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t21 = value[2]
                while ___t21[0] != 0:
                    test_loop(___t21[1])
                    ___t21 = ___t21[2]
        test_loop(test)