import Bosatsu.List as ___iList2
import Bosatsu.Option as ___iOption1
import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest3

def applicative_from_pure_map_product(___bpure0, ___bmap0, ___bproduct0):
    ___t1 = (___bmap0, ___bproduct0)
    def ___t0(___bfn0, ___bfa0):
        def ___t2(___bb0):
            return ___bb0[0](___bb0[1])
        return ___t1[0](___t2, ___t1[1](___bfn0, ___bfa0))
    ___t4 = (___bmap0, ___bproduct0)
    def ___t3(___bfa1, ___bfb0, ___bfn2):
        ___t6 = (___bfn2,)
        def ___t5(___bc0):
            return ___t6[0](___bc0[0], ___bc0[1])
        return ___t4[0](___t5, ___t4[1](___bfa1, ___bfb0))
    return (___bpure0, ___bmap0, ___t0, ___t3, ___bproduct0)

def applicative_from_pure_ap(___bpure1, ___bap0):
    ___t8 = (___bap0, ___bpure1)
    def ___t7(___bfn3, ___bfa2):
        return ___t8[0](___t8[1](___bfn3), ___bfa2)
    ___t10 = (___bap0, ___bpure1)
    def ___t9(___bfa3, ___bfb1, ___bfn4):
        ___t12 = (___bfn4,)
        def ___t11(___ba2):
            ___t14 = (___t12[0], ___ba2)
            def ___t13(___bb2):
                return ___t14[0](___t14[1], ___bb2)
            return ___t13
        return ___t10[0](___t10[0](___t10[1](___t11), ___bfa3), ___bfb1)
    ___t16 = (___bap0, ___bpure1)
    def ___t15(___bfa4, ___bfb2):
        def ___t17(___ba3):
            ___t19 = (___ba3,)
            def ___t18(___bb3):
                return (___t19[0], ___bb3)
            return ___t18
        return ___t16[0](___t16[0](___t16[1](___t17), ___bfa4), ___bfb2)
    return (___bpure1, ___t7, ___bap0, ___t9, ___t15)

def ___t21(___bfn5, ___bopt0):
    if ___bopt0[0] == 1:
        return (1, ___bfn5(___bopt0[1]))
    else:
        return (0,)
def ___t22(___bas0, ___bbs0):
    ___a0 = (___bas0, ___bbs0)
    ___a2 = ___a0[1]
    ___a1 = ___a0[0]
    if (___a1[0] == 1) and (___a2[0] == 1):
        return (1, (___a1[1], ___a2[1]))
    else:
        return (0,)
applicative_Option = applicative_from_pure_map_product(lambda ___t20: (1, ___t20), ___t21, ___t22)

def trav_l(___bapp0, ___bfn6, ___blst0):
    ___bpure2 = ___bapp0[0]
    ___bmap1 = ___bapp0[1]
    ___bproduct1 = ___bapp0[4]
    ___t23 = (___bfn6, ___bmap1, ___bproduct1)
    def ___bloop0(___blst1, ___bftail0):
        ___a6 = ___blst1
        ___a8 = ___bftail0
        ___a3 = 1
        ___t25 = ___a3 == 1
        while ___t25:
            if ___a6[0] == 0:
                ___a3 = 0
                ___a4 = ___a8
            else:
                ___bh0 = ___a6[1]
                ___bt0 = ___a6[2]
                ___a5 = ___bt0
                def ___t24(___ba6):
                    return (1, ___ba6[0], ___ba6[1])
                ___a7 = ___t23[1](___t24, ___t23[2](___t23[0](___bh0), ___a8))
                ___a6 = ___a5
                ___a8 = ___a7
            ___t25 = ___a3 == 1
        return ___a4
    return ___bmap1(___iPredef0.reverse, ___bloop0(___blst0, ___bpure2((0,))))

traverse_List = trav_l

eq_opt_list_int = ___iOption1.eq_Option(___iList2.eq_List(lambda ___t26, ___t27: ___t26 == ___t27))

def ___t30(___bx1):
    return (0,) if (___bx1 == 3) == 1 else (1, ___bx1)
def ___t31(___bx2):
    return (0,) if (___bx2 == 3) == 1 else (1, ___bx2)
test = (1,
    "applicative/traverse tests",
    (1,
        (0,
            eq_opt_list_int(trav_l(applicative_Option,
                    lambda ___bx0: (1, ___bx0 + ___bx0),
                    (1, 1, (1, 2, (1, 3, (0,))))),
                (1, (1, 2, (1, 4, (1, 6, (0,)))))),
            "double"),
        (1,
            (0,
                eq_opt_list_int(trav_l(applicative_Option,
                        lambda ___ba7: (0,),
                        (1, 1, (1, 2, (1, 3, (0,))))),
                    (0,)),
                "all to None"),
            (1,
                (0,
                    eq_opt_list_int(trav_l(applicative_Option,
                            ___t30,
                            (1, 1, (1, 2, (1, 3, (0,))))),
                        (0,)),
                    "3 to None"),
                (1,
                    (0,
                        eq_opt_list_int(trav_l(applicative_Option, ___t31, (0,)), (1, (0,))),
                        "empty to Some"),
                    (0,))))))

class BosatsuTests(___iunittest3.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t32 = value[2]
                while ___t32[0] != 0:
                    test_loop(___t32[1])
                    ___t32 = ___t32[2]
        test_loop(test)