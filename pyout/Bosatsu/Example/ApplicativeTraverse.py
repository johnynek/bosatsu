import Bosatsu.List as ___iList2
import Bosatsu.Option as ___iOption1
import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest3

def applicative_from_pure_map_product(___bpure0, ___bmap0, ___bproduct0):
    def ___t0(___bfn0, ___bfa0):
        def ___t1(___bb0):
            return ___bb0[0](___bb0[1])
        return ___bmap0(___t1, ___bproduct0(___bfn0, ___bfa0))
    def ___t2(___bfa1, ___bfb0, ___bfn2):
        ___t4 = (___bfn2,)
        def ___t3(___bc0):
            return ___t4[0](___bc0[0], ___bc0[1])
        return ___bmap0(___t3, ___bproduct0(___bfa1, ___bfb0))
    return (___bpure0, ___bmap0, ___t0, ___t2, ___bproduct0)

def applicative_from_pure_ap(___bpure1, ___bap0):
    def ___t6(___bfa3, ___bfb1, ___bfn4):
        ___t8 = (___bfn4,)
        def ___t7(___ba2):
            ___t10 = (___t8[0], ___ba2)
            def ___t9(___bb2):
                return ___t10[0](___t10[1], ___bb2)
            return ___t9
        return ___bap0(___bap0(___bpure1(___t7), ___bfa3), ___bfb1)
    return (___bpure1,
        lambda ___bfn3, ___bfa2: ___bap0(___bpure1(___bfn3), ___bfa2),
        ___bap0,
        ___t6,
        lambda ___bfa4, ___bfb2: ___bap0(___bap0(___bpure1(lambda ___ba3: lambda ___bb3: (___ba3,
                        ___bb3)),
                ___bfa4),
            ___bfb2))

def ___t15(___bfn5, ___bopt0):
    if ___bopt0[0] == 1:
        return (1, ___bfn5(___bopt0[1]))
    else:
        return (0,)
def ___t16(___bas0, ___bbs0):
    ___a0 = (___bas0, ___bbs0)
    ___a2 = ___a0[1]
    ___a1 = ___a0[0]
    if (___a1[0] == 1) and (___a2[0] == 1):
        return (1, (___a1[1], ___a2[1]))
    else:
        return (0,)
applicative_Option = applicative_from_pure_map_product(lambda ___t14: (1, ___t14), ___t15, ___t16)

def trav_l(___bapp0, ___bfn6, ___blst0):
    ___bpure2 = ___bapp0[0]
    ___bmap1 = ___bapp0[1]
    ___bproduct1 = ___bapp0[4]
    def ___bloop0(___blst1, ___bftail0):
        ___a6 = ___blst1
        ___a8 = ___bftail0
        ___a3 = 1
        ___t18 = ___a3 == 1
        while ___t18:
            if ___a6[0] == 0:
                ___a3 = 0
                ___a4 = ___a8
            else:
                ___bh0 = ___a6[1]
                ___bt0 = ___a6[2]
                ___a5 = ___bt0
                def ___t17(___ba6):
                    return (1, ___ba6[0], ___ba6[1])
                ___a7 = ___bmap1(___t17, ___bproduct1(___bfn6(___bh0), ___a8))
                ___a6 = ___a5
                ___a8 = ___a7
            ___t18 = ___a3 == 1
        return ___a4
    return ___bmap1(___iPredef0.reverse, ___bloop0(___blst0, ___bpure2((0,))))

traverse_List = trav_l

eq_opt_list_int = ___iOption1.eq_Option(___iList2.eq_List(lambda ___t19, ___t20: ___t19 == ___t20))

def ___t23(___bx1):
    return (0,) if (___bx1 == 3) == 1 else (1, ___bx1)
def ___t24(___bx2):
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
                            ___t23,
                            (1, 1, (1, 2, (1, 3, (0,))))),
                        (0,)),
                    "3 to None"),
                (1,
                    (0,
                        eq_opt_list_int(trav_l(applicative_Option, ___t24, (0,)), (1, (0,))),
                        "empty to Some"),
                    (0,))))))

class BosatsuTests(___iunittest3.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t25 = value[2]
                while ___t25[0] != 0:
                    test_loop(___t25[1])
                    ___t25 = ___t25[2]
        test_loop(test)