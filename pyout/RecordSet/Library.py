import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

def list_of_rows(___bb0):
    ___bfields0 = ___bb0[0]
    ___brows0 = ___bb0[1]
    ___bgetters0 = ___bb0[2]
    ___btraverse0 = ___bb0[3]
    ___brecord__to__list0 = ___bb0[4]
    def ___t0(___brow0):
        ___t2 = (___bfields0, ___brow0)
        def ___t1(___ba0):
            return ___ba0[0](___t2[0])[1](___ba0[1](___t2[1]))
        return ___brecord__to__list0(___btraverse0(___bgetters0)(___t1))
    return ___iPredef0.map_List(___brows0, ___t0)

def restructure(___ba1, ___bf0):
    ___bfields1 = ___ba1[0]
    ___brows1 = ___ba1[1]
    ___bgetters1 = ___ba1[2]
    ___a1 = ___bf0(___bgetters1)
    return (___a1[0](___bfields1),
        ___iPredef0.map_List(___brows1, ___a1[1]),
        ___a1[2],
        ___a1[3],
        ___a1[4])

new_record_set = ((), (0,), (), lambda ___ba2: lambda ___ba3: (), lambda ___ba4: (0,))

ps_end = (lambda ___ba5: (),
    lambda ___ba6: (),
    (),
    lambda ___ba7: lambda ___ba8: (),
    lambda ___ba9: (0,))

def ps(___bc0, ___bd0):
    ___bfF0 = ___bc0[0]
    ___bfV0 = ___bc0[1]
    ___breshaper1F0 = ___bd0[0]
    ___breshaper1V0 = ___bd0[1]
    ___bgetters10 = ___bd0[2]
    ___btraverse10 = ___bd0[3]
    ___brecord__to__list10 = ___bd0[4]
    def ___t11(___bb1):
        ___bf10 = ___bb1[0]
        ___bv10 = ___bb1[1]
        def ___t12(___ba10):
            return ___bf10(___ba10[1])
        def ___t13(___ba11):
            return ___bv10(___ba11[1])
        return (___t12, ___t13)
    ___bgetters20 = ___btraverse10(___bgetters10)(___t11)
    def ___t18(___ba14):
        ___bx0 = ___ba14[0]
        ___bsh22 = ___ba14[1]
        ___t20 = (___bx0, ___btraverse10, ___bsh22)
        def ___t19(___bg0):
            return (___bg0(___t20[0]), ___t20[1](___t20[2])(___bg0))
        return ___t19
    def ___t21(___ba15):
        return ___iPredef0.concat((1, ___ba15[0], (0,)), ___brecord__to__list10(___ba15[1]))
    return (lambda ___bsh10: (___bfF0(___bsh10), ___breshaper1F0(___bsh10)),
        lambda ___bsh11: (___bfV0(___bsh11), ___breshaper1V0(___bsh11)),
        ((lambda ___ba12: ___ba12[0], lambda ___ba13: ___ba13[0]), ___bgetters20),
        ___t18,
        ___t21)

def equal_List(___bis__equal0, ___bl10, ___bl20):
    ___a7 = ___bis__equal0
    ___a9 = ___bl10
    ___a11 = ___bl20
    ___a4 = 1
    ___t22 = ___a4 == 1
    while ___t22:
        if ___a9[0] == 0:
            ___a4 = 0
            ___a5 = ___a11[0] == 0
        else:
            ___bh10 = ___a9[1]
            ___br10 = ___a9[2]
            if ___a11[0] == 0:
                ___a4 = 0
                ___a5 = 0
            else:
                ___bh20 = ___a11[1]
                ___br20 = ___a11[2]
                ___a3 = ___a7(___bh10, ___bh20)
                if ___a3 == 1:
                    ___a8 = ___br10
                    ___a10 = ___br20
                    ___a9 = ___a8
                    ___a11 = ___a10
                else:
                    ___a4 = 0
                    ___a5 = 0
        ___t22 = ___a4 == 1
    return ___a5

def equal_RowEntry(___bre10, ___bre20):
    ___a20 = (___bre10, ___bre20)
    ___a23 = ___a20[1]
    ___a21 = ___a20[0]
    if ___a21[0] == 0:
        ___a22 = ___a21[1]
        if ___a23[0] == 0:
            ___a24 = ___a23[1]
            ___bx10 = ___a22
            ___bx20 = ___a24
            ___a12 = (___bx10, ___bx20)
            ___a14 = ___a12[1]
            ___a13 = ___a12[0]
            if (___a13 == 1) and (___a14 == 0):
                ___a17 = 2
            else:
                ___a16 = ___a12[1]
                ___a15 = ___a12[0]
                ___a17 = not ((___a15 == 0) and (___a16 == 1))
            return ___a17 == 1
        else:
            ___a27 = ___a20[1]
            ___a25 = ___a20[0]
            if ___a25[0] == 1:
                ___a26 = ___a25[1]
                if ___a27[0] == 1:
                    ___a28 = ___a27[1]
                    return (0 if ___a26 < ___a28 else 1 if ___a26 == ___a28 else 2) == 1
                else:
                    ___a31 = ___a20[1]
                    ___a29 = ___a20[0]
                    if ___a29[0] == 2:
                        ___a30 = ___a29[1]
                        if ___a31[0] == 2:
                            ___a32 = ___a31[1]
                            return (0 if ___a30 < ___a32 else 1 if ___a30 == ___a32 else 2) == 1
                        else:
                            return 0
                    else:
                        return 0
            else:
                ___a31 = ___a20[1]
                ___a29 = ___a20[0]
                if ___a29[0] == 2:
                    ___a30 = ___a29[1]
                    if ___a31[0] == 2:
                        ___a32 = ___a31[1]
                        return (0 if ___a30 < ___a32 else 1 if ___a30 == ___a32 else 2) == 1
                    else:
                        return 0
                else:
                    return 0
    else:
        ___a27 = ___a20[1]
        ___a25 = ___a20[0]
        if ___a25[0] == 1:
            ___a26 = ___a25[1]
            if ___a27[0] == 1:
                ___a28 = ___a27[1]
                return (0 if ___a26 < ___a28 else 1 if ___a26 == ___a28 else 2) == 1
            else:
                ___a31 = ___a20[1]
                ___a29 = ___a20[0]
                if ___a29[0] == 2:
                    ___a30 = ___a29[1]
                    if ___a31[0] == 2:
                        ___a32 = ___a31[1]
                        return (0 if ___a30 < ___a32 else 1 if ___a30 == ___a32 else 2) == 1
                    else:
                        return 0
                else:
                    return 0
        else:
            ___a31 = ___a20[1]
            ___a29 = ___a20[0]
            if ___a29[0] == 2:
                ___a30 = ___a29[1]
                if ___a31[0] == 2:
                    ___a32 = ___a31[1]
                    return (0 if ___a30 < ___a32 else 1 if ___a30 == ___a32 else 2) == 1
                else:
                    return 0
            else:
                return 0

def equal_rows(___ba16, ___bb2):
    return equal_List(equal_RowEntry, ___ba16, ___bb2)

def ___t23(___ba17):
    return ps((lambda ___ba18: ("String field", lambda ___t24: (2, ___t24)), lambda ___ba19: ""),
        ps((lambda ___ba20: ("Int field", lambda ___t27: (1, ___t27)), lambda ___ba21: 0),
            ps((lambda ___ba22: ("Bool field", lambda ___t30: (0, ___t30)), lambda ___ba23: 1),
                ps_end)))
rs_empty = restructure(new_record_set, ___t23)

___bfields2 = rs_empty[0]
___brows2 = rs_empty[1]
___bgetters2 = rs_empty[2]
___btraverse2 = rs_empty[3]
___brecord__to__list2 = rs_empty[4]
rs = (___bfields2,
    ___iPredef0.concat(___brows2, (1, ("a", (1, (0, ()))), (0,))),
    ___bgetters2,
    ___btraverse2,
    ___brecord__to__list2)

def ___t33(___bd1):
    ___a34 = ___bd1[1]
    ___a35 = ___a34[1]
    ___ba24 = ___bd1[0]
    ___bb3 = ___a34[0]
    ___bc1 = ___a35[0]
    ___brf3 = ("Plus 2", lambda ___t34: (1, ___t34))
    def ___t36(___bsh0):
        return ___bb3[1](___bsh0) + 2
    return ps(___bc1, ps(___bb3, ps(___ba24, ps((lambda ___ba25: ___brf3, ___t36), ps_end))))
rs0 = restructure(rs, ___t33)

tests = (1,
    "reordering",
    (1,
        (0,
            equal_List(equal_rows,
                list_of_rows(rs0),
                (1, (1, (0, 0), (1, (1, 1), (1, (2, "a"), (1, (1, 3), (0,))))), (0,))),
            "swap"),
        (0,)))

class BosatsuTests(___iunittest1.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t37 = value[2]
                while ___t37[0] != 0:
                    test_loop(___t37[1])
                    ___t37 = ___t37[2]
        test_loop(tests)