import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

def quick_sort0(___bcmp0, ___bleft0, ___bright0):
    if ___bleft0[0] == 0:
        return ___bright0
    else:
        ___btail0 = ___bleft0[2]
        if ___bright0[0] == 0:
            return ___bright0
        else:
            if ___bright0[0] == 1:
                ___a4 = ___bright0[2]
                ___t4 = ___a4[0] == 0
            else:
                ___t4 = False
            if ___t4:
                return ___bright0
            else:
                ___bpivot0 = ___bright0[1]
                ___brs0 = ___bright0[2]
                ___t1 = (___bcmp0, ___bpivot0)
                def ___t0(___bx0):
                    ___a0 = ___t1[0](___bx0, ___t1[1])
                    return (1, ___bx0, (0,)) if (1 if ___a0 == 0 else ___a0 == 1) == 1 else (0,)
                ___bsmaller0 = ___iPredef0.flat_map_List(___brs0, ___t0)
                ___t3 = (___bcmp0, ___bpivot0)
                def ___t2(___bx1):
                    return (1, ___bx1, (0,)) if (___t3[0](___bx1, ___t3[1]) == 2) == 1 else (0,)
                return ___iPredef0.concat(quick_sort0(___bcmp0, ___btail0, ___bsmaller0),
                    (1,
                        ___bpivot0,
                        quick_sort0(___bcmp0,
                            ___btail0,
                            ___iPredef0.flat_map_List(___brs0, ___t2))))

def quick_sort(___blist0, ___bcmp1):
    return quick_sort0(___bcmp1, ___blist0, ___blist0)

def eq_List(___bas0, ___bbs0):
    ___a9 = ___bas0
    ___a11 = ___bbs0
    ___a6 = 1
    ___t5 = ___a6 == 1
    while ___t5:
        if ___a9[0] == 1:
            ___ba0 = ___a9[1]
            ___batail0 = ___a9[2]
            if ___a11[0] == 1:
                ___bb0 = ___a11[1]
                ___bbtail0 = ___a11[2]
                ___a5 = ___ba0 == ___bb0
                if ___a5 == 1:
                    ___a8 = ___batail0
                    ___a10 = ___bbtail0
                    ___a9 = ___a8
                    ___a11 = ___a10
                else:
                    ___a6 = 0
                    ___a7 = 0
            else:
                ___a6 = 0
                ___a7 = 0
        else:
            ___a6 = 0
            ___a7 = ___a11[0] == 0
        ___t5 = ___a6 == 1
    return ___a7

___blist1 = (1, 3, (1, 2, (1, 1, (0,))))
___a12 = quick_sort0(lambda ___t6, ___t7: 0 if ___t6 < ___t7 else 1 if ___t6 == ___t7 else 2,
    ___blist1,
    ___blist1)
if ___a12[0] == 1:
    ___a13 = ___a12[1]
    ___a14 = ___a12[2]
    if ___a13 == 1:
        if ___a14[0] == 1:
            ___a15 = ___a14[1]
            ___a16 = ___a14[2]
            if ___a15 == 2:
                if ___a16[0] == 1:
                    ___a17 = ___a16[1]
                    ___a18 = ___a16[2]
                    ___t8 = (___a17 == 3) and (___a18[0] == 0)
                else:
                    ___t8 = False
            else:
                ___t8 = False
        else:
            ___t8 = False
    else:
        ___t8 = False
else:
    ___t8 = False
___blist2 = (1, 2, (1, 3, (1, 1, (0,))))
___a19 = quick_sort0(lambda ___t9, ___t10: 0 if ___t9 < ___t10 else 1 if ___t9 == ___t10 else 2,
    ___blist2,
    ___blist2)
if ___a19[0] == 1:
    ___a20 = ___a19[1]
    ___a21 = ___a19[2]
    if ___a20 == 1:
        if ___a21[0] == 1:
            ___a22 = ___a21[1]
            ___a23 = ___a21[2]
            if ___a22 == 2:
                if ___a23[0] == 1:
                    ___a24 = ___a23[1]
                    ___a25 = ___a23[2]
                    ___t11 = (___a24 == 3) and (___a25[0] == 0)
                else:
                    ___t11 = False
            else:
                ___t11 = False
        else:
            ___t11 = False
    else:
        ___t11 = False
else:
    ___t11 = False
___blist3 = (1, 2, (1, 2, (1, 1, (0,))))
___a26 = quick_sort0(lambda ___t12, ___t13: 0 if ___t12 < ___t13 else 1 if ___t12 == ___t13 else 2,
    ___blist3,
    ___blist3)
if ___a26[0] == 1:
    ___a27 = ___a26[1]
    ___a28 = ___a26[2]
    if ___a27 == 1:
        if ___a28[0] == 1:
            ___a29 = ___a28[1]
            ___a30 = ___a28[2]
            if ___a29 == 2:
                if ___a30[0] == 1:
                    ___a31 = ___a30[1]
                    ___a32 = ___a30[2]
                    ___t14 = (___a31 == 2) and (___a32[0] == 0)
                else:
                    ___t14 = False
            else:
                ___t14 = False
        else:
            ___t14 = False
    else:
        ___t14 = False
else:
    ___t14 = False
___blist4 = (1, 2, (1, 2, (1, 1, (0,))))
___a33 = quick_sort0(lambda ___t15, ___t16: 0 if ___t15 < ___t16 else 1 if ___t15 == ___t16 else 2,
    ___blist4,
    ___blist4)
if ___a33[0] == 1:
    ___a34 = ___a33[1]
    ___a35 = ___a33[2]
    if ___a34 == 1:
        if ___a35[0] == 1:
            ___a36 = ___a35[1]
            ___a37 = ___a35[2]
            if ___a36 == 2:
                if ___a37[0] == 1:
                    ___a38 = ___a37[1]
                    ___a39 = ___a37[2]
                    ___t17 = (___a38 == 2) and (___a39[0] == 0)
                else:
                    ___t17 = False
            else:
                ___t17 = False
        else:
            ___t17 = False
    else:
        ___t17 = False
else:
    ___t17 = False
test = (1,
    "quicksort",
    (1,
        (0, ___t8, "3, 2, 1"),
        (1,
            (0, ___t11, "2, 3, 1"),
            (1,
                (0, ___t14, "2, 2, 1"),
                (1,
                    (0, ___t17, "2, 2, 1"),
                    (1,
                        (0,
                            eq_List(quick_sort(___iPredef0.reverse(___iPredef0.range(100)),
                                    lambda ___t18, ___t19: 0 if ___t18 < ___t19 else 1 if ___t18 == ___t19 else 2),
                                ___iPredef0.range(100)),
                            "range(100).reverse"),
                        (0,)))))))

class BosatsuTests(___iunittest1.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t20 = value[2]
                while ___t20[0] != 0:
                    test_loop(___t20[1])
                    ___t20 = ___t20[2]
        test_loop(test)