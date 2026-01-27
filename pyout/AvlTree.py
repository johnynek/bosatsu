import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

def rotation(___bleft0, ___bright0, ___bmax__diff0):
    ___a0 = 0 if (___bleft0 - ___bright0) < ___bmax__diff0 else 1 if (___bleft0 - ___bright0) == ___bmax__diff0 else 2
    ___a3 = ___a0 == 2
    if ___a3 == 1:
        return 2
    else:
        return ((0 if (___bright0 - ___bleft0) < ___bmax__diff0 else 1 if (___bright0 - ___bleft0) == ___bmax__diff0 else 2) == 2) != 1

def max(___bi0, ___bj0):
    return ___bj0 if (0 if ___bi0 < ___bj0 else 1 if ___bi0 == ___bj0 else 2) == 0 else ___bi0

def branch(___bsz0, ___bitem0, ___bleft1, ___bright1):
    ___bi1 = 0 if ___bleft1[0] == 0 else ___bleft1[2]
    ___bj1 = 0 if ___bright1[0] == 0 else ___bright1[2]
    return (1,
        ___bsz0,
        (___bj1 if (0 if ___bi1 < ___bj1 else 1 if ___bi1 == ___bj1 else 2) == 0 else ___bi1) + 1,
        ___bitem0,
        ___bleft1,
        ___bright1)

def branch_s(___bitem1, ___bleft2, ___bright2):
    return branch((0 if ___bleft2[0] == 0 else ___bleft2[1]) + (0 if ___bright2[0] == 0 else ___bright2[1]) + 1,
        ___bitem1,
        ___bleft2,
        ___bright2)

def balance(___bt0):
    if ___bt0[0] == 0:
        return (0,)
    else:
        ___btop__item0 = ___bt0[3]
        ___bleft3 = ___bt0[4]
        ___bright3 = ___bt0[5]
        ___a8 = rotation(0 if ___bleft3[0] == 0 else ___bleft3[2],
            0 if ___bright3[0] == 0 else ___bright3[2],
            1)
        if ___a8 == 1:
            return ___bt0
        elif ___a8 == 2:
            if ___bleft3[0] == 0:
                return ___bt0
            else:
                ___binner__item0 = ___bleft3[3]
                ___bleft4 = ___bleft3[4]
                ___bleft__right0 = ___bleft4[5]
                ___a6 = rotation(0 if ___bleft4[0] == 0 else ___bleft4[2],
                    0 if ___bleft__right0[0] == 0 else ___bleft__right0[2],
                    0)
                if ___a6 == 2:
                    return branch_s(___binner__item0,
                        ___bleft4,
                        branch_s(___btop__item0, ___bleft__right0, ___bright3))
                elif ___a6 == 1:
                    return branch_s(___binner__item0,
                        ___bleft4,
                        branch_s(___btop__item0, ___bleft__right0, ___bright3))
                elif ___bleft__right0[0] == 0:
                    print("unreachable", ___bt0)
                    return ___bt0
                else:
                    return branch_s(___bleft__right0[3],
                        branch_s(___binner__item0, ___bleft4, ___bleft__right0[4]),
                        branch_s(___btop__item0, ___bleft__right0[5], ___bright3))
        elif ___bright3[0] == 0:
            return ___bt0
        else:
            ___binner__item1 = ___bright3[3]
            ___bright__left0 = ___bright3[4]
            ___bright__right0 = ___bright3[5]
            ___a7 = rotation(0 if ___bright__left0[0] == 0 else ___bright__left0[2],
                0 if ___bright__right0[0] == 0 else ___bright__right0[2],
                0)
            if ___a7 == 0:
                return branch_s(___binner__item1,
                    branch_s(___btop__item0, ___bleft3, ___bright__left0),
                    ___bright__right0)
            elif ___a7 == 1:
                return branch_s(___binner__item1,
                    branch_s(___btop__item0, ___bleft3, ___bright__left0),
                    ___bright__right0)
            elif ___bright__left0[0] == 0:
                print("unreachable", ___bt0)
                return ___bt0
            else:
                return branch_s(___bright__left0[3],
                    branch_s(___btop__item0, ___bleft3, ___bright__left0[5]),
                    branch_s(___binner__item1, ___bright__left0[4], ___bright__right0))

def add_item(___bord0, ___btree0, ___bitem2):
    ___bfn0 = ___bord0
    ___t0 = (___bfn0, ___bitem2)
    def ___bloop0(___btree1):
        if ___btree1[0] == 0:
            return (1, 1, 1, ___t0[1], (0,), (0,))
        else:
            ___bs0 = ___btree1[1]
            ___bh0 = ___btree1[2]
            ___bitem00 = ___btree1[3]
            ___bleft5 = ___btree1[4]
            ___bright4 = ___btree1[5]
            ___a9 = ___t0[0](___t0[1], ___bitem00)
            return (1,
                ___bs0,
                ___bh0,
                ___t0[1],
                ___bleft5,
                ___bright4) if ___a9 == 1 else balance(branch(___bs0 + 1,
                    ___bitem00,
                    ___bloop0(___bleft5),
                    ___bright4)) if ___a9 == 0 else balance(branch(___bs0 + 1,
                    ___bitem00,
                    ___bleft5,
                    ___bloop0(___bright4)))
    return ___bloop0(___btree0)

def contains(___bord1, ___btree2, ___bitem3):
    ___bfn1 = ___bord1
    ___t1 = (___bfn1, ___bitem3)
    def ___bloop1(___btree3):
        ___a14 = ___btree3
        ___a11 = 1
        ___t2 = ___a11 == 1
        while ___t2:
            if ___a14[0] == 0:
                ___a11 = 0
                ___a12 = (0,)
            else:
                ___bkey0 = ___a14[3]
                ___bleft6 = ___a14[4]
                ___bright5 = ___a14[5]
                ___a10 = ___t1[0](___t1[1], ___bkey0)
                if ___a10 == 1:
                    ___a11 = 0
                    ___a12 = (1, ___bkey0)
                elif ___a10 == 0:
                    ___a13 = ___bleft6
                    ___a14 = ___a13
                else:
                    ___a13 = ___bright5
                    ___a14 = ___a13
            ___t2 = ___a11 == 1
        return ___a12
    return ___bloop1(___btree2)

def min(___btree4):
    ___a19 = ___btree4
    ___a16 = 1
    ___t4 = ___a16 == 1
    while ___t4:
        if ___a19[0] == 0:
            ___a16 = 0
            ___a17 = (0,)
        else:
            if ___a19[0] == 1:
                ___a15 = ___a19[4]
                ___t3 = ___a15[0] == 0
            else:
                ___t3 = False
            if ___t3:
                ___a16 = 0
                ___bkey1 = ___a19[3]
                ___a17 = (1, ___bkey1)
            else:
                ___bleft7 = ___a19[4]
                ___a18 = ___bleft7
                ___a19 = ___a18
        ___t4 = ___a16 == 1
    return ___a17

def remove_item(___bord2, ___btree5, ___bitem4):
    ___bfn2 = ___bord2
    ___t5 = (___bfn2, ___bitem4)
    def ___bloop2(___btree6):
        if ___btree6[0] == 0:
            return (0,)
        else:
            ___bsize0 = ___btree6[1]
            ___bkey2 = ___btree6[3]
            ___bleft8 = ___btree6[4]
            ___bright6 = ___btree6[5]
            ___a20 = ___t5[0](___t5[1], ___bkey2)
            return (___bleft8 if ___bright6[0] == 0 else balance(branch(___bsize0 + -1,
                    ___bkey2,
                    ___bleft8,
                    ___bloop2(___bright6)))) if ___a20 == 1 else balance(branch(___bsize0 + -1,
                    ___bkey2,
                    ___bloop2(___bleft8),
                    ___bright6)) if ___a20 == 0 else balance(branch(___bsize0 + -1,
                    ___bkey2,
                    ___bleft8,
                    ___bloop2(___bright6)))
    return ___bloop2(___btree5)

def fold_left_Tree(___bt1, ___bleft__v0, ___bfn3):
    if ___bt1[0] == 0:
        return ___bleft__v0
    else:
        return fold_left_Tree(___bt1[5],
            ___bfn3(fold_left_Tree(___bt1[4], ___bleft__v0, ___bfn3), ___bt1[3]),
            ___bfn3)

def fold_right_Tree(___bt2, ___bright__v0, ___bfn4):
    if ___bt2[0] == 0:
        return ___bright__v0
    else:
        return fold_right_Tree(___bt2[4],
            ___bfn4(___bt2[3], fold_right_Tree(___bt2[5], ___bright__v0, ___bfn4)),
            ___bfn4)

def module(___bord3):
    ___t7 = (___bord3,)
    def ___t6(___bt3, ___ba0):
        return add_item(___t7[0], ___bt3, ___ba0)
    ___t9 = (___bord3,)
    def ___t8(___ba1):
        return add_item(___t9[0], (0,), ___ba1)
    ___t11 = (___bord3,)
    def ___t10(___bt4, ___ba2):
        return contains(___t11[0], ___bt4, ___ba2)
    ___t13 = (___bord3,)
    def ___t12(___bt5, ___ba3):
        return remove_item(___t13[0], ___bt5, ___ba3)
    return (___bord3, (0,), ___t6, ___t8, ___t10, ___t12, fold_left_Tree, fold_right_Tree)

_a = module(lambda ___t14, ___t15: 0 if ___t14 < ___t15 else 1 if ___t14 == ___t15 else 2)

___a21 = _a[4](_a[2]((0,), 2), 2)
___a22 = _a[4](_a[2](_a[3](2), 2), 2)
___a23 = _a[4](_a[2](_a[3](3), 42), 42)
___a24 = _a[4](_a[3](2), 3)
___a25 = ___a24[0] == 1
___a26 = _a[4](_a[5](_a[3](2), 2), 2)
___a27 = ___a26[0] == 1
___a28 = _a[4](_a[5](_a[5](_a[3](2), 2), 2), 2)
___a29 = ___a28[0] == 1
contains_test = (1,
    "contains tests",
    (1,
        (0, ___a21[0] == 1, "Empty.add_law 2"),
        (1,
            (0, ___a22[0] == 1, "single(2) + 2 add_law"),
            (1,
                (0, ___a23[0] == 1, "single(3) add_law 42"),
                (1,
                    (0, ___a25 != 1, "single(2) ! contains 3"),
                    (1,
                        (0, ___a27 != 1, "Empty + 2 - 2, !contains(2)"),
                        (1, (0, ___a29 != 1, "Empty + 2 - 2, !contains(2)"), (0,))))))))

def add_increases_size(___bt6, ___bi2, ___bmsg0):
    ___a30 = _a[2](___bt6, ___bi2)
    return (0,
        ((0 if ___a30[0] == 0 else ___a30[1]) - (0 if ___bt6[0] == 0 else ___bt6[1])) == 1,
        ___bmsg0)

def rem_decreases_size(___bt7, ___bi3, ___bmsg1):
    ___a31 = _a[5](___bt7, ___bi3)
    return (0,
        ((0 if ___bt7[0] == 0 else ___bt7[1]) - (0 if ___a31[0] == 0 else ___a31[1])) == 1,
        ___bmsg1)

___a32 = _a[3](1)
___a33 = _a[2](_a[3](1), 1)
size_tests = (1,
    "size tests",
    (1,
        add_increases_size((0,), 1, "Empty.add(1)"),
        (1,
            add_increases_size(_a[3](1), 2, "single(1).add(2)"),
            (1,
                (0,
                    (0 if ___a32[0] == 0 else ___a32[1]) == (0 if ___a33[0] == 0 else ___a33[1]),
                    "single(1) + 1 has same size"),
                (1,
                    rem_decreases_size(_a[3](1), 1, "single(1) - 1"),
                    (1, rem_decreases_size(_a[2](_a[3](2), 3), 2, "single(2) + 3 - 2"), (0,)))))))

def log2(___bi4):
    ___t17 = 0 < ___bi4
    ___t18 = 0
    ___t19 = ___bi4
    ___t20 = 0
    while ___t17:
        ___t21 = ___t19 // 2
        ___t20 = ___t20 + 1
        ___t17 = (0 < ___t21) and (___t21 < ___t19)
        ___t19 = ___t21
    return ___t20

def ___t22(___bn1):
    ___bt8 = ___iPredef0.foldl_List(___iPredef0.range(___bn1), (0,), _a[2])
    ___bh1 = 0 if ___bt8[0] == 0 else ___bt8[2]
    ___bn10 = 0 if ___bt8[0] == 0 else ___bt8[1]
    ___a34 = 0 if (___bh1 + ___bh1) < (log2(___bn10 + 2) + log2(___bn10 + 2) + log2(___bn10 + 2)) else 1 if (___bh1 + ___bh1) == (log2(___bn10 + 2) + log2(___bn10 + 2) + log2(___bn10 + 2)) else 2
    ___t23 = []
    ___t24 = (1, "size_law for range(", (1, ___bn10.__str__(), (1, ")", (0,))))
    while ___t24[0] != 0:
        ___t23.append(___t24[1])
        ___t24 = ___t24[2]
    return (0, ___a34 == 0, "".join(___t23))
height_tests = (1, "height_tests", ___iPredef0.map_List(___iPredef0.range(30), ___t22))

fold_left_tests = (1,
    "fold_left_tests",
    (1,
        (0,
            fold_left_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                0,
                lambda ___bi5, ___ba4: ___bi5 + 1) == 100,
            "sum 100"),
        (1,
            (0,
                fold_left_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                    0,
                    max) == 99,
                "max 100"),
            (1,
                (0,
                    fold_left_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                        -1,
                        lambda ___bacc0, ___bi6: ___bi6 if ___bacc0 == -1 else ___bacc0) == 0,
                    "first is 0"),
                (0,)))))

fold_right_tests = (1,
    "fold_right_tests",
    (1,
        (0,
            fold_right_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                0,
                lambda ___ba5, ___bi7: ___bi7 + 1) == 100,
            "sum 100"),
        (1,
            (0,
                fold_right_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                    0,
                    max) == 99,
                "max 100"),
            (1,
                (0,
                    fold_right_Tree(___iPredef0.foldl_List(___iPredef0.range(100), (0,), _a[2]),
                        -1,
                        lambda ___bi8, ___bacc1: ___bi8 if ___bacc1 == -1 else ___bacc1) == 99,
                    "last is 99"),
                (0,)))))

tests = (1,
    "AvlTree tests",
    (1,
        contains_test,
        (1, size_tests, (1, height_tests, (1, fold_left_tests, (1, fold_right_tests, (0,)))))))

class BosatsuTests(___iunittest1.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t29 = value[2]
                while ___t29[0] != 0:
                    test_loop(___t29[1])
                    ___t29 = ___t29[2]
        test_loop(tests)