import Bosatsu.List as ___iList1
import Bosatsu.Nat as ___iNat3
import Bosatsu.Predef as ___iPredef0
import Bosatsu.Properties as ___iProperties4
import Bosatsu.Rand as ___iRand2
import unittest as ___iunittest5

empty_Queue = ((0,), (0,))

def from_List(___blist0):
    return (___blist0, (0,))

def push(___ba0, ___bitem0):
    return (___ba0[0], (1, ___bitem0, ___ba0[1]))

def unpush(___bqueue0):
    ___a1 = ___bqueue0[0]
    if ___a1[0] == 1:
        return (1, (___a1[1], (___a1[2], ___bqueue0[1])))
    else:
        ___bb2 = ___bqueue0[1]
        ___a0 = ___iPredef0.reverse(___bb2)
        if ___a0[0] == 0:
            return (0,)
        else:
            return (1, (___a0[1], (___a0[2], (0,))))

def pop_value(___bqueue1):
    ___a3 = unpush(___bqueue1)
    if ___a3[0] == 1:
        return (1, ___a3[1][0])
    else:
        return (0,)

def pop(___bqueue2):
    ___a5 = unpush(___bqueue2)
    if ___a5[0] == 1:
        return ___a5[1][1]
    else:
        return empty_Queue

def fold_Queue(___ba2, ___binit0, ___bfold__fn0):
    return ___iPredef0.foldl_List(___iPredef0.reverse(___ba2[1]),
        ___iPredef0.foldl_List(___ba2[0], ___binit0, ___bfold__fn0),
        ___bfold__fn0)

def reverse_Queue(___ba3):
    return (___ba3[1], ___ba3[0])

def eq_Queue(___beq__fn0):
    ___t1 = (___beq__fn0,)
    def ___t0(___bleft0, ___bright0):
        ___binit1 = (1, ___bright0)
        ___bf3 = ___bleft0[0]
        ___bb5 = ___bleft0[1]
        ___t3 = (___t1[0],)
        def ___t2(___ba4, ___bal0):
            ___bg0 = ___ba4[0]
            ___bright1 = ___ba4[1]
            if ___bg0 == 1:
                ___a7 = unpush(___bright1)
                if ___a7[0] == 0:
                    return (0, empty_Queue)
                else:
                    ___a8 = ___a7[1]
                    return (___t3[0](___bal0, ___a8[0]), ___a8[1])
            else:
                return (0, empty_Queue)
        ___t5 = (___t1[0],)
        def ___t4(___ba5, ___bal1):
            ___bg1 = ___ba5[0]
            ___bright3 = ___ba5[1]
            if ___bg1 == 1:
                ___a9 = unpush(___bright3)
                if ___a9[0] == 0:
                    return (0, empty_Queue)
                else:
                    ___a10 = ___a9[1]
                    return (___t5[0](___bal1, ___a10[0]), ___a10[1])
            else:
                return (0, empty_Queue)
        ___a11 = ___iPredef0.foldl_List(___iPredef0.reverse(___bb5),
            ___iPredef0.foldl_List(___bf3, ___binit1, ___t2),
            ___t4)
        ___a13 = ___a11[1]
        ___a12 = ___a11[0]
        if ___a12 == 1:
            ___a15 = ___a13[1]
            ___a14 = ___a13[0]
            ___t6 = (___a14[0] == 0) and (___a15[0] == 0)
        else:
            ___t6 = False
        return ___t6
    return ___t0

def to_List(___ba6):
    return ___iPredef0.concat(___ba6[0], ___iPredef0.reverse(___ba6[1]))

eq_qi = eq_Queue(lambda ___t7, ___t8: ___t7 == ___t8)

eq_li = ___iList1.eq_List(lambda ___t9, ___t10: ___t9 == ___t10)

___a16 = push(empty_Queue, 1)
___bf5 = ___a16[0]
___bb7 = ___a16[1]
q12 = (___bf5, (1, 2, ___bb7))

rand_int = ___iRand2.int_range(128)

rand_geo_List_Int = ___iRand2.flat_map_Rand(___iRand2.geometric_Int,
    lambda ___blen0: ___iRand2.sequence_Rand(___iPredef0.replicate_List(rand_int, ___blen0)))

queue_from_list = ___iRand2.map_Rand(rand_geo_List_Int, from_List)

def rand_Queue_depth(___bdepth0):
    if ___bdepth0 == 0:
        return queue_from_list
    else:
        ___a17 = ___bdepth0 - 1
        ___bn0 = ___a17
        ___bsmaller0 = rand_Queue_depth(___bn0)
        ___bpop__rand0 = ___iRand2.map_Rand(___bsmaller0, pop)
        def ___t12(___ba7):
            ___bh2 = ___ba7[0]
            ___bq0 = ___ba7[1]
            return (___bq0[0], (1, ___bh2, ___bq0[1]))
        return ___iRand2.one_of(___bpop__rand0,
            (1,
                ___iRand2.map_Rand(___iRand2.prod_Rand(rand_int, ___bsmaller0), ___t12),
                (1, ___iRand2.map_Rand(___bsmaller0, reverse_Queue), (0,))))

rand_Queue_Int = rand_Queue_depth(___iNat3.to_Nat(50))

def show_List(___blst0, ___bshowa0):
    ___t13 = (___bshowa0,)
    def ___binner0(___blst1):
        if ___blst1[0] == 0:
            return ""
        else:
            if ___blst1[0] == 1:
                ___a18 = ___blst1[2]
                ___t16 = ___a18[0] == 0
            else:
                ___t16 = False
            if ___t16:
                return ___t13[0](___blst1[1])
            else:
                ___ba9 = ___blst1[1]
                ___bt2 = ___blst1[2]
                ___t14 = []
                ___t15 = (1, ___t13[0](___ba9), (1, ", ", (1, ___binner0(___bt2), (0,))))
                while ___t15[0] != 0:
                    ___t14.append(___t15[1])
                    ___t15 = ___t15[2]
                return "".join(___t14)
    ___t17 = []
    ___t18 = (1, "[", (1, ___binner0(___blst0), (1, "]", (0,))))
    while ___t18[0] != 0:
        ___t17.append(___t18[1])
        ___t18 = ___t18[2]
    return "".join(___t17)

def show_Queue(___bq1, ___bshowa1):
    ___bf7 = ___bq1[0]
    ___bb9 = ___bq1[1]
    ___t19 = []
    ___t20 = (1,
        "Queue(",
        (1,
            show_List(___bf7, ___bshowa1),
            (1, ", ", (1, show_List(___bb9, ___bshowa1), (1, ")", (0,))))))
    while ___t20[0] != 0:
        ___t19.append(___t20[1])
        ___t20 = ___t20[2]
    return "".join(___t19)

def ___t21(___bq2):
    ___a19 = unpush(___bq2)
    if ___a19[0] == 1:
        ___a20 = ___a19[1]
        ___ba10 = ___a20[0]
        ___a23 = (1, ___ba10)
    else:
        ___a23 = (0,)
    if ___a23[0] == 0:
        ___bf8 = ___bq2[0]
        ___bb10 = ___bq2[1]
        ___a21 = ___iPredef0.concat(___bf8, ___iPredef0.reverse(___bb10))
        ___t22 = ___a21[0] == 0
    else:
        ___bi0 = ___a23[1]
        ___bf9 = ___bq2[0]
        ___bb11 = ___bq2[1]
        ___a22 = ___iPredef0.concat(___bf9, ___iPredef0.reverse(___bb11))
        if ___a22[0] == 1:
            ___bh3 = ___a22[1]
            ___t22 = ___bh3 == ___bi0
        else:
            ___t22 = 0
    return (0, ___t22, "check head")
def ___t23(___bq3):
    ___a24 = reverse_Queue(___bq3)
    ___bf10 = ___a24[0]
    ___bb12 = ___a24[1]
    ___brev__tl0 = ___iPredef0.concat(___bf10, ___iPredef0.reverse(___bb12))
    ___btl__rev0 = ___iPredef0.reverse(to_List(___bq3))
    ___t27 = []
    ___t28 = (1,
        "rev_tl = ",
        (1,
            show_List(___brev__tl0, lambda ___t24: ___t24.__str__()),
            (1,
                " tl_rev = ",
                (1,
                    show_List(___btl__rev0, lambda ___t25: ___t25.__str__()),
                    (1, ": ", (1, show_Queue(___bq3, lambda ___t26: ___t26.__str__()), (0,)))))))
    while ___t28[0] != 0:
        ___t27.append(___t28[1])
        ___t28 = ___t28[2]
    return (0, eq_li(___brev__tl0, ___btl__rev0), "".join(___t27))
def ___t29(___ba11):
    ___bh4 = ___ba11[0]
    ___bq4 = ___ba11[1]
    ___bf11 = ___bq4[0]
    ___bb13 = ___bq4[1]
    ___a25 = (___bf11, (1, ___bh4, ___bb13))
    return (0,
        eq_li(___iPredef0.concat(___a25[0], ___iPredef0.reverse(___a25[1])),
            ___iPredef0.reverse((1,
                    ___bh4,
                    ___iPredef0.reverse(___iPredef0.concat(___bq4[0],
                            ___iPredef0.reverse(___bq4[1])))))),
        "push isomorphism")
def ___t30(___bq5):
    ___a27 = unpush(___bq5)
    if ___a27[0] == 1:
        ___a28 = ___a27[1]
        ___bh5 = ___a28[0]
        ___bt3 = ___a28[1]
        return (0,
            eq_li((1, ___bh5, ___iPredef0.concat(___bt3[0], ___iPredef0.reverse(___bt3[1]))),
                ___iPredef0.concat(___bq5[0], ___iPredef0.reverse(___bq5[1]))),
            "pop non-empty")
    else:
        return (0,
            ___iPredef0.concat(___bq5[0], ___iPredef0.reverse(___bq5[1]))[0] == 0,
            "empty is only unpush")
queue_laws = ___iProperties4.suite_Prop("queue properties",
    (1,
        ___iProperties4.forall_Prop(rand_Queue_Int, "pop-law/toList", ___t21),
        (1,
            ___iProperties4.forall_Prop(rand_Queue_Int, "reverse isomorphism", ___t23),
            (1,
                ___iProperties4.forall_Prop(___iRand2.prod_Rand(rand_int, rand_Queue_Int),
                    "push is the same as reverse prepend reverse",
                    ___t29),
                (1,
                    ___iProperties4.forall_Prop(rand_Queue_Int, "pop isomorphism", ___t30),
                    (0,))))))

___a29 = unpush(q12)
if ___a29[0] == 1:
    ___a30 = ___a29[1]
    ___ba12 = ___a30[0]
    ___t31 = (1, ___ba12)
else:
    ___t31 = (0,)
___a31 = (___t31, (1, 1))
___a33 = ___a31[1]
___a32 = ___a31[0]
if (___a32[0] == 1) and (___a33[0] == 1):
    ___ba13 = ___a32[1]
    ___bb19 = ___a33[1]
    ___t32 = ___ba13 == ___bb19
else:
    ___a35 = ___a31[1]
    ___a34 = ___a31[0]
    ___t32 = (___a34[0] == 0) and (___a35[0] == 0)
___bf17 = q12[0]
___bb20 = q12[1]
___bf18 = q12[0]
___bb21 = q12[1]
___bf19 = q12[0]
___bb22 = q12[1]
___bf20 = q12[0]
___bb23 = q12[1]
___bf21 = q12[0]
___bb24 = q12[1]
___a36 = unpush(empty_Queue)
if ___a36[0] == 1:
    ___a37 = ___a36[1]
    ___t41 = ___a37[1]
else:
    ___t41 = empty_Queue
tests = (1,
    "queue tests",
    (1,
        (0, ___t32, "1"),
        (1,
            (0,
                ___iPredef0.foldl_List(___iPredef0.reverse(___bb20),
                    ___iPredef0.foldl_List(___bf17, 0, lambda ___t33, ___t34: ___t33 + ___t34),
                    lambda ___t35, ___t36: ___t35 + ___t36) == 3,
                "fold_Queue add"),
            (1,
                (0,
                    ___iPredef0.foldl_List(___iPredef0.reverse(___bb21),
                        ___iPredef0.foldl_List(___bf18, 0, lambda ___ba14, ___bx0: ___bx0),
                        lambda ___ba15, ___bx1: ___bx1) == 2,
                    "take the second"),
                (1,
                    (0,
                        ___iPredef0.foldl_List(___iPredef0.reverse(___bb22),
                            ___iPredef0.foldl_List(___bf19, 0, lambda ___bx2, ___ba16: ___bx2),
                            lambda ___bx3, ___ba17: ___bx3) == 0,
                        "take the first"),
                    (1,
                        (0, eq_qi(reverse_Queue((___bb23, ___bf20)), q12), "reverse is idempotent"),
                        (1,
                            (0, eq_qi(q12, from_List((1, 1, (1, 2, (0,))))), "from list [1, 2]"),
                            (1,
                                (0,
                                    eq_qi((___bf21, (1, 3, ___bb24)),
                                        from_List((1, 1, (1, 2, (1, 3, (0,)))))),
                                    "from list [1, 2, 3]"),
                                (1,
                                    (0,
                                        eq_qi(empty_Queue, ((0,), (0,))),
                                        "empty_Queue == from_List([])"),
                                    (1,
                                        (0,
                                            eq_qi(q12, from_List((1, 1, (1, 2, (0,))))),
                                            "from list [1, 2]"),
                                        (1,
                                            (0,
                                                eq_qi(pop(pop(pop(from_List((1,
                                                                        1,
                                                                        (1, 2, (1, 3, (0,)))))))),
                                                    empty_Queue),
                                                "pop to empty"),
                                            (1,
                                                (0,
                                                    eq_qi(___t41, empty_Queue),
                                                    "pop empty is okay"),
                                                (1,
                                                    (0,
                                                        eq_li(to_List(from_List((1,
                                                                        1,
                                                                        (1,
                                                                            1,
                                                                            (1,
                                                                                2,
                                                                                (1,
                                                                                    2,
                                                                                    (1,
                                                                                        3,
                                                                                        (1,
                                                                                            3,
                                                                                            (0,))))))))),
                                                            (1,
                                                                1,
                                                                (1,
                                                                    1,
                                                                    (1,
                                                                        2,
                                                                        (1,
                                                                            2,
                                                                            (1,
                                                                                3,
                                                                                (1, 3, (0,)))))))),
                                                        "to/from List"),
                                                    (1,
                                                        ___iProperties4.run_Prop(queue_laws,
                                                            100,
                                                            4242),
                                                        (0,)))))))))))))))

class BosatsuTests(___iunittest5.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t42 = value[2]
                while ___t42[0] != 0:
                    test_loop(___t42[1])
                    ___t42 = ___t42[2]
        test_loop(tests)