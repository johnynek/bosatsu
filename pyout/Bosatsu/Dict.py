import Bosatsu.List as ___iList0
import Bosatsu.Predef as ___iPredef1

def eq_Pair(___beq__a0, ___beq__b0):
    ___t1 = (___beq__a0, ___beq__b0)
    def ___t0(___ba0, ___bb0):
        return ___t1[1](___ba0[1], ___bb0[1]) if ___t1[0](___ba0[0], ___bb0[0]) == 1 else 0
    return ___t0

def eq_Dict(___beq__key0, ___beq__value0):
    ___t3 = (___beq__key0, ___beq__value0)
    def ___t2(___bleft0, ___bright0):
        ___t5 = (___t3[0], ___t3[1])
        def ___t4(___ba1, ___bb1):
            return ___t5[1](___ba1[1], ___bb1[1]) if ___t5[0](___ba1[0], ___bb1[0]) == 1 else 0
        return ___iList0.eq_List(___t4)(___iPredef1.items(___bleft0), ___iPredef1.items(___bright0))
    return ___t2