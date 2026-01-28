import Bosatsu.Nat as ___iNat0

done = lambda ___t0: (0, ___t0)

def flat_map(___be0, ___bfn0):
    ___t2 = (___be0, ___bfn0)
    def ___t1(___bcb0):
        return ___bcb0(___t2[0], ___t2[1])
    return (1, ___t1)

def bind(___be1):
    ___t4 = (___be1,)
    def ___t3(___bfn1):
        ___t6 = (___t4[0], ___bfn1)
        def ___t5(___bcb1):
            return ___bcb1(___t6[0], ___t6[1])
        return (1, ___t5)
    return ___t3

def map(___be2, ___bfn2):
    ___t8 = (___be2, ___bfn2)
    def ___t7(___bcb2):
        ___t10 = (___t8[1],)
        def ___t9(___bx0):
            return (0, ___t10[0](___bx0))
        return ___bcb2(___t8[0], ___t9)
    return (1, ___t7)

def run(___bbudget0, ___barg0):
    if ___bbudget0 == 0:
        return (0,)
    else:
        ___a4 = ___bbudget0 - 1
        ___bbalance0 = ___a4
        if ___barg0[0] == 0:
            ___a0 = ___barg0[2]
            ___t19 = ___a0[0] == 0
        else:
            ___t19 = False
        if ___t19:
            return (1, ___a0[1](___barg0[1]))
        else:
            if ___barg0[0] == 1:
                ___a1 = ___barg0[1]
                ___t18 = ___a1[0] == 0
            else:
                ___t18 = False
            if ___t18:
                return run(___bbalance0, (0, ___a1[1], ___barg0[2]))
            else:
                if ___barg0[0] == 1:
                    ___a2 = ___barg0[1]
                    ___t17 = ___a2[0] == 1
                else:
                    ___t17 = False
                if ___t17:
                    ___buse0 = ___a2[1]
                    ___bstack1 = ___barg0[2]
                    ___t12 = (run, ___bbalance0, ___bstack1)
                    def ___t11(___bprev0, ___bfn4):
                        ___t14 = (___bfn4, ___t12[2])
                        def ___t13(___buse1):
                            return ___buse1(___t14[0], ___t14[1])
                        return ___t12[0](___t12[1], (1, ___bprev0, (1, ___t13)))
                    return ___buse0(___t11)
                else:
                    ___a3 = ___barg0[2]
                    ___ba2 = ___barg0[1]
                    ___buse2 = ___a3[1]
                    ___t16 = (run, ___bbalance0, ___ba2)
                    def ___t15(___bfn5, ___bstack2):
                        return ___t16[0](___t16[1], (1, ___bfn5(___t16[2]), ___bstack2))
                    return ___buse2(___t15)

def eval(___bbudget1, ___bea0):
    return run(___iNat0.to_Nat(___bbudget1), (1, ___bea0, (0, lambda ___ba3: ___ba3)))