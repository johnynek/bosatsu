import Bosatsu.Nat as ___iNat0

done = lambda ___t0: (0, ___t0)

def flat_map(___be0, ___bfn0):
    return (1, lambda ___bcb0: ___bcb0(___be0, ___bfn0))

def bind(___be1):
    def ___t2(___bfn1):
        ___t4 = (___be1, ___bfn1)
        def ___t3(___bcb1):
            return ___bcb1(___t4[0], ___t4[1])
        return (1, ___t3)
    return ___t2

def map(___be2, ___bfn2):
    def ___t5(___bcb2):
        ___t7 = (___bfn2,)
        def ___t6(___bx0):
            return (0, ___t7[0](___bx0))
        return ___bcb2(___be2, ___t6)
    return (1, ___t5)

def run(___bbudget0, ___barg0):
    if ___bbudget0 == 0:
        return (0,)
    else:
        ___a4 = ___bbudget0 - 1
        ___bbalance0 = ___a4
        if ___barg0[0] == 0:
            ___a0 = ___barg0[2]
            ___t14 = ___a0[0] == 0
        else:
            ___t14 = False
        if ___t14:
            return (1, ___a0[1](___barg0[1]))
        else:
            if ___barg0[0] == 1:
                ___a1 = ___barg0[1]
                ___t13 = ___a1[0] == 0
            else:
                ___t13 = False
            if ___t13:
                return run(___bbalance0, (0, ___a1[1], ___barg0[2]))
            else:
                if ___barg0[0] == 1:
                    ___a2 = ___barg0[1]
                    ___t12 = ___a2[0] == 1
                else:
                    ___t12 = False
                if ___t12:
                    ___buse0 = ___a2[1]
                    ___bstack1 = ___barg0[2]
                    def ___t8(___bprev0, ___bfn4):
                        ___t10 = (___bfn4, ___bstack1)
                        def ___t9(___buse1):
                            return ___buse1(___t10[0], ___t10[1])
                        return run(___bbalance0, (1, ___bprev0, (1, ___t9)))
                    return ___buse0(___t8)
                else:
                    return ___barg0[2][1](lambda ___bfn5, ___bstack2: run(___bbalance0,
                            (1, ___bfn5(___barg0[1]), ___bstack2)))

def eval(___bbudget1, ___bea0):
    return run(___iNat0.to_Nat(___bbudget1), (1, ___bea0, (0, lambda ___ba3: ___ba3)))