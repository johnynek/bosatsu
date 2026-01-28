import Bosatsu.BinNat as ___iBinNat0

def cmp(___ba0, ___bb0):
    if ___ba0[0] == 0:
        ___ba1 = ___ba0[1]
        if ___bb0[0] == 0:
            return ___iBinNat0.cmp_BinNat(___ba1, ___bb0[1])
        else:
            return 2
    else:
        ___ba2 = ___ba0[1]
        if ___bb0[0] == 1:
            return ___iBinNat0.cmp_BinNat(___bb0[1], ___ba2)
        else:
            return 0

def eq(___ba3, ___bb3):
    if ___ba3[0] == 0:
        ___ba4 = ___ba3[1]
        if ___bb3[0] == 0:
            return ___iBinNat0.eq_BinNat(___ba4, ___bb3[1])
        else:
            return 0
    else:
        ___ba5 = ___ba3[1]
        if ___bb3[0] == 1:
            return ___iBinNat0.eq_BinNat(___ba5, ___bb3[1])
        else:
            return 0

binNat_to_BinInt = lambda ___t0: (0, ___t0)

def ___nnot(___bbi0):
    if ___bbi0[0] == 0:
        return (1, ___bbi0[1])
    else:
        return (0, ___bbi0[1])

def int_to_BinInt(___bi0):
    return (1,
        ___iBinNat0.toBinNat(-1 - ___bi0)) if (0 if ___bi0 < 0 else 1 if ___bi0 == 0 else 2) == 0 else (0,
        ___iBinNat0.toBinNat(___bi0))

def binInt_to_Int(___bbi1):
    if ___bbi1[0] == 0:
        return ___iBinNat0.toInt(___bbi1[1])
    else:
        return -1 - ___iBinNat0.toInt(___bbi1[1])

def negate(___bbi2):
    if ___bbi2[0] == 0:
        ___a1 = ___bbi2[1]
        ___t1 = ___a1[0] == 0
    else:
        ___t1 = False
    if ___t1:
        return ___bbi2
    elif ___bbi2[0] == 0:
        return (1, ___iBinNat0.prev(___bbi2[1]))
    else:
        return (0, ___iBinNat0.next(___bbi2[1]))

def abs(___bbi3):
    if ___bbi3[0] == 0:
        return ___bbi3[1]
    else:
        return ___iBinNat0.next(___bbi3[1])

def add(___bx3, ___by0):
    ___a6 = (___bx3, ___by0)
    ___a8 = ___a6[1]
    ___a7 = ___a6[0]
    if (___a7[0] == 0) and (___a8[0] == 0):
        return (0, ___iBinNat0.add_BinNat(___a7[1], ___a8[1]))
    else:
        ___a10 = ___a6[1]
        ___a9 = ___a6[0]
        if (___a9[0] == 0) and (___a10[0] == 1):
            ___bx5 = ___a9[1]
            ___by2 = ___a10[1]
            ___bypos0 = ___iBinNat0.next(___by2)
            ___a3 = ___iBinNat0.sub_Option(___bx5, ___bypos0)
            if ___a3[0] == 1:
                return (0, ___a3[1])
            else:
                ___bbi5 = (0, ___iBinNat0.sub_BinNat(___bypos0, ___bx5))
                if ___bbi5[0] == 0:
                    ___a2 = ___bbi5[1]
                    ___t2 = ___a2[0] == 0
                else:
                    ___t2 = False
                if ___t2:
                    return ___bbi5
                else:
                    return (1, ___iBinNat0.prev(___bbi5[1]))
        else:
            ___a12 = ___a6[1]
            ___a11 = ___a6[0]
            if (___a11[0] == 1) and (___a12[0] == 0):
                ___bx6 = ___a11[1]
                ___by3 = ___a12[1]
                ___bxpos0 = ___iBinNat0.next(___bx6)
                ___a5 = ___iBinNat0.sub_Option(___by3, ___bxpos0)
                if ___a5[0] == 1:
                    return (0, ___a5[1])
                else:
                    ___bbi7 = (0, ___iBinNat0.sub_BinNat(___bxpos0, ___by3))
                    if ___bbi7[0] == 0:
                        ___a4 = ___bbi7[1]
                        ___t3 = ___a4[0] == 0
                    else:
                        ___t3 = False
                    if ___t3:
                        return ___bbi7
                    else:
                        return (1, ___iBinNat0.prev(___bbi7[1]))
            else:
                return (1, ___iBinNat0.next(___iBinNat0.add_BinNat(___a6[0][1], ___a6[1][1])))

def sub(___ba6, ___bb8):
    if ___bb8[0] == 0:
        ___a15 = ___bb8[1]
        ___t4 = ___a15[0] == 0
    else:
        ___t4 = False
    if ___t4:
        ___t5 = ___bb8
    elif ___bb8[0] == 0:
        ___bbn4 = ___bb8[1]
        ___t5 = (1, ___iBinNat0.prev(___bbn4))
    else:
        ___bx8 = ___bb8[1]
        ___t5 = (0, ___iBinNat0.next(___bx8))
    return add(___ba6, ___t5)