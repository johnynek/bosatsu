

def ack1(___bn0):
    if ___bn0 == 0:
        return lambda ___t0: ___t0 + 1
    else:
        ___a1 = ___bn0 - 1
        ___bn__prev0 = ___a1
        ___back__p0 = ack1(___bn__prev0)
        ___t1 = (___back__p0,)
        def ___binner0(___bm0):
            if ___bm0 == 0:
                ___t2 = 1
            else:
                ___a0 = ___bm0 - 1
                ___bm__prev0 = ___a0
                ___t2 = ___binner0(___bm__prev0)
            return ___t1[0](___t2)
        return ___binner0

def ack(___bn1, ___bm1):
    return ack1(___bn1)(___bm1)