

def eq_Option(___beq0):
    ___t1 = (___beq0,)
    def ___t0(___bleft0, ___bright0):
        ___a0 = (___bleft0, ___bright0)
        ___a2 = ___a0[1]
        ___a1 = ___a0[0]
        if (___a1[0] == 1) and (___a2[0] == 1):
            return ___t1[0](___a1[1], ___a2[1])
        else:
            return (___a0[0][0] == 0) and (___a0[1][0] == 0)
    return ___t0