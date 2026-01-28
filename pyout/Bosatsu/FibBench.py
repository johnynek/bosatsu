import Bosatsu.Nat as ___iNat1
import Bosatsu.Prog as ___iProg2
import ProgExt as ___iProgExt0

def fib_Nat(___bn0):
    if ___bn0 == 0:
        return 1
    else:
        if ___bn0 > 0:
            ___a0 = ___bn0 - 1
            ___t0 = ___a0 == 0
        else:
            ___t0 = False
        if ___t0:
            return 1
        else:
            ___a1 = ___bn0 - 1
            return fib_Nat(___a1) + fib_Nat(___a1 - 1)

def print_fib(___bstr0):
    ___a3 = (1,
        int(___bstr0)) if ((___bstr0[0] == "-") and ___bstr0[1:].isdigit()) or ___bstr0.isdigit() else (0,)
    if ___a3[0] == 1:
        ___bi0 = ___a3[1]
        ___t1 = []
        ___t2 = (1,
            "fib(",
            (1, ___bstr0, (1, ") = ", (1, fib_Nat(___iNat1.to_Nat(___bi0)).__str__(), (0,)))))
        while ___t2[0] != 0:
            ___t1.append(___t2[1])
            ___t2 = ___t2[2]
        return ___iProgExt0.println("".join(___t1))
    else:
        ___t3 = []
        ___t4 = (1, "could not parse ", (1, ___bstr0, (0,)))
        while ___t4[0] != 0:
            ___t3.append(___t4[1])
            ___t4 = ___t4[2]
        return ___iProgExt0.println("".join(___t3))

def list_len(___blst0, ___bacc0):
    ___a7 = ___blst0
    ___a9 = ___bacc0
    ___a4 = 1
    ___t5 = ___a4 == 1
    while ___t5:
        if ___a7[0] == 0:
            ___a4 = 0
            ___a5 = ___a9
        else:
            ___btail0 = ___a7[2]
            ___a6 = ___btail0
            ___a8 = ___a9 + 1
            ___a7 = ___a6
            ___a9 = ___a8
        ___t5 = ___a4 == 1
    return ___a5

def _a(___bargs0):
    if ___bargs0[0] == 1:
        ___a10 = ___bargs0[2]
        if ___a10[0] == 1:
            ___a11 = ___a10[2]
            ___t8 = ___a11[0] == 0
        else:
            ___t8 = False
    else:
        ___t8 = False
    if ___t8:
        return print_fib(___a10[1])
    else:
        ___t6 = []
        ___t7 = (1, "expected exactly one arg, got: ", (1, list_len(___bargs0, 0).__str__(), (0,)))
        while ___t7[0] != 0:
            ___t6.append(___t7[1])
            ___t7 = ___t7[2]
        return ___iProgExt0.println("".join(___t6))

main = ___iProg2.___nawait(___iProgExt0.read_env)(lambda ___bargs1: ___iProg2.___nawait(___iProg2.ignore_env(_a(___bargs1)))(lambda ___ba0: ___iProgExt0.pure(0)))

if __name__ == "__main__":
    ___iProgExt0.run(main)