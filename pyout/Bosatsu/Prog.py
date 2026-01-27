import Bosatsu.Predef as ___iPredef1
import ProgExt as ___iProgExt0

def map(___bprog0, ___bfn0):
    ___t1 = (___bfn0,)
    def ___t0(___bres0):
        return ___iProgExt0.pure(___t1[0](___bres0))
    return ___iProgExt0.flat_map(___bprog0, ___t0)

def map_err(___bprog1, ___bfn1):
    ___t3 = (___bfn1,)
    def ___t2(___bres1):
        return ___iProgExt0.raise_error(___t3[0](___bres1))
    return ___iProgExt0.recover(___bprog1, ___t2)

def with_env(___bp0, ___benv0):
    ___t5 = (___benv0,)
    def ___t4(___ba0):
        return ___t5[0]
    return ___iProgExt0.remap_env(___bp0, ___t4)

def ignore_env(___bp1):
    return ___iProgExt0.remap_env(___bp1, lambda ___ba1: ())

def ___nawait(___bp2):
    ___t8 = (___bp2,)
    def ___t7(___bfn2):
        return ___iProgExt0.flat_map(___t8[0], ___bfn2)
    return ___t7

def recursive(___bfn3):
    ___t10 = (___bfn3,)
    def ___t9(___ba2):
        return ___iProgExt0.apply_fix(___ba2, ___t10[0])
    return ___t9

unit = ___iProgExt0.pure(())

def count_down(___ba3):
    def ___t11(___bloop0):
        ___t13 = (___bloop0,)
        def ___t12(___bi0):
            ___a0 = 0 if ___bi0 < 0 else 1 if ___bi0 == 0 else 2
            if ___a0 == 1:
                return ___iProgExt0.println("\ndone")
            elif ___a0 == 0:
                return ___iProgExt0.println("\ndone")
            else:
                ___t14 = []
                ___t15 = (1, ___bi0.__str__(), (1, ", ", (0,)))
                while ___t15[0] != 0:
                    ___t14.append(___t15[1])
                    ___t15 = ___t15[2]
                ___t17 = (___t13[0], ___bi0)
                def ___t16(___ba4):
                    return ___t17[0](___t17[1] + -1)
                return ___iProgExt0.flat_map(___iProgExt0.print_stdout("".join(___t14)), ___t16)
        return ___t12
    return ___iProgExt0.apply_fix(___ba3, ___t11)

def ___t18(___bargs0):
    def ___t19(___bs0, ___bitem0):
        if ___bs0 == "":
            return ___bitem0
        else:
            ___t20 = []
            ___t21 = (1, ___bs0, (1, ", ", (1, ___bitem0, (0,))))
            while ___t21[0] != 0:
                ___t20.append(___t21[1])
                ___t21 = ___t21[2]
            return "".join(___t20)
    ___barg__str0 = ___iPredef1.foldl_List(___bargs0, "", ___t19)
    ___bprog2 = ___iProgExt0.println("<failed to read stdin")
    ___t23 = (___bprog2,)
    def ___t22(___ba5):
        return ___iProgExt0.flat_map(___t23[0], lambda ___ba6: ___iProgExt0.pure(""))
    ___t26 = (___barg__str0,)
    def ___t25(___bstdin0):
        ___t27 = []
        ___t28 = (1, "found stdin: ", (1, ___bstdin0, (0,)))
        while ___t28[0] != 0:
            ___t27.append(___t28[1])
            ___t28 = ___t28[2]
        ___t30 = (___t26[0],)
        def ___t29(___ba7):
            ___t32 = (___t30[0],)
            def ___t31(___ba8):
                ___t33 = []
                ___t34 = (1, "args = ", (1, ___t32[0], (0,)))
                while ___t34[0] != 0:
                    ___t33.append(___t34[1])
                    ___t34 = ___t34[2]
                return ___iProgExt0.flat_map(___iProgExt0.println("".join(___t33)),
                    lambda ___ba9: ___iProgExt0.pure(0))
            return ___iProgExt0.flat_map(count_down(10), ___t31)
        return ___iProgExt0.flat_map(___iProgExt0.println("".join(___t27)), ___t29)
    return ignore_env(___iProgExt0.flat_map(___iProgExt0.recover(___iProgExt0.read_stdin_utf8_bytes(1000),
                ___t22),
            ___t25))
to_run = ___iProgExt0.flat_map(___iProgExt0.read_env, ___t18)

if __name__ == "__main__":
    ___iProgExt0.run(to_run)