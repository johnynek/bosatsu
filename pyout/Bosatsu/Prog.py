import Bosatsu.Predef as ___iPredef1
import ProgExt as ___iProgExt0

def map(___bprog0, ___bfn0):
    return ___iProgExt0.flat_map(___bprog0, lambda ___bres0: ___iProgExt0.pure(___bfn0(___bres0)))

def map_err(___bprog1, ___bfn1):
    return ___iProgExt0.recover(___bprog1,
        lambda ___bres1: ___iProgExt0.raise_error(___bfn1(___bres1)))

def with_env(___bp0, ___benv0):
    return ___iProgExt0.remap_env(___bp0, lambda ___ba0: ___benv0)

def ignore_env(___bp1):
    return ___iProgExt0.remap_env(___bp1, lambda ___ba1: ())

def ___nawait(___bp2):
    return lambda ___bfn2: ___iProgExt0.flat_map(___bp2, ___bfn2)

def recursive(___bfn3):
    return lambda ___ba2: ___iProgExt0.apply_fix(___ba2, ___bfn3)

unit = ___iProgExt0.pure(())

def count_down(___ba3):
    def ___t6(___bloop0):
        def ___t7(___bi0):
            ___a0 = 0 if ___bi0 < 0 else 1 if ___bi0 == 0 else 2
            if ___a0 == 1:
                return ___iProgExt0.println("\ndone")
            elif ___a0 == 0:
                return ___iProgExt0.println("\ndone")
            else:
                ___t8 = []
                ___t9 = (1, ___bi0.__str__(), (1, ", ", (0,)))
                while ___t9[0] != 0:
                    ___t8.append(___t9[1])
                    ___t9 = ___t9[2]
                ___t11 = (___bloop0, ___bi0)
                def ___t10(___ba4):
                    return ___t11[0](___t11[1] + -1)
                return ___iProgExt0.flat_map(___iProgExt0.print_stdout("".join(___t8)), ___t10)
        return ___t7
    return ___iProgExt0.apply_fix(___ba3, ___t6)

def ___t12(___bargs0):
    def ___t13(___ba5):
        return ___iProgExt0.flat_map(___iProgExt0.println("<failed to read stdin"),
            lambda ___ba6: ___iProgExt0.pure(""))
    def ___t15(___bstdin0):
        ___t16 = []
        ___t17 = (1, "found stdin: ", (1, ___bstdin0, (0,)))
        while ___t17[0] != 0:
            ___t16.append(___t17[1])
            ___t17 = ___t17[2]
        ___t19 = (___bargs0,)
        def ___t18(___ba7):
            ___t21 = (___t19[0],)
            def ___t20(___ba8):
                def ___t22(___bs0, ___bitem0):
                    if ___bs0 == "":
                        return ___bitem0
                    else:
                        ___t23 = []
                        ___t24 = (1, ___bs0, (1, ", ", (1, ___bitem0, (0,))))
                        while ___t24[0] != 0:
                            ___t23.append(___t24[1])
                            ___t24 = ___t24[2]
                        return "".join(___t23)
                ___barg__str0 = ___iPredef1.foldl_List(___t21[0], "", ___t22)
                ___t25 = []
                ___t26 = (1, "args = ", (1, ___barg__str0, (0,)))
                while ___t26[0] != 0:
                    ___t25.append(___t26[1])
                    ___t26 = ___t26[2]
                return ___iProgExt0.flat_map(___iProgExt0.println("".join(___t25)),
                    lambda ___ba9: ___iProgExt0.pure(0))
            return ___iProgExt0.flat_map(count_down(10), ___t20)
        return ___iProgExt0.flat_map(___iProgExt0.println("".join(___t16)), ___t18)
    return ignore_env(___iProgExt0.flat_map(___iProgExt0.recover(___iProgExt0.read_stdin_utf8_bytes(1000),
                ___t13),
            ___t15))
to_run = ___iProgExt0.flat_map(___iProgExt0.read_env, ___t12)

if __name__ == "__main__":
    ___iProgExt0.run(to_run)