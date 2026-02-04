import Bosatsu.Bool as ___iBool0
import Bosatsu.List as ___iList2
import Bosatsu.Predef as ___iPredef1
import unittest as ___iunittest3

def int_loop_up(___btop0, ___bres0, ___bfn0):
    def ___t0(___bi0, ___bres1):
        ___a0 = ___bfn0(___btop0 - ___bi0, ___bres1)
        return (___btop0 - ___a0[0], ___a0[1])
    ___t1 = 0 < ___btop0
    ___t2 = ___bres0
    ___t3 = ___btop0
    ___t4 = ___bres0
    while ___t1:
        ___t2 = ___t0(___t3, ___t4)
        ___t5 = ___t2[0]
        ___t4 = ___t2[1]
        ___t1 = (0 < ___t5) and (___t5 < ___t3)
        ___t3 = ___t5
    return ___t4

def is_prime(___bx0):
    def ___t6(___bi1, ___bdiv0):
        ___bcandidate0 = ___bi1 + 2
        return ((___bx0, 1) if ___bdiv0 == 1 else (___bi1 + 1,
            (___bx0 % ___bcandidate0 if ___bcandidate0 else ___bx0) == 0)) if (0 if ___bcandidate0 < ___bx0 else 1 if ___bcandidate0 == ___bx0 else 2) == 0 else (___bx0,
            0)
    return ___iBool0.___nnot(int_loop_up(___bx0, 0, ___t6))

def ith_prime(___btotal0):
    ___bmax__size0 = ___iPredef1.range(___btotal0 * ___btotal0)
    def ___t7(___bprime__cnt0, ___bi2):
        ___bcandidate1 = ___bi2 + 2
        ___bprimes0 = ___bprime__cnt0[0]
        ___bcnt0 = ___bprime__cnt0[1]
        ___a3 = 0 if ___bcnt0 < ___btotal0 else 1 if ___bcnt0 == ___btotal0 else 2
        if ___a3 == 0:
            ___t9 = (___bcandidate1,)
            def ___t8(___bp0):
                return ___iBool0.___nnot((___t9[0] % ___bp0 if ___bp0 else ___t9[0]) == 0)
            return ((1, ___bcandidate1, ___bprimes0),
                ___bcnt0 + 1) if ___iList2.for_all(___bprimes0, ___t8) == 1 else ___bprime__cnt0
        else:
            return ___bprime__cnt0
    ___a4 = ___iPredef1.foldl_List(___bmax__size0, ((0,), 0), ___t7)
    ___bps0 = ___a4[0]
    return ___bps0[1] if ___bps0[0] == 1 else -1

test = (1,
    "euler 7",
    (1,
        (0, is_prime(13), "6th prime is 13"),
        (1,
            (0, ith_prime(6) == 13, "6th prime is 13"),
            (1, (0, ith_prime(11) == 31, "11th prime is 31"), (0,)))))

class BosatsuTests(___iunittest3.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t10 = value[2]
                while ___t10[0] != 0:
                    test_loop(___t10[1])
                    ___t10 = ___t10[2]
        test_loop(test)