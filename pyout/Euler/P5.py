import Bosatsu.List as ___iList1
import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest2

def factorial(___bn0):
    ___t1 = 0 < ___bn0
    ___t2 = 1
    ___t3 = ___bn0
    ___t4 = 1
    while ___t1:
        ___t5 = ___t3 - 1
        ___t4 = ___t4 * ___t3
        ___t1 = (0 < ___t5) and (___t5 < ___t3)
        ___t3 = ___t5
    return ___t4

max_candidate = factorial(10)

def int_loop_up(___btop0, ___bres0, ___bfn0):
    def ___t6(___bi1, ___bres1):
        ___a0 = ___bfn0(___btop0 - ___bi1, ___bres1)
        return (___btop0 - ___a0[0], ___a0[1])
    ___t7 = 0 < ___btop0
    ___t8 = ___bres0
    ___t9 = ___btop0
    ___t10 = ___bres0
    while ___t7:
        ___t8 = ___t6(___t9, ___t10)
        ___t11 = ___t8[0]
        ___t10 = ___t8[1]
        ___t7 = (0 < ___t11) and (___t11 < ___t9)
        ___t9 = ___t11
    return ___t10

bound = max_candidate + 1

factors = ___iPredef0.range(10)

def ___t12(___bi2, ___ba0):
    ___bcand0 = ___bi2 + 1
    return (bound, ___bcand0) if ___iList1.for_all(factors,
        lambda ___bf0: (___bcand0 % (___bf0 + 1) if ___bf0 + 1 else ___bcand0) == 0) == 1 else (___bi2 + 1,
        0)
div_all = int_loop_up(bound, 0, ___t12)

test = (0, div_all == 2520, "test")

class BosatsuTests(___iunittest2.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t14 = value[2]
                while ___t14[0] != 0:
                    test_loop(___t14[1])
                    ___t14 = ___t14[2]
        test_loop(test)