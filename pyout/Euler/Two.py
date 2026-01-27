import Bosatsu.List as ___iList1
import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest2

def fib(___bn0):
    def ___t0(___brevFib0, ___ba0):
        if ___brevFib0[0] == 0:
            return (1, 1, (0,))
        else:
            if ___brevFib0[0] == 1:
                ___a0 = ___brevFib0[2]
                ___t1 = ___a0[0] == 0
            else:
                ___t1 = False
            if ___t1:
                return (1, 2, (1, ___brevFib0[1], (0,)))
            else:
                return (1, ___brevFib0[1] + ___brevFib0[2][1], ___brevFib0)
    return ___iPredef0.foldl_List(___iPredef0.range(___bn0), (0,), ___t0)

def ___t2(___bf0):
    return (1,
        ___bf0,
        (0,)) if (0 if (0 if ___bf0 < 4000000 else 1 if ___bf0 == 4000000 else 2) == 2 else (___bf0 % 2) == 0) == 1 else (0,)
computed = ___iList1.sum(___iPredef0.flat_map_List(fib(35), ___t2))

test = (0, computed == 4613732, "expected 4613732")

class BosatsuTests(___iunittest2.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t3 = value[2]
                while ___t3[0] != 0:
                    test_loop(___t3[1])
                    ___t3 = ___t3[2]
        test_loop(test)