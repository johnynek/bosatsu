import Bosatsu.Predef as ___iPredef0
import unittest as ___iunittest1

def keep(___bi0):
    return 1 if ((___bi0 % 3) == 0) == 1 else (___bi0 % 5) == 0

def ___t0(___bi1):
    return (1, ___bi1, (0,)) if keep(___bi1) == 1 else (0,)
computed = ___iPredef0.foldl_List(___iPredef0.flat_map_List(___iPredef0.range(1000), ___t0),
    0,
    lambda ___t1, ___t2: ___t1 + ___t2)

___t3 = []
___t4 = (1, "expected 233168 got ", (1, computed.__str__(), (0,)))
while ___t4[0] != 0:
    ___t3.append(___t4[1])
    ___t4 = ___t4[2]
test = (0, computed == 233168, "".join(___t3))

class BosatsuTests(___iunittest1.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t5 = value[2]
                while ___t5[0] != 0:
                    test_loop(___t5[1])
                    ___t5 = ___t5[2]
        test_loop(test)